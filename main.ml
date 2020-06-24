open Core

(* module type X_int = sig
  val x : int
end

module Three : X_int = struct let x = 3 end

let three = (module Three : X_int)

module Four = struct let x = 4 end

let numbers = [three; (module Four); (module struct let x = 5 end)]

module Unpaked = (val three : X_int)

let to_int m =
  let module M = (val three : X_int) in
  M.x

let plus m1 m2 = (module struct let x = to_int m1 + to_int m2 end : X_int)
let x (module M : X_int) = M.x

module type Bumpable = sig
  type t

  val bump : t -> t
end

module Int_bumper = struct
  type t = int

  let bump t = t + 1
end

module Float_bumper = struct
  type t = float

  let bump t = t +. 1.
end

let int_bumper = (module Int_bumper : Bumpable with type t = int)
let float_bumper = (module Float_bumper : Bumpable with type t = float)

let bump (type a) (module Bumper : Bumpable with type t = a) (x : a) =
  Bumper.bump x

let bump_list (type a) (module Bumper : Bumpable with type t = a) (l : a list) =
  List.map ~f:Bumper.bump l

let res = bump int_bumper 1

module type Comparable = sig
  type t

  val compare : t -> t -> int
end *)
(* 
let create_comparable (type a) compare =
  ( module struct
    type t = a

    let compare = compare end : Comparable
    with type t = a ) *)

module type Query_handler = sig
  type config

  val sexp_of_config : config -> Sexp.t
  val config_of_sexp : Sexp.t -> config

  (* クエリハンドルサービス名 *)
  val name : string

  (* クエリの状態のデータ構造 *)
  type t

  val create : config -> t
  val eval : t -> Sexp.t -> Sexp.t Or_error.t
end

module Unique = struct
  type config = int [@@deriving sexp]
  type t = {mutable next_id: int}

  let name = "unique"
  let create start_at = {next_id= start_at}

  let eval t _ =
    match Or_error.try_with (fun () -> unit_of_sexp) with
    | Error _ as err -> err
    | Ok _ ->
        let response = Ok (Int.sexp_of_t t.next_id) in
        t.next_id <- t.next_id + 1 ;
        response
end

module List_dir = struct
  type config = string [@@deriving sexp]
  type t = {cwd: string}

  let is_abs p = String.length p > 0 && Char.( = ) p.[0] '/'
  let name = "ls"
  let create cwd = {cwd}

  let eval t sexp =
    match Or_error.try_with (fun () -> string_of_sexp sexp) with
    | Error _ as err -> err
    | Ok dir ->
        let dir = if is_abs dir then dir else Core.Filename.concat t.cwd dir in
        Ok (Array.sexp_of_t String.sexp_of_t (Core.Sys.readdir dir))
end

module type Query_handler_instance = sig
  module Query_handler : Query_handler

  val this : Query_handler.t
end

(* let build_instance (type a) (module Q : Query_handler with type config = a)
    config =
  ( module struct
    module Query_handler = Q

    let this = Q.create config end : Query_handler_instance )

let unique_instance = build_instance (module Unique) 0

let list_dir_instance =
  build_instance (module List_dir) "/Users/masaharuhosomichi"

let build_dispatch_table handlers =
  let table = Hashtbl.create (module String) in
  List.iter handlers
    ~f:(fun ((module I : Query_handler_instance) as instance) ->
      Hashtbl.set table ~key:I.Query_handler.name ~data:instance) ;
  table *)

let dispatch dispatch_table name_and_query =
  match name_and_query with
  | Sexp.List [Sexp.Atom name; query] -> (
    match Hashtbl.find dispatch_table name with
    | None ->
        Or_error.error "Could not find matching handler" name String.sexp_of_t
    | Some (module I : Query_handler_instance) ->
        I.Query_handler.eval I.this query )
  | _ -> Or_error.error_string "malformed query"

open Stdio

let rec cli dispatch_table =
  printf ">>> %!" ;
  let result =
    match In_channel.(input_line stdin) with
    | None -> `Stop
    | Some line -> (
      match Or_error.try_with (fun () -> Core_kernel.Sexp.of_string line) with
      | Error e -> `Continue (Error.to_string_hum e)
      | Ok (Sexp.Atom "quit") -> `Stop
      | Ok query -> (
        match dispatch dispatch_table query with
        | Error e -> `Continue (Error.to_string_hum e)
        | Ok s -> `Continue (Sexp.to_string_hum s) ) ) in
  match result with
  | `Stop -> ()
  | `Continue msg -> printf "%s\n%!" msg ; cli dispatch_table

(* let () = cli (build_dispatch_table [unique_instance; list_dir_instance]) *)

module Loader = struct
  type config = ((module Query_handler) list[@sexp.opaque]) [@@deriving sexp]

  type t =
    { known: (module Query_handler) String.Table.t
    ; active: (module Query_handler_instance) String.Table.t }

  let name = "loader"

  let create known_list =
    let active = String.Table.create () in
    let known = String.Table.create () in
    List.iter known_list ~f:(fun ((module Q : Query_handler) as m) ->
        Hashtbl.set known ~key:Q.name ~data:m) ;
    {active; known}

  let load t handler_name config =
    if Hashtbl.mem t.active handler_name then
      Or_error.error "Can't re-register an active handler" handler_name
        String.sexp_of_t
    else
      match Hashtbl.find t.known handler_name with
      | None -> Or_error.error "Unknown handler" handler_name String.sexp_of_t
      | Some (module Q : Query_handler) ->
          let instance =
            ( module struct
              module Query_handler = Q

              let this = Q.create (Q.config_of_sexp config) end
            : Query_handler_instance ) in
          Hashtbl.set t.active ~key:handler_name ~data:instance ;
          Ok Sexp.unit

  let unload t handler_name =
    if not (Hashtbl.mem t.active handler_name) then
      Or_error.error "Handler not active" handler_name String.sexp_of_t
    else if String.( = ) handler_name name then
      Or_error.error_string "It's unwise to unload yourself"
    else (
      Hashtbl.remove t.active handler_name ;
      Ok Sexp.unit )

  type request =
    | Load of string * Sexp.t
    | Unload of string
    | Known_services
    | Active_services
  [@@deriving sexp]

  let eval t sexp =
    match Or_error.try_with (fun () -> request_of_sexp sexp) with
    | Error _ as err -> err
    | Ok resp -> (
      match resp with
      | Load (name, config) -> load t name config
      | Unload name -> unload t name
      | Known_services -> Ok ([%sexp_of: string list] (Hashtbl.keys t.known))
      | Active_services -> Ok ([%sexp_of: string list] (Hashtbl.keys t.active))
      )
end

let () =
  let loader = Loader.create [(module Unique); (module List_dir)] in
  let loader_instance =
    ( module struct
      module Query_handler = Loader

      let this = loader end : Query_handler_instance ) in
  Hashtbl.set loader.Loader.active ~key:Loader.name ~data:loader_instance ;
  cli loader.Loader.active
