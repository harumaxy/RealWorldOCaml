(* open Base

module type Dictionary = sig
  type ('a, 'b) t

  val create : hash:('a -> int) -> equal:('a -> 'a -> bool) -> ('a, 'b) t
  val length : ('a, 'b) t -> int
  val add : ('a, 'b) t -> key:'a -> data:'b -> unit
  val find : ('a, 'b) t -> 'a -> 'b option
  val iter : ('a, 'b) t -> f:(key:'a -> data:'b -> unit) -> unit
  val remove : ('a, 'b) t -> 'a -> unit
end

module Dictionary : Dictionary = struct
  type ('a, 'b) t =
    { mutable length: int
    ; buckets: ('a * 'b) list array
    ; hash: 'a -> int
    ; equal: 'a -> 'a -> bool }

  let num_buckets = 17
  let hash_bucket t key = t.hash key % num_buckets

  let create ~hash ~equal =
    {length= 0; buckets= Array.create ~len:num_buckets []; hash; equal}

  let length t = t.length

  let find t key =
    List.find_map
      t.buckets.(hash_bucket t key)
      ~f:(fun (key', data) -> if t.equal key' key then Some data else None)

  let bucket_has_key t i key =
    List.exists t.buckets.(i) ~f:(fun (key', _) -> t.equal key' key)

  let add t ~key ~data =
    let i = hash_bucket t key in
    let replace = bucket_has_key t i key in
    let filtered_bucket =
      if replace then
        List.filter t.buckets.(i) ~f:(fun (key', _) -> not (t.equal key' key))
      else t.buckets.(i) in
    t.buckets.(i) <- (key, data) :: filtered_bucket ;
    if not replace then t.length <- t.length + 1

  let remove t key =
    let i = hash_bucket t key in
    if bucket_has_key t i key then (
      let filtered_bucket =
        List.filter t.buckets.(i) ~f:(fun (key', _) -> not (t.equal key' key))
      in
      t.buckets.(i) <- filtered_bucket ;
      t.length <- t.length - 1 )

  let iter t ~f =
    for i = 0 to Array.length t.buckets - 1 do
      List.iter t.buckets.(i) ~f:(fun (key, data) -> f ~key ~data)
    done
end

module DList = struct
  type 'a element =
    {value: 'a; mutable next: 'a element option; mutable prev: 'a element option}

  type 'a t = 'a element option ref

  let create () = ref None
  let is_empty t = Option.is_none !t
  let value elt = elt.value
  let first t = !t
  let next elt = elt.next
  let prev elt = elt.prev

  let insert_first t value =
    let new_elt = {value; prev= None; next= !t} in
    ( match !t with
    | Some old_first -> old_first.prev <- Some new_elt
    | None -> () ) ;
    t := Some new_elt ;
    new_elt

  let insert_after elt value =
    let new_elt = {value; prev= Some elt; next= elt.next} in
    (match elt.next with Some next -> next.prev <- Some new_elt | None -> ()) ;
    elt.next <- Some new_elt ;
    new_elt

  let remove t elt =
    let {prev; next; _} = elt in
    (match prev with Some prev -> prev.next <- next | None -> ()) ;
    (match next with Some next -> next.prev <- prev | None -> ()) ;
    elt.prev <- None ;
    elt.next <- None

  let iter t ~f =
    let rec loop = function None -> () | Some elt -> f elt ; loop elt.next in
    loop !t

  let find_el t ~f =
    let rec loop = function
      | None -> None
      | Some elt -> if f elt then Some elt else loop elt.next in
    loop !t
end

let memoize m f =
  let memo_table = Hashtbl.create m in
  fun x -> Hashtbl.find_or_add memo_table x ~default:(fun () -> f x)

let rec edit_distance s t =
  match (String.length s, String.length t) with
  | 0, x | x, 0 -> x
  | len_s, len_t ->
      let s' = String.drop_suffix s 1 in
      let t' = String.drop_suffix t 1 in
      let cost_to_drop_both =
        if Char.( = ) s.[len_s - 1] t.[len_t - 1] then 0 else 1 in
      List.reduce_exn ~f:Int.min
        [ edit_distance s' t + 1; edit_distance s t' + 1
        ; edit_distance s' t' + cost_to_drop_both ]

let fib_norec fib i = if i <= 1 then i else fib (i - 1) + fib (i - 2)
let rec fib i = fib_norec fib i

let make_rec f_norec =
  let rec f x = f_norec f x in
  f

let memo_rec m f_norec x =
  let fref = ref (fun _ -> assert false) in
  let f = memoize m (fun x -> f_norec !fref x) in
  fref := f ;
  f x

module String_pair = struct
  type t = string * string [@@deriving sexp_of, hash, compare]
end

let edit_distance =
  memo_rec
    (module String_pair)
    (fun edit_distance (s, t) ->
      match (String.length s, String.length t) with
      | 0, x | x, 0 -> x
      | len_s, len_t ->
          let s' = String.drop_suffix s 1 in
          let t' = String.drop_suffix t 1 in
          let cost_to_drop_both =
            if Char.( = ) s.[len_s - 1] t.[len_t - 1] then 0 else 1 in
          List.reduce_exn ~f:Int.min
            [ edit_distance (s', t) + 1; edit_distance (s, t') + 1
            ; edit_distance (s', t') + cost_to_drop_both ])

let lazy_memo_rec m f_norec x =
  let rec f = lazy (memoize m (fun x -> f_norec (force f) x)) in
  (force f) x *)

open Core

(* let () =
  Out_channel.output_string stdout "Pick a timezone: " ;
  Out_channel.flush stdout ;
  match In_channel.(input_line stdin) with
  | None -> failwith "No timezone provided"
  | Some zone_string ->
      let zone = Time.Zone.find_exn zone_string in
      let time_string = Time.to_string_abs (Time.now ()) ~zone in
      Out_channel.output_string stdout
        (String.concat
           ["The time in"; Time.Zone.to_string zone; " is "; time_string; ".\n"]) ;
      Out_channel.flush stdout *)

let fmt : ('a, 'b, 'c) format = "%i is an integer \n"
let () = printf fmt 3

let create_number_file filename numbers =
  let outc = Out_channel.create filename in
  Exn.protect
    ~finally:(fun () -> Out_channel.close outc)
    ~f:(fun () ->
      List.iter numbers ~f:(fun x -> Out_channel.fprintf outc "%d\n" x))

let sum_file filename =
  let file = In_channel.create filename in
  let numbers = List.map ~f:Int.of_string (In_channel.input_lines file) in
  let sum = List.fold ~init:0 ~f:( + ) numbers in
  Exn.protect ~finally:(fun () -> In_channel.close file) ~f:(fun () -> sum)

let sum_file filename =
  In_channel.with_file filename ~f:(fun file ->
      let numbers =
        In_channel.fold_lines file ~init:[] ~f:(fun acc line ->
            Int.of_string line :: acc) in
      List.fold ~init:0 ~f:( + ) numbers)

let isok =
  let x = lazy (Float.sin 120.) in
  let y = lazy (Float.sin 75.) in
  let z = lazy (Float.sin 128.) in
  List.exists ~f:(fun x -> Float.O.(Lazy.force x < 0.)) [x; y; z]

let remember =
  let cache = ref None in
  fun x ->
    match !cache with
    | Some y -> y
    | None ->
        cache := Some x ;
        x

let a = remember 'a'
let b = remember 1
