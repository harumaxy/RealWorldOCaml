open Core
open Async

(* 
module type Delayer_intf = sig
  type t

  val create : Time.Span.t -> t
  val schedule : t -> (unit -> 'a Deferred.t) -> 'a Deferred.t
end

module Delayer : Delayer_intf = struct
  type t = {delay: Time.Span.t; jobs: (unit -> unit) Queue.t}

  let create delay = {delay; jobs= Queue.create ()}

  let schedule t thunk =
    let ivar = Ivar.create () in
    Queue.enqueue t.jobs (fun () -> upon (thunk ()) (fun x -> Ivar.fill ivar x)) ;
    upon (after t.delay) (fun () ->
        let job = Queue.dequeue_exn t.jobs in
        job ()) ;
    Ivar.read ivar
end

let rec copy_blocks buffer r w =
  Reader.read r buffer
  >>= function
  | `Eof -> return ()
  | `Ok bytes_read ->
      Writer.write w (Bytes.to_string buffer) ~len:bytes_read ;
      Writer.flushed w >>= fun () -> copy_blocks buffer r w

let run ~uppercase ~port =
  let host_and_port =
    Async.Tcp.Server.create ~on_handler_error:`Raise
      (Tcp.Where_to_listen.of_port port) (fun _addr r w ->
        Pipe.transfer (Reader.pipe r) (Writer.pipe w)
          ~f:(if uppercase then String.uppercase else Fn.id)) in
  ignore (host_and_port : (Socket.Address.Inet.t, int) Tcp.Server.t Deferred.t) ;
  Deferred.never ()

let () =
  Command.async_spec ~summary:"Start an echo server"
    Command.Spec.(
      empty
      +> flag "-uppercase" no_arg
           ~doc:" Convert to uppercase before echoing back"
      +> flag "-port"
           (optional_with_default 8765 int)
           ~doc:" Port to listen on (default 8765)")
    (fun uppercase port () -> run ~uppercase ~port)
  |> Command.run

let r, w = Pipe.create ()
let write_complete = Pipe.write w "Hello, world"
let read_complete = Pipe.read r
let a = Fn.id

let query_uri query =
  let base_uri = Uri.of_string "http://api.duckduckgo.com/?format=json" in
  Uri.add_query_param base_uri ("q", [query])


let get_definition word =
  Cohttp_async.Client.get (query_uri word)
  >>= fun (_, body) ->
  Cohttp_async.Body.to_string body
  >>| fun body_text -> (word, get_definition_from_json body_text)

let print_result (word, definition) =
  printf "%s\n%s\n\n%s\n\n" word
    (String.init (String.length word) ~f:(fun _ -> '-'))
    ( match definition with
    | None -> "No definition found"
    | Some def -> String.concat ~sep:"\n" (Wrapper.wrap (Wrapper.make 70) def)
    )



let maybe_raise =
  let should_fail = ref false in
  fun () ->
    let will_fail = !should_fail in
    should_fail := not will_fail ;
    after (Time.Span.of_sec 0.5)
    >>= fun () -> if will_fail then raise Exit else return ()

let handle_error () =
  try_with (fun () -> maybe_raise ())
  >>| function Ok () -> "success" | Error _ -> "failure"

let blow_up () =
  let monitor = Monitor.create ~name:"blow up monitor" () in
  within' ~monitor maybe_raise

let swallow_error () =
  let monitor = Monitor.create () in
  Stream.iter (Monitor.detach_and_get_error_stream monitor) ~f:(fun _exn ->
      printf "an error happend\n") ;
  within' ~monitor (fun () -> after (Time.Span.of_sec 0.25))
  >>= fun () -> failwith "Kaboom!"

exception Ignore_me

let swallow_some_errors exn_to_raise =
  let child_monitor = Monitor.create () in
  let parent_monitor = Monitor.current () in
  Stream.iter (Monitor.detach_and_get_error_stream child_monitor)
    ~f:(fun error ->
      match Monitor.extract_exn error with
      | Ignore_me -> printf "ignoring exn\n"
      | _ -> Monitor.send_exn parent_monitor error) ;
  within' ~monitor:child_monitor (fun () ->
      after (Time.Span.of_sec 0.25) >>= fun () -> raise exn_to_raise) *)

let get_definition_from_json json_str =
  match Yojson.Safe.from_string json_str with
  | `Assoc kv_list -> (
      let find key =
        match List.Assoc.find ~equal:String.equal kv_list key with
        | None | Some (`String "") -> None
        | Some s -> Some (Yojson.Safe.to_string s) in
      match find "Abstract" with Some _ as x -> x | None -> find "Definition" )
  | _ -> None

let query_uri ~server query =
  let base_uri =
    Uri.of_string (String.concat ["https://"; server; "/?format=json"]) in
  Uri.add_query_param base_uri ("q", [query])

let get_definition ~server word =
  try_with (fun () ->
      Cohttp_async.Client.get (query_uri ~server word)
      >>= fun (_, body) ->
      Cohttp_async.Body.to_string body
      >>| fun string -> (word, get_definition_from_json string))
  >>| function
  | Ok (word, result) -> (word, Ok result)
  | Error _ -> (word, Error "Unexpected failure")

let print_result (word, definition) =
  printf "%s\n%s\n\n%s\n\n" word
    (String.init (String.length word) ~f:(fun _ -> '-'))
    ( match definition with
    | Error s -> "DuckDcukGo query failed: " ^ s
    | Ok None -> "No definition found"
    | Ok (Some def) ->
        String.concat ~sep:"\n" (Wrapper.wrap (Wrapper.make 70) def) )

let search_and_print words =
  Deferred.all_unit
    (List.map words ~f:(fun word ->
         get_definition ~server:"api.duckduckgo.com" word >>| print_result))

let () =
  Command.async_spec
    ~summary:"Retrieve definitions from duckduckgo search engine"
    Command.Spec.(empty +> anon (sequence ("word" %: string)))
    (fun words () -> search_and_print words)
  |> Command.run

(* let get_definition_with_timeout ~server ~timeout word =
  Deferred.any
    [ (after timeout >>| fun () -> (word, Error "Timed out"))
    ; ( get_definition ~server word
      >>| fun (word, result) ->
      let result' =
        match result with
        | Ok _ as x -> x
        | Error _ -> Error "Unexpected failure" in
      (word, result') ) ] *)

let get_definition ~server ~interrupt word =
  try_with (fun () ->
      Cohttp_async.Client.get ~interrupt (query_uri ~server word)
      >>= fun (_, body) ->
      Cohttp_async.Body.to_string body
      >>| fun string -> (word, get_definition_from_json string))
  >>| function
  | Ok (word, result) -> (word, Ok result)
  | Error _ -> (word, Error "Unexpected failure")

let get_definition_with_timeout ~server ~timeout word =
  get_definition ~server ~interrupt:(after timeout) word
  >>| fun (word, result) ->
  let result' =
    match result with Ok _ as x -> x | Error _ -> Error "Unexpected failure"
  in
  (word, result')

let get_definition_with_timeout ~server ~timeout word =
  let interrupt = Ivar.create () in
  choose
    [ choice (after timeout) (fun () ->
          Ivar.fill interrupt () ; (word, Error "Time out"))
    ; choice
        (get_definition ~server ~interrupt:(Ivar.read interrupt) word)
        (fun (word, result) ->
          let result' =
            match result with
            | Ok _ as x -> x
            | Error _ -> Error "Unexpected failure" in
          (word, result')) ]

let def = In_thread.run (fun () -> List.range 1 10)
