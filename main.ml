open Base

(* 'a list -> f:('a -> bool) -> 'a option *)

(* let compute_bounds ~compare list =
  let sorted = List.sort ~compare list in
  match (List.hd sorted, List.last sorted) with
  | None, _ | _, None -> None
  | Some x, Some y -> Some (x, y) *)

(* こちらはエラーの概念がなく、キーを見つけられなかったとしても空のmismatchが帰るだけ *)
(* ただし、見つからなかった場合がエラーなのか正しいのか、戻り値からは判別しにくい　 *)
let find_mismatches table1 table2 =
  Hashtbl.fold table1 ~init:[] ~f:(fun ~key ~data mismatches ->
      match Hashtbl.find table2 key with
      | Some data' when data' <> data -> key :: mismatches
      | _ -> mismatches)

let custom_to_sexp = [%sexp_of: float * string list * int]

let e =
  Error.create "Something went terribly wrong"
    (3.5, [ "a"; "b"; "c" ], 6034)
    [%sexp_of: float * string list * int]

(* let bind option f =
  match option with 
  | None -> None 
  | Some x -> f x
   *)

(* bind >>= 演算子を使って書き直す、最大値と最小値の抽出 *)

(* let compute_bounds ~compare list =
  let open Option.Monad_infix in
  let sorted = List.sort ~compare list in
  List.hd sorted >>= fun first ->
  List.last sorted >>= fun last -> Some (first, last) *)

(* ラムダ式で、見つけた値を取る *)
(* どこかで失敗したらNoneが戻る *)
(* let compute_bounds ~compare list =
  let open Option.Monad_infix in
  let sorted = List.sort ~compare list in
  List.hd sorted >>= fun first ->
  List.last sorted >>= fun last -> Some (first, last) *)

(* 
let compute_bounds ~compare list =
  let open Option.Let_syntax in 
  let sorted = List.sort ~compare list in
  let%bind first = List.hd sorted in 
  let%bind last = List.last sorted in 
  Some (first, last) *)

let compute_bounds ~compare list =
  let sorted = List.sort ~compare list in
  Option.both (List.hd sorted) (List.last sorted)

exception Key_not_found of string

open Stdio

let parse_line line =
  String.split_on_chars ~on:[ ',' ] line |> List.map ~f:Float.of_string

let load filename =
  let inc = In_channel.create filename in
  let data = In_channel.input_lines inc |> List.map ~f:parse_line in
  In_channel.close inc;
  data List.Assoc.find List.Assoc.find_exn
