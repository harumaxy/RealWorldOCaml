open Base

let digit_alist =
  [ (0, "zero"); (1, "one"); (2, "tow"); (3, "three"); (4, "four"); (5, "five")
  ; (6, "six"); (7, "seven"); (8, "eight"); (9, "nine") ]

let a = List.Assoc.find ~equal:Int.equal digit_alist 9

module type counter = sig
  type t

  val empty : t
  val touch : t -> string -> t
  val to_list : t -> (string * int) list
end

(* 
module Map = struct
  type t = (string, int, String.comparator_witness) Map.t

  let empty = Map.empty (module String)
  let to_list t = Map.to_alist t

  let touch t s =
    let count = match Map.find t s with Some x -> x | None -> 0 in
    Map.set t ~key:s ~data:(count + 1)
end *)

let s = Set.of_list (module Int) [1; 1; 2; 3; 4]
let digit_map = Map.of_alist_exn (module Int) digit_alist
let res = Map.find digit_map 3

module Book = struct
  module T = struct
    type t = {title: string; isbn: string}

    let compare t1 t2 =
      let cmp_title = String.compare t1.title t2.title in
      if cmp_title <> 0 then cmp_title else String.compare t1.isbn t2.isbn

    let sexp_of_t t : Sexp.t = List [Atom t.title; Atom t.isbn]
  end

  include T
  include Base.Comparator.Make (T)
end

let new_map = Map.empty (module Book)

let some_programming_books =
  Set.of_list
    (module Book)
    [ {title= "Real World OCaml"; isbn= "978-1449323912"}
    ; { title= "Structure and Interpretation of Computer Programs"
      ; isbn= "978-0262510875" }
    ; {title= "The C Programming Language"; isbn= "978-0131101630"} ]

let left = Map.of_alist_exn (module String) [("foo", 1); ("bar", 3); ("snoo", 0)]
let right = Map.of_alist_exn (module String) [("foo", 0); ("snoo", 0)]

(* Map.symmetric_diff ~data_equal:Int.equal left right |> Sequence.to_list *)

(* - : (string, int) Map.Symmetric_diff_element.t list =
[("bar", `Left 3); ("foo", `Unequal (1, 0))] *)

module Book = struct
  module T = struct
    type t = {title: string; isbn: string} [@@deriving compare, sexp_of]
  end

  include T
  include Comparator.Make (T)
end

let a = ref 1
let b = ref 1
