open Base

module type Comparable = sig
  type t

  val compare : t -> t -> int
end

module type Interval_intf = sig
  type t
  type endpoint

  val create : endpoint -> endpoint -> t
  val is_empty : t -> bool
  val contains : t -> endpoint -> bool
  val intersect : t -> t -> t
end

module type Int_interval_intf = Interval_intf with type endpoint := int

module Make_interval (Endpoint : sig
  type t

  include Comparable with type t := t
  include Core_kernel.Sexpable with type t := t
end) : Interval_intf with type endpoint := Endpoint.t = struct
  type t = Interval of Endpoint.t * Endpoint.t | Empty [@@deriving sexp]

  let create low high =
    if Endpoint.compare low high > 0 then Empty else Interval (low, high)

  let is_empty = function Empty -> true | Interval _ -> false

  let contains t x =
    match t with
    | Empty -> false
    | Interval (low, high) ->
        Endpoint.compare low x <= 0 && Endpoint.compare x high <= 0

  let intersect t1 t2 =
    let min x y = if Endpoint.compare x y <= 0 then x else y in
    let max x y = if Endpoint.compare x y <= 0 then y else x in
    match (t1, t2) with
    | Empty, _ | _, Empty -> Empty
    | Interval (l1, h1), Interval (l2, h2) -> create (max l1 l2) (min h1 h2)
end

module Int_interval = Make_interval (Int)

type some_type = int * string list [@@deriving sexp]

module type Interval_intf_with_sexp = sig
  include Interval_intf
  include Core_kernel.Sexpable with type t := t
end

module Foldable = struct
  module type S = sig
    type 'a t

    val fold : 'a t -> init:'acc -> f:('acc -> 'a -> 'acc) -> 'acc
  end

  module type Extension = sig
    type 'a t

    val iter : 'a t -> f:('a -> unit) -> unit
    val length : 'a t -> int
    val count : 'a t -> f:('a -> bool) -> int
    val for_all : 'a t -> f:('a -> bool) -> bool
    val exists : 'a t -> f:('a -> bool) -> bool
  end

  module Extend (Arg : S) : Extension with type 'a t := 'a Arg.t = struct
    open Arg

    let iter t ~f = fold t ~init:() ~f:(fun () a -> f a)
    let length t = fold t ~init:0 ~f:(fun acc _ -> acc + 1)

    let count t ~f =
      fold t ~init:0 ~f:(fun count x -> count + if f x then 1 else 0)

    exception Short_circuit

    let for_all c ~f =
      try
        iter c ~f:(fun x -> if not (f x) then raise Short_circuit) ;
        true
      with Short_circuit -> false

    let exists c ~f =
      try
        iter c ~f:(fun x -> if f x then raise Short_circuit) ;
        false
      with Short_circuit -> true
  end
end

module Fqueue = struct
  type 'a t = 'a list * 'a list

  let empty = ([], [])
  let enqueue (in_list, out_list) x = (x :: in_list, out_list)

  let dequeue (in_list, out_list) =
    match out_list with
    | hd :: tl -> Some (hd, (in_list, tl))
    | [] -> (
      match List.rev in_list with [] -> None | hd :: tl -> Some (hd, ([], tl)) )

  let fold (in_list, out_list) ~init ~f =
    let after_out = List.fold ~init ~f out_list in
    List.fold_right ~init:after_out ~f:(fun x acc -> f acc x) in_list
end

module Fqueue = struct include Fqueue include Foldable.Extend (Fqueue) end

let em = Fqueue.empty
let aa = Fqueue.iter em ~f:(fun x -> ())
