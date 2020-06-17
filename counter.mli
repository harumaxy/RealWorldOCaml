(* collection type *)
type t

val empty : t

val touch : t -> string -> t

val to_list : t -> (string * int) list

include module type of Option

val apply : ('a -> 'b) t -> 'a -> 'b t
