open Base

let s =
  object
    val mutable v = [0; 2]

    method pop =
      match v with
      | hd :: tl ->
          v <- tl ;
          Some hd
      | [] -> None

    method push hd = v <- hd :: v
  end

let () =
  s#push 1 ;
  let _ = s#pop in
  ()

let stack init =
  object
    val mutable v = init

    method pop =
      match v with
      | hd :: tl ->
          v <- tl ;
          Some hd
      | [] -> None

    method push hd = v <- hd :: v
  end

let area sq = sq#width * sq#height
let close_area (sq : < width: int ; height: int >) = sq#width * sq#height

let imm_stack init =
  object
    val v = init

    method pop = match v with hd :: tl -> Some (hd, {<v = tl>}) | [] -> None

    method push hd = {<v = hd :: v>}
  end

let ano =
  object
    val v = 1

    method copy = {<v = 2>}
  end

type shape = < area: float >

let square w =
  object
    method area = Float.of_int (w * w)

    method width = w
  end

type num = [`Int of int | `Float of float]
type const = [num | `String of string]

module Either = struct
  type ('a, 'b) t = Left of 'a | Right of 'b

  let left x = Left x
  let right x = Right x
end

let left_square = Either.left (square 40)
