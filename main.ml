(* open Base

class istack =
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

let s = new istack

type 'a iterator = < get: 'a ; has_value: bool ; next: unit >

class ['a] list_iterator init =
  object
    val mutable current : 'a list = init

    method has_value = Poly.(current <> [])

    method get =
      match current with
      | hd :: _ -> hd
      | [] -> raise (Invalid_argument "no value")

    method next =
      match current with
      | _ :: tl -> current <- tl
      | [] -> raise (Invalid_argument "no value")
  end

class ['a] stack init =
  object
    val mutable v : 'a list = init

    method pop =
      match v with
      | hd :: tl ->
          v <- tl ;
          Some hd
      | [] -> None

    method push hd = v <- hd :: v

    method iter f = List.iter ~f v
  end

class double_stack init =
  object
    inherit [int] stack init as super

    method push hd = super#push (hd * 2)
  end

type 'a t = 'a stack

module Stack = struct
  class ['a] stack init =
    object
      val mutable v : 'a list = init

      method pop =
        match v with
        | hd :: tl ->
            v <- tl ;
            Some hd
        | [] -> None

      method push hd = v <- hd :: v
    end

  type 'a t = 'a stack

  let make init = new stack init
end

(* 
module VisibleStack : sig
  type 'a t = < pop: 'a option ; push: 'a -> unit >

  class ['a] stack :
    object
      val mutable v : 'a list

      method pop : 'a option

      method push : 'a -> unit
    end

  val make : 'a list -> 'a t
end =
  Stack *)

type doc =
  | Heading of string
  | Paragraph of text_item list
  | Definition of string list_item list

and text_item =
  | Raw of string
  | Bold of text_item list
  | Enumerate of int list_item list
  | Quote of doc

and 'a list_item = {tag: 'a; text: text_item list}

open Core

class ['a] folder =
  object (self)
    method doc acc =
      function
      | Heading _ -> acc
      | Paragraph text -> List.fold ~f:self#text_item ~init:acc text
      | Definition list -> List.fold ~f:self#list_item ~init:acc list

    method list_item : 'b. 'a -> 'b list_item -> 'a =
      fun acc {tag; text} -> List.fold ~f:self#text_item ~init:acc text

    method text_item acc =
      function
      | Raw _ -> acc
      | Bold text -> List.fold ~f:self#text_item ~init:acc text
      | Enumerate list -> List.fold ~f:self#list_item ~init:acc list
      | Quote doc -> self#doc acc doc
  end

class counter =
  object
    inherit [int] folder as super

    method list_item acc li = acc

    method text_item acc ti =
      let acc = super#text_item acc ti in
      match ti with Bold _ -> acc + 1 | _ -> acc
  end

let count_doc = (new counter)#doc *)

open Core
open Base
open Async
open Async_graphics

type drawable = < draw: unit >

class virtual shape x y =
  object (self)
    method virtual private contains : int -> int -> bool

    val mutable x : int = x

    val mutable y : int = y

    method x = x

    method y = y

    method on_click ?start ?stop f =
      on_click ?start ?stop (fun ev ->
          if self#contains ev.mouse_x ev.mouse_y then f ev.mouse_x ev.mouse_y)

    method on_mousedown ?start ?stop f =
      on_mousedown ?start ?stop (fun ev ->
          if self#contains ev.mouse_x ev.mouse_y then f ev.mouse_x ev.mouse_y)
  end

class virtual draggable =
  object (self)
    method virtual on_mousedown
        :    ?start:unit Deferred.t
          -> ?stop:unit Deferred.t
          -> (int -> int -> unit)
          -> unit

    val virtual mutable x : int

    val virtual mutable y : int

    val mutable dragging = false

    method dragging = dragging

    initializer
    self#on_mousedown (fun mouse_x mouse_y ->
        let offset_x = x - mouse_x in
        let offset_y = y - mouse_y in
        let mouse_up = Ivar.create () in
        let stop = Ivar.read mouse_up in
        dragging <- true ;
        on_mouseup ~stop (fun _ ->
            Ivar.fill mouse_up () ;
            dragging <- false) ;
        on_mousemove ~stop (fun ev ->
            x <- ev.mouse_x + offset_x ;
            y <- ev.mouse_y + offset_y))
  end

class virtual animated span =
  object (self)
    method virtual on_click
        :    ?start:unit Deferred.t
          -> ?stop:unit Deferred.t
          -> (int -> int -> unit)
          -> unit

    val mutable updates : (int -> unit) list = []

    val mutable step = 0

    val mutable running = false

    method running = running

    method animate =
      step <- 0 ;
      running <- true ;
      let stop = Clock.after span >>| fun () -> running <- false in
      Clock.every ~stop
        (Time.Span.of_sec (1.0 /. 24.0))
        (fun () ->
          step <- step + 1 ;
          List.iter ~f:(fun f -> f step) updates)

    initializer
    self#on_click (fun _x _y -> if not self#running then self#animate)
  end

class square w x y =
  object
    inherit shape x y

    val mutable width : int = w

    method width = width

    method draw = fill_rect x y width width

    method contains x' y' =
      x <= x' && x' <= x + width && y <= y' && y' <= y + width
  end

class circle r x y =
  object
    inherit shape x y

    val mutable radius = r

    method radius = radius

    method draw = fill_circle x y radius

    method private contains x' y' =
      let dx = abs (x' - x) in
      let dy = abs (y' - y) in
      let dist = Float.sqrt (Float.of_int ((dx * dx) + (dy * dy))) in
      Base.Float.O.(dist <= Float.of_int radius)
  end

class growing_circle r x y =
  object (self)
    inherit circle r x y

    initializer self#on_click (fun _x _y -> radius <- radius * 2)
  end

class virtual linear x' y' =
  object
    val virtual mutable updates : (int -> unit) list

    val virtual mutable x : int

    val virtual mutable y : int

    initializer
    let update _ =
      x <- x + x' ;
      y <- y + y' in
    updates <- update :: updates
  end

let pi = Float.atan 1.0 *. 4.0

class virtual harmonic offset x' y' =
  object
    val virtual mutable updates : (int -> unit) list

    val virtual mutable x : int

    val virtual mutable y : int

    initializer
    let update step =
      let m = Float.sin (offset +. (Float.of_int step *. (pi /. 64.))) in
      let x' = Float.to_int (m *. Float.of_int x') in
      let y' = Float.to_int (m *. Float.of_int y') in
      x <- x + x' ;
      y <- y + y' in
    updates <- update :: updates
  end

class my_square x y =
  object
    inherit square 40 x y

    inherit draggable

    inherit animated (Time.Span.of_int_sec 5)

    inherit linear 5 0

    inherit harmonic 0.0 7 ~-10
  end

let my_circle =
  object
    inherit circle 30 250 250

    inherit animated Time.Span.minute

    inherit harmonic 0.0 10 0

    inherit harmonic (pi /. 2.0) 0 10
  end

let main () =
  let shapes =
    [ (my_circle :> drawable); (new my_square 50 350 :> drawable)
    ; (new my_square 50 200 :> drawable)
    ; (new growing_circle 20 70 70 :> drawable) ] in
  let repaint () =
    clear_graph () ;
    List.iter ~f:(fun s -> s#draw) shapes ;
    synchronize () in
  open_graph "" ;
  auto_synchronize false ;
  Clock.every (Time.Span.of_sec (1.0 /. 24.0)) repaint

let () = never_returns (Scheduler.go_main ~main ())
