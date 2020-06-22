open Base
open Stdio

type basic_color =
  | Black
  | Red
  | Green
  | Yellow
  | Blue
  | Magenta
  | Cyan
  | White

;;
[ Blue; Magenta; Cyan ]

let basic_color_to_int = function
  | Black -> 0
  | Red -> 1
  | Green -> 2
  | Yellow -> 3
  | Blue -> 4
  | Magenta -> 5
  | Cyan -> 6
  | White -> 7

;;
List.map ~f:basic_color_to_int [ Blue; Magenta; Cyan ]

let color_by_number number text =
  Printf.sprintf "\027[38;5;%dm%s\027[0m" number text

let blue = color_by_number (basic_color_to_int Blue) "Blue"

type weight = Regular | Bold

type color =
  | Basic of basic_color * weight
  | RGB of int * int * int
  | Gray of int

;;
[ RGB (255, 0, 0); Basic (Green, Regular) ]

type 'a expr =
  | Base of 'a
  | Const of bool
  | And of 'a expr list
  | Or of 'a expr list
  | Not of 'a expr

type mail_field = To | From | CC | Date | Subject
type mail_predicate = { field : mail_field; contains : string }
