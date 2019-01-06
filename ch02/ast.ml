type t =
  | Int of int
  | Read
  | Neg of t
  | Add of t * t
  | Var of string
  | Let of string * t * t

type prog = Program of string list * t
