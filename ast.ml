type t =
  | Int of int
  | Read
  | Add of t * t
  | Sub of t

type prog = Program t
