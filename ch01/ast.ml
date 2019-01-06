type t =
  | Int of int
  | Read
  | Neg of t
  | Add of t * t

type prog = Program t
