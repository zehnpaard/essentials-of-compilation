type arg =
  | Int of int
  | Var of string

type exp =
  | Atom of arg
  | Read
  | Neg of arg
  | Add of arg * arg

type stmt =
  | Assign of arg * exp

type tail =
  | Return of exp
  | Seq of stmt * tail

type program = Program of string list * (string * tail) list
