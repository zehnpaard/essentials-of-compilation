type t =
  | Int of int
  | Read
  | Neg of t
  | Add of t * t
  | Var of string
  | Let of string * t * t

type prog = Program of string list * t

let rec string_of_t = function
  | Read -> "(read)"
  | Int n -> string_of_int n
  | Var s -> s
  | Neg e -> Printf.sprintf "(- %s)" (string_of_t e)
  | Add (e1, e2) -> Printf.sprintf "(+ %s %s)" (string_of_t e1) (string_of_t e2)
  | Let (s, e', b) -> Printf.sprintf "(let [%s %s] %s)" s (string_of_t e') (string_of_t b)
