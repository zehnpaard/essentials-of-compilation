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

type prog = Program of string list * (string * tail) list

let string_of_arg = function
  | Int n -> string_of_int n
  | Var s -> s

let string_of_exp = function
  | Atom a -> string_of_arg a
  | Read -> "(read)"
  | Neg a -> "(- " ^ string_of_arg a ^ ")"
  | Add (a1, a2) -> string_of_arg a1 ^ " + " ^ string_of_arg a2

let string_of_stmt = function
  | Assign (a, e) -> "assign " ^ string_of_arg a ^ " " ^ string_of_exp e ^ "\n"

let rec string_of_tail = function
  | Return e -> "return " ^ string_of_exp e ^ "\n"
  | Seq (s, t) -> string_of_stmt s ^ string_of_tail t
