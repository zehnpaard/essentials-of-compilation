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
  | Neg a -> Printf.sprintf "(- %s)" (string_of_arg a)
  | Add (a1, a2) -> Printf.sprintf "%s + %s" (string_of_arg a1) (string_of_arg a2)

let string_of_stmt = function
  | Assign (a, e) -> Printf.sprintf "assign %s %s\n" (string_of_arg a) (string_of_exp e)

let rec string_of_tail = function
  | Return e -> Printf.sprintf "return %s\n" (string_of_exp e)
  | Seq (s, t) -> string_of_stmt s ^ string_of_tail t

let rec string_of_nts = function
  | [] -> ""
  | (label, tail) :: nts ->
      Printf.sprintf "%s\n%s\n" label (string_of_tail tail) ^ string_of_nts nts

let rec string_of_info' = function
  | [] -> ""
  | [s] -> s
  | s :: ss -> s ^ ", " ^ string_of_info' ss

let string_of_info info = Printf.sprintf "(%s)" (string_of_info' info)

let string_of_prog = function Program (info, named_tails) ->
  Printf.sprintf "Program:\n%s\n%s\n" (string_of_info info) (string_of_nts named_tails)
