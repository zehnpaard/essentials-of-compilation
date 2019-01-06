open Ast

let partial_eval_neg e = function
  | Int n -> Int (-n)
  | e -> Neg e

let partial_eval_add e1 e2 = match e1, e2 with
  | Int n1, Int n2 -> Int (n1 + n2)
  | e1, e2 -> Add (e1, e2)

let rec partial_eval = function
  | Int _ as e -> e
  | Read -> Read
  | Neg e -> partial_eval_neg (partial_eval e)
  | Add (e1, e2) -> partial_eval_add (partial_eval e1) (partial_eval e2)
