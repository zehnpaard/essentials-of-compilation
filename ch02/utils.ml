open Ast

let is_leaf = function
  | Int _ -> true
  | Read -> true
  | Neg _ -> false
  | Add _ -> false

let rec is_exp = function
  | Int _ -> true
  | Read -> true
  | Neg e -> is_exp e
  | Add (e1, e2) -> (is_exp e1) && (is_exp e2)
  | _ -> false

let is_r0 = function Program e -> is_exp e
