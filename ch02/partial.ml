open Ast

let rec partial_eval_neg = function
  | Int n        -> Int (-n)
  | Read         -> Neg Read
  | Neg Read     -> Read
  | Add (e1, e2) -> Add (partial_eval_neg e1, partial_eval_neg e2)
  | _ -> failwith "partial_eval_neg called on expression that is not partially evaluated"

let partial_eval_add e1 e2 = match e1, e2 with
  | Int n1, Int n2                                    -> Int (n1 + n2)
  | Int n1, Add (Int n2, e) | Add (Int n2, e), Int n1 -> Add (Int (n1 + n2), e)
  | Int n, e | e, Int n                               -> Add (Int n, e)
  | Add (Int n1, e1), Add (Int n2, e2)                -> Add (Int (n1 + n2), Add (e1, e2))
  | Add (Int n, e1), e2 | e2, Add (Int n, e1)         -> Add (Int n, Add (e1, e2))
  | e1, e2                                            -> Add (e1, e2)

let rec partial_eval = function
  | Int _ as e   -> e
  | Read         -> Read
  | Neg e        -> partial_eval_neg (partial_eval e)
  | Add (e1, e2) -> partial_eval_add (partial_eval e1) (partial_eval e2)
