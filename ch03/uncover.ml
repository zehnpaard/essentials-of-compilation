open Czero

let rec find_locals' locals tail = match tail with
  | Return _ -> locals
  | Seq (Assign (Var s, _), tail') -> find_locals' (s :: locals) tail'
  | _ -> failwith "Assignment to non-variable found during uncover-locals"

let rec find_locals locals = function
  | [] -> locals
  | (_, tail) :: p -> find_locals (find_locals' locals tail) p

let uncover_locals = function
  | Program (_, p) -> Program (find_locals [] p, p)
