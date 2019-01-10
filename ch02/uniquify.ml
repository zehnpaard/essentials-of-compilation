open Ast

let numbers = Hashtbl.create 100

let rec find env v = match env with
  | [] -> failwith "Variable not found in env"
  | (x, n) :: env' ->
      if x = v then x ^ string_of_int n
      else find env' v

let rec uniquify env e = match e with
  | Read | Int _ -> e
  | Neg e' -> Neg (uniquify env e')
  | Add (e1, e2) -> Add (uniquify env e1, uniquify env e2)
  | Var v -> Var (find env v)
  | Let (v, e', b) ->
      let e'' = uniquify env e' in
      let n = match Hashtbl.find_opt numbers v with
        | None -> (Hashtbl.add numbers v 1; 1)
        | Some m -> (Hashtbl.replace numbers v (m+1); m+1)
      in
      let env' = (v, n) :: env in
      Let (v ^ string_of_int n, e'', uniquify env' b)
