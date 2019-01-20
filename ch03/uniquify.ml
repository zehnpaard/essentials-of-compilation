open Ast

let rec find env v = match env with
  | [] -> failwith "Variable not found in env"
  | (x, n) :: env' ->
      if x = v then x ^ string_of_int n
      else find env' v

let rec uniquify numbers env e = match e with
  | Read | Int _ -> e
  | Neg e' -> Neg (uniquify numbers env e')
  | Add (e1, e2) -> Add (uniquify numbers env e1, uniquify numbers env e2)
  | Var v -> Var (find env v)
  | Let (v, e', b) ->
      let e'' = uniquify numbers env e' in
      let n = match Hashtbl.find_opt numbers v with
        | None -> (Hashtbl.add numbers v 1; 1)
        | Some m -> (Hashtbl.replace numbers v (m+1); m+1)
      in
      let env' = (v, n) :: env in
      Let (v ^ string_of_int n, e'', uniquify numbers env' b)

let f =
  let numbers = Hashtbl.create 100 in
  function Program (info, e) -> Program (info, uniquify numbers [] e)
