open Czero

let convert_arg = function
  | Ast.Int n -> Int n
  | Ast.Var s -> Var s
  | _ -> failwith "Attempting to convert non-Int/Var to C0 arg"

let convert_exp e = match e with
  | Ast.Int _ | Ast.Var _ -> Atom (convert_arg e)
  | Ast.Read -> Read
  | Ast.Neg e' -> Neg (convert_arg e')
  | Ast.Add (e1, e2) -> Add (convert_arg e1, convert_arg e2)
  | Ast.Let _ -> failwith "Attempting to convert let to C0 exp"

let rec explicate_assign e s t = match e with
  | Ast.Int _ | Ast.Var _ | Ast.Read | Ast.Neg _ | Ast.Add _ ->
      Seq (Assign (Var s, convert_exp e), t)
  | Ast.Let (s', e', b) ->
      explicate_assign e' s' (explicate_assign b s t)

let rec explicate_tail e = match e with
  | Ast.Int _ | Ast.Var _ | Ast.Read | Ast.Neg _ | Ast.Add _ ->
      Return (convert_exp e)
  | Ast.Let (s, e', b) ->
      explicate_assign e' s (explicate_tail b)

let f = function Ast.Program (_, e) ->
  Program ([], [("", explicate_tail e)])
