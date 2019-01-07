let rec interpret_exp env = function
  | Ast.Int _ as e -> e
  | Ast.Read -> int_of_string (read_line ())
  | Ast.Neg e -> - (interpret_exp env e)
  | Ast.Add (e1, e2) -> (interpret_exp env e1) + (interpret_exp env e2)

let interpret_r0 = function Ast.Program (_, e) -> interpret_exp [] e
