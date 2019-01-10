let rec interpret_exp = function
  | Ast.Int n -> n
  | Ast.Read -> int_of_string (read_line ())
  | Ast.Neg e -> - (interpret_exp e)
  | Ast.Add (e1, e2) -> (interpret_exp e1) + (interpret_exp e2)

let interpret_r0 = function Ast.Program e -> interpret_exp e
