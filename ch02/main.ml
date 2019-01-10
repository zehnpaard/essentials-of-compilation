open Ast

let x = Let ("x", (Int 5), (Add (Let ("x", (Int 10), (Var "x")), Var "x")))
let y = Uniquify.uniquify [] x
let z = Decomplex.decomplex_exp y
let a = Explicate.explicate_tail z
let () = 
  begin
    print_endline (string_of_t x);
    print_endline (string_of_int (Interpreter.interpret_exp [] x));
    print_endline (string_of_t y);
    print_endline (string_of_int (Interpreter.interpret_exp [] y));
    print_endline (string_of_t z);
    print_endline (string_of_int (Interpreter.interpret_exp [] z));
    print_endline (Czero.string_of_tail a);
  end
