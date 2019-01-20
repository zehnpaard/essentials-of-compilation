open Ast

let x = Let ("x", (Int 5), (Add (Let ("x", (Int 10), (Var "x")), Var "x")))
let astp = Program ([], x)
let astp2 = Uniquify.f astp
let astp3 = Decomplex.f astp2
let czp1 = Explicate.f astp3
let czp2 = Uncover.uncover_locals czp1
let Czero.Program (info, _) = czp2
let asmp1 = Select_instructions.f czp2
let asmp2 = Uncover_live.f asmp1
let asmp2 = Build_interference.f asmp2
let asmp2 = Color_graph.f asmp2
let asmp2 = Allocate_reg.f asmp2
let asmp3 = Assign_home.f asmp2
let asmp4 = Patch_instructions.f asmp3

let () = 
  begin
    print_endline (string_of_prog astp);
    print_endline (string_of_prog astp2);
    print_endline (string_of_prog astp3);
    print_endline (Czero.string_of_prog czp1);
    print_endline (Czero.string_of_prog czp2);
    print_endline (Asm.string_of_prog asmp1);
    print_endline (Asm.string_of_prog asmp2);
    print_endline (Asm.string_of_prog asmp3);
    print_endline (Asm.string_of_prog asmp4);
  end
