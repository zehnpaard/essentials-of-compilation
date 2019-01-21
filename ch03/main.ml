open Ast

let x = Let ("x", (Int 5), (Add (Let ("x", (Int 10), (Var "x")), Var "x")))
let astp = Program ([], x)
let astp2 = Uniquify.f astp
let astp3 = Decomplex.f astp2
let czp1 = Explicate.f astp3
let asmp1 = Select_instructions.f czp1
let asmp2 = Uncover_live.f asmp1
let asmp3 = Build_interference.f asmp2
let asmp4 = Color_graph.f asmp3
let asmp5 = Allocate_reg.f asmp4
let asmp6 = List_vars.f asmp5
let asmp7 = Assign_home.f asmp6
let asmp8 = Patch_instructions.f asmp7

let () = 
  begin
    print_endline (Asm.string_of_prog asmp4);
    print_endline (Asm.string_of_prog_block_info asmp4);
    print_endline (Asm.string_of_prog asmp8);
    print_endline (Asm.string_of_prog_block_info asmp8);
  end
