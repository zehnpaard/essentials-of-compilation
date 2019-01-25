open OUnit2

let instrs =
  [
    Asm.Movq (Int 10, Var "x");
    Asm.Movq (Int 10, Var "y");
    Asm.Movq (Var "y", Reg Rax);
    Asm.Movq (Var "x", Reg Rax);
    Retq
  ]

let uncovered = Uncover_live.uncover instrs

let interference = Build_interference.build uncovered instrs

let test1 = "test1" >::
    (fun _ ->
        assert_equal 2 (1 + 1);
        assert_equal 0 (2 - 2);
    )

let test_suite = "Fixture1" >:::
[
    test1;
]

let () =
  begin
    print_endline (Asm.string_of_live (Some uncovered));
    print_endline (Asm.string_of_interference (Some interference));
    run_test_tt_main test_suite;
  end
