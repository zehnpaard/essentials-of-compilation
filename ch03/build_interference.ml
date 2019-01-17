open Asm

let g res l_after = function
  | Addq (_, Var d) | Subq  (_, Var d) | Negq (Var d) -> h_arith d l_after :: res
  | Callq _ -> h_callq l_after :: res
  | Movq (Var s, Var d) -> h_movq s d l_after :: res
  | Addq _ | Subq _ | Negq _ | Movq _ | Pushq _ | Popq _ | Retq -> res

let build (Block (lives, instrs)) =
  List.fold_left2 g [] (List.tl lives) instrs
