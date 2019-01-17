open Asm

let rec h_arith d = function
  | [] -> []
  | v :: vs when v = d -> h_arith d vs
  | v :: vs -> (if v < d then (Var v, Var d) else (Var d, Var v)) :: h_arith d vs

let caller_regs = [Rax; Rdx; Rcx; Rsi; Rdi; R8; R9; R10; R11]

let rec h_callq = function
  | [] -> []
  | v :: vs -> List.map (fun r -> (Var v, Reg r)) caller_regs @ h_callq vs

let rec h_movq s d = function
  | [] -> []
  | v :: vs when (v = s || v = d) -> h_movq s d vs
  | v :: vs -> (if v < d then (Var v, Var d) else (Var d, Var v)) :: h_movq s d vs

let g res l_after = function
  | Addq (_, Var d) | Subq  (_, Var d) | Negq (Var d) -> h_arith d l_after :: res
  | Callq _ -> h_callq l_after :: res
  | Movq (Var s, Var d) -> h_movq s d l_after :: res
  | Addq _ | Subq _ | Negq _ | Movq _ | Pushq _ | Popq _ | Retq -> res

let build (Block (lives, instrs)) =
  List.fold_left2 g [] (List.tl lives) instrs
  |> List.sort_uniq compare
