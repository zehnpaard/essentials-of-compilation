open Asm

let find_in_arg res = function
  | Int _ -> res
  | Reg _ -> res
  | Deref _ -> res
  | Var s -> s :: res

let find_in_instrs res = function
  | Retq | Callq _ -> res
  | Negq a | Pushq a | Popq a -> find_in_arg res a
  | Addq (a, b) | Subq (a, b) | Movq (a, b) ->
    let res' = find_in_arg res a in
    find_in_arg res' b

let find_in_nbs res (_, (Block (_, instrs))) =
  List.fold_left find_in_instrs res instrs

let find_locals nbs = List.fold_left find_in_nbs [] nbs

let f = function Program (_, nbs) ->
  Program (find_locals nbs, nbs)
