open Asm

let convert_arg env a = match a with
  | Int _ -> a
  | Reg _ -> a
  | Deref _ -> a
  | Var s -> Deref (List.assoc s env, Rbp)

let rec assign_home' env out = function
  | [] -> out
  | Retq as instr :: xs -> instr :: assign_home' env out xs
  | Callq _ as instr :: xs  -> instr :: assign_home' env out xs
  | Negq a :: xs -> Negq (convert_arg env a) :: assign_home' env out xs
  | Pushq a :: xs -> Pushq (convert_arg env a) :: assign_home' env out xs
  | Popq a :: xs -> Popq (convert_arg env a) :: assign_home' env out xs
  | Addq (a, b) :: xs ->
      Addq (convert_arg env a, convert_arg env b) :: assign_home' env out xs
  | Subq (a, b) :: xs ->
      Subq (convert_arg env a, convert_arg env b) :: assign_home' env out xs
  | Movq (a, b) :: xs ->
      Movq (convert_arg env a, convert_arg env b) :: assign_home' env out xs

let assign_home locals instrs =
  let env = List.mapi (fun i v -> (v, - 8 * (i+1))) locals in
  assign_home' env [] instrs

let convert_block locals (label, (Block (info, instrs))) =
  (label, Block (info, assign_home locals instrs))

let f = function Program (info, nbs) ->
  Program (info, List.map (convert_block info) nbs)
