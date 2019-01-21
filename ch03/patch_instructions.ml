open Asm

let rec patch = function
  | [] -> []
  | Addq ((Deref _ as a), (Deref _ as b)) :: xs ->
      Movq (a, Reg Rax) :: (Addq (Reg Rax, b)) :: patch xs 
  | Subq ((Deref _ as a), (Deref _ as b)) :: xs ->
      Movq (a, Reg Rax) :: Subq (Reg Rax, b) :: patch xs 
  | Movq ((Deref _ as a), (Deref _ as b)) :: xs ->
      Movq (a, Reg Rax) :: Movq (Reg Rax, b) :: patch xs 
  | e :: xs -> e :: patch xs

let convert_block (label, (Block (info, instrs))) =
  (label, Block (info, patch instrs))

let f (Program (info, nbs)) =
  Program (info, List.map convert_block nbs)
