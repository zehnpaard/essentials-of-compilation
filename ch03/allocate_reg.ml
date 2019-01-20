open Asm

let usable_regs = [| Rbx; Rcx; Rdx; Rsi; Rdi; R8; R9; R10; R11; R12; R13; R14; R15 |]

let convert_arg colors a = match a with
  | Int _ -> a
  | Reg _ -> a
  | Deref _ -> a
  | Var _ ->
    let n = (match Hashtbl.find_opt colors a with
      | Some x -> x
      | None -> 0)
    in
    if n < Array.length usable_regs
    then Reg usable_regs.(n)
    else a

let rec allocate colors out = function
  | [] -> out
  | Retq as instr :: xs -> instr :: allocate colors out xs
  | Callq _ as instr :: xs  -> instr :: allocate colors out xs
  | Negq a :: xs -> Negq (convert_arg colors a) :: allocate colors out xs
  | Pushq a :: xs -> Pushq (convert_arg colors a) :: allocate colors out xs
  | Popq a :: xs -> Popq (convert_arg colors a) :: allocate colors out xs
  | Addq (a, b) :: xs ->
      Addq (convert_arg colors a, convert_arg colors b) :: allocate colors out xs
  | Subq (a, b) :: xs ->
      Subq (convert_arg colors a, convert_arg colors b) :: allocate colors out xs
  | Movq (a, b) :: xs ->
      Movq (convert_arg colors a, convert_arg colors b) :: allocate colors out xs

let allocate_regs info instrs =
  match info.colors with
  | Some colors -> allocate colors [] instrs
  | None -> failwith "Attempting register allocation without graph coloring"

let convert_block (label, (Block (info, instrs))) =
  (label, Block (info, allocate_regs info instrs))

let f = function Program (info, nbs) ->
  Program (info, List.map convert_block nbs)
