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
  | _ -> []


let assign_home locals instrs =
  let env = List.mapi (fun i v -> (v, - 8 * (i+1))) locals in
  assign_home' env [] instrs
