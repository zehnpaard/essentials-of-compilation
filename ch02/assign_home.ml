open Asm

let rec assign_home' env out = function
    | [] -> out
    | Retq as instr :: xs -> instr :: assign_home' env out xs
    | Callq _ as instr :: xs  -> instr :: assign_home' env out xs
    | _ -> []


let assign_home locals instrs =
  let env = List.mapi (fun i v -> (v, - 8 * i)) locals in
  assign_home' env [] instrs
