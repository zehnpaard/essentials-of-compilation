open Asm

let g instr live = match instr with
  | Addq (a1, a2) | Subq (a1, a2) | Movq (a1, a2) ->
    (match a1, a2 with
     | Var v1, Var v2 when v1 = v2 -> live
     | Var v1, Var v2 -> v1 :: List.filter ((=) v2) live
     | Var v, _ -> v :: live
     | _, Var v -> List.filter ((=) v) live
     | _, _ -> live)
  | Pushq (Var v) -> v :: live
  | Popq (Var v) -> List.filter ((=) v) live
  | Pushq _ | Popq _ | Negq _ | Callq _ | Retq -> live

let f lives rinstrs =
  let f' lives instr = g instr (List.hd lives) :: lives in
  List.fold_left f' lives rinstrs

let uncover instrs = f [[]] (List.rev instrs)
