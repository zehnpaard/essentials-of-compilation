let rec convert_tail = function
  | Czero.Return e -> convert_exp (Asm.Reg Asm.Rax) e
  | Czero.Seq (s, t) -> convert_stmt s @ convert_tail t

let convert_stmt = function
  | Czero.Assign (Czero.Var s, e) -> convert_exp (Asm.Var s) e
  | _ -> failwith "Assignment to non-variable found during instruction selection"

let convert_exp v e = match v, e with
    | _, Czero.Read -> [Asm.Callq read_int; Asm.Movq (Asm.Reg Asm.Rax) v]
    | Asm.Var s1, Czero.Atom (Czero.Var s2) when s1 = s2 -> []
    | _, Czero.Atom a -> [Asm.Movq (convert_arg a) v]

let convert_arg = function
    | Czero.Int n -> Asm.Int n
    | Czero.Var s -> Asm.Var s
