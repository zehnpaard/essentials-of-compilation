let convert_arg = function
    | Czero.Int n -> Asm.Int n
    | Czero.Var s -> Asm.Var s

let convert_exp v e = match v, e with
    | _, Czero.Read -> [Asm.Callq "read_int"; Asm.Movq (Asm.Reg Asm.Rax, v)]
    | Asm.Var s1, Czero.Atom (Czero.Var s2) when s1 = s2 -> []
    | _, Czero.Atom a -> [Asm.Movq (convert_arg a, v)]
    | Asm.Var s1, Czero.Neg (Czero.Var s2) when s1 = s2 -> [Asm.Negq v]
    | _, Czero.Neg a -> [Asm.Movq (convert_arg a, v); Asm.Negq v]
    | Asm.Var s1, Czero.Add (Czero.Var s2, a) when s1 = s2 ->
        [Asm.Addq (convert_arg a, v)]
    | Asm.Var s1, Czero.Add (a, Czero.Var s2) when s1 = s2 ->
        [Asm.Addq (convert_arg a, v)]
    | _, Czero.Add (a1, a2) -> [Asm.Movq (convert_arg a1, v); Asm.Addq (convert_arg a2, v)]

let convert_stmt = function
  | Czero.Assign (Czero.Var s, e) -> convert_exp (Asm.Var s) e
  | _ -> failwith "Assignment to non-variable found during instruction selection"

let rec convert_tail = function
  | Czero.Return e -> convert_exp (Asm.Reg Asm.Rax) e
  | Czero.Seq (s, t) -> convert_stmt s @ convert_tail t
