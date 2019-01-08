type reg =
  | Rsp
  | Rbp
  | Rax
  | Rbx
  | Rcx
  | Rdx
  | Rsi
  | Rdi
  | R8
  | R9
  | R10
  | R11
  | R12
  | R13
  | R14
  | R15

type arg =
  | Int of int
  | Reg of reg
  | Stack of int * reg

type instr =
  | Addq of arg * arg
  | Subq of arg * arg
  | Negq of arg
  | Movq of arg * arg
  | Callq of string
  | Pushq of arg
  | Popq of arg
  | Retq
  | Labeled of string * instr

type prog =
  | Program of instr list
