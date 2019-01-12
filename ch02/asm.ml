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
  | Deref of int * reg
  | Var of string

type instr =
  | Addq of arg * arg
  | Subq of arg * arg
  | Negq of arg
  | Movq of arg * arg
  | Callq of string
  | Pushq of arg
  | Popq of arg
  | Retq

type block =
  | Block of string * instr list

type prog =
  | Program of string * (string * block) list

let string_of_reg = function
  | Rsp -> "%rsp"
  | Rbp -> "%rbp"
  | Rax -> "%rax"
  | Rbx -> "%rbx"
  | Rcx -> "%rcx"
  | Rdx -> "%rdx"
  | Rsi -> "%rsi"
  | Rdi -> "%rdi"
  | R8 -> "%r8"
  | R9 -> "%r9"
  | R10 -> "%r10"
  | R11 -> "%r11"
  | R12 -> "%r12"
  | R13 -> "%r13"
  | R14 -> "%r14"
  | R15 -> "%r15"

let string_of_arg = function
  | Int n -> string_of_int n
  | Reg r -> string_of_reg r
  | Deref (i, r) -> string_of_int i ^ "(" ^ string_of_reg r ^ ")"
  | Var s -> s

let string_of_instr = function
  | Addq (a1, a2) -> "addq " ^ string_of_arg a1 ^ " " ^ string_of_arg a2 ^ "\n"
  | Subq (a1, a2) -> "subq " ^ string_of_arg a1 ^ " " ^ string_of_arg a2 ^ "\n"
  | Movq (a1, a2) -> "movq " ^ string_of_arg a1 ^ " " ^ string_of_arg a2 ^ "\n"
  | Negq a -> "negq " ^ string_of_arg a ^ "\n"
  | Pushq a -> "pushq " ^ string_of_arg a ^ "\n"
  | Popq a -> "popq " ^ string_of_arg a ^ "\n"
  | Callq f -> "callq " ^ f ^ "\n"
  | Retq -> "return\n"
