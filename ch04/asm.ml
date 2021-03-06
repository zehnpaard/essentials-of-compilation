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

type info = {live: (string list) list option;
             interference: (arg * arg) list option;
             colors: (arg, int) Hashtbl.t option}

let empty_info = {live=None; interference=None; colors=None}

type block =
  | Block of info * instr list

type prog =
  | Program of string list * (string * block) list

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
  | Deref (i, r) -> Printf.sprintf "%s(%s)" (string_of_int i) (string_of_reg r)
  | Var s -> s

let string_of_instr = function
  | Addq (a1, a2) -> Printf.sprintf "addq %s %s\n" (string_of_arg a1) (string_of_arg a2)
  | Subq (a1, a2) -> Printf.sprintf "subq %s %s\n" (string_of_arg a1) (string_of_arg a2)
  | Movq (a1, a2) -> Printf.sprintf "movq %s %s\n" (string_of_arg a1) (string_of_arg a2)
  | Negq a -> Printf.sprintf "negq %s\n" (string_of_arg a)
  | Pushq a -> Printf.sprintf "pushq %s\n" (string_of_arg a)
  | Popq a -> Printf.sprintf "popq %s\n" (string_of_arg a)
  | Callq f -> Printf.sprintf "callq %s\n" f
  | Retq -> "return\n"

let rec string_of_instrs = function
  | [] -> ""
  | x :: xs -> "\t" ^ string_of_instr x ^ string_of_instrs xs

let string_of_prog = function
  | Program (labels, [_, Block (_, instrs)]) ->
      let n = List.length labels in
      let s = string_of_int (8 * (n+1)) in
      "start:\n" ^ string_of_instrs instrs ^ "\tjmp conclusion\n\n\t.globl main\nmain:\n\tpushq\t%rbp\n\tmovq\t%rsp, %rbp\n\tsubq\t$" ^ s ^ ", %rsp\n\tjmp start\nconclusion:\n\taddq\t$" ^ s ^ ", %rsp\n\tpopq\t%rbp\n\tretq"
  | _ -> failwith "Unknown"

let string_of_live = function
  | None -> "()"
  | Some xss ->
    let f xs = Printf.sprintf "(%s)\n" (String.concat "," xs) in
    String.concat "" (List.map f xss)


let string_of_interference = function
  | None -> "()"
  | Some as' ->
    let f (a1, a2) = Printf.sprintf "%s, %s\n" (string_of_arg a1) (string_of_arg a2) in
    String.concat "" (List.map f as')

let string_of_colors = function
  | None -> "()"
  | Some as' ->
    let f (a, n) = Printf.sprintf "%s, %d\n" (string_of_arg a) n in
    Hashtbl.to_seq as'
    |> Seq.map f
    |> List.of_seq
    |> String.concat ""

let string_of_block_info info =
  let slive = string_of_live info.live in
  let sinterference = string_of_interference info.interference in
  let scolors = string_of_colors info.colors in
  Printf.sprintf "Live:\n%s\nInterference:\n%s\nColors:\n%s\n" slive sinterference scolors

let string_of_prog_block_info (Program (_, nbs)) =
  let f (_, Block (info, _)) = string_of_block_info info in
  String.concat "" (List.map f nbs)
