{
open Parser
open Lexing

exception SyntaxError of string
}

let digit = ['0'-'9']
let int = digit | ['1'-'9'] digit*

let var = ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9']*

let white = [' ''\t']+

rule read = parse
  | '(' { LPAREN }
  | ')' { RPAREN }
  | '[' { LSBRACE }
  | ']' { RSBRACE }
  | int { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | '+' { ADD }
  | '-' { SUB }
  | "read" { READ }
  | "let" { LET }
  | var { VAR }
  | white { read lexbuf }
  | _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
  | eof { EOF }
