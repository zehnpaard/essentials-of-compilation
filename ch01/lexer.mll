{
open Parser
open Lexing

exception SyntaxError of string
}

let digit = ['0'-'9']
let int = digit | ['1'-'9'] digit*

let white = [' ''\t']+

rule read = parse
  | '(' { LPAREN }
  | ')' { RPAREN }
  | int { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | '+' { ADD }
  | '-' { SUB }
  | "read" { READ }
  | white { read lexbuf }
  | _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
  | eof { EOF }
