%token <int> INT
%token <string> VAR
%token READ
%token LET
%token ADD
%token SUB
%token LPAREN
%token RPAREN
%token LSBRACE
%token RSBRACE
%token EOF

%start <Ast.prog> prog

prog:
  | e = exp; EOF { Ast.Program e }
  ;

exp:
  | n = INT { Ast.Int n }
  | LPAREN; READ; RPAREN { Ast.Read }
  | LPAREN; SUB; e = exp; RPAREN { Ast.Neg e }
  | LPAREN; ADD; e1 = exp; e2 = exp; RPAREN { Ast.Add (e1, e2) }
  | s = VAR { Ast.Var s }
  | LPAREN; LET; LPAREN; LSBRACE; s = VAR; e = exp; RSBRACE; RPAREN; b = exp; RPAREN { Ast.Let (s, e, b) }
  ;
