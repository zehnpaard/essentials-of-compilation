%token <int> INT
%token READ
%token ADD
%token SUB
%token LPAREN
%token RPAREN
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
  ;
