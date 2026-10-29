%{
open Ast
%}

%token <int> NUM
%token <string> ID
%token TIMES
%token PLUS
%token MINUS
%token DIV
%token LPAREN
%token RPAREN
%token LBRACE
%token RBRACE
%token LBRACKET
%token RBRACKET
%token SEMICOLON
%token EOF

/* keywords C */
%token RETURN

%left PLUS MINUS
%left TIMES DIV

%start <stm> prog

%%

prog:
  | s = stm; EOF { s }
  ;

stm:
/* function definition (no parameters yet TODO) */
  | ftype = ID; fn = ID; LPAREN;  RPAREN; LBRACE ; s = stm; RBRACE { FunctiondefStm (ftype, fn, s)}
  | RETURN ; e = exp ; SEMICOLON { Return e }
/* function call */
  /* | f = ID; LPAREN; e = exp; RPAREN; SEMICOLON { FunctioncallStm (f, e) } */
  ;

exp:
  | c = NUM { LitExp c }
  | e1 = exp; TIMES; e2 = exp { OpExp (e1, Times, e2) }
  | e1 = exp; MINUS; e2 = exp { OpExp (e1, Minus, e2) }
  | e1 = exp; PLUS;  e2 = exp  { OpExp (e1, Plus, e2) }
  | e1 = exp; DIV;   e2 = exp   { OpExp (e1, Div, e2) }
  ;
