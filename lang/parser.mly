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
%token SEMICOLON
%token EOF

%left PLUS
%left TIMES
%left MINUS
%left DIV

%start <Ast.stm> prog

%%

prog:
  | s = stm; EOF { s }
  ;

stm:
  | f = ID; LPAREN; e = exp; RPAREN; SEMICOLON { FunctioncallStm (f, e) }
  ;

exp:
  | c = NUM { LitExp c }
  | e1 = exp; TIMES; e2 = exp { OpExp (e1, Times, e2) }
  | e1 = exp; MINUS; e2 = exp { OpExp (e1, Minus, e2) }
  | e1 = exp; PLUS;  e2 = exp  { OpExp (e1, Plus, e2) }
  | e1 = exp; DIV;   e2 = exp   { OpExp (e1, Div, e2) }
  ;
