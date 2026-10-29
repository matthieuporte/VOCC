module Ast = Lang.Ast

let string_of_binop = function
  | Ast.Plus -> "+"
  | Ast.Minus -> "-"
  | Ast.Times -> "*"
  | Ast.Div -> "/"

let rec string_of_exp = function
  | Ast.LitExp n -> string_of_int n
  | Ast.OpExp (a, op, b) ->
      Printf.sprintf "(%s %s %s)" (string_of_binop op)
        (string_of_exp a) (string_of_exp b)

let rec string_of_stm = function
  | Ast.FunctiondefStm (ty, name, body) ->
      Printf.sprintf "function %s %s { %s }" ty name (string_of_stm body)
  | Ast.Return e -> "return " ^ string_of_exp e ^ ";"
