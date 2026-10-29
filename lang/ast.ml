(* ast.ml *)
type binop = Plus | Minus | Times | Div
type id = string

type exp = LitExp of int
         | OpExp of exp * binop * exp

type stm =
  | FunctiondefStm of id * id * stm
  | Return of exp

type prog = stm

(* type func = { name : string; body : stm list } *)

(* type program = func *)
