type binop = Plus | Minus | Times | Div

type id = string

type stm = FunctioncallStm of id * exp

 and exp = LitExp of char
            | OpExp of exp * binop * exp
