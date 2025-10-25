(* Entry point of the program *)
open Ast

let parse (s : string) : stm =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast
