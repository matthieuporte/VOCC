(* Entry point of the program *)
module Lexer = Lang.Lexer
module Parser = Lang.Parser
module Ast = Lang.Ast

(* TODO comprendre  *)
let parse lexbuf =
  try Ok (Parser.prog Lexer.read lexbuf) with
  | Parser.Error ->
      Error (Printf.sprintf "erreur de syntaxe ligne %d"
               lexbuf.Lexing.lex_curr_p.Lexing.pos_lnum)
  | Failure msg -> Error msg


let run lexbuf =
  match parse lexbuf with
  (* | Ok ast -> print_endline (Pretty.string_of_stm ast) *)
  | Ok ast -> print_string (Codegen.gen_prog ast)
  | Error msg -> prerr_endline msg; exit 1

let () =
  match Sys.argv with
  | [| _; file |] ->
      In_channel.with_open_text file (fun ic -> run (Lexing.from_channel ic))
  | _ -> run (Lexing.from_channel stdin)

