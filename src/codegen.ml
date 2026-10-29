module Ast = Lang.Ast

let counter = ref 0
let fresh () = incr counter; Printf.sprintf "%%t%d" !counter

let llvm_type = function
  | "int" -> "i32"
  | t -> failwith ("type inconnu : " ^ t)

let rec gen_exp buf = function
  | Ast.LitExp n -> string_of_int n
  | Ast.OpExp (a, op, b) ->
      let va = gen_exp buf a in
      let vb = gen_exp buf b in
      let r = fresh () in
      let instr = match op with
        | Ast.Plus -> "add" | Ast.Minus -> "sub"
        | Ast.Times -> "mul" | Ast.Div -> "sdiv" in
      Printf.bprintf buf "  %s = %s i32 %s, %s\n" r instr va vb;
      r

let rec gen_stm buf ret_ty = function
  | Ast.Return e ->
      let v = gen_exp buf e in
      Printf.bprintf buf "  ret %s %s\n" ret_ty v
  | Ast.FunctiondefStm (ty, name, body) ->
      let t = llvm_type ty in
      Printf.bprintf buf "define %s @%s() {\nentry:\n" t name;
      gen_stm buf t body;
      Buffer.add_string buf "}\n"


let gen_prog (p : Ast.prog) : string =
  counter := 0;
  let buf = Buffer.create 256 in
  gen_stm buf "i32" p;
  Buffer.contents buf
