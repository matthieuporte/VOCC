(* Entry point of the program *)
open Llvm
open Ast  (* assuming ast.ml above *)

let context = global_context ()
let the_module = create_module context "toy_module"
let builder = builder context
let i32_t = i32_type context

let rec codegen_expr builder = function
  | LitExp n -> const_int i32_t n

let rec codegen_stmt builder = function
  | FunctioncallStm (fname, arg) -> ignore (codegen_expr builder e)

let codegen_func ast =
  let ftype = function_type i32_t [||] in
  let func = define_function "main" ftype the_module in
  let bb = append_block context "entry" func in
  position_at_end bb builder;
  codegen_stmt builder ast;
  (* If no explicit return, add one *)
  if block_terminator (insertion_block builder) = None then
    ignore (build_ret (const_int i32_t 0) builder)

let parse (s : string) : stm =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

let () =
  (* Declare external functions (optional) *)
  let ftype = function_type i32_t [| i32_t |] in
  ignore (declare_function name ftype the_module);;

  (* Generate main *)
  let ast : int = parse "putchar(72);" in
  codegen_func ast;;

  (* Print LLVM IR *)
  dump_module the_module

