type id = string

type binop = Plus | Minus | Times | Div

type stm = CompoundStm of stm * stm
             | AssignStm of id * exp
             | PrintStm of exp list

 and exp = IdExp of id
            | NumExp of int
            | OpExp of exp * binop * exp
            | EseqExp of stm * exp

let max_list l =
    let rec _h acc = function
        [] -> acc
      | h :: t -> _h (max acc h) t
    in
    _h 0 l

let rec maxargs (s : stm) : int =
  let rec maxexp (e : exp) : int =
    match e with
      OpExp (e1, _, e2) -> max (maxexp e1) (maxexp e2)
    | EseqExp (s, e)    -> max (maxargs s) (maxexp e)
    | _                 -> 0
    in
  match s with
    CompoundStm (s1, s2) -> max (maxargs s1) (maxargs s2)
  | AssignStm (_, e) -> maxexp e
  | PrintStm l -> max (max_list (List.map maxexp l)) (List.length l)



type table = (id * int) list

let update (t : table) (i : id) (x : int) : table = (i, x) :: t
let rec lookup (tbl : table) (i : id) : int =
  match tbl with
    [] -> 0
  | (j, x) :: t when j = i -> x
  | _ :: t -> lookup t i

let rec interpStm (s : stm) (tbl : table) : table =
  let process acc_table e =
    let x, t2 = interpExp e acc_table in
    Printf.printf "%d " x; t2 in
  match s with
  | CompoundStm (s1, s2) ->
     let tbl2 = interpStm s1 tbl in
     interpStm s2 tbl2
  | AssignStm (id, e) ->
     let x, tbl2 = interpExp e tbl in
     update tbl2 id x
  | PrintStm l -> let tbl2 = List.fold_left process tbl l in
                  Printf.printf "\n"; tbl2

and interpExp (e : exp) (tbl : table) : int * table =
  match e with
  | IdExp id -> (lookup tbl id), tbl
  | NumExp x -> x, tbl
  | OpExp (e1, op, e2) -> let x1, tbl2 = interpExp e1 tbl in
                          let x2, tbl3 = interpExp e2 tbl2 in
                          let r =
                            match op with
                            | Plus -> x1 + x2
                            | Minus -> x1 - x2
                            | Times -> x1 * x2
                            | Div -> x1 / x2 in
                          r, tbl3
  | EseqExp (s, e1) -> let tbl2 = interpStm s tbl in
                       interpExp e1 tbl2

(* the unit type is used when nothing has to be returned from the function. *)
let rec interp (s : stm) : unit = ignore (interpStm s [])

let prog =
  CompoundStm(AssignStm("a",OpExp(NumExp 5, Plus, NumExp 3)),
              CompoundStm(AssignStm("b",
                                    EseqExp(PrintStm[IdExp"a";OpExp(IdExp"a", Minus,NumExp 1)],
                                            OpExp(NumExp 10, Times, IdExp"a"))),
                          PrintStm[IdExp "b"]))


(* let m = maxargs prog *)

let a =  PrintStm[IdExp"a";IdExp"b";OpExp(IdExp"a", Minus,NumExp 1)]

(* let m2 = maxargs a *)

let _ = interp prog
