  let rec last = function
    | [] -> None
    | a :: [] -> Some a
    | a :: t -> last t in

let print_option printer = function
  | Some x ->
      print_string "Some ";
      printer x
  | None -> print_string "None" in

let rep = last ["a" ; "b" ;  "c"] in

print_option print_endline rep
