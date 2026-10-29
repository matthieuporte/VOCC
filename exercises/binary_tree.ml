type key = string
type 'a tree = LEAF | TREE of 'a tree * key * 'a * 'a tree

let rec insert key value = function
  | LEAF -> TREE (LEAF, key, value, LEAF)
  | TREE (l, k, v, r) ->
      if key < k then TREE (insert key value l, k,v, r)
      else if key > k then TREE (l, k,v, insert key value r)
      else TREE (l, key, value, r)

let mytree = insert "Matthieu" 42 (insert "goodbye" 35 (insert "hello" 50 LEAF))

let rec member key2 = function
  | LEAF -> false
  | TREE (l, k , _, r) ->
      if key2 < k then member key2 l
      else if key2 > k then member key2 r
      else true


let _ = Printf.printf "%B\n" (member "Matthieu" mytree)
let _ = Printf.printf "%B\n" (member "Mathis" mytree)


let rec lookup key2 = function
  | LEAF -> raise Not_found
  | TREE (l, k , v, r) ->
      if key2 < k then lookup key2 l
      else if key2 > k then lookup key2 r
      else v

let () =
  assert (lookup "hello" mytree = 50);
  assert (member "Mathis" mytree = false);
  (* remplacement d'une valeur existante *)
  let t2 = insert "hello" 99 mytree in
  assert (lookup "hello" t2 = 99);
  assert (lookup "hello" mytree = 50);  (* persistance : l'ancien arbre est intact *)
  (* clé absente *)
  assert (try ignore (lookup "nope" mytree); false with Not_found -> true)
