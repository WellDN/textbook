type 'a tree =
    | Leaf
    | Node of 'a * 'a tree * 'a tree
(* Map *)
let rec map_tree f = function
    | Leaf -> Leaf
    | Node (v, l, r) -> Node (f v, map_tree f l, map_tree f r)

(* Fold *)
type 'a mylist =
    | Nil
    | Cons of 'a * 'a mylist

let rec fold_right_mylist f acc = function
    | Nil -> acc
    | Cons (h, t) -> f h (fold_right_mylist f acc t)

(* Fold on tree f *)
let rec fold_tree_right f acc = function
    | Leaf -> acc
    | Node (v, l, r) -> f v (fold_tree_right f acc l) (fold_tree_right f acc r)
(* not fold left because fold left is recursive which it can't achieve on binary trees *)

(* Filter on trees f *)
let rec filter_tree p = function
    | Leaf -> Leaf
    | Node (v, l, r) -> 
            if p v then Node (v, filter_tree p l, filter_tree p r) else Leaf

