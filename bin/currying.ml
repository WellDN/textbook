(* curried f *)
let add x y = x + y;;   (* int -> int -> int makes it curried t1 -> t2 -> t3 *)

(* uncurried f *)
let add' t = fst t + snd t;; (* int * int -> int *)
(* or (they are both using tuple at the variable declaration (left side relatively to the equal) which make them uncurried)*)
let add'' (x, y) = x + y;; (* int * int -> int *)

(* High Order Function *)

let curry f x y = f (x, y)
(* val curry : ('a * 'b -> 'c) -> 'a -> 'b -> 'c = <fun> *)

let uncurry f (x, y) = f x y
(* val uncurry : ('a -> 'b -> 'c) -> 'a * 'b -> 'c = <fun> *)
