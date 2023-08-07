(* to enable option with pepeline you use fmap *)
(* Option.map aka fmap *)
let ( >>| ) opt f =
match opt with
| None -> None
| Some x -> Some (f x)
(* Option.bind *)
let ( >>= ) opt f =
match opt with
| None -> None
| Some x -> f x

(*
val ( >>| ) : 'a option -> ('a -> 'b) -> 'b option = <fun>
val ( >>= ) : 'a option -> ('a -> 'b option) -> 'b option = <fun> 
*)
