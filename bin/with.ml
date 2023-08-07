module type Ring = sig
    type t (* abstract type *)
    val zero : t
    val one : t
    val ( + ) : t -> t -> t
    val ( * ) : t -> t -> t
    val ( ~- ) : t -> t (* additive inverse *)
    val to_string : t -> string
end
(* you would have to use pretty printing to string to pass the code under, use with. *)
(* if you didn't use pretty printer or with keyword in this case you wouldn't pass because of the abstraction *)
module IntRing = struct
    type t = int
    let zero = 0
    let one = 1
    let ( + ) = Stdlib.( + )
    let ( * ) = Stdlib.( * )
    let ( ~- ) = Stdlib.( ~- )
    let to_string = string_of_int
end
module _ : Ring = IntRing

(* use with *)
module type INT_RING = Ring with type t = int
