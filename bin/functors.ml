(* passing a module to another *)
module type X = sig
    val x : int
end
(* functor: (the parentheses are required. (M : X) ) *)
module IncX (M : X) = struct
    let x = M.x + 1
end

(* anonymous functor : *)
module F = functor (M : S) -> ...
