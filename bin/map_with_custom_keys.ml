type name = {first : string; last : string}
module Name = struct
    type t = name
    let compare { first = first1; last = last1 } { first = first2; last = last2 }
    =
        match String.compare last1 last2 with
        | 0 -> String.compare first1 first2
        | c -> c
end

module NameMap = Map.Make (Name)
(* Now we could use that map to associate names with birth years *)

let k1 = {last = "Kardashian"; first = "Kourtney"}
let k2 = {last = "Kardashian"; first = "Kimberly"}
let k3 = {last = "Kardashian"; first = "Khloe"}
let nm =
    NameMap.(empty |> add k1 1979 |> add k2 1980 |> add k3 1984)
let lst = NameMap.bindings nm
