List.init 15 (fun n -> List.init n (Fun.const n))
(*
- : int list list =
[[]; [1]; [2; 2]; [3; 3; 3]; [4; 4; 4; 4]; [5; 5; 5; 5; 5];
[6; 6; 6; 6; 6; 6]; [7; 7; 7; 7; 7; 7; 7]; [8; 8; 8; 8; 8; 8; 8; 8];
[9; 9; 9; 9; 9; 9; 9; 9; 9]; [10; 10; 10; 10; 10; 10; 10; 10; 10; 10];
[11; 11; 11; 11; 11; 11; 11; 11; 11; 11; 11];
[12; 12; 12; 12; 12; 12; 12; 12; 12; 12; 12; 12];
[13; 13; 13; 13; 13; 13; 13; 13; 13; 13; 13; 13; 13];
[14; 14; 14; 14; 14; 14; 14; 14; 14; 14; 14; 14; 14; 14]]
*)

(* now adds " kupo" to each string while printing it *)
let kupo_pp fmt s = Format.fprintf fmt "%s kupo" s;;

(* lets add pretty printing to the ListStack *)

module type Stack = sig
    type 'a t
    exception Empty
    val empty : 'a t
    val is_empty : 'a t -> bool
    val push : 'a -> 'a t -> 'a t
    val peek : 'a t -> 'a
    val pop : 'a t -> 'a t
    val size : 'a t -> int
    val pp :
        (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
end

module ListStack : Stack = struct
    type 'a t = 'a list
    exception Empty
    let empty = []
    let is_empty = function [] -> true | _ -> false
    let push x s = x :: s
    let peek = function [] -> raise Empty | x :: _ -> x
    let pop = function [] -> raise Empty | _ :: s -> s
    let size = List.length
    let pp pp_val fmt s =
        let open Format in
        let pp_break fmt () = fprintf fmt "@," in
        fprintf fmt "@[<v 0>top of stack";
        if s <> [] then fprintf fmt "@,";
        pp_print_list ~pp_sep:pp_break pp_val fmt s;
        fprintf fmt "@,bottom of stack@]"
end

(*
#install_printer ListStack.pp
ListStack.empty
- : 'a ListStack.t = top of stack
bottom of stack
ListStack.(empty |> push 1 |> push 2)
- : int ListStack.t = top of stack
2
1
bottom of stack
*)
