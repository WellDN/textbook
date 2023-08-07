module type LIST_STACK = sig
    type 'a stack (* <- abstract type. used to hide the type 'a list * int while exposing the operation push 'a stack makes the type everywhere. *)
    exception Empty
    val empty : 'a stack
    val is_empty : 'a stack -> bool
    val push : 'a -> 'a stack -> 'a stack
    val peek : 'a stack -> 'a
    val pop : 'a stack -> 'a stack
    val size : 'a stack -> int
end

module type Stack = sig
    type 'a stack
    exception Empty
    val empty : 'a stack
    val is_empty : 'a stack -> bool
    val push : 'a -> 'a stack -> 'a stack
    val peek : 'a stack -> 'a
    val pop : 'a stack -> 'a stack
    val size : 'a stack -> int
end

module ListStack : Stack = struct
    type 'a stack = 'a list
    exception Empty
    let empty = []
    let is_empty = function [] -> true | _ -> false
    let push x s = x :: s
    let peek = function [] -> raise Empty | x :: _ -> x
    let pop = function [] -> raise Empty | _ :: s -> s
    let size = List.length
end

