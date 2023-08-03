module MyModule = struct
    let inc x = x + 1
    type primary_color = Red | Green | Blue
    exception Oops
end

(* module for stacks implemented as linked lists *)

module ListStack = struct
    (** [empty] is the empty stack. *)
    let empty = []
    (** [is_empty s] is whether [s] is empty. *)
    let is_empty = function [] -> true | _ -> false
    (** [push x s] pushes [x] onto the top of [s]. *)
    let push x s = x :: s
    (** [Empty] is raised when an operation cannot be applied
    to an empty stack. *)
    exception Empty
    (** [peek s] is the top element of [s].
    Raises [Empty] if [s] is empty. *)
    let peek = function
        | [] -> raise Empty
    | x :: _ -> x
    (** [pop s] is all but the top element of [s].
    Raises [Empty] if [s] is empty. *)
    let pop = function
        | [] -> raise Empty
    | _ :: s -> s
end

(* open can be open a module inside another *)
module M = struct
    open List
    (** [uppercase_all lst] upper-cases all the elements of [lst]. *)
    let uppercase_all = map String.uppercase_ascii
end

(* Module Type Definitions *)
(* the comments specify the behavior *)
module type LIST_STACK = sig
    (** [Empty] is raised when an operation cannot be applied
    to an empty stack. *)
    exception Empty
    (** [empty] is the empty stack. *)
    val empty : 'a list
    (** [is_empty s] is whether [s] is empty. *)
    val is_empty : 'a list -> bool
    (** [push x s] pushes [x] onto the top of [s]. *)
    val push : 'a -> 'a list -> 'a list
    (** [peek s] is the top element of [s].
    Raises [Empty] if [s] is empty. *)
    val peek : 'a list -> 'a
    (** [pop s] is all but the top element of [s].
    Raises [Empty] if [s] is empty. *)
    val pop : 'a list -> 'a list
end

module ListStack = struct
    let empty = []
    let is_empty = function [] -> true | _ -> false
    let push x s = x :: s
    exception Empty
    let peek = function
        | [] -> raise Empty
        | x :: _ -> x
    let pop = function
        | [] -> raise Empty
        | _ :: s -> s
end

(* to specify type just add a type annotation *)
module ListStack : LIST_STACK = struct
    let empty = []
    let is_empty = function [] -> true | _ -> false
    let push x s = x :: s
    exception Empty
    let peek = function
        | [] -> raise Empty
        | x :: _ -> x
    let pop = function
        | [] -> raise Empty
        | _ :: s -> s
end

(* namespaces for modules and types are distinct *)
(*
module type ListStack = sig ... end
module ListStack : ListStack = struct ... end 
*)

(* nested module specifications *)
module type X = sig
    val x : int
end

module type T = sig
    module Inner : X
end

module M : T = struct
    module Inner : X = struct
        let x = 42
    end
end


