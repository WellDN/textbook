module BatchedQueue : Queue = struct
    (** [{o; i}] represents the queue [o @ List.rev i]. For example,
    [{o = [1; 2]; i = [5; 4; 3]}] represents the queue [1, 2, 3, 4, 5],
    where [1] is the front element. To avoid ambiguity about emptiness,
    whenever only one of the lists is empty, it must be [i]. For example,
    [{o = [1]; i = []}] is a legal representation, but [{o = []; i = [1]}]
    is not. This implies that if [o] is empty, [i] must also be empty. *)
type 'a t = {o : 'a list; i : 'a list}
exception Empty
let empty = {o = []; i = []}
let is_empty = function
    | {o = []} -> true
    | _ -> false
let enqueue x = function
    | {o = []} -> {o = [x]; i = []}
    | {o; i} -> {o; i = x :: i}
let front = function
    | {o = []} -> raise Empty
    | {o = h :: _} -> h
let dequeue = function
    | {o = []} -> raise Empty
    | {o = [_]; i} -> {o = List.rev i; i = []}
    | {o = _ :: t; i} -> {o = t; i}
let size {o; i} = List.(length o + length i)
let to_list {o; i} = o @ List.rev i
end
