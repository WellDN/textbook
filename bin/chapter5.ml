(*
Exercise: complex synonym [★]
Here is a module type for complex numbers, which have a real and imaginary component:
*)
module type ComplexSig = sig
    type t = float * float
    val zero : t
    val add : t -> t -> t
end
(*
Improve that code by adding type t = float * float. Show how the signature can be written more tersely
because of the type synonym.
*)

(* Exercise: complex encapsulation [★★]
Here is a module for the module type from the previous exercise: *)
module Complex : ComplexSig = struct
    type t = float * float
    let zero = (0., 0.)
    let add (r1, i1) (r2, i2) = r1 +. r2, i1 +. i2
end
(* Investigate what happens if you make the following changes (each independently), and explain why any errors arise:
• remove zero from the structure
when you remove val zero, the let zero its not gonna be typed.
• remove add from the signature
removing val add will make the let add not having time so its gonna be automatically infered.
• change zero in the structure to let zero = 0, 0 
without val zero the let zero its not gonna be read as float, but infered the type accordingly with the value to you declare.
*)

(*Exercise: big list queue [★★]
Use the following code to create ListQueue of exponentially increasing length: 10, 100, 1000, etc. How big of a queue
can you create before there is a noticeable delay? How big until there’s a delay of at least 10 seconds? (Note: you can
abort utop computations with Ctrl-C.) *)
(** Creates a ListQueue filled with [n] elements. *)
module ListQueue = struct 
    type 'a queue = 'a list

    let empty = []

    let is_empty q = (q = [])
    
    let enqueue x q = q @ [x]

    let peek = function
        | [] -> failwith "empty"
        | p :: _ -> p

    let dequeue = function
        | [] -> failwith "empty"
        | _ :: q -> q

end

let fill_listqueue n =
    let rec loop n q =
        if n = 0 then q
        else loop (n - 1) (ListQueue.enqueue n q) in
    loop n ListQueue.empty

(*
Exercise: big batched queue [★★]
Use the following function to create BatchedQueue of exponentially increasing length:
*)
module BatchedQueue = struct
    type 'a t = {outbox:'a list; inbox:'a list}

    let empty = {outbox=[]; inbox=[]}

    let is_empty = function
        | {outbox=[]; inbox=[]} -> true
    | _ -> false

    let norm = function
        | {outbox=[]; inbox} -> {outbox=List.rev inbox; inbox=[]}
        | q -> q

    let enqueue x q = norm {q with inbox=x::q.inbox}

    let peek = function
    | {outbox=[]; _} -> None
    | {outbox=x::_; _} -> Some x

    let dequeue = function
    | {outbox=[]; _} -> None
    | {outbox=_::xs; inbox} -> Some (norm {outbox=xs; inbox})
end

let fill_batchedqueue n =
    let rec loop n q =
        if n = 0 then q
        else loop (n - 1) (BatchedQueue.enqueue n q) in
    loop n BatchedQueue.empty

(* 
   Exercise: queue efficiency [★★★]
Compare the implementations of enqueue in ListQueue vs. BatchedQueue. Explain in your own words why
the efficiency of ListQueue.enqueue is linear time in the length of the queue. Hint: consider the @ operator. Then
explain why adding 𝑛 elements to the queue takes time that is quadratic in 𝑛.
Now consider BatchedQueue.enqueue. Suppose that the queue is in a state where it has never had any elements
dequeued. Explain in your own words why BatchedQueue.enqueue is constant time. Then explain why adding 𝑛
elements to the queue takes time that is linear in 𝑛.

appending the enqueue is walking through all the list until the tail. which makes it o(n²)

batched enqueue uses :: which is constant time which still O(n) irregardless the size o the list
 *)

(*
Exercise: binary search tree map [★★★★]
Write a module BstMap that implements the Map module type using a binary search tree type. Binary trees were covered
earlier when we discussed algebraic data types. A binary search tree (BST) is a binary tree that obeys the following BST
Invariant:
For any node n, every node in the left subtree of n has a value less than n’s value, and every node in the right
subtree of n has a value greater than n’s value.
Your nodes should store pairs of keys and values. The keys should be ordered by the BST Invariant. Based on that
invariant, you will always know whether to look left or right in a tree to find a particular key.
*)

module type Map = sig
    type ('k, 'v) t  

    val empty : ('k, 'v) t

    val insert : 'k -> 'v -> ('k, 'v) t -> ('k, 'v) t

    val lookup : 'k -> ('k, 'v) t -> 'v
end

module BstMap : Map = struct
    type 'a tree =
        | Leaf
        | Node of 'a * 'a tree * 'a tree
        
    type ('k, 'v) t = ('k * 'v) tree

    let empty =
        Leaf

let rec insert k v = function
    | Leaf -> Node((k, v), Leaf, Leaf)
    | Node ((k',v'), l, r) ->
      if (k = k') then Node ((k, v), l, r)
      else if (k < k') then Node ((k',v'), insert k v l, r)
      else Node ((k',v'), l, insert k v r)

  let rec lookup k = function
    | Leaf -> failwith "Not_found"
    | Node ((k',v'), l, r) ->
      if (k = k') then v'
      else if (k < k') then lookup k l
      else lookup k r
end

(* Exercise: fraction [★★★]
Write a module that implements the Fraction module type below: *)
module type Fraction = sig
    (* A fraction is a rational number p/q, where q != 0.*)
    type t
    (** [make n d] is n/d. Requires d != 0. *)
    val make : int -> int -> t
    val numerator : t -> int
    val denominator : t -> int
    val to_string : t -> string
    val to_float : t -> float
    val add : t -> t -> t
    val mul : t -> t -> t
end

module Impl : Fraction = struct
    type t = int * int

    let make n d = 
        assert (d != 0);
        (n, d)

    let numerator (n, _) = n
    let denominator (_, d) = d
      let to_string (n,d) =
          string_of_int n ^ " / " ^ string_of_int d
      let to_float (n,d) =
          float_of_int n /. float_of_int d
      let add (n1,d1) (n2,d2) =
          let d' = d1 * d2 in
          (n1 * d2 + n2 * d1, d')
      let mul (n1,d1) (n2,d2) =
          (n1 * n2, d1 * d2)
end

(*Exercise: make char map [★]
To create a standard library map, we first have to use the Map.Make functor to produce a module that is specialized for
the type of keys we want. Type the following in utop: 
# module CharMap = Map.Make(Char);;
The output tells you that a new module named CharMap has been defined, and it gives you a signature for it. Find the
values empty, add, and remove in that signature. Explain their types in your own words. 

empty: it maps the key of the character (key). ('a) is of type values and (t) is the abstract type

add: it maps the key of the character (key). ('a) is the value that you are gonna add and pass to the ('a, t) which is the value ('a) and the type (t) and return 'a, t.

remove: it maps the key of the character (key). ('a t) takes a existing value and passes to ('a t) that removes the precedent value.
*)


(*Exercise: char ordered [★]
The Map.Make functor requires its input module to match the Map.OrderedType signature. Look at that signature
as well as the signature for the Char module. Explain in your own words why we are allowed to pass Char as an argument
to Map.Make. 

because Map.OrderedType contains type t which can be assigned to any type including char
*)

(*
Exercise: use char map [★★]
Using the CharMap you just made, create a map that contains the following bindings:
• 'A' maps to "Alpha"
• 'E' maps to "Echo"
• 'S' maps to "Sierra"
• 'V' maps to "Victor"
Use CharMap.find to find the binding for 'E'.
Now remove the binding for 'A'. Use CharMap.mem to find whether 'A' is still bound.
Use the function CharMap.bindings to convert your map into an association list.
*)

module CharMap = Map.Make(Char)
let map = CharMap.(
    empty
    |> add 'A' "Alpha"
    |> add 'E' "Echo"
    |> add 'S' "Sierra"
    |> add 'V' "Victor"
)

let echo = CharMap.find 'E' map
let map' = CharMap.remove 'E' map
let a_exists = CharMap.mem 'S' map
let bindings = CharMap.bindings map'

(*
Exercise: bindings [★★]
Investigate the documentation of the Map.S signature to find the specification of bindings. Which of these expressions
will return the same association list?
1. CharMap.(empty |> add 'x' 0 |> add 'y' 1 |> bindings)
2. CharMap.(empty |> add 'y' 1 |> add 'x' 0 |> bindings)
3. CharMap.(empty |> add 'x' 2 |> add 'y' 1 |> remove 'x' |> add 'x' 0 |> bindings) 

all the same because they are all associated with the key.
*)

(*Exercise: date order [★★]
Here is a type for dates: *)
type date = {month : int; day : int}
(*For example, March 31st would be represented as {month = 3; day = 31}. Our goal in the next few exercises is
to implement a map whose keys have type date.
Obviously it’s possible to represent invalid dates with type date—for example, { month=6; day=50 } would be
June 50th, which is not a real date. The behavior of your code in the exercises below is unspecified for invalid dates.
To create a map over dates, we need a module that we can pass as input to Map.Make. That module will need to match
the Map.OrderedType signature. Create such a module. Here is some code to get you started: *)
module Date = struct
    type t = date
    let compare d1 d2 =
        if d1.month = d2.month then d1.day - d2.day
        else d1.month - d2.month
end

(*Recall the specification of compare in Map.OrderedType as you write your Date.compare function. *)


(*Exercise: calendar [★★]
Use the Map.Make functor with your Date module to create a DateMap module. Then define a calendar type as
follows: *)
module DateMap = Map.Make(Date)

type calendar = string DateMap.t

let my_calendar =
DateMap.(
    empty
    |> add { month = 3; day = 16 } "s day"
    |> add { month = 7; day = 7 } "e day"
    |> add { month = 9; day = 9 } "x day"
    |> add { month = 1; day = 14 } "o day"
    |> add { month = 6; day = 18 } "w day"
)

(*The idea is that calendar maps a date to the name of an event occurring on that date.
Using the functions in the DateMap module, create a calendar with a few entries in it, such as birthdays or anniversaries. *)

(* print calendar *)
let print_calendar cal =
    DateMap.iter
    (fun date event -> Printf.printf "%d%d: %s\n" date.month date.day event)
    cal

(* Exercise: is for [★★★]
Write a function is_for : string CharMap.t -> string CharMap.t that given an input map with
bindings from 𝑘1 to 𝑣1, …, 𝑘𝑛 to 𝑣𝑛, produces an output map with the same keys, but where each key 𝑘𝑖 is now bound to
the string “𝑘𝑖 is for 𝑣𝑖”. For example, if m maps 'a' to "apple", then is_for m would map 'a' to "a is for
apple". Hint: there is a one-line solution that uses a function from the Map.S signature. To convert a character to a
string, you could use String.make. An even fancier way would be to use Printf.sprintf. *)
let is_for m = 
    CharMap.mapi (fun key v -> Printf.sprintf "%c is for %s" key v) m


(* first after *)

let thd (_,_,x) = x
    
let first_after date cal =
    DateMap.(split date cal |> thd |> min_binding |> snd)

(* to string *)

module type ToString = sig
    type t
    val to_string : t -> string
end

(*
Exercise: Print [★★]
Write a functor Print that takes as input a module named M of type ToString. The module returned by your func-
tor should have exactly one value in it, print, which is a function that takes a value of type M.t and prints a string
representation of that value.
*)

module Print (M : ToString) = struct
    let print v = print_string (M.to_string v)
end

(********************************************************************
 * exercise: Print Int
 ********************************************************************)
module Int = struct
  type t = int
  let to_string = string_of_int
end

module PrintInt = Print(Int)
let _ = PrintInt.print 5

(********************************************************************
 * exercise: Print String
 ********************************************************************)

module MyString = struct
  type t = string
  let to_string s = s
end

module PrintString = Print(MyString)
let _ = PrintString.print "Harambe"


(********************************************************************
 * exercise: Print reuse
 ********************************************************************)

(* Functor [Print] wraps the details of printing for us, so each module
 * [M] only has to specify how to turn [M.t] into a string. Specifically,
 * the application of `print_string` has been factored out. That is
 * admittedly a tiny piece of code to factor out!  But if printing
 * required a lot more code to implement, we'd have felt good about this.
*)

(********************************************************************
 * exercise: sets
 ********************************************************************)

module CisSet = Set.Make(struct
    type t = string
    let compare s1 s2 =
      String.compare (String.lowercase_ascii s1) (String.lowercase_ascii s2)
  end)

let _ = CisSet.(equal (of_list ["grr"; "argh"]) (of_list ["GRR"; "aRgh"]))


(********************************************************************
 * exercise: Print String reuse revisited
 ********************************************************************)
module StringWithPrint = struct
  include String
  include Print(MyString)
end

(********************************************************************
 * exercise: printer for date
 ********************************************************************)

(* put this definition in date.ml:
     let format fmt d = Format.fprintf fmt "%s" (to_string d)
   now instead of printing <abstr> as the response to [make_date],
   utop will print the string representation of the date.
*)
