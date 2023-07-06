(* Write a function that returns the product of all the elements in a list. The product of all the elements of an empty list is 1. *)
let rec prod = function
    | [] -> 1
    | h :: t -> h * prod t;;


(*Write a function that concatenates all the strings in a list. The concatenation of all the strings in an empty list is the empty
string "". *)
let rec conca = function
    | [] -> ""
    | h :: t -> h ^ conca t;;

(* Using pattern matching, write three functions, one for each of the following properties. Your functions should return
true if the input list has the property and false otherwise

• the list’s first element is "bigred"
• the list has exactly two or four elements; do not use the length function
• the first two elements of the list are equal *)

let rec bigred = function
    | [] -> false
    | h :: _ -> h = "bigred";;

let two_or_four = function
    | _ :: _ :: [] -> true
    | _ :: _ :: _ :: _ :: [] -> true
    | _ -> false;;

let first_two = function
    | a :: b :: _ -> a = b
    | _ -> false;;

(* • Write a function that takes an int list and returns the fifth element of that list, if such an element exists. If the
list has fewer than five elements, return 0. Hint: List.length and List.nth.
• Write a function that takes an int list and returns the list sorted in descending order. Hint: List.sort with
Stdlib.compare as its first argument, and List.rev. *)

let fifth_element lst =
    if (List.length lst) >= 5 then List.nth lst 4 else 0;;

let descending_sort lst =
    List.rev (List.sort Stdlib.compare lst);;

(*
• Write a function that returns the last element of a list. Your function may assume that the list is non-empty. Hint:
Use two library functions, and do not write any pattern matching code of your own.
• Write a function any_zeroes : int list -> bool that returns true if and only if the input list contains
at least one 0. Hint: use one library function, and do not write any pattern matching code of your own.
*)

let last_elem lst =
    List.nth lst (List.length lst -1);;

let any_zeroes lst =
    List.exists (fun x -> x = 0) lst;;

(*
• Write a function take : int -> 'a list -> 'a list such that take n lst returns the first n
elements of lst. If lst has fewer than n elements, return all of them.
• Write a function drop : int -> 'a list -> 'a list such that drop n lst returns all but the first
n elements of lst. If lst has fewer than n elements, return the empty list.
*)

let rec take n lst =
    if n = 0 then [] else match lst with
    | [] -> []
    | x :: xs -> x :: take (n - 1) xs;;
    
let rec drop n lst =
    if n = 0 then lst else match lst with
    | [] -> []
    | _ :: xs -> drop (n - 1) xs;;

(*
Revise your solutions for take and drop to be tail recursive, if they aren’t already. Test them on long lists with large
values of n to see whether they run out of stack space. To construct long lists, use the -- operator from the lists section.
*)

let rec take_rev n xs acc =
    if n = 0 then acc else match xs with
    | [] -> acc
    | x :: xs' -> take_rev (n - 1) xs' (x :: acc)

let take_tr n lst =
    take_rev n lst [] |> List.rev

let drop_tr = drop

(*
Write a function is_unimodal : int list -> bool that takes an integer list and returns whether that list is
unimodal. A unimodal list is a list that monotonically increases to some maximum value then monotonically decreases
after that value. Either or both segments (increasing or decreasing) may be empty. A constant list is unimodal, as is the
empty list.
*)

let rec is_mod_dec = function
    | [] | [_] -> true
    | h1 :: (h2 :: t2 as t) ->
            h1 >= h2 && is_mod_dec t 

let rec is_mod_inc_then_dec = function
    | [] | [_] -> true
    | h1 :: (h2 :: t2 as t) as lst ->
            if h1 <= h2 then is_mod_inc_then_dec lst else is_mod_dec lst

let is_unimodal lst =
    is_mod_inc_then_dec lst;;

(*
Write a function powerset : int list -> int list list that takes a set S represented as a list and returns
the set of all subsets of S. The order of subsets in the powerset and the order of elements in the subsets do not matter.
*)

let rec powerset = function
    | [] -> [ [] ]
    | x :: s -> let p = powerset s in
    List.map (List.cons x) p @ p

(*
Write a function print_int_list : int list -> unit that prints its input list, one number per line
*)

let rec print_int_list = function
    | [] -> ()
    | h :: t -> print_endline (string_of_int h); print_int_list t

(*
Write a function print_int_list' : int list -> unit whose specification is the same as
print_int_list. Do not use the keyword rec in your solution, but instead to use the List module function List.
iter.
*)

let print_int_list' lst =
    List.iter (fun x -> (print_endline (string_of_int x))) lst

(*
Assume the following type definition:
type student = {first_name : string; last_name : string; gpa : float}
Give OCaml expressions that have the following types:
• student
• student -> string * string (a function that extracts the student’s name)
• string -> string -> float -> student (a function that creates a student record)
*)

type student = {first_name : string; last_name : string; gpa : float}

let s =
    { first_name = "stu"; last_name = "dent"; gpa = 3.14 }

let get_full_name student =
    student.first_name, student.last_name

let stu_record first last g =
    { first_name = first; last_name = last; gpa = g }

(*
• Define the type pokemon to be a record with fields name (a string), hp (an integer), and ptype (a poketype).
• Create a record named charizard of type pokemon that represents a Pokémon with 78 HP and Fire type.
• Create a record named squirtle of type pokemon that represents a Pokémon with 44 HP and Water type.
*)

type poketype = Normal | Fire | Water

type pokemon =
    { name : string; hp : int; ptype : poketype }
    
let charizard  =
    { name = "charizard"; hp = 78; ptype = Fire }
    
let squirtle  =
    { name = "squirtle"; hp = 44; ptype = Water }

(*
Write a function safe_hd : 'a list -> 'a option that returns Some x if the head of the input list is x,
and None if the input list is empty.
Also write a function safe_tl : 'a list -> 'a list option that returns the tail of the list, or None if
the list is empty.
*)

let safe_hd = function
    | [] -> None
    | h :: _ -> Some h;;

let safe_tl = function
    | [] -> None
    | _ :: t -> Some t;;

(*
Write a function max_hp : pokemon list -> pokemon option that, given a list of pokemon, finds the
Pokémon with the highest HP.
*)

let rec max_hp = function
    | [] -> None
    | poke1 :: t -> begin
        match max_hp t with
        | None -> Some poke1
        | Some poke2 -> Some (if poke1.hp >= poke2.hp then poke1 else poke2)
    end

(*
Define a date-like triple to be a value of type int * int * int. Examples of date-like triples include (2013, 2,
1) and (0, 0, 1000). A date is a date-like triple whose first part is a positive year (i.e., a year in the common era),
second part is a month between 1 and 12, and third part is a day between 1 and 31 (or 30, 29, or 28, depending on the
month and year). (2013, 2, 1) is a date; (0, 0, 1000) is not.
*)
type date = int * int * int

let is_before date1 date2 =
    let (y1, m1, d1) = date1 in
    let (y2, m2, d2) = date2 in
    y1 < y2 || (y1 = y2 && m1 < m2) || (y1 = y2 && m1 = m2 && d1 < d2)

(*
Write a function earliest : (int*int*int) list -> (int * int * int) option. It evaluates to
None if the input list is empty, and to Some d if date d is the earliest date in the list. Hint: use is_before.
As in the previous exercise, your function needs to work correctly only for dates, not for arbitrary date-like triples.
*)

let rec earliest = function
    | [] -> None
    | d1 :: t -> begin
        match earliest t with
        | None -> Some d1
        | Some d2 -> Some (if is_before d1 d2 then d1 else d2)
    end

(* Use the functions insert and lookup from the section on association lists to construct an association list that maps the
integer 1 to the string “one”, 2 to “two”, and 3 to “three”. Lookup the key 2. Lookup the key 4.*)

let insert k v lst =
    (k, v) :: lst

let rec lookup k = function
    | [] -> None
    | (k', v) :: t -> if k = k' then Some v else lookup k t

let dict = insert 3 "three" (insert 2 "two" (insert 1 "one" []))
let lk_two = lookup 2 dict
let lk_four = lookup 4 dict

(*
• Define a variant type suit that represents the four suits, ♣ ♦ ♥ ♠, in a standard 52-card deck. All the constructors
of your type should be constant.
• Define a type rank that represents the possible ranks of a card: 2, 3, …, 10, Jack, Queen, King, or Ace. There
are many possible solutions; you are free to choose whatever works for you. One is to make rank be a synonym
of int, and to assume that Jack=11, Queen=12, King=13, and Ace=1 or 14. Another is to use variants.
• Define a type card that represents the suit and rank of a single card. Make it a record with two fields.
Define a few values of type card: the Ace of Clubs, the Queen of Hearts, the Two of Diamonds, the Seven of
Spades.
*)

type suit  = Clubs | Diamonds | Hearts | Spades

type rank = Number of int | Jack | Queen | King | Ace

type card = { suit: suit; rank: rank }

let ace_of_clubs : card = { suit = Clubs; rank = Ace }
let queen_of_hearts : card = { suit = Hearts; rank = Queen }
let two_of_diamonds : card = { suit = Diamonds; rank = Number 2 }
let seven_of_spades : card = { suit = Spades; rank = Number 7 }

(*
For each pattern in the list below, give a value of type int option list that does not match the pattern and is not
the empty list, or explain why that’s impossible.
• Some x :: tl
• [Some 3110; None]
• [Some x; _]
• h1 :: h2 :: tl
• h :: tl
*)

let lst1 : int option list = [None; Some 0]
let lst2 : int option list = [Some 3110; None]
let lst3 : int option list = [None; Some 0]
let lst4 : int option list = [Some 0]

(*
Complete the quadrant function below, which should return the quadrant of the given x, y point according to the
diagram on the right (borrowed from Wikipedia). Points that lie on an axis do not belong to any quandrant. Hints: (a)
define a helper function for the sign of an integer, (b) match against a pair.
*)

type quad = I | II | III | IV

type sign = Neg | Zero | Pos

let sign (x: int) : sign =
    if x = 0 then Zero
    else if x > 0 then Pos
    else Neg

let quadrant : int * int -> quad option = fun (x, y) ->
    match sign x, sign y with
    | Pos, Pos -> Some I
    | Neg, Pos -> Some II
    | Neg, Neg -> Some III
    | Pos, Neg -> Some IV
    | _ -> None

(*
Rewrite the quadrant function to use the when syntax. You won’t need your helper function from before.
*)

let quadrant_when : int * int -> quad option = function
    | x, y when x > 0 && y > 0 -> Some I
    | x, y when x < 0 && y > 0 -> Some II
    | x, y when x < 0 && y < 0 -> Some III
    | x, y when x > 0 && y < 0 -> Some IV
    | _ -> None

(*
Write a function depth : 'a tree -> int that returns the number of nodes in any longest path from the root to
a leaf. For example, the depth of an empty tree (simply Leaf) is 0, and the depth of tree t above is 3. Hint: there is a
library function max : 'a -> 'a -> 'a that returns the maximum of any two values of the same type.
*)


type 'a tree = Leaf | Node of 'a * 'a tree * 'a tree

let rec depth = function
    | Leaf -> 0
    | Node (_, left, right) -> 1 + max (depth left) (depth right)

(*
Write a function same_shape : 'a tree -> 'b tree -> bool that determines whether two trees have the
same shape, regardless of whether the values they carry at each node are the same. Hint: use a pattern match with three
branches, where the expression being matched is a pair of trees.
*)

let rec same_shape t1 t2 =
    match t1, t2 with
    | Leaf, Leaf -> true
    | Node (_, l1, r1), Node (_, l2, r2) -> (same_shape l1 l2) && (same_shape r1 r2)
    | _ -> false

(*
Write a function list_max : int list -> int that returns the maximum integer in a list, or raises Failure
"list_max" if the list is empty.
*)


let rec list_max_safe x = function
    | [] -> x
    | h :: t -> list_max_safe (Stdlib.max x h) t

    let list_max = function
    | [] -> failwith "list_max"
    | h :: t -> list_max_safe h t

(*
Write a function list_max_string : int list -> string that returns a string containing the maximum
integer in a list, or the string "empty" (note, not the exception Failure "empty" but just the string "empty") if
the list is empty. Hint: string_of_int in the standard library will do what its name suggests.
*)

let list_max_string lst =
    try string_of_int (list_max lst) with
    | Failure _ -> "empty"

(*
Write a function is_bst : ('a*'b) tree -> bool that returns true if and only if the given tree satisfies the
binary search tree invariant. An efficient version of this function that visits each node at most once is somewhat tricky to
write. Hint: write a recursive helper function that takes a tree and either gives you (i) the minimum and maximum value
in the tree, or (ii) tells you that the tree is empty, or (iii) tells you that the tree does not satisfy the invariant. Your is_bst
function will not be recursive, but will call your helper function and pattern match on the result. You will need to define a
new variant type for the return type of your helper function.
*)

type 'a tree =
    Node of 'a tree * 'a tree * 'a tree | Leaf;;

let rec is_bst x = function
    | Leaf -> Node (Leaf, x, Leaf) 
    | Node (l, v, r) as n ->
            if x < v then Node (is_bst x l, v, r)
            else if x > v then Node (l, v, is_bst x r)
            else n 

(*
   Modify your definition of quadrant to use polymorphic variants.
 *)

let poly_sign x : [> `Neg | `Pos | `Zero ] =
    if x < 0 then `Neg
    else if x = 0 then `Zero
    else `Pos 

let poly_quad (x, y) : [> `I | `II | `III | `IV ] option =
    match poly_sign x, poly_sign y with
    | `Pos, `Pos -> Some `I
    | `Neg, `Pos -> Some `II
    | `Neg, `Neg -> Some `III
    | `Pos, `Neg -> Some `IV
    | _ -> None
