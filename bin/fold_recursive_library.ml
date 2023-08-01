(*Consider writing a function lst_and: bool list -> bool, such that lst_and [a1; ...; an] returns
whether all elements of the list are true. That is, it evaluates the same as a1 && a2 && ... && an. When applied
to an empty list, it evaluates to true.*)

(* Recursive f *)
let rec lst_and_rec = function
    | [] -> true
    | h :: t -> h && lst_and_rec t;;

(* Fold f *)
let lst_and_fold =
    List.fold_left (fun acc elt -> acc && elt) true;;

(* Library f *)
let lst_and_lib =
    List.for_all (fun x -> x);;
