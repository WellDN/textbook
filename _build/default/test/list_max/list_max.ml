let rec list_max_safe x = function
  | [] -> x
  | h::t -> list_max_safe (Stdlib.max x h) t

let list_max = function
  | [] -> failwith "list_max"
  | h::t -> list_max_safe h t

let list_max_string lst =
  try string_of_int (list_max lst) with
  | Failure _ -> "empty"
