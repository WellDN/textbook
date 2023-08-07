module type Set = sig
    type 'a t
    val empty : 'a t
    val mem : 'a -> 'a t -> bool
    val add : 'a -> 'a t -> 'a t
    val elements : 'a t -> 'a list
end

module ListSet : Set = struct
    type 'a t = 'a list
    let empty = []
    let mem = List.mem
    let add = List.cons
    let elements s = List.sort_uniq Stdlib.compare s
end

module ListSetExtended = struct
    include ListSet (* avoids duplication/copy and include of_list properly in ListSet *)
    let of_list lst = List.fold_right add lst empty (* to include we have to fold to infer directly on the module whereas without it it wouldnt recognize *)
end
