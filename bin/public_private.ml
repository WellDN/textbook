module type C_PUBLIC = sig
    val y : int
end

module CPrivate = struct
    let x = 0
    let y = 0
end

module C : C_PUBLIC = CPrivate (* C has access only to C_PUBLIC *)
