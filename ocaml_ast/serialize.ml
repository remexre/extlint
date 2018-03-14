module type Serializer = sig
    type t

    val print_out : t -> string
    val serialize_prim : string -> string -> t
    val serialize_seq : t list -> t
    val serialize_struct : string -> (string * t) list -> t
    val serialize_tuple : string -> t list -> t
end
