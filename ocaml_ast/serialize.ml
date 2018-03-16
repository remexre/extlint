module type S = sig
    type t

    val string_of_t : t -> string
    val serialize_bool : bool -> t
    val serialize_int : int -> t
    val serialize_null : t
    val serialize_string : string -> t
    val serialize_option : t option -> t
    val serialize_seq : t list -> t
    val serialize_struct : string -> (string * t) list -> t
    val serialize_variant : string -> t -> t
end

module S_ext (S : S) = struct
    let build_list (f: 'a -> S.t) (l: 'a list) : S.t =
        S.serialize_seq (List.map f l)

    let build_option (f: 'a -> S.t) (o: 'a option) : S.t =
        S.serialize_option
            (match o with
            | Some(x) -> Some(f x)
            | None    -> None)

    let build_pair f g (x, y) =
        S.serialize_seq [f x; g y]

    let build_triple f g h (x, y, z) =
        S.serialize_seq [f x; g y; h z]

    let serialize_char c =
        S.serialize_string (String.make 1 c)
end
