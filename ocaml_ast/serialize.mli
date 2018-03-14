module type Serializer = sig
    (** The type being serialized to. *)
    type t

    (** A print function for the type. *)
    val print_out : t -> string

    (** Serializes a primitive.
     *
     * The first argument is the type, for example "int".
     * The second argument is the value, for example "10". *)
    val serialize_prim : string -> string -> t

    (** Serializes a sequence. *)
    val serialize_seq : t list -> t

    (** Serializes a struct. *)
    val serialize_struct : string -> (string * t) list -> t

    (** Serializes a tuple. *)
    val serialize_tuple : string -> t list -> t
end
