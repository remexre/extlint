(** A serializer, which corresponds to a format to serialize into. *)
module type S = sig
    (** The type being serialized to. *)
    type t

    (** A to-string function for the type. *)
    val string_of_t : t -> string

    (** Serializes a boolean. *)
    val serialize_bool : bool -> t

    (** Serializes an integer. *)
    val serialize_int : int -> t

    (** Serializes a null value. *)
    val serialize_null : t

    (** Serializes a string. *)
    val serialize_string : string -> t

    (** Serializes an option. *)
    val serialize_option : t option -> t

    (** Serializes a sequence. *)
    val serialize_seq : t list -> t

    (** Serializes a struct. *)
    val serialize_struct : string -> (string * t) list -> t

    (** Serializes a variant. *)
    val serialize_variant : string -> t -> t
end

(** Helper functions for serialization. *)
module S_ext : functor (S : S) -> sig
    (** A helper for building lists. *)
    val build_list : ('a -> S.t) -> 'a list -> S.t

    (** A helper for building options. *)
    val build_option : ('a -> S.t) -> 'a option -> S.t

    (** A helper for building pairs. *)
    val build_pair : ('a -> S.t) -> ('b -> S.t) -> ('a * 'b) -> S.t

    (** A helper for building triples. *)
    val build_triple : ('a -> S.t) -> ('b -> S.t) -> ('c -> S.t) -> ('a * 'b * 'c) -> S.t

    (** Serializes a character. *)
    val serialize_char : char -> S.t
end
