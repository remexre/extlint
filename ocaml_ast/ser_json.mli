type t =
| Array of t list
| Bool of bool
| Int of int
| Null
| Object of (string * t) list
| String of string

val string_of_t : t -> string
val serialize_bool : bool -> t
val serialize_int : int -> t
val serialize_null : t
val serialize_string : string -> t
val serialize_option : t option -> t
val serialize_seq : t list -> t
val serialize_struct : string -> (string * t) list -> t
val serialize_variant : string -> t -> t
