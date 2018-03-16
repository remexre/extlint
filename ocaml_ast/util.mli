(** Function composition *)
val (%) : ('b -> 'c) -> ('a -> 'b) -> 'a -> 'c

(** Reverse function composition *)
val (%%) : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c

(** The identity function. *)
val id : 'a -> 'a

(** Returns whether, for all characters of the string after the given index,
 * the predicate holds true. *)
val str_all_from : (char -> bool) -> string -> int -> bool

(** Returns whether the predicate holds true for all characters in the string. *)
val str_all : (char -> bool) -> string -> bool

(** Sorts by the values returned from a function. *)
val sort_by_key : ('a -> 'b) -> 'a list -> 'a list

(** Composes several functions to combine their state transformation. *)
val comp_eff : ('a -> 'a) list -> ('a -> 'a)
