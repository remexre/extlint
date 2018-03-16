(** Parses an AST. *)
val parse_ast : src:string -> path:string -> Parsetree.toplevel_phrase list

module Make : functor (S : Serialize.S) -> sig
    val serialize_lexer_error : Lexer.error -> S.t

    val serialize_syntax_error : Syntaxerr.error -> S.t

    (** Serializes an AST. *)
    val serialize_ast : Parsetree.toplevel_phrase list -> S.t

    (** Serializes the AST from a file. *)
    val serialize_ast_from : src:string -> path:string -> S.t
end
