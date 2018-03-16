open Core

type t =
| Prim of string * string
| Seq of t list
| Struct of string * t list
| Tuple of t list

let serialize_bool b = Prim("bool", string_of_bool b)
let serialize_int i = Prim("int", string_of_int i)
let serialize_null = Tuple([])
let serialize_string s = Prim("string", s)
let serialize_seq ts = Seq(ts)
let serialize_struct name vs = Struct(name, List.map vs snd)

let serialize_variant ty value = Struct(ty, [value])

let serialize_option = function
| Some(x) -> serialize_variant "Some" x
| None    -> serialize_variant "None" serialize_null

module Ast_builder = struct
    (** A map from strings to some other type. *)
    module StringMap = Map.Make(String)

    (** The type of the AST. *)
    type ast = string list list StringMap.t

    (** Converts an AST to a string. *)
    let string_of_ast ast =
        let datalog_char ch =
            let n = Char.to_int ch in
            if n >= 0x20 && n < 0x7f && not (ch = '\'' || ch = '\\') then
                String.make 1 ch
            else if n <= 0xffff then
                "\\u" ^ Printf.sprintf "%04x" n
            else
                "\\U" ^ Printf.sprintf "%08x" n
        in
        let string_of_term term =
            let symbol_start ch = failwith "TODO"
            and symbol_body ch = failwith "TODO"
            in
            if term = "" || not (symbol_start (String.get term 0))
                         || not (Util.str_all_from symbol_body term 1) then
                failwith "TODO string_of_term"
            else
                term
        in
        let string_of_fact ftor terms =
            failwith "TODO string_of_fact"
        in
        let string_of_facts ftor facts =
            let facts = Util.sort_by_key List.length facts
            in
            failwith "TODO string_of_facts"
        in
        String.concat ~sep:"\n" (failwith "TODO string_of_ast")

    (** The type of the AST builder. *)
    type ast_builder = {
        ast: ast;
        n: int;
    }

    (** The type of the fact builder. *)
    type fact_builder = {
        ftor: string;
        terms: string list;
    }

    (** Adds a term to a fact. *)
    let add_term (term: string) (fact: fact_builder) : fact_builder =
        { ftor = fact.ftor
        ; terms = term :: fact.terms
        }

    (** An empty AST. *)
    let empty : ast_builder =
        { ast = StringMap.empty
        ; n = 0
        }

    (** Adds data to the AST. *)
    let rec add_to_ast data builder =
        match data with
        | Prim(name, value) -> failwith "TODO add_to_ast Prim"
        | Seq(values) -> failwith "TODO add_to_ast Seq"
        | Struct(name, values) -> failwith "TODO add_to_ast Struct"
        | Tuple(values) -> failwith "TODO add_to_ast Tuple"

    (** Adds data to a fact. *)
    and add_to_fact data (ast, fact) =
        match data with
        | Prim(_, value) -> failwith "TODO add_to_fact Prim"
        | Tuple(values) -> failwith "TODO add_to_fact Tuple"
        | data ->
            let (ast, id) = add_to_ast data ast in
            let fact = add_term (string_of_int id) fact in
            (ast, fact)

    (** Creates an AST from a data value. *)
    let from_data data =
        let add_to_ast' data b = fst @@ add_to_ast data b in
        let f = match data with
        | Seq(ds) -> List.map ds add_to_ast' |> Util.comp_eff
        | data    -> add_to_ast' data
        in (f empty).ast
end

let string_of_t = Fn.compose Ast_builder.string_of_ast Ast_builder.from_data
