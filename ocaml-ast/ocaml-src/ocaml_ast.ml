(* This module parses an AST and converts it to a JSON string, since I trust
 * shipping a string over two FFIs a lot more than a complex tree.
 *)

open Asttypes
open Longident
open Parsetree

(* The basic JSON type. *)
type json =
| Array of json list | Bool of bool | Int of int
| Null
| Object of (string * json) list
| String of string

(* Reverse composition. *)
let (%%) (f: 'a -> 'b) (g: 'b -> 'c) : 'a -> 'c =
    (fun x -> g (f x))

(* Places commas between strings. *)
let comma_sep : string list -> string =
    let or_empty = function
    | Some(s) -> s
    | None -> ""
    in
    let combine (a: string option) (b: string) : string option =
        match a with
        | Some(a) -> Some(a ^ "," ^ b)
        | None -> Some(b)
    in List.fold_left combine None %% or_empty

(* Escapes a string according to JSON rules. *)
let escape_string (s: string) : string =
    (* TODO *)
    "\"" ^ s ^ "\""

(* Converts the JSON to a string. *)
let rec string_of_json = function
| Array(a) -> "[" ^ comma_sep (List.map string_of_json a) ^ "]"
| Bool(b) -> string_of_bool b
| Int(i) -> string_of_int(i)
| Null -> "null"
| Object(o) -> let helper (n, v) = escape_string n ^ ":" ^ string_of_json v in
                   "{" ^ comma_sep (List.map helper o) ^ "}"
| String(s) -> escape_string s

let json_of_option f = function
| Some(x) -> f x
| None -> Null

let json_map f xs = Array(List.map f xs)

let json_of_string s = String(s)

let variant (ty: string) (value: json) : json =
    Object([
        ("type", String(ty));
        ("value", value);
    ])

let rec json_of_long_ident = function
| Lident(s) -> variant "Ident" @@ json_of_string s
| Ldot(i, s) -> variant "Dot" @@ Array([json_of_long_ident i; json_of_string s])
| Lapply(l, r) ->
        variant "Apply" @@ Array([
            json_of_long_ident l;
            json_of_long_ident r;
        ])

(*****************************************************************************)
(********************************** LOCATIONS ********************************)
(*****************************************************************************)

let json_of_position (pos: Lexing.position) : json =
    Object([
        ("filename", String(pos.pos_fname));
        ("line", Int(pos.pos_lnum));
        ("column", Int(pos.pos_cnum - pos.pos_bol));
        ("offset", Int(pos.pos_cnum));
    ])

let json_of_location (loc: Location.t) : json =
    Object([
        ("start", json_of_position loc.loc_start);
        ("end", json_of_position loc.loc_end);
        ("ghost", Bool(loc.loc_ghost));
    ])

let json_of_loc (f: 'a -> json) (x: 'a loc) : json =
    Object([
        ("txt", f x.txt);
        ("location", json_of_location x.loc);
    ])

(*****************************************************************************)
(********************************* PARSE TREE ********************************)
(*****************************************************************************)

let json_of_constant = function
| Pconst_integer(str, suffix) -> failwith "Pconst_integer"
| Pconst_char(c) -> failwith "Pconst_char"
| Pconst_string(str, suffix) ->
        variant "String" @@ Array([
            String(str);
            json_of_option json_of_string suffix;
        ])
| Pconst_float(str, suffix) -> failwith "Pconst_float"

(*****************************************************************************)
(****************************** EXTENSION POINTS *****************************)
(*****************************************************************************)

let json_of_attribute ((name, payload): attribute) : json =
    failwith "TODO json_of_attribute"

let json_of_attributes (attrs: attributes) : json =
    json_map json_of_attribute attrs

(* TODO json_of_extension *)

let json_of_payload = function
| PStr(s) -> failwith "PStr"
| PSig(s) -> failwith "PSig"
| PTyp(s) -> failwith "PTyp"
| PPat(p, e) -> failwith "PPat"

(*****************************************************************************)
(****************************** EXTENSION POINTS *****************************)
(*****************************************************************************)

(*****************************************************************************)
(******************************** CORE LANGUAGE ******************************)
(*****************************************************************************)

let rec json_of_pattern_desc = function
| Ppat_any -> variant "Any" Null
| Ppat_var(n) -> variant "Var" @@ json_of_loc json_of_string n
| Ppat_alias(p, n) ->
        variant "Alias" @@ Array([
            json_of_pattern p;
            json_of_loc json_of_string n;
        ])
| Ppat_constant(c) -> variant "Constant" @@ json_of_constant c
| Ppat_interval(f, t) ->
        variant "Interval" @@ Array([
            json_of_constant f;
            json_of_constant t;
        ])
| Ppat_tuple(ps) -> variant "Tuple" @@ json_map json_of_pattern ps
| Ppat_construct(n, p) ->
        variant "Construct" @@ Array([
            json_of_loc json_of_long_ident n;
            json_of_option json_of_pattern p;
        ])
| Ppat_variant(n, p) ->
        variant "Variant" @@ Array([
            json_of_string n;
            json_of_option json_of_pattern p;
        ])
(*
| Ppat_record of (Longident.t Asttypes.loc * pattern) list * Asttypes.closed_flag
*)
| Ppat_array(ps) -> variant "Array" @@ json_map json_of_pattern ps
| Ppat_or(l, r) ->
        variant "Or" @@ Array([
            json_of_pattern l;
            json_of_pattern r;
        ])
(*
| Ppat_constraint of pattern * core_type
| Ppat_type of Longident.t Asttypes.loc
*)
| Ppat_lazy(p) -> variant "Lazy" @@ json_of_pattern p
(*
| Ppat_unpack of string Asttypes.loc
*)
| Ppat_exception(p) -> variant "Exception" @@ json_of_pattern p
(*
| Ppat_extension of extension
*)
| Ppat_open(n, p) ->
        variant "Open" @@ Array([
            json_of_loc json_of_long_ident n;
            json_of_pattern p;
        ])

and json_of_pattern (pat: pattern) : json =
    Object([
        ("desc", json_of_pattern_desc pat.ppat_desc);
        ("location", json_of_location pat.ppat_loc);
        ("attributes", json_of_attributes pat.ppat_attributes);
    ])

let json_of_arg_label = function
| Nolabel -> variant "NoLabel" Null

let rec json_of_expression_desc = function
| Pexp_ident(i) -> variant "Ident" @@ json_of_loc json_of_long_ident i
| Pexp_constant(c) -> variant "Constant" @@ json_of_constant c
(*
| Pexp_let of Asttypes.rec_flag * value_binding list * expression
| Pexp_function of case list
| Pexp_fun of Asttypes.arg_label * expression option * pattern
* expression
*)
| Pexp_apply(f, a) ->
        let helper (a, e) = Array([json_of_arg_label a; json_of_expression e]) in
            variant "Apply" @@ Array([json_of_expression f; json_map helper a])
(*
| Pexp_match of expression * case list
| Pexp_try of expression * case list
| Pexp_tuple of expression list
| Pexp_construct of Longident.t Asttypes.loc * expression option
| Pexp_variant of Asttypes.label * expression option
| Pexp_record of (Longident.t Asttypes.loc * expression) list
* expression option
| Pexp_field of expression * Longident.t Asttypes.loc
| Pexp_setfield of expression * Longident.t Asttypes.loc * expression
| Pexp_array of expression list
| Pexp_ifthenelse of expression * expression * expression option
| Pexp_sequence of expression * expression
| Pexp_while of expression * expression
| Pexp_for of pattern * expression * expression
* Asttypes.direction_flag * expression
| Pexp_constraint of expression * core_type
| Pexp_coerce of expression * core_type option * core_type
| Pexp_send of expression * Asttypes.label Asttypes.loc
| Pexp_new of Longident.t Asttypes.loc
| Pexp_setinstvar of Asttypes.label Asttypes.loc * expression
| Pexp_override of (Asttypes.label Asttypes.loc * expression) list
| Pexp_letmodule of string Asttypes.loc * module_expr * expression
| Pexp_letexception of extension_constructor * expression
| Pexp_assert of expression
| Pexp_lazy of expression
| Pexp_poly of expression * core_type option
| Pexp_object of class_structure
| Pexp_newtype of string Asttypes.loc * expression
| Pexp_pack of module_expr
| Pexp_open of Asttypes.override_flag * Longident.t Asttypes.loc * expression
| Pexp_extension of extension
*)
| Pexp_unreachable -> variant "Unreachable" Null

and json_of_expression (exp: expression) : json =
    Object([
        ("desc", json_of_expression_desc exp.pexp_desc);
        ("location", json_of_location exp.pexp_loc);
        ("attributes", json_of_attributes exp.pexp_attributes);
    ])

(*****************************************************************************)
(******************************* CLASS LANGUAGE ******************************)
(*****************************************************************************)

(*****************************************************************************)
(******************************* MODULE LANGUAGE *****************************)
(*****************************************************************************)

let json_of_rec_flag = function
| Nonrecursive -> Bool(false)
| Recursive -> Bool(true)

let json_of_value_binding (b: value_binding) : json =
    Object([
        ("pat", json_of_pattern b.pvb_pat);
        ("expr", json_of_expression b.pvb_expr);
        ("attributes", json_of_attributes b.pvb_attributes);
        ("location", json_of_location b.pvb_loc);
    ])

let json_of_structure_item_desc = function
| Pstr_eval(e, a) -> failwith "Pstr_eval"
| Pstr_value(r, bs) -> variant "Value" @@ Array([
                           json_of_rec_flag r;
                           json_map json_of_value_binding bs;
                       ])
(*
| Pstr_primitive of value_description
| Pstr_type of Asttypes.rec_flag * type_declaration list
| Pstr_typext of type_extension
| Pstr_exception of extension_constructor
| Pstr_module of module_binding
| Pstr_recmodule of module_binding list
| Pstr_modtype of module_type_declaration
| Pstr_open of open_description
| Pstr_class of class_declaration list
| Pstr_class_type of class_type_declaration list
| Pstr_include of include_declaration
*)
| Pstr_attribute(a) -> variant "Attribute" @@ json_of_attribute a
(*
| Pstr_extension of extension * attributes
*)

let json_of_structure_item (si: structure_item) : json =
    Object([
        ("location", json_of_location si.pstr_loc);
        ("desc", json_of_structure_item_desc si.pstr_desc);
    ])

let json_of_structure (s: structure) : json =
    json_map json_of_structure_item s

(* TODO json_of_module_binding *)

(*****************************************************************************)
(********************************** TOPLEVEL **********************************)
(*****************************************************************************)

(* TODO json_of_directive_argument *)

let json_of_toplevel_phrase = function
| Ptop_def(s) -> variant "Def" @@ json_of_structure s
| Ptop_dir(n, a) -> failwith "Ptop_dir"

(*****************************************************************************)
(******************************* PARSE FUNCTION ******************************)
(*****************************************************************************)

let parse (src: string) (path: string): string =
    let buf = Lexing.from_string src in
    buf.lex_start_p <- { buf.lex_start_p with pos_fname = path };
    buf.lex_curr_p <- { buf.lex_curr_p with pos_fname = path };
    buf
    |> Parse.use_file
    |> json_map json_of_toplevel_phrase
    |> string_of_json

(* Register the parse function to be callable from C. *)
let () =
    Callback.register "parse" parse
