(* This module parses an AST and converts it to a JSON string, since I trust
 * shipping a string over two FFIs a lot more than a complex tree.
 *)

open Asttypes
open Longident
open Parsetree

(* The basic JSON type. *)
type json =
| Array of json list
| Bool of bool
| Int of int
| Null
| Object of (string * json) list
| String of string

(* Composition. *)
let (%) (f: 'b -> 'c) (g: 'a -> 'b) : 'a -> 'c =
    (fun x -> f (g x))

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
    let escape_char (ch: char) : string =
    match Char.code ch with
    |  0 -> "\\u0000"
    |  1 -> "\\u0001"
    |  2 -> "\\u0002"
    |  3 -> "\\u0003"
    |  4 -> "\\u0004"
    |  5 -> "\\u0005"
    |  6 -> "\\u0006"
    |  7 -> "\\u0007"
    |  8 -> "\\b"
    |  9 -> "\\t"
    | 10 -> "\\n"
    | 11 -> "\\u000b"
    | 12 -> "\\f"
    | 13 -> "\\r"
    | 14 -> "\\u000e"
    | 15 -> "\\u000f"
    | 16 -> "\\u0010"
    | 17 -> "\\u0011"
    | 18 -> "\\u0012"
    | 19 -> "\\u0013"
    | 20 -> "\\u0014"
    | 21 -> "\\u0015"
    | 22 -> "\\u0016"
    | 23 -> "\\u0017"
    | 24 -> "\\u0018"
    | 25 -> "\\u0019"
    | 26 -> "\\u001a"
    | 27 -> "\\u001b"
    | 28 -> "\\u001c"
    | 29 -> "\\u001d"
    | 30 -> "\\u001e"
    | 31 -> "\\u001f"
    | 34 -> "\\\""
    | 92 -> "\\\\"
    | ch -> String.make 1 (Char.chr ch)
    in let explode (s: string) : char list =
      let rec expl i l =
        if i < 0 then l else
        expl (i - 1) (s.[i] :: l) in
      expl (String.length s - 1) []
    in "\"" ^ String.concat "" (List.map escape_char (explode s)) ^ "\""

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

let json_of_list f xs = Array(List.map f xs)
let json_of_pair f g (x, y) = Array([f x; g y])

let json_of_string s = String(s)
let json_of_char = json_of_string % String.make 1

let variant (ty: string) (value: json) : json =
    Object([
        ("type", String(ty));
        ("value", value);
    ])

let json_of_arg_label = function
| Nolabel -> variant "NoLabel" Null
| Labelled s -> variant "Labelled" @@ String(s)
| Optional s -> variant "Optional" @@ String(s)

let json_of_closed_flag = function
| Closed -> Bool(true)
| Open -> Bool(false)

let rec json_of_long_ident = function
| Lident(s) -> variant "Ident" @@ json_of_string s
| Ldot(i, s) -> variant "Dot" @@ Array([json_of_long_ident i; json_of_string s])
| Lapply(l, r) ->
        variant "Apply" @@ Array([
            json_of_long_ident l;
            json_of_long_ident r;
        ])

let json_of_variance = function
| Covariant -> String("Covariant")
| Contravariant -> String("Contravariant")
| Invariant -> String("Invariant")

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
| Pconst_integer(str, suffix) ->
        variant "Integer" @@ Array([
            String(str);
            json_of_option json_of_char suffix;
        ])
| Pconst_char(c) -> variant "Char" @@ String(String.make 1 c)
| Pconst_string(str, suffix) ->
        variant "String" @@ Array([
            String(str);
            json_of_option json_of_string suffix;
        ])
| Pconst_float(str, suffix) ->
        variant "Float" @@ Array([
            String(str);
            json_of_option json_of_char suffix;
        ])

(*****************************************************************************)
(****************************** EXTENSION POINTS *****************************)
(*****************************************************************************)

let rec json_of_payload = function
| PStr(s) -> variant "Structure" @@ failwith "PStr"
| PSig(s) -> variant "Signature" @@ failwith "PSig"
| PTyp(s) -> variant "Type" @@ json_of_core_type s
| PPat(p, e) -> variant "Pattern" @@ Array([
                    json_of_pattern p;
                    json_of_option json_of_expression e;
                ])

and json_of_attribute (attr: attribute) =
    json_of_pair (json_of_loc json_of_string) json_of_payload attr

and json_of_attributes (attrs: attributes) : json =
    json_of_list json_of_attribute attrs

(* TODO json_of_extension *)

and json_of_override_flag = function
| Override -> Bool(true)
| Fresh -> Bool(false)

(*****************************************************************************)
(******************************** CORE LANGUAGE ******************************)
(*****************************************************************************)

and json_of_core_type (ct: core_type) : json =
    Object([
        ("desc", json_of_core_type_desc ct.ptyp_desc);
        ("location", json_of_location ct.ptyp_loc);
        ("attributes", json_of_attributes ct.ptyp_attributes);
    ])

and json_of_core_type_desc = function
| Ptyp_any -> variant "Any" Null
| Ptyp_var(s) -> variant "Var" @@ json_of_string s
| Ptyp_arrow(a, l, r) -> variant "Arrow" @@ Array([
                                                json_of_arg_label a;
                                                json_of_core_type l;
                                                json_of_core_type r;
                                            ])
| Ptyp_tuple(ts) -> variant "Tuple" @@ json_of_list json_of_core_type ts
| Ptyp_constr(a, ts) -> variant "Constr" @@ Array([
                                                json_of_loc json_of_long_ident a;
                                                json_of_list json_of_core_type ts;
                                            ])
| Ptyp_object(fs, c) -> failwith "TODO Object"
| Ptyp_class(l, ts) -> failwith "TODO Class"
| Ptyp_alias(t, s) -> failwith "TODO Alias"
| Ptyp_variant(vs, c, ls) -> failwith "TODO Variant"
| Ptyp_poly(vs, t) -> variant "Poly" @@ Array([
                                            json_of_list (json_of_loc json_of_string) vs;
                                            json_of_core_type t;
                                        ])
| Ptyp_package(e) -> failwith "TODO Package"
| Ptyp_extension(e) -> failwith "TODO Extension"

and json_of_pattern_desc = function
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
| Ppat_tuple(ps) -> variant "Tuple" @@ json_of_list json_of_pattern ps
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
| Ppat_record(ms, closed) ->
    variant "Record" @@ Array([
        json_of_list (json_of_pair (json_of_loc json_of_long_ident) json_of_pattern) ms;
        json_of_closed_flag closed;
    ])
| Ppat_array(ps) -> variant "Array" @@ json_of_list json_of_pattern ps
| Ppat_or(l, r) ->
        variant "Or" @@ Array([
            json_of_pattern l;
            json_of_pattern r;
        ])
| Ppat_constraint(p, t) -> variant "Constraint" @@ Array([
                                                       json_of_pattern p;
                                                       json_of_core_type t;
                                                   ])
| Ppat_type(t) -> variant "Type" @@ json_of_loc json_of_long_ident t
| Ppat_lazy(p) -> variant "Lazy" @@ json_of_pattern p
| Ppat_unpack(s) -> variant "Unpack" @@ json_of_loc json_of_string s
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

and json_of_mutable_flag = function
| Immutable -> Bool(false)
| Mutable -> Bool(true)

and json_of_label_decl (decl: label_declaration) =
    Object([
        ("name", json_of_loc json_of_string decl.pld_name);
        ("mutable", json_of_mutable_flag decl.pld_mutable);
        ("type", json_of_core_type decl.pld_type);
        ("location", json_of_location decl.pld_loc);
        ("attributes", json_of_attributes decl.pld_attributes);
    ])

and json_of_ctor_args = function
| Pcstr_tuple(tys) -> variant "Tuple" @@ json_of_list json_of_core_type tys
| Pcstr_record(ls) -> variant "Record" @@ json_of_list json_of_label_decl ls

and json_of_extension_constructor_kind = function
| Pext_decl(args, ty) -> variant "Decl" @@ Array([
                                               json_of_ctor_args args;
                                               json_of_option json_of_core_type ty;
                                           ])
| Pext_rebind(id) -> variant "Rebind" @@ json_of_loc json_of_long_ident id

and json_of_extension_constructor (ctor: extension_constructor) =
    Object([
        ("name", json_of_loc json_of_string ctor.pext_name);
        ("kind", json_of_extension_constructor_kind ctor.pext_kind);
        ("location", json_of_location ctor.pext_loc);
        ("attributes", json_of_attributes ctor.pext_attributes);
    ])

and json_of_rec_flag = function
| Nonrecursive -> Bool(false)
| Recursive -> Bool(true)

and json_of_case (c: case) : json =
    Object([
        ("pat", json_of_pattern c.pc_lhs);
        ("guard", json_of_option json_of_expression c.pc_guard);
        ("expr", json_of_expression c.pc_rhs);
    ])

and json_of_expression_desc = function
| Pexp_ident(i) -> variant "Ident" @@ json_of_loc json_of_long_ident i
| Pexp_constant(c) -> variant "Constant" @@ json_of_constant c
| Pexp_let(r, vs, e) -> variant "Let" @@ Array([
                                             json_of_rec_flag r;
                                             json_of_list json_of_value_binding vs;
                                             json_of_expression e;
                                         ])
| Pexp_function(cs) -> variant "Function" @@ json_of_list json_of_case cs
| Pexp_fun(a, eo, p, b) -> variant "Fun" @@ Array([
                                                json_of_arg_label a;
                                                json_of_option json_of_expression eo;
                                                json_of_pattern p;
                                                json_of_expression b;
                                            ])
| Pexp_apply(f, a) ->
        let helper (a, e) = Array([json_of_arg_label a; json_of_expression e]) in
            variant "Apply" @@ Array([json_of_expression f; json_of_list helper a])
| Pexp_match(e, cs) -> variant "Match" @@ Array([
                                              json_of_expression e;
                                              json_of_list json_of_case cs;
                                          ])
| Pexp_try(e, cs) -> variant "Try" @@ Array([
                                          json_of_expression e;
                                          json_of_list json_of_case cs;
                                      ])
| Pexp_tuple(es) -> variant "Tuple" @@ json_of_list json_of_expression es
| Pexp_construct(t, eo) -> variant "Construct" @@ Array([
                                                      json_of_loc json_of_long_ident t;
                                                      json_of_option json_of_expression eo;
                                                  ])
(* | Pexp_construct of Longident.t Asttypes.loc * expression option *)
| Pexp_variant(_, _) -> failwith "TODO Variant"
(* | Pexp_variant of Asttypes.label * expression option *)
| Pexp_record(vs, eo) -> variant "Record" @@ Array ([
                                                 (let helper (n, e) = Array([
                                                     json_of_loc json_of_long_ident n;
                                                     json_of_expression e;
                                                 ]) in json_of_list helper vs);
                                                 json_of_option json_of_expression eo;
                                             ])
(* | Pexp_record of (Longident.t Asttypes.loc * expression) list * expression option *)
| Pexp_field(e, n) -> variant "Field" @@ Array([
                                             json_of_expression e;
                                             json_of_loc json_of_long_ident n;
                                         ])
(* | Pexp_field of expression * Longident.t Asttypes.loc *)
| Pexp_setfield(e, n, v) -> variant "SetField" @@ Array([
                                                      json_of_expression e;
                                                      json_of_loc json_of_long_ident n;
                                                      json_of_expression v;
                                                  ])
(* | Pexp_setfield of expression * Longident.t Asttypes.loc * expression *)
| Pexp_array(es) -> variant "Array" @@ json_of_list json_of_expression es
(* | Pexp_array of expression list *)
| Pexp_ifthenelse(c, t, e) -> variant "IfThenElse" @@ Array([
                                                          json_of_expression c;
                                                          json_of_expression t;
                                                          json_of_option json_of_expression e;
                                                      ])
(* | Pexp_ifthenelse of expression * expression * expression option *)
| Pexp_sequence(e1, e2) -> variant "Sequence" @@ Array([
                                                     json_of_expression e1;
                                                     json_of_expression e2;
                                                 ])
(* | Pexp_sequence of expression * expression *)
| Pexp_while(_, _) -> failwith "TODO While"
(* | Pexp_while of expression * expression *)
| Pexp_for(_, _, _, _, _) -> failwith "TODO For"
(* | Pexp_for of pattern * expression * expression * Asttypes.direction_flag * expression *)
| Pexp_constraint(e, t) -> variant "Constraint" @@ Array([
                                                       json_of_expression e;
                                                       json_of_core_type t;
                                                   ])
(* | Pexp_constraint of expression * core_type *)
| Pexp_coerce(_, _, _) -> failwith "TODO Coerce"
(* | Pexp_coerce of expression * core_type option * core_type *)
| Pexp_send(_, _) -> failwith "TODO Send"
(* | Pexp_send of expression * Asttypes.label Asttypes.loc *)
| Pexp_new(_) -> failwith "TODO New"
(* | Pexp_new of Longident.t Asttypes.loc *)
| Pexp_setinstvar(_, _) -> failwith "TODO SetInstVar"
(* | Pexp_setinstvar of Asttypes.label Asttypes.loc * expression *)
| Pexp_override(_) -> failwith "TODO Override"
(* | Pexp_override of (Asttypes.label Asttypes.loc * expression) list *)
| Pexp_letmodule(_, _, _) -> failwith "TODO LetModule"
(* | Pexp_letmodule of string Asttypes.loc * module_expr * expression *)
| Pexp_letexception(_, _) -> failwith "TODO LetException"
(* | Pexp_letexception of extension_constructor * expression *)
| Pexp_assert(_) -> failwith "TODO Assert"
(* | Pexp_assert of expression *)
| Pexp_lazy(_) -> failwith "TODO Lazy"
(* | Pexp_lazy of expression *)
| Pexp_poly(_, _) -> failwith "TODO Poly"
(* | Pexp_poly of expression * core_type option *)
| Pexp_object(_) -> failwith "TODO Object"
(* | Pexp_object of class_structure *)
| Pexp_newtype(_, _) -> failwith "TODO Newtype"
(* | Pexp_newtype of string Asttypes.loc * expression *)
| Pexp_pack(_) -> failwith "TODO Pack"
(* | Pexp_pack of module_expr *)
| Pexp_open(_, _, _) -> failwith "TODO Open"
(* | Pexp_open of Asttypes.override_flag * Longident.t Asttypes.loc * expression *)
| Pexp_extension(_) -> failwith "TODO Extension"
(* | Pexp_extension of extension *)
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

and json_of_value_binding (vb: value_binding) : json =
    Object([
        ("pat", json_of_pattern vb.pvb_pat);
        ("expr", json_of_expression vb.pvb_expr);
        ("attributes", json_of_attributes vb.pvb_attributes);
        ("location", json_of_location vb.pvb_loc);
    ])

and json_of_type_declaration (d: type_declaration) : json =
    Object([
        ("name", json_of_loc json_of_string d.ptype_name);
        (let helper (ct, v) = Array([json_of_core_type ct; json_of_variance v])
        in ("params", json_of_list helper d.ptype_params));
      (*ptype_params : (core_type * Asttypes.variance) list;*)
        (*("cstrs", failwith "TODO");*)
      (*ptype_cstrs : (core_type * core_type * Location.t) list;*)
        (*("kind", failwith "TODO");*)
      (*ptype_kind : type_kind;*)
        (*("private", failwith "TODO");*)
      (*ptype_private : Asttypes.private_flag;*)
        (*("manifest", failwith "TODO");*)
      (*ptype_manifest : core_type option;*)
        (*("attributes", failwith "TODO");*)
      (*ptype_attributes : attributes;*)
        ("location", json_of_location d.ptype_loc);
    ])

and json_of_open_description (o: open_description) : json =
    Object([
        ("id", json_of_loc json_of_long_ident o.popen_lid);
        ("override", json_of_override_flag o.popen_override);
        ("location", json_of_location o.popen_loc);
        ("attributes", json_of_attributes o.popen_attributes)
    ])

and json_of_structure_item_desc = function
| Pstr_eval(e, a) -> variant "Eval" @@ Array([
                         json_of_expression e;
                         json_of_attributes a;
                     ])
| Pstr_value(r, bs) -> variant "Value" @@ Array([
                           json_of_rec_flag r;
                           json_of_list json_of_value_binding bs;
                       ])
(*
| Pstr_primitive of value_description
*)
| Pstr_type(r, ds) -> variant "Type" @@ Array([
                           json_of_rec_flag r;
                           json_of_list json_of_type_declaration ds;
                      ])
(*
| Pstr_typext of type_extension
*)
| Pstr_exception(c) -> variant "Exception" @@ json_of_extension_constructor c
(*
| Pstr_module of module_binding
| Pstr_recmodule of module_binding list
| Pstr_modtype of module_type_declaration
*)
| Pstr_open(o) -> variant "Open" @@ json_of_open_description o
(*
| Pstr_class of class_declaration list
| Pstr_class_type of class_type_declaration list
| Pstr_include of include_declaration
*)
| Pstr_attribute(a) -> variant "Attribute" @@ json_of_attribute a
(*
| Pstr_extension of extension * attributes
*)

and json_of_structure_item (si: structure_item) : json =
    Object([
        ("location", json_of_location si.pstr_loc);
        ("desc", json_of_structure_item_desc si.pstr_desc);
    ])

and json_of_structure (s: structure) : json =
    json_of_list json_of_structure_item s

(* TODO json_of_module_binding *)

(*****************************************************************************)
(********************************** TOPLEVEL *********************************)
(*****************************************************************************)

let json_of_directive_argument = function
| Pdir_none -> variant "None" @@ Null
| Pdir_string(s) -> variant "String" @@ String(s)
| Pdir_int(c, s) -> failwith "Pdir_int"
| Pdir_ident(i) -> variant "Ident" @@ json_of_long_ident i
| Pdir_bool(b) -> variant "Bool" @@ Bool(b)

let json_of_toplevel_phrase = function
| Ptop_def(s) -> variant "Def" @@ json_of_structure s
| Ptop_dir(n, a) -> variant "Dir" @@ Array([
                        json_of_string n;
                        json_of_directive_argument a;
                    ])

(*****************************************************************************)
(******************************* ERROR HANDLING ******************************)
(*****************************************************************************)

let json_of_lexer_error = function
| Lexer.Illegal_character(ch) -> failwith "TODO Illegal_character"
| Lexer.Illegal_escape(esc) -> failwith "TODO Illegal_escape"
| Lexer.Unterminated_comment(loc) -> failwith "TODO Unterminated_comment"
| Lexer.Unterminated_string -> failwith "TODO Unterminated_string"
| Lexer.Unterminated_string_in_comment(l1, l2) -> failwith "TODO Unterminated_string_in_comment"
| Lexer.Keyword_as_label(str) -> failwith "Keyword_as_label"
| Lexer.Invalid_literal(str) -> failwith "Invalid_literal"
| Lexer.Invalid_directive(dir, expl) -> failwith "Invalid_directive"

let json_of_syntax_error = function
| Syntaxerr.Unclosed(la, sa, lb, sb) ->
    variant "Unclosed" @@ Array([
        json_of_location la;
        json_of_string sa;
        json_of_location lb;
        json_of_string sb;
    ])
| Syntaxerr.Expecting(loc, str) ->
    variant "Expecting" @@ Array([
        json_of_location loc;
        json_of_string str;
    ])
| Syntaxerr.Not_expecting(loc, str) ->
    variant "NotExpecting" @@ Array([
        json_of_location loc;
        json_of_string str;
    ])
| Syntaxerr.Applicative_path(loc) -> variant "ApplicativePath" @@ json_of_location loc
| Syntaxerr.Variable_in_scope(loc, str) ->
    variant "VariableInScope" @@ Array([
        json_of_location loc;
        json_of_string str;
    ])
| Syntaxerr.Other(loc) -> variant "Other" @@ json_of_location loc
| Syntaxerr.Ill_formed_ast(loc, str) ->
    variant "IllFormedAst" @@ Array([
        json_of_location loc;
        json_of_string str;
    ])
| Syntaxerr.Invalid_package_type(loc, str) ->
    variant "InvalidPackageType" @@ Array([
        json_of_location loc;
        json_of_string str;
    ])

(*****************************************************************************)
(******************************* PARSE FUNCTION ******************************)
(*****************************************************************************)

let json_of_ocaml (src: string) (path: string) : json =
    let buf = Lexing.from_string src in
    buf.lex_start_p <- { buf.lex_start_p with pos_fname = path };
    buf.lex_curr_p <- { buf.lex_curr_p with pos_fname = path };
    buf
    |> Parse.use_file
    |> json_of_list json_of_toplevel_phrase

let parse (src: string) (path: string) : string =
    let json =
        try
            Object([
                ("Ok", json_of_ocaml src path);
            ])
        with
            | Lexer.Error(err, loc) ->
                Object([
                    ("Err", variant "Lexer" @@ Array([
                        json_of_lexer_error err;
                        json_of_location loc;
                    ]))
                ])
            | Syntaxerr.Error(err) ->
                Object([
                    ("Err", variant "Syntax" @@ json_of_syntax_error err)
                ])
            | err ->
                let err = json_of_string (Printexc.to_string err) in
                Object([ ("Err", variant "Other" @@ err) ])
    in
    string_of_json json

(* Register the parse function to be callable from C. *)
let () =
    Callback.register "parse" parse
