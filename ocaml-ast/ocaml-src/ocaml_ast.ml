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
let (%) : 'a 'b 'c. ('b -> 'c) -> ('a -> 'b) -> 'a -> 'c =
    (fun f g x -> f (g x))

(* Reverse composition. *)
let (%%) : 'a 'b 'c. ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c =
    (fun f g x -> g (f x))

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
    in
    let explode (s: string) : char list =
      let rec expl i l =
        if i < 0 then l else
        expl (i - 1) (s.[i] :: l) in
      expl (String.length s - 1) []
    in
    let quote (s: string) : string =
        "\"" ^ s ^ "\""
    in
    explode s
    |> List.map escape_char
    |> String.concat ""
    |> quote

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

(*****************************************************************************)
(********************************* AST TYPES *********************************)
(*****************************************************************************)

let rec json_of_rec_flag = function
| Nonrecursive -> Bool(false)
| Recursive -> Bool(true)

and json_of_direction_flag = function
| Upto -> Bool(true)
| Downto -> Bool(false)

and json_of_private_flag = function
| Private -> Bool(true)
| Public -> Bool(false)

and json_of_mutable_flag = function
| Immutable -> Bool(false)
| Mutable -> Bool(true)

and json_of_virtual_flag = function
| Virtual -> Bool(true)
| Concrete -> Bool(false)

and json_of_override_flag = function
| Override -> Bool(true)
| Fresh -> Bool(false)

and json_of_closed_flag = function
| Closed -> Bool(true)
| Open -> Bool(false)

and json_of_label : label -> json = json_of_string

and json_of_arg_label = function
| Nolabel -> variant "NoLabel" Null
| Labelled s -> variant "Labelled" @@ String(s)
| Optional s -> variant "Optional" @@ String(s)

and json_of_variance = function
| Covariant -> String("Covariant")
| Contravariant -> String("Contravariant")
| Invariant -> String("Invariant")

(*****************************************************************************)
(********************************** LOCATIONS ********************************)
(*****************************************************************************)

let rec json_of_position (pos: Lexing.position) : json =
    Object([
        ("filename", String(pos.pos_fname));
        ("line", Int(pos.pos_lnum));
        ("column", Int(pos.pos_cnum - pos.pos_bol));
        ("offset", Int(pos.pos_cnum));
    ])

and json_of_location (loc: Location.t) : json =
    Object([
        ("start", json_of_position loc.loc_start);
        ("end", json_of_position loc.loc_end);
        ("ghost", Bool(loc.loc_ghost));
    ])

and json_of_loc (f: 'a -> json) (x: 'a loc) : json =
    Object([
        ("txt", f x.txt);
        ("location", json_of_location x.loc);
    ])

(*****************************************************************************)
(********************************* PARSE TREE ********************************)
(*****************************************************************************)

let rec json_of_constant = function
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

and json_of_long_ident = function
| Lident(s) -> variant "Ident" @@ json_of_string s
| Ldot(i, s) -> variant "Dot" @@ Array([json_of_long_ident i; json_of_string s])
| Lapply(l, r) ->
        variant "Apply" @@ Array([
            json_of_long_ident l;
            json_of_long_ident r;
        ])

(*****************************************************************************)
(****************************** EXTENSION POINTS *****************************)
(*****************************************************************************)

and json_of_attribute (attr: attribute) =
    json_of_pair (json_of_loc json_of_string) json_of_payload attr

and json_of_extension (ext: extension) =
    json_of_pair (json_of_loc json_of_string) json_of_payload ext

and json_of_attributes (attrs: attributes) : json =
    json_of_list json_of_attribute attrs

and json_of_payload = function
| PStr(s) -> variant "Structure" @@ json_of_structure s
| PSig(s) -> variant "Signature" @@ failwith "TODO PSig"
| PTyp(s) -> variant "Type" @@ json_of_core_type s
| PPat(p, e) -> variant "Pattern" @@ Array([
                    json_of_pattern p;
                    json_of_option json_of_expression e;
                ])

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

and json_of_package_type (pt: package_type) : json =
    json_of_pair
        (json_of_loc json_of_long_ident)
        (json_of_list (json_of_pair (json_of_loc json_of_long_ident) json_of_core_type))
        pt

and json_of_row_field : row_field -> json = function
| Rtag(lbl, attrs, b, tys) ->
    variant "Tag" @@ Array([
        json_of_loc json_of_label lbl;
        json_of_attributes attrs;
        Bool(b);
        json_of_list json_of_core_type tys;
    ])
| Rinherit(t) -> variant "Inherit" @@ json_of_core_type t

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
| Ppat_extension(e) -> variant "Extension" @@ json_of_extension e
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
| Pexp_variant(_, _) -> failwith "TODO Variant"
| Pexp_record(vs, eo) -> variant "Record" @@ Array ([
                                                 (let helper (n, e) = Array([
                                                     json_of_loc json_of_long_ident n;
                                                     json_of_expression e;
                                                 ]) in json_of_list helper vs);
                                                 json_of_option json_of_expression eo;
                                             ])
| Pexp_field(e, n) -> variant "Field" @@ Array([
                                             json_of_expression e;
                                             json_of_loc json_of_long_ident n;
                                         ])
| Pexp_setfield(e, n, v) -> variant "SetField" @@ Array([
                                                      json_of_expression e;
                                                      json_of_loc json_of_long_ident n;
                                                      json_of_expression v;
                                                  ])
| Pexp_array(es) -> variant "Array" @@ json_of_list json_of_expression es
| Pexp_ifthenelse(c, t, e) -> variant "IfThenElse" @@ Array([
                                                          json_of_expression c;
                                                          json_of_expression t;
                                                          json_of_option json_of_expression e;
                                                      ])
| Pexp_sequence(e1, e2) -> variant "Sequence" @@ Array([
                                                     json_of_expression e1;
                                                     json_of_expression e2;
                                                 ])
| Pexp_while(_, _) -> failwith "TODO While"
| Pexp_for(_, _, _, _, _) -> failwith "TODO For"
| Pexp_constraint(e, t) -> variant "Constraint" @@ Array([
                                                       json_of_expression e;
                                                       json_of_core_type t;
                                                   ])
| Pexp_coerce(_, _, _) -> failwith "TODO Coerce"
| Pexp_send(_, _) -> failwith "TODO Send"
| Pexp_new(_) -> failwith "TODO New"
| Pexp_setinstvar(_, _) -> failwith "TODO SetInstVar"
| Pexp_override(_) -> failwith "TODO Override"
| Pexp_letmodule(_, _, _) -> failwith "TODO LetModule"
| Pexp_letexception(_, _) -> failwith "TODO LetException"
| Pexp_assert(expr) -> variant "Assert" @@ json_of_expression expr
| Pexp_lazy(_) -> failwith "TODO Lazy"
| Pexp_poly(_, _) -> failwith "TODO Poly"
| Pexp_object(_) -> failwith "TODO Object"
| Pexp_newtype(_, _) -> failwith "TODO Newtype"
| Pexp_pack(_) -> failwith "TODO Pack"
| Pexp_open(_, _, _) -> failwith "TODO Open"
| Pexp_extension(_) -> failwith "TODO Extension"
| Pexp_unreachable -> variant "Unreachable" Null

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

and json_of_expression (exp: expression) : json =
    Object([
        ("desc", json_of_expression_desc exp.pexp_desc);
        ("location", json_of_location exp.pexp_loc);
        ("attributes", json_of_attributes exp.pexp_attributes);
    ])

and json_of_value_description (vd: value_description) : json =
    Object([
        ("name", json_of_loc json_of_string vd.pval_name);
        ("type", json_of_core_type vd.pval_type);
        ("prim", json_of_list json_of_string vd.pval_prim);
        ("attributes", json_of_attributes vd.pval_attributes);
        ("location", json_of_location vd.pval_loc);
    ])

and json_of_type_extension (te: type_extension) : json =
    Object([
        ("path", json_of_loc json_of_long_ident te.ptyext_path);
        ("params", json_of_list (json_of_pair json_of_core_type json_of_variance) te.ptyext_params);
        ("constructors", json_of_list json_of_extension_constructor te.ptyext_constructors);
        ("private", json_of_private_flag te.ptyext_private);
        ("attributes", json_of_attributes te.ptyext_attributes);
    ])

(*****************************************************************************)
(******************************* CLASS LANGUAGE ******************************)
(*****************************************************************************)

and json_of_class_type (ct: class_type) : json =
    Object([
        ("desc", json_of_class_type_desc ct.pcty_desc);
        ("location", json_of_location ct.pcty_loc);
        ("attributes", json_of_attributes ct.pcty_attributes);
    ])

and json_of_class_type_desc : class_type_desc -> json = function
| Pcty_constr(name, types) ->
    variant "Constr" @@ Array([
        json_of_loc json_of_long_ident name;
        json_of_list json_of_core_type types;
    ])
| Pcty_signature(s) -> variant "Signature" @@ json_of_class_signature s
| Pcty_arrow(n, t1, t2) ->
    variant "Arrow" @@ Array([
        json_of_arg_label n;
        json_of_core_type t1;
        json_of_class_type t2;
    ])
| Pcty_extension(e) -> variant "Extension" @@ json_of_extension e
| Pcty_open(o, n, t) ->
    variant "Open" @@ Array([
        json_of_override_flag o;
        json_of_loc json_of_long_ident n;
        json_of_class_type t;
    ])

and json_of_class_signature (cs: class_signature) : json =
    Object([
        ("self", json_of_core_type cs.pcsig_self);
        ("fields", json_of_list json_of_class_type_field cs.pcsig_fields);
    ])

and json_of_class_type_field (ctf: class_type_field) : json =
    Object([
        ("desc", json_of_class_type_field_desc ctf.pctf_desc);
        ("location", json_of_location ctf.pctf_loc);
        ("attributes", json_of_attributes ctf.pctf_attributes);
    ])

and json_of_class_type_field_desc = function
| Pctf_inherit(ct) -> variant "Inherit" @@ json_of_class_type ct
| Pctf_val(n, m, v, t) ->
    variant "Val" @@ Array([
        json_of_loc json_of_label n;
        json_of_mutable_flag m;
        json_of_virtual_flag v;
        json_of_core_type t;
    ])
| Pctf_method(n, p, v, t) ->
    variant "Method" @@ Array([
        json_of_loc json_of_label n;
        json_of_private_flag p;
        json_of_virtual_flag v;
        json_of_core_type t;
    ])
| Pctf_constraint(t1, t2) ->
    variant "Constraint" @@ Array([
        json_of_core_type t1;
        json_of_core_type t2;
    ])
| Pctf_attribute(a) -> variant "Attribute" @@ json_of_attribute a
| Pctf_extension(e) -> variant "Extension" @@ json_of_extension e

and json_of_class_infos : 'a. ('a -> json) -> ('a class_infos) -> json =
    fun f ci -> Object([
        ("virt", json_of_virtual_flag ci.pci_virt);
        ("params", json_of_list (json_of_pair json_of_core_type json_of_variance) ci.pci_params);
        ("name", json_of_loc json_of_string ci.pci_name);
        ("expr", f ci.pci_expr);
        ("location", json_of_location ci.pci_loc);
        ("attributes", json_of_attributes ci.pci_attributes);
    ])

and json_of_class_description (cd: class_description) : json =
    json_of_class_infos json_of_class_type cd

and json_of_class_type_declaration (cd: class_type_declaration) : json =
    json_of_class_infos json_of_class_type cd

and json_of_class_expr (ce: class_expr) : json =
    Object([
        ("desc", json_of_class_expr_desc ce.pcl_desc);
        ("location", json_of_location ce.pcl_loc);
        ("attributes", json_of_attributes ce.pcl_attributes);
    ])

and json_of_class_expr_desc : class_expr_desc -> json = function
| Pcl_constr(name, types) ->
    variant "Constr" @@ Array([
        json_of_loc json_of_long_ident name;
        json_of_list json_of_core_type types;
    ])

and json_of_class_declaration (cd: class_declaration) : json =
    json_of_class_infos json_of_class_expr cd

(*****************************************************************************)
(******************************* MODULE LANGUAGE *****************************)
(*****************************************************************************)

and json_of_module_type (mt: module_type) : json =
    Object([
        ("desc", json_of_module_type_desc mt.pmty_desc);
        ("location", json_of_location mt.pmty_loc);
        ("attributes", json_of_attributes mt.pmty_attributes);
    ])

and json_of_module_type_desc = function
| Pmty_ident(ident) -> variant "Ident" @@ json_of_loc json_of_long_ident ident
| Pmty_signature(s) -> variant "Signature" @@ failwith "TODO Pmty_signature"
| Pmty_functor(name, arg, body) ->
    variant "Functor" @@ Array([
        json_of_loc json_of_string name;
        json_of_option json_of_module_type arg;
        json_of_module_type body;
    ])
| Pmty_with(mt, wcs) ->
    variant "With" @@ Array([
        json_of_module_type mt;
        json_of_list json_of_with_constraint wcs;
    ])
| Pmty_typeof(me) -> variant "TypeOf" @@ json_of_module_expr me
| Pmty_extension(e) -> variant "Extension" @@ json_of_extension e
| Pmty_alias(n) -> variant "Alias" @@ json_of_loc json_of_long_ident n

and json_of_module_type_declaration (mtd: module_type_declaration) : json =
    Object([
        ("name", json_of_loc json_of_string mtd.pmtd_name);
        ("type", json_of_option json_of_module_type mtd.pmtd_type);
        ("location", json_of_location mtd.pmtd_loc);
        ("attributes", json_of_attributes mtd.pmtd_attributes);
    ])

and json_of_open_description (o: open_description) : json =
    Object([
        ("id", json_of_loc json_of_long_ident o.popen_lid);
        ("override", json_of_override_flag o.popen_override);
        ("location", json_of_location o.popen_loc);
        ("attributes", json_of_attributes o.popen_attributes);
    ])

and json_of_include_infos: 'a. ('a -> json) -> ('a include_infos) -> json =
    fun f ii ->
    Object([
        ("mod", f ii.pincl_mod);
        ("location", json_of_location ii.pincl_loc);
        ("attributes", json_of_attributes ii.pincl_attributes);
    ])

and json_of_include_description (id: include_description) : json =
    json_of_include_infos json_of_module_type id

and json_of_include_declaration (id: include_declaration) : json =
    json_of_include_infos json_of_module_expr id

and json_of_with_constraint = function
| Pwith_type(name, td) ->
    variant "Type" @@ Array([
        json_of_loc json_of_long_ident name;
        json_of_type_declaration td;
    ])
| Pwith_module(name, m) ->
    variant "Module" @@ Array([
        json_of_loc json_of_long_ident name;
        json_of_loc json_of_long_ident m;
    ])
| Pwith_typesubst(name, td) ->
    variant "TypeSubst" @@ Array([
        json_of_loc json_of_long_ident name;
        json_of_type_declaration td;
    ])
| Pwith_modsubst(name, m) ->
    variant "ModuleSubst" @@ Array([
        json_of_loc json_of_long_ident name;
        json_of_loc json_of_long_ident m;
    ])

and json_of_module_expr (me: module_expr) : json =
    Object([
        ("desc", json_of_module_expr_desc me.pmod_desc);
        ("location", json_of_location me.pmod_loc);
        ("attributes", json_of_attributes me.pmod_attributes);
    ])

and json_of_module_expr_desc = function
| Pmod_ident(ident) -> variant "Ident" @@ json_of_loc json_of_long_ident ident;
| Pmod_structure(s) -> variant "Structure" @@ json_of_structure s
| Pmod_functor(n, t, e) ->
    variant "Functor" @@ Array([
        json_of_loc json_of_string n;
        json_of_option json_of_module_type t;
        json_of_module_expr e;
    ])
| Pmod_apply(e1, e2) ->
    variant "Apply" @@ Array([
        json_of_module_expr e1;
        json_of_module_expr e2;
    ])
| Pmod_constraint(e, t) ->
    variant "Constraint" @@ Array([
        json_of_module_expr e;
        json_of_module_type t;
    ])
| Pmod_unpack(e) -> variant "Unpack" @@ json_of_expression e
| Pmod_extension(e) -> variant "Extension" @@ json_of_extension e

and json_of_structure (s: structure) : json =
    json_of_list json_of_structure_item s

and json_of_structure_item (si: structure_item) : json =
    Object([
        ("location", json_of_location si.pstr_loc);
        ("desc", json_of_structure_item_desc si.pstr_desc);
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
| Pstr_primitive(vd) -> variant "Primitive" @@ json_of_value_description vd
| Pstr_type(r, ds) -> variant "Type" @@ Array([
                           json_of_rec_flag r;
                           json_of_list json_of_type_declaration ds;
                      ])
| Pstr_typext(te) -> variant "TypeExt" @@ json_of_type_extension te
| Pstr_exception(c) -> variant "Exception" @@ json_of_extension_constructor c
| Pstr_module(mb) -> variant "Module" @@ json_of_module_binding mb
| Pstr_recmodule(mbs) -> variant "RecModule" @@ json_of_list json_of_module_binding mbs
| Pstr_modtype(mtd) -> variant "ModType" @@ json_of_module_type_declaration mtd
| Pstr_open(o) -> variant "Open" @@ json_of_open_description o
| Pstr_class(cds) -> variant "Class" @@ json_of_list json_of_class_declaration cds
| Pstr_class_type(ctds) -> variant "ClassType" @@ json_of_list json_of_class_type_declaration ctds
| Pstr_include(id) -> variant "Include" @@ json_of_include_declaration id
| Pstr_attribute(a) -> variant "Attribute" @@ json_of_attribute a
| Pstr_extension(e, a) ->
    variant "Extension" @@ Array([
        json_of_extension e;
        json_of_attributes a;
    ])

and json_of_value_binding (vb: value_binding) : json =
    Object([
        ("pat", json_of_pattern vb.pvb_pat);
        ("expr", json_of_expression vb.pvb_expr);
        ("attributes", json_of_attributes vb.pvb_attributes);
        ("location", json_of_location vb.pvb_loc);
    ])

and json_of_module_binding (mb: module_binding) : json =
    Object([
        ("name", json_of_loc json_of_string mb.pmb_name);
        ("expr", json_of_module_expr mb.pmb_expr);
        ("location", json_of_location mb.pmb_loc);
        ("attributes", json_of_attributes mb.pmb_attributes)
    ])

(*****************************************************************************)
(********************************** TOPLEVEL *********************************)
(*****************************************************************************)

let json_of_directive_argument = function
| Pdir_none -> variant "None" @@ Null
| Pdir_string(s) -> variant "String" @@ String(s)
| Pdir_int(c, s) -> failwith "TODO Pdir_int"
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
| Lexer.Illegal_character(ch) -> variant "IllegalCharacter" @@ json_of_char ch
| Lexer.Illegal_escape(esc) -> variant "IllegalEscape" @@ json_of_string esc
| Lexer.Unterminated_comment(loc) -> variant "UnterminatedComment" @@ json_of_location loc
| Lexer.Unterminated_string -> variant "UnterminatedString" @@ Null
| Lexer.Unterminated_string_in_comment(l1, l2) ->
    variant "UnterminatedStringInComment" @@ Array([
        json_of_location l1;
        json_of_location l2;
    ])
| Lexer.Keyword_as_label(str) -> variant "KeywordAsLabel" @@ json_of_string str
| Lexer.Invalid_literal(str) -> variant "InvalidLiteral" @@ json_of_string str
| Lexer.Invalid_directive(dir, expl) ->
    variant "InvalidDirective" @@ Array([
        json_of_string dir;
        json_of_option json_of_string expl;
    ])

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
    Parse.use_file buf
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
