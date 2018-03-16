let parse_ast ~src:src ~path:path : Parsetree.toplevel_phrase list =
    let buf = Lexing.from_string src in
    buf.lex_start_p <- { buf.lex_start_p with pos_fname = path };
    buf.lex_curr_p <- { buf.lex_curr_p with pos_fname = path };
    Parse.use_file buf

module Make (S : Serialize.S) = struct
    open Asttypes
    open Longident
    open Parsetree

    module S_ext = Serialize.S_ext(S)
    open S
    open S_ext

    (*************************************************************************)
    (******************************* AST TYPES *******************************)
    (*************************************************************************)

    let rec serialize_rec_flag = function
    | Nonrecursive -> serialize_bool false
    | Recursive -> serialize_bool true

    and serialize_direction_flag = function
    | Upto -> serialize_bool true
    | Downto -> serialize_bool false

    and serialize_private_flag = function
    | Private -> serialize_bool true
    | Public -> serialize_bool false

    and serialize_mutable_flag = function
    | Immutable -> serialize_bool false
    | Mutable -> serialize_bool true

    and serialize_virtual_flag = function
    | Virtual -> serialize_bool true
    | Concrete -> serialize_bool false

    and serialize_override_flag = function
    | Override -> serialize_bool true
    | Fresh -> serialize_bool false

    and serialize_closed_flag = function
    | Closed -> serialize_bool true
    | Open -> serialize_bool false

    and serialize_label = serialize_string

    and serialize_arg_label = function
    | Nolabel -> serialize_variant "NoLabel" serialize_null
    | Labelled s -> serialize_variant "Labelled" @@ serialize_string s
    | Optional s -> serialize_variant "Optional" @@ serialize_string s

    and serialize_variance = function
    | Covariant -> serialize_variant "Covariant" serialize_null
    | Contravariant -> serialize_variant "Contravariant" serialize_null
    | Invariant -> serialize_variant "Invariant" serialize_null

    (*************************************************************************)
    (******************************** LOCATIONS ******************************)
    (*************************************************************************)

    let rec serialize_position (pos: Lexing.position) : S.t =
        serialize_struct "Position" ([
            ("filename", serialize_string pos.pos_fname);
            ("line", serialize_int pos.pos_lnum);
            ("column", serialize_int (pos.pos_cnum - pos.pos_bol));
            ("offset", serialize_int pos.pos_cnum);
        ])

    and serialize_location (loc: Location.t) : S.t =
        serialize_struct "Location" ([
            ("start", serialize_position loc.loc_start);
            ("end", serialize_position loc.loc_end);
            ("ghost", serialize_bool loc.loc_ghost);
        ])

    and serialize_loc (f: 'a -> S.t) (x: 'a loc) : S.t =
        serialize_struct "Loc" ([
            ("txt", f x.txt);
            ("location", serialize_location x.loc);
        ])

    (*************************************************************************)
    (******************************* PARSE TREE ******************************)
    (*************************************************************************)

    let rec serialize_constant = function
    | Pconst_integer(str, suffix) ->
            serialize_variant "Integer" @@ serialize_seq ([
                serialize_string str;
                build_option serialize_char suffix;
            ])
    | Pconst_char(c) -> serialize_variant "Char" @@ serialize_char c
    | Pconst_string(str, suffix) ->
            serialize_variant "String" @@ serialize_seq ([
                serialize_string str;
                build_option serialize_string suffix;
            ])
    | Pconst_float(str, suffix) ->
            serialize_variant "Float" @@ serialize_seq ([
                serialize_string str;
                build_option serialize_char suffix;
            ])

    and serialize_long_ident = function
    | Lident(s) -> serialize_variant "Ident" @@ serialize_string s
    | Ldot(i, s) -> serialize_variant "Dot" @@ serialize_seq ([serialize_long_ident i; serialize_string s])
    | Lapply(l, r) ->
        serialize_variant "Apply" @@ serialize_seq ([
            serialize_long_ident l;
            serialize_long_ident r;
        ])

    (*****************************************************************************)
    (****************************** EXTENSION POINTS *****************************)
    (*****************************************************************************)

    and serialize_attribute (attr: attribute) =
        build_pair (serialize_loc serialize_string) serialize_payload attr

    and serialize_extension (ext: extension) =
        build_pair (serialize_loc serialize_string) serialize_payload ext

    and serialize_attributes (attrs: attributes) : S.t =
        build_list serialize_attribute attrs

    and serialize_payload = function
    | PStr(s) -> serialize_variant "Structure" @@ serialize_structure s
    | PSig(s) -> serialize_variant "Signature" @@ serialize_signature s
    | PTyp(s) -> serialize_variant "Type" @@ serialize_core_type s
    | PPat(p, e) ->
        serialize_variant "Pattern" @@ serialize_seq ([
            serialize_pattern p;
            build_option serialize_expression e;
        ])

    (*************************************************************************)
    (****************************** CORE LANGUAGE ****************************)
    (*************************************************************************)

    and serialize_core_type (ct: core_type) : S.t =
        serialize_struct "CoreType" ([
            ("desc", serialize_core_type_desc ct.ptyp_desc);
            ("location", serialize_location ct.ptyp_loc);
            ("attributes", serialize_attributes ct.ptyp_attributes);
        ])

    and serialize_core_type_desc = function
    | Ptyp_any -> serialize_variant "Any" serialize_null
    | Ptyp_var(s) -> serialize_variant "Var" @@ serialize_string s
    | Ptyp_arrow(a, l, r) ->
        serialize_variant "Arrow" @@ serialize_seq ([
            serialize_arg_label a;
            serialize_core_type l;
            serialize_core_type r;
        ])
    | Ptyp_tuple(ts) -> serialize_variant "Tuple" @@ build_list serialize_core_type ts
    | Ptyp_constr(a, ts) ->
        serialize_variant "Constr" @@ serialize_seq ([
            serialize_loc serialize_long_ident a;
            build_list serialize_core_type ts;
        ])
    | Ptyp_object(fs, c) ->
        serialize_variant "Object" @@ serialize_seq ([
            build_list serialize_object_field fs;
            serialize_closed_flag c;
        ])
    | Ptyp_class(l, ts) ->
        serialize_variant "Class" @@ serialize_seq ([
            serialize_loc serialize_long_ident l;
            build_list serialize_core_type ts;
        ])
    | Ptyp_alias(t, s) ->
        serialize_variant "Alias" @@ serialize_seq ([
            serialize_core_type t;
            serialize_string s;
        ])
    | Ptyp_variant(vs, c, ls) ->
        serialize_variant "Variant" @@ serialize_seq ([
            build_list serialize_row_field vs;
            serialize_closed_flag c;
            build_option (build_list serialize_label) ls;
        ])
    | Ptyp_poly(vs, t) ->
        serialize_variant "Poly" @@ serialize_seq ([
            build_list (serialize_loc serialize_string) vs;
            serialize_core_type t;
        ])
    | Ptyp_package(e) -> serialize_variant "Package" @@ serialize_package_type e
    | Ptyp_extension(e) -> serialize_variant "Extension" @@ serialize_extension e

    and serialize_package_type (pt: package_type) : S.t =
        build_pair
            (serialize_loc serialize_long_ident)
            (build_list (build_pair (serialize_loc serialize_long_ident) serialize_core_type))
            pt

    and serialize_row_field : row_field -> S.t = function
    | Rtag(lbl, attrs, b, tys) ->
        serialize_variant "Tag" @@ serialize_seq ([
            serialize_loc serialize_label lbl;
            serialize_attributes attrs;
            serialize_bool b;
            build_list serialize_core_type tys;
        ])
    | Rinherit(t) -> serialize_variant "Inherit" @@ serialize_core_type t

    and serialize_object_field = function
    | Otag(n, a, t) ->
        serialize_variant "Tag" @@ serialize_seq ([
            serialize_loc serialize_label n;
            serialize_attributes a;
            serialize_core_type t;
        ])
    | Oinherit(t) -> serialize_variant "Inherit" @@ serialize_core_type t

    and serialize_pattern (pat: pattern) : S.t =
        serialize_struct "Pattern" ([
            ("desc", serialize_pattern_desc pat.ppat_desc);
            ("location", serialize_location pat.ppat_loc);
            ("attributes", serialize_attributes pat.ppat_attributes);
        ])

    and serialize_pattern_desc = function
    | Ppat_any -> serialize_variant "Any" serialize_null
    | Ppat_var(n) -> serialize_variant "Var" @@ serialize_loc serialize_string n
    | Ppat_alias(p, n) ->
        serialize_variant "Alias" @@ serialize_seq ([
            serialize_pattern p;
            serialize_loc serialize_string n;
        ])
    | Ppat_constant(c) -> serialize_variant "Constant" @@ serialize_constant c
    | Ppat_interval(f, t) ->
        serialize_variant "Interval" @@ serialize_seq ([
            serialize_constant f;
            serialize_constant t;
        ])
    | Ppat_tuple(ps) -> serialize_variant "Tuple" @@ build_list serialize_pattern ps
    | Ppat_construct(n, p) ->
        serialize_variant "Construct" @@ serialize_seq ([
            serialize_loc serialize_long_ident n;
            build_option serialize_pattern p;
        ])
    | Ppat_variant(n, p) ->
        serialize_variant "Variant" @@ serialize_seq ([
            serialize_string n;
            build_option serialize_pattern p;
        ])
    | Ppat_record(ms, closed) ->
        serialize_variant "Record" @@ serialize_seq ([
            build_list (build_pair (serialize_loc serialize_long_ident) serialize_pattern) ms;
            serialize_closed_flag closed;
        ])
    | Ppat_array(ps) -> serialize_variant "serialize_seq " @@ build_list serialize_pattern ps
    | Ppat_or(l, r) ->
        serialize_variant "Or" @@ serialize_seq ([
            serialize_pattern l;
            serialize_pattern r;
        ])
    | Ppat_constraint(p, t) ->
        serialize_variant "Constraint" @@ serialize_seq ([
            serialize_pattern p;
            serialize_core_type t;
        ])
    | Ppat_type(t) -> serialize_variant "Type" @@ serialize_loc serialize_long_ident t
    | Ppat_lazy(p) -> serialize_variant "Lazy" @@ serialize_pattern p
    | Ppat_unpack(s) -> serialize_variant "Unpack" @@ serialize_loc serialize_string s
    | Ppat_exception(p) -> serialize_variant "Exception" @@ serialize_pattern p
    | Ppat_extension(e) -> serialize_variant "Extension" @@ serialize_extension e
    | Ppat_open(n, p) ->
        serialize_variant "Open" @@ serialize_seq ([
            serialize_loc serialize_long_ident n;
            serialize_pattern p;
        ])

    and serialize_expression (exp: expression) : S.t =
        serialize_struct "Expression" ([
            ("desc", serialize_expression_desc exp.pexp_desc);
            ("location", serialize_location exp.pexp_loc);
            ("attributes", serialize_attributes exp.pexp_attributes);
        ])

    and serialize_expression_desc = function
    | Pexp_ident(i) -> serialize_variant "Ident" @@ serialize_loc serialize_long_ident i
    | Pexp_constant(c) -> serialize_variant "Constant" @@ serialize_constant c
    | Pexp_let(r, vs, e) ->
        serialize_variant "Let" @@ serialize_seq ([
            serialize_rec_flag r;
            build_list serialize_value_binding vs;
            serialize_expression e;
        ])
    | Pexp_function(cs) -> serialize_variant "Function" @@ build_list serialize_case cs
    | Pexp_fun(a, eo, p, b) ->
        serialize_variant "Fun" @@ serialize_seq ([
            serialize_arg_label a;
            build_option serialize_expression eo;
            serialize_pattern p;
            serialize_expression b;
        ])
    | Pexp_apply(f, a) ->
        let helper (a, e) = serialize_seq ([serialize_arg_label a; serialize_expression e]) in
        serialize_variant "Apply" @@ serialize_seq ([serialize_expression f; build_list helper a])
    | Pexp_match(e, cs) ->
        serialize_variant "Match" @@ serialize_seq ([
            serialize_expression e;
            build_list serialize_case cs;
        ])
    | Pexp_try(e, cs) ->
        serialize_variant "Try" @@ serialize_seq ([
            serialize_expression e;
            build_list serialize_case cs;
        ])
    | Pexp_tuple(es) -> serialize_variant "Tuple" @@ build_list serialize_expression es
    | Pexp_construct(t, eo) ->
        serialize_variant "Construct" @@ serialize_seq ([
            serialize_loc serialize_long_ident t;
            build_option serialize_expression eo;
        ])
    | Pexp_variant(l, eo) ->
        serialize_variant "Variant" @@ serialize_seq ([
            serialize_label l;
            build_option serialize_expression eo;
        ])
    | Pexp_record(vs, eo) ->
        serialize_variant "Record" @@ serialize_seq  ([
            build_list (build_pair (serialize_loc serialize_long_ident) serialize_expression) vs;
            build_option serialize_expression eo;
        ])
    | Pexp_field(e, n) ->
        serialize_variant "Field" @@ serialize_seq ([
            serialize_expression e;
            serialize_loc serialize_long_ident n;
        ])
    | Pexp_setfield(e, n, v) ->
        serialize_variant "SetField" @@ serialize_seq ([
            serialize_expression e;
            serialize_loc serialize_long_ident n;
            serialize_expression v;
        ])
    | Pexp_array(es) -> serialize_variant "serialize_seq " @@ build_list serialize_expression es
    | Pexp_ifthenelse(c, t, e) ->
        serialize_variant "IfThenElse" @@ serialize_seq ([
            serialize_expression c;
            serialize_expression t;
            build_option serialize_expression e;
        ])
    | Pexp_sequence(e1, e2) ->
        serialize_variant "Sequence" @@ serialize_seq ([
            serialize_expression e1;
            serialize_expression e2;
        ])
    | Pexp_while(c, l) ->
        serialize_variant "While" @@ serialize_seq ([
            serialize_expression c;
            serialize_expression l;
        ])
    | Pexp_for(p, i, n, d, b) ->
        serialize_variant "For" @@ serialize_seq ([
            serialize_pattern p;
            serialize_expression i;
            serialize_expression n;
            serialize_direction_flag d;
            serialize_expression b;
        ])
    | Pexp_constraint(e, t) ->
        serialize_variant "Constraint" @@ serialize_seq ([
            serialize_expression e;
            serialize_core_type t;
        ])
    | Pexp_coerce(e, t1, t2) ->
        serialize_variant "Coerce" @@ serialize_seq ([
            serialize_expression e;
            build_option serialize_core_type t1;
            serialize_core_type t2;
        ])
    | Pexp_send(e, n) ->
        serialize_variant "Send" @@ serialize_seq ([
            serialize_expression e;
            serialize_loc serialize_label n;
        ])
    | Pexp_new(n) -> serialize_variant "New" @@ serialize_loc serialize_long_ident n
    | Pexp_setinstvar(n, e) ->
        serialize_variant "SetInstVar" @@ serialize_seq ([
            serialize_loc serialize_label n;
            serialize_expression e;
        ])
    | Pexp_override(bs) -> serialize_variant "Override" @@ build_list
        (build_pair (serialize_loc serialize_label) serialize_expression) bs
    | Pexp_letmodule(n, m, e) ->
        serialize_variant "LetModule" @@ serialize_seq ([
            serialize_loc serialize_string n;
            serialize_module_expr m;
            serialize_expression e;
        ])
    | Pexp_letexception(ec, e) ->
        serialize_variant "LetException" @@ serialize_seq ([
            serialize_extension_constructor ec;
            serialize_expression e;
        ])
    | Pexp_assert(expr) -> serialize_variant "Assert" @@ serialize_expression expr
    | Pexp_lazy(e) -> serialize_variant "Lazy" @@ serialize_expression e
    | Pexp_poly(e, t) ->
        serialize_variant "Poly" @@ serialize_seq ([
            serialize_expression e;
            build_option serialize_core_type t;
        ])
    | Pexp_object(cs) -> serialize_variant "Object" @@ serialize_class_structure cs
    | Pexp_newtype(n, e) ->
        serialize_variant "NewType" @@ serialize_seq ([
            serialize_loc serialize_string n;
            serialize_expression e;
        ])
    | Pexp_pack(me) -> serialize_variant "Pack" @@ serialize_module_expr me
    | Pexp_open(o, n, e) ->
        serialize_variant "Open" @@ serialize_seq ([
            serialize_override_flag o;
            serialize_loc serialize_long_ident n;
            serialize_expression e;
        ])
    | Pexp_extension(e) -> serialize_variant "Extension" @@ serialize_extension e
    | Pexp_unreachable -> serialize_variant "Unreachable" serialize_null

    and serialize_case (c: case) : S.t =
        serialize_struct "Case" ([
            ("pat", serialize_pattern c.pc_lhs);
            ("guard", build_option serialize_expression c.pc_guard);
            ("expr", serialize_expression c.pc_rhs);
        ])

    and serialize_value_description (vd: value_description) : S.t =
        serialize_struct "ValueDescription" ([
            ("name", serialize_loc serialize_string vd.pval_name);
            ("type", serialize_core_type vd.pval_type);
            ("prim", build_list serialize_string vd.pval_prim);
            ("attributes", serialize_attributes vd.pval_attributes);
            ("location", serialize_location vd.pval_loc);
        ])

    and serialize_type_declaration (d: type_declaration) : S.t =
        serialize_struct "TypeDeclaration" ([
            ("name", serialize_loc serialize_string d.ptype_name);
            ("params", build_list (build_pair serialize_core_type serialize_variance) d.ptype_params);
            ("cstrs", build_list
               (build_triple serialize_core_type serialize_core_type serialize_location)
               d.ptype_cstrs);
            ("kind", serialize_type_kind d.ptype_kind);
            ("private", serialize_private_flag d.ptype_private);
            ("manifest", build_option serialize_core_type d.ptype_manifest);
            ("attributes", serialize_attributes d.ptype_attributes);
            ("location", serialize_location d.ptype_loc);
        ])

    and serialize_type_kind = function
    | Ptype_abstract -> serialize_variant "Abstract" serialize_null
    | Ptype_variant(cds) -> serialize_variant "Variant" @@ build_list serialize_constructor_declaration cds
    | Ptype_record(lds) -> serialize_variant "Record" @@ build_list serialize_label_declaration lds
    | Ptype_open -> serialize_variant "Open" serialize_null

    and serialize_label_declaration (decl: label_declaration) =
        serialize_struct "LabelDeclaration" ([
            ("name", serialize_loc serialize_string decl.pld_name);
            ("mutable", serialize_mutable_flag decl.pld_mutable);
            ("type", serialize_core_type decl.pld_type);
            ("location", serialize_location decl.pld_loc);
            ("attributes", serialize_attributes decl.pld_attributes);
        ])

    and serialize_constructor_declaration (cd: constructor_declaration) : S.t =
        serialize_struct "ConstructorDeclaration" ([
            ("name", serialize_loc serialize_string cd.pcd_name);
            ("args", serialize_constructor_arguments cd.pcd_args);
            ("res", build_option serialize_core_type cd.pcd_res);
            ("location", serialize_location cd.pcd_loc);
            ("attributes", serialize_attributes cd.pcd_attributes);
        ])

    and serialize_constructor_arguments = function
    | Pcstr_tuple(tys) -> serialize_variant "Tuple" @@ build_list serialize_core_type tys
    | Pcstr_record(ls) -> serialize_variant "Record" @@ build_list serialize_label_declaration ls

    and serialize_type_extension (te: type_extension) : S.t =
        serialize_struct "TypeExtension" ([
            ("path", serialize_loc serialize_long_ident te.ptyext_path);
            ("params", build_list (build_pair serialize_core_type serialize_variance) te.ptyext_params);
            ("constructors", build_list serialize_extension_constructor te.ptyext_constructors);
            ("private", serialize_private_flag te.ptyext_private);
            ("attributes", serialize_attributes te.ptyext_attributes);
        ])

    and serialize_extension_constructor (ctor: extension_constructor) =
        serialize_struct "ExtensionConstructor" ([
            ("name", serialize_loc serialize_string ctor.pext_name);
            ("kind", serialize_extension_constructor_kind ctor.pext_kind);
            ("location", serialize_location ctor.pext_loc);
            ("attributes", serialize_attributes ctor.pext_attributes);
        ])

    and serialize_extension_constructor_kind = function
    | Pext_decl(args, ty) ->
        serialize_variant "Decl" @@ serialize_seq ([
            serialize_constructor_arguments args;
            build_option serialize_core_type ty;
        ])
    | Pext_rebind(id) -> serialize_variant "Rebind" @@ serialize_loc serialize_long_ident id


    (*************************************************************************)
    (***************************** CLASS LANGUAGE ****************************)
    (*************************************************************************)

    and serialize_class_type (ct: class_type) : S.t =
        serialize_struct "ClassType" ([
            ("desc", serialize_class_type_desc ct.pcty_desc);
            ("location", serialize_location ct.pcty_loc);
            ("attributes", serialize_attributes ct.pcty_attributes);
        ])

    and serialize_class_type_desc : class_type_desc -> S.t = function
    | Pcty_constr(name, types) ->
        serialize_variant "Constr" @@ serialize_seq ([
            serialize_loc serialize_long_ident name;
            build_list serialize_core_type types;
        ])
    | Pcty_signature(s) -> serialize_variant "Signature" @@ serialize_class_signature s
    | Pcty_arrow(n, t1, t2) ->
        serialize_variant "Arrow" @@ serialize_seq ([
            serialize_arg_label n;
            serialize_core_type t1;
            serialize_class_type t2;
        ])
    | Pcty_extension(e) -> serialize_variant "Extension" @@ serialize_extension e
    | Pcty_open(o, n, t) ->
        serialize_variant "Open" @@ serialize_seq ([
            serialize_override_flag o;
            serialize_loc serialize_long_ident n;
            serialize_class_type t;
        ])

    and serialize_class_signature (cs: class_signature) : S.t =
        serialize_struct "ClassSignature" ([
            ("self", serialize_core_type cs.pcsig_self);
            ("fields", build_list serialize_class_type_field cs.pcsig_fields);
        ])

    and serialize_class_type_field (ctf: class_type_field) : S.t =
        serialize_struct "ClassTypeField" ([
            ("desc", serialize_class_type_field_desc ctf.pctf_desc);
            ("location", serialize_location ctf.pctf_loc);
            ("attributes", serialize_attributes ctf.pctf_attributes);
        ])

    and serialize_class_type_field_desc = function
    | Pctf_inherit(ct) -> serialize_variant "Inherit" @@ serialize_class_type ct
    | Pctf_val(n, m, v, t) ->
        serialize_variant "Val" @@ serialize_seq ([
            serialize_loc serialize_label n;
            serialize_mutable_flag m;
            serialize_virtual_flag v;
            serialize_core_type t;
        ])
    | Pctf_method(n, p, v, t) ->
        serialize_variant "Method" @@ serialize_seq ([
            serialize_loc serialize_label n;
            serialize_private_flag p;
            serialize_virtual_flag v;
            serialize_core_type t;
        ])
    | Pctf_constraint(t1, t2) ->
        serialize_variant "Constraint" @@ serialize_seq ([
            serialize_core_type t1;
            serialize_core_type t2;
        ])
    | Pctf_attribute(a) -> serialize_variant "Attribute" @@ serialize_attribute a
    | Pctf_extension(e) -> serialize_variant "Extension" @@ serialize_extension e

    and serialize_class_infos : 'a. ('a -> S.t) -> ('a class_infos) -> S.t =
        fun f ci -> serialize_struct "ClassInfos" ([
            ("virt", serialize_virtual_flag ci.pci_virt);
            ("params", build_list (build_pair serialize_core_type serialize_variance) ci.pci_params);
            ("name", serialize_loc serialize_string ci.pci_name);
            ("expr", f ci.pci_expr);
            ("location", serialize_location ci.pci_loc);
            ("attributes", serialize_attributes ci.pci_attributes);
        ])

    and serialize_class_description (cd: class_description) : S.t =
        serialize_class_infos serialize_class_type cd

    and serialize_class_type_declaration (cd: class_type_declaration) : S.t =
        serialize_class_infos serialize_class_type cd

    and serialize_class_expr (ce: class_expr) : S.t =
        serialize_struct "ClassExpr" ([
            ("desc", serialize_class_expr_desc ce.pcl_desc);
            ("location", serialize_location ce.pcl_loc);
            ("attributes", serialize_attributes ce.pcl_attributes);
        ])

    and serialize_class_expr_desc : class_expr_desc -> S.t = function
    | Pcl_constr(name, types) ->
        serialize_variant "Constr" @@ serialize_seq ([
            serialize_loc serialize_long_ident name;
            build_list serialize_core_type types;
        ])
    | Pcl_structure(cl) -> serialize_variant "Structure" @@ serialize_class_structure cl
    | Pcl_fun(l, eo, p, e) ->
        serialize_variant "Fun" @@ serialize_seq ([
            serialize_arg_label l;
            build_option serialize_expression eo;
            serialize_pattern p;
            serialize_class_expr e;
        ])
    | Pcl_apply(e, a) ->
        serialize_variant "Apply" @@ serialize_seq ([
            serialize_class_expr e;
            build_list (build_pair serialize_arg_label serialize_expression) a;
        ])
    | Pcl_let(r, bs, e) ->
        serialize_variant "Let" @@ serialize_seq ([
            serialize_rec_flag r;
            build_list serialize_value_binding bs;
            serialize_class_expr e;
        ])
    | Pcl_constraint(e, t) ->
        serialize_variant "Constraint" @@ serialize_seq ([
            serialize_class_expr e;
            serialize_class_type t;
        ])
    | Pcl_extension(ext) -> serialize_variant "Extension" @@ serialize_extension ext
    | Pcl_open(o, n, e) ->
        serialize_variant "Open" @@ serialize_seq ([
            serialize_override_flag o;
            serialize_loc serialize_long_ident n;
            serialize_class_expr e;
        ])

    and serialize_class_structure (cs: class_structure) : S.t =
        serialize_struct "ClassStructure" ([
            ("self", serialize_pattern cs.pcstr_self);
            ("fields", build_list serialize_class_field cs.pcstr_fields);
        ])

    and serialize_class_field (cf: class_field) : S.t =
        serialize_struct "ClassField" ([
            ("desc", serialize_class_field_desc cf.pcf_desc);
            ("location", serialize_location cf.pcf_loc);
            ("attributes", serialize_attributes cf.pcf_attributes);
        ])

    and serialize_class_field_desc = function
    | Pcf_inherit(o, e, n) ->
        serialize_variant "Inherit" @@ serialize_seq ([
            serialize_override_flag o;
            serialize_class_expr e;
            build_option (serialize_loc serialize_string) n;
        ])
    | Pcf_val(n, m, k) ->
        serialize_variant "Val" @@ serialize_seq ([
            serialize_loc serialize_label n;
            serialize_mutable_flag m;
            serialize_class_field_kind k;
        ])
    | Pcf_method(n, p, k) ->
        serialize_variant "Val" @@ serialize_seq ([
            serialize_loc serialize_label n;
            serialize_private_flag p;
            serialize_class_field_kind k;
        ])
    | Pcf_constraint(t1, t2) ->
        serialize_variant "Constraint" @@ serialize_seq ([
            serialize_core_type t1;
            serialize_core_type t2;
        ])
    | Pcf_initializer(e) -> serialize_variant "Initializer" @@ serialize_expression e
    | Pcf_attribute(a) -> serialize_variant "Attribute" @@ serialize_attribute a
    | Pcf_extension(e) -> serialize_variant "Extension" @@ serialize_extension e

    and serialize_class_field_kind = function
    | Cfk_virtual(t) -> serialize_variant "Virtual" @@ serialize_core_type t
    | Cfk_concrete(o, e) ->
        serialize_variant "Concrete" @@ serialize_seq ([
            serialize_override_flag o;
            serialize_expression e;
        ])

    and serialize_class_declaration (cd: class_declaration) : S.t =
        serialize_class_infos serialize_class_expr cd

    (*************************************************************************)
    (***************************** MODULE LANGUAGE ***************************)
    (*************************************************************************)

    and serialize_module_type (mt: module_type) : S.t =
        serialize_struct "ModuleType" ([
            ("desc", serialize_module_type_desc mt.pmty_desc);
            ("location", serialize_location mt.pmty_loc);
            ("attributes", serialize_attributes mt.pmty_attributes);
        ])

    and serialize_module_type_desc = function
    | Pmty_ident(ident) -> serialize_variant "Ident" @@ serialize_loc serialize_long_ident ident
    | Pmty_signature(s) -> serialize_variant "Signature" @@ serialize_signature s
    | Pmty_functor(name, arg, body) ->
        serialize_variant "Functor" @@ serialize_seq ([
            serialize_loc serialize_string name;
            build_option serialize_module_type arg;
            serialize_module_type body;
        ])
    | Pmty_with(mt, wcs) ->
        serialize_variant "With" @@ serialize_seq ([
            serialize_module_type mt;
            build_list serialize_with_constraint wcs;
        ])
    | Pmty_typeof(me) -> serialize_variant "TypeOf" @@ serialize_module_expr me
    | Pmty_extension(e) -> serialize_variant "Extension" @@ serialize_extension e
    | Pmty_alias(n) -> serialize_variant "Alias" @@ serialize_loc serialize_long_ident n

    and serialize_signature (s: signature) : S.t =
        build_list serialize_signature_item s

    and serialize_signature_item (si: signature_item) : S.t =
        serialize_struct "SignatureItem" ([
            ("desc", serialize_signature_item_desc si.psig_desc);
            ("location", serialize_location si.psig_loc);
        ])

    and serialize_signature_item_desc = function
    | Psig_value(vd) -> serialize_variant "Value" @@ serialize_value_description vd
    | Psig_type(r, tds) ->
        serialize_variant "Type" @@ serialize_seq ([
            serialize_rec_flag r;
            build_list serialize_type_declaration tds;
        ])
    | Psig_typext(te) -> serialize_variant "TypeExt" @@ serialize_type_extension te
    | Psig_exception(ec) -> serialize_variant "Exception" @@ serialize_extension_constructor ec
    | Psig_module(md) -> serialize_variant "Module" @@ serialize_module_declaration md
    | Psig_recmodule(mds) -> serialize_variant "RecModule" @@ build_list serialize_module_declaration mds
    | Psig_modtype(mt) -> serialize_variant "ModType" @@ serialize_module_type_declaration mt
    | Psig_open(o) -> serialize_variant "Open" @@ serialize_open_description o
    | Psig_include(id) -> serialize_variant "Include" @@ serialize_include_description id
    | Psig_class(cds) -> serialize_variant "Class" @@ build_list serialize_class_description cds
    | Psig_class_type(ctds) -> serialize_variant "ClassType" @@ build_list serialize_class_type_declaration ctds
    | Psig_attribute(a) -> serialize_variant "Attribute" @@ serialize_attribute a
    | Psig_extension(e, a) ->
        serialize_variant "Extension" @@ serialize_seq ([
            serialize_extension e;
            serialize_attributes a;
        ])

    and serialize_module_declaration (md: module_declaration) : S.t =
        serialize_struct "ModuleDeclaration" ([
            ("name", serialize_loc serialize_string md.pmd_name);
            ("type", serialize_module_type md.pmd_type);
            ("attributes", serialize_attributes md.pmd_attributes);
            ("location", serialize_location md.pmd_loc);
        ])

    and serialize_module_type_declaration (mtd: module_type_declaration) : S.t =
        serialize_struct "ModuleTypeDeclaration" ([
            ("name", serialize_loc serialize_string mtd.pmtd_name);
            ("type", build_option serialize_module_type mtd.pmtd_type);
            ("location", serialize_location mtd.pmtd_loc);
            ("attributes", serialize_attributes mtd.pmtd_attributes);
        ])

    and serialize_open_description (o: open_description) : S.t =
        serialize_struct "OpenDescription" ([
            ("id", serialize_loc serialize_long_ident o.popen_lid);
            ("override", serialize_override_flag o.popen_override);
            ("location", serialize_location o.popen_loc);
            ("attributes", serialize_attributes o.popen_attributes);
        ])

    and serialize_include_infos: 'a. ('a -> S.t) -> ('a include_infos) -> S.t =
        fun f ii ->
        serialize_struct "IncludeInfos" ([
            ("mod", f ii.pincl_mod);
            ("location", serialize_location ii.pincl_loc);
            ("attributes", serialize_attributes ii.pincl_attributes);
        ])

    and serialize_include_description (id: include_description) : S.t =
        serialize_include_infos serialize_module_type id

    and serialize_include_declaration (id: include_declaration) : S.t =
        serialize_include_infos serialize_module_expr id

    and serialize_with_constraint = function
    | Pwith_type(name, td) ->
        serialize_variant "Type" @@ serialize_seq ([
            serialize_loc serialize_long_ident name;
            serialize_type_declaration td;
        ])
    | Pwith_module(name, m) ->
        serialize_variant "Module" @@ serialize_seq ([
            serialize_loc serialize_long_ident name;
            serialize_loc serialize_long_ident m;
        ])
    | Pwith_typesubst(name, td) ->
        serialize_variant "TypeSubst" @@ serialize_seq ([
            serialize_loc serialize_long_ident name;
            serialize_type_declaration td;
        ])
    | Pwith_modsubst(name, m) ->
        serialize_variant "ModuleSubst" @@ serialize_seq ([
            serialize_loc serialize_long_ident name;
            serialize_loc serialize_long_ident m;
        ])

    and serialize_module_expr (me: module_expr) : S.t =
        serialize_struct "ModuleExpr" ([
            ("desc", serialize_module_expr_desc me.pmod_desc);
            ("location", serialize_location me.pmod_loc);
            ("attributes", serialize_attributes me.pmod_attributes);
        ])

    and serialize_module_expr_desc = function
    | Pmod_ident(ident) -> serialize_variant "Ident" @@ serialize_loc serialize_long_ident ident;
    | Pmod_structure(s) -> serialize_variant "Structure" @@ serialize_structure s
    | Pmod_functor(n, t, e) ->
        serialize_variant "Functor" @@ serialize_seq ([
            serialize_loc serialize_string n;
            build_option serialize_module_type t;
            serialize_module_expr e;
        ])
    | Pmod_apply(e1, e2) ->
        serialize_variant "Apply" @@ serialize_seq ([
            serialize_module_expr e1;
            serialize_module_expr e2;
        ])
    | Pmod_constraint(e, t) ->
        serialize_variant "Constraint" @@ serialize_seq ([
            serialize_module_expr e;
            serialize_module_type t;
        ])
    | Pmod_unpack(e) -> serialize_variant "Unpack" @@ serialize_expression e
    | Pmod_extension(e) -> serialize_variant "Extension" @@ serialize_extension e

    and serialize_structure (s: structure) : S.t =
        build_list serialize_structure_item s

    and serialize_structure_item (si: structure_item) : S.t =
        serialize_struct "StructureItem" ([
            ("location", serialize_location si.pstr_loc);
            ("desc", serialize_structure_item_desc si.pstr_desc);
        ])

    and serialize_structure_item_desc = function
    | Pstr_eval(e, a) -> serialize_variant "Eval" @@ serialize_seq ([
                             serialize_expression e;
                             serialize_attributes a;
                         ])
    | Pstr_value(r, bs) -> serialize_variant "Value" @@ serialize_seq ([
                               serialize_rec_flag r;
                               build_list serialize_value_binding bs;
                           ])
    | Pstr_primitive(vd) -> serialize_variant "Primitive" @@ serialize_value_description vd
    | Pstr_type(r, ds) -> serialize_variant "Type" @@ serialize_seq ([
                               serialize_rec_flag r;
                               build_list serialize_type_declaration ds;
                          ])
    | Pstr_typext(te) -> serialize_variant "TypeExt" @@ serialize_type_extension te
    | Pstr_exception(c) -> serialize_variant "Exception" @@ serialize_extension_constructor c
    | Pstr_module(mb) -> serialize_variant "Module" @@ serialize_module_binding mb
    | Pstr_recmodule(mbs) -> serialize_variant "RecModule" @@ build_list serialize_module_binding mbs
    | Pstr_modtype(mtd) -> serialize_variant "ModType" @@ serialize_module_type_declaration mtd
    | Pstr_open(o) -> serialize_variant "Open" @@ serialize_open_description o
    | Pstr_class(cds) -> serialize_variant "Class" @@ build_list serialize_class_declaration cds
    | Pstr_class_type(ctds) -> serialize_variant "ClassType" @@ build_list serialize_class_type_declaration ctds
    | Pstr_include(id) -> serialize_variant "Include" @@ serialize_include_declaration id
    | Pstr_attribute(a) -> serialize_variant "Attribute" @@ serialize_attribute a
    | Pstr_extension(e, a) ->
        serialize_variant "Extension" @@ serialize_seq ([
            serialize_extension e;
            serialize_attributes a;
        ])

    and serialize_value_binding (vb: value_binding) : S.t =
        serialize_struct "ValueBinding" ([
            ("pat", serialize_pattern vb.pvb_pat);
            ("expr", serialize_expression vb.pvb_expr);
            ("attributes", serialize_attributes vb.pvb_attributes);
            ("location", serialize_location vb.pvb_loc);
        ])

    and serialize_module_binding (mb: module_binding) : S.t =
        serialize_struct "ModuleBinding" ([
            ("name", serialize_loc serialize_string mb.pmb_name);
            ("expr", serialize_module_expr mb.pmb_expr);
            ("location", serialize_location mb.pmb_loc);
            ("attributes", serialize_attributes mb.pmb_attributes)
        ])

    (*************************************************************************)
    (******************************** TOPLEVEL *******************************)
    (*************************************************************************)

    let serialize_directive_argument = function
    | Pdir_none -> serialize_variant "None" @@ serialize_null
    | Pdir_string(s) -> serialize_variant "String" @@ serialize_string s
    | Pdir_int(c, s) ->
        serialize_variant "Int" @@ serialize_seq ([
            serialize_string c;
            build_option serialize_char s;
        ])
    | Pdir_ident(i) -> serialize_variant "Ident" @@ serialize_long_ident i
    | Pdir_bool(b) -> serialize_variant "Bool" @@ serialize_bool b

    let serialize_toplevel_phrase = function
    | Ptop_def(s) -> serialize_variant "Def" @@ serialize_structure s
    | Ptop_dir(n, a) ->
        serialize_variant "Dir" @@ serialize_seq ([
            serialize_string n;
            serialize_directive_argument a;
        ])

    (*************************************************************************)
    (***************************** ERROR HANDLING ****************************)
    (*************************************************************************)

    let serialize_lexer_error = function
    | Lexer.Illegal_character(ch) -> serialize_variant "IllegalCharacter" @@ serialize_char ch
    | Lexer.Illegal_escape(esc) -> serialize_variant "IllegalEscape" @@ serialize_string esc
    | Lexer.Unterminated_comment(loc) -> serialize_variant "UnterminatedComment" @@ serialize_location loc
    | Lexer.Unterminated_string -> serialize_variant "UnterminatedString" @@ serialize_null
    | Lexer.Unterminated_string_in_comment(l1, l2) ->
        serialize_variant "UnterminatedStringInComment" @@ serialize_seq ([
            serialize_location l1;
            serialize_location l2;
        ])
    | Lexer.Keyword_as_label(str) -> serialize_variant "KeywordAsLabel" @@ serialize_string str
    | Lexer.Invalid_literal(str) -> serialize_variant "InvalidLiteral" @@ serialize_string str
    | Lexer.Invalid_directive(dir, expl) ->
        serialize_variant "InvalidDirective" @@ serialize_seq ([
            serialize_string dir;
            build_option serialize_string expl;
        ])

    let serialize_syntax_error = function
    | Syntaxerr.Unclosed(la, sa, lb, sb) ->
        serialize_variant "Unclosed" @@ serialize_seq ([
            serialize_location la;
            serialize_string sa;
            serialize_location lb;
            serialize_string sb;
        ])
    | Syntaxerr.Expecting(loc, str) ->
        serialize_variant "Expecting" @@ serialize_seq ([
            serialize_location loc;
            serialize_string str;
        ])
    | Syntaxerr.Not_expecting(loc, str) ->
        serialize_variant "NotExpecting" @@ serialize_seq ([
            serialize_location loc;
            serialize_string str;
        ])
    | Syntaxerr.Applicative_path(loc) -> serialize_variant "ApplicativePath" @@ serialize_location loc
    | Syntaxerr.Variable_in_scope(loc, str) ->
        serialize_variant "VariableInScope" @@ serialize_seq ([
            serialize_location loc;
            serialize_string str;
        ])
    | Syntaxerr.Other(loc) -> serialize_variant "Other" @@ serialize_location loc
    | Syntaxerr.Ill_formed_ast(loc, str) ->
        serialize_variant "IllFormedAst" @@ serialize_seq ([
            serialize_location loc;
            serialize_string str;
        ])
    | Syntaxerr.Invalid_package_type(loc, str) ->
        serialize_variant "InvalidPackageType" @@ serialize_seq ([
            serialize_location loc;
            serialize_string str;
        ])

    (*************************************************************************)
    (***************************** PARSE FUNCTION ****************************)
    (*************************************************************************)

    let serialize_ast (ast: Parsetree.toplevel_phrase list) : S.t =
        build_list serialize_toplevel_phrase ast

    let serialize_ast_from ~src:src ~path:path : S.t =
        serialize_ast (parse_ast src path)
end
