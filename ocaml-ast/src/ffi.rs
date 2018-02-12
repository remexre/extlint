use std::os::raw::{c_char, c_int};

#[repr(C)]
pub union out_data {
    pub success: *mut c_char,
    pub missing_value: *const c_char,
}

#[link(name = "ocaml_ast", kind = "static")]
extern "C" {
    pub fn ocaml_ast_init() -> c_int;

    pub fn ocaml_ast_parse(
        src: *const c_char,
        src_len: usize,
        path: *const c_char,
        path_len: usize,
        out_data: *mut out_data,
    ) -> c_int;
}
