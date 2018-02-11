use std::ffi::{CStr, CString};
use std::mem::{forget, zeroed};
use std::os::raw::{c_char, c_int};
use std::ptr::null;

#[repr(C)]
union out_data {
    pub success: *mut c_char,
    pub missing_value: *const c_char,
}

#[link(name = "ocaml_ast", kind = "static")]
extern "C" {
    pub fn ocaml_ast_init() -> c_int;

    fn ocaml_ast_parse(
        src: *const c_char,
        path: *const c_char,
        out_data: *mut out_data,
    ) -> c_int;
}

pub fn parse(
    src: &[u8],
    filename: Option<&str>,
) -> Result<String, &'static str> {
    // Convert src and filename to be *const [i8] and Option<*const [i8]>,
    // respectively.
    let src = src.as_ptr() as *const i8;
    let filename = filename.map(|f| f.as_ptr()).unwrap_or(null()) as *const i8;

    let mut out_data: out_data = unsafe { zeroed() };
    let ret = unsafe { ocaml_ast_parse(src, filename, &mut out_data) };

    match ret {
        0 => {
            let s = unsafe { out_data.success };
            assert!(!s.is_null());
            let s = unsafe { CString::from_raw(s) };
            let s = s.into_string().unwrap(); // TODO Handle error
            Ok(s)
        }
        1 => {
            let s = unsafe { out_data.missing_value };
            assert!(!s.is_null());
            let s = unsafe { CStr::from_ptr::<'static>(s) };
            let s = s.to_str().unwrap(); // TODO Handle error
            Err(s)
        }
        n => {
            // out_data shouldn't have a destructor, but there's no harm to
            // being extra-safe.
            forget(out_data);

            panic!("Unexpected return value from ocaml_ast_parse: {}", n);
        }
    }
}
