use std::ffi::{CStr, CString, NulError};
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
    fn ocaml_ast_init(exec_name: *const c_char) -> c_int;

    fn ocaml_ast_parse(
        src: *const c_char,
        path: *const c_char,
        out_data: *mut out_data,
    ) -> c_int;
}

pub fn init(s: &str) -> Result<bool, NulError> {
    let cstr = CString::new(s)?;
    let raw = cstr.into_raw();

    let ret = unsafe { ocaml_ast_init(raw) };
    match ret {
        0 => Ok(true),
        1 => {
            let cstr = unsafe { CString::from_raw(raw) };
            drop(cstr);
            Ok(false)
        }
        n => panic!("Unexpected return value from ocaml_ast_init: {}", n),
    }
}

pub fn parse(
    src: &str,
    filename: Option<&str>,
) -> Result<String, &'static str> {
    let src = CString::new(src).unwrap(); // TODO Handle error
    let filename = filename.map(|f| {
        CString::new(f).unwrap() // TODO Handle error
    });

    let mut out_data: out_data = unsafe { zeroed() };
    let ret = {
        let filename = match filename.as_ref() {
            Some(f) => f.as_ptr(),
            None => null(),
        };
        unsafe { ocaml_ast_parse(src.as_ptr(), filename, &mut out_data) }
    };

    drop(src);
    drop(filename);

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
