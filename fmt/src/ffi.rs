use std::ffi::{CStr, CString};
use std::os::raw::c_char;
use std::ptr;

use calibre_parser::ast::formatter::Formatter;

#[repr(C)]
pub struct CalibreFmtResult {
    pub output: *mut c_char,
    pub error: *mut c_char,
}

impl CalibreFmtResult {
    fn ok(output: impl Into<String>) -> *mut Self {
        let output = CString::new(output.into()).unwrap();
        let result = Box::new(Self {
            output: output.into_raw(),
            error: ptr::null_mut(),
        });
        Box::into_raw(result)
    }

    fn err(message: impl Into<String>) -> *mut Self {
        let error = CString::new(message.into()).unwrap();
        let result = Box::new(Self {
            output: ptr::null_mut(),
            error: error.into_raw(),
        });
        Box::into_raw(result)
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn calibre_fmt_format(source: *const c_char) -> *mut CalibreFmtResult {
    if source.is_null() {
        return CalibreFmtResult::err("source pointer is null");
    }

    let source = unsafe { CStr::from_ptr(source).to_string_lossy().into_owned() };
    let mut formatter = Formatter {
        max_width: 150,
        ..Default::default()
    };

    match formatter.start_format(&source, None) {
        Ok(output) => CalibreFmtResult::ok(output),
        Err(err) => CalibreFmtResult::err(err.to_string()),
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn calibre_fmt_format_with_width(
    source: *const c_char,
    max_width: usize,
) -> *mut CalibreFmtResult {
    if source.is_null() {
        return CalibreFmtResult::err("source pointer is null");
    }

    let source = unsafe { CStr::from_ptr(source).to_string_lossy().into_owned() };
    let mut formatter = Formatter {
        max_width,
        ..Default::default()
    };

    match formatter.start_format(&source, None) {
        Ok(output) => CalibreFmtResult::ok(output),
        Err(err) => CalibreFmtResult::err(err.to_string()),
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn calibre_fmt_result_free(ptr: *mut CalibreFmtResult) {
    if ptr.is_null() {
        return;
    }

    let result = unsafe { Box::from_raw(ptr) };
    if !result.output.is_null() {
        unsafe {
            let _ = CString::from_raw(result.output);
        }
    }
    if !result.error.is_null() {
        unsafe {
            let _ = CString::from_raw(result.error);
        }
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn calibre_fmt_string_free(ptr: *mut c_char) {
    if ptr.is_null() {
        return;
    }

    unsafe {
        let _ = CString::from_raw(ptr);
    }
}
