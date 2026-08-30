use crate::CalibreEngine;
use crate::standalone::CalibreStandalone;
use std::ffi::{CStr, CString};
use std::os::raw::c_char;
use std::ptr;

type OutputCallback = Option<unsafe extern "C" fn() -> *const c_char>;
type InputCallback = Option<unsafe extern "C" fn(*const c_char) -> *const c_char>;

#[repr(C)]
pub struct CalibreRunResult {
    pub return_value: *mut c_char,
    pub captured_output: *mut c_char,
}

#[repr(C)]
pub struct CalibreArtifacts {
    pub entry_name: *mut c_char,
    pub mappings: *mut *mut c_char,
    pub mappings_len: usize,
}

#[unsafe(no_mangle)]
pub extern "C" fn calibre_engine_new() -> *mut CalibreEngine {
    Box::into_raw(Box::new(CalibreEngine::default()))
}

#[unsafe(no_mangle)]
pub extern "C" fn calibre_engine_free(ptr: *mut CalibreEngine) {
    if ptr.is_null() {
        return;
    }

    unsafe {
        let _ = Box::from_raw(ptr);
    };
}

#[unsafe(no_mangle)]
pub extern "C" fn calibre_engine_set_no_std(ptr: *mut CalibreEngine, no_std: bool) {
    if ptr.is_null() {
        return;
    }

    let engine = unsafe { &mut *ptr };
    engine.set_no_std(no_std);
}

#[unsafe(no_mangle)]
pub extern "C" fn calibre_engine_set_entry_name(ptr: *mut CalibreEngine, name: *const c_char) {
    if ptr.is_null() || name.is_null() {
        return;
    }

    let engine = unsafe { &mut *ptr };
    let s = unsafe { CStr::from_ptr(name).to_string_lossy().into_owned() };
    engine.set_entry_name(s);
}

#[unsafe(no_mangle)]
pub extern "C" fn calibre_engine_add_prelude(ptr: *mut CalibreEngine, src: *const c_char) {
    if ptr.is_null() || src.is_null() {
        return;
    }

    let engine = unsafe { &mut *ptr };
    let s = unsafe { CStr::from_ptr(src).to_string_lossy().into_owned() };
    engine.add_prelude(s);
}

#[unsafe(no_mangle)]
pub extern "C" fn calibre_engine_add_input(ptr: *mut CalibreEngine, input: *const c_char) {
    if ptr.is_null() || input.is_null() {
        return;
    }

    let engine = unsafe { &mut *ptr };
    let s = unsafe { CStr::from_ptr(input).to_string_lossy().into_owned() };
    engine.add_input(s);
}

#[unsafe(no_mangle)]
pub extern "C" fn calibre_compile_source(
    ptr: *mut CalibreEngine,
    src: *const c_char,
) -> *mut CalibreArtifacts {
    if ptr.is_null() || src.is_null() {
        return ptr::null_mut();
    }

    let engine = unsafe { &mut *ptr };
    let source = unsafe { CStr::from_ptr(src).to_string_lossy().into_owned() };

    match engine.compile_source(source, false) {
        Ok(arts) => {
            let entry = CString::new(arts.entry_name).unwrap();
            let entry_ptr = entry.into_raw();

            let mut mapping_ptrs: Vec<*mut c_char> = arts
                .mappings
                .into_iter()
                .map(|m| CString::new(m).unwrap().into_raw())
                .collect();

            let mappings_len = mapping_ptrs.len();
            let mappings_buf = mapping_ptrs.as_mut_ptr();
            std::mem::forget(mapping_ptrs);

            let ca = CalibreArtifacts {
                entry_name: entry_ptr,
                mappings: mappings_buf,
                mappings_len,
            };

            Box::into_raw(Box::new(ca))
        }
        Err(_) => ptr::null_mut(),
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn calibre_artifacts_free(ptr: *mut CalibreArtifacts) {
    if ptr.is_null() {
        return;
    }

    let ca = unsafe { Box::from_raw(ptr) };

    if !ca.entry_name.is_null() {
        unsafe {
            let _ = CString::from_raw(ca.entry_name);
        }
    }

    if !ca.mappings.is_null() {
        let slice = unsafe { std::slice::from_raw_parts_mut(ca.mappings, ca.mappings_len) };
        for &p in slice.iter() {
            if !p.is_null() {
                unsafe {
                    let _ = CString::from_raw(p as *mut c_char);
                }
            }
        }
        unsafe {
            Vec::from_raw_parts(ca.mappings, ca.mappings_len, ca.mappings_len);
        }
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn calibre_engine_run_source(
    ptr: *mut CalibreEngine,
    src: *const c_char,
) -> *mut CalibreRunResult {
    if ptr.is_null() || src.is_null() {
        return ptr::null_mut();
    }

    let engine = unsafe { &mut *ptr };

    let source = unsafe { CStr::from_ptr(src).to_string_lossy().into_owned() };

    match engine.run_source(source) {
        Ok(result) => {
            let mut vm = result.vm;
            let ret_display = result.return_value.display(&mut vm);
            let captured = vm.captured_output.clone();

            let r = CalibreRunResult {
                return_value: CString::new(ret_display).unwrap().into_raw(),
                captured_output: CString::new(captured).unwrap().into_raw(),
            };

            Box::into_raw(Box::new(r))
        }
        Err(_) => ptr::null_mut(),
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn calibre_run_result_free(ptr: *mut CalibreRunResult) {
    if ptr.is_null() {
        return;
    }

    let rr = unsafe { Box::from_raw(ptr) };

    if !rr.return_value.is_null() {
        unsafe {
            let _ = CString::from_raw(rr.return_value);
        }
    }

    if !rr.captured_output.is_null() {
        unsafe {
            let _ = CString::from_raw(rr.captured_output);
        }
    }
}
