use crate::{
    gc::calibre_gc_add_root,
    values::{GcObject, register_alloc},
};

#[repr(C)]
pub struct GcString {
    pub len: usize,
    pub data: Vec<u8>,
}

#[unsafe(no_mangle)]
pub extern "C" fn calibre_gc_alloc_string(len: usize) -> *mut GcString {
    let header = register_alloc(GcObject::String(GcString {
        len,
        data: vec![0u8; len],
    }));

    unsafe {
        let object = &mut (*header).inner;

        let ptr = object as *mut GcObject as usize;
        calibre_gc_add_root(ptr);

        if let GcObject::String(value) = object {
            value as *mut GcString
        } else {
            std::ptr::null_mut()
        }
    }
}
