use crate::{
    gc::calibre_gc_add_root,
    values::{GcObject, register_alloc},
};

#[repr(C)]
#[derive(Default)]
pub struct GcString {
    pub data: String,
}

impl GcString {
    pub fn alloc(self) -> *mut GcString {
        let header = register_alloc(GcObject::String(self));

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
}

#[unsafe(no_mangle)]
pub extern "C" fn calibre_gc_alloc_string(capacity: usize) -> *mut GcString {
    let data = String::from_utf8(vec![0u8; capacity]).unwrap_or_default();
    GcString { data }.alloc()
}

#[unsafe(no_mangle)]
pub extern "C" fn calibre_string_push(string: *mut GcString, char: u32) {
    unsafe {
        if !string.is_null() {
            (*string).data.push(char::from_u32_unchecked(char));
        }
    }
}
