use crate::{
    gc::calibre_gc_add_root,
    values::{GcObject, Value, ValueTag, normalize_index, register_alloc, resolve_slice_range},
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

#[unsafe(no_mangle)]
pub extern "C" fn calibre_to_string(tag: ValueTag, value: i64) -> *mut GcString {
    let value = Value::from_tag(tag, value);

    GcString {
        data: value.to_string(),
    }
    .alloc()
}

#[unsafe(no_mangle)]
pub extern "C" fn calibre_string_index(string: *mut GcString, index: i64) -> u32 {
    if string.is_null() {
        return 0;
    }

    unsafe {
        let items = &mut (*string).data;
        if let Some(i) = normalize_index(items.len() as u64, index) {
            items.chars().nth(i).unwrap() as u32
        } else {
            0
        }
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn calibre_string_slice(string: *mut GcString, slice: i64) -> *mut GcString {
    if string.is_null() {
        return std::ptr::null_mut();
    }

    let start = slice as i32 as i64;
    let end = (slice >> 32) as i32 as i64;

    unsafe {
        let items = &mut (*string).data;
        if let Some((start, end)) = resolve_slice_range(items.len(), start, end) {
            GcString {
                data: items[start..end].to_string(),
            }
            .alloc()
        } else {
            std::ptr::null_mut()
        }
    }
}
