use std::io::BufRead;

use crate::{
    gc::calibre_gc_add_root,
    values::{
        GcObject, Value, aggregate::GcAggregate, enums::calibre_option_none, register_alloc,
        string::GcString,
    },
};

pub struct GcReader(pub Box<dyn BufRead + Send + Sync>);

impl GcReader {
    pub fn alloc(self) -> *mut GcReader {
        let header = register_alloc(GcObject::Reader(self));

        unsafe {
            let object = &mut (*header).inner;

            let ptr = object as *mut GcObject as usize;
            calibre_gc_add_root(ptr);

            if let GcObject::Reader(value) = object {
                value as *mut GcReader
            } else {
                std::ptr::null_mut()
            }
        }
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn calibre_reader_next(reader: *mut GcReader) -> *mut GcAggregate {
    if reader.is_null() {
        return calibre_option_none();
    }

    let reader = unsafe { &mut *reader };

    let mut data = String::new();
    if reader.0.read_line(&mut data).is_ok() {
        Value::String(GcString { data }.alloc()).some()
    } else {
        calibre_option_none()
    }
}
