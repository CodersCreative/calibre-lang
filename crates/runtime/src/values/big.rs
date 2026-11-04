use std::str::FromStr;

use crate::{
    BIG_PRECISION,
    gc::calibre_gc_add_root,
    values::{GcObject, register_alloc, string::GcString},
};
use astro_float::BigFloat;

pub fn alloc_big(value: BigFloat) -> *mut BigFloat {
    let header = register_alloc(GcObject::Big(value));

    unsafe {
        let object = &mut (*header).inner;

        let ptr = object as *mut GcObject as usize;
        calibre_gc_add_root(ptr);

        if let GcObject::Big(value) = object {
            value as *mut BigFloat
        } else {
            std::ptr::null_mut()
        }
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn calibre_gc_alloc_big_from_float(num: f64) -> *mut BigFloat {
    let num = BigFloat::from_f64(num, BIG_PRECISION);

    alloc_big(num)
}

#[unsafe(no_mangle)]
pub extern "C" fn calibre_gc_alloc_big(num: *mut GcString) -> *mut BigFloat {
    let num =
        BigFloat::from_str(unsafe { (*num).data.as_str() }).unwrap_or_else(|_| BigFloat::default());

    alloc_big(num)
}
