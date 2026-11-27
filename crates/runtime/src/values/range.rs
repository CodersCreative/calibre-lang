use crate::values::aggregate::GcAggregate;
use crate::values::{
    Value, enums::calibre_option_none, invalid_index_panic, normalize_index, resolve_slice_range,
};

#[unsafe(no_mangle)]
pub extern "C" fn calibre_range_index(range: i64, index: i64) -> i64 {
    let start = range as i32 as i64;
    let end = (range >> 32) as i32 as i64;

    if let Some(i) = normalize_index((end - start).max(0) as u64, index) {
        start + i as i64
    } else {
        invalid_index_panic();
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn calibre_range_slice(range: i64, slice: i64) -> i64 {
    let start = range as i32 as i64;
    let end = (range >> 32) as i32 as i64;

    let s = slice as i32 as i64;
    let e = (slice >> 32) as i32 as i64;

    if let Some((s, e)) = resolve_slice_range((end - start).max(0) as usize, s, e) {
        let from_i32 = start + s as i64;
        let to_i32 = start + e as i64;
        (to_i32 << 32) | (from_i32 & 0xFFFFFFFF)
    } else {
        invalid_index_panic();
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn calibre_range_index_option(range: i64, index: i64) -> *mut GcAggregate {
    let start = range as i32 as i64;
    let end = (range >> 32) as i32 as i64;

    if let Some(i) = normalize_index((end - start).max(0) as u64, index) {
        Value::Int(start + i as i64).some()
    } else {
        calibre_option_none()
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn calibre_range_slice_option(range: i64, slice: i64) -> *mut GcAggregate {
    let start = range as i32 as i64;
    let end = (range >> 32) as i32 as i64;

    let s = slice as i32 as i64;
    let e = (slice >> 32) as i32 as i64;

    if let Some((s, e)) = resolve_slice_range((end - start).max(0) as usize, s, e) {
        Value::Range(start + s as i64, start + e as i64).some()
    } else {
        calibre_option_none()
    }
}
