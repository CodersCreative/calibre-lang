use crate::values::{normalize_index, resolve_slice_range};

#[unsafe(no_mangle)]
pub extern "C" fn calibre_range_index(range: i64, index: i64) -> i64 {
    let start = range as i32 as i64;
    let end = (range >> 32) as i32 as i64;

    if let Some(i) = normalize_index((end - start).max(0) as u64, index) {
        start + i as i64
    } else {
        0
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
        ((to_i32 as i64) << 32) | (from_i32 as i64 & 0xFFFFFFFF)
    } else {
        0
    }
}
