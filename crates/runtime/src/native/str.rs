use crate::values::{Value, list::GcList, string::GcString};

#[unsafe(no_mangle)]
pub extern "C" fn calibre_str_char_lowercase(char: u32) -> u32 {
    let char = unsafe { char::from_u32_unchecked(char) };
    char.to_lowercase().next().unwrap_or(char) as u32
}

#[unsafe(no_mangle)]
pub extern "C" fn calibre_str_char_uppercase(char: u32) -> u32 {
    let char = unsafe { char::from_u32_unchecked(char) };
    char.to_uppercase().next().unwrap_or(char) as u32
}

#[unsafe(no_mangle)]
pub extern "C" fn calibre_str_split(text: *mut GcString, delim: *mut GcString) -> *mut GcList {
    if text.is_null() || delim.is_null() {
        return GcList::default().alloc();
    }

    unsafe {
        let delim = (*delim).data.as_str();

        GcList {
            data: if delim.is_empty() {
                (*text)
                    .data
                    .chars()
                    .map(|x| {
                        Value::String(
                            GcString {
                                data: x.to_string(),
                            }
                            .alloc(),
                        )
                    })
                    .collect()
            } else {
                (*text)
                    .data
                    .split(delim)
                    .map(|x| {
                        Value::String(
                            GcString {
                                data: x.to_string(),
                            }
                            .alloc(),
                        )
                    })
                    .collect()
            },
        }
        .alloc()
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn calibre_str_contains(text: *mut GcString, needle: *mut GcString) -> bool {
    if text.is_null() || needle.is_null() {
        return false;
    }

    unsafe { (*text).data.contains((*needle).data.as_str()) }
}

#[unsafe(no_mangle)]
pub extern "C" fn calibre_str_starts_with(text: *mut GcString, prefix: *mut GcString) -> bool {
    if text.is_null() || prefix.is_null() {
        return false;
    }

    unsafe { (*text).data.starts_with((*prefix).data.as_str()) }
}

#[unsafe(no_mangle)]
pub extern "C" fn calibre_str_ends_with(text: *mut GcString, suffix: *mut GcString) -> bool {
    if text.is_null() || suffix.is_null() {
        return false;
    }

    unsafe { (*text).data.ends_with((*suffix).data.as_str()) }
}
