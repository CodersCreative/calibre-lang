use regex::Regex;

use crate::values::{
    ValueTag,
    aggregate::GcAggregate,
    enums::{calibre_option_none, calibre_option_some},
    string::GcString,
};

#[unsafe(no_mangle)]
pub extern "C" fn calibre_regex_is_match(text: *mut GcString, pattern: *mut GcString) -> bool {
    if text.is_null() || pattern.is_null() {
        return false;
    }

    unsafe {
        let Ok(re) = Regex::new((*pattern).data.as_str()) else {
            return false;
        };
        re.is_match((*text).data.as_str())
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn calibre_regex_find(
    text: *mut GcString,
    pattern: *mut GcString,
) -> *mut GcAggregate {
    if text.is_null() || pattern.is_null() {
        return calibre_option_none();
    }

    unsafe {
        let Ok(re) = Regex::new((*pattern).data.as_str()) else {
            return calibre_option_none();
        };

        match re.find((*text).data.as_str()) {
            Some(x) => {
                let txt = GcString {
                    data: x.as_str().to_string(),
                }
                .alloc();
                calibre_option_some(ValueTag::String, txt as i64)
            }
            _ => calibre_option_none(),
        }
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn calibre_regex_replace(
    text: *mut GcString,
    pattern: *mut GcString,
    replacement: *mut GcString,
) -> *mut GcString {
    if text.is_null() || pattern.is_null() || replacement.is_null() {
        return text;
    }

    unsafe {
        let Ok(re) = Regex::new((*pattern).data.as_str()) else {
            return text;
        };

        let txt = re.replace_all((*text).data.as_str(), (*replacement).data.as_str());

        GcString {
            data: txt.to_string(),
        }
        .alloc()
    }
}
