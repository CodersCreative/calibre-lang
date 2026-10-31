use crate::values::{
    Value, ValueTag,
    aggregate::GcAggregate,
    enums::{calibre_option_none, calibre_option_some},
    list::GcList,
};

#[unsafe(no_mangle)]
pub extern "C" fn calibre_list_raw_remove(lst: *mut GcList, index: u64) -> *mut GcAggregate {
    if lst.is_null() {
        return calibre_option_none();
    }

    unsafe {
        if (index as usize) < (*lst).data.len() {
            let value = (*lst).data.remove(index as usize);
            let (tag, payload) = value.to_tag_and_payload();
            calibre_option_some(tag, payload)
        } else {
            calibre_option_none()
        }
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn calibre_list_raw_insert(
    lst: *mut GcList,
    index: u64,
    tag: ValueTag,
    payload: i64,
) -> bool {
    if lst.is_null() {
        return false;
    }

    unsafe {
        if (index as usize) < (*lst).data.len() {
            (*lst)
                .data
                .insert(index as usize, Value::from_tag(tag, payload));
            true
        } else {
            false
        }
    }
}
