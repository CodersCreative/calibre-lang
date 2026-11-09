use crate::{
    gc::calibre_gc_add_root,
    values::{GcObject, Value, ValueTag, normalize_index, register_alloc, resolve_slice_range},
};

#[repr(C)]
#[derive(Default, Clone)]
pub struct GcList {
    pub data: Vec<Value>,
}

impl GcList {
    pub fn alloc(self) -> *mut GcList {
        let header = register_alloc(GcObject::List(self));

        unsafe {
            let object = &mut (*header).inner;

            let ptr = object as *mut GcObject as usize;
            calibre_gc_add_root(ptr);

            if let GcObject::List(value) = object {
                value as *mut GcList
            } else {
                std::ptr::null_mut()
            }
        }
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn calibre_list_deep_clone(lst: *mut GcList) -> *mut GcList {
    if lst.is_null() {
        return lst;
    }

    let mut data = unsafe { &*lst }.clone();

    data.data = data.data.into_iter().map(|x| x.deep_clone()).collect();

    data.alloc()
}

#[unsafe(no_mangle)]
pub extern "C" fn calibre_gc_alloc_list(capacity: usize) -> *mut GcList {
    GcList {
        data: Vec::with_capacity(capacity),
    }
    .alloc()
}

#[unsafe(no_mangle)]
pub extern "C" fn calibre_list_set_index(
    lst: *mut GcList,
    index: usize,
    tag: ValueTag,
    payload: i64,
) {
    unsafe {
        if !lst.is_null() {
            let data = (*lst).data.as_mut_ptr();

            if index < (*lst).data.capacity() {
                *data.add(index) = Value::from_tag(tag, payload);
            }
        }
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn calibre_list_push(lst: *mut GcList, tag: ValueTag, payload: i64) {
    unsafe {
        if !lst.is_null() {
            (*lst).data.push(Value::from_tag(tag, payload));
        }
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn calibre_list_index(lst: *mut GcList, index: i64) -> *mut Value {
    if lst.is_null() {
        return std::ptr::null_mut();
    }

    unsafe {
        let items = &mut (*lst).data;
        if let Some(i) = normalize_index(items.len() as u64, index) {
            items.get_mut(i).unwrap() as *mut Value
        } else {
            std::ptr::null_mut()
        }
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn calibre_list_ref_index(lst: *mut Value, index: i64) -> *mut Value {
    if lst.is_null() {
        return lst;
    }

    match unsafe { &mut (*lst) } {
        Value::List(x) => calibre_list_index(*x, index),
        _ => lst,
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn calibre_list_slice(lst: *mut GcList, slice: i64) -> *mut GcList {
    if lst.is_null() {
        return std::ptr::null_mut();
    }

    let start = slice as i32 as i64;
    let end = (slice >> 32) as i32 as i64;

    unsafe {
        let items = &mut (*lst).data;
        if let Some((start, end)) = resolve_slice_range(items.len(), start, end) {
            GcList {
                data: items[start..end].to_vec(),
            }
            .alloc()
        } else {
            std::ptr::null_mut()
        }
    }
}
