use crate::{
    gc::calibre_gc_add_root,
    values::{GcObject, Value, ValueTag, register_alloc},
};

#[repr(C)]
pub struct GcList {
    pub len: usize,
    pub capacity: usize,
    pub data: Vec<Value>,
}

#[unsafe(no_mangle)]
pub extern "C" fn calibre_gc_alloc_list(capacity: usize) -> *mut GcList {
    let header = register_alloc(GcObject::List(GcList {
        len: 0,
        capacity,
        data: Vec::with_capacity(capacity),
    }));

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

            if index < (*lst).capacity {
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
