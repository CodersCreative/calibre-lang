use crate::{
    gc::calibre_gc_add_root,
    values::{GcObject, Value, ValueTag, register_alloc},
};

#[repr(C)]
pub struct GcAggregate {
    pub field_count: usize,
    pub data: Vec<Value>,
}

impl GcAggregate {
    pub fn alloc(self) -> *mut GcAggregate {
        let header = register_alloc(GcObject::Aggregate(self));

        unsafe {
            let object = &mut (*header).inner;

            let ptr = object as *mut GcObject as usize;
            calibre_gc_add_root(ptr);

            if let GcObject::Aggregate(value) = object {
                value as *mut GcAggregate
            } else {
                std::ptr::null_mut()
            }
        }
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn calibre_gc_alloc_aggregate(field_count: usize) -> *mut GcAggregate {
    GcAggregate {
        field_count,
        data: vec![Value::Null; field_count],
    }
    .alloc()
}

#[unsafe(no_mangle)]
pub extern "C" fn calibre_aggregate_set_field(
    agg: *mut GcAggregate,
    index: usize,
    tag: ValueTag,
    payload: i64,
) {
    unsafe {
        if !agg.is_null() {
            let data = (*agg).data.as_mut_ptr();
            if index < (*agg).data.len() {
                *data.add(index) = Value::from_tag(tag, payload);
            }
        }
    }
}
