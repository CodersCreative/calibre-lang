use crate::{
    gc::calibre_gc_add_root,
    values::{GcObject, Value, ValueTag, aggregate::GcAggregate, register_alloc},
};

#[unsafe(no_mangle)]
pub extern "C" fn calibre_gc_alloc_enum(
    variant: i64,
    tag: ValueTag,
    payload: i64,
) -> *mut GcAggregate {
    let variant = Value::Int(variant);
    let value = Value::from_tag(tag, payload);

    let header = register_alloc(GcObject::Aggregate(GcAggregate {
        field_count: 2,
        data: vec![variant, value],
    }));

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

#[unsafe(no_mangle)]
pub extern "C" fn calibre_enum_get(val: *const GcAggregate) -> i64 {
    if val.is_null() {
        return 0;
    }

    unsafe { i64::from((&(*val).data).get(1).copied().unwrap_or(Value::Null)) }
}

#[unsafe(no_mangle)]
pub extern "C" fn calibre_enum_discriminant(val: *const GcAggregate) -> u64 {
    if val.is_null() {
        return 0;
    }

    unsafe {
        match (&(*val).data).first().copied().unwrap_or(Value::Null) {
            Value::Int(x) => x.cast_unsigned(),
            Value::UInt(x) => x,
            _ => 0,
        }
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn calibre_result_ok(tag: ValueTag, payload: i64) -> *mut GcAggregate {
    calibre_gc_alloc_enum(0, tag, payload)
}

#[unsafe(no_mangle)]
pub extern "C" fn calibre_result_err(tag: ValueTag, payload: i64) -> *mut GcAggregate {
    calibre_gc_alloc_enum(1, tag, payload)
}

#[unsafe(no_mangle)]
pub extern "C" fn calibre_option_some(tag: ValueTag, payload: i64) -> *mut GcAggregate {
    calibre_gc_alloc_enum(0, tag, payload)
}

#[unsafe(no_mangle)]
pub extern "C" fn calibre_option_none() -> *mut GcAggregate {
    calibre_gc_alloc_enum(1, ValueTag::Null, 0)
}
