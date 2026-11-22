use crate::{
    gc::gc_header_at,
    values::{
        Value, ValueTag, aggregate::calibre_aggregate_deep_clone, big::alloc_big,
        list::calibre_list_deep_clone,
    },
};

#[unsafe(no_mangle)]
pub extern "C" fn calibre_deep_clone(tag: ValueTag, value: i64) -> i64 {
    let value = Value::from_tag(tag, value);
    i64::from(value.deep_clone())
}

impl Value {
    pub fn deep_clone(self) -> Self {
        let old_ptr = match &self {
            Value::Enum(p) | Value::Struct(p) | Value::Tuple(p) => Some(*p as usize),
            Value::List(p) => Some(*p as usize),
            _ => None,
        };

        let value = match self {
            Value::Big(value) => Value::Big(alloc_big(unsafe { &*value }.clone())),
            Value::Enum(value) => Value::Enum(calibre_aggregate_deep_clone(value)),
            Value::List(value) => Value::List(calibre_list_deep_clone(value)),
            Value::Struct(value) => Value::Struct(calibre_aggregate_deep_clone(value)),
            Value::Tuple(value) => Value::Tuple(calibre_aggregate_deep_clone(value)),
            x => x,
        };

        let Some(old_ptr) = old_ptr else {
            return value;
        };

        let new_ptr = match &value {
            Value::Enum(p) | Value::Struct(p) | Value::Tuple(p) => *p as usize,
            Value::List(p) => *p as usize,
            _ => return value,
        };

        if let (Some(old), Some(new)) = (gc_header_at(old_ptr), gc_header_at(new_ptr)) {
            unsafe {
                (*new).type_id = (*old).type_id;
                (*new).type_tag = (*old).type_tag;
            }
        }

        value
    }
}
