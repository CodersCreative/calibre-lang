use crate::values::{
    Value, ValueTag, aggregate::calibre_aggregate_deep_clone, big::alloc_big,
    list::calibre_list_deep_clone,
};

#[unsafe(no_mangle)]
pub extern "C" fn calibre_deep_clone(tag: ValueTag, value: i64) -> i64 {
    let value = Value::from_tag(tag, value);
    i64::from(value.deep_clone())
}

impl Value {
    pub fn deep_clone(self) -> Self {
        match self {
            Value::Big(value) => Value::Big(alloc_big(unsafe { &*value }.clone())),
            Value::Enum(value) => Value::Enum(calibre_aggregate_deep_clone(value)),
            Value::List(value) => Value::List(calibre_list_deep_clone(value)),
            Value::Struct(value) => Value::Struct(calibre_aggregate_deep_clone(value)),
            Value::Tuple(value) => Value::Tuple(calibre_aggregate_deep_clone(value)),
            x => x,
        }
    }
}
