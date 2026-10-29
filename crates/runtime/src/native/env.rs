use crate::values::{Value, aggregate::GcAggregate, list::GcList, string::GcString};

#[unsafe(no_mangle)]
pub extern "C" fn calibre_env_vars() -> *mut GcList {
    let vars = std::env::vars();

    GcList {
        data: vars
            .into_iter()
            .map(|(key, value)| {
                let key = GcString { data: key }.alloc();
                let value = GcString { data: value }.alloc();

                Value::Tuple(
                    GcAggregate {
                        field_count: 2,
                        data: vec![Value::String(key), Value::String(value)],
                    }
                    .alloc(),
                )
            })
            .collect(),
    }
    .alloc()
}
