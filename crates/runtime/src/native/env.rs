use crate::values::{
    Value, aggregate::GcAggregate, enums::calibre_option_none, list::GcList, string::GcString,
};
use std::{env, sync::LazyLock};
use wasm_sync::Mutex;

pub static PROGRAM_ARGS: LazyLock<Mutex<Vec<String>>> =
    LazyLock::new(|| Mutex::new(env::args().skip(1).collect()));

#[unsafe(no_mangle)]
pub extern "C" fn calibre_env_get_all() -> *mut GcList {
    GcList {
        data: PROGRAM_ARGS
            .lock()
            .expect("Failed to lock PROGRAM_ARGS")
            .clone()
            .into_iter()
            .map(|data| Value::String(GcString { data }.alloc()))
            .collect(),
    }
    .alloc()
}

#[unsafe(no_mangle)]
pub extern "C" fn calibre_env_get(index: u64) -> *mut GcAggregate {
    match PROGRAM_ARGS
        .lock()
        .expect("Failed to lock PROGRAM_ARGS")
        .get(index as usize)
        .cloned()
    {
        Some(data) => Value::String(GcString { data }.alloc()).some(),
        _ => calibre_option_none(),
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn calibre_env_var(name: *mut GcString) -> *mut GcAggregate {
    if name.is_null() {
        return calibre_option_none();
    }

    match unsafe { std::env::var((*name).data.as_str()) } {
        Ok(data) => Value::String(GcString { data }.alloc()).some(),
        _ => calibre_option_none(),
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn calibre_env_set_var(name: *mut GcString, value: *mut GcString) {
    if name.is_null() || value.is_null() {
        return;
    }

    unsafe { std::env::set_var((*name).data.as_str(), (*value).data.as_str()) };
}

#[unsafe(no_mangle)]
pub extern "C" fn calibre_env_remove_var(name: *mut GcString) {
    if name.is_null() {
        return;
    }

    unsafe { std::env::remove_var((*name).data.as_str()) };
}

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
