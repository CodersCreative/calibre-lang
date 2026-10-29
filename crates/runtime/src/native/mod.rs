use crate::values::{Value, ValueTag};
use std::io::{self, Write};

// TODO Add debug information
#[unsafe(no_mangle)]
pub extern "C" fn calibre_panic(tag: ValueTag, value: i64) {
    let value = Value::from_tag(tag, value);
    let value = value.to_string();

    eprintln!("calibre panicked with message: {}", value);
    std::process::exit(1);
}

#[unsafe(no_mangle)]
pub extern "C" fn calibre_console_output(handle_type: i64, tag: ValueTag, value: i64) {
    let value = Value::from_tag(tag, value);
    let value = value.to_string();

    if handle_type == 2 {
        let stderr = io::stderr();
        let mut handle = stderr.lock();
        let _ = handle.write_all(value.as_bytes());
        let _ = handle.flush();
    } else {
        let stdout = io::stdout();
        let mut handle = stdout.lock();
        let _ = handle.write_all(value.as_bytes());
        let _ = handle.flush();
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn calibre_len(tag: ValueTag, value: i64) -> u64 {
    match Value::from_tag(tag, value) {
        Value::Range(from, to) => to.abs_diff(from),
        Value::String(string) => {
            let string = unsafe { &*string };
            string.data.len() as u64
        }
        Value::List(list) => {
            let list = unsafe { &*list };
            list.len as u64
        }
        Value::Struct(agg) => {
            let agg = unsafe { &*agg };
            agg.data.len() as u64
        }
        Value::Int(x) => x.cast_unsigned(),
        Value::UInt(x) => x,
        Value::Float(x) => x as u64,
        Value::Char(x) => x.cast_unsigned() as u64,
        _ => 0,
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn calibre_min_or_zero(tag: ValueTag, value: i64) -> i64 {
    match tag {
        ValueTag::Range => value as i32 as i64,
        _ => 0,
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn calibre_gen_suspend(value: i64) -> i64 {
    // TODO Implement proper suspend with state machine
    value
}
