use crate::values::{Value, ValueTag, string::GcString};
use std::{
    io::{self, BufRead, Write},
    thread,
    time::Duration,
};

pub mod env;
pub mod str;

// TODO Add debug information
#[unsafe(no_mangle)]
pub extern "C" fn calibre_panic(tag: ValueTag, value: i64) {
    let value = Value::from_tag(tag, value);
    let value = value.to_string();

    eprintln!("calibre panicked with message: {}", value);
    std::process::exit(1);
}

#[unsafe(no_mangle)]
pub extern "C" fn calibre_console_input() -> *mut GcString {
    let stdin = io::stdin();
    let mut handle = stdin.lock();
    let mut line = String::new();

    match handle.read_line(&mut line) {
        Ok(_) => GcString { data: line }.alloc(),
        _ => GcString::default().alloc(),
    }
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
            list.data.len() as u64
        }
        Value::Struct(agg) => {
            let agg = unsafe { &*agg };
            agg.data.len() as u64
        }
        Value::Tuple(agg) => {
            let agg = unsafe { &*agg };
            agg.data.len() as u64
        }
        Value::Int(x) => x.cast_unsigned(),
        Value::UInt(x) => x,
        Value::Float(x) => x as u64,
        Value::Char(x) => x as u64,
        Value::Ptr(_) | Value::Null | Value::Bool(_) | Value::Big(_) | Value::Enum(_) => 0,
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

#[unsafe(no_mangle)]
pub extern "C" fn calibre_trim(text: *mut GcString) -> *mut GcString {
    if text.is_null() {
        return text;
    }

    unsafe {
        GcString {
            data: (*text).data.trim().to_string(),
        }
        .alloc()
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn calibre_wait(time: u64) {
    thread::sleep(Duration::from_millis(time));
}
