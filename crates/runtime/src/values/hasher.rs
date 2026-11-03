use crate::{
    gc::calibre_gc_add_root,
    values::{GcObject, Value, ValueTag, register_alloc},
};
use rustc_hash::FxHasher;
use std::{
    collections::HashMap,
    hash::{Hash, Hasher},
    sync::LazyLock,
};
use wasm_sync::Mutex;

pub static MEMO_CACHE: LazyLock<Mutex<HashMap<u64, i64>>> =
    LazyLock::new(|| Mutex::new(HashMap::new()));

pub fn alloc_hasher(value: Box<dyn Hasher>) -> *mut Box<dyn Hasher> {
    let header = register_alloc(GcObject::Hasher(value));

    unsafe {
        let object = &mut (*header).inner;

        let ptr = object as *mut GcObject as usize;
        calibre_gc_add_root(ptr);

        if let GcObject::Hasher(value) = object {
            value as *mut Box<dyn Hasher>
        } else {
            std::ptr::null_mut()
        }
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn calibre_new_hasher() -> *mut Box<dyn Hasher> {
    alloc_hasher(Box::new(FxHasher::default()))
}

impl Hash for Value {
    fn hash<H: Hasher>(&self, state: &mut H) {
        core::mem::discriminant(self).hash(state);

        match self {
            Value::Null => {}
            Value::Int(v) => v.hash(state),
            Value::UInt(v) => v.hash(state),
            Value::Byte(v) => v.hash(state),
            Value::Bool(v) => v.hash(state),
            Value::Char(v) => v.hash(state),
            Value::String(v) => unsafe { &**v }.data.hash(state),
            Value::Float(v) => v.to_bits().hash(state),
            Value::Big(v) => unsafe { &**v }.to_string().hash(state),
            Value::Ptr(v) => v.hash(state),
            Value::Range(a, b) => {
                a.hash(state);
                b.hash(state);
            }
            Value::List(v) => unsafe { &**v }.data.hash(state),
            Value::Tuple(v) | Value::Enum(v) | Value::Struct(v) => unsafe { &**v }.data.hash(state),
        }
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn calibre_hash_add(hasher: *mut Box<dyn Hasher>, tag: ValueTag, payload: i64) {
    let value = Value::from_tag(tag, payload);

    unsafe {
        value.hash(&mut *hasher);
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn calibre_hash_finish(hasher: *mut Box<dyn Hasher>) -> u64 {
    unsafe { (&*hasher).finish() }
}
