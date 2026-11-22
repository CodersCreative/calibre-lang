use crate::{
    gc::{GcHeader, gc_header_at},
    values::{Value, dyn_box_inner, string::GcString},
};
use rustc_hash::FxHashMap;
use std::sync::LazyLock;
use wasm_sync::Mutex;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[repr(u64)]
pub enum RuntimeTypeKind {
    Struct,
    Enum,
    Other,
}

#[derive(Clone, Debug)]
pub struct RuntimeTypeInfo {
    pub name: String,
    pub kind: RuntimeTypeKind,
    pub members: Vec<String>,
}

impl RuntimeTypeInfo {
    pub(crate) fn get(id: u64) -> Option<RuntimeTypeInfo> {
        with_registry(|registry| registry.get(&id).cloned())?
    }

    pub(crate) fn get_name(id: u64) -> Option<String> {
        Self::get(id).map(|info| info.name)
    }
}

static TYPE_REGISTRY: LazyLock<Mutex<FxHashMap<u64, RuntimeTypeInfo>>> =
    LazyLock::new(|| Mutex::new(FxHashMap::default()));

fn with_registry<T>(f: impl FnOnce(&mut FxHashMap<u64, RuntimeTypeInfo>) -> T) -> Option<T> {
    let mut registry = TYPE_REGISTRY.lock().ok()?;
    Some(f(&mut registry))
}

pub(crate) fn member_index(id: u64, name: &str) -> Option<usize> {
    with_registry(|registry| {
        registry
            .get(&id)
            .and_then(|info| info.members.iter().position(|member| member == name))
    })?
}

pub(crate) fn ensure_type_name(id: u64, name: String) {
    if id == 0 {
        return;
    }

    let _ = with_registry(|registry| {
        registry.entry(id).or_insert(RuntimeTypeInfo {
            name,
            kind: RuntimeTypeKind::Other,
            members: Vec::new(),
        });
    });
}

impl GcHeader {
    pub(crate) fn get_type_id<T>(ptr: *mut T) -> Option<u64> {
        let header = gc_header_at(ptr as usize)?;
        unsafe { (*header).type_id }
    }
}

impl Value {
    pub(crate) fn get_type_id(&self) -> Option<u64> {
        match self {
            Value::Struct(ptr) => GcHeader::get_type_id(*ptr),
            Value::Tuple(ptr) => GcHeader::get_type_id(*ptr),
            Value::Enum(ptr) => GcHeader::get_type_id(*ptr),
            Value::Result(ptr) => GcHeader::get_type_id(*ptr),
            Value::Option(ptr) => GcHeader::get_type_id(*ptr),
            Value::List(ptr) => GcHeader::get_type_id(*ptr),
            Value::Host(ptr) => GcHeader::get_type_id(*ptr),
            Value::Reader(ptr) => GcHeader::get_type_id(*ptr),
            Value::Dynamic(payload) => Self::get_dyn_type_id(*payload),
            _ => None,
        }
    }

    fn get_dyn_type_id(payload: usize) -> Option<u64> {
        if let Some(inner) = dyn_box_inner(payload) {
            return inner.get_type_id();
        }

        let header = gc_header_at(payload)?;
        unsafe { (*header).type_id }
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn calibre_type_register(
    id: i64,
    name: *mut GcString,
    kind: RuntimeTypeKind,
    member_count: i64,
) -> i64 {
    if id == 0 || name.is_null() {
        return 0;
    }

    let name = unsafe { &*name }.data.clone();
    let members = vec![String::new(); member_count.max(0) as usize];

    with_registry(|registry| {
        registry.insert(
            id as u64,
            RuntimeTypeInfo {
                name,
                kind,
                members,
            },
        );
    });

    id
}

#[unsafe(no_mangle)]
pub extern "C" fn calibre_type_register_member(id: i64, index: i64, name: *mut GcString) {
    if id == 0 || index < 0 || name.is_null() {
        return;
    }

    let name = unsafe { &*name }.data.clone();
    let _ = with_registry(|registry| {
        if let Some(info) = registry.get_mut(&(id as u64))
            && let Some(member) = info.members.get_mut(index as usize)
        {
            *member = name;
        }
    });
}

#[unsafe(no_mangle)]
pub extern "C" fn calibre_is_type_id(payload: i64, type_id: i64) -> bool {
    if payload == 0 || type_id == 0 {
        return false;
    }

    Value::get_dyn_type_id(payload as usize) == Some(type_id as u64)
}
