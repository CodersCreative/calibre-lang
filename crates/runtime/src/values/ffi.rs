use crate::{
    gc::{GcHeader, gc_header_at},
    values::{GcObject, Value, aggregate::GcAggregate, list::GcList, string::GcString},
};
use libffi::{
    low::CodePtr,
    middle::{Arg, Cif, Type},
};
use libloading::Library;
use std::{
    collections::HashMap,
    ffi::{CStr, CString},
    os::raw::c_void,
    sync::{LazyLock, Mutex},
};

#[repr(u64)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum FFI {
    I8,
    U8,
    I16,
    U16,
    I32,
    U32,
    I64,
    U64,
    Isize,
    Usize,
    F32,
    F64,
    Ptr,
    Struct,
    Cstr,
}

impl From<FFI> for Type {
    fn from(value: FFI) -> Self {
        match value {
            FFI::I8 => Type::i8(),
            FFI::U8 => Type::u8(),
            FFI::I16 => Type::i16(),
            FFI::U16 => Type::u16(),
            FFI::I32 => Type::i32(),
            FFI::U32 => Type::u32(),
            FFI::I64 => Type::i64(),
            FFI::U64 => Type::u64(),
            FFI::Isize => Type::isize(),
            FFI::Usize => Type::usize(),
            FFI::F32 => Type::f32(),
            FFI::F64 => Type::f64(),
            FFI::Struct => Type::structure(Vec::new()),
            FFI::Ptr | FFI::Cstr => Type::pointer(),
        }
    }
}

static SYMBOLS: LazyLock<Mutex<HashMap<(String, String), usize>>> =
    LazyLock::new(|| Mutex::new(HashMap::new()));

static LIBRARIES: LazyLock<Mutex<Vec<Library>>> = LazyLock::new(|| Mutex::new(Vec::new()));

fn library_candidates(library: &str) -> Vec<String> {
    let has_path = library.contains('/') || library.contains('\\');
    let lower = library.to_ascii_lowercase();
    let has_ext = lower.ends_with(".so") || lower.ends_with(".dylib") || lower.ends_with(".dll");

    let base_candidates = if has_path || has_ext {
        vec![library.to_string()]
    } else {
        let base = match library {
            "c" | "libc" => "c",
            other => other,
        };

        let mut out: Vec<String> = Vec::new();

        #[cfg(target_os = "android")]
        {
            if base == "c" {
                out.push("libc.so".to_string());
            }
            out.push(format!("lib{}.so", base));
            out.push(format!("{}.so", base));
            out.push(base.to_string());
        }
        #[cfg(any(target_os = "linux", target_os = "android"))]
        {
            if base == "c" {
                out.push("libc.so.6".to_string());
            }
            out.push(format!("lib{}.so", base));
            out.push(format!("{}.so", base));
            out.push(base.to_string());
        }
        #[cfg(target_os = "macos")]
        {
            if base == "c" {
                out.push("libc.dylib".to_string());
                out.push("/usr/lib/libc.dylib".to_string());
            }
            out.push(format!("lib{}.dylib", base));
            out.push(format!("{}.dylib", base));
            out.push(base.to_string());
        }
        #[cfg(target_os = "windows")]
        {
            if base == "c" {
                out.push("msvcrt.dll".to_string());
            }
            out.push(format!("{}.dll", base));
            out.push(format!("lib{}.dll", base));
            out.push(base.to_string());
        }

        out.into_iter().filter(|c| !c.is_empty()).collect()
    };

    let mut out = Vec::with_capacity(base_candidates.len() * 2);

    if let Ok(dir) = crate::native::env::SOURCE_DIR.lock()
        && let Some(dir) = dir.as_ref()
    {
        for candidate in &base_candidates {
            if !candidate.contains('/') && !candidate.contains('\\') {
                out.push(format!("{}/{}", dir, candidate));
            }
        }
    }

    out.extend(base_candidates);
    out
}

fn resolve_symbol(library: &str, symbol: &str) -> Result<usize, String> {
    let mut symbols = SYMBOLS
        .lock()
        .map_err(|_| "ffi cache poisoned".to_string())?;

    let key = (library.to_string(), symbol.to_string());
    if let Some(ptr) = symbols.get(&key) {
        return Ok(*ptr);
    }

    let mut last_err = String::new();
    let mut resolved = None;

    for candidate in library_candidates(library) {
        match unsafe { Library::new(&candidate) } {
            Ok(lib) => {
                let name = match CString::new(symbol) {
                    Ok(name) => name,
                    Err(_) => return Err("invalid symbol name".to_string()),
                };

                match unsafe { lib.get::<*const c_void>(name.as_c_str()) } {
                    Ok(sym) => {
                        resolved = Some(*sym as usize);

                        if let Ok(mut libs) = LIBRARIES.lock() {
                            libs.push(lib);
                        }

                        break;
                    }
                    Err(e) => last_err = e.to_string(),
                }
            }
            Err(e) => last_err = e.to_string(),
        }
    }

    let Some(ptr) = resolved else {
        return Err(format!("failed to load library {} ({})", library, last_err));
    };

    symbols.insert(key, ptr);
    Ok(ptr)
}

fn pack_struct(agg: *mut GcAggregate) -> (Vec<u8>, Vec<Type>) {
    let mut fields = Vec::new();
    let mut bytes = Vec::new();
    let mut offset = 0usize;
    let mut max_align = 1usize;

    if agg.is_null() {
        return (bytes, fields);
    }

    for value in unsafe { &(*agg).data }.iter() {
        let (field_bytes, field_ty, size, align) = match value {
            Value::Int(x) => ((*x).to_le_bytes().to_vec(), Type::i64(), 8, 8),
            Value::UInt(x) => ((*x).to_le_bytes().to_vec(), Type::u64(), 8, 8),
            Value::Float(x) => ((*x).to_le_bytes().to_vec(), Type::f64(), 8, 8),
            Value::Bool(x) => (vec![*x as u8], Type::u8(), 1, 1),
            Value::Char(x) => ((*x as u8).to_le_bytes().to_vec(), Type::u8(), 1, 1),
            Value::Byte(x) => (vec![*x], Type::u8(), 1, 1),
            Value::Ptr(x) => {
                let bytes = (*x).to_le_bytes().to_vec();
                (bytes, Type::pointer(), 8, 8)
            }
            _ => continue,
        };

        max_align = max_align.max(align);
        let padding = (align - (offset % align)) % align;
        if padding > 0 {
            bytes.extend(std::iter::repeat_n(0u8, padding));
            offset += padding;
        }

        bytes.extend_from_slice(&field_bytes);
        offset += size;
        fields.push(field_ty);
    }

    let tail_padding = (max_align - (offset % max_align)) % max_align;
    if tail_padding > 0 {
        bytes.extend(std::iter::repeat_n(0u8, tail_padding));
    }

    (bytes, fields)
}

fn pack_list_bytes(list: *mut GcList) -> Vec<u8> {
    let mut bytes = Vec::new();
    if list.is_null() {
        return bytes;
    }

    for value in unsafe { &(*list).data }.iter() {
        match value {
            Value::Int(x) => bytes.push((*x).cast_unsigned() as u8),
            Value::UInt(x) => bytes.push(*x as u8),
            Value::Float(x) => bytes.push(*x as u8),
            Value::Bool(x) => bytes.push(*x as u8),
            Value::Char(x) => bytes.push(*x as u8),
            Value::Byte(x) => bytes.push(*x),
            _ => {}
        }
    }

    bytes
}

fn pack_aggregate_bytes(agg: *mut GcAggregate) -> Vec<u8> {
    let mut bytes = Vec::new();
    if agg.is_null() {
        return bytes;
    }

    for value in unsafe { &(*agg).data }.iter() {
        match value {
            Value::Int(x) => bytes.extend_from_slice(&x.to_le_bytes()),
            Value::UInt(x) => bytes.extend_from_slice(&x.to_le_bytes()),
            Value::Float(x) => bytes.extend_from_slice(&x.to_le_bytes()),
            Value::Bool(x) => bytes.push(*x as u8),
            Value::Char(x) => bytes.push(*x as u8),
            Value::Byte(x) => bytes.push(*x),
            _ => {}
        }
    }

    bytes
}

struct FfiSlot {
    kind: FFI,
    buf: [u8; 16],
    ptr: *const c_void,
    fields: Vec<Type>,
    keep: Vec<Box<dyn Send>>,
}

impl From<&FfiSlot> for Arg<'static> {
    fn from(value: &FfiSlot) -> Self {
        let value: &'static FfiSlot = unsafe { &*(value as *const FfiSlot) };
        let buf = value.buf.as_ptr() as usize;

        unsafe {
            match value.kind {
                FFI::I8 => Arg::new(&*(buf as *const i8)),
                FFI::U8 => Arg::new(&*(buf as *const u8)),
                FFI::I16 => Arg::new(&*(buf as *const i16)),
                FFI::U16 => Arg::new(&*(buf as *const u16)),
                FFI::I32 => Arg::new(&*(buf as *const i32)),
                FFI::U32 => Arg::new(&*(buf as *const u32)),
                FFI::I64 => Arg::new(&*(buf as *const i64)),
                FFI::U64 => Arg::new(&*(buf as *const u64)),
                FFI::Isize => Arg::new(&*(buf as *const isize)),
                FFI::Usize => Arg::new(&*(buf as *const usize)),
                FFI::F32 => Arg::new(&*(buf as *const f32)),
                FFI::F64 => Arg::new(&*(buf as *const f64)),
                FFI::Struct => {
                    if value.ptr.is_null() {
                        Arg::new(&0u64)
                    } else {
                        Arg::new(&*(value.ptr as *const u8))
                    }
                }
                FFI::Ptr | FFI::Cstr => Arg::new(&value.ptr),
            }
        }
    }
}

impl FfiSlot {
    fn scalar(kind: FFI, value: i64) -> Self {
        let mut buf = [0u8; 16];
        match kind {
            FFI::I8 => buf[..1].copy_from_slice(&(value as i8).to_ne_bytes()),
            FFI::U8 => buf[..1].copy_from_slice(&(value.cast_unsigned() as u8).to_ne_bytes()),
            FFI::I16 => buf[..2].copy_from_slice(&(value as i16).to_ne_bytes()),
            FFI::U16 => buf[..2].copy_from_slice(&(value.cast_unsigned() as u16).to_ne_bytes()),
            FFI::I32 => buf[..4].copy_from_slice(&(value as i32).to_ne_bytes()),
            FFI::U32 => buf[..4].copy_from_slice(&(value.cast_unsigned() as u32).to_ne_bytes()),
            FFI::F32 => buf[..4]
                .copy_from_slice(&(f64::from_bits(value.cast_unsigned()) as f32).to_ne_bytes()),
            FFI::Isize => buf[..8].copy_from_slice(&(value as isize).to_ne_bytes()),
            FFI::Usize => buf[..8].copy_from_slice(&(value.cast_unsigned() as usize).to_ne_bytes()),
            FFI::F64 => {
                buf[..8].copy_from_slice(&f64::from_bits(value.cast_unsigned()).to_ne_bytes())
            }
            _ => buf[..8].copy_from_slice(&value.to_ne_bytes()),
        }

        Self {
            kind,
            buf,
            ptr: std::ptr::null(),
            fields: Vec::new(),
            keep: Vec::new(),
        }
    }

    fn pointer(kind: FFI, ptr: *const c_void) -> Self {
        Self {
            kind,
            buf: [0u8; 16],
            ptr,
            fields: Vec::new(),
            keep: Vec::new(),
        }
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn calibre_ffi_call(
    library: i64,
    symbol: i64,
    params: i64,
    param_count: i64,
    return_kind: FFI,
) -> i64 {
    let library_c = unsafe { CStr::from_ptr(library as *const i8) };
    let symbol_c = unsafe { CStr::from_ptr(symbol as *const i8) };

    let library = library_c.to_string_lossy();
    let symbol = symbol_c.to_string_lossy();

    let ptr = match resolve_symbol(&library, &symbol) {
        Ok(ptr) => ptr,
        Err(_) => return 0,
    };

    let mut slots: Vec<Box<FfiSlot>> = Vec::with_capacity(param_count.cast_unsigned() as usize);

    let pairs = unsafe {
        std::slice::from_raw_parts(
            params as *const u64,
            (param_count.cast_unsigned() as usize) * 2,
        )
    };

    for pair in pairs.as_chunks::<2>().0 {
        let kind = unsafe { std::mem::transmute::<u64, FFI>(pair[0]) };

        let payload = pair[1] as i64;

        let slot = match kind {
            FFI::Struct => {
                let agg = payload as *mut GcAggregate;

                let (bytes, fields) = pack_struct(agg);
                let mut backing: Vec<u64> = Vec::with_capacity(bytes.len().div_ceil(8));

                if !bytes.is_empty() {
                    backing.resize(bytes.len().div_ceil(8), 0);
                    let raw = backing.as_mut_ptr() as *mut u8;
                    let raw_len = backing.len() * std::mem::size_of::<u64>();
                    let dst = unsafe { std::slice::from_raw_parts_mut(raw, raw_len) };
                    dst[..bytes.len()].copy_from_slice(&bytes);
                }

                let mut slot = FfiSlot::pointer(kind, backing.as_ptr() as *const c_void);
                slot.fields = fields;
                slot.keep.push(Box::new(backing));

                slot
            }
            FFI::Cstr => {
                let s = payload as *mut GcString;

                let data = if s.is_null() {
                    String::new()
                } else {
                    unsafe { (*s).data.clone() }
                };

                let cstr = match CString::new(data) {
                    Ok(c) => c,
                    Err(_) => return 0,
                };

                let ptr = cstr.as_ptr() as *const c_void;
                let mut slot = FfiSlot::pointer(kind, ptr);
                slot.keep.push(Box::new(cstr));

                slot
            }
            FFI::Ptr => {
                let addr = payload.cast_unsigned() as usize;

                match gc_header_at(addr).and_then(GcHeader::get_from_ptr) {
                    Some(GcObject::String(s)) => {
                        let data = s.data.clone();

                        let cstr = match CString::new(data) {
                            Ok(c) => c,
                            Err(_) => return 0,
                        };

                        let ptr = cstr.as_ptr() as *const c_void;
                        let mut slot = FfiSlot::pointer(kind, ptr);
                        slot.keep.push(Box::new(cstr));

                        slot
                    }
                    Some(GcObject::List(l)) => {
                        let bytes = pack_list_bytes(std::ptr::from_ref(l).cast_mut());
                        let ptr = bytes.as_ptr() as *const c_void;

                        let mut slot = FfiSlot::pointer(kind, ptr);
                        slot.keep.push(Box::new(bytes));

                        slot
                    }
                    Some(GcObject::Aggregate(a)) => {
                        let bytes = pack_aggregate_bytes(std::ptr::from_ref(a).cast_mut());
                        let ptr = bytes.as_ptr() as *const c_void;

                        let mut slot = FfiSlot::pointer(kind, ptr);
                        slot.keep.push(Box::new(bytes));

                        slot
                    }
                    _ => FfiSlot::pointer(kind, addr as *const c_void),
                }
            }
            _ => FfiSlot::scalar(kind, payload),
        };

        slots.push(Box::new(slot));
    }

    let arg_types: Vec<Type> = slots
        .iter()
        .map(|s| match s.kind {
            FFI::Struct => Type::structure(s.fields.clone()),
            _ => Type::from(s.kind),
        })
        .collect();

    let ffi_args: Vec<Arg> = slots.iter().map(|s| Arg::from(&**s)).collect();

    if return_kind == FFI::Struct {
        return 0;
    }

    let ret_ty = return_kind.into();
    let cif = Cif::new(arg_types, ret_ty);
    let code = CodePtr::from_ptr(ptr as *mut c_void);

    let result: i64 = unsafe {
        match return_kind {
            FFI::F32 => {
                let res: f32 = cif.call(code, &ffi_args);
                (res as f64).to_bits() as i64
            }
            FFI::F64 => {
                let res: f64 = cif.call(code, &ffi_args);
                res.to_bits() as i64
            }
            FFI::Cstr => {
                let res: *const i8 = cif.call(code, &ffi_args);
                if res.is_null() {
                    0
                } else {
                    let cstr = CStr::from_ptr(res);
                    let data = cstr.to_string_lossy().to_string();
                    GcString { data }.alloc() as i64
                }
            }
            FFI::I8 => cif.call::<i8>(code, &ffi_args) as i64,
            FFI::U8 => cif.call::<u8>(code, &ffi_args) as i64,
            FFI::I16 => cif.call::<i16>(code, &ffi_args) as i64,
            FFI::U16 => cif.call::<u16>(code, &ffi_args) as i64,
            FFI::I32 => cif.call::<i32>(code, &ffi_args) as i64,
            FFI::U32 => cif.call::<u32>(code, &ffi_args) as i64,
            FFI::I64 => cif.call::<i64>(code, &ffi_args),
            FFI::Isize => cif.call::<isize>(code, &ffi_args) as i64,
            FFI::U64 => cif.call::<u64>(code, &ffi_args) as i64,
            FFI::Usize | FFI::Ptr => cif.call::<usize>(code, &ffi_args) as i64,
            // TODO
            FFI::Struct => cif.call::<i64>(code, &ffi_args),
        }
    };

    result
}
