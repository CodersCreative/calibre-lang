use crate::{
    VM,
    error::RuntimeError,
    value::{
        RuntimeValue,
        ffi::{ExternFunction, FfiArg},
        hashable::HashKey,
    },
};
use calibre_lir::MirDataType;
use calibre_parser::ast::{ffi::ParserFfiInnerType, types::ParserInnerType};
use libffi::{
    low::CodePtr,
    middle::{Cif, Type},
};
use rustc_hash::FxHashMap;
use std::{
    ffi::{CStr, CString},
    os::raw::{c_char, c_void},
    sync::Arc,
};
use ustr::Ustr;
use wasm_sync::Mutex;

impl ExternFunction {
    pub fn call(&self, env: &mut VM, args: &[RuntimeValue]) -> Result<RuntimeValue, RuntimeError> {
        if self.memo {
            let mut key: Option<Vec<HashKey>> = Some(Vec::with_capacity(args.len()));
            for (i, a) in args.iter().enumerate() {
                if self.memo_params == 0 || self.memo_params & (1 << i) != 0 {
                    match HashKey::try_from(a.clone()) {
                        Ok(k) => key.as_mut().unwrap().push(k),
                        Err(_) => {
                            key = None;
                            break;
                        }
                    }
                }
            }

            if let Some(k) = key {
                let cache_entry = env
                    .caches
                    .memo
                    .entry(self.symbol)
                    .or_insert_with(|| Arc::new(Mutex::new(FxHashMap::default())));

                let guard = cache_entry.lock().unwrap();
                if let Some(value) = guard.get(&k) {
                    return Ok(value.clone());
                }
                drop(guard);

                let result = self.call_inner(env, args)?;
                env.caches
                    .memo
                    .entry(self.symbol)
                    .or_insert_with(|| Arc::new(Mutex::new(FxHashMap::default())))
                    .lock()
                    .unwrap()
                    .insert(k, result.clone());
                return Ok(result);
            }
        }

        self.call_inner(env, args)
    }

    fn call_inner(
        &self,
        env: &mut VM,
        args: &[RuntimeValue],
    ) -> Result<RuntimeValue, RuntimeError> {
        let mut arg_types = Vec::with_capacity(self.parameters.len());
        let mut ffi_args: Vec<FfiArg> = Vec::with_capacity(self.parameters.len());

        if args.len() != self.parameters.len() {
            return Err(RuntimeError::InvalidFunctionCall);
        }

        for (param, value) in self.parameters.iter().zip(args) {
            match &param {
                x if true => match (env.resolve_value_ref(value).unwrap_or_default(), x) {
                    (RuntimeValue::Str(x), MirDataType::Str) => {
                        arg_types.push(Type::pointer());
                        let value = CString::new(x.as_str())
                            .map_err(|_| RuntimeError::InvalidFunctionCall)?;
                        let ptr = value.as_ptr() as *const c_void;
                        Self::push_arg(&mut ffi_args, FfiArg::CString { _value: value, ptr });
                    }
                    (RuntimeValue::UInt(x), MirDataType::UInt) => {
                        arg_types.push(Type::u64());
                        Self::push_arg(&mut ffi_args, FfiArg::U64(x));
                    }
                    (RuntimeValue::UInt(x), MirDataType::Int) => {
                        arg_types.push(Type::i64());
                        Self::push_arg(&mut ffi_args, FfiArg::I64(x as i64));
                    }
                    (RuntimeValue::UInt(x), MirDataType::Float) => {
                        arg_types.push(Type::f64());
                        Self::push_arg(&mut ffi_args, FfiArg::F64(x as f64));
                    }
                    (RuntimeValue::Int(x), MirDataType::Int) => {
                        arg_types.push(Type::i64());
                        Self::push_arg(&mut ffi_args, FfiArg::I64(x));
                    }
                    (RuntimeValue::Int(x), MirDataType::Float) => {
                        arg_types.push(Type::f64());
                        Self::push_arg(&mut ffi_args, FfiArg::F64(x as f64));
                    }
                    (RuntimeValue::Int(x), MirDataType::UInt) => {
                        arg_types.push(Type::u64());
                        Self::push_arg(&mut ffi_args, FfiArg::U64(x as u64));
                    }
                    (RuntimeValue::Float(x), MirDataType::Float) => {
                        arg_types.push(Type::f64());
                        Self::push_arg(&mut ffi_args, FfiArg::F64(x));
                    }
                    (RuntimeValue::Float(x), MirDataType::UInt) => {
                        arg_types.push(Type::u64());
                        Self::push_arg(&mut ffi_args, FfiArg::U64(x as u64));
                    }
                    (RuntimeValue::Float(x), MirDataType::Int) => {
                        arg_types.push(Type::i64());
                        Self::push_arg(&mut ffi_args, FfiArg::I64(x as i64));
                    }
                    (RuntimeValue::Char(x), MirDataType::Char) => {
                        arg_types.push(Type::u8());
                        Self::push_arg(&mut ffi_args, FfiArg::Char(x as u8));
                    }
                    (RuntimeValue::Bool(x), MirDataType::Bool) => {
                        arg_types.push(Type::u8());
                        Self::push_arg(&mut ffi_args, FfiArg::Bool(x as u8));
                    }
                    (RuntimeValue::Bool(x), MirDataType::UInt) => {
                        arg_types.push(Type::u64());
                        Self::push_arg(&mut ffi_args, FfiArg::U64(x as u64));
                    }
                    (RuntimeValue::Bool(x), MirDataType::Int) => {
                        arg_types.push(Type::i64());
                        Self::push_arg(&mut ffi_args, FfiArg::I64(x as i64));
                    }
                    (RuntimeValue::UInt(x), MirDataType::Ptr(_)) => {
                        arg_types.push(Type::pointer());
                        Self::push_arg(&mut ffi_args, FfiArg::Ptr(x as *const c_void));
                    }
                    (RuntimeValue::Int(x), MirDataType::Ptr(_)) => {
                        arg_types.push(Type::pointer());
                        Self::push_arg(&mut ffi_args, FfiArg::Ptr(x as usize as *const c_void));
                    }
                    (RuntimeValue::Str(x), MirDataType::Ptr(_)) => {
                        arg_types.push(Type::pointer());
                        let value = CString::new(x.as_str())
                            .map_err(|_| RuntimeError::InvalidFunctionCall)?;
                        let ptr = value.as_ptr() as *const c_void;
                        Self::push_arg(&mut ffi_args, FfiArg::CString { _value: value, ptr });
                    }
                    (RuntimeValue::Ptr(id), MirDataType::Ptr(_)) => {
                        arg_types.push(Type::pointer());
                        Self::push_arg(&mut ffi_args, FfiArg::Ptr(id as *const c_void));
                    }
                    (RuntimeValue::List(list), MirDataType::Ptr(_)) => {
                        arg_types.push(Type::pointer());
                        let mut bytes = Vec::new();
                        for item in list.as_ref().0.iter() {
                            match env.resolve_value_ref(item).unwrap_or_default() {
                                RuntimeValue::UInt(x) => bytes.push(x as u8),
                                RuntimeValue::Int(x) => bytes.push(x as u8),
                                RuntimeValue::Float(x) => bytes.push(x as u8),
                                RuntimeValue::Bool(x) => bytes.push(x as u8),
                                RuntimeValue::Char(x) => bytes.push(x as u8),
                                _ => {
                                    return Err(RuntimeError::InvalidFunctionCall);
                                }
                            }
                        }
                        let ptr = bytes.as_ptr() as *const c_void;
                        Self::push_arg(&mut ffi_args, FfiArg::Bytes { _value: bytes, ptr });
                    }
                    (value, MirDataType::Ptr(_)) => {
                        arg_types.push(Type::pointer());
                        if let Some(bytes) = Self::pack_aggregate_bytes(
                            &env.resolve_value(value).unwrap_or_default(),
                        ) {
                            let ptr = bytes.as_ptr() as *const c_void;
                            Self::push_arg(&mut ffi_args, FfiArg::Bytes { _value: bytes, ptr });
                        } else {
                            return Err(RuntimeError::InvalidFunctionCall);
                        }
                    }
                    (value, MirDataType::Struct { .. }) => {
                        let resolved = env.resolve_value(value).unwrap_or_default();

                        let (bytes, ty) = match Self::pack_struct_arg(env, resolved) {
                            Some(data) => data,
                            None => {
                                return Err(RuntimeError::Ffi(String::from(
                                    "unsupported struct arg",
                                )));
                            }
                        };

                        arg_types.push(ty.clone());
                        let mut backing = vec![0u64; bytes.len().div_ceil(8)];

                        if !bytes.is_empty() {
                            let raw = backing.as_mut_ptr() as *mut u8;
                            let raw_len = backing.len() * std::mem::size_of::<u64>();
                            let dst = unsafe { std::slice::from_raw_parts_mut(raw, raw_len) };
                            dst[..bytes.len()].copy_from_slice(&bytes);
                        }

                        Self::push_arg(&mut ffi_args, FfiArg::Struct { backing });
                    }
                    _ => return Err(RuntimeError::InvalidFunctionCall),
                },
                /*ParserInnerType::FfiType(x) => {
                    arg_types.push(Self::type_to_libffi_type(param));
                    let value = env.resolve_value_ref(value).unwrap_or_default();
                    let arg = match (x, value) {
                        (
                            ParserFfiInnerType::U8 | ParserFfiInnerType::UChar,
                            RuntimeValue::UInt(x),
                        ) => FfiArg::U8(x as u8),
                        (
                            ParserFfiInnerType::U8 | ParserFfiInnerType::UChar,
                            RuntimeValue::Int(x),
                        ) => FfiArg::U8(x as u8),
                        (
                            ParserFfiInnerType::U8 | ParserFfiInnerType::UChar,
                            RuntimeValue::Float(x),
                        ) => FfiArg::U8(x as u8),
                        (
                            ParserFfiInnerType::I8 | ParserFfiInnerType::SChar,
                            RuntimeValue::Int(x),
                        ) => FfiArg::I8(x as i8),
                        (
                            ParserFfiInnerType::I8 | ParserFfiInnerType::SChar,
                            RuntimeValue::UInt(x),
                        ) => FfiArg::I8(x as i8),
                        (
                            ParserFfiInnerType::I8 | ParserFfiInnerType::SChar,
                            RuntimeValue::Float(x),
                        ) => FfiArg::I8(x as i8),
                        (
                            ParserFfiInnerType::U16 | ParserFfiInnerType::UShort,
                            RuntimeValue::UInt(x),
                        ) => FfiArg::U16(x as u16),
                        (
                            ParserFfiInnerType::U16 | ParserFfiInnerType::UShort,
                            RuntimeValue::Int(x),
                        ) => FfiArg::U16(x as u16),
                        (
                            ParserFfiInnerType::U16 | ParserFfiInnerType::UShort,
                            RuntimeValue::Float(x),
                        ) => FfiArg::U16(x as u16),
                        (
                            ParserFfiInnerType::I16 | ParserFfiInnerType::Short,
                            RuntimeValue::Int(x),
                        ) => FfiArg::I16(x as i16),
                        (
                            ParserFfiInnerType::I16 | ParserFfiInnerType::Short,
                            RuntimeValue::UInt(x),
                        ) => FfiArg::I16(x as i16),
                        (
                            ParserFfiInnerType::I16 | ParserFfiInnerType::Short,
                            RuntimeValue::Float(x),
                        ) => FfiArg::I16(x as i16),
                        (
                            ParserFfiInnerType::U32 | ParserFfiInnerType::UInt,
                            RuntimeValue::UInt(x),
                        ) => FfiArg::U32(x as u32),
                        (
                            ParserFfiInnerType::U32 | ParserFfiInnerType::UInt,
                            RuntimeValue::Int(x),
                        ) => FfiArg::U32(x as u32),
                        (
                            ParserFfiInnerType::U32 | ParserFfiInnerType::UInt,
                            RuntimeValue::Float(x),
                        ) => FfiArg::U32(x as u32),
                        (
                            ParserFfiInnerType::I32 | ParserFfiInnerType::Int,
                            RuntimeValue::Int(x),
                        ) => FfiArg::I32(x as i32),
                        (
                            ParserFfiInnerType::I32 | ParserFfiInnerType::Int,
                            RuntimeValue::UInt(x),
                        ) => FfiArg::I32(x as i32),
                        (
                            ParserFfiInnerType::I32 | ParserFfiInnerType::Int,
                            RuntimeValue::Float(x),
                        ) => FfiArg::I32(x as i32),
                        (
                            ParserFfiInnerType::U64
                            | ParserFfiInnerType::ULong
                            | ParserFfiInnerType::ULongLong,
                            RuntimeValue::UInt(x),
                        ) => FfiArg::U64(x),
                        (
                            ParserFfiInnerType::U64
                            | ParserFfiInnerType::ULong
                            | ParserFfiInnerType::ULongLong,
                            RuntimeValue::Int(x),
                        ) => FfiArg::U64(x as u64),
                        (
                            ParserFfiInnerType::U64
                            | ParserFfiInnerType::ULong
                            | ParserFfiInnerType::ULongLong,
                            RuntimeValue::Float(x),
                        ) => FfiArg::U64(x as u64),
                        (
                            ParserFfiInnerType::I64
                            | ParserFfiInnerType::Long
                            | ParserFfiInnerType::LongLong,
                            RuntimeValue::Int(x),
                        ) => FfiArg::I64(x),
                        (
                            ParserFfiInnerType::I64
                            | ParserFfiInnerType::Long
                            | ParserFfiInnerType::LongLong,
                            RuntimeValue::UInt(x),
                        ) => FfiArg::I64(x as i64),
                        (
                            ParserFfiInnerType::I64
                            | ParserFfiInnerType::Long
                            | ParserFfiInnerType::LongLong,
                            RuntimeValue::Float(x),
                        ) => FfiArg::I64(x as i64),
                        (ParserFfiInnerType::USize, RuntimeValue::UInt(x)) => FfiArg::U64(x),
                        (ParserFfiInnerType::USize, RuntimeValue::Int(x)) => FfiArg::U64(x as u64),
                        (ParserFfiInnerType::USize, RuntimeValue::Float(x)) => {
                            FfiArg::U64(x as u64)
                        }
                        (ParserFfiInnerType::ISize, RuntimeValue::Int(x)) => FfiArg::I64(x),
                        (ParserFfiInnerType::ISize, RuntimeValue::UInt(x)) => FfiArg::I64(x as i64),
                        (ParserFfiInnerType::ISize, RuntimeValue::Float(x)) => {
                            FfiArg::I64(x as i64)
                        }
                        (ParserFfiInnerType::F32, RuntimeValue::Float(x)) => FfiArg::F32(x as f32),
                        (ParserFfiInnerType::F32, RuntimeValue::Int(x)) => FfiArg::F32(x as f32),
                        (ParserFfiInnerType::F32, RuntimeValue::UInt(x)) => FfiArg::F32(x as f32),
                        (
                            ParserFfiInnerType::F64 | ParserFfiInnerType::LongDouble,
                            RuntimeValue::Float(x),
                        ) => FfiArg::F64(x),
                        (
                            ParserFfiInnerType::F64 | ParserFfiInnerType::LongDouble,
                            RuntimeValue::Int(x),
                        ) => FfiArg::F64(x as f64),
                        (
                            ParserFfiInnerType::F64 | ParserFfiInnerType::LongDouble,
                            RuntimeValue::UInt(x),
                        ) => FfiArg::F64(x as f64),
                        (
                            ParserFfiInnerType::U8 | ParserFfiInnerType::UChar,
                            RuntimeValue::Bool(x),
                        ) => FfiArg::U8(x as u8),
                        (
                            ParserFfiInnerType::I8 | ParserFfiInnerType::SChar,
                            RuntimeValue::Bool(x),
                        ) => FfiArg::I8(x as i8),
                        (
                            ParserFfiInnerType::U8 | ParserFfiInnerType::UChar,
                            RuntimeValue::Char(x),
                        ) => FfiArg::U8(x as u8),
                        (
                            ParserFfiInnerType::I8 | ParserFfiInnerType::SChar,
                            RuntimeValue::Char(x),
                        ) => FfiArg::I8(x as i8),
                        _ => return Err(RuntimeError::InvalidFunctionCall),
                    };
                    Self::push_arg(&mut ffi_args, arg);
                }*/
                _ => return Err(RuntimeError::InvalidFunctionCall),
            }
        }

        let libffi_args = Self::ffi_args_to_libffi(&ffi_args);

        let cif = Cif::new(arg_types, Self::type_to_libffi_type(&self.return_type));

        let symbol = unsafe {
            self.handle
                .get::<*const c_void>(self.symbol.as_bytes())
                .map_err(|_| RuntimeError::InvalidFunctionCall)?
        };

        let code = CodePtr::from_ptr(*symbol as *mut c_void);

        unsafe {
            match &self.return_type {
                MirDataType::Float => Ok(RuntimeValue::Float(cif.call(code, &libffi_args))),
                MirDataType::UInt => Ok(RuntimeValue::UInt(cif.call(code, &libffi_args))),
                MirDataType::Int => Ok(RuntimeValue::Int(cif.call(code, &libffi_args))),
                MirDataType::Bool => {
                    let res: u8 = cif.call(code, &libffi_args);
                    Ok(RuntimeValue::Bool(res != 0))
                }
                MirDataType::Null => {
                    let _: () = cif.call(code, &libffi_args);
                    Ok(RuntimeValue::Null)
                }
                MirDataType::Char => {
                    let res: u8 = cif.call(code, &libffi_args);
                    Ok(RuntimeValue::Char(res as char))
                }
                MirDataType::Str => {
                    let res: *const c_char = cif.call(code, &libffi_args);
                    if res.is_null() {
                        Ok(RuntimeValue::Str(Ustr::default()))
                    } else {
                        let c_str = CStr::from_ptr(res);
                        Ok(RuntimeValue::Str(Ustr::from(&c_str.to_string_lossy())))
                    }
                }
                MirDataType::Ptr(_) => {
                    let res: *const c_void = cif.call(code, &libffi_args);
                    Ok(RuntimeValue::UInt(res as u64))
                }
                _ => Err(RuntimeError::InvalidFunctionCall),
            }
        }
    }
}
