use crate::{
    VM,
    value::{
        RuntimeValue,
        ffi::{ExternFunction, FfiArg},
    },
};
use calibre_lir::MirDataType;
use libffi::middle::{Arg, Type};
use std::os::raw::c_void;

impl ExternFunction {
    pub(crate) fn type_to_libffi_type(typ: &MirDataType) -> Type {
        match &typ {
            MirDataType::Int => Type::i64(),
            MirDataType::UInt => Type::u64(),
            MirDataType::Float => Type::f64(),
            MirDataType::Bool => Type::u8(),
            MirDataType::Char => Type::u8(),
            MirDataType::Str => Type::pointer(),
            MirDataType::Ptr(_) => Type::pointer(),
            MirDataType::Null => Type::void(),
            _ => Type::pointer(),
        }
    }

    pub(crate) fn push_arg(ffi_args: &mut Vec<FfiArg>, arg: FfiArg) {
        ffi_args.push(arg);
    }

    pub(crate) fn ffi_args_to_libffi(ffi_args: &[FfiArg]) -> Vec<Arg<'_>> {
        ffi_args
            .iter()
            .map(|arg| match arg {
                FfiArg::U8(x) => Arg::new(x),
                FfiArg::I8(x) => Arg::new(x),
                FfiArg::U16(x) => Arg::new(x),
                FfiArg::I16(x) => Arg::new(x),
                FfiArg::U32(x) => Arg::new(x),
                FfiArg::I32(x) => Arg::new(x),
                FfiArg::U64(x) => Arg::new(x),
                FfiArg::I64(x) => Arg::new(x),
                FfiArg::F32(x) => Arg::new(x),
                FfiArg::F64(x) => Arg::new(x),
                FfiArg::Bool(x) => Arg::new(x),
                FfiArg::Char(x) => Arg::new(x),
                FfiArg::Ptr(x) => Arg::new(x),
                FfiArg::CString { ptr, .. } => Arg::new(ptr),
                FfiArg::Bytes { ptr, .. } => Arg::new(ptr),
                FfiArg::Struct { backing } => {
                    if let Some(first) = backing.first() {
                        Arg::new(first)
                    } else {
                        Arg::new(&0u64)
                    }
                }
            })
            .collect()
    }

    pub(crate) fn struct_field_to_bytes(
        value: RuntimeValue,
    ) -> Option<(Vec<u8>, Type, usize, usize)> {
        match value {
            RuntimeValue::UInt(x) => {
                let size = std::mem::size_of::<u64>();
                Some((x.to_le_bytes().to_vec(), Type::u64(), size, size))
            }
            RuntimeValue::Int(x) => {
                let size = std::mem::size_of::<i64>();
                Some((x.to_le_bytes().to_vec(), Type::i64(), size, size))
            }
            RuntimeValue::Float(x) => {
                let size = std::mem::size_of::<f64>();
                Some((x.to_le_bytes().to_vec(), Type::f64(), size, size))
            }
            RuntimeValue::Bool(x) => Some((vec![x as u8], Type::u8(), 1, 1)),
            RuntimeValue::Char(x) => Some((vec![x as u8], Type::u8(), 1, 1)),
            RuntimeValue::Ptr(id) => {
                let size = std::mem::size_of::<*const c_void>();
                let bytes = (id as usize).to_le_bytes().to_vec();
                Some((bytes, Type::pointer(), size, size))
            }
            _ => None,
        }
    }

    pub(crate) fn pack_struct_arg(env: &mut VM, value: RuntimeValue) -> Option<(Vec<u8>, Type)> {
        match value {
            RuntimeValue::Aggregate(_, data) => {
                let field_count = data.as_ref().0.0.len();
                let mut bytes = Vec::with_capacity(field_count.saturating_mul(8));
                let mut fields = Vec::with_capacity(field_count);
                let mut offset = 0usize;
                let mut max_align = 1usize;

                for (_, field) in data.as_ref().0.0.iter() {
                    let resolved = env.resolve_value_ref(field).unwrap_or_default();
                    let (field_bytes, field_ty, size, align) =
                        Self::struct_field_to_bytes(resolved)?;
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

                Some((bytes, Type::structure(fields)))
            }
            _ => None,
        }
    }

    pub(crate) fn pack_aggregate_bytes(value: &RuntimeValue) -> Option<Vec<u8>> {
        fn push_number(bytes: &mut Vec<u8>, value: &RuntimeValue) -> bool {
            match value {
                RuntimeValue::UInt(x) => {
                    bytes.extend_from_slice(&x.to_le_bytes());
                    true
                }
                RuntimeValue::Int(x) => {
                    bytes.extend_from_slice(&x.to_le_bytes());
                    true
                }
                RuntimeValue::Float(x) => {
                    bytes.extend_from_slice(&x.to_le_bytes());
                    true
                }
                RuntimeValue::Bool(x) => {
                    bytes.push(*x as u8);
                    true
                }
                RuntimeValue::Char(x) => {
                    bytes.push(*x as u8);
                    true
                }
                _ => false,
            }
        }

        match value {
            RuntimeValue::Aggregate(_, data) => {
                let field_count = data.as_ref().0.0.len();
                let mut bytes = Vec::with_capacity(field_count.saturating_mul(8));
                for (_, field) in data.as_ref().0.0.iter() {
                    if !push_number(&mut bytes, field) {
                        return None;
                    }
                }
                Some(bytes)
            }
            _ => None,
        }
    }
}
