use calibre_parser::ast::types::ParserDataType;
use libloading::Library;
use std::os::raw::c_void;
use std::{ffi::CString, sync::Arc};
use ustr::Ustr;

pub mod calling;
pub mod conversion;

#[derive(Debug, Clone)]
pub struct ExternFunction {
    pub abi: Ustr,
    pub library: Ustr,
    pub symbol: Ustr,
    pub parameters: Box<[ParserDataType]>,
    pub return_type: ParserDataType,
    pub handle: Arc<Library>,
    pub memo_params: usize,
    pub memo: bool,
    pub pure: bool,
}

#[derive(Debug)]
pub(crate) enum FfiArg {
    U8(u8),
    I8(i8),
    U16(u16),
    I16(i16),
    U32(u32),
    I32(i32),
    U64(u64),
    I64(i64),
    F32(f32),
    F64(f64),
    Bool(u8),
    Char(u8),
    Ptr(*const c_void),
    CString { _value: CString, ptr: *const c_void },
    Bytes { _value: Vec<u8>, ptr: *const c_void },
    Struct { backing: Vec<u64> },
}
