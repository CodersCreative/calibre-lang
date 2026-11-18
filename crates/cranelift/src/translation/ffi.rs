use calibre_lir::MirDataType;
use calibre_parser::ast::ffi::ParserFfiInnerType;
use calibre_runtime::values::ffi::FFI;

pub fn ffi_kind(data_type: &MirDataType) -> FFI {
    match data_type.unwrap_all_refs() {
        MirDataType::FfiType(inner) => match inner {
            ParserFfiInnerType::F32 => FFI::F32,
            ParserFfiInnerType::F64 | ParserFfiInnerType::LongDouble => FFI::F64,
            ParserFfiInnerType::U8 => FFI::U8,
            ParserFfiInnerType::I8 | ParserFfiInnerType::SChar => FFI::I8,
            ParserFfiInnerType::U16 | ParserFfiInnerType::UShort => FFI::U16,
            ParserFfiInnerType::I16 | ParserFfiInnerType::Short => FFI::I16,
            ParserFfiInnerType::U32 | ParserFfiInnerType::UInt => FFI::U32,
            ParserFfiInnerType::I32 | ParserFfiInnerType::Int => FFI::I32,
            ParserFfiInnerType::U64 | ParserFfiInnerType::ULong | ParserFfiInnerType::ULongLong => {
                FFI::U64
            }
            ParserFfiInnerType::I64 | ParserFfiInnerType::Long | ParserFfiInnerType::LongLong => {
                FFI::I64
            }
            ParserFfiInnerType::USize => FFI::Usize,
            ParserFfiInnerType::ISize => FFI::Isize,
            ParserFfiInnerType::UChar => FFI::U8,
        },
        MirDataType::Int => FFI::I64,
        MirDataType::UInt => FFI::U64,
        MirDataType::Float => FFI::F64,
        MirDataType::Bool | MirDataType::Char | MirDataType::Byte => FFI::U8,
        MirDataType::Str => FFI::Cstr,
        MirDataType::Struct { .. } => FFI::Struct,
        _ => FFI::Ptr,
    }
}
