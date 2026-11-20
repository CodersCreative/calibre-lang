use calibre_lir::MirDataType;
use calibre_parser::ast::ffi::ParserFfiInnerType;
use cranelift::prelude::{Type, Value, types};

#[derive(Clone, Copy, Debug)]
pub struct CraneliftTypes {
    ptr_bit_width: u32,
    ptr: Type,
}

impl CraneliftTypes {
    pub fn new(ptr: Type) -> Self {
        Self {
            ptr_bit_width: ptr.bits(),
            ptr,
        }
    }

    #[inline(always)]
    pub fn ptr(&self) -> Type {
        self.ptr
    }

    #[inline(always)]
    pub fn int(&self) -> Type {
        types::I64
    }

    #[inline(always)]
    pub fn uint(&self) -> Type {
        types::I64
    }

    #[inline(always)]
    pub fn float(&self) -> Type {
        types::F64
    }

    #[inline(always)]
    pub fn bool(&self) -> Type {
        types::I8
    }

    #[inline(always)]
    pub fn byte(&self) -> Type {
        types::I8
    }

    #[inline(always)]
    pub fn char(&self) -> Type {
        types::I32
    }

    #[inline(always)]
    pub fn pointer_bytes(&self) -> u32 {
        self.ptr_bit_width / 8
    }

    pub fn to_cranelift_type(&self, ty: &MirDataType) -> Type {
        if let MirDataType::FfiType(inner) = ty.unwrap_all_refs() {
            if matches!(
                inner,
                ParserFfiInnerType::F32 | ParserFfiInnerType::F64 | ParserFfiInnerType::LongDouble
            ) {
                return types::F64;
            }

            return types::I64;
        }

        match ty {
            MirDataType::Int => self.int(),
            MirDataType::UInt => self.uint(),
            MirDataType::Float => self.float(),
            MirDataType::Bool | MirDataType::Byte => self.bool(),
            MirDataType::Char => self.char(),
            _ => self.ptr(),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct CompiledValue {
    pub value: Value,
    pub data_type: MirDataType,
}

impl CompiledValue {
    pub fn new(value: Value, data_type: MirDataType) -> Self {
        Self { value, data_type }
    }
}
