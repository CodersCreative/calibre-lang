use calibre_lir::MirDataType;
use cranelift::prelude::{Type, Value, types};

#[derive(Clone, Copy, Debug)]
pub struct CraneliftTypes {
    ptr: Type,
}

impl CraneliftTypes {
    pub fn new(ptr: Type) -> Self {
        Self { ptr }
    }

    pub fn ptr(&self) -> Type {
        self.ptr
    }

    pub fn int(&self) -> Type {
        types::I64
    }

    pub fn uint(&self) -> Type {
        types::I64
    }

    pub fn float(&self) -> Type {
        types::F64
    }

    pub fn bool(&self) -> Type {
        types::I8
    }

    pub fn byte(&self) -> Type {
        types::I8
    }

    pub fn char(&self) -> Type {
        types::I32
    }

    pub fn to_cranelift_type(&self, ty: &MirDataType) -> Type {
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
