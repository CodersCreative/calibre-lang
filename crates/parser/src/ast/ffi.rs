use crate::{
    Span,
    ast::types::{ParserDataType, ParserInnerType, PotentialNewType},
};
use serde::{Deserialize, Serialize};
use std::{fmt::Display, ops::Deref, str::FromStr};

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ParserFfiDataType {
    pub data_type: ParserFfiInnerType,
    pub span: Span,
}

impl From<ParserFfiDataType> for ParserDataType {
    fn from(value: ParserFfiDataType) -> Self {
        Self {
            span: value.span,
            data_type: value.data_type.into(),
        }
    }
}

impl Display for ParserFfiDataType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.data_type)
    }
}

impl From<ParserFfiInnerType> for ParserFfiDataType {
    fn from(value: ParserFfiInnerType) -> Self {
        Self {
            data_type: value,
            span: Span::default(),
        }
    }
}

impl ParserFfiDataType {
    pub fn new(span: Span, data_type: ParserFfiInnerType) -> Self {
        Self { data_type, span }
    }
}

impl Deref for ParserFfiDataType {
    type Target = ParserFfiInnerType;
    fn deref(&self) -> &Self::Target {
        &self.data_type
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum ParserFfiInnerType {
    F32,
    F64,
    LongDouble,
    U8,
    I8,
    U16,
    I16,
    U32,
    I32,
    U64,
    I64,
    USize,
    ISize,
    SChar,
    UChar,
    Int,
    UInt,
    Short,
    UShort,
    Long,
    ULong,
    LongLong,
    ULongLong,
}

impl Into<ParserInnerType> for ParserFfiInnerType {
    fn into(self) -> ParserInnerType {
        match self {
            Self::F32 | Self::F64 | Self::LongDouble => ParserInnerType::Float,
            Self::SChar | Self::UChar => ParserInnerType::Char,
            Self::U16
            | Self::U8
            | Self::U32
            | Self::U64
            | Self::USize
            | Self::UInt
            | Self::UShort
            | Self::ULong
            | Self::ULongLong => ParserInnerType::UInt,
            _ => ParserInnerType::Int,
        }
    }
}

impl Display for ParserFfiInnerType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "@{}",
            match self {
                Self::U8 => "u8",
                Self::I8 => "i8",
                Self::U16 => "u16",
                Self::I16 => "i16",
                Self::U32 => "u32",
                Self::I32 => "i32",
                Self::U64 => "u64",
                Self::I64 => "i64",
                Self::USize => "usize",
                Self::ISize => "isize",
                Self::UInt => "uint",
                Self::Int => "int",
                Self::UShort => "ushort",
                Self::Short => "short",
                Self::ULong => "ulong",
                Self::Long => "long",
                Self::ULongLong => "ulonglong",
                Self::LongLong => "longlong",
                Self::LongDouble => "longdouble",
                Self::F32 => "f32",
                Self::F64 => "f64",
                Self::SChar => "schar",
                Self::UChar => "uchar",
            }
        )
    }
}

impl FromStr for ParserFfiInnerType {
    type Err = ();
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(match s.trim().trim_start_matches("@") {
            "u8" => Self::U8,
            "i8" => Self::I8,
            "u16" => Self::U16,
            "i16" => Self::I16,
            "u32" => Self::U32,
            "i32" => Self::I32,
            "u64" => Self::U64,
            "i64" => Self::I64,
            "usize" => Self::USize,
            "isize" => Self::ISize,
            "uint" => Self::UInt,
            "int" => Self::Int,
            "ushort" => Self::UShort,
            "short" => Self::Short,
            "ulong" => Self::ULong,
            "long" => Self::Long,
            "ulonglong" => Self::ULongLong,
            "longlong" => Self::LongLong,
            "f32" => Self::F32,
            "f64" => Self::F64,
            "schar" => Self::SChar,
            "uchar" => Self::UChar,
            _ => return Err(()),
        })
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum PotentialNewTypeFfiType {
    DataType(PotentialNewType),
    Ffi(ParserFfiDataType),
}

impl PotentialNewTypeFfiType {
    pub fn is_auto(&self) -> bool {
        match self {
            Self::DataType(x) => x.is_auto(),
            _ => false,
        }
    }

    pub fn span(&self) -> &Span {
        match self {
            Self::Ffi(x) => &x.span,
            Self::DataType(x) => x.span(),
        }
    }
}

impl Display for PotentialNewTypeFfiType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::DataType(x) => write!(f, "{}", x),
            Self::Ffi(x) => write!(f, "@{}", x),
        }
    }
}

impl From<ParserDataType> for PotentialNewTypeFfiType {
    fn from(value: ParserDataType) -> Self {
        Self::DataType(value.into())
    }
}
