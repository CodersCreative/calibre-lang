pub mod cast;

use calibre_parser::ast::{ParserDataType, ParserInnerType};
use cranelift::prelude::Value;

#[derive(Clone, Debug)]
pub struct RuntimeValue {
    pub value: Value,
    pub data_type: RuntimeType,
}

impl RuntimeValue {
    pub fn new(value: Value, data_type: RuntimeType) -> Self {
        Self { value, data_type }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct MemberType {
    pub name: String,
    pub ty: RuntimeType,
}

impl From<ParserDataType> for RuntimeType {
    fn from(value: ParserDataType) -> Self {
        match value.data_type {
            ParserInnerType::Bool => RuntimeType::Bool,
            ParserInnerType::Int => RuntimeType::Int,
            ParserInnerType::Float => RuntimeType::Float,
            ParserInnerType::Str => RuntimeType::Str,
            ParserInnerType::Char => RuntimeType::Char,
            ParserInnerType::Range => RuntimeType::Range,
            ParserInnerType::List(x) => RuntimeType::List(Box::new(match x {
                Some(x) => RuntimeType::from(*x),
                None => RuntimeType::Dynamic,
            })),
            ParserInnerType::Struct(Some(x)) => RuntimeType::Named(x),
            ParserInnerType::Tuple(x) => {
                RuntimeType::Tuple(x.into_iter().map(|x| x.into()).collect())
            }
            _ => unimplemented!(),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Hash, Eq)]
pub enum RuntimeType {
    Float,
    Int,
    Bool,
    Str,
    Char,
    Dynamic,
    Ref {
        mutable: bool,
        data_type: Box<RuntimeType>,
    },
    Tuple(Vec<RuntimeType>),
    List(Box<RuntimeType>),
    Range,
    Option(Box<RuntimeType>),
    Result(Box<RuntimeType>, Box<RuntimeType>),
    Function {
        return_type: Option<Box<RuntimeType>>,
        captures: Vec<String>,
        parameters: Vec<(RuntimeType, calibre_parser::ast::RefMutability)>,
        is_async: bool,
    },
    Enum {
        uid: u32,
    },
    Struct {
        uid: Option<u32>,
        members: Vec<MemberType>,
    },
    Named(String),
}

impl RuntimeType {
    pub fn is_aggregate(&self) -> bool {
        match self {
            RuntimeType::Struct { .. }
            | RuntimeType::Enum { .. }
            | RuntimeType::Result { .. }
            | RuntimeType::List { .. }
            | RuntimeType::Dynamic => true,
            _ => false,
        }
    }
}
