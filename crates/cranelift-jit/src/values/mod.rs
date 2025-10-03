pub mod cast;

use calibre_parser::ast::ParserDataType;
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

impl From<ParserDataType> for RuntimeType {
    fn from(value: ParserDataType) -> Self {
        match value {
            ParserDataType::Bool => RuntimeType::Bool,
            ParserDataType::Int => RuntimeType::Int,
            ParserDataType::Float => RuntimeType::Float,
            ParserDataType::Str => RuntimeType::Str,
            ParserDataType::Char => RuntimeType::Char,
            ParserDataType::Range => RuntimeType::Range,
            ParserDataType::List(x) => RuntimeType::List(Box::new(match *x {
                Some(x) => Some(x.into()),
                None => None,
            })),
            ParserDataType::Struct(x) => RuntimeType::Struct(x),
            _ => unimplemented!(),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum RuntimeType {
    Float,
    Int,
    Bool,
    Str,
    Char,
    Tuple(Vec<RuntimeType>),
    List(Box<Option<RuntimeType>>),
    Range,
    Option(Box<RuntimeType>),
    Result(Box<RuntimeType>, Box<RuntimeType>),
    Function {
        return_type: Box<Option<RuntimeType>>,
        parameters: Vec<(RuntimeType, calibre_parser::ast::RefMutability)>,
        is_async: bool,
    },
    Enum(String),
    Struct(Option<String>),
}
