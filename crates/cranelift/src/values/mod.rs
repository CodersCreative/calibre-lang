pub mod cast;

use calibre_mir_ty::MiddleNode;
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

impl<T> From<ParserDataType<T>> for RuntimeType {
    fn from(value: ParserDataType<T>) -> Self {
        match value.data_type {
            ParserInnerType::Null => RuntimeType::Null,
            ParserInnerType::Bool => RuntimeType::Bool,
            ParserInnerType::Int => RuntimeType::Int,
            ParserInnerType::Float => RuntimeType::Float,
            ParserInnerType::Str => RuntimeType::Str,
            ParserInnerType::Char => RuntimeType::Char,
            ParserInnerType::Range => RuntimeType::Range,
            ParserInnerType::List(x) => RuntimeType::List(Box::new(RuntimeType::from(*x))),
            ParserInnerType::Struct(x) => RuntimeType::Named(x),
            ParserInnerType::Tuple(x) => RuntimeType::Aggregate {
                name: None,
                members: x
                    .into_iter()
                    .enumerate()
                    .map(|(i, x)| MemberType {
                        name: i.to_string(),
                        ty: x.into(),
                    })
                    .collect(),
            },
            x => {
                // eprintln!("Data type not implemented {}", x);
                RuntimeType::Dynamic
            }
        }
    }
}

#[derive(Clone, Debug, PartialEq, Hash, Eq)]
pub enum RuntimeType {
    Null,
    Float,
    Int,
    Bool,
    Str,
    Char,
    Dynamic,
    Ref {
        data_type: Box<RuntimeType>,
    },
    List(Box<RuntimeType>),
    Range,
    Option(Box<RuntimeType>),
    Result(Box<RuntimeType>, Box<RuntimeType>),
    Function {
        return_type: Box<RuntimeType>,
        captures: Vec<String>,
        parameters: Vec<(RuntimeType, calibre_parser::ast::RefMutability)>,
        is_async: bool,
    },
    Enum {
        name: String,
        variant: usize,
    },
    Aggregate {
        name: Option<String>,
        members: Vec<MemberType>,
    },
    Named(String),
}

impl RuntimeType {
    pub fn is_aggregate(&self) -> bool {
        match self {
            RuntimeType::Aggregate { .. }
            | RuntimeType::Result { .. }
            | RuntimeType::List { .. }
            | RuntimeType::Dynamic => true,
            _ => false,
        }
    }

    pub fn is_type(&self, other: &Self) -> bool {
        if self == other {
            return true;
        }

        match (self, other) {
            (
                RuntimeType::Named(x),
                RuntimeType::Enum {
                    name: y,
                    variant: _,
                },
            )
            | (
                RuntimeType::Enum {
                    name: y,
                    variant: _,
                },
                RuntimeType::Named(x),
            )
            | (RuntimeType::Named(x), RuntimeType::Aggregate { name: Some(y), .. })
            | (RuntimeType::Aggregate { name: Some(y), .. }, RuntimeType::Named(x))
                if x == y =>
            {
                true
            }
            _ => false,
        }
    }
}
