use crate::runtime::scope::{Environment, ScopeErr};
use calibre_parser::ast::{ObjectType, ParserDataType};
use std::{
    fmt::Debug,
    num::{ParseFloatError, ParseIntError},
};
use thiserror::Error;

pub mod helper;

#[derive(Error, Debug, Clone)]
pub enum ValueErr {
    #[error("Unable to convert: {0:?} -> {1:?}.")]
    Conversion(RuntimeType, RuntimeType),
    #[error("Unable to progress value.")]
    ProgressErr,
    #[error("{0}")]
    Scope(ScopeErr),
    #[error("{0}")]
    ParseIntError(ParseIntError),
    #[error("{0}")]
    ParseFloatError(ParseFloatError),
}

impl From<ParseIntError> for ValueErr {
    fn from(value: ParseIntError) -> Self {
        Self::ParseIntError(value)
    }
}

impl From<ParseFloatError> for ValueErr {
    fn from(value: ParseFloatError) -> Self {
        Self::ParseFloatError(value)
    }
}

impl From<ScopeErr> for ValueErr {
    fn from(value: ScopeErr) -> Self {
        Self::Scope(value)
    }
}

#[derive(Clone, PartialEq, PartialOrd, Debug)]
pub enum RuntimeType {
    Float,
    Dynamic,
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
        parameters: Vec<(String, RuntimeType, calibre_parser::ast::RefMutability, bool)>,
        is_async: bool,
    },
    Enum(u64, String, Option<ObjectType<RuntimeType>>),
    Struct(u64, Option<String>, ObjectType<RuntimeType>),
    Null,
    NativeFunction,
}

impl From<ParserDataType> for RuntimeType {
    fn from(value: ParserDataType) -> Self {
        match value {
            ParserDataType::Float => Self::Float,
            ParserDataType::Dynamic => Self::Dynamic,
            ParserDataType::Double => Self::Float,
            ParserDataType::Int => Self::Int,
            ParserDataType::Long => Self::Int,
            ParserDataType::UInt => Self::Int,
            ParserDataType::ULong => Self::Int,
            ParserDataType::Bool => Self::Bool,
            ParserDataType::Str => Self::Str,
            ParserDataType::Char => Self::Char,
            ParserDataType::Tuple(x) => {
                Self::Tuple(x.into_iter().map(|x| RuntimeType::from(x)).collect())
            }
            ParserDataType::List(x) => Self::List(Box::new(match *x {
                Some(x) => Some(RuntimeType::from(x)),
                None => None,
            })),
            ParserDataType::Range => Self::Range,
            ParserDataType::Struct(x) => Self::Struct(0, x, ObjectType::Tuple(Vec::new())),
            ParserDataType::Function {
                return_type,
                parameters,
                is_async,
            } => Self::Function {
                return_type: Box::new(match *return_type {
                    Some(x) => Some(RuntimeType::from(x)),
                    None => None,
                }),
                parameters: parameters
                    .into_iter()
                    .map(|x| (String::new(), RuntimeType::from(x.0), x.1, false))
                    .collect(),
                is_async,
            },
            ParserDataType::Option(x) => Self::Option(Box::new(RuntimeType::from(*x))),
            ParserDataType::Result(x, y) => Self::Result(
                Box::new(RuntimeType::from(*x)),
                Box::new(RuntimeType::from(*y)),
            ),
        }
    }
}

impl RuntimeType {
    pub fn into_type(
        &self,
        env: &Environment,
        scope: &u64,
        t: &RuntimeType,
    ) -> Result<RuntimeType, ValueErr> {
        Ok(t.clone())
    }
}
