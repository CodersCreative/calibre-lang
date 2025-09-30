use calibre_common::errors::ValueErr;
use calibre_parser::ast::{ObjectType, ParserDataType};
use std::{
    collections::HashMap, fmt::Debug
};

use crate::runtime::scope::CheckerEnvironment;

pub mod helper;

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
    NativeFunction(Box<RuntimeType>),
}

impl calibre_common::environment::RuntimeType for RuntimeType {}
impl calibre_common::environment::RuntimeValue for RuntimeType {
    fn string(_txt : String) -> Self {
        Self::Str
    }

    fn constants() -> std::collections::HashMap<String, Self> {
        HashMap::from([
            (String::from("PI"), RuntimeType::Float),
            (String::from("FLOAT_MAX"), RuntimeType::Float),
            (String::from("DOUBLE_MAX"), RuntimeType::Float),
            (String::from("INT_MAX"), RuntimeType::Int),
            (String::from("LONG_MAX"), RuntimeType::Int),
            (String::from("FLOAT_MIN"), RuntimeType::Float),
            (String::from("DOUBLE_MIN"), RuntimeType::Float),
            (String::from("INT_MIN"), RuntimeType::Int),
            (String::from("LONG_MIN"), RuntimeType::Int),
            (String::from("true"), RuntimeType::Bool),
            (String::from("false"), RuntimeType::Bool),
        ])
    }

    fn natives() -> HashMap<String, Self> {
        let lst : Vec<(&'static str, RuntimeType)> = vec![
            ("print", RuntimeType::Null),
            ("ok", RuntimeType::Result(Box::new(RuntimeType::Dynamic), Box::new(RuntimeType::Dynamic))),
            ("err", RuntimeType::Result(Box::new(RuntimeType::Dynamic), Box::new(RuntimeType::Dynamic))),
            ("some", RuntimeType::Option(Box::new(RuntimeType::Dynamic))),
            ("len", RuntimeType::Int),
            ("range", RuntimeType::Range),
            ("trim", RuntimeType::Str),
            ("console.out", RuntimeType::Null),
            ("console.input", RuntimeType::Str),
            ("console.err", RuntimeType::Null),
            ("console.clear", RuntimeType::Null),
            ("thread.wait", RuntimeType::Null),
            ("random.generate", RuntimeType::Float),
            ("random.bool", RuntimeType::Bool),
            ("random.ratio", RuntimeType::Bool),
        ];

        let mut map = HashMap::new();

        for val in lst {
            map.insert(val.0.to_string(), RuntimeType::NativeFunction(Box::new(val.1)));
        }

        map
    }
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
        env: &CheckerEnvironment,
        scope: &u64,
        t: &RuntimeType,
    ) -> Result<RuntimeType, ValueErr<RuntimeType, RuntimeType>> {
        Ok(t.clone())
    }
}
