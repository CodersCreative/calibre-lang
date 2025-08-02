pub mod conversion;
pub mod helper;

use core::panic;
use std::{
    fmt::Debug,
    num::{ParseFloatError, ParseIntError},
    rc::Rc,
    str::FromStr,
    string::ParseError,
};

use helper::Block;
use thiserror::Error;

use crate::{
    ast::{NodeType, RefMutability},
    native::NativeFunction,
    runtime::{
        scope::ScopeErr,
        values::helper::{MatchBlock, ObjectType},
    },
};

#[derive(Error, Debug, Clone)]
pub enum ValueErr {
    #[error("Unable to convert: {0:?} -> {1:?}.")]
    Conversion(RuntimeValue, RuntimeType),
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
    Double,
    Int,
    Long,
    UInt,
    ULong,
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
        parameters: Vec<(RuntimeType, RefMutability)>,
        is_async: bool,
    },
    Enum(u64, String),
    Struct(u64, Option<String>),
}

impl FromStr for RuntimeType {
    type Err = ParseError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(match s {
            "int" => RuntimeType::Int,
            "dyn" => RuntimeType::Dynamic,
            "uint" => RuntimeType::UInt,
            "long" => RuntimeType::Long,
            "ulong" => RuntimeType::ULong,
            "float" => RuntimeType::Float,
            "double" => RuntimeType::Double,
            "struct" => RuntimeType::Struct(0, None),
            "bool" => RuntimeType::Bool,
            "string" => RuntimeType::Str,
            "char" => RuntimeType::Char,
            _ => RuntimeType::Struct(0, Some(s.to_string())),
        })
    }
}

impl Into<RuntimeType> for &RuntimeValue {
    fn into(self) -> RuntimeType {
        match self {
            RuntimeValue::Null => panic!("Tried to convert null value to type"),
            RuntimeValue::Float(_) => RuntimeType::Float,
            RuntimeValue::Double(_) => RuntimeType::Double,
            RuntimeValue::Int(_) => RuntimeType::Int,
            RuntimeValue::Long(_) => RuntimeType::Long,
            RuntimeValue::UInt(_) => RuntimeType::UInt,
            RuntimeValue::ULong(_) => RuntimeType::ULong,
            RuntimeValue::Link(_, _, x) => x.clone(),
            RuntimeValue::Enum(y, x, _, _) => RuntimeType::Enum(*y, x.clone()),
            RuntimeValue::Struct(y, x, _) => RuntimeType::Struct(*y, x.clone()),
            RuntimeValue::Bool(_) => RuntimeType::Bool,
            RuntimeValue::Option(_, x) => x.clone(),
            RuntimeValue::Result(_, x) => x.clone(),
            RuntimeValue::Str(_) => RuntimeType::Str,
            RuntimeValue::Char(_) => RuntimeType::Char,
            RuntimeValue::Range(_, _) => RuntimeType::Range,
            RuntimeValue::Tuple(data) => {
                RuntimeType::Tuple(data.into_iter().map(|x| x.into()).collect())
            }
            RuntimeValue::List { data, data_type } => RuntimeType::List(data_type.clone()),
            RuntimeValue::Function {
                parameters,
                body,
                return_type,
                is_async,
            } => RuntimeType::Function {
                return_type: match return_type {
                    Some(x) => Box::new(Some(x.clone())),
                    None => Box::new(None),
                },
                parameters: parameters
                    .iter()
                    .map(|x| (x.1.clone(), x.2.clone()))
                    .collect(),
                is_async: *is_async,
            },
            RuntimeValue::NativeFunction(_) => panic!("Cannot get type of native functions"),
        }
    }
}

#[derive(Clone, PartialEq, PartialOrd, Debug)]
pub enum RuntimeValue {
    Null,
    Float(f32),
    Double(f64),
    Int(i64),
    Long(i128),
    UInt(u64),
    ULong(u128),
    Range(i32, i32),
    Bool(bool),
    Str(String),
    Char(char),
    Struct(u64, Option<String>, ObjectType<RuntimeValue>),
    Enum(u64, String, usize, Option<ObjectType<RuntimeValue>>),
    Tuple(Vec<RuntimeValue>),
    Link(u64, Vec<String>, RuntimeType),
    List {
        data: Vec<RuntimeValue>,
        data_type: Box<Option<RuntimeType>>,
    },
    Option(Option<Box<RuntimeValue>>, RuntimeType),
    Result(Result<Box<RuntimeValue>, Box<RuntimeValue>>, RuntimeType),
    Function {
        parameters: Vec<(String, RuntimeType, RefMutability, Option<RuntimeValue>)>,
        body: FunctionType,
        return_type: Option<RuntimeType>,
        is_async: bool,
    },
    NativeFunction(Rc<dyn NativeFunction>),
}

#[derive(Clone, PartialEq, PartialOrd, Debug)]
pub enum FunctionType {
    Regular(Block),
    Match(MatchBlock),
}

impl ToString for ObjectType<RuntimeValue> {
    fn to_string(&self) -> String {
        match self {
            ObjectType::Map(x) => {
                let mut txt = String::from("{");
                for (k, v) in x {
                    txt.push_str(&format!("{k} : {}, ", v.to_string()));
                }

                let _ = (txt.pop(), txt.pop());
                txt.push_str("}");

                txt
            }
            ObjectType::Tuple(data) => print_list(data, '(', ')'),
        }
    }
}

fn print_list<T: ToString>(data: &Vec<T>, open: char, close: char) -> String {
    let mut txt = String::from(open);

    for val in data.iter() {
        txt.push_str(&format!("{}, ", val.to_string()));
    }

    let _ = (txt.pop(), txt.pop());
    txt.push(close);

    txt
}

impl ToString for RuntimeValue {
    fn to_string(&self) -> String {
        match self {
            Self::Null => String::from("null"),
            Self::Float(x) => x.to_string(),
            Self::UInt(x) => x.to_string(),
            Self::Int(x) => x.to_string(),
            Self::Long(x) => x.to_string(),
            Self::ULong(x) => x.to_string(),
            Self::Double(x) => x.to_string(),
            Self::Enum(_, x, y, z) => format!("{:?}({:?}) -> {:?}", x, y, z),
            Self::Range(from, to) => format!("{}..{}", from, to),
            Self::Link(_, path, _) => format!("link -> {:?}", path),
            Self::Bool(x) => x.to_string(),
            Self::Struct(_, y, x) => format!("{y:?} = {}", x.to_string()),
            Self::NativeFunction(_) => format!("native function"),
            Self::List { data, data_type: _ } => print_list(data, '[', ']'),
            Self::Tuple(data) => print_list(data, '(', ')'),
            Self::Option(x, _) => format!("{:?}", x),
            Self::Result(x, _) => format!("{:?}", x),
            Self::Str(x) => x.to_string(),
            Self::Char(x) => x.to_string(),
            Self::Function {
                parameters,
                body: _,
                return_type,
                is_async: _,
            } => {
                format!("({:?}) -> {:?}", parameters, return_type)
            }
        }
    }
}
