use core::panic;
use std::{
    collections::HashMap,
    fmt::{Debug, format, write},
    ops::Deref,
    str::FromStr,
    string::ParseError,
};

use crate::{ast::NodeType, lexer::TokenType, runtime::scope::Scope};

#[derive(Debug, Clone, PartialEq)]
pub enum NativeFunctions {
    Print,
}

impl NativeFunctions {}

#[derive(Debug, Clone, PartialEq)]
pub enum RuntimeType {
    Float,
    Integer,
    Map,
    Bool,
    Str,
    Char,
    Function {
        return_type: Option<Box<RuntimeType>>,
        parameters: Vec<RuntimeType>,
        is_async: bool,
    },
    Struct(String),
}

impl FromStr for RuntimeType {
    type Err = ParseError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(match s {
            "int" => RuntimeType::Integer,
            "float" => RuntimeType::Float,
            "map" => RuntimeType::Map,
            "bool" => RuntimeType::Bool,
            "string" => RuntimeType::Str,
            "char" => RuntimeType::Char,
            _ => RuntimeType::Struct(s.to_string()),
        })
    }
}

#[derive(Clone, PartialEq)]
pub enum RuntimeValue {
    Null,
    Float(f64),
    Integer(i64),
    Map(HashMap<String, RuntimeValue>),
    Bool(bool),
    Str(String),
    Char(char),
    // Option(Option<Box<RuntimeValue>>),
    // Result(Result<Box<RuntimeValue>, Box<RuntimeValue>>),
    Function {
        identifier: String,
        parameters: Vec<(String, RuntimeType)>,
        body: Vec<NodeType>,
        return_type: Option<RuntimeType>,
        is_async: bool,
        // scope : Rc<Ref>
    },
    NativeFunction(NativeFunctions),
}

impl ToString for RuntimeValue {
    fn to_string(&self) -> String {
        match self {
            Self::Null => String::from("null"),
            Self::Float(x) => x.to_string(),
            Self::Integer(x) => x.to_string(),
            Self::Bool(x) => x.to_string(),
            Self::Map(x) => format!("{:?}", x),
            Self::NativeFunction(x) => format!("native function : {:?}", x),
            Self::Str(x) => x.to_string(),
            Self::Char(x) => x.to_string(),
            Self::Function {
                identifier,
                parameters,
                body,
                return_type,
                is_async,
            } => {
                format!("{:?} ({:?}) -> {:?}", identifier, parameters, return_type)
            }
        }
    }
}

impl Debug for RuntimeValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.to_string())
    }
}

impl RuntimeValue {
    pub fn is_number(&self) -> bool {
        match self {
            RuntimeValue::Float(_) => true,
            RuntimeValue::Integer(_) => true,
            _ => false,
        }
    }

    pub fn call_native(&self, args: Vec<RuntimeValue>, scope: &mut Scope) -> RuntimeValue {
        if let Self::NativeFunction(func) = self {
            match func {
                NativeFunctions::Print => {
                    println!("{:?}", args);

                    RuntimeValue::Null
                }
            }
        } else {
            panic!();
        }
    }

    pub fn into_type(&self, scope: &mut Scope, t: RuntimeType) -> RuntimeValue {
        let panic_type = || {
            panic!("Cannot convert {:?} into {:?}", self, t);
            RuntimeValue::Null
        };

        match self {
            RuntimeValue::Integer(x) => match t {
                RuntimeType::Integer => self.clone(),
                RuntimeType::Float => RuntimeValue::Float(*x as f64),
                RuntimeType::Bool => RuntimeValue::Bool(match x {
                    0 => false,
                    1 => true,
                    _ => {
                        panic_type();
                        false
                    }
                }),
                RuntimeType::Map => panic_type(),
                RuntimeType::Struct(_) => panic_type(),
                RuntimeType::Str => RuntimeValue::Str(self.to_string()),
                RuntimeType::Char => panic_type(),
                RuntimeType::Function { .. } => panic_type(),
            },
            RuntimeValue::Float(x) => match t {
                RuntimeType::Integer => RuntimeValue::Integer(*x as i64),
                RuntimeType::Float => self.clone(),
                RuntimeType::Bool => RuntimeValue::Bool(match x {
                    0.0 => false,
                    1.0 => true,
                    _ => {
                        panic_type();
                        false
                    }
                }),
                RuntimeType::Map => panic_type(),
                RuntimeType::Struct(_) => panic_type(),
                RuntimeType::Str => RuntimeValue::Str(self.to_string()),
                RuntimeType::Char => panic_type(),
                RuntimeType::Function { .. } => panic_type(),
            },
            RuntimeValue::Str(x) => match t {
                RuntimeType::Integer => RuntimeValue::Integer(x.parse().unwrap()),
                RuntimeType::Float => RuntimeValue::Float(x.parse().unwrap()),
                RuntimeType::Bool => panic_type(),
                RuntimeType::Map => panic_type(),
                RuntimeType::Str => self.clone(),
                RuntimeType::Char => RuntimeValue::Char(x.chars().nth(0).unwrap()),
                RuntimeType::Struct(_) => panic_type(),
                RuntimeType::Function { .. } => panic_type(),
            },
            RuntimeValue::Null => panic_type(),
            RuntimeValue::Char(x) => {
                RuntimeValue::into_type(&RuntimeValue::Str(x.to_string()), scope, t)
            }
            RuntimeValue::Map(x) => match t {
                RuntimeType::Map => self.clone(),
                RuntimeType::Str => RuntimeValue::Str(self.to_string()),
                RuntimeType::Char => panic_type(),
                RuntimeType::Struct(identifier) => {
                    let properties = scope.resolve_struct(&identifier).get_struct(&identifier);
                    for property in properties {
                        if !x.contains_key(property.0) {
                            panic!("Struct Declaration is missing {:?}", property);
                        }
                    }
                    self.clone()
                }
                RuntimeType::Function { .. } => panic_type(),
                _ => panic_type(),
            },
            RuntimeValue::Function {
                identifier: _,
                parameters: val_parameters,
                body: _,
                return_type: val_type,
                is_async: val_is_async,
            } => match t {
                RuntimeType::Function {
                    return_type,
                    parameters,
                    is_async,
                } => {
                    if is_async != *val_is_async {
                        panic!("Check whether the function or type is async.");
                    };

                    if let Some(x) = return_type {
                        if let Some(y) = val_type {
                            if x.deref() != y {
                                panic!("Function has wrong return type");
                            }
                        } else {
                            panic!("Expects a return type");
                        }
                    }

                    if val_parameters.len() != parameters.len() {
                        panic!("Parameters are not equal");
                    };

                    self.clone()
                }
                _ => panic_type(),
            },

            _ => panic_type(),
        }
    }
}
