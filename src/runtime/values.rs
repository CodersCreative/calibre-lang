use std::{
    collections::HashMap,
    fmt::{Debug, write},
};

use crate::runtime::scope::Scope;

#[derive(Debug, Clone, PartialEq)]
pub enum NativeFunctions {
    Print,
}

impl NativeFunctions {}

#[derive(Clone, PartialEq)]
pub enum RuntimeValue {
    Null,
    Float(f64),
    Integer(i64),
    Map(HashMap<String, RuntimeValue>),
    Bool(bool),
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
}
