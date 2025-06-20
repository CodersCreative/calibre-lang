pub mod enums;
pub mod structs;
pub mod variables;

use std::{
    cell::RefCell,
    collections::HashMap,
    f64::{self, consts::PI},
    i64,
    rc::Rc,
};

use thiserror::Error;

use crate::runtime::values::{NativeFunctions, RuntimeValue};

use super::values::RuntimeType;

#[derive(Error, Debug, Clone)]
pub enum ScopeErr {
    #[error("Unable to resolve variable : {0}.")]
    Variable(String),
    #[error("Unable to assign immutable variable : {0}.")]
    AssignConstant(String),
    #[error("Unable to shadow immutable variable : {0}.")]
    ShadowConstant(String),
    #[error("Variable types dont match : {0:?} and {1:?}.")]
    TypeMismatch(RuntimeValue, RuntimeValue),
    #[error("Unable to resolve struct : {0}.")]
    Struct(String),
    #[error("Unable to resolve static function : {0}.")]
    StructFunction(String),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Scope {
    pub parent: Option<Rc<RefCell<Self>>>,
    pub variables: HashMap<String, RuntimeValue>,
    pub alias: HashMap<String, String>,
    pub enums: HashMap<String, Vec<(String, Option<HashMap<String, RuntimeType>>)>>,
    pub structs: HashMap<String, HashMap<String, RuntimeType>>,
    pub structs_functions: HashMap<String, HashMap<String, (RuntimeValue, bool)>>,
    pub constants: HashMap<String, RuntimeValue>,
}

fn get_global_variables() -> HashMap<String, RuntimeValue> {
    HashMap::from([
        (String::from("PI"), RuntimeValue::Float(PI)),
        (String::from("FLOAT_MAX"), RuntimeValue::Float(f64::MAX)),
        (String::from("INT_MAX"), RuntimeValue::Integer(i64::MAX)),
        (String::from("FLOAT_MIN"), RuntimeValue::Float(f64::MIN)),
        (String::from("INT_MIN"), RuntimeValue::Integer(i64::MIN)),
        (String::from("true"), RuntimeValue::Bool(true)),
        (String::from("false"), RuntimeValue::Bool(false)),
        (String::from("null"), RuntimeValue::Null),
        (
            String::from("print"),
            RuntimeValue::NativeFunction(NativeFunctions::Print),
        ),
        (String::from("INFINITY"), RuntimeValue::Float(f64::INFINITY)),
        (
            String::from("NEG_INFINITY"),
            RuntimeValue::Float(f64::NEG_INFINITY),
        ),
    ])
}

impl Scope {
    pub fn new(parent: Option<Rc<RefCell<Self>>>) -> Self {
        Self {
            constants: if let None = parent {
                get_global_variables()
            } else {
                HashMap::new()
            },
            alias: HashMap::new(),
            enums: HashMap::new(),
            variables: HashMap::new(),
            structs: HashMap::new(),
            structs_functions: HashMap::new(),
            parent,
        }
    }
}
