pub mod objects;
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
    #[error("Unable to resolve object : {0}.")]
    Object(String),
    #[error("Unable to resolve static function : {0}.")]
    Function(String),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum StopValue {
    Return,
    Break,
    Continue,
}

#[derive(Debug, Clone, PartialEq)]
pub enum VarType {
    Mutable(Option<String>),
    Immutable(Option<String>),
    Constant,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Object {
    Enum(Vec<(String, Option<HashMap<String, RuntimeType>>)>),
    Struct(HashMap<String, RuntimeType>),
    NewType(RuntimeType),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Scope {
    pub parent: Option<Rc<RefCell<Self>>>,
    pub variables: HashMap<String, (RuntimeValue, VarType)>,
    pub objects: HashMap<String, Object>,
    pub functions: HashMap<String, HashMap<String, (RuntimeValue, bool)>>,
    pub stop: Option<StopValue>,
}

fn get_global_variables() -> HashMap<String, (RuntimeValue, VarType)> {
    let vars = vec![
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
        (
            String::from("range"),
            RuntimeValue::NativeFunction(NativeFunctions::Range),
        ),
        (String::from("INFINITY"), RuntimeValue::Float(f64::INFINITY)),
        (
            String::from("NEG_INFINITY"),
            RuntimeValue::Float(f64::NEG_INFINITY),
        ),
    ];

    let mut var_hashmap = HashMap::new();

    for var in vars {
        var_hashmap.insert(var.0, (var.1, VarType::Constant));
    }

    var_hashmap
}

impl Scope {
    pub fn new(parent: Option<Rc<RefCell<Self>>>) -> Self {
        Self {
            variables: if let None = parent {
                get_global_variables()
            } else {
                HashMap::new()
            },
            objects: HashMap::new(),
            stop: None,
            functions: HashMap::new(),
            parent,
        }
    }
}
