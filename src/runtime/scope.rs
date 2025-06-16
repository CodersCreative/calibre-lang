use std::{
    collections::HashMap,
    f64::{self, consts::PI}, panic,
};

use crate::runtime::values::RuntimeValue;

#[derive(Clone)]
pub struct Scope {
    pub parent: Option<Box<Self>>,
    pub variables: HashMap<String, RuntimeValue>,
    pub constants: HashMap<String, RuntimeValue>,
}

fn get_global_variables() -> HashMap<String, RuntimeValue> {
    HashMap::from([
        (String::from("PI"), RuntimeValue::Number(PI)),
        (String::from("MAX"), RuntimeValue::Number(f64::MAX)),
        (String::from("MIN"), RuntimeValue::Number(f64::MIN)),
        (String::from("true"), RuntimeValue::Bool(true)),
        (String::from("false"), RuntimeValue::Bool(false)),
        (String::from("null"), RuntimeValue::Null),
        (
            String::from("INFINITY"),
            RuntimeValue::Number(f64::INFINITY),
        ),
        (
            String::from("NEG_INFINITY"),
            RuntimeValue::Number(f64::NEG_INFINITY),
        ),
    ])
}

impl Scope {
    pub fn new(parent: Option<Box<Self>>) -> Self {
        Self {
            constants: if let None = parent {
                get_global_variables()
            } else {
                HashMap::new()
            },
            variables: HashMap::new(),
            parent,
        }
    }

    pub fn push_var(&mut self, key: String, value: &RuntimeValue, is_mutable: bool) {
        if !self.constants.contains_key(&key) {
            if is_mutable {
                self.variables.insert(key, value.clone());
            } else {
                self.constants.insert(key, value.clone());
            }
        } else {
            panic!("Cannot shadow a non-mutable value");
        }
    }

    pub fn assign_var(&mut self, key: String, value: &RuntimeValue) {
        if self.resolve(&key).variables.contains_key(&key) {
            self.variables.insert(key, value.clone());
        } else {
            panic!("Variable is a immutable.");
        }
    }

    pub fn get_var(&mut self, key: &str) -> &RuntimeValue {
        if let Some(value) = self.resolve(key).variables.get(key) {
            return value;
        } else if let Some(value) = self.resolve(key).constants.get(key) {
            return value;
        } else {
            panic!("Cannot resolve variable : '{}'", key);
        }
    }

    pub fn resolve(&self, key: &str) -> &Self {
        if self.variables.contains_key(key) || self.constants.contains_key(key) {
            self
        } else if let Some(parent) = &self.parent {
            parent.resolve(key)
        } else {
            panic!("Cannot resolve variable : '{}'", key);
        }
    }
}
