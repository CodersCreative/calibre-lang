use std::{
    collections::HashMap,
    f64::{self, consts::PI},
    i64,
    mem::discriminant,
    panic,
};

use crate::runtime::values::{NativeFunctions, RuntimeValue};

#[derive(Debug, Clone, PartialEq)]
pub struct Scope {
    pub parent: Option<Box<Self>>,
    pub variables: HashMap<String, RuntimeValue>,
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
        let scope = self.resolve_mut(&key);
        if scope.variables.contains_key(&key) {
            if let Some(v) = scope.variables.get_mut(&key) {
                if discriminant(v) == discriminant(value) || v.is_number() && value.is_number() {
                    *v = value.clone()
                } else {
                    panic!("Cannot assign differently typed values to one another.");
                }
            }
        } else {
            panic!("Variable is a immutable.");
        }
    }

    pub fn get_var(&self, key: &str) -> &RuntimeValue {
        if let Some(value) = self.resolve(key).variables.get(key) {
            return value;
        } else if let Some(value) = self.resolve(key).constants.get(key) {
            return value;
        } else {
            panic!("Cannot resolve variable : '{}'", key);
        }
    }

    pub fn safe_resolve(&self, key: &str) -> Option<&Self> {
        if self.variables.contains_key(key) || self.constants.contains_key(key) {
            Some(self)
        } else if let Some(parent) = &self.parent {
            parent.safe_resolve(key)
        } else {
            None
        }
    }

    pub fn safe_resolve_mut(&mut self, key: &str) -> Option<&mut Self> {
        if self.variables.contains_key(key) || self.constants.contains_key(key) {
            Some(self)
        } else if let Some(parent) = &mut self.parent {
            parent.safe_resolve_mut(key)
        } else {
            None
        }
    }

    pub fn resolve_mut(&mut self, key: &str) -> &mut Self {
        if self.variables.contains_key(key) || self.constants.contains_key(key) {
            self
        } else if let Some(parent) = &mut self.parent {
            parent.resolve_mut(key)
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
