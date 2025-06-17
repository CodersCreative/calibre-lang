use std::{collections::HashMap, mem::discriminant};

use crate::runtime::values::{RuntimeType, RuntimeValue};

use super::Scope;

impl Scope {
    pub fn push_struct(&mut self, key: String, value: &HashMap<String, RuntimeType>) {
        if !self.structs.contains_key(&key) {
            self.structs.insert(key, value.clone());
        } else {
            panic!("Struct identifier already exists.");
        }
    }

    pub fn get_struct(&self, key: &str) -> &HashMap<String, RuntimeType> {
        if let Some(value) = self.resolve_struct(key).structs.get(key) {
            return value;
        } else {
            panic!("Cannot resolve struct : '{}'", key);
        }
    }

    pub fn safe_resolve_struct(&self, key: &str) -> Option<&Self> {
        if self.structs.contains_key(key) {
            Some(self)
        } else if let Some(parent) = &self.parent {
            parent.safe_resolve_struct(key)
        } else {
            None
        }
    }

    pub fn safe_resolve_struct_mut(&mut self, key: &str) -> Option<&mut Self> {
        if self.structs.contains_key(key) {
            Some(self)
        } else if let Some(parent) = &mut self.parent {
            parent.safe_resolve_struct_mut(key)
        } else {
            None
        }
    }

    pub fn resolve_struct_mut(&mut self, key: &str) -> &mut Self {
        if self.structs.contains_key(key) {
            self
        } else if let Some(parent) = &mut self.parent {
            parent.resolve_struct_mut(key)
        } else {
            panic!("Cannot resolve variable : '{}'", key);
        }
    }

    pub fn resolve_struct(&self, key: &str) -> &Self {
        if self.structs.contains_key(key) {
            self
        } else if let Some(parent) = &self.parent {
            parent.resolve_struct(key)
        } else {
            panic!("Cannot resolve variable : '{}'", key);
        }
    }
}
