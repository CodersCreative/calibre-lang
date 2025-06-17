use std::mem::discriminant;

use crate::runtime::values::RuntimeValue;

use super::Scope;

impl Scope {
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
        let scope = self.resolve_var_mut(&key);
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
        if let Some(value) = self.resolve_var(key).variables.get(key) {
            return value;
        } else if let Some(value) = self.resolve_var(key).constants.get(key) {
            return value;
        } else {
            panic!("Cannot resolve variable : '{}'", key);
        }
    }

    pub fn safe_resolve_var(&self, key: &str) -> Option<&Self> {
        if self.variables.contains_key(key) || self.constants.contains_key(key) {
            Some(self)
        } else if let Some(parent) = &self.parent {
            parent.safe_resolve_var(key)
        } else {
            None
        }
    }

    pub fn safe_resolve_var_mut(&mut self, key: &str) -> Option<&mut Self> {
        if self.variables.contains_key(key) || self.constants.contains_key(key) {
            Some(self)
        } else if let Some(parent) = &mut self.parent {
            parent.safe_resolve_var_mut(key)
        } else {
            None
        }
    }

    pub fn resolve_var_mut(&mut self, key: &str) -> &mut Self {
        if self.variables.contains_key(key) || self.constants.contains_key(key) {
            self
        } else if let Some(parent) = &mut self.parent {
            parent.resolve_var_mut(key)
        } else {
            panic!("Cannot resolve variable : '{}'", key);
        }
    }

    pub fn resolve_var(&self, key: &str) -> &Self {
        if self.variables.contains_key(key) || self.constants.contains_key(key) {
            self
        } else if let Some(parent) = &self.parent {
            parent.resolve_var(key)
        } else {
            panic!("Cannot resolve variable : '{}'", key);
        }
    }
}
