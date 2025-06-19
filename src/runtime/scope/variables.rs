use std::{cell::RefCell, mem::discriminant, rc::Rc};

use crate::runtime::{scope::variables, values::RuntimeValue};

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

    fn resolve_alias<'a>(&'a self, original: &'a str) -> &'a str {
        if let Some(x) = self.alias.get(original) {
            x
        } else {
            original
        }
    }

    pub fn assign_var(&mut self, og_key: String, value: &RuntimeValue) {
        let key = self.resolve_alias(&og_key).to_string();

        if og_key == key {
            if let Some(v) = self.variables.get_mut(&key) {
                if discriminant(v) == discriminant(value) || v.is_number() && value.is_number() {
                    *v = value.clone();
                    return;
                } else {
                    panic!("Cannot assign differently typed values to one another.");
                }
            } else if self.constants.contains_key(&key) {
                panic!("Variable is a immutable.");
            }
            println!("Variablee. {:?}, {:?}", key, self.variables);
        }

        if let Some(parent) = &self.parent {
            parent.borrow_mut().assign_var(key.to_string(), value);
        } else {
            panic!("Failed to resolve variable {:?}", key)
        }
    }
}

pub fn get_var(this: Rc<RefCell<Scope>>, key: &str) -> RuntimeValue {
    let scope = resolve_var(this, key);
    if let Some(value) = scope.0.borrow().variables.get(&scope.1) {
        return value.clone();
    } else if let Some(value) = scope.0.borrow().constants.get(&scope.1) {
        return value.clone();
    } else {
        panic!("Cannot resolve variable : '{}'", key);
    }
}

pub fn safe_resolve_var(
    this: Rc<RefCell<Scope>>,
    og_key: &str,
) -> Option<(Rc<RefCell<Scope>>, String)> {
    let key = this.borrow().resolve_alias(og_key).to_string();

    if this.borrow().variables.contains_key(&key) || this.borrow().constants.contains_key(&key) {
        return Some((this, key));
    } else if let Some(parent) = &this.borrow().parent {
        return safe_resolve_var(parent.clone(), &key);
    } else {
        None
    }
}

pub fn resolve_var(this: Rc<RefCell<Scope>>, key: &str) -> (Rc<RefCell<Scope>>, String) {
    safe_resolve_var(this, key).expect(&format!("Cannot resolve variable : {}", key))
}
