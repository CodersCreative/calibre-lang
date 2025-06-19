use std::{cell::RefCell, collections::HashMap, rc::Rc};

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

    pub fn push_struct_function(&mut self, key: String, value: (String, RuntimeValue, bool)) {
        if let Some(x) = self.structs_functions.get_mut(&key) {
            x.insert(value.0, (value.1, value.2));
        } else if self.structs.contains_key(&key) {
            self.structs_functions
                .insert(key, HashMap::from([(value.0, (value.1, value.2))]));
        } else {
            panic!("Struct does not exist in scope.");
        }
    }
}

pub fn get_struct_function(
    this: Rc<RefCell<Scope>>,
    key: &str,
) -> HashMap<String, (RuntimeValue, bool)> {
    let scope = resolve_struct(this, key);
    if let Some(value) = scope.borrow().structs_functions.get(key) {
        return value.clone();
    } else {
        panic!("Cannot resolve struct : '{}'", key);
    }
}

pub fn safe_resolve_struct_function(
    this: Rc<RefCell<Scope>>,
    og_key: &str,
) -> Option<Rc<RefCell<Scope>>> {
    if this.borrow().structs.contains_key(og_key) {
        return Some(this);
    } else if let Some(parent) = &this.borrow().parent {
        return safe_resolve_struct_function(parent.clone(), og_key);
    } else {
        None
    }
}

pub fn resolve_struct_function(this: Rc<RefCell<Scope>>, key: &str) -> Rc<RefCell<Scope>> {
    safe_resolve_struct_function(this, key).expect(&format!("Cannot resolve variable : {}", key))
}

pub fn get_struct(this: Rc<RefCell<Scope>>, key: &str) -> HashMap<String, RuntimeType> {
    let scope = resolve_struct(this, key);
    if let Some(value) = scope.borrow().structs.get(key) {
        return value.clone();
    } else {
        panic!("Cannot resolve struct : '{}'", key);
    }
}

pub fn safe_resolve_struct(this: Rc<RefCell<Scope>>, og_key: &str) -> Option<Rc<RefCell<Scope>>> {
    if this.borrow().structs.contains_key(og_key) {
        return Some(this);
    } else if let Some(parent) = &this.borrow().parent {
        return safe_resolve_struct(parent.clone(), og_key);
    } else {
        None
    }
}

pub fn resolve_struct(this: Rc<RefCell<Scope>>, key: &str) -> Rc<RefCell<Scope>> {
    safe_resolve_struct(this, key).expect(&format!("Cannot resolve variable : {}", key))
}
