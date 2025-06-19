use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::runtime::values::RuntimeType;

use super::Scope;

impl Scope {
    pub fn push_struct(&mut self, key: String, value: &HashMap<String, RuntimeType>) {
        if !self.structs.contains_key(&key) {
            self.structs.insert(key, value.clone());
        } else {
            panic!("Struct identifier already exists.");
        }
    }
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
