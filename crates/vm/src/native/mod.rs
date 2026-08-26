use crate::{VM, error::RuntimeError, value::RuntimeValue};
use std::{cmp::Ordering, fmt::Debug};

pub mod global;
pub mod stdlib;
pub mod utils;

pub trait NativeFunction: Send + Sync {
    fn run(&self, env: &mut VM, args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError>;

    fn name(&self) -> String;

    fn get_resolved_name(&self, env: &VM) -> String {
        let name = self.name();
        env.registry.natives.get(&name).cloned().unwrap_or_default()
    }
}

impl Debug for dyn NativeFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.name())
    }
}

impl PartialEq for dyn NativeFunction {
    fn eq(&self, _other: &Self) -> bool {
        true
    }
}

impl PartialOrd for dyn NativeFunction {
    fn gt(&self, _other: &Self) -> bool {
        false
    }

    fn lt(&self, _other: &Self) -> bool {
        false
    }

    fn ge(&self, _other: &Self) -> bool {
        true
    }

    fn le(&self, _other: &Self) -> bool {
        true
    }

    fn partial_cmp(&self, _other: &Self) -> Option<std::cmp::Ordering> {
        Some(Ordering::Equal)
    }
}

impl VM {
    pub fn setup_stdlib(&mut self) {
        for (full_name, value) in RuntimeValue::constants()
            .iter()
            .chain(RuntimeValue::natives())
        {
            let name = self.registry.natives.get(full_name).unwrap();
            let _ = self.variables.insert(name, value.clone());
        }
    }
}
