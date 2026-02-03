use std::{cmp::Ordering, fmt::Debug};

use crate::{VM, error::RuntimeError, value::RuntimeValue};

pub mod global;
pub mod stdlib;

pub trait NativeFunction: Send + Sync {
    fn run(&self, env: &mut VM, args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError>;

    fn name(&self) -> String;

    fn get_resolved_name(&self, env: &VM) -> String {
        let name = self.name();
        let name = env
            .mappings
            .iter()
            .find(|x| x.split_once(":").map(|v| v.1) == Some(name.as_str()))
            .map(|x| x.to_string())
            .unwrap_or(name);

        name
    }
}

impl Debug for dyn NativeFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.name())
    }
}

impl PartialEq for dyn NativeFunction {
    fn ne(&self, _other: &Self) -> bool {
        false
    }

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
    pub fn setup_global(&mut self) {
        let funcs: Vec<&str> = vec![
            "ok",
            "err",
            "some",
            "trim",
            "print",
            "len",
            "panic",
            "tuple",
            "discriminant",
            "min_or_zero",
        ];

        let map = RuntimeValue::natives();

        let mut funcs: Vec<(String, RuntimeValue)> = funcs
            .into_iter()
            .filter_map(|x| map.get(x).cloned().map(|v| (String::from(x), v)))
            .collect();

        let mut vars: Vec<(String, RuntimeValue)> =
            RuntimeValue::constants().into_iter().map(|x| x).collect();
        vars.append(&mut funcs);

        for var in vars {
            let name = self
                .mappings
                .iter()
                .find(|x| x.split_once(":").map(|v| v.1) == Some(var.0.as_str()))
                .map(|x| x.to_string())
                .unwrap_or(var.0);

            let _ = self.variables.insert(name, var.1);
        }
    }
}
