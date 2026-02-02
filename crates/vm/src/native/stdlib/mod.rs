use std::collections::HashMap;

use crate::{VM, value::RuntimeValue};

pub mod console;
pub mod random;
pub mod thread;

impl VM {
    pub fn setup_stdlib(&mut self) {
        setup_scope(self, "thread", &["wait"]);
        setup_scope(self, "console", &["out", "input", "err", "clear"]);
        setup_scope(self, "random", &["generate", "bool", "ratio"]);
    }
}

pub fn setup_scope(env: &mut VM, name: &str, funcs: &[&'static str]) {
    let map: HashMap<String, RuntimeValue> = RuntimeValue::natives();
    let funcs = funcs.into_iter().filter_map(|x| {
        map.get(&format!("{}.{}", name, x))
            .cloned()
            .map(|value| (String::from(*x), value))
    });

    for var in funcs {
        let name = env
            .mappings
            .iter()
            .find(|x| x.split_once(":").map(|v| v.1) == Some(var.0.as_str()))
            .map(|x| x.to_string())
            .unwrap_or(var.0);

        let _ = env.variables.insert(name.clone(), var.1);
    }
}
