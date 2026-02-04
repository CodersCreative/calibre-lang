use crate::{VM, value::RuntimeValue};
use rustc_hash::FxHashMap;

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
    fn scope_id(name: &str) -> Option<u64> {
        let mut parts = name.splitn(3, '-');
        let _prefix = parts.next()?;
        let scope = parts.next()?;
        scope.parse::<u64>().ok()
    }

    let map: FxHashMap<String, RuntimeValue> = RuntimeValue::natives();
    let funcs = funcs.into_iter().filter_map(|x| {
        map.get(&format!("{}.{}", name, x))
            .cloned()
            .map(|value| (String::from(*x), value))
    });

    for var in funcs {
        let mut matches: Vec<&String> = env
            .mappings
            .iter()
            .filter(|x| x.split_once(":").map(|v| v.1) == Some(var.0.as_str()))
            .collect();
        matches.sort();
        let name = if let Some(found) = matches
            .iter()
            .find(|x| scope_id(x.as_str()).is_some_and(|id| id != 0))
            .cloned()
        {
            found.to_string()
        } else if matches.len() == 1 {
            matches[0].to_string()
        } else {
            var.0
        };

        let _ = env.variables.insert(name, var.1);
    }
}
