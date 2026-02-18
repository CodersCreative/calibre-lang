use rustc_hash::FxHashMap;
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
        fn scope_id(name: &str) -> Option<u64> {
            let mut parts = name.splitn(3, '-');
            let _prefix = parts.next()?;
            let scope = parts.next()?;
            scope.parse::<u64>().ok()
        }

        let mut buckets: FxHashMap<String, Vec<String>> =
            FxHashMap::with_capacity_and_hasher(self.mappings.len(), Default::default());
        for full in self.mappings.iter() {
            if let Some((_, short)) = full.split_once(':') {
                buckets
                    .entry(short.to_string())
                    .or_default()
                    .push(full.clone());
            }
        }
        let mut mapping_index: FxHashMap<String, Option<String>> =
            FxHashMap::with_capacity_and_hasher(buckets.len(), Default::default());
        for (short, mut matches) in buckets {
            matches.sort();
            let chosen =
                if let Some(found) = matches.iter().find(|x| scope_id(x.as_str()) == Some(0)) {
                    Some(found.clone())
                } else if matches.len() == 1 {
                    Some(matches[0].clone())
                } else {
                    None
                };
            mapping_index.insert(short, chosen);
        }

        let funcs: Vec<&str> = vec![
            "console_output",
            "ok",
            "err",
            "some",
            "trim",
            "len",
            "panic",
            "assert",
            "tuple",
            "discriminant",
            "min_or_zero",
            "http_request_raw",
            "http_request_try",
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
            let name = mapping_index
                .get(&var.0)
                .and_then(|x| x.clone())
                .unwrap_or(var.0);

            let _ = self.variables.insert(name, var.1);
        }
    }
}
