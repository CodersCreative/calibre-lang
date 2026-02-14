use crate::{VM, value::RuntimeValue};
use rustc_hash::FxHashMap;

pub mod r#async;
pub mod str;
pub mod crypto;
pub mod regex;
pub mod net;
pub mod collections;

impl VM {
    pub fn setup_stdlib(&mut self) {
        setup_scope(
            self,
            "async",
            &[
                "channel_new",
                "channel_send",
                "channel_get",
                "channel_try_get",
                "channel_try_send",
                "channel_close",
                "channel_closed",
                "waitgroup_new",
                "waitgroup_raw_add",
                "waitgroup_raw_done",
                "waitgroup_join",
                "waitgroup_wait",
                "waitgroup_count",
                "mutex_new",
                "mutex_get",
                "mutex_set",
                "mutex_with",
                "mutex_write",
            ],
        );

        setup_scope(
            self,
            "str",
            &["split", "contains", "starts_with", "ends_with"],
        );
        setup_scope(
            self,
            "collections",
            &[
                "hashmap_new",
                "hashmap_set",
                "hashmap_get",
                "hashmap_remove",
                "hashmap_contains",
                "hashmap_len",
                "hashmap_keys",
                "hashmap_values",
                "hashmap_entries",
                "hashmap_clear",
                "hashset_new",
                "hashset_add",
                "hashset_remove",
                "hashset_contains",
                "hashset_len",
                "hashset_values",
                "hashset_clear",
            ],
        );
        setup_scope(self, "crypto", &["sha256", "sha512", "blake3"]);
        setup_scope(self, "regex", &["is_match", "find", "replace"]);
        setup_scope(
            self,
            "net",
            &[
                "tcp_connect",
                "tcp_listen",
                "tcp_accept",
                "tcp_read",
                "tcp_write",
                "tcp_close",
            ],
        );
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
