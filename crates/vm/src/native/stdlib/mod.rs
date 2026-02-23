use crate::{VM, value::RuntimeValue};
use rustc_hash::FxHashMap;

pub mod args;
pub mod r#async;
pub mod collections;
pub mod crypto;
pub mod list;
pub mod net;
pub mod regex;
pub mod str;

impl VM {
    pub fn setup_stdlib(&mut self) {
        let mapping_index = build_mapping_index(&self.mappings);
        let async_funcs = &[
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
        ];
        let str_funcs = &["split", "contains", "starts_with", "ends_with"];
        let args_funcs = &["len", "get", "all"];
        let collections_funcs = &[
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
        ];
        let crypto_funcs = &["sha256", "sha512", "blake3"];
        let list_funcs = &["sort_by", "binary_search_by", "raw_remove"];
        let regex_funcs = &["is_match", "find", "replace"];
        let net_funcs = &[
            "tcp_connect",
            "tcp_listen",
            "tcp_accept",
            "tcp_read",
            "tcp_write",
            "tcp_close",
        ];

        let mapping_index_ref = &mapping_index;
        let prepared = std::thread::scope(|s| {
            let async_h = s.spawn(|| prepare_scope(mapping_index_ref, "async", async_funcs));
            let str_h = s.spawn(|| prepare_scope(mapping_index_ref, "str", str_funcs));
            let collections_h =
                s.spawn(|| prepare_scope(mapping_index_ref, "collections", collections_funcs));
            let crypto_h = s.spawn(|| prepare_scope(mapping_index_ref, "crypto", crypto_funcs));
            let list_h = s.spawn(|| prepare_scope(mapping_index_ref, "list", list_funcs));
            let regex_h = s.spawn(|| prepare_scope(mapping_index_ref, "regex", regex_funcs));
            let net_h = s.spawn(|| prepare_scope(mapping_index_ref, "net", net_funcs));
            let args_h =
                s.spawn(|| prepare_scope_with_alias(mapping_index_ref, "args", args_funcs, false));

            let mut out = Vec::with_capacity(
                async_funcs.len()
                    + str_funcs.len()
                    + collections_funcs.len()
                    + crypto_funcs.len()
                    + list_funcs.len()
                    + regex_funcs.len()
                    + net_funcs.len()
                    + args_funcs.len(),
            );
            out.extend(async_h.join().unwrap_or_default());
            out.extend(str_h.join().unwrap_or_default());
            out.extend(collections_h.join().unwrap_or_default());
            out.extend(crypto_h.join().unwrap_or_default());
            out.extend(list_h.join().unwrap_or_default());
            out.extend(regex_h.join().unwrap_or_default());
            out.extend(net_h.join().unwrap_or_default());
            out.extend(args_h.join().unwrap_or_default());
            out
        });

        for (name, value) in prepared {
            let _ = self.variables.insert(name, value);
        }
    }
}

fn build_mapping_index(
    mappings: &std::sync::Arc<Vec<String>>,
) -> FxHashMap<String, Option<String>> {
    fn scope_id(name: &str) -> Option<u64> {
        let mut parts = name.splitn(3, '-');
        let _prefix = parts.next()?;
        let scope = parts.next()?;
        scope.parse::<u64>().ok()
    }

    let mut buckets: FxHashMap<String, Vec<String>> =
        FxHashMap::with_capacity_and_hasher(mappings.len(), Default::default());
    for full in mappings.iter() {
        if let Some((_, short)) = full.split_once(':') {
            buckets
                .entry(short.to_string())
                .or_default()
                .push(full.clone());
        }
    }

    let mut index: FxHashMap<String, Option<String>> =
        FxHashMap::with_capacity_and_hasher(buckets.len(), Default::default());
    for (short, mut matches) in buckets {
        matches.sort();
        let chosen = if let Some(found) = matches
            .iter()
            .find(|x| scope_id(x.as_str()).is_some_and(|id| id != 0))
        {
            Some(found.clone())
        } else if matches.len() == 1 {
            Some(matches[0].clone())
        } else {
            None
        };
        index.insert(short, chosen);
    }

    index
}

pub fn setup_scope(
    env: &mut VM,
    mapping_index: &FxHashMap<String, Option<String>>,
    name: &str,
    funcs: &[&'static str],
) {
    let map: FxHashMap<String, RuntimeValue> = RuntimeValue::natives();
    let funcs = funcs.into_iter().filter_map(|x| {
        map.get(&format!("{}.{}", name, x))
            .cloned()
            .map(|value| (String::from(*x), value))
    });

    for var in funcs {
        let name = mapping_index
            .get(&var.0)
            .and_then(|x| x.clone())
            .unwrap_or(var.0);
        let _ = env.variables.insert(name, var.1);
    }
}

fn prepare_scope(
    mapping_index: &FxHashMap<String, Option<String>>,
    name: &str,
    funcs: &[&'static str],
) -> Vec<(String, RuntimeValue)> {
    prepare_scope_with_alias(mapping_index, name, funcs, true)
}

fn prepare_scope_with_alias(
    mapping_index: &FxHashMap<String, Option<String>>,
    name: &str,
    funcs: &[&'static str],
    include_short_alias: bool,
) -> Vec<(String, RuntimeValue)> {
    let map: FxHashMap<String, RuntimeValue> = RuntimeValue::natives();
    let mut out = Vec::with_capacity(funcs.len());
    for func in funcs {
        if let Some(value) = map.get(&format!("{}.{}", name, func)).cloned() {
            let key = mapping_index
                .get(*func)
                .and_then(|x| x.clone())
                .unwrap_or_else(|| (*func).to_string());
            out.push((key.clone(), value.clone()));
            if include_short_alias && key != *func {
                out.push(((*func).to_string(), value));
            }
        }
    }
    out
}
