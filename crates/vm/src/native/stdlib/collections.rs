use std::sync::{Arc, Mutex};

use rustc_hash::{FxHashMap, FxHashSet};

use crate::{
    VM,
    error::RuntimeError,
    native::NativeFunction,
    value::{HashKey, RuntimeValue},
};
use dumpster::sync::Gc;

fn expect_hash_key(env: &VM, value: RuntimeValue) -> Result<HashKey, RuntimeError> {
    let resolved = env.resolve_value_for_op(value)?;
    HashKey::try_from(resolved)
}

fn tuple_pair(value: RuntimeValue) -> Result<(RuntimeValue, RuntimeValue), RuntimeError> {
    match value {
        RuntimeValue::Aggregate(_, map) => {
            let left = map
                .as_ref()
                .0
                .get("0")
                .cloned()
                .ok_or(RuntimeError::UnexpectedType(RuntimeValue::Null))?;
            let right = map
                .as_ref()
                .0
                .get("1")
                .cloned()
                .ok_or(RuntimeError::UnexpectedType(RuntimeValue::Null))?;
            Ok((left, right))
        }
        other => Err(RuntimeError::UnexpectedType(other)),
    }
}

pub struct HashMapNew;
pub struct HashMapSet;
pub struct HashMapGet;
pub struct HashMapRemove;
pub struct HashMapContains;
pub struct HashMapLen;
pub struct HashMapKeys;
pub struct HashMapValues;
pub struct HashMapEntries;
pub struct HashMapClear;

impl NativeFunction for HashMapNew {
    fn name(&self) -> String {
        String::from("collections.hashmap_new")
    }

    fn run(&self, env: &mut VM, mut args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        let entries = args.pop().unwrap_or(RuntimeValue::List(Gc::new(crate::value::GcVec(Vec::new()))));
        let mut map: FxHashMap<HashKey, RuntimeValue> = FxHashMap::default();

        let RuntimeValue::List(list) = env.resolve_value_for_op(entries)? else {
            return Err(RuntimeError::UnexpectedType(RuntimeValue::Null));
        };

        for item in list.as_ref().0.iter().cloned() {
            let (key, value) = tuple_pair(item)?;
            let key = expect_hash_key(env, key)?;
            let value = env.convert_runtime_var_into_saveable(value);
            map.insert(key, value);
        }

        Ok(RuntimeValue::HashMap(Arc::new(Mutex::new(map))))
    }
}

impl NativeFunction for HashMapSet {
    fn name(&self) -> String {
        String::from("collections.hashmap_set")
    }

    fn run(&self, env: &mut VM, mut args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        let value = args.pop().unwrap_or(RuntimeValue::Null);
        let key = args.pop().unwrap_or(RuntimeValue::Null);
        let map_val = args.pop().unwrap_or(RuntimeValue::Null);
        let map_val = env.resolve_value_for_op(map_val)?;
        let RuntimeValue::HashMap(map) = map_val else {
            return Err(RuntimeError::UnexpectedType(map_val));
        };

        let key = expect_hash_key(env, key)?;
        let value = env.convert_runtime_var_into_saveable(value);
        if let Ok(mut guard) = map.lock() {
            guard.insert(key, value);
        }

        Ok(RuntimeValue::Null)
    }
}

impl NativeFunction for HashMapGet {
    fn name(&self) -> String {
        String::from("collections.hashmap_get")
    }

    fn run(&self, env: &mut VM, mut args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        let key = args.pop().unwrap_or(RuntimeValue::Null);
        let map_val = args.pop().unwrap_or(RuntimeValue::Null);
        let map_val = env.resolve_value_for_op(map_val)?;
        let RuntimeValue::HashMap(map) = map_val else {
            return Err(RuntimeError::UnexpectedType(map_val));
        };

        let key = expect_hash_key(env, key)?;
        if let Ok(guard) = map.lock() {
            if let Some(value) = guard.get(&key) {
                return Ok(RuntimeValue::Option(Some(Gc::new(value.clone()))));
            }
        }
        Ok(RuntimeValue::Option(None))
    }
}

impl NativeFunction for HashMapRemove {
    fn name(&self) -> String {
        String::from("collections.hashmap_remove")
    }

    fn run(&self, env: &mut VM, mut args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        let key = args.pop().unwrap_or(RuntimeValue::Null);
        let map_val = args.pop().unwrap_or(RuntimeValue::Null);
        let map_val = env.resolve_value_for_op(map_val)?;
        let RuntimeValue::HashMap(map) = map_val else {
            return Err(RuntimeError::UnexpectedType(map_val));
        };

        let key = expect_hash_key(env, key)?;
        if let Ok(mut guard) = map.lock() {
            if let Some(value) = guard.remove(&key) {
                return Ok(RuntimeValue::Option(Some(Gc::new(value))));
            }
        }
        Ok(RuntimeValue::Option(None))
    }
}

impl NativeFunction for HashMapContains {
    fn name(&self) -> String {
        String::from("collections.hashmap_contains")
    }

    fn run(&self, env: &mut VM, mut args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        let key = args.pop().unwrap_or(RuntimeValue::Null);
        let map_val = args.pop().unwrap_or(RuntimeValue::Null);
        let map_val = env.resolve_value_for_op(map_val)?;
        let RuntimeValue::HashMap(map) = map_val else {
            return Err(RuntimeError::UnexpectedType(map_val));
        };

        let key = expect_hash_key(env, key)?;
        if let Ok(guard) = map.lock() {
            return Ok(RuntimeValue::Bool(guard.contains_key(&key)));
        }
        Ok(RuntimeValue::Bool(false))
    }
}

impl NativeFunction for HashMapLen {
    fn name(&self) -> String {
        String::from("collections.hashmap_len")
    }

    fn run(&self, env: &mut VM, mut args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        let map_val = args.pop().unwrap_or(RuntimeValue::Null);
        let map_val = env.resolve_value_for_op(map_val)?;
        let RuntimeValue::HashMap(map) = map_val else {
            return Err(RuntimeError::UnexpectedType(map_val));
        };

        let len = map.lock().map(|m| m.len() as i64).unwrap_or(0);
        Ok(RuntimeValue::Int(len))
    }
}

impl NativeFunction for HashMapKeys {
    fn name(&self) -> String {
        String::from("collections.hashmap_keys")
    }

    fn run(&self, env: &mut VM, mut args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        let map_val = args.pop().unwrap_or(RuntimeValue::Null);
        let map_val = env.resolve_value_for_op(map_val)?;
        let RuntimeValue::HashMap(map) = map_val else {
            return Err(RuntimeError::UnexpectedType(map_val));
        };

        let mut out = Vec::new();
        if let Ok(guard) = map.lock() {
            for key in guard.keys() {
                out.push(RuntimeValue::from(key.clone()));
            }
        }

        Ok(RuntimeValue::List(Gc::new(crate::value::GcVec(out))))
    }
}

impl NativeFunction for HashMapValues {
    fn name(&self) -> String {
        String::from("collections.hashmap_values")
    }

    fn run(&self, env: &mut VM, mut args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        let map_val = args.pop().unwrap_or(RuntimeValue::Null);
        let map_val = env.resolve_value_for_op(map_val)?;
        let RuntimeValue::HashMap(map) = map_val else {
            return Err(RuntimeError::UnexpectedType(map_val));
        };

        let mut out = Vec::new();
        if let Ok(guard) = map.lock() {
            for value in guard.values() {
                out.push(value.clone());
            }
        }

        Ok(RuntimeValue::List(Gc::new(crate::value::GcVec(out))))
    }
}

impl NativeFunction for HashMapEntries {
    fn name(&self) -> String {
        String::from("collections.hashmap_entries")
    }

    fn run(&self, env: &mut VM, mut args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        let map_val = args.pop().unwrap_or(RuntimeValue::Null);
        let map_val = env.resolve_value_for_op(map_val)?;
        let RuntimeValue::HashMap(map) = map_val else {
            return Err(RuntimeError::UnexpectedType(map_val));
        };

        let mut out = Vec::new();
        if let Ok(guard) = map.lock() {
            for (key, value) in guard.iter() {
                let pair = RuntimeValue::Aggregate(
                    None,
                    Gc::new(crate::value::GcMap(
                        vec![
                            ("0".to_string(), RuntimeValue::from(key.clone())),
                            ("1".to_string(), value.clone()),
                        ]
                        .into(),
                    )),
                );
                out.push(pair);
            }
        }

        Ok(RuntimeValue::List(Gc::new(crate::value::GcVec(out))))
    }
}

impl NativeFunction for HashMapClear {
    fn name(&self) -> String {
        String::from("collections.hashmap_clear")
    }

    fn run(&self, env: &mut VM, mut args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        let map_val = args.pop().unwrap_or(RuntimeValue::Null);
        let map_val = env.resolve_value_for_op(map_val)?;
        let RuntimeValue::HashMap(map) = map_val else {
            return Err(RuntimeError::UnexpectedType(map_val));
        };

        if let Ok(mut guard) = map.lock() {
            guard.clear();
        }

        Ok(RuntimeValue::Null)
    }
}

pub struct HashSetNew;
pub struct HashSetAdd;
pub struct HashSetRemove;
pub struct HashSetContains;
pub struct HashSetLen;
pub struct HashSetValues;
pub struct HashSetClear;

impl NativeFunction for HashSetNew {
    fn name(&self) -> String {
        String::from("collections.hashset_new")
    }

    fn run(&self, env: &mut VM, mut args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        let entries = args.pop().unwrap_or(RuntimeValue::List(Gc::new(crate::value::GcVec(Vec::new()))));
        let mut set: FxHashSet<HashKey> = FxHashSet::default();

        let RuntimeValue::List(list) = env.resolve_value_for_op(entries)? else {
            return Err(RuntimeError::UnexpectedType(RuntimeValue::Null));
        };

        for item in list.as_ref().0.iter().cloned() {
            let key = expect_hash_key(env, item)?;
            set.insert(key);
        }

        Ok(RuntimeValue::HashSet(Arc::new(Mutex::new(set))))
    }
}

impl NativeFunction for HashSetAdd {
    fn name(&self) -> String {
        String::from("collections.hashset_add")
    }

    fn run(&self, env: &mut VM, mut args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        let key = args.pop().unwrap_or(RuntimeValue::Null);
        let set_val = args.pop().unwrap_or(RuntimeValue::Null);
        let set_val = env.resolve_value_for_op(set_val)?;
        let RuntimeValue::HashSet(set) = set_val else {
            return Err(RuntimeError::UnexpectedType(set_val));
        };

        let key = expect_hash_key(env, key)?;
        let inserted = if let Ok(mut guard) = set.lock() {
            guard.insert(key)
        } else {
            false
        };

        Ok(RuntimeValue::Bool(inserted))
    }
}

impl NativeFunction for HashSetRemove {
    fn name(&self) -> String {
        String::from("collections.hashset_remove")
    }

    fn run(&self, env: &mut VM, mut args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        let key = args.pop().unwrap_or(RuntimeValue::Null);
        let set_val = args.pop().unwrap_or(RuntimeValue::Null);
        let set_val = env.resolve_value_for_op(set_val)?;
        let RuntimeValue::HashSet(set) = set_val else {
            return Err(RuntimeError::UnexpectedType(set_val));
        };

        let key = expect_hash_key(env, key)?;
        let removed = if let Ok(mut guard) = set.lock() {
            guard.remove(&key)
        } else {
            false
        };

        Ok(RuntimeValue::Bool(removed))
    }
}

impl NativeFunction for HashSetContains {
    fn name(&self) -> String {
        String::from("collections.hashset_contains")
    }

    fn run(&self, env: &mut VM, mut args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        let key = args.pop().unwrap_or(RuntimeValue::Null);
        let set_val = args.pop().unwrap_or(RuntimeValue::Null);
        let set_val = env.resolve_value_for_op(set_val)?;
        let RuntimeValue::HashSet(set) = set_val else {
            return Err(RuntimeError::UnexpectedType(set_val));
        };

        let key = expect_hash_key(env, key)?;
        let contains = if let Ok(guard) = set.lock() {
            guard.contains(&key)
        } else {
            false
        };

        Ok(RuntimeValue::Bool(contains))
    }
}

impl NativeFunction for HashSetLen {
    fn name(&self) -> String {
        String::from("collections.hashset_len")
    }

    fn run(&self, env: &mut VM, mut args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        let set_val = args.pop().unwrap_or(RuntimeValue::Null);
        let set_val = env.resolve_value_for_op(set_val)?;
        let RuntimeValue::HashSet(set) = set_val else {
            return Err(RuntimeError::UnexpectedType(set_val));
        };

        let len = set.lock().map(|s| s.len() as i64).unwrap_or(0);
        Ok(RuntimeValue::Int(len))
    }
}

impl NativeFunction for HashSetValues {
    fn name(&self) -> String {
        String::from("collections.hashset_values")
    }

    fn run(&self, env: &mut VM, mut args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        let set_val = args.pop().unwrap_or(RuntimeValue::Null);
        let set_val = env.resolve_value_for_op(set_val)?;
        let RuntimeValue::HashSet(set) = set_val else {
            return Err(RuntimeError::UnexpectedType(set_val));
        };

        let mut out = Vec::new();
        if let Ok(guard) = set.lock() {
            for key in guard.iter() {
                out.push(RuntimeValue::from(key.clone()));
            }
        }

        Ok(RuntimeValue::List(Gc::new(crate::value::GcVec(out))))
    }
}

impl NativeFunction for HashSetClear {
    fn name(&self) -> String {
        String::from("collections.hashset_clear")
    }

    fn run(&self, env: &mut VM, mut args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        let set_val = args.pop().unwrap_or(RuntimeValue::Null);
        let set_val = env.resolve_value_for_op(set_val)?;
        let RuntimeValue::HashSet(set) = set_val else {
            return Err(RuntimeError::UnexpectedType(set_val));
        };

        if let Ok(mut guard) = set.lock() {
            guard.clear();
        }

        Ok(RuntimeValue::Null)
    }
}
