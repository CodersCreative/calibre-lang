use crate::{
    VM,
    error::RuntimeError,
    native::{
        NativeFunction,
        utils::{expect_num_args, pop_or_null, resolve_hash_key, resolve_hashmap, resolve_hashset},
    },
    value::{GcVec, HashKey, RuntimeValue},
};
use dumpster::sync::Gc;
use rustc_hash::{FxHashMap, FxHashSet};
use std::sync::Arc;
use wasm_sync::Mutex;

fn tuple_pair(value: RuntimeValue) -> Result<(RuntimeValue, RuntimeValue), RuntimeError> {
    match value {
        RuntimeValue::Aggregate(_, map) => {
            let left = map
                .as_ref()
                .0
                .get("0")
                .cloned()
                .ok_or(RuntimeError::UnexpectedType(Box::new(RuntimeValue::Null)))?;
            let right = map
                .as_ref()
                .0
                .get("1")
                .cloned()
                .ok_or(RuntimeError::UnexpectedType(Box::new(RuntimeValue::Null)))?;
            Ok((left, right))
        }
        other => Err(RuntimeError::UnexpectedType(Box::new(other))),
    }
}

pub struct HashMapNew;

impl NativeFunction for HashMapNew {
    fn name(&self) -> String {
        String::from("collections.hashmap_new")
    }

    fn run(&self, env: &mut VM, mut args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        expect_num_args(&args, &[0, 1])?;

        let entries = args
            .pop()
            .unwrap_or(RuntimeValue::List(Gc::new(GcVec(Vec::new()))));
        let mut map: FxHashMap<HashKey, RuntimeValue> = FxHashMap::default();

        let RuntimeValue::List(list) = env.resolve_value_for_op_ref(&entries)? else {
            return Err(RuntimeError::UnexpectedType(Box::new(RuntimeValue::Null)));
        };

        for item in list.as_ref().0.iter().cloned() {
            let (key, value) = tuple_pair(item)?;

            let key = resolve_hash_key(env, &key)?;
            let value = env.convert_runtime_var_into_saveable(value);

            map.insert(key, value);
        }

        Ok(RuntimeValue::HashMap(Arc::new(Mutex::new(map))))
    }
}

pub struct HashMapSet;

impl NativeFunction for HashMapSet {
    fn name(&self) -> String {
        String::from("collections.hashmap_set")
    }

    fn run(&self, env: &mut VM, mut args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        expect_num_args(&args, &[3])?;

        let value = pop_or_null(&mut args);
        let key = resolve_hash_key(env, &pop_or_null(&mut args))?;
        let map = resolve_hashmap(env, &pop_or_null(&mut args))?;

        if let Ok(mut guard) = map.try_lock() {
            guard.insert(key, value);
        }

        Ok(RuntimeValue::Null)
    }
}

pub struct HashMapGet;

impl NativeFunction for HashMapGet {
    fn name(&self) -> String {
        String::from("collections.hashmap_get")
    }

    fn run(&self, env: &mut VM, mut args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        expect_num_args(&args, &[2])?;

        let key = resolve_hash_key(env, &pop_or_null(&mut args))?;
        let map = resolve_hashmap(env, &pop_or_null(&mut args))?;

        if let Ok(guard) = map.try_lock()
            && let Some(value) = guard.get(&key)
        {
            return Ok(RuntimeValue::Option(Some(Gc::new(value.clone()))));
        }

        Ok(RuntimeValue::Option(None))
    }
}

pub struct HashMapRemove;

impl NativeFunction for HashMapRemove {
    fn name(&self) -> String {
        String::from("collections.hashmap_remove")
    }

    fn run(&self, env: &mut VM, mut args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        expect_num_args(&args, &[2])?;

        let key = resolve_hash_key(env, &pop_or_null(&mut args))?;
        let map = resolve_hashmap(env, &pop_or_null(&mut args))?;

        if let Ok(mut guard) = map.try_lock()
            && let Some(value) = guard.remove(&key)
        {
            return Ok(RuntimeValue::Option(Some(Gc::new(value))));
        }

        Ok(RuntimeValue::Option(None))
    }
}

pub struct HashMapContains;

impl NativeFunction for HashMapContains {
    fn name(&self) -> String {
        String::from("collections.hashmap_contains")
    }

    fn run(&self, env: &mut VM, mut args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        expect_num_args(&args, &[2])?;

        let key = resolve_hash_key(env, &pop_or_null(&mut args))?;
        let map = resolve_hashmap(env, &pop_or_null(&mut args))?;

        if let Ok(guard) = map.try_lock() {
            return Ok(RuntimeValue::Bool(guard.contains_key(&key)));
        }

        Ok(RuntimeValue::Bool(false))
    }
}

pub struct HashMapLen;

impl NativeFunction for HashMapLen {
    fn name(&self) -> String {
        String::from("collections.hashmap_len")
    }

    fn run(&self, env: &mut VM, mut args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        expect_num_args(&args, &[1])?;

        let map = resolve_hashmap(env, &pop_or_null(&mut args))?;

        let len = map.lock().unwrap().len() as i64;
        Ok(RuntimeValue::Int(len))
    }
}

pub struct HashMapKeys;

impl NativeFunction for HashMapKeys {
    fn name(&self) -> String {
        String::from("collections.hashmap_keys")
    }

    fn run(&self, env: &mut VM, mut args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        expect_num_args(&args, &[1])?;

        let map = resolve_hashmap(env, &pop_or_null(&mut args))?;

        let mut out = Vec::new();
        if let Ok(guard) = map.try_lock() {
            out = guard
                .keys()
                .map(|key| RuntimeValue::from(key.clone()))
                .collect();
        }

        Ok(RuntimeValue::List(Gc::new(GcVec(out))))
    }
}

pub struct HashMapValues;

impl NativeFunction for HashMapValues {
    fn name(&self) -> String {
        String::from("collections.hashmap_values")
    }

    fn run(&self, env: &mut VM, mut args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        expect_num_args(&args, &[1])?;

        let map = resolve_hashmap(env, &pop_or_null(&mut args))?;

        let mut out = Vec::new();
        if let Ok(guard) = map.try_lock() {
            out = guard.values().cloned().collect();
        }

        Ok(RuntimeValue::List(Gc::new(GcVec(out))))
    }
}

pub struct HashMapEntries;

impl NativeFunction for HashMapEntries {
    fn name(&self) -> String {
        String::from("collections.hashmap_entries")
    }

    fn run(&self, env: &mut VM, mut args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        expect_num_args(&args, &[1])?;

        let map = resolve_hashmap(env, &pop_or_null(&mut args))?;

        let mut out = Vec::new();
        if let Ok(guard) = map.try_lock() {
            out = guard
                .clone()
                .into_iter()
                .map(|(key, value)| {
                    RuntimeValue::Aggregate(
                        None,
                        Gc::new(crate::value::GcMap(
                            vec![
                                ("0".to_string(), RuntimeValue::from(key)),
                                ("1".to_string(), value),
                            ]
                            .into(),
                        )),
                    )
                })
                .collect();
        }

        Ok(RuntimeValue::List(Gc::new(GcVec(out))))
    }
}

pub struct HashMapClear;

impl NativeFunction for HashMapClear {
    fn name(&self) -> String {
        String::from("collections.hashmap_clear")
    }

    fn run(&self, env: &mut VM, mut args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        expect_num_args(&args, &[1])?;

        let map = resolve_hashmap(env, &pop_or_null(&mut args))?;

        if let Ok(mut guard) = map.try_lock() {
            guard.clear();
        }

        Ok(RuntimeValue::Null)
    }
}

pub struct HashSetNew;

impl NativeFunction for HashSetNew {
    fn name(&self) -> String {
        String::from("collections.hashset_new")
    }

    fn run(&self, env: &mut VM, mut args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        expect_num_args(&args, &[0, 1])?;

        let entries = args
            .pop()
            .unwrap_or(RuntimeValue::List(Gc::new(GcVec(Vec::new()))));
        let mut set: FxHashSet<HashKey> = FxHashSet::default();

        let RuntimeValue::List(list) = env.resolve_value_for_op_ref(&entries)? else {
            return Err(RuntimeError::UnexpectedType(Box::new(RuntimeValue::Null)));
        };

        for item in list.as_ref().0.iter() {
            let key = resolve_hash_key(env, item)?;
            set.insert(key);
        }

        Ok(RuntimeValue::HashSet(Arc::new(Mutex::new(set))))
    }
}

pub struct HashSetAdd;

impl NativeFunction for HashSetAdd {
    fn name(&self) -> String {
        String::from("collections.hashset_add")
    }

    fn run(&self, env: &mut VM, mut args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        expect_num_args(&args, &[2])?;

        let key = resolve_hash_key(env, &pop_or_null(&mut args))?;
        let set = resolve_hashset(env, &pop_or_null(&mut args))?;

        let inserted = if let Ok(mut guard) = set.try_lock() {
            guard.insert(key)
        } else {
            false
        };

        Ok(RuntimeValue::Bool(inserted))
    }
}

pub struct HashSetRemove;

impl NativeFunction for HashSetRemove {
    fn name(&self) -> String {
        String::from("collections.hashset_remove")
    }

    fn run(&self, env: &mut VM, mut args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        expect_num_args(&args, &[2])?;

        let key = resolve_hash_key(env, &pop_or_null(&mut args))?;
        let set = resolve_hashset(env, &pop_or_null(&mut args))?;

        let removed = if let Ok(mut guard) = set.try_lock() {
            guard.remove(&key)
        } else {
            false
        };

        Ok(RuntimeValue::Bool(removed))
    }
}

pub struct HashSetContains;

impl NativeFunction for HashSetContains {
    fn name(&self) -> String {
        String::from("collections.hashset_contains")
    }

    fn run(&self, env: &mut VM, mut args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        expect_num_args(&args, &[2])?;

        let key = resolve_hash_key(env, &pop_or_null(&mut args))?;
        let set = resolve_hashset(env, &pop_or_null(&mut args))?;

        let contains = if let Ok(guard) = set.try_lock() {
            guard.contains(&key)
        } else {
            false
        };

        Ok(RuntimeValue::Bool(contains))
    }
}

pub struct HashSetLen;

impl NativeFunction for HashSetLen {
    fn name(&self) -> String {
        String::from("collections.hashset_len")
    }

    fn run(&self, env: &mut VM, mut args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        expect_num_args(&args, &[1])?;

        let set = resolve_hashset(env, &pop_or_null(&mut args))?;

        let len = set.lock().unwrap().len() as i64;
        Ok(RuntimeValue::Int(len))
    }
}

pub struct HashSetValues;

impl NativeFunction for HashSetValues {
    fn name(&self) -> String {
        String::from("collections.hashset_values")
    }

    fn run(&self, env: &mut VM, mut args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        expect_num_args(&args, &[1])?;

        let set = resolve_hashset(env, &pop_or_null(&mut args))?;

        let mut out = Vec::new();
        if let Ok(guard) = set.try_lock() {
            out = guard.clone().into_iter().map(RuntimeValue::from).collect();
        }

        Ok(RuntimeValue::List(Gc::new(GcVec(out))))
    }
}

pub struct HashSetClear;

impl NativeFunction for HashSetClear {
    fn name(&self) -> String {
        String::from("collections.hashset_clear")
    }

    fn run(&self, env: &mut VM, mut args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        expect_num_args(&args, &[1])?;

        let set = resolve_hashset(env, &pop_or_null(&mut args))?;

        if let Ok(mut guard) = set.try_lock() {
            guard.clear();
        }

        Ok(RuntimeValue::Null)
    }
}
