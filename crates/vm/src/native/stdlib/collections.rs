use crate::{
    VM,
    error::RuntimeError,
    native::{
        NativeFunction,
        utils::{
            expect_num_args, pop_or_null, resolve_hash_key, resolve_hash_key_ref, resolve_hashmap,
            resolve_hashset,
        },
    },
    value::{
        GcVec, RuntimeValue,
        hashable::{HashKey, RuntimeHashMap, RuntimeHashSet},
    },
};
use calibre_parser::ast::types::ParserInnerType;
use dumpster::sync::Gc;
use rustc_hash::{FxHashMap, FxHashSet};
use std::sync::Arc;

fn tuple_pair(value: RuntimeValue) -> Result<(RuntimeValue, RuntimeValue), RuntimeError> {
    match value {
        RuntimeValue::Aggregate(_, map) => {
            let left = map.as_ref().0.get("0").cloned().ok_or(
                RuntimeError::UnexpectedTypeInConversion {
                    value: Box::new(RuntimeValue::Null),
                    target_type: ParserInnerType::Str,
                },
            )?;

            let right = map.as_ref().0.get("1").cloned().ok_or(
                RuntimeError::UnexpectedTypeInConversion {
                    value: Box::new(RuntimeValue::Null),
                    target_type: ParserInnerType::Str,
                },
            )?;

            Ok((RuntimeValue::from(left), RuntimeValue::from(right)))
        }
        other => Err(RuntimeError::UnexpectedTypeInConversion {
            value: Box::new(other),
            target_type: ParserInnerType::Str,
        }),
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
            .unwrap_or(RuntimeValue::List(Arc::new(GcVec::new(Vec::new()))));

        #[allow(clippy::mutable_key_type)]
        let mut map: FxHashMap<HashKey, crate::value::ValueSlot> = FxHashMap::default();

        let RuntimeValue::List(list) = env.resolve_value(entries)? else {
            return Err(RuntimeError::UnexpectedTypeInConversion {
                value: Box::new(RuntimeValue::Null),
                target_type: ParserInnerType::Str,
            });
        };

        for item in list.as_ref().0.iter() {
            let (key, value) = tuple_pair(RuntimeValue::from(item.clone()))?;

            let key = resolve_hash_key(env, key)?;
            map.insert(key, value.into());
        }

        Ok(RuntimeValue::HashMap(RuntimeHashMap { map: Arc::new(map) }))
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
        let key = resolve_hash_key(env, pop_or_null(&mut args))?;
        let target = pop_or_null(&mut args);
        let mut map = resolve_hashmap(env, target.clone())?;

        Arc::make_mut(&mut map.map).insert(key, value.into());
        env.update_ref_value(target, RuntimeValue::HashMap(map));

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

        let key = resolve_hash_key(env, pop_or_null(&mut args))?;
        let map = resolve_hashmap(env, pop_or_null(&mut args))?;

        if let Some(value) = map.map.get(&key) {
            return Ok(RuntimeValue::Option(Some(Gc::new(RuntimeValue::from(
                value.clone(),
            )))));
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

        let key = resolve_hash_key(env, pop_or_null(&mut args))?;
        let target = pop_or_null(&mut args);
        let mut map = resolve_hashmap(env, target.clone())?;
        let removed = Arc::make_mut(&mut map.map).remove(&key);
        env.update_ref_value(target, RuntimeValue::HashMap(map));

        if let Some(value) = removed {
            return Ok(RuntimeValue::Option(Some(Gc::new(RuntimeValue::from(
                value.clone(),
            )))));
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

        let key = resolve_hash_key(env, pop_or_null(&mut args))?;
        let map = resolve_hashmap(env, pop_or_null(&mut args))?;

        Ok(RuntimeValue::Bool(map.map.contains_key(&key)))
    }
}

pub struct HashMapLen;

impl NativeFunction for HashMapLen {
    fn name(&self) -> String {
        String::from("collections.hashmap_len")
    }

    fn run(&self, env: &mut VM, mut args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        expect_num_args(&args, &[1])?;

        let map = resolve_hashmap(env, pop_or_null(&mut args))?;

        let len = map.map.len() as i64;
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

        let map = resolve_hashmap(env, pop_or_null(&mut args))?;

        let out: Vec<_> = map
            .map
            .keys()
            .map(|key| RuntimeValue::from(key.clone()))
            .collect();

        Ok(RuntimeValue::List(Arc::new(GcVec::new(out))))
    }
}

pub struct HashMapValues;

impl NativeFunction for HashMapValues {
    fn name(&self) -> String {
        String::from("collections.hashmap_values")
    }

    fn run(&self, env: &mut VM, mut args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        expect_num_args(&args, &[1])?;

        let map = resolve_hashmap(env, pop_or_null(&mut args))?;

        let out: Vec<_> = map
            .map
            .values()
            .map(|value| RuntimeValue::from(value.clone()))
            .collect();

        Ok(RuntimeValue::List(Arc::new(GcVec::new(out))))
    }
}

pub struct HashMapEntries;

impl NativeFunction for HashMapEntries {
    fn name(&self) -> String {
        String::from("collections.hashmap_entries")
    }

    fn run(&self, env: &mut VM, mut args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        expect_num_args(&args, &[1])?;

        let map = resolve_hashmap(env, pop_or_null(&mut args))?;

        let out: Vec<_> = map
            .map
            .iter()
            .map(|(key, value)| (key.clone(), RuntimeValue::from(value.clone())))
            .map(|(key, value)| {
                RuntimeValue::Aggregate(
                    None,
                    Arc::new(crate::value::GcMap::new(
                        vec![
                            ("0".to_string(), RuntimeValue::from(key)),
                            ("1".to_string(), value),
                        ]
                        .into(),
                    )),
                )
            })
            .collect();

        Ok(RuntimeValue::List(Arc::new(GcVec::new(out))))
    }
}

pub struct HashMapClear;

impl NativeFunction for HashMapClear {
    fn name(&self) -> String {
        String::from("collections.hashmap_clear")
    }

    fn run(&self, env: &mut VM, mut args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        expect_num_args(&args, &[1])?;

        let target = pop_or_null(&mut args);
        let mut map = resolve_hashmap(env, target.clone())?;

        Arc::make_mut(&mut map.map).clear();
        env.update_ref_value(target, RuntimeValue::HashMap(map));

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
            .unwrap_or(RuntimeValue::List(Arc::new(GcVec::new(Vec::new()))));

        let RuntimeValue::List(list) = env.resolve_value(entries)? else {
            return Err(RuntimeError::UnexpectedTypeInConversion {
                value: Box::new(RuntimeValue::Null),
                target_type: ParserInnerType::Str,
            });
        };

        #[allow(clippy::mutable_key_type)]
        let set: FxHashSet<HashKey> = list
            .as_ref()
            .0
            .iter()
            .map(|item| resolve_hash_key_ref(env, item))
            .collect::<Result<FxHashSet<_>, RuntimeError>>()?;

        Ok(RuntimeValue::HashSet(RuntimeHashSet { set: Arc::new(set) }))
    }
}

pub struct HashSetAdd;

impl NativeFunction for HashSetAdd {
    fn name(&self) -> String {
        String::from("collections.hashset_add")
    }

    fn run(&self, env: &mut VM, mut args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        expect_num_args(&args, &[2])?;

        let key = resolve_hash_key(env, pop_or_null(&mut args))?;
        let target = pop_or_null(&mut args);
        let mut set = resolve_hashset(env, target.clone())?;

        let inserted = Arc::make_mut(&mut set.set).insert(key);

        env.update_ref_value(target, RuntimeValue::HashSet(set));
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

        let key = resolve_hash_key(env, pop_or_null(&mut args))?;
        let target = pop_or_null(&mut args);
        let mut set = resolve_hashset(env, target.clone())?;

        let removed = Arc::make_mut(&mut set.set).remove(&key);

        env.update_ref_value(target, RuntimeValue::HashSet(set));
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

        let key = resolve_hash_key(env, pop_or_null(&mut args))?;
        let set = resolve_hashset(env, pop_or_null(&mut args))?;

        let contains = set.set.contains(&key);

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

        let set = resolve_hashset(env, pop_or_null(&mut args))?;

        let len = set.set.len() as i64;
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

        let set = resolve_hashset(env, pop_or_null(&mut args))?;

        let out: Vec<_> = set.set.iter().cloned().map(RuntimeValue::from).collect();

        Ok(RuntimeValue::List(Arc::new(GcVec::new(out))))
    }
}

pub struct HashSetClear;

impl NativeFunction for HashSetClear {
    fn name(&self) -> String {
        String::from("collections.hashset_clear")
    }

    fn run(&self, env: &mut VM, mut args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        expect_num_args(&args, &[1])?;

        let target = pop_or_null(&mut args);
        let mut set = resolve_hashset(env, target.clone())?;

        Arc::make_mut(&mut set.set).clear();
        env.update_ref_value(target, RuntimeValue::HashSet(set));

        Ok(RuntimeValue::Null)
    }
}
