use rustc_hash::FxHashMap;
use std::{cmp::Ordering, fmt::Debug, sync::Arc};

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

#[inline]
pub(crate) fn first_arg(args: &[RuntimeValue]) -> Result<&RuntimeValue, RuntimeError> {
    args.first().ok_or(RuntimeError::InvalidFunctionCall)
}

#[inline]
pub(crate) fn pop_or_null(args: &mut Vec<RuntimeValue>) -> RuntimeValue {
    args.pop().unwrap_or(RuntimeValue::Null)
}

#[inline]
pub(crate) fn expect_str_ref(value: &RuntimeValue) -> Result<&Arc<String>, RuntimeError> {
    if let RuntimeValue::Str(s) = value {
        Ok(s)
    } else {
        Err(RuntimeError::UnexpectedType(value.clone()))
    }
}

#[inline]
pub(crate) fn expect_str_owned(value: RuntimeValue) -> Result<Arc<String>, RuntimeError> {
    if let RuntimeValue::Str(s) = value {
        Ok(s)
    } else {
        Err(RuntimeError::UnexpectedType(value))
    }
}

#[inline]
pub(crate) fn expect_int(value: RuntimeValue) -> Result<i64, RuntimeError> {
    if let RuntimeValue::Int(v) = value {
        Ok(v)
    } else {
        Err(RuntimeError::UnexpectedType(value))
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
    pub(crate) fn build_mapping_index(
        mappings: &[String],
        prefer_nonzero_scope: bool,
    ) -> FxHashMap<String, Option<String>> {
        fn scope_id(name: &str) -> Option<u64> {
            let mut parts = name.splitn(3, '-');
            let _prefix = parts.next()?;
            let scope = parts.next()?;
            scope.parse::<u64>().ok()
        }

        let mut buckets: FxHashMap<String, Vec<String>> =
            FxHashMap::with_capacity_and_hasher(mappings.len(), Default::default());
        for full in mappings {
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
            let chosen = if prefer_nonzero_scope {
                matches
                    .iter()
                    .find(|x| scope_id(x.as_str()).is_some_and(|id| id != 0))
                    .cloned()
            } else {
                matches
                    .iter()
                    .find(|x| scope_id(x.as_str()) == Some(0))
                    .cloned()
            };
            let chosen = chosen.or_else(|| {
                if matches.len() == 1 {
                    Some(matches[0].clone())
                } else {
                    None
                }
            });
            index.insert(short, chosen);
        }

        index
    }

    pub(crate) fn mapped_name(
        mapping_index: &FxHashMap<String, Option<String>>,
        key: &str,
    ) -> String {
        mapping_index
            .get(key)
            .and_then(|x| x.clone())
            .unwrap_or_else(|| key.to_string())
    }

    pub fn setup_global(&mut self) {
        let mapping_index = Self::build_mapping_index(self.mappings.as_ref(), false);

        for (short_name, value) in RuntimeValue::constants().into_iter().chain(
            RuntimeValue::natives()
                .into_iter()
                .filter(|(name, _)| !name.contains('.')),
        ) {
            let name = Self::mapped_name(&mapping_index, short_name.as_str());
            let _ = self.variables.insert(name, value);
        }
    }
}
