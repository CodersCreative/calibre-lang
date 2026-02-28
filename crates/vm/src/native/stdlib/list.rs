use std::cmp::Ordering;

use dumpster::sync::Gc;

use crate::{VM, error::RuntimeError, native::NativeFunction, value::RuntimeValue};

fn compare_callback_result(result: RuntimeValue) -> Result<Ordering, RuntimeError> {
    match result {
        RuntimeValue::Int(v) => Ok(v.cmp(&0)),
        RuntimeValue::UInt(v) => Ok((v as i128).cmp(&0)),
        RuntimeValue::Float(v) => Ok(v.partial_cmp(&0.0).unwrap_or(Ordering::Equal)),
        other => Err(RuntimeError::UnexpectedType(other)),
    }
}

#[inline]
fn parse_list_callable_needle_args(
    env: &mut VM,
    args: Vec<RuntimeValue>,
    need_needle: bool,
) -> Result<(Vec<RuntimeValue>, RuntimeValue, Option<RuntimeValue>), RuntimeError> {
    let mut list_value = None;
    let mut callable = None;
    let mut needle = None;

    for arg in args {
        let resolved = env.resolve_value_for_op_ref(&arg)?;
        if list_value.is_none()
            && let RuntimeValue::List(values) = &resolved
        {
            list_value = Some(values.as_ref().0.clone());
            continue;
        }
        if callable.is_none() && VM::is_runtime_callable(&resolved) {
            callable = Some(resolved);
            continue;
        }
        if need_needle && needle.is_none() {
            needle = Some(resolved);
        }
    }

    let Some(list) = list_value else {
        return Err(RuntimeError::InvalidFunctionCall);
    };
    let Some(callable) = callable else {
        return Err(RuntimeError::InvalidFunctionCall);
    };
    if need_needle && needle.is_none() {
        return Err(RuntimeError::InvalidFunctionCall);
    }
    Ok((list, callable, needle))
}

fn parse_sort_args(
    env: &mut VM,
    args: Vec<RuntimeValue>,
) -> Result<(Vec<RuntimeValue>, RuntimeValue), RuntimeError> {
    let (list, callable, _) = parse_list_callable_needle_args(env, args, false)?;
    Ok((list, callable))
}

fn parse_binary_search_args(
    env: &mut VM,
    args: Vec<RuntimeValue>,
) -> Result<(Vec<RuntimeValue>, RuntimeValue, RuntimeValue), RuntimeError> {
    let (list, callable, needle) = parse_list_callable_needle_args(env, args, true)?;
    let Some(needle) = needle else {
        return Err(RuntimeError::InvalidFunctionCall);
    };
    Ok((list, needle, callable))
}

pub struct ListSortBy;

impl NativeFunction for ListSortBy {
    fn name(&self) -> String {
        String::from("list.sort_by")
    }

    fn run(&self, env: &mut VM, args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        let (mut items, comparator) = parse_sort_args(env, args)?;
        let mut compare_error = None;
        items.sort_by(|a, b| {
            if compare_error.is_some() {
                return Ordering::Equal;
            }
            match env
                .call_runtime_callable_at(
                    comparator.clone(),
                    vec![a.clone(), b.clone()],
                    usize::MAX,
                    u32::MAX.saturating_sub(2),
                )
                .and_then(compare_callback_result)
            {
                Ok(ordering) => ordering,
                Err(err) => {
                    compare_error = Some(err);
                    Ordering::Equal
                }
            }
        });
        if let Some(err) = compare_error {
            return Err(err);
        }

        Ok(RuntimeValue::List(Gc::new(crate::value::GcVec(items))))
    }
}

pub struct ListBinarySearchBy;

impl NativeFunction for ListBinarySearchBy {
    fn name(&self) -> String {
        String::from("list.binary_search_by")
    }

    fn run(&self, env: &mut VM, args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        let (items, needle, comparator) = parse_binary_search_args(env, args)?;
        if items.is_empty() {
            return Ok(RuntimeValue::Option(None));
        }

        let mut low = 0i64;
        let mut high = items.len() as i64 - 1;

        while low <= high {
            let mid = low + ((high - low) / 2);
            let probe = items[mid as usize].clone();
            let ordering = env
                .call_runtime_callable_at(
                    comparator.clone(),
                    vec![probe, needle.clone()],
                    usize::MAX,
                    u32::MAX.saturating_sub(3),
                )
                .and_then(compare_callback_result)?;
            match ordering {
                Ordering::Less => low = mid + 1,
                Ordering::Greater => high = mid - 1,
                Ordering::Equal => {
                    return Ok(RuntimeValue::Option(Some(Gc::new(RuntimeValue::Int(mid)))));
                }
            }
        }

        Ok(RuntimeValue::Option(None))
    }
}

pub struct ListRawRemove;

fn normalize_remove_index(len: usize, idx: i64) -> Option<usize> {
    if idx < 0 || idx as usize >= len {
        return None;
    }
    Some(idx as usize)
}

fn remove_from_list_value(list: &mut Gc<crate::value::GcVec>, idx: i64) -> Option<RuntimeValue> {
    let vec = &mut Gc::make_mut(list).0;
    let idx = normalize_remove_index(vec.len(), idx)?;
    Some(vec.remove(idx))
}

fn remove_from_target(
    env: &mut VM,
    target: RuntimeValue,
    idx: i64,
) -> Result<Option<RuntimeValue>, RuntimeError> {
    match target {
        RuntimeValue::Ref(name) => {
            let Some(RuntimeValue::List(mut list)) = env.variables.get(&name).cloned() else {
                return Err(RuntimeError::UnexpectedType(RuntimeValue::Null));
            };
            let removed = remove_from_list_value(&mut list, idx);
            env.variables.insert(&name, RuntimeValue::List(list));
            Ok(removed)
        }
        RuntimeValue::VarRef(id) => {
            let Some(RuntimeValue::List(mut list)) = env.variables.get_by_id(id).cloned() else {
                return Err(RuntimeError::UnexpectedType(RuntimeValue::Null));
            };
            let removed = remove_from_list_value(&mut list, idx);
            let _ = env.variables.set_by_id(id, RuntimeValue::List(list));
            Ok(removed)
        }
        RuntimeValue::RegRef { frame, reg } => {
            let RuntimeValue::List(mut list) = env.get_reg_value_in_frame(frame, reg).clone()
            else {
                return Err(RuntimeError::UnexpectedType(RuntimeValue::Null));
            };
            let removed = remove_from_list_value(&mut list, idx);
            env.set_reg_value_in_frame(frame, reg, RuntimeValue::List(list));
            Ok(removed)
        }
        other => {
            let resolved = env.resolve_value_for_op_ref(&other)?;
            let RuntimeValue::List(mut list) = resolved else {
                return Err(RuntimeError::UnexpectedType(other));
            };
            Ok(remove_from_list_value(&mut list, idx))
        }
    }
}

impl NativeFunction for ListRawRemove {
    fn name(&self) -> String {
        String::from("list.raw_remove")
    }

    fn run(&self, env: &mut VM, args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        let mut list_target = None;
        let mut idx = None;

        for arg in args {
            if list_target.is_none()
                && matches!(
                    arg,
                    RuntimeValue::Ref(_)
                        | RuntimeValue::VarRef(_)
                        | RuntimeValue::RegRef { .. }
                        | RuntimeValue::List(_)
                )
            {
                list_target = Some(arg);
                continue;
            }
            if idx.is_none() {
                match env.resolve_value_for_op_ref(&arg)? {
                    RuntimeValue::Int(v) => idx = Some(v),
                    RuntimeValue::UInt(v) => idx = Some(v as i64),
                    _ => {}
                }
            }
        }

        let Some(target) = list_target else {
            return Err(RuntimeError::InvalidFunctionCall);
        };
        let Some(idx) = idx else {
            return Err(RuntimeError::InvalidFunctionCall);
        };

        let removed = remove_from_target(env, target, idx)?;
        Ok(RuntimeValue::Option(removed.map(Gc::new)))
    }
}
