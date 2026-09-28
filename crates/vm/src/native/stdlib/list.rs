use crate::{
    VM,
    error::RuntimeError,
    evaluate::calling::CallSite,
    native::NativeFunction,
    value::{GcVec, RuntimeValue},
};
use dumpster::sync::Gc;
use std::cmp::Ordering;
use std::sync::Arc;

fn compare_callback_result(env: &VM, result: RuntimeValue) -> Result<Ordering, RuntimeError> {
    match env.resolve_value(result)? {
        RuntimeValue::Int(v) => Ok(v.cmp(&0)),
        RuntimeValue::UInt(v) => Ok((v as i128).cmp(&0)),
        RuntimeValue::Float(v) => Ok(v.partial_cmp(&0.0).unwrap_or(Ordering::Equal)),
        other => Err(RuntimeError::ExpectedNumericFound {
            found: Box::new(other),
        }),
    }
}

#[inline]
fn parse_list_callable_needle_args(
    env: &mut VM,
    args: Vec<RuntimeValue>,
    need_needle: bool,
) -> Result<(Arc<GcVec>, RuntimeValue, Option<RuntimeValue>), RuntimeError> {
    let mut list_target = None;
    let mut callable = None;
    let mut needle = None;

    for arg in args {
        let resolved = env.resolve_value(arg)?;

        if list_target.is_none() && matches!(resolved, RuntimeValue::List(_)) {
            if let RuntimeValue::List(values) = resolved {
                list_target = Some(values);
            }
            continue;
        }

        if callable.is_none() && resolved.is_callable() {
            callable = Some(resolved);
            continue;
        }

        if need_needle && needle.is_none() {
            needle = Some(resolved);
            continue;
        }
    }

    let Some(list) = list_target else {
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
) -> Result<(Arc<GcVec>, RuntimeValue), RuntimeError> {
    let (list, callable, _) = parse_list_callable_needle_args(env, args, false)?;
    Ok((list, callable))
}

fn parse_binary_search_args(
    env: &mut VM,
    args: Vec<RuntimeValue>,
) -> Result<(Arc<GcVec>, RuntimeValue, RuntimeValue), RuntimeError> {
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
        let (list_arc, comparator) = parse_sort_args(env, args)?;

        let mut items: Vec<RuntimeValue> = match Arc::try_unwrap(list_arc) {
            Ok(gc_vec) => gc_vec.0.into_iter().map(RuntimeValue::from).collect(),
            Err(arc) => arc
                .0
                .iter()
                .map(|v| RuntimeValue::from(v.clone()))
                .collect(),
        };

        let mut compare_error = None;
        items.sort_by(|a, b| {
            if compare_error.is_some() {
                return Ordering::Equal;
            }
            match env
                .call_runtime_callable_at(
                    comparator.clone(),
                    vec![a.clone(), b.clone()],
                    CallSite {
                        block: usize::MAX,
                        tag: u32::MAX.saturating_sub(2),
                    },
                    true,
                )
                .and_then(|x| compare_callback_result(env, x))
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

        Ok(RuntimeValue::List(Arc::new(GcVec::new(items))))
    }
}

pub struct ListBinarySearchBy;

impl NativeFunction for ListBinarySearchBy {
    fn name(&self) -> String {
        String::from("list.binary_search_by")
    }

    fn run(&self, env: &mut VM, args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        let (list_arc, needle, comparator) = parse_binary_search_args(env, args)?;

        let slice = &list_arc.0;
        if slice.is_empty() {
            return Ok(RuntimeValue::Option(None));
        }

        let mut low = 0;
        let mut high = slice.len();

        while low < high {
            let mid = low + (high - low) / 2;
            let probe = RuntimeValue::from(slice[mid].clone());

            let ordering = env
                .call_runtime_callable_at(
                    comparator.clone(),
                    vec![probe, needle.clone()],
                    CallSite {
                        block: usize::MAX,
                        tag: u32::MAX.saturating_sub(3),
                    },
                    true,
                )
                .and_then(|x| compare_callback_result(env, x))?;

            match ordering {
                Ordering::Less => low = mid + 1,
                Ordering::Greater => high = mid,
                Ordering::Equal => {
                    return Ok(RuntimeValue::Option(Some(Gc::new(RuntimeValue::Int(
                        mid as i64,
                    )))));
                }
            }
        }

        Ok(RuntimeValue::Option(None))
    }
}

fn normalize_remove_index(len: usize, idx: i64) -> Option<usize> {
    if idx < 0 || idx as usize >= len {
        return None;
    }
    Some(idx as usize)
}

fn normalize_insert_index(len: usize, idx: i64) -> Option<usize> {
    (idx >= 0 && (idx as usize) <= len).then_some(idx as usize)
}

fn is_list_target(value: &RuntimeValue) -> bool {
    matches!(
        value,
        RuntimeValue::Ref(_)
            | RuntimeValue::VarRef(_)
            | RuntimeValue::RegRef { .. }
            | RuntimeValue::List(_)
    )
}

fn parse_list_index_args(
    env: &mut VM,
    args: Vec<RuntimeValue>,
) -> Result<(RuntimeValue, i64), RuntimeError> {
    let mut list_target = None;
    let mut index = None;

    for arg in args {
        if list_target.is_none() && is_list_target(&arg) {
            list_target = Some(arg);
            if index.is_some() {
                break;
            }
            continue;
        }

        if index.is_none() {
            index = match env.resolve_value(arg)? {
                RuntimeValue::Int(value) => Some(value),
                RuntimeValue::UInt(value) => Some(value as i64),
                _ => None,
            };
            if index.is_some() && list_target.is_some() {
                break;
            }
        }
    }

    match (list_target, index) {
        (Some(target), Some(index)) => Ok((target, index)),
        _ => Err(RuntimeError::InvalidFunctionCall),
    }
}

fn insert_into_list_value(list: &mut Arc<GcVec>, idx: i64, value: RuntimeValue) -> bool {
    let list = Arc::make_mut(list);
    let Some(index) = normalize_insert_index(list.len(), idx) else {
        return false;
    };
    list.insert(index, value.into());
    true
}

fn mutate_list_target<T, F>(
    env: &mut VM,
    target: RuntimeValue,
    mutation: F,
) -> Result<T, RuntimeError>
where
    F: FnOnce(&mut Arc<GcVec>) -> T,
{
    let mut current_target = target;

    loop {
        match current_target {
            RuntimeValue::Ref(name) => {
                let current = env.variables.get(&name).cloned().ok_or_else(|| {
                    RuntimeError::ExpectedListOrStrFound {
                        found: Box::new(RuntimeValue::Null),
                    }
                })?;
                match current {
                    RuntimeValue::List(_) => {
                        let mut list_val = env.variables.remove(&name).unwrap();
                        let RuntimeValue::List(ref mut list) = list_val else {
                            unreachable!()
                        };
                        let result = mutation(list);
                        env.variables.insert(name, list_val);
                        return Ok(result);
                    }
                    alias @ (RuntimeValue::Ref(_)
                    | RuntimeValue::VarRef(_)
                    | RuntimeValue::RegRef { .. }) => {
                        current_target = alias;
                    }
                    other => {
                        return Err(RuntimeError::ExpectedListOrStrFound {
                            found: Box::new(other),
                        });
                    }
                }
            }
            RuntimeValue::VarRef(id) => {
                let current = env.variables.get_by_id(id).cloned().ok_or_else(|| {
                    RuntimeError::ExpectedListOrStrFound {
                        found: Box::new(RuntimeValue::Null),
                    }
                })?;
                match current {
                    RuntimeValue::List(_) => {
                        env.variables.set_by_id(id, RuntimeValue::Null);
                        let mut list_val = current;
                        let RuntimeValue::List(ref mut list) = list_val else {
                            unreachable!()
                        };
                        let result = mutation(list);
                        let _ = env.variables.set_by_id(id, list_val);
                        return Ok(result);
                    }
                    alias @ (RuntimeValue::Ref(_)
                    | RuntimeValue::VarRef(_)
                    | RuntimeValue::RegRef { .. }) => {
                        current_target = alias;
                    }
                    other => {
                        return Err(RuntimeError::ExpectedListOrStrFound {
                            found: Box::new(other),
                        });
                    }
                }
            }
            RuntimeValue::RegRef { frame, reg } => {
                let current = env.get_reg_value_in_frame(frame, reg).clone();
                match current {
                    RuntimeValue::List(_) => {
                        env.set_reg_value_in_frame(frame, reg, RuntimeValue::Null);
                        let mut list_val = current;
                        let RuntimeValue::List(ref mut list) = list_val else {
                            unreachable!()
                        };
                        let result = mutation(list);
                        env.set_reg_value_in_frame(frame, reg, list_val);

                        if let Some(handle) = env.get_mutation_handle(reg) {
                            let updated = env.get_reg_value_in_frame(frame, reg).clone();
                            let _ = env.replace_mutation_handle(&handle, updated);
                        }
                        return Ok(result);
                    }
                    alias @ (RuntimeValue::Ref(_)
                    | RuntimeValue::VarRef(_)
                    | RuntimeValue::RegRef { .. }) => {
                        current_target = alias;
                    }
                    other => {
                        return Err(RuntimeError::ExpectedListOrStrFound {
                            found: Box::new(other),
                        });
                    }
                }
            }
            other => {
                let resolved = env.resolve_value_ref(&other)?;
                let RuntimeValue::List(mut list) = resolved else {
                    return Err(RuntimeError::ExpectedListOrStrFound {
                        found: Box::new(other),
                    });
                };
                return Ok(mutation(&mut list));
            }
        }
    }
}

fn remove_from_target(
    env: &mut VM,
    target: RuntimeValue,
    idx: i64,
) -> Result<Option<RuntimeValue>, RuntimeError> {
    mutate_list_target(env, target, |list| {
        let vec = Arc::make_mut(list);
        let idx = normalize_remove_index(vec.len(), idx)?;
        Some(RuntimeValue::from(vec.remove(idx)))
    })
}

pub struct ListRawRemove;

impl NativeFunction for ListRawRemove {
    fn name(&self) -> String {
        String::from("list.raw_remove")
    }

    fn run(&self, env: &mut VM, args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        let (target, idx) = parse_list_index_args(env, args)?;
        let removed = remove_from_target(env, target, idx)?;
        Ok(RuntimeValue::Option(removed.map(Gc::new)))
    }
}

pub struct ListRawInsert;

impl NativeFunction for ListRawInsert {
    fn name(&self) -> String {
        String::from("list.raw_insert")
    }

    fn run(&self, env: &mut VM, args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        let mut target = None;
        let mut index = None;
        let mut value = None;

        for arg in args {
            if target.is_none() && is_list_target(&arg) {
                target = Some(arg);
                continue;
            }

            let resolved = env.resolve_value(arg)?;
            if index.is_none() {
                match resolved {
                    RuntimeValue::Int(v) => index = Some(v),
                    RuntimeValue::UInt(v) => index = Some(v as i64),
                    other => {
                        value = Some(other);
                    }
                };
            } else if value.is_none() {
                value = Some(resolved);
            } else {
                return Err(RuntimeError::InvalidFunctionCall);
            }
        }

        let target = target.ok_or(RuntimeError::InvalidFunctionCall)?;
        let index = index.ok_or(RuntimeError::InvalidFunctionCall)?;
        let value = value.ok_or(RuntimeError::InvalidFunctionCall)?;

        Ok(RuntimeValue::Bool(mutate_list_target(
            env,
            target,
            |list| insert_into_list_value(list, index, value),
        )?))
    }
}
