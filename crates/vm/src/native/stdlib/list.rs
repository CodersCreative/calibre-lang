use std::cmp::Ordering;

use dumpster::sync::Gc;

use crate::{VM, error::RuntimeError, native::NativeFunction, value::RuntimeValue};

fn call_callable(
    env: &mut VM,
    callable: &RuntimeValue,
    args: Vec<RuntimeValue>,
) -> Result<RuntimeValue, RuntimeError> {
    match callable {
        RuntimeValue::Function { name, captures } => {
            let Some(func_def) = env.resolve_function_by_name(name.as_str()) else {
                return Err(RuntimeError::FunctionNotFound(name.as_str().to_string()));
            };
            env.run_function(func_def.as_ref(), args, captures.clone())
        }
        RuntimeValue::NativeFunction(func) => func.run(env, args),
        RuntimeValue::ExternFunction(func) => func.call(env, args),
        _ => Err(RuntimeError::InvalidFunctionCall),
    }
}

fn compare_callback_result(result: RuntimeValue) -> Result<Ordering, RuntimeError> {
    match result {
        RuntimeValue::Int(v) => Ok(v.cmp(&0)),
        RuntimeValue::UInt(v) => Ok((v as i128).cmp(&0)),
        RuntimeValue::Float(v) => Ok(v.partial_cmp(&0.0).unwrap_or(Ordering::Equal)),
        other => Err(RuntimeError::UnexpectedType(other)),
    }
}

fn is_callable(value: &RuntimeValue) -> bool {
    matches!(
        value,
        RuntimeValue::Function { .. }
            | RuntimeValue::NativeFunction(_)
            | RuntimeValue::ExternFunction(_)
    )
}

fn parse_sort_args(
    env: &mut VM,
    args: Vec<RuntimeValue>,
) -> Result<(Vec<RuntimeValue>, RuntimeValue), RuntimeError> {
    let mut list_value = None;
    let mut callable = None;

    for arg in args {
        let resolved = env.resolve_value_for_op_ref(&arg)?;
        if list_value.is_none()
            && let RuntimeValue::List(values) = resolved
        {
            list_value = Some(values.as_ref().0.clone());
            continue;
        }
        if callable.is_none() && is_callable(&resolved) {
            callable = Some(resolved);
            continue;
        }
    }

    let Some(list) = list_value else {
        return Err(RuntimeError::InvalidFunctionCall);
    };
    let Some(callable) = callable else {
        return Err(RuntimeError::InvalidFunctionCall);
    };
    Ok((list, callable))
}

fn parse_binary_search_args(
    env: &mut VM,
    args: Vec<RuntimeValue>,
) -> Result<(Vec<RuntimeValue>, RuntimeValue, RuntimeValue), RuntimeError> {
    let mut list_value = None;
    let mut callable = None;
    let mut needle = None;

    for arg in args {
        let resolved = env.resolve_value_for_op_ref(&arg)?;
        if let RuntimeValue::List(values) = &resolved {
            if list_value.is_none() {
                list_value = Some(values.as_ref().0.clone());
            }
            continue;
        }
        if callable.is_none() && is_callable(&resolved) {
            callable = Some(resolved);
            continue;
        }
        if needle.is_none() {
            needle = Some(resolved);
        }
    }

    let Some(list) = list_value else {
        return Err(RuntimeError::InvalidFunctionCall);
    };
    let Some(needle) = needle else {
        return Err(RuntimeError::InvalidFunctionCall);
    };
    let Some(callable) = callable else {
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
            match call_callable(env, &comparator, vec![a.clone(), b.clone()])
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
            let ordering = call_callable(env, &comparator, vec![probe, needle.clone()])
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
