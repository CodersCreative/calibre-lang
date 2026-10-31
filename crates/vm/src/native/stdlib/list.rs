use crate::{
    VM,
    error::RuntimeError,
    native::{
        NativeFunction,
        utils::{expect_num_args, pop_or_null, resolve_int, resolve_list, update_list_mut},
    },
    value::{GcVec, RuntimeValue, ValueSlot},
};
use std::cmp::Ordering;

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

pub struct ListSortBy;

impl NativeFunction for ListSortBy {
    fn name(&self) -> String {
        String::from("list.sort_by")
    }

    fn run(&self, env: &mut VM, mut args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        expect_num_args(&args, &[2])?;

        let comparator = pop_or_null(&mut args);

        let list = resolve_list(env, pop_or_null(&mut args))?;
        let mut list: Vec<RuntimeValue> = list.0.into_iter().map(RuntimeValue::from).collect();

        let mut compare_error = None;
        list.sort_by(|a, b| {
            if compare_error.is_some() {
                return Ordering::Equal;
            }
            match env
                .call_runtime_callable(comparator.clone(), vec![a.clone(), b.clone()], true)
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

        Ok(RuntimeValue::List(GcVec::new(list)))
    }
}

pub struct ListBinarySearchBy;

impl NativeFunction for ListBinarySearchBy {
    fn name(&self) -> String {
        String::from("list.binary_search_by")
    }

    fn run(&self, env: &mut VM, mut args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        expect_num_args(&args, &[3])?;

        let comparator = pop_or_null(&mut args);
        let needle = pop_or_null(&mut args);

        let list = resolve_list(env, pop_or_null(&mut args))?;
        if list.is_empty() {
            return Ok(RuntimeValue::Option(None));
        }

        let mut low = 0;
        let mut high = list.len();

        while low < high {
            let mid = low + (high - low) / 2;
            let probe = RuntimeValue::from(list[mid].clone());

            let ordering = env
                .call_runtime_callable(comparator.clone(), vec![probe, needle.clone()], true)
                .and_then(|x| compare_callback_result(env, x))?;

            match ordering {
                Ordering::Less => low = mid + 1,
                Ordering::Greater => high = mid,
                Ordering::Equal => {
                    return Ok(RuntimeValue::Option(Some(ValueSlot::new(
                        RuntimeValue::Int(mid as isize),
                    ))));
                }
            }
        }

        Ok(RuntimeValue::Option(None))
    }
}

pub struct ListRawRemove;

impl NativeFunction for ListRawRemove {
    fn name(&self) -> String {
        String::from("list.raw_remove")
    }

    fn run(&self, env: &mut VM, mut args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        expect_num_args(&args, &[2])?;

        let index = resolve_int(env, pop_or_null(&mut args))?;

        update_list_mut(env, &mut pop_or_null(&mut args), |list| {
            if index >= 0 && (index as usize) < list.len() {
                let value = list.remove(index as usize);
                Ok(RuntimeValue::Option(Some(ValueSlot::new(
                    RuntimeValue::from(value),
                ))))
            } else {
                Ok(RuntimeValue::Option(None))
            }
        })
    }
}

pub struct ListRawInsert;

impl NativeFunction for ListRawInsert {
    fn name(&self) -> String {
        String::from("list.raw_insert")
    }

    fn run(&self, env: &mut VM, mut args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        expect_num_args(&args, &[3])?;

        let value = pop_or_null(&mut args);
        let index = resolve_int(env, pop_or_null(&mut args))?;

        update_list_mut(env, &mut pop_or_null(&mut args), |list| {
            if index >= 0 && (index as usize) < list.len() {
                list.insert(index as usize, value.into());
                Ok(RuntimeValue::Bool(false))
            } else {
                Ok(RuntimeValue::Bool(false))
            }
        })
    }
}
