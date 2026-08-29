use crate::{
    VM,
    error::RuntimeError,
    value::{
        ChannelInner, HashKey, Host, MutexInner, RuntimeHashMap, RuntimeHashSet, RuntimeValue,
        WaitGroupInner,
    },
};
use std::sync::Arc;
use wasm_sync::Mutex;

pub fn panic_message_arg(value: &RuntimeValue) -> String {
    match value {
        RuntimeValue::Str(s) => s.lock().unwrap().clone(),
        other => format!("{other:?}"),
    }
}

#[inline]
pub fn expect_num_args(args: &[RuntimeValue], valid: &[usize]) -> Result<(), RuntimeError> {
    if valid.contains(&args.len()) {
        Ok(())
    } else {
        Err(RuntimeError::InvalidNativeFunctionCall(format!(
            "Native function called with incorrect arguments : expected {} arguments found {}",
            valid
                .iter()
                .map(|x| x.to_string())
                .collect::<Vec<_>>()
                .join(", "),
            args.len()
        )))
    }
}

#[inline]
pub fn pop_or_null(args: &mut Vec<RuntimeValue>) -> RuntimeValue {
    args.pop().unwrap_or(RuntimeValue::Null)
}

#[inline]
pub fn first_or_null(args: &mut Vec<RuntimeValue>) -> RuntimeValue {
    if args.is_empty() {
        return RuntimeValue::Null;
    }

    args.remove(0)
}

#[inline]
pub fn resolve_str(env: &VM, value: &RuntimeValue) -> Result<Arc<Mutex<String>>, RuntimeError> {
    let value = env.resolve_value_for_op_ref(value)?;
    if let RuntimeValue::Str(s) = value {
        Ok(s)
    } else {
        Err(RuntimeError::UnexpectedType(Box::new(value)))
    }
}

#[inline]
pub fn resolve_host(env: &VM, value: &RuntimeValue) -> Result<Host, RuntimeError> {
    let value = env.resolve_value_for_op_ref(value)?;
    if let RuntimeValue::Host(v) = value {
        Ok(v)
    } else {
        Err(RuntimeError::UnexpectedType(Box::new(value)))
    }
}

#[inline]
pub fn resolve_int(env: &VM, value: &RuntimeValue) -> Result<i64, RuntimeError> {
    Ok(match env.resolve_value_for_op_ref(value)? {
        RuntimeValue::Int(v) => v,
        RuntimeValue::Char(c) => c as i64,
        RuntimeValue::UInt(v) => v as i64,
        RuntimeValue::Byte(v) => v as i64,
        RuntimeValue::Float(v) => v as i64,
        v => return Err(RuntimeError::UnexpectedType(Box::new(v))),
    })
}

#[inline]
pub fn resolve_char(env: &VM, value: &RuntimeValue) -> Result<char, RuntimeError> {
    let value = env.resolve_value_for_op_ref(value)?;
    if let RuntimeValue::Char(v) = value {
        Ok(v)
    } else {
        Err(RuntimeError::UnexpectedType(Box::new(value)))
    }
}

// Async
#[inline]
pub fn resolve_channel(env: &VM, value: &RuntimeValue) -> Result<Arc<ChannelInner>, RuntimeError> {
    let value = env.resolve_value_for_op_ref(value)?;
    if let RuntimeValue::Channel(ch) = value {
        Ok(ch)
    } else {
        Err(RuntimeError::UnexpectedType(Box::new(value)))
    }
}

#[inline]
pub fn resolve_waitgroup(
    env: &VM,
    value: &RuntimeValue,
) -> Result<Arc<WaitGroupInner>, RuntimeError> {
    let value = env.resolve_value_for_op_ref(value)?;
    if let RuntimeValue::WaitGroup(wg) = value {
        Ok(wg)
    } else {
        Err(RuntimeError::UnexpectedType(Box::new(value)))
    }
}

#[inline]
pub fn resolve_mutex(env: &VM, value: &RuntimeValue) -> Result<Arc<MutexInner>, RuntimeError> {
    let value = env.resolve_value_for_op_ref(value)?;
    if let RuntimeValue::Mutex(mutex) = value {
        Ok(mutex)
    } else {
        Err(RuntimeError::UnexpectedType(Box::new(value)))
    }
}

// Collections

pub fn resolve_hash_key(env: &VM, value: &RuntimeValue) -> Result<HashKey, RuntimeError> {
    let resolved = env.resolve_value_for_op_ref(value)?;
    HashKey::try_from(resolved)
}

#[inline]
pub fn resolve_hashmap(env: &mut VM, value: &RuntimeValue) -> Result<RuntimeHashMap, RuntimeError> {
    let resolved = env.resolve_value_for_op_ref(value)?;
    if let RuntimeValue::HashMap(map) = resolved {
        Ok(map)
    } else {
        Err(RuntimeError::UnexpectedType(Box::new(resolved)))
    }
}

#[inline]
pub fn resolve_hashset(env: &mut VM, value: &RuntimeValue) -> Result<RuntimeHashSet, RuntimeError> {
    let resolved = env.resolve_value_for_op_ref(value)?;
    if let RuntimeValue::HashSet(set) = resolved {
        Ok(set)
    } else {
        Err(RuntimeError::UnexpectedType(Box::new(resolved)))
    }
}
