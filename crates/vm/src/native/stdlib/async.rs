use std::sync::Arc;

use crate::{
    VM,
    error::RuntimeError,
    native::NativeFunction,
    value::{ChannelInner, MutexInner, RuntimeValue, WaitGroupInner},
};

pub struct ChannelNew();

impl NativeFunction for ChannelNew {
    fn name(&self) -> String {
        String::from("async.channel_new")
    }

    fn run(&self, _env: &mut VM, _args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        Ok(RuntimeValue::Channel(Arc::new(ChannelInner::new())))
    }
}

pub struct ChannelSend();

impl NativeFunction for ChannelSend {
    fn name(&self) -> String {
        String::from("async.channel_send")
    }

    fn run(&self, env: &mut VM, mut args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        let value = args.pop().unwrap_or(RuntimeValue::Null);
        let value = env.convert_runtime_var_into_saveable(value);
        let channel = args.remove(0);
        let resolved = env.resolve_value_for_op(channel)?;
        let RuntimeValue::Channel(ch) = resolved else {
            return Err(RuntimeError::UnexpectedType(resolved));
        };

        if ch.closed.load(std::sync::atomic::Ordering::Acquire) {
            return Ok(RuntimeValue::Null);
        }

        if let Ok(mut queue) = ch.queue.lock() {
            queue.push_back(value);
            ch.cvar.notify_one();
        }

        Ok(RuntimeValue::Null)
    }
}

pub struct ChannelGet();

impl NativeFunction for ChannelGet {
    fn name(&self) -> String {
        String::from("async.channel_get")
    }

    fn run(&self, env: &mut VM, mut args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        let channel = args.remove(0);
        let resolved = env.resolve_value_for_op(channel)?;
        let RuntimeValue::Channel(ch) = resolved else {
            return Err(RuntimeError::UnexpectedType(resolved));
        };

        let mut guard = ch
            .queue
            .lock()
            .map_err(|_| RuntimeError::UnexpectedType(RuntimeValue::Null))?;

        loop {
            if let Some(value) = guard.pop_front() {
                return Ok(RuntimeValue::Option(Some(dumpster::sync::Gc::new(value))));
            }

            if ch.closed.load(std::sync::atomic::Ordering::Acquire) {
                return Ok(RuntimeValue::Option(None));
            }

            guard = ch
                .cvar
                .wait(guard)
                .map_err(|_| RuntimeError::UnexpectedType(RuntimeValue::Null))?;
        }
    }
}

pub struct ChannelTryGet();

impl NativeFunction for ChannelTryGet {
    fn name(&self) -> String {
        String::from("async.channel_try_get")
    }

    fn run(&self, env: &mut VM, mut args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        let channel = args.remove(0);
        let resolved = env.resolve_value_for_op(channel)?;
        let RuntimeValue::Channel(ch) = resolved else {
            return Err(RuntimeError::UnexpectedType(resolved));
        };

        let mut guard = ch
            .queue
            .lock()
            .map_err(|_| RuntimeError::UnexpectedType(RuntimeValue::Null))?;

        if let Some(value) = guard.pop_front() {
            return Ok(RuntimeValue::Option(Some(dumpster::sync::Gc::new(value))));
        }

        Ok(RuntimeValue::Option(None))
    }
}

pub struct ChannelTrySend();

impl NativeFunction for ChannelTrySend {
    fn name(&self) -> String {
        String::from("async.channel_try_send")
    }

    fn run(&self, env: &mut VM, mut args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        let value = args.pop().unwrap_or(RuntimeValue::Null);
        let value = env.convert_runtime_var_into_saveable(value);
        let channel = args.remove(0);
        let resolved = env.resolve_value_for_op(channel)?;
        let RuntimeValue::Channel(ch) = resolved else {
            return Err(RuntimeError::UnexpectedType(resolved));
        };

        if ch.closed.load(std::sync::atomic::Ordering::Acquire) {
            return Ok(RuntimeValue::Bool(false));
        }

        if let Ok(mut queue) = ch.queue.lock() {
            queue.push_back(value);
            ch.cvar.notify_one();
        }

        Ok(RuntimeValue::Bool(true))
    }
}

pub struct ChannelClose();

impl NativeFunction for ChannelClose {
    fn name(&self) -> String {
        String::from("async.channel_close")
    }

    fn run(&self, env: &mut VM, mut args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        let channel = args.remove(0);
        let resolved = env.resolve_value_for_op(channel)?;
        let RuntimeValue::Channel(ch) = resolved else {
            return Err(RuntimeError::UnexpectedType(resolved));
        };

        ch.closed.store(true, std::sync::atomic::Ordering::Release);
        ch.cvar.notify_all();
        Ok(RuntimeValue::Null)
    }
}

pub struct ChannelClosed();

impl NativeFunction for ChannelClosed {
    fn name(&self) -> String {
        String::from("async.channel_closed")
    }

    fn run(&self, env: &mut VM, mut args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        let channel = args.remove(0);
        let resolved = env.resolve_value_for_op(channel)?;
        let RuntimeValue::Channel(ch) = resolved else {
            return Err(RuntimeError::UnexpectedType(resolved));
        };

        if !ch.closed.load(std::sync::atomic::Ordering::Acquire) {
            return Ok(RuntimeValue::Bool(false));
        }

        let empty = ch.queue.lock().map(|q| q.is_empty()).unwrap_or(true);

        Ok(RuntimeValue::Bool(empty))
    }
}

pub struct WaitGroupNew();

impl NativeFunction for WaitGroupNew {
    fn name(&self) -> String {
        String::from("async.waitgroup_new")
    }

    fn run(&self, _env: &mut VM, _args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        Ok(RuntimeValue::WaitGroup(Arc::new(WaitGroupInner::new())))
    }
}

pub struct WaitGroupRawAdd();

impl NativeFunction for WaitGroupRawAdd {
    fn name(&self) -> String {
        String::from("async.waitgroup_raw_add")
    }

    fn run(&self, env: &mut VM, mut args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        let value = match args.pop() {
            Some(RuntimeValue::Int(v)) => v,
            Some(other) => return Err(RuntimeError::UnexpectedType(other)),
            None => 1,
        };

        let wg_val = args.remove(0);
        let resolved = env.resolve_value_for_op(wg_val)?;
        let RuntimeValue::WaitGroup(wg) = resolved else {
            return Err(RuntimeError::UnexpectedType(resolved));
        };

        wg.count
            .fetch_add(value as isize, std::sync::atomic::Ordering::AcqRel);
        Ok(RuntimeValue::Null)
    }
}

pub struct WaitGroupRawDone();

impl NativeFunction for WaitGroupRawDone {
    fn name(&self) -> String {
        String::from("async.waitgroup_raw_done")
    }

    fn run(&self, env: &mut VM, mut args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        let wg_val = args.remove(0);
        let resolved = env.resolve_value_for_op(wg_val)?;
        let RuntimeValue::WaitGroup(wg) = resolved else {
            return Err(RuntimeError::UnexpectedType(resolved));
        };

        wg.done();

        Ok(RuntimeValue::Null)
    }
}

pub struct WaitGroupJoin();

impl NativeFunction for WaitGroupJoin {
    fn name(&self) -> String {
        String::from("async.waitgroup_join")
    }

    fn run(&self, env: &mut VM, mut args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        let inner = args.pop().unwrap_or(RuntimeValue::Null);
        let outer = args.pop().unwrap_or(RuntimeValue::Null);

        let outer = env.resolve_value_for_op(outer)?;
        let inner = env.resolve_value_for_op(inner)?;

        let RuntimeValue::WaitGroup(outer) = outer else {
            return Err(RuntimeError::UnexpectedType(outer));
        };
        let RuntimeValue::WaitGroup(inner) = inner else {
            return Err(RuntimeError::UnexpectedType(inner));
        };

        let mut joined = outer
            .joined
            .lock()
            .map_err(|_| RuntimeError::UnexpectedType(RuntimeValue::Null))?;
        joined.push(inner);
        Ok(RuntimeValue::Null)
    }
}

pub struct WaitGroupWait();

impl NativeFunction for WaitGroupWait {
    fn name(&self) -> String {
        String::from("async.waitgroup_wait")
    }

    fn run(&self, env: &mut VM, mut args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        let wg_val = args.remove(0);
        let resolved = env.resolve_value_for_op(wg_val)?;
        let RuntimeValue::WaitGroup(wg) = resolved else {
            return Err(RuntimeError::UnexpectedType(resolved));
        };

        wg.wait()?;
        Ok(RuntimeValue::Null)
    }
}

pub struct WaitGroupCount();

impl NativeFunction for WaitGroupCount {
    fn name(&self) -> String {
        String::from("async.waitgroup_count")
    }

    fn run(&self, env: &mut VM, mut args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        let wg_val = args.remove(0);
        let resolved = env.resolve_value_for_op(wg_val)?;
        let RuntimeValue::WaitGroup(wg) = resolved else {
            return Err(RuntimeError::UnexpectedType(resolved));
        };

        let count = wg.count.load(std::sync::atomic::Ordering::Acquire);
        Ok(RuntimeValue::Int(count as i64))
    }
}

pub struct MutexNew();

impl NativeFunction for MutexNew {
    fn name(&self) -> String {
        String::from("async.mutex_new")
    }

    fn run(
        &self,
        _env: &mut VM,
        mut args: Vec<RuntimeValue>,
    ) -> Result<RuntimeValue, RuntimeError> {
        let value = args.pop().unwrap_or(RuntimeValue::Null);
        Ok(RuntimeValue::Mutex(Arc::new(MutexInner::new(value))))
    }
}

pub struct MutexGet();

impl NativeFunction for MutexGet {
    fn name(&self) -> String {
        String::from("async.mutex_get")
    }

    fn run(&self, env: &mut VM, mut args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        let mutex = args.remove(0);
        let resolved = env.resolve_value_for_op(mutex)?;
        let RuntimeValue::Mutex(m) = resolved else {
            return Err(RuntimeError::UnexpectedType(resolved));
        };
        let guard = m.lock();
        Ok(guard.get_clone())
    }
}

pub struct MutexSet();

impl NativeFunction for MutexSet {
    fn name(&self) -> String {
        String::from("async.mutex_set")
    }

    fn run(&self, env: &mut VM, mut args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        let value = args.pop().unwrap_or(RuntimeValue::Null);
        let mutex = args.remove(0);
        let resolved = env.resolve_value_for_op(mutex)?;
        let RuntimeValue::Mutex(m) = resolved else {
            return Err(RuntimeError::UnexpectedType(resolved));
        };
        let guard = m.lock();
        guard.set_value(value);
        Ok(RuntimeValue::Null)
    }
}

pub struct MutexWith();

impl NativeFunction for MutexWith {
    fn name(&self) -> String {
        String::from("async.mutex_with")
    }

    fn run(&self, env: &mut VM, mut args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        if args.len() != 2 {
            return Err(RuntimeError::InvalidFunctionCall);
        }
        let func = args.pop().unwrap_or(RuntimeValue::Null);
        let mutex = args.remove(0);
        let resolved = env.resolve_value_for_op(mutex)?;
        let RuntimeValue::Mutex(m) = resolved else {
            return Err(RuntimeError::UnexpectedType(resolved));
        };
        let guard = m.lock();
        let current = guard.get_clone();

        let result = match func {
            RuntimeValue::Function { name, captures } => {
                let func_opt = if let Some(x) = env.registry.functions.get(&name) {
                    Some(x.clone())
                } else if let Some((prefix, _)) = name.split_once("->") {
                    env.registry
                        .functions
                        .iter()
                        .find(|(k, _)| k.starts_with(prefix))
                        .map(|(_, v)| v.clone())
                } else if let Some((_, short)) = name.rsplit_once(':') {
                    let suffix = format!(":{}", short);
                    env.registry
                        .functions
                        .iter()
                        .find(|(k, _)| k.ends_with(&suffix))
                        .map(|(_, v)| v.clone())
                } else {
                    None
                };
                let Some(func_def) = func_opt else {
                    return Err(RuntimeError::FunctionNotFound(name));
                };
                env.run_function(func_def.as_ref(), vec![current], captures)?
            }
            RuntimeValue::NativeFunction(func) => func.run(env, vec![current])?,
            RuntimeValue::ExternFunction(func) => func.call(env, vec![current])?,
            _other => return Err(RuntimeError::InvalidFunctionCall),
        };

        guard.set_value(result.clone());
        Ok(result)
    }
}

pub struct MutexWrite();

impl NativeFunction for MutexWrite {
    fn name(&self) -> String {
        String::from("async.mutex_write")
    }

    fn run(&self, env: &mut VM, mut args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        let mutex = args.remove(0);
        let resolved = env.resolve_value_for_op(mutex)?;
        let RuntimeValue::Mutex(m) = resolved else {
            return Err(RuntimeError::UnexpectedType(resolved));
        };
        let guard = m.lock();
        Ok(RuntimeValue::MutexGuard(Arc::new(guard)))
    }
}
