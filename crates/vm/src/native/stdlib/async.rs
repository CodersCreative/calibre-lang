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

pub struct WaitGroupAdd();

impl NativeFunction for WaitGroupAdd {
    fn name(&self) -> String {
        String::from("async.waitgroup_add")
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

pub struct WaitGroupDone();

impl NativeFunction for WaitGroupDone {
    fn name(&self) -> String {
        String::from("async.waitgroup_done")
    }

    fn run(&self, env: &mut VM, mut args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        let wg_val = args.remove(0);
        let resolved = env.resolve_value_for_op(wg_val)?;
        let RuntimeValue::WaitGroup(wg) = resolved else {
            return Err(RuntimeError::UnexpectedType(resolved));
        };

        let remaining = wg.count.fetch_sub(1, std::sync::atomic::Ordering::AcqRel) - 1;

        if remaining <= 0 {
            wg.cvar.notify_all();
        }

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

        let mut guard = wg
            .mutex
            .lock()
            .map_err(|_| RuntimeError::UnexpectedType(RuntimeValue::Null))?;
        while wg.count.load(std::sync::atomic::Ordering::Acquire) > 0 {
            guard = wg
                .cvar
                .wait(guard)
                .map_err(|_| RuntimeError::UnexpectedType(RuntimeValue::Null))?;
        }
        drop(guard);
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
