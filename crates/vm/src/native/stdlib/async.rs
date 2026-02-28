use std::sync::Arc;

use crate::{
    VM,
    error::RuntimeError,
    native::NativeFunction,
    value::{ChannelInner, MutexInner, RuntimeValue, WaitGroupInner},
};

#[inline]
fn first_arg(args: &mut Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
    if args.is_empty() {
        Err(RuntimeError::InvalidFunctionCall)
    } else {
        Ok(args.remove(0))
    }
}

#[inline]
fn resolve_channel(
    env: &mut VM,
    args: &mut Vec<RuntimeValue>,
) -> Result<Arc<ChannelInner>, RuntimeError> {
    let raw = first_arg(args)?;
    let resolved = env.resolve_value_for_op_ref(&raw)?;
    if let RuntimeValue::Channel(ch) = resolved {
        Ok(ch)
    } else {
        Err(RuntimeError::UnexpectedType(resolved))
    }
}

#[inline]
fn resolve_waitgroup(
    env: &mut VM,
    args: &mut Vec<RuntimeValue>,
) -> Result<Arc<WaitGroupInner>, RuntimeError> {
    let raw = first_arg(args)?;
    let resolved = env.resolve_value_for_op_ref(&raw)?;
    if let RuntimeValue::WaitGroup(wg) = resolved {
        Ok(wg)
    } else {
        Err(RuntimeError::UnexpectedType(resolved))
    }
}

#[inline]
fn resolve_mutex(
    env: &mut VM,
    args: &mut Vec<RuntimeValue>,
) -> Result<Arc<MutexInner>, RuntimeError> {
    let raw = first_arg(args)?;
    let resolved = env.resolve_value_for_op_ref(&raw)?;
    if let RuntimeValue::Mutex(mutex) = resolved {
        Ok(mutex)
    } else {
        Err(RuntimeError::UnexpectedType(resolved))
    }
}

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
        let ch = resolve_channel(env, &mut args)?;

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
        let ch = resolve_channel(env, &mut args)?;

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
        let ch = resolve_channel(env, &mut args)?;

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
        let ch = resolve_channel(env, &mut args)?;

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
        let ch = resolve_channel(env, &mut args)?;

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
        let ch = resolve_channel(env, &mut args)?;

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

        let wg = resolve_waitgroup(env, &mut args)?;

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
        let wg = resolve_waitgroup(env, &mut args)?;

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

        let outer = env.resolve_value_for_op_ref(&outer)?;
        let inner = env.resolve_value_for_op_ref(&inner)?;

        let RuntimeValue::WaitGroup(outer) = outer else {
            return Err(RuntimeError::UnexpectedType(outer));
        };
        let RuntimeValue::WaitGroup(inner) = inner else {
            return Err(RuntimeError::UnexpectedType(inner));
        };

        if Arc::ptr_eq(&outer, &inner) {
            return Ok(RuntimeValue::Null);
        }

        let mut joined = outer
            .joined
            .lock()
            .map_err(|_| RuntimeError::UnexpectedType(RuntimeValue::Null))?;
        if joined.iter().all(|existing| !Arc::ptr_eq(existing, &inner)) {
            joined.push(inner);
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
        let wg = resolve_waitgroup(env, &mut args)?;

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
        let wg = resolve_waitgroup(env, &mut args)?;

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
        let m = resolve_mutex(env, &mut args)?;
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
        let m = resolve_mutex(env, &mut args)?;
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
        let m = resolve_mutex(env, &mut args)?;
        let guard = m.lock();
        let current = guard.get_clone();

        let result = env.call_runtime_callable_at(
            func,
            vec![current],
            usize::MAX,
            u32::MAX.saturating_sub(4),
        )?;

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
        let m = resolve_mutex(env, &mut args)?;
        let guard = m.lock();
        Ok(RuntimeValue::MutexGuard(Arc::new(guard)))
    }
}
