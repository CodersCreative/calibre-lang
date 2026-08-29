use crate::{
    VM,
    error::RuntimeError,
    native::{
        NativeFunction,
        utils::{
            expect_num_args, pop_or_null, resolve_channel, resolve_int, resolve_mutex,
            resolve_waitgroup,
        },
    },
    value::{ChannelInner, MutexInner, RuntimeValue, WaitGroupInner},
};
use std::sync::Arc;

pub struct ChannelNew;

impl NativeFunction for ChannelNew {
    fn name(&self) -> String {
        String::from("async.channel_new")
    }

    fn run(&self, _env: &mut VM, _args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        Ok(RuntimeValue::Channel(Arc::new(ChannelInner::default())))
    }
}

pub struct ChannelSend;

impl NativeFunction for ChannelSend {
    fn name(&self) -> String {
        String::from("async.channel_send")
    }

    fn run(&self, env: &mut VM, mut args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        expect_num_args(&args, &[2])?;

        let (ch, value) = {
            let first = env.resolve_value_for_op_ref(&pop_or_null(&mut args))?;
            let second = env.resolve_value_for_op_ref(&pop_or_null(&mut args))?;

            match (first, second) {
                (RuntimeValue::Channel(ch), value) => (ch, value),
                (value, RuntimeValue::Channel(ch)) => (ch, value),
                (left, _) => return Err(RuntimeError::UnexpectedType(Box::new(left))),
            }
        };

        let value = env.convert_runtime_var_into_saveable(value);

        if ch.closed.load(std::sync::atomic::Ordering::Acquire) {
            return Ok(RuntimeValue::Null);
        }

        if let Ok(mut queue) = ch.queue.try_lock() {
            queue.push_back(value);
            ch.cvar.notify_one();
        }

        Ok(RuntimeValue::Null)
    }
}

pub struct ChannelTrySend;

impl NativeFunction for ChannelTrySend {
    fn name(&self) -> String {
        String::from("async.channel_try_send")
    }

    fn run(&self, env: &mut VM, mut args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        expect_num_args(&args, &[2])?;

        let (ch, value) = {
            let first = env.resolve_value_for_op_ref(&pop_or_null(&mut args))?;
            let second = env.resolve_value_for_op_ref(&pop_or_null(&mut args))?;

            match (first, second) {
                (RuntimeValue::Channel(ch), value) => (ch, value),
                (value, RuntimeValue::Channel(ch)) => (ch, value),
                (left, _) => return Err(RuntimeError::UnexpectedType(Box::new(left))),
            }
        };

        let value = env.convert_runtime_var_into_saveable(value);

        if ch.closed.load(std::sync::atomic::Ordering::Acquire) {
            return Ok(RuntimeValue::Bool(false));
        }

        if let Ok(mut queue) = ch.queue.try_lock() {
            queue.push_back(value);
            ch.cvar.notify_one();
        }

        Ok(RuntimeValue::Bool(true))
    }
}

pub struct ChannelGet;

impl NativeFunction for ChannelGet {
    fn name(&self) -> String {
        String::from("async.channel_get")
    }

    fn run(&self, env: &mut VM, mut args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        expect_num_args(&args, &[1])?;

        let ch = resolve_channel(env, &pop_or_null(&mut args))?;

        let mut guard = ch.queue.lock_sync();

        loop {
            if let Some(value) = guard.pop_front() {
                return Ok(RuntimeValue::Option(Some(dumpster::sync::Gc::new(value))));
            }

            if ch.closed.load(std::sync::atomic::Ordering::Acquire) {
                return Ok(RuntimeValue::Option(None));
            }

            guard = ch.cvar.wait_sync(guard);
        }
    }
}

pub struct ChannelTryGet;

impl NativeFunction for ChannelTryGet {
    fn name(&self) -> String {
        String::from("async.channel_try_get")
    }

    fn run(&self, env: &mut VM, mut args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        expect_num_args(&args, &[1])?;

        let ch = resolve_channel(env, &pop_or_null(&mut args))?;

        let mut guard = ch.queue.lock_sync();

        if let Some(value) = guard.pop_front() {
            return Ok(RuntimeValue::Option(Some(dumpster::sync::Gc::new(value))));
        }

        Ok(RuntimeValue::Option(None))
    }
}

pub struct ChannelClose;

impl NativeFunction for ChannelClose {
    fn name(&self) -> String {
        String::from("async.channel_close")
    }

    fn run(&self, env: &mut VM, mut args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        expect_num_args(&args, &[1])?;

        let ch = resolve_channel(env, &pop_or_null(&mut args))?;

        ch.closed.store(true, std::sync::atomic::Ordering::Release);
        ch.cvar.notify_all();
        Ok(RuntimeValue::Null)
    }
}

pub struct ChannelClosed;

impl NativeFunction for ChannelClosed {
    fn name(&self) -> String {
        String::from("async.channel_closed")
    }

    fn run(&self, env: &mut VM, mut args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        expect_num_args(&args, &[1])?;

        let ch = resolve_channel(env, &pop_or_null(&mut args))?;

        if !ch.closed.load(std::sync::atomic::Ordering::Acquire) {
            return Ok(RuntimeValue::Bool(false));
        }

        let empty = ch.queue.lock_sync().is_empty();

        Ok(RuntimeValue::Bool(empty))
    }
}

pub struct WaitGroupNew;

impl NativeFunction for WaitGroupNew {
    fn name(&self) -> String {
        String::from("async.waitgroup_new")
    }

    fn run(&self, _env: &mut VM, _args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        Ok(RuntimeValue::WaitGroup(Arc::new(WaitGroupInner::default())))
    }
}

pub struct WaitGroupRawAdd;

impl NativeFunction for WaitGroupRawAdd {
    fn name(&self) -> String {
        String::from("async.waitgroup_raw_add")
    }

    fn run(&self, env: &mut VM, mut args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        expect_num_args(&args, &[2])?;

        let value = resolve_int(env, &pop_or_null(&mut args))?;
        let wg = resolve_waitgroup(env, &pop_or_null(&mut args))?;

        wg.count
            .fetch_add(value as isize, std::sync::atomic::Ordering::AcqRel);

        Ok(RuntimeValue::Null)
    }
}

pub struct WaitGroupRawDone;

impl NativeFunction for WaitGroupRawDone {
    fn name(&self) -> String {
        String::from("async.waitgroup_raw_done")
    }

    fn run(&self, env: &mut VM, mut args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        expect_num_args(&args, &[1])?;

        let wg = resolve_waitgroup(env, &pop_or_null(&mut args))?;

        wg.done();

        Ok(RuntimeValue::Null)
    }
}

pub struct WaitGroupJoin;

impl NativeFunction for WaitGroupJoin {
    fn name(&self) -> String {
        String::from("async.waitgroup_join")
    }

    fn run(&self, env: &mut VM, mut args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        expect_num_args(&args, &[2])?;

        let wg = resolve_waitgroup(env, &pop_or_null(&mut args))?;
        let func = pop_or_null(&mut args);

        wg.count.fetch_add(1, std::sync::atomic::Ordering::AcqRel);
        env.spawn_async_task(func, Some(wg.clone()));

        Ok(RuntimeValue::WaitGroup(wg))
    }
}

pub struct WaitGroupWait;

impl NativeFunction for WaitGroupWait {
    fn name(&self) -> String {
        String::from("async.waitgroup_wait")
    }

    fn run(&self, env: &mut VM, mut args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        expect_num_args(&args, &[1])?;

        let wg = resolve_waitgroup(env, &pop_or_null(&mut args))?;

        wg.wait()?;
        Ok(RuntimeValue::Null)
    }
}

pub struct WaitGroupCount;

impl NativeFunction for WaitGroupCount {
    fn name(&self) -> String {
        String::from("async.waitgroup_count")
    }

    fn run(&self, env: &mut VM, mut args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        expect_num_args(&args, &[1])?;

        let wg = resolve_waitgroup(env, &pop_or_null(&mut args))?;

        let count = wg.count.load(std::sync::atomic::Ordering::Acquire);
        Ok(RuntimeValue::Int(count as i64))
    }
}

pub struct MutexNew;

impl NativeFunction for MutexNew {
    fn name(&self) -> String {
        String::from("async.mutex_new")
    }

    fn run(
        &self,
        _env: &mut VM,
        mut args: Vec<RuntimeValue>,
    ) -> Result<RuntimeValue, RuntimeError> {
        expect_num_args(&args, &[1])?;

        let value = pop_or_null(&mut args);

        Ok(RuntimeValue::Mutex(Arc::new(MutexInner::new(value))))
    }
}

pub struct MutexGet;

impl NativeFunction for MutexGet {
    fn name(&self) -> String {
        String::from("async.mutex_get")
    }

    fn run(&self, env: &mut VM, mut args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        expect_num_args(&args, &[1])?;

        let m = resolve_mutex(env, &pop_or_null(&mut args))?;
        let guard = m.lock();

        Ok(guard.get_clone())
    }
}

pub struct MutexSet;

impl NativeFunction for MutexSet {
    fn name(&self) -> String {
        String::from("async.mutex_set")
    }

    fn run(&self, env: &mut VM, mut args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        expect_num_args(&args, &[2])?;

        let value = pop_or_null(&mut args);
        let m = resolve_mutex(env, &pop_or_null(&mut args))?;

        let guard = m.lock();
        guard.set_value(value);
        Ok(RuntimeValue::Null)
    }
}

pub struct MutexWith;

impl NativeFunction for MutexWith {
    fn name(&self) -> String {
        String::from("async.mutex_with")
    }

    fn run(&self, env: &mut VM, mut args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        expect_num_args(&args, &[2])?;

        let func = pop_or_null(&mut args);
        let m = resolve_mutex(env, &pop_or_null(&mut args))?;

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

pub struct MutexWrite;

impl NativeFunction for MutexWrite {
    fn name(&self) -> String {
        String::from("async.mutex_write")
    }

    fn run(&self, env: &mut VM, mut args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        expect_num_args(&args, &[1])?;

        let m = resolve_mutex(env, &pop_or_null(&mut args))?;

        let guard = m.lock();
        Ok(RuntimeValue::MutexGuard(Arc::new(guard)))
    }
}
