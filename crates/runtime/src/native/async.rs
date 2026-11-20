use crate::{
    scheduler::{self, BlockResult},
    values::{Value, ValueTag, aggregate::GcAggregate, enums::calibre_option_none, host::GcHost},
};
use std::{
    cell::UnsafeCell,
    collections::VecDeque,
    fmt,
    hash::{Hash, Hasher},
    sync::atomic::{AtomicBool, AtomicIsize, Ordering},
    time::Duration,
};
use wasm_sync::{Condvar, Mutex};

pub struct ChannelState {
    pub queue: Mutex<VecDeque<Value>>,
    pub closed: AtomicBool,
    pub cvar: Condvar,
}

unsafe impl Send for ChannelState {}
unsafe impl Sync for ChannelState {}

impl fmt::Debug for ChannelState {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("ChannelState")
    }
}

impl Hash for ChannelState {
    fn hash<H: Hasher>(&self, state: &mut H) {
        (self as *const Self as usize).hash(state);
    }
}

impl ChannelState {
    pub fn from_ptr(ptr: *mut GcHost) -> Option<&'static Self> {
        GcHost::downcast_ref::<ChannelState>(ptr)
    }
}

pub struct MutexState {
    pub lock: Mutex<()>,
    pub value: UnsafeCell<Value>,
}

unsafe impl Send for MutexState {}
unsafe impl Sync for MutexState {}

impl fmt::Debug for MutexState {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("MutexState")
    }
}

impl Hash for MutexState {
    fn hash<H: Hasher>(&self, state: &mut H) {
        (self as *const Self as usize).hash(state);
    }
}

impl MutexState {
    pub fn get_clone(&self) -> Value {
        unsafe { (*self.value.get()).clone() }
    }

    pub fn set_value(&self, value: Value) -> Value {
        unsafe { std::ptr::replace(self.value.get(), value) }
    }

    pub fn get_ptr(&self) -> *mut Value {
        self.value.get()
    }

    pub fn from_ptr(ptr: *mut GcHost) -> Option<&'static Self> {
        GcHost::downcast_ref::<MutexState>(ptr)
    }
}

pub struct WaitGroupState {
    pub count: AtomicIsize,
    pub mutex: Mutex<()>,
    pub cvar: Condvar,
    pub joined: Mutex<Vec<*mut GcHost>>,
}

unsafe impl Send for WaitGroupState {}
unsafe impl Sync for WaitGroupState {}

impl fmt::Debug for WaitGroupState {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("WaitGroupState")
    }
}

impl Hash for WaitGroupState {
    fn hash<H: Hasher>(&self, state: &mut H) {
        (self as *const Self as usize).hash(state);
    }
}

impl WaitGroupState {
    pub fn done(&self) {
        let remaining = self.count.fetch_sub(1, Ordering::AcqRel) - 1;

        if remaining <= 0 {
            self.cvar.notify_all();
            scheduler::wake_all(self as *const Self as usize);
        }
    }

    pub fn join(&self, other: *mut GcHost) {
        if other.is_null() {
            return;
        }

        if let Ok(mut joined) = self.joined.lock() {
            joined.push(other);
        }
    }

    pub fn wait(&self) {
        let key = self as *const Self as usize;

        loop {
            if self.count.load(Ordering::Acquire) <= 0 {
                break;
            }

            match scheduler::block_on(key, None, &|| self.count.load(Ordering::Acquire) <= 0) {
                BlockResult::Woken | BlockResult::TimedOut => continue,
                BlockResult::Unavailable => {
                    if let Ok(mut guard) = self.mutex.lock() {
                        while self.count.load(Ordering::Acquire) > 0 {
                            match self.cvar.wait(guard) {
                                Ok(x) => guard = x,
                                Err(_) => return,
                            }
                        }
                    }
                }
            }
        }

        if let Ok(joined) = self.joined.lock() {
            for host in joined.iter().copied() {
                if let Some(state) = WaitGroupState::from_ptr(host) {
                    state.wait();
                }
            }
        }
    }

    pub fn from_ptr(ptr: *mut GcHost) -> Option<&'static Self> {
        GcHost::downcast_ref::<WaitGroupState>(ptr)
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn calibre_channel_new() -> *mut GcHost {
    GcHost::new(ChannelState {
        queue: Mutex::new(VecDeque::new()),
        closed: AtomicBool::new(false),
        cvar: Condvar::new(),
    })
    .alloc()
}

#[unsafe(no_mangle)]
pub extern "C" fn calibre_channel_send(host: *mut GcHost, tag: ValueTag, payload: i64) -> bool {
    let Some(ch) = ChannelState::from_ptr(host) else {
        return false;
    };

    if ch.closed.load(Ordering::Acquire) {
        return false;
    }

    let Ok(mut queue) = ch.queue.lock() else {
        return false;
    };

    queue.push_back(Value::from_tag(tag, payload));
    ch.cvar.notify_one();

    drop(queue);
    scheduler::wake_one(host as usize);

    true
}

#[unsafe(no_mangle)]
pub extern "C" fn calibre_channel_get(host: *mut GcHost, timeout_ms: i64) -> *mut GcAggregate {
    let Some(ch) = ChannelState::from_ptr(host) else {
        return calibre_option_none();
    };

    let key = host as usize;

    let timeout = if timeout_ms > 0 {
        Some(Duration::from_millis(timeout_ms.cast_unsigned()))
    } else {
        None
    };

    loop {
        if let Ok(mut queue) = ch.queue.try_lock()
            && let Some(value) = queue.pop_front()
        {
            return value.some();
        }

        if ch.closed.load(Ordering::Acquire) {
            return calibre_option_none();
        }

        if timeout_ms <= 0 {
            return calibre_option_none();
        }

        let ready = || {
            ch.queue.lock().map(|q| !q.is_empty()).unwrap_or(false)
                || ch.closed.load(Ordering::Acquire)
        };

        match scheduler::block_on(key, timeout, &ready) {
            BlockResult::Woken => continue,
            BlockResult::TimedOut => return calibre_option_none(),
            BlockResult::Unavailable => {
                let Ok(mut guard) = ch.queue.lock() else {
                    return calibre_option_none();
                };

                loop {
                    if let Some(value) = guard.pop_front() {
                        return value.some();
                    }

                    if ch.closed.load(Ordering::Acquire) {
                        return calibre_option_none();
                    }

                    match ch.cvar.wait_timeout(
                        guard,
                        Duration::from_millis(timeout_ms.max(0).cast_unsigned()),
                    ) {
                        Ok((g, result)) => {
                            if result.timed_out() {
                                return calibre_option_none();
                            }

                            guard = g;
                        }
                        Err(_) => return calibre_option_none(),
                    }
                }
            }
        }
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn calibre_channel_close(host: *mut GcHost) {
    if let Some(ch) = ChannelState::from_ptr(host) {
        ch.closed.store(true, Ordering::Release);
        ch.cvar.notify_all();

        scheduler::wake_all(host as usize);
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn calibre_channel_closed(host: *mut GcHost) -> bool {
    let Some(ch) = ChannelState::from_ptr(host) else {
        return false;
    };

    if !ch.closed.load(Ordering::Acquire) {
        return false;
    }

    ch.queue.lock().is_ok_and(|x| x.is_empty())
}

#[unsafe(no_mangle)]
pub extern "C" fn calibre_mutex_new(tag: ValueTag, payload: i64) -> *mut GcHost {
    GcHost::new(MutexState {
        lock: Mutex::new(()),
        value: UnsafeCell::new(Value::from_tag(tag, payload)),
    })
    .alloc()
}

#[unsafe(no_mangle)]
pub extern "C" fn calibre_mutex_get(host: *mut GcHost) -> i64 {
    let Some(m) = MutexState::from_ptr(host) else {
        return 0;
    };

    let _guard = m.lock.lock();
    i64::from(m.get_clone())
}

#[unsafe(no_mangle)]
pub extern "C" fn calibre_mutex_set(host: *mut GcHost, tag: ValueTag, payload: i64) -> i64 {
    let Some(m) = MutexState::from_ptr(host) else {
        return 0;
    };

    let _guard = m.lock.lock();
    let old = m.set_value(Value::from_tag(tag, payload));

    i64::from(old)
}

#[unsafe(no_mangle)]
pub extern "C" fn calibre_mutex_write(host: *mut GcHost) -> *mut Value {
    let Some(m) = MutexState::from_ptr(host) else {
        return std::ptr::null_mut();
    };

    m.get_ptr()
}

#[unsafe(no_mangle)]
pub extern "C" fn calibre_waitgroup_new() -> *mut GcHost {
    GcHost::new(WaitGroupState {
        count: AtomicIsize::new(0),
        mutex: Mutex::new(()),
        cvar: Condvar::new(),
        joined: Mutex::new(Vec::new()),
    })
    .alloc()
}

#[unsafe(no_mangle)]
pub extern "C" fn calibre_waitgroup_raw_add(host: *mut GcHost, n: i64) {
    if let Some(wg) = WaitGroupState::from_ptr(host) {
        wg.count.fetch_add(n as isize, Ordering::AcqRel);
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn calibre_waitgroup_raw_done(host: *mut GcHost) {
    if let Some(wg) = WaitGroupState::from_ptr(host) {
        wg.done();
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn calibre_waitgroup_join(host: *mut GcHost, other: *mut GcHost) {
    if let Some(wg) = WaitGroupState::from_ptr(host) {
        wg.join(other);
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn calibre_waitgroup_wait(host: *mut GcHost) {
    if let Some(wg) = WaitGroupState::from_ptr(host) {
        wg.wait();
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn calibre_waitgroup_count(host: *mut GcHost) -> i64 {
    WaitGroupState::from_ptr(host)
        .map(|wg| wg.count.load(Ordering::Acquire) as i64)
        .unwrap_or(0)
}

pub fn waitgroup_new_host() -> *mut GcHost {
    GcHost::new(WaitGroupState {
        count: AtomicIsize::new(1),
        mutex: Mutex::new(()),
        cvar: Condvar::new(),
        joined: Mutex::new(Vec::new()),
    })
    .alloc()
}

pub fn waitgroup_host_done(ptr: *mut GcHost) {
    if let Some(wg) = WaitGroupState::from_ptr(ptr) {
        wg.done();
    }
}
