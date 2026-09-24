use crate::{error::RuntimeError, value::RuntimeValue};
use std::{
    cell::UnsafeCell,
    collections::VecDeque,
    sync::{
        Arc,
        atomic::{AtomicBool, AtomicIsize, Ordering},
    },
};
use wasm_sync::{Condvar, Mutex};

#[derive(Debug, Default)]
pub struct ChannelInner {
    pub queue: Mutex<VecDeque<RuntimeValue>>,
    pub closed: AtomicBool,
    pub cvar: Condvar,
}

#[derive(Debug, Default)]
pub struct WaitGroupInner {
    pub count: AtomicIsize,
    pub mutex: Mutex<()>,
    pub cvar: Condvar,
    pub joined: Mutex<Vec<Arc<WaitGroupInner>>>,
}

impl WaitGroupInner {
    pub fn done(&self) {
        let remaining = self.count.fetch_sub(1, Ordering::AcqRel) - 1;
        if remaining <= 0 {
            self.cvar.notify_all();
        }
    }

    pub fn wait(&self) -> Result<(), RuntimeError> {
        let mut guard = self.mutex.lock().unwrap();

        while self.count.load(Ordering::Acquire) > 0 {
            guard = self.cvar.wait(guard).unwrap();
        }

        drop(guard);

        let joined = self.joined.lock().unwrap();

        for inner in joined.iter() {
            inner.wait()?;
        }
        Ok(())
    }
}

#[derive(Debug)]
pub struct MutexInner {
    locked: AtomicBool,
    mutex: Mutex<()>,
    cvar: Condvar,
    value: UnsafeCell<RuntimeValue>,
}

unsafe impl Send for MutexInner {}
unsafe impl Sync for MutexInner {}

impl MutexInner {
    pub fn new(value: RuntimeValue) -> Self {
        Self {
            locked: AtomicBool::new(false),
            mutex: Mutex::new(()),
            cvar: Condvar::new(),
            value: UnsafeCell::new(value),
        }
    }

    pub fn lock(self: &Arc<Self>) -> MutexGuardInner {
        let mut guard = self.mutex.lock().unwrap();
        while self.locked.load(Ordering::Acquire) {
            guard = self.cvar.wait(guard).unwrap();
        }

        self.locked.store(true, Ordering::Release);
        drop(guard);

        MutexGuardInner {
            inner: self.clone(),
            released: AtomicBool::new(false),
        }
    }

    fn unlock(&self) {
        self.locked.store(false, Ordering::Release);
        self.cvar.notify_one();
    }

    fn get_clone(&self) -> RuntimeValue {
        unsafe { (*self.value.get()).clone() }
    }

    fn set_value(&self, value: RuntimeValue) -> RuntimeValue {
        unsafe {
            let ptr = self.value.get();
            std::ptr::replace(ptr, value)
        }
    }
}

#[derive(Debug)]
pub struct MutexGuardInner {
    inner: Arc<MutexInner>,
    released: AtomicBool,
}

impl MutexGuardInner {
    pub fn get_clone(&self) -> RuntimeValue {
        self.inner.get_clone()
    }

    pub fn set_value(&self, value: RuntimeValue) -> RuntimeValue {
        self.inner.set_value(value)
    }
}

impl Drop for MutexGuardInner {
    fn drop(&mut self) {
        if !self.released.swap(true, Ordering::AcqRel) {
            self.inner.unlock();
        }
    }
}
