use crate::native::r#async::{waitgroup_host_done, waitgroup_new_host};
use crate::values::aggregate::GcAggregate;
use crate::values::closure::calibre_fn_entry;
use crate::values::host::GcHost;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::{Condvar, Mutex};
use std::time::Duration;

static CONFIG_MAX_PER_THREAD: AtomicUsize = AtomicUsize::new(0);
static CONFIG_STACK_SIZE: AtomicUsize = AtomicUsize::new(0);

fn stack_size() -> usize {
    let configured = CONFIG_STACK_SIZE.load(Ordering::Acquire);

    if configured != 0 {
        return configured;
    }

    1024 * 1024
}

pub enum BlockResult {
    Woken,
    TimedOut,
    Unavailable,
}

struct TaskArgs {
    entry: usize,
    env: usize,
    args: usize,
    waitgroup: usize,
}

static PENDING: AtomicUsize = AtomicUsize::new(0);
static DRAIN_LOCK: Mutex<()> = Mutex::new(());
static DRAIN_CVAR: Condvar = Condvar::new();

fn note_task_spawned() {
    PENDING.fetch_add(1, Ordering::AcqRel);
}

fn note_task_finished() {
    PENDING.fetch_sub(1, Ordering::AcqRel);

    let _guard = DRAIN_LOCK.lock().unwrap_or_else(|e| e.into_inner());
    DRAIN_CVAR.notify_all();
}

fn run_task(task: TaskArgs) {
    let panicked = std::panic::catch_unwind(|| {
        let f: extern "C" fn(i64, i64) -> i64 =
            unsafe { std::mem::transmute(task.entry as *const ()) };

        f(task.env as i64, task.args as i64);
    })
    .is_err();

    if panicked {
        eprintln!("calibre: spawned task panicked");
    }

    waitgroup_host_done(task.waitgroup as *mut GcHost);
    note_task_finished();
}

#[cfg(not(target_arch = "wasm32"))]
mod imp {
    use super::{
        BlockResult, TaskArgs, note_task_finished, note_task_spawned, run_task, stack_size,
    };
    use may::coroutine::{self, Coroutine};
    use std::collections::HashMap;
    use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};
    use std::sync::{Arc, LazyLock, MutexGuard};
    use std::time::Duration;
    use wasm_sync::Mutex;

    struct Waiter {
        id: usize,
        co: Coroutine,
        woken: Arc<AtomicBool>,
    }

    static WAITERS: LazyLock<Mutex<HashMap<usize, Vec<Waiter>>>> =
        LazyLock::new(|| Mutex::new(HashMap::new()));

    static WAITER_COUNTER: AtomicUsize = AtomicUsize::new(1);

    fn lock_waiters() -> MutexGuard<'static, HashMap<usize, Vec<Waiter>>> {
        WAITERS.lock().unwrap_or_else(|e| e.into_inner())
    }

    fn remove_waiter(key: usize, id: usize) {
        let mut waiters = lock_waiters();

        if let Some(list) = waiters.get_mut(&key) {
            list.retain(|w| w.id != id);
            if list.is_empty() {
                waiters.remove(&key);
            }
        }
    }

    pub fn spawn_task(task: TaskArgs) -> bool {
        note_task_spawned();

        let spawned = unsafe {
            coroutine::Builder::new()
                .name("calibre-task".to_string())
                .stack_size(stack_size())
                .spawn(move || run_task(task))
        };

        if spawned.is_ok() {
            true
        } else {
            note_task_finished();
            false
        }
    }

    pub fn configure(max_per_thread: u64) {
        if max_per_thread > 0 {
            may::config().set_workers(max_per_thread as usize);
        }
    }

    pub fn block_on(
        key: usize,
        timeout: Option<Duration>,
        ready: &dyn Fn() -> bool,
    ) -> BlockResult {
        if !coroutine::is_coroutine() {
            return BlockResult::Unavailable;
        }

        if ready() {
            return BlockResult::Woken;
        }

        let co = coroutine::current();
        let woken = Arc::new(AtomicBool::new(false));
        let id = WAITER_COUNTER.fetch_add(1, Ordering::Relaxed);

        lock_waiters().entry(key).or_default().push(Waiter {
            id,
            co: co.clone(),
            woken: woken.clone(),
        });

        if ready() {
            remove_waiter(key, id);
            return BlockResult::Woken;
        }

        let timed_out = match timeout {
            Some(dur) => {
                coroutine::park_timeout(dur);
                true
            }
            None => {
                coroutine::park();
                false
            }
        };

        remove_waiter(key, id);

        if woken.load(Ordering::Acquire) || !timed_out {
            BlockResult::Woken
        } else {
            BlockResult::TimedOut
        }
    }

    pub fn wake_all(key: usize) {
        let list = lock_waiters().remove(&key).unwrap_or_default();

        for w in list {
            w.woken.store(true, Ordering::Release);
            w.co.unpark();
        }
    }

    pub fn wake_one(key: usize) {
        let waiter = {
            let mut waiters = lock_waiters();

            let w = waiters.get_mut(&key).and_then(|list| {
                if list.is_empty() {
                    None
                } else {
                    Some(list.remove(0))
                }
            });

            if waiters.get(&key).is_some_and(|list| list.is_empty()) {
                waiters.remove(&key);
            }

            w
        };

        if let Some(w) = waiter {
            w.woken.store(true, Ordering::Release);
            w.co.unpark();
        }
    }
}

#[cfg(target_arch = "wasm32")]
mod imp {
    use super::{BlockResult, TaskArgs, note_task_spawned, run_task};
    use std::time::Duration;

    pub fn spawn_task(task: TaskArgs) -> bool {
        note_task_spawned();
        rayon::spawn(move || run_task(task));

        true
    }

    pub fn block_on(
        _key: usize,
        _timeout: Option<Duration>,
        _ready: &dyn Fn() -> bool,
    ) -> BlockResult {
        BlockResult::Unavailable
    }

    pub fn wake_all(_key: usize) {}
    pub fn wake_one(_key: usize) {}
    pub fn configure(_max_per_thread: i64) {}
}

fn spawn(closure: *mut GcAggregate, waitgroup: *mut GcHost) -> bool {
    let entry = calibre_fn_entry(closure).cast_unsigned() as usize;

    if entry == 0 {
        return false;
    }

    imp::spawn_task(TaskArgs {
        entry,
        env: closure as usize,
        args: empty_args_aggregate(),
        waitgroup: waitgroup as usize,
    })
}

fn empty_args_aggregate() -> usize {
    use std::sync::OnceLock;
    static EMPTY: OnceLock<usize> = OnceLock::new();

    *EMPTY.get_or_init(|| {
        let agg = GcAggregate::new(Vec::new());
        agg.alloc() as usize
    })
}

#[inline(always)]
pub fn block_on(key: usize, timeout: Option<Duration>, ready: &dyn Fn() -> bool) -> BlockResult {
    imp::block_on(key, timeout, ready)
}

#[inline(always)]
pub fn wake_all(key: usize) {
    imp::wake_all(key)
}

#[inline(always)]
pub fn wake_one(key: usize) {
    imp::wake_one(key)
}

#[unsafe(no_mangle)]
pub extern "C" fn calibre_scheduler_configure(max_per_thread: u64, stack_size_bytes: u64) {
    if max_per_thread > 0 {
        CONFIG_MAX_PER_THREAD.store(max_per_thread as usize, Ordering::Release);
    }

    if stack_size_bytes > 0 {
        CONFIG_STACK_SIZE.store(stack_size_bytes as usize, Ordering::Release);
    }

    imp::configure(max_per_thread);
}

#[unsafe(no_mangle)]
pub extern "C" fn calibre_spawn_task(closure: *mut GcAggregate) -> i64 {
    let wg = waitgroup_new_host();

    if !spawn(closure, wg) {
        waitgroup_host_done(wg);
    }

    wg as i64
}

#[unsafe(no_mangle)]
pub extern "C" fn calibre_scheduler_drain() {
    let mut guard = DRAIN_LOCK.lock().unwrap_or_else(|e| e.into_inner());

    while PENDING.load(Ordering::Acquire) > 0 {
        guard = DRAIN_CVAR.wait(guard).unwrap_or_else(|e| e.into_inner());
    }
}
