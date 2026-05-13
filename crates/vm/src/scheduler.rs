use std::{
    collections::VecDeque,
    sync::{
        Arc, Condvar, Mutex,
        atomic::{AtomicBool, AtomicUsize, Ordering},
    },
    thread,
};

use crate::{
    VM,
    config::VMConfig,
    value::{RuntimeValue, WaitGroupInner},
};

#[derive(Clone, Debug)]
pub struct SchedulerHandle {
    inner: Arc<SchedulerInner>,
}

#[derive(Debug)]
struct SchedulerInner {
    max_per_thread: usize,
    quantum: usize,
    workers: Mutex<Vec<Arc<Worker>>>,
    handles: Mutex<Vec<thread::JoinHandle<()>>>,
    shutdown: AtomicBool,
}

#[derive(Debug)]
struct Worker {
    queue: Mutex<VecDeque<Task>>,
    cvar: Condvar,
    quantum: usize,
    tasks: AtomicUsize,
}

#[derive(Debug)]
struct Task {
    vm: VM,
    func: RuntimeValue,
    wait_group: Option<Arc<WaitGroupInner>>,
}

impl SchedulerHandle {
    pub fn new(config: &VMConfig) -> Self {
        let max_per_thread = config.async_max_per_thread.unwrap_or(1);
        let quantum = config.async_quantum.unwrap_or(1024);

        let inner = Arc::new(SchedulerInner {
            max_per_thread,
            quantum,
            workers: Mutex::new(Vec::new()),
            handles: Mutex::new(Vec::new()),
            shutdown: AtomicBool::new(false),
        });

        let worker_count = std::thread::available_parallelism()
            .map(|n| n.get())
            .unwrap_or(2);

        {
            let mut workers = inner.workers.lock().unwrap_or_else(|e| e.into_inner());
            let mut handles = inner.handles.lock().unwrap_or_else(|e| e.into_inner());
            for _ in 0..worker_count {
                let worker = Arc::new(Worker {
                    queue: Mutex::new(VecDeque::new()),
                    cvar: Condvar::new(),
                    quantum,
                    tasks: AtomicUsize::new(0),
                });
                let handle = Self::start_worker(worker.clone(), inner.clone());
                workers.push(worker);
                handles.push(handle);
            }
        }

        SchedulerHandle { inner }
    }

    pub fn spawn(&self, base_vm: &VM, func: RuntimeValue, wait_group: Option<Arc<WaitGroupInner>>) {
        if self.inner.shutdown.load(Ordering::Acquire) {
            return;
        }

        let worker = {
            let mut workers = self.inner.workers.lock().unwrap_or_else(|e| e.into_inner());
            let mut selected = None;
            let mut best_load = usize::MAX;

            for candidate in workers.iter() {
                let load = candidate.tasks.load(Ordering::Acquire);
                if load < self.inner.max_per_thread && load < best_load {
                    best_load = load;
                    selected = Some(candidate.clone());
                }
            }

            if selected.is_none() {
                let worker = Arc::new(Worker {
                    queue: Mutex::new(VecDeque::new()),
                    cvar: Condvar::new(),
                    quantum: self.inner.quantum,
                    tasks: AtomicUsize::new(0),
                });
                let handle = Self::start_worker(worker.clone(), self.inner.clone());
                let mut handles = self.inner.handles.lock().unwrap_or_else(|e| e.into_inner());
                handles.push(handle);
                workers.push(worker.clone());
                selected = Some(worker);
            }

            if let Some(selected) = selected {
                selected
            } else {
                return;
            }
        };

        let mut vm = VM::new_shared(
            base_vm.registry.clone(),
            base_vm.mappings.clone(),
            base_vm.config.clone(),
        );

        for id in 0..vm.variables.slot_len() {
            if let Some(value) = vm.variables.get_by_id(id).cloned() {
                let resolved = base_vm.convert_runtime_var_into_saveable(value);
                let _ = vm.variables.set_by_id(id, resolved);
            }
        }

        vm.ptr_heap = base_vm.ptr_heap.clone();
        vm.moved_functions = Default::default();

        let mut queue = worker.queue.lock().unwrap_or_else(|e| e.into_inner());
        worker.tasks.fetch_add(1, Ordering::AcqRel);
        queue.push_back(Task {
            vm,
            func,
            wait_group,
        });
        worker.cvar.notify_one();
    }

    fn start_worker(worker: Arc<Worker>, scheduler: Arc<SchedulerInner>) -> thread::JoinHandle<()> {
        thread::spawn(move || {
            loop {
                let mut task = {
                    let mut queue = worker.queue.lock().unwrap_or_else(|e| e.into_inner());
                    loop {
                        if let Some(task) = queue.pop_front() {
                            break task;
                        }
                        if scheduler.shutdown.load(Ordering::Acquire) {
                            return;
                        }
                        queue = worker.cvar.wait(queue).unwrap_or_else(|e| e.into_inner());
                    }
                };

                if let Some(status) = run_task_slice(&mut task, worker.quantum) {
                    match status {
                        TaskStatus::Yielded => {
                            let mut queue = worker.queue.lock().unwrap_or_else(|e| e.into_inner());
                            queue.push_back(task);
                        }
                        TaskStatus::Finished => {
                            worker.tasks.fetch_sub(1, Ordering::AcqRel);
                            if let Some(wg) = task.wait_group.take() {
                                wg.done();
                            }
                        }
                    }
                }
            }
        })
    }

    fn stop_workers(&self) {
        self.inner.shutdown.store(true, Ordering::Release);

        let workers = self.inner.workers.lock().unwrap_or_else(|e| e.into_inner());
        for worker in workers.iter() {
            worker.cvar.notify_all();
        }

        drop(workers);

        let mut handles = self.inner.handles.lock().unwrap_or_else(|e| e.into_inner());
        for handle in handles.drain(..) {
            let _ = handle.join();
        }
    }
}

impl Drop for SchedulerHandle {
    fn drop(&mut self) {
        if Arc::strong_count(&self.inner) == 1 {
            self.stop_workers();
        }
    }
}

impl Default for SchedulerHandle {
    fn default() -> Self {
        Self::new(&VMConfig::default())
    }
}

#[derive(Debug)]
enum TaskStatus {
    Yielded,
    Finished,
}

fn run_task_slice(task: &mut Task, quantum: usize) -> Option<TaskStatus> {
    let status = match &task.func {
        RuntimeValue::Function { name, captures } => {
            let Some(func) = task.vm.resolve_function_by_name(name) else {
                return Some(TaskStatus::Finished);
            };

            let mut state = task.vm.take_task_state();

            let status = task.vm.run_function_with_budget(
                func.as_ref(),
                Vec::new(),
                captures.clone(),
                quantum,
                &mut state,
            );

            task.vm.store_task_state(state);

            match status {
                Ok(Some(_)) => TaskStatus::Finished,
                Ok(None) => TaskStatus::Yielded,
                Err(err) => {
                    eprintln!("async task error: {err}");
                    TaskStatus::Finished
                }
            }
        }
        RuntimeValue::NativeFunction(func) => {
            let _ = func.run(&mut task.vm, Vec::new());
            TaskStatus::Finished
        }
        RuntimeValue::ExternFunction(func) => {
            let _ = func.call(&mut task.vm, Vec::new());
            TaskStatus::Finished
        }
        _ => TaskStatus::Finished,
    };

    Some(status)
}
