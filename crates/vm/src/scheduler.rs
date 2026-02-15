use std::{
    collections::VecDeque,
    sync::{
        Arc, Condvar, Mutex,
        atomic::{AtomicUsize, Ordering},
    },
    thread,
};

use crate::{
    VM,
    config::VMConfig,
    conversion::VMFunction,
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
        });
        let worker_count = std::thread::available_parallelism()
            .map(|n| n.get())
            .unwrap_or(1);

        {
            let mut workers = inner.workers.lock().unwrap_or_else(|e| e.into_inner());
            for _ in 0..worker_count {
                let worker = Arc::new(Worker {
                    queue: Mutex::new(VecDeque::new()),
                    cvar: Condvar::new(),
                    quantum,
                    tasks: AtomicUsize::new(0),
                });
                Self::start_worker(worker.clone(), inner.clone());
                workers.push(worker);
            }
        }

        SchedulerHandle { inner }
    }

    pub fn spawn(&self, base_vm: &VM, func: RuntimeValue, wait_group: Option<Arc<WaitGroupInner>>) {
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
                Self::start_worker(worker.clone(), self.inner.clone());
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
        vm.variables = base_vm.variables.clone();
        vm.ptr_heap = base_vm.ptr_heap.clone();
        vm.moved_functions = base_vm.moved_functions.clone();

        let mut queue = worker.queue.lock().unwrap_or_else(|e| e.into_inner());
        worker.tasks.fetch_add(1, Ordering::AcqRel);
        queue.push_back(Task {
            vm,
            func,
            wait_group,
        });
        worker.cvar.notify_one();
    }

    fn start_worker(worker: Arc<Worker>, _scheduler: Arc<SchedulerInner>) {
        thread::spawn(move || {
            loop {
                let mut task = {
                    let mut queue = worker.queue.lock().unwrap_or_else(|e| e.into_inner());
                    loop {
                        if let Some(task) = queue.pop_front() {
                            break task;
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
        });
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

fn resolve_function(vm: &VM, name: &str) -> Option<Arc<VMFunction>> {
    if let Some(found) = vm.get_function(name) {
        return Some(found);
    }
    if let Some((prefix, _)) = name.split_once("->") {
        if let Some(found) = vm
            .registry
            .functions
            .iter()
            .filter(|(k, _)| !vm.moved_functions.contains(*k))
            .find(|(k, _)| k.starts_with(prefix))
            .map(|(_, v)| v.clone())
        {
            return Some(found);
        }
    }
    if let Some((_, short)) = name.rsplit_once(':') {
        let suffix = format!(":{}", short);
        let mut found = None;
        for (k, v) in vm.registry.functions.iter() {
            if vm.moved_functions.contains(k) {
                continue;
            }
            if k.ends_with(&suffix) {
                if found.is_some() {
                    return None;
                }
                found = Some(v.clone());
            }
        }
        return found;
    }
    None
}

fn run_task_slice(task: &mut Task, quantum: usize) -> Option<TaskStatus> {
    match &task.func {
        RuntimeValue::Function { name, captures } => {
            let Some(func) = resolve_function(&task.vm, name) else {
                return Some(TaskStatus::Finished);
            };
            let mut state = task.vm.take_task_state();
            let status = task.vm.run_function_with_budget(
                func.as_ref(),
                Vec::<RuntimeValue>::new(),
                captures.clone(),
                quantum,
                &mut state,
            );
            task.vm.store_task_state(state);
            match status {
                Ok(Some(_)) => Some(TaskStatus::Finished),
                Ok(None) => Some(TaskStatus::Yielded),
                Err(_) => Some(TaskStatus::Finished),
            }
        }
        RuntimeValue::NativeFunction(func) => {
            let _ = func.run(&mut task.vm, Vec::new());
            Some(TaskStatus::Finished)
        }
        RuntimeValue::ExternFunction(func) => {
            let _ = func.call(&mut task.vm, Vec::new());
            Some(TaskStatus::Finished)
        }
        _ => Some(TaskStatus::Finished),
    }
}
