use std::{
    collections::VecDeque,
    sync::{Arc, Condvar, Mutex},
    thread,
    time::Duration,
};

use crate::{VM, config::VMConfig, conversion::VMFunction, value::RuntimeValue};

#[derive(Clone, Debug)]
pub struct SchedulerHandle {
    inner: Arc<SchedulerInner>,
}

#[derive(Debug)]
struct SchedulerInner {
    max_per_thread: usize,
    quantum: usize,
    max_idle_ticks: usize,
    idle_wait: Duration,
    workers: Mutex<Vec<Arc<Worker>>>,
}

#[derive(Debug)]
struct Worker {
    queue: Mutex<VecDeque<Task>>,
    cvar: Condvar,
    quantum: usize,
}

#[derive(Debug)]
struct Task {
    vm: VM,
    func: RuntimeValue,
}

impl SchedulerHandle {
    pub fn new(config: &VMConfig) -> Self {
        let max_per_thread = config.async_max_per_thread.unwrap_or(1);
        let quantum = config.async_quantum.unwrap_or(1024);
        let inner = Arc::new(SchedulerInner {
            max_per_thread,
            quantum,
            max_idle_ticks: 20,
            idle_wait: Duration::from_millis(50),
            workers: Mutex::new(Vec::new()),
        });

        SchedulerHandle { inner }
    }

    pub fn spawn(&self, base_vm: &VM, func: RuntimeValue) {
        let mut workers = self.inner.workers.lock().unwrap();
        let worker = workers
            .iter()
            .find(|w| w.queue.lock().unwrap().len() < self.inner.max_per_thread)
            .cloned()
            .unwrap_or_else(|| {
                let worker = Arc::new(Worker {
                    queue: Mutex::new(VecDeque::new()),
                    cvar: Condvar::new(),
                    quantum: self.inner.quantum,
                });
                Self::start_worker(worker.clone(), self.inner.clone());
                workers.push(worker.clone());
                worker
            });

        drop(workers);

        let mut vm = VM::new(
            base_vm.registry.clone(),
            base_vm.mappings.clone(),
            base_vm.config.clone(),
        );
        vm.variables = base_vm.variables.clone();
        vm.ptr_heap = base_vm.ptr_heap.clone();

        let mut queue = worker.queue.lock().unwrap();
        queue.push_back(Task { vm, func });
        worker.cvar.notify_one();
    }

    fn start_worker(worker: Arc<Worker>, scheduler: Arc<SchedulerInner>) {
        thread::spawn(move || {
            let mut idle_ticks = 0usize;
            loop {
                let mut task = {
                    let mut queue = worker.queue.lock().unwrap();
                    loop {
                        if let Some(task) = queue.pop_front() {
                            idle_ticks = 0;
                            break task;
                        }
                        let (guard, timeout) = worker
                            .cvar
                            .wait_timeout(queue, scheduler.idle_wait)
                            .unwrap();
                        queue = guard;
                        if timeout.timed_out() {
                            idle_ticks += 1;
                            if idle_ticks >= scheduler.max_idle_ticks {
                                return;
                            }
                        }
                    }
                };

                if let Some(status) = run_task_slice(&mut task, worker.quantum) {
                    if matches!(status, TaskStatus::Yielded) {
                        let mut queue = worker.queue.lock().unwrap();
                        queue.push_back(task);
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
    if let Some(found) = vm.registry.functions.get(name) {
        return Some(found.clone());
    }
    if let Some((prefix, _)) = name.split_once("->") {
        if let Some(found) = vm
            .registry
            .functions
            .iter()
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
    let func = task.func.clone();
    match func {
        RuntimeValue::Function { name, captures } => {
            let Some(func) = resolve_function(&task.vm, &name) else {
                return Some(TaskStatus::Finished);
            };
            let mut state = task.vm.take_task_state();
            let status = task.vm.run_function_with_budget(
                func.as_ref(),
                Vec::<RuntimeValue>::new(),
                captures,
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
