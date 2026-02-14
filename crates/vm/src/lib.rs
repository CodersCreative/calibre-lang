use crate::{
    config::VMConfig,
    conversion::{Reg, VMBlock, VMFunction, VMRegistry},
    error::RuntimeError,
    value::{RuntimeValue, WaitGroupInner},
};
use calibre_lir::BlockId;
use rustc_hash::{FxHashMap, FxHashSet};
use std::{
    fmt::Debug,
    sync::{
        Arc,
        atomic::{AtomicBool, Ordering},
    },
};

pub mod config;
pub mod conversion;
pub mod error;
pub mod evaluate;
pub mod native;
pub mod scheduler;
pub mod serialization;
pub mod value;

#[derive(Debug, Clone)]
struct VMFrame {
    regs: Vec<RuntimeValue>,
    local_map: FxHashMap<String, Reg>,
    func_ptr: usize,
    acc: RuntimeValue,
}

#[derive(Debug, Clone, Default)]
pub struct TaskState {
    pub block: Option<BlockId>,
    pub ip: usize,
    pub prev_block: Option<BlockId>,
}

impl Default for VMFrame {
    fn default() -> Self {
        Self {
            regs: Vec::new(),
            local_map: FxHashMap::default(),
            func_ptr: 0,
            acc: RuntimeValue::Null,
        }
    }
}

#[derive(Debug, Clone, Default)]
pub struct VM {
    pub variables: FxHashMap<String, RuntimeValue>,
    pub registry: VMRegistry,
    pub mappings: Vec<String>,
    pub counter: u64,
    pub ptr_heap: FxHashMap<u64, RuntimeValue>,
    pub config: VMConfig,
    frames: Vec<VMFrame>,
    frame_pool: Vec<VMFrame>,
    caches: VMCaches,
    gc: VMGC,
    scheduler: scheduler::SchedulerHandle,
    task_state: TaskState,
}

#[derive(Debug, Clone, Default)]
pub struct VMCaches {
    call: FxHashMap<String, Arc<VMFunction>>,
    globals: FxHashMap<String, RuntimeValue>,
}

#[derive(Debug, Clone)]
pub struct VMGC {
    interval: u64,
    counter: u64,
    in_flight: Arc<AtomicBool>,
}

impl Default for VMGC {
    fn default() -> Self {
        Self {
            interval: 4096,
            counter: 0,
            in_flight: Arc::new(AtomicBool::new(false)),
        }
    }
}

impl From<VMRegistry> for VM {
    fn from(value: VMRegistry) -> Self {
        let config = VMConfig::default();
        let scheduler = scheduler::SchedulerHandle::new(&config);
        Self {
            variables: FxHashMap::default(),
            mappings: Vec::new(),
            registry: value,
            counter: 0,
            ptr_heap: FxHashMap::default(),
            config,
            frames: vec![VMFrame::default()],
            frame_pool: Vec::new(),
            caches: VMCaches::default(),
            gc: VMGC::default(),
            scheduler,
            task_state: TaskState::default(),
        }
    }
}

impl VM {
    fn find_unique_function_by_suffix(&self, short_name: &str) -> Option<Arc<VMFunction>> {
        let suffix = format!(":{}", short_name);
        for (name, func) in self.registry.functions.iter() {
            if name.ends_with(&suffix) {
                return Some(func.clone());
            }
        }

        None
    }

    fn find_unique_var_by_suffix(&self, short_name: &str) -> Option<String> {
        let suffix = format!(":{}", short_name);
        for name in self.variables.keys() {
            if name.ends_with(&suffix) {
                return Some(name.clone());
            }
        }

        None
    }

    fn find_unique_local_by_suffix(&self, short_name: &str) -> Option<Reg> {
        let suffix = format!(":{}", short_name);
        for (name, reg) in self.current_frame().local_map.iter() {
            if name.ends_with(&suffix) {
                return Some(*reg);
            }
        }
        None
    }

    fn resolve_library_candidates(name: &str) -> Vec<String> {
        let has_path = name.contains('/') || name.contains('\\');
        let lower = name.to_ascii_lowercase();
        let has_ext =
            lower.ends_with(".so") || lower.ends_with(".dylib") || lower.ends_with(".dll");

        if has_path || has_ext {
            return vec![name.to_string()];
        }

        let base = match name {
            "c" | "libc" => "c",
            other => other,
        };

        let mut out = Vec::new();

        #[cfg(target_os = "android")]
        {
            if base == "c" {
                out.push("libc.so".to_string());
            }
            out.push(format!("lib{}.so", base));
            out.push(format!("{}.so", base));
            out.push(base.to_string());
        }
        #[cfg(any(target_os = "linux", target_os = "android"))]
        {
            if base == "c" {
                out.push("libc.so.6".to_string());
            }
            out.push(format!("lib{}.so", base));
            out.push(format!("{}.so", base));
            out.push(base.to_string());
        }
        #[cfg(target_os = "macos")]
        {
            if base == "c" {
                out.push("libc.dylib".to_string());
                out.push("/usr/lib/libc.dylib".to_string());
            }
            out.push(format!("lib{}.dylib", base));
            out.push(format!("{}.dylib", base));
            out.push(base.to_string());
        }
        #[cfg(target_os = "windows")]
        {
            if base == "c" {
                out.push("msvcrt.dll".to_string());
            }
            out.push(format!("{}.dll", base));
            out.push(format!("lib{}.dll", base));
            out.push(base.to_string());
        }

        out.into_iter()
            .filter(|c| !c.is_empty())
            .collect::<Vec<_>>()
    }

    fn capture_value(&self, name: &str, seen: &mut FxHashSet<String>) -> RuntimeValue {
        if let Some(reg) = self.current_frame().local_map.get(name).copied() {
            return self.get_reg_value(reg);
        }
        if let Some((_, short)) = name.rsplit_once(':') {
            if let Some(reg) = self.find_unique_local_by_suffix(short) {
                return self.get_reg_value(reg);
            }
        }

        match self.resolve_var_name(name) {
            VarName::Var(var) => self
                .variables
                .get(&var)
                .cloned()
                .map(|v| self.copy_saveable_into_runtime_var(v))
                .unwrap_or(RuntimeValue::Null),
            VarName::Func(func) => self
                .registry
                .functions
                .get(&func)
                .map(|f| self.make_runtime_function_inner(f, seen))
                .unwrap_or(RuntimeValue::Null),
            VarName::Global => {
                if let Some((_, short_name)) = name.rsplit_once(':') {
                    if let Some(func) = self.find_unique_function_by_suffix(short_name) {
                        return self.make_runtime_function_inner(func.as_ref(), seen);
                    }
                }
                RuntimeValue::Null
            }
        }
    }

    fn capture_values(
        &self,
        captures: &[String],
        seen: &mut FxHashSet<String>,
    ) -> Vec<(String, RuntimeValue)> {
        captures
            .iter()
            .map(|name| (name.clone(), self.capture_value(name, seen)))
            .collect()
    }

    fn make_runtime_function(&self, func: &VMFunction) -> RuntimeValue {
        let mut seen = FxHashSet::default();
        self.make_runtime_function_inner(func, &mut seen)
    }

    fn make_runtime_function_inner(
        &self,
        func: &VMFunction,
        seen: &mut FxHashSet<String>,
    ) -> RuntimeValue {
        if !seen.insert(func.name.clone()) {
            return RuntimeValue::Function {
                name: func.name.clone(),
                captures: Vec::new(),
            };
        }
        RuntimeValue::Function {
            name: func.name.clone(),
            captures: self.capture_values(&func.captures, seen),
        }
    }

    pub fn new(registry: VMRegistry, mappings: Vec<String>, config: VMConfig) -> Self {
        let scheduler = scheduler::SchedulerHandle::new(&config);
        let mut vm = Self {
            registry,
            mappings,
            variables: FxHashMap::default(),
            counter: 0,
            ptr_heap: FxHashMap::default(),
            config: config.clone(),
            frames: vec![VMFrame::default()],
            frame_pool: Vec::new(),
            caches: VMCaches::default(),
            gc: VMGC::default(),
            scheduler,
            task_state: TaskState::default(),
        };

        if let Some(interval) = config.gc_interval {
            vm.gc.interval = interval;
        }

        vm.setup_global();
        vm.setup_stdlib();
        vm
    }

    pub fn spawn_async_task(&self, func: RuntimeValue, wait_group: Option<Arc<WaitGroupInner>>) {
        self.scheduler.spawn(self, func, wait_group);
    }

    pub fn take_task_state(&mut self) -> TaskState {
        std::mem::take(&mut self.task_state)
    }

    pub fn store_task_state(&mut self, state: TaskState) {
        self.task_state = state;
    }

    pub fn get_ref_id(&mut self) -> u64 {
        let id = self.counter;
        self.counter = self.counter.wrapping_add(1).max(1);
        id
    }

    fn push_frame(&mut self, reg_count: usize, func_ptr: usize) {
        if let Some(mut frame) = self.frame_pool.pop() {
            frame.regs.clear();
            frame.regs.resize(reg_count, RuntimeValue::Null);
            frame.local_map.clear();
            frame.func_ptr = func_ptr;
            frame.acc = RuntimeValue::Null;
            self.frames.push(frame);
        } else {
            self.frames.push(VMFrame {
                regs: vec![RuntimeValue::Null; reg_count],
                local_map: FxHashMap::default(),
                func_ptr,
                acc: RuntimeValue::Null,
            });
        }
    }

    fn pop_frame(&mut self) {
        if self.frames.len() <= 1 {
            if let Some(frame) = self.frames.pop() {
                self.frame_pool.push(frame);
            }
            return;
        }
        if let Some(frame) = self.frames.pop() {
            self.frame_pool.push(frame);
        }
    }

    fn current_frame_mut(&mut self) -> &mut VMFrame {
        self.frames
            .last_mut()
            .expect("vm should always have a frame")
    }

    fn current_frame(&self) -> &VMFrame {
        self.frames.last().expect("vm should always have a frame")
    }

    #[inline(always)]
    pub(crate) fn get_reg_value(&self, reg: Reg) -> RuntimeValue {
        let frame = self.current_frame();
        let idx = reg as usize;
        if idx < frame.regs.len() {
            unsafe { frame.regs.get_unchecked(idx).clone() }
        } else {
            RuntimeValue::Null
        }
    }

    #[inline(always)]
    pub(crate) fn get_reg_value_in_frame(&self, frame_idx: usize, reg: Reg) -> RuntimeValue {
        if let Some(frame) = self.frames.get(frame_idx) {
            let idx = reg as usize;
            if idx < frame.regs.len() {
                return frame.regs[idx].clone();
            }
        }
        RuntimeValue::Null
    }

    #[inline(always)]
    pub(crate) fn set_reg_value(&mut self, reg: Reg, value: RuntimeValue) {
        let _ = self.replace_reg_value(reg, value);
    }

    pub(crate) fn set_reg_value_in_frame(
        &mut self,
        frame_idx: usize,
        reg: Reg,
        value: RuntimeValue,
    ) {
        if let Some(frame) = self.frames.get_mut(frame_idx) {
            let idx = reg as usize;
            if idx < frame.regs.len() {
                frame.regs[idx] = value;
            }
        }
    }

    #[inline(always)]
    pub(crate) fn replace_reg_value(&mut self, reg: Reg, value: RuntimeValue) -> RuntimeValue {
        let frame = self.current_frame_mut();
        if reg as usize >= frame.regs.len() {
            frame.regs.resize(reg as usize + 1, RuntimeValue::Null);
        }
        let old = frame.regs[reg as usize].clone();
        frame.regs[reg as usize] = value;
        old
    }

    #[inline]
    pub(crate) fn maybe_collect_garbage(&mut self) {
        self.gc.counter = self.gc.counter.wrapping_add(1);
        if self.gc.counter < self.gc.interval {
            return;
        }
        self.gc.counter = 0;
        if self
            .gc
            .in_flight
            .compare_exchange(false, true, Ordering::AcqRel, Ordering::Acquire)
            .is_err()
        {
            return;
        }
        let in_flight = self.gc.in_flight.clone();
        std::thread::spawn(move || {
            dumpster::sync::collect();
            in_flight.store(false, Ordering::Release);
        });
    }

    #[inline(always)]
    pub(crate) fn resolve_value_for_op(
        &self,
        value: RuntimeValue,
    ) -> Result<RuntimeValue, RuntimeError> {
        let mut current = value;

        for _ in 0..64 {
            match current {
                RuntimeValue::Ref(pointer) => {
                    current = self
                        .variables
                        .get(&pointer)
                        .cloned()
                        .ok_or(RuntimeError::DanglingRef(pointer))?;
                }
                RuntimeValue::RegRef { frame, reg } => {
                    current = self.get_reg_value_in_frame(frame, reg);
                }
                RuntimeValue::MutexGuard(ref guard) => {
                    current = guard.get_clone();
                }
                other => return Ok(other),
            }
        }

        Err(RuntimeError::DanglingRef("<ref-depth-limit>".to_string()))
    }

    fn drop_runtime_value(&mut self, value: RuntimeValue) {
        let mut seen = FxHashSet::default();
        let mut seen_regs = FxHashSet::default();
        self.drop_runtime_value_inner(value, &mut seen, &mut seen_regs);
    }

    fn drop_runtime_value_inner(
        &mut self,
        value: RuntimeValue,
        seen: &mut FxHashSet<String>,
        seen_regs: &mut FxHashSet<usize>,
    ) {
        match value {
            RuntimeValue::Ref(name) => {
                if !seen.insert(name.clone()) {
                    return;
                }
                if let Some(inner) = self.variables.remove(&name) {
                    self.drop_runtime_value_inner(inner, seen, seen_regs);
                }
            }
            RuntimeValue::RegRef { frame, reg } => {
                let key = (frame << 16) ^ reg as usize;
                if !seen_regs.insert(key) {
                    return;
                }
                let inner = self.get_reg_value_in_frame(frame, reg);
                self.set_reg_value_in_frame(frame, reg, RuntimeValue::Null);
                self.drop_runtime_value_inner(inner, seen, seen_regs);
            }
            RuntimeValue::List(list) => {
                for item in list.as_ref().0.iter().cloned() {
                    self.drop_runtime_value_inner(item, seen, seen_regs);
                }
            }
            RuntimeValue::Aggregate(_, data) => {
                for (_, value) in data.as_ref().0.0.iter().cloned() {
                    self.drop_runtime_value_inner(value, seen, seen_regs);
                }
            }
            RuntimeValue::HashMap(map) => {
                if let Ok(guard) = map.lock() {
                    for (_, value) in guard.iter() {
                        self.drop_runtime_value_inner(value.clone(), seen, seen_regs);
                    }
                }
            }
            RuntimeValue::HashSet(_) => {}
            RuntimeValue::Option(Some(x)) => {
                self.drop_runtime_value_inner(x.as_ref().clone(), seen, seen_regs);
            }
            RuntimeValue::Result(Ok(x)) => {
                self.drop_runtime_value_inner(x.as_ref().clone(), seen, seen_regs);
            }
            RuntimeValue::Result(Err(x)) => {
                self.drop_runtime_value_inner(x.as_ref().clone(), seen, seen_regs);
            }
            RuntimeValue::Enum(_, _, payload) => {
                if let Some(val) = payload {
                    self.drop_runtime_value_inner(val.as_ref().clone(), seen, seen_regs);
                }
            }
            RuntimeValue::Channel(ch) => {
                if let Ok(mut queue) = ch.queue.lock() {
                    while let Some(item) = queue.pop_front() {
                        self.drop_runtime_value_inner(item, seen, seen_regs);
                    }
                }
            }
            _ => {}
        }
    }
}

#[derive(Debug, Clone)]
enum VarName {
    Var(String),
    Func(String),
    Global,
}

impl VM {
    fn resolve_var_name(&self, name: &str) -> VarName {
        if self.variables.contains_key(name) {
            return VarName::Var(name.to_string());
        }
        if self.registry.functions.contains_key(name) {
            return VarName::Func(name.to_string());
        }
        if name.contains("::") {
            if let Some((_prefix, short)) = name.split_once("::") {
                if let Some(var) = self.find_unique_var_by_suffix(short) {
                    return VarName::Var(var);
                }
                if let Some(func) = self.find_unique_function_by_suffix(short) {
                    return VarName::Func(func.name.clone());
                }
            }
        } else if let Some((_, short_name)) = name.rsplit_once(':') {
            if let Some(var) = self.find_unique_var_by_suffix(short_name) {
                return VarName::Var(var);
            }
            if let Some(func) = self.find_unique_function_by_suffix(short_name) {
                return VarName::Func(func.name.clone());
            }
        } else {
            if let Some(var) = self.find_unique_var_by_suffix(name) {
                return VarName::Var(var);
            }
            if let Some(func) = self.find_unique_function_by_suffix(name) {
                return VarName::Func(func.name.clone());
            }
        }

        VarName::Global
    }

    fn local_string<'a>(&self, block: &'a VMBlock, idx: u16) -> Result<&'a str, RuntimeError> {
        let idx = idx as usize;
        if idx >= block.local_strings.len() {
            return Err(RuntimeError::InvalidBytecode(format!(
                "missing string {}",
                idx
            )));
        }
        Ok(unsafe { block.local_strings.get_unchecked(idx).as_str() })
    }

    fn local_string_owned<'a>(
        &self,
        block: &'a VMBlock,
        idx: u16,
    ) -> Result<&'a String, RuntimeError> {
        let idx = idx as usize;
        if idx >= block.local_strings.len() {
            return Err(RuntimeError::InvalidBytecode(format!(
                "missing string {}",
                idx
            )));
        }
        Ok(unsafe { block.local_strings.get_unchecked(idx) })
    }
}
