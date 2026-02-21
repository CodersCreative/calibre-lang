use crate::{
    config::VMConfig,
    conversion::{Reg, VMBlock, VMFunction, VMRegistry},
    error::RuntimeError,
    native::NativeFunction,
    value::{ExternFunction, GcMap, RuntimeValue, WaitGroupInner},
    variables::VariableStore,
};
use calibre_lir::BlockId;
use rustc_hash::{FxHashMap, FxHashSet};
use std::sync::OnceLock;
use std::{
    fmt::Debug,
    sync::{
        Arc,
        atomic::{AtomicBool, Ordering},
    },
};

static NULL_RUNTIME_VALUE: RuntimeValue = RuntimeValue::Null;
static EMPTY_FRAME: OnceLock<VMFrame> = OnceLock::new();
static EMPTY_CAPTURES: OnceLock<std::sync::Arc<Vec<(String, RuntimeValue)>>> = OnceLock::new();

pub mod config;
pub mod conversion;
pub mod error;
pub mod evaluate;
pub mod native;
pub mod scheduler;
pub mod serialization;
pub mod value;
pub mod variables;

#[derive(Debug, Clone)]
struct VMFrame {
    reg_start: usize,
    reg_count: usize,
    local_map_base: Option<Arc<FxHashMap<Arc<str>, Reg>>>,
    local_map: FxHashMap<Arc<str>, Reg>,
    func_ptr: usize,
    acc: RuntimeValue,
}

#[derive(Debug, Clone, Default)]
pub struct TaskState {
    pub block: Option<BlockId>,
    pub ip: usize,
    pub prev_block: Option<BlockId>,
    pub yielded: Option<RuntimeValue>,
}

impl Default for VMFrame {
    fn default() -> Self {
        Self {
            reg_start: 0,
            reg_count: 0,
            local_map_base: None,
            local_map: FxHashMap::default(),
            func_ptr: 0,
            acc: RuntimeValue::Null,
        }
    }
}

#[derive(Debug, Clone, Default)]
pub struct VM {
    pub variables: VariableStore,
    pub registry: Arc<VMRegistry>,
    pub mappings: Arc<Vec<String>>,
    pub counter: u64,
    pub ptr_heap: FxHashMap<u64, RuntimeValue>,
    pub config: VMConfig,
    reg_arena: Vec<RuntimeValue>,
    reg_top: usize,
    name_arena: FxHashMap<Arc<str>, Arc<str>>,
    frames: Vec<VMFrame>,
    frame_pool: Vec<VMFrame>,
    caches: VMCaches,
    func_suffix: FxHashMap<String, Option<Arc<VMFunction>>>,
    gc: VMGC,
    scheduler: scheduler::SchedulerHandle,
    task_state: TaskState,
    pub(crate) moved_functions: FxHashSet<String>,
    pub suppress_output: bool,
    pub captured_output: String,
}

#[derive(Debug, Clone)]
pub struct VMCaches {
    call: FxHashMap<String, Arc<VMFunction>>,
    globals: FxHashMap<String, RuntimeValue>,
    callsite: FxHashMap<(usize, usize, u32), Arc<VMFunction>>,
    globals_direct: FxHashMap<String, RuntimeValue>,
    locals: FxHashMap<usize, Arc<FxHashMap<Arc<str>, Reg>>>,
    globals_id: FxHashMap<String, usize>,
    local_str: FxHashMap<(u32, u16), Arc<str>>,
    aggregate_member_slots: FxHashMap<String, FxHashMap<String, usize>>,
    prepared_direct_calls: FxHashMap<(usize, usize, u32), PreparedDirectCall>,
    unique_var_suffix: FxHashMap<String, Option<String>>,
}

impl Default for VMCaches {
    fn default() -> Self {
        Self {
            call: FxHashMap::default(),
            globals: FxHashMap::default(),
            callsite: FxHashMap::default(),
            globals_direct: FxHashMap::default(),
            locals: FxHashMap::default(),
            globals_id: FxHashMap::default(),
            local_str: FxHashMap::default(),
            aggregate_member_slots: FxHashMap::default(),
            prepared_direct_calls: FxHashMap::default(),
            unique_var_suffix: FxHashMap::default(),
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) enum PreparedDirectCall {
    Vm(Arc<VMFunction>),
    Native(Arc<dyn NativeFunction>),
    Extern(Arc<ExternFunction>),
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
            interval: 1_048_576,
            counter: 0,
            in_flight: Arc::new(AtomicBool::new(false)),
        }
    }
}

impl From<VMRegistry> for VM {
    fn from(value: VMRegistry) -> Self {
        let config = VMConfig::default();
        let scheduler = scheduler::SchedulerHandle::new(&config);
        let func_suffix = build_function_suffix_index(&value);
        let globals_direct = build_globals_direct_cache(&value);
        let mut vm = Self {
            variables: VariableStore::default(),
            mappings: Arc::new(Vec::new()),
            registry: Arc::new(value),
            counter: 0,
            ptr_heap: FxHashMap::default(),
            config,
            reg_arena: Vec::new(),
            reg_top: 0,
            name_arena: FxHashMap::default(),
            frames: vec![VMFrame::default()],
            frame_pool: Vec::new(),
            caches: VMCaches {
                globals_direct,
                ..VMCaches::default()
            },
            func_suffix,
            gc: VMGC::default(),
            scheduler,
            task_state: TaskState::default(),
            moved_functions: FxHashSet::default(),
            suppress_output: false,
            captured_output: String::new(),
        };
        vm.preallocate_execution_buffers();
        vm
    }
}

impl VM {
    fn preallocate_execution_buffers(&mut self) {
        let mut max_regs = 0usize;
        for func in self.registry.functions.values() {
            max_regs = max_regs.max(func.reg_count as usize);
        }
        let frame_capacity = 256usize;
        let reg_capacity = max_regs.max(32).saturating_mul(16).min(1_000_000);
        self.frames.reserve(frame_capacity);
        self.frame_pool.reserve(frame_capacity);
        self.reg_arena.reserve(reg_capacity);
    }

    #[inline]
    pub(crate) fn empty_captures() -> std::sync::Arc<Vec<(String, RuntimeValue)>> {
        EMPTY_CAPTURES
            .get_or_init(|| std::sync::Arc::new(Vec::new()))
            .clone()
    }

    pub(crate) fn get_function(&self, name: &str) -> Option<Arc<VMFunction>> {
        if self.moved_functions.contains(name) {
            return None;
        }
        self.registry.functions.get(name).cloned()
    }

    pub(crate) fn take_function(&mut self, name: &str) -> Option<Arc<VMFunction>> {
        if self.moved_functions.contains(name) {
            return None;
        }
        let func = self.registry.functions.get(name).cloned();
        if func.is_some() {
            self.moved_functions.insert(name.to_string());
        }
        func
    }

    fn find_unique_function_by_suffix(&self, short_name: &str) -> Option<Arc<VMFunction>> {
        let Some(entry) = self.func_suffix.get(short_name) else {
            return None;
        };
        let Some(func) = entry.as_ref() else {
            return None;
        };
        if self.moved_functions.contains(&func.name) {
            return None;
        }
        Some(func.clone())
    }

    fn find_unique_var_by_suffix(&self, short_name: &str) -> Option<String> {
        for name in self.variables.keys() {
            if let Some((_, s)) = name.rsplit_once(':') {
                if s == short_name {
                    return Some(name.to_string());
                }
            }
        }

        None
    }

    #[inline]
    fn find_unique_var_by_suffix_cached(&mut self, short_name: &str) -> Option<String> {
        if let Some(cached) = self.caches.unique_var_suffix.get(short_name) {
            return cached.clone();
        }
        let found = self.find_unique_var_by_suffix(short_name);
        self.caches
            .unique_var_suffix
            .insert(short_name.to_string(), found.clone());
        found
    }

    #[inline]
    pub(crate) fn invalidate_name_resolution_caches(&mut self) {
        self.caches.unique_var_suffix.clear();
    }

    fn find_unique_local_by_suffix(&self, short_name: &str) -> Option<Reg> {
        if let Some(base) = self.current_frame().local_map_base.as_ref() {
            for (name, reg) in base.iter() {
                if let Some((_, s)) = name.as_ref().rsplit_once(':') {
                    if s == short_name {
                        return Some(*reg);
                    }
                }
            }
        }
        for (name, reg) in self.current_frame().local_map.iter() {
            if let Some((_, s)) = name.as_ref().rsplit_once(':') {
                if s == short_name {
                    return Some(*reg);
                }
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
        if let Some(base) = self.current_frame().local_map_base.as_ref() {
            if let Some(reg) = base.get(name).copied() {
                return self.get_reg_value(reg);
            }
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
        let name = func.name.clone();
        if !seen.insert(name.clone()) {
            return RuntimeValue::Function {
                name: name.clone().into(),
                captures: std::sync::Arc::new(Vec::new()),
            };
        }
        RuntimeValue::Function {
            name: name.clone().into(),
            captures: std::sync::Arc::new(self.capture_values(&func.captures, seen)),
        }
    }

    pub fn new(registry: VMRegistry, mappings: Vec<String>, config: VMConfig) -> Self {
        let scheduler = scheduler::SchedulerHandle::new(&config);
        let func_suffix = build_function_suffix_index(&registry);
        let globals_direct = build_globals_direct_cache(&registry);
        let mut vm = Self {
            registry: Arc::new(registry),
            mappings: Arc::new(mappings),
            variables: VariableStore::default(),
            counter: 0,
            ptr_heap: FxHashMap::default(),
            config: config.clone(),
            reg_arena: Vec::new(),
            reg_top: 0,
            name_arena: FxHashMap::default(),
            frames: vec![VMFrame::default()],
            frame_pool: Vec::new(),
            caches: VMCaches {
                globals_direct,
                ..VMCaches::default()
            },
            func_suffix,
            gc: VMGC::default(),
            scheduler,
            task_state: TaskState::default(),
            moved_functions: FxHashSet::default(),
            suppress_output: false,
            captured_output: String::new(),
        };

        if let Some(interval) = config.gc_interval {
            vm.gc.interval = interval;
        }

        vm.preallocate_execution_buffers();
        vm.setup_global();
        vm.setup_stdlib();
        vm
    }

    pub fn new_shared(
        registry: Arc<VMRegistry>,
        mappings: Arc<Vec<String>>,
        config: VMConfig,
    ) -> Self {
        let scheduler = scheduler::SchedulerHandle::new(&config);
        let func_suffix = build_function_suffix_index(&registry);
        let globals_direct = build_globals_direct_cache(&registry);
        let mut vm = Self {
            registry,
            mappings,
            variables: VariableStore::default(),
            counter: 0,
            ptr_heap: FxHashMap::default(),
            config: config.clone(),
            reg_arena: Vec::new(),
            reg_top: 0,
            name_arena: FxHashMap::default(),
            frames: vec![VMFrame::default()],
            frame_pool: Vec::new(),
            caches: VMCaches {
                globals_direct,
                ..VMCaches::default()
            },
            func_suffix,
            gc: VMGC::default(),
            scheduler,
            task_state: TaskState::default(),
            moved_functions: FxHashSet::default(),
            suppress_output: false,
            captured_output: String::new(),
        };

        if let Some(interval) = config.gc_interval {
            vm.gc.interval = interval;
        }

        vm.preallocate_execution_buffers();
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

    pub fn take_captured_output(&mut self) -> String {
        std::mem::take(&mut self.captured_output)
    }

    pub fn get_ref_id(&mut self) -> u64 {
        let id = self.counter;
        self.counter = self.counter.wrapping_add(1).max(1);
        id
    }

    fn push_frame(&mut self, reg_count: usize, func_ptr: usize) {
        let start = self.reg_top;
        self.reg_top = self.reg_top.saturating_add(reg_count);
        if self.reg_top > self.reg_arena.len() {
            self.reg_arena.resize(self.reg_top, RuntimeValue::Null);
        }
        if let Some(mut frame) = self.frame_pool.pop() {
            frame.reg_start = start;
            frame.reg_count = reg_count;
            frame.local_map_base = None;
            frame.local_map.clear();
            frame.func_ptr = func_ptr;
            frame.acc = RuntimeValue::Null;
            self.frames.push(frame);
        } else {
            self.frames.push(VMFrame {
                reg_start: start,
                reg_count,
                local_map_base: None,
                local_map: FxHashMap::default(),
                func_ptr,
                acc: RuntimeValue::Null,
            });
        }
    }

    fn pop_frame(&mut self) {
        if self.frames.len() <= 1 {
            if let Some(frame) = self.frames.pop() {
                self.reg_top = frame.reg_start;
                self.frame_pool.push(frame);
            }
            return;
        }
        if let Some(frame) = self.frames.pop() {
            self.reg_top = frame.reg_start;
            self.frame_pool.push(frame);
        }
    }

    fn current_frame_mut(&mut self) -> &mut VMFrame {
        if self.frames.is_empty() {
            self.frames.push(VMFrame::default());
        }
        let idx = self.frames.len() - 1;
        &mut self.frames[idx]
    }

    fn current_frame(&self) -> &VMFrame {
        self.frames
            .last()
            .unwrap_or_else(|| EMPTY_FRAME.get_or_init(VMFrame::default))
    }

    #[inline(always)]
    pub(crate) fn get_reg_value(&self, reg: Reg) -> RuntimeValue {
        let frame = self.current_frame();
        let idx = reg as usize;
        if idx < frame.reg_count {
            unsafe { self.reg_arena.get_unchecked(frame.reg_start + idx).clone() }
        } else {
            RuntimeValue::Null
        }
    }

    #[inline(always)]
    pub(crate) fn get_reg_value_ref(&self, reg: Reg) -> &RuntimeValue {
        let frame = self.current_frame();
        let idx = reg as usize;
        if idx < frame.reg_count {
            unsafe { self.reg_arena.get_unchecked(frame.reg_start + idx) }
        } else {
            &NULL_RUNTIME_VALUE
        }
    }

    #[inline(always)]
    pub(crate) fn get_reg_value_in_frame(&self, frame_idx: usize, reg: Reg) -> RuntimeValue {
        if let Some(frame) = self.frames.get(frame_idx) {
            let idx = reg as usize;
            if idx < frame.reg_count {
                return self.reg_arena[frame.reg_start + idx].clone();
            }
        }
        RuntimeValue::Null
    }

    #[inline(always)]
    pub(crate) fn get_reg_value_in_frame_ref(&self, frame_idx: usize, reg: Reg) -> &RuntimeValue {
        if let Some(frame) = self.frames.get(frame_idx) {
            let idx = reg as usize;
            if idx < frame.reg_count {
                return &self.reg_arena[frame.reg_start + idx];
            }
        }
        &NULL_RUNTIME_VALUE
    }

    #[inline(always)]
    pub(crate) fn get_reg_value_in_frame_mut(
        &mut self,
        frame_idx: usize,
        reg: Reg,
    ) -> Option<&mut RuntimeValue> {
        let frame = self.frames.get(frame_idx)?;
        let idx = reg as usize;
        if idx >= frame.reg_count {
            return None;
        }
        let pos = frame.reg_start + idx;
        self.reg_arena.get_mut(pos)
    }

    #[inline(always)]
    pub(crate) fn set_reg_value(&mut self, reg: Reg, value: RuntimeValue) {
        if let RuntimeValue::Null = value {
            let frame = self.current_frame();
            let idx = reg as usize;
            if idx < frame.reg_count {
                if let RuntimeValue::Null = self.reg_arena[frame.reg_start + idx] {
                    return;
                }
            }
        }
        let _ = self.replace_reg_value(reg, value);
    }

    pub(crate) fn intern_name(&mut self, name: &str) -> Arc<str> {
        if let Some(existing) = self.name_arena.get(name) {
            return existing.clone();
        }
        let arc: Arc<str> = Arc::from(name);
        self.name_arena.insert(arc.clone(), arc.clone());
        arc
    }

    pub(crate) fn intern_local_string(
        &mut self,
        block: &VMBlock,
        idx: u16,
    ) -> Result<Arc<str>, RuntimeError> {
        let key = (block.id.0, idx);
        if let Some(existing) = self.caches.local_str.get(&key) {
            return Ok(existing.clone());
        }
        let name = self.local_string(block, idx)?;
        let interned = self.intern_name(name);
        self.caches.local_str.insert(key, interned.clone());
        Ok(interned)
    }

    pub(crate) fn local_map_base_for(
        &mut self,
        func: &VMFunction,
    ) -> Arc<FxHashMap<Arc<str>, Reg>> {
        let key = func as *const VMFunction as usize;
        if let Some(found) = self.caches.locals.get(&key) {
            return found.clone();
        }
        let mut map: FxHashMap<Arc<str>, Reg> = FxHashMap::default();
        for (name, reg) in func.params.iter().zip(func.param_regs.iter().copied()) {
            let interned = self.intern_name(name);
            map.insert(interned, reg);
        }
        let arc = Arc::new(map);
        self.caches.locals.insert(key, arc.clone());
        arc
    }

    pub(crate) fn set_reg_value_in_frame(
        &mut self,
        frame_idx: usize,
        reg: Reg,
        value: RuntimeValue,
    ) {
        if let Some(frame) = self.frames.get_mut(frame_idx) {
            let idx = reg as usize;
            if idx < frame.reg_count {
                let pos = frame.reg_start + idx;
                self.reg_arena[pos] = value;
            }
        }
    }

    #[inline(always)]
    pub(crate) fn replace_reg_value(&mut self, reg: Reg, value: RuntimeValue) -> RuntimeValue {
        let idx = reg as usize;
        let (start, mut reg_count) = {
            let frame = self.current_frame();
            (frame.reg_start, frame.reg_count)
        };
        if idx >= reg_count {
            let new_len = idx + 1;
            if start + new_len > self.reg_arena.len() {
                self.reg_arena.resize(start + new_len, RuntimeValue::Null);
            }
            if start + new_len > self.reg_top {
                self.reg_top = start + new_len;
            }
            reg_count = new_len;
            let frame = self.current_frame_mut();
            frame.reg_count = reg_count;
        }
        let pos = start + idx;
        std::mem::replace(&mut self.reg_arena[pos], value)
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
        dumpster::sync::collect();
        self.gc.in_flight.store(false, Ordering::Release);
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
                RuntimeValue::VarRef(id) => {
                    current = self
                        .variables
                        .get_by_id(id)
                        .ok_or(RuntimeError::DanglingRef(format!("#{}", id)))?;
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

    pub(crate) fn resolve_value_for_op_ref(
        &self,
        value: &RuntimeValue,
    ) -> Result<RuntimeValue, RuntimeError> {
        let mut owned: Option<RuntimeValue> = None;

        for _ in 0..64 {
            let (current, from_owned) = match &owned {
                Some(v) => (v, true),
                None => (value, false),
            };

            match current {
                RuntimeValue::Ref(pointer) => {
                    let v = self
                        .variables
                        .get(pointer)
                        .cloned()
                        .ok_or(RuntimeError::DanglingRef(pointer.clone()))?;
                    owned = Some(v);
                }
                RuntimeValue::VarRef(id) => {
                    let v = self
                        .variables
                        .get_by_id(*id)
                        .ok_or(RuntimeError::DanglingRef(format!("#{}", id)))?;
                    owned = Some(v);
                }
                RuntimeValue::RegRef { frame, reg } => {
                    let v = self.get_reg_value_in_frame(*frame, *reg);
                    owned = Some(v);
                }
                RuntimeValue::MutexGuard(guard) => {
                    let v = guard.get_clone();
                    owned = Some(v);
                }
                _ => {
                    if from_owned {
                        if let Some(value) = owned.take() {
                            return Ok(value);
                        }
                        return Err(RuntimeError::DanglingRef("<owned-missing>".to_string()));
                    }
                    return Ok(current.clone());
                }
            }
        }

        Err(RuntimeError::DanglingRef("<ref-depth-limit>".to_string()))
    }

    fn drop_runtime_value(&mut self, value: RuntimeValue) {
        let mut seen = FxHashSet::default();
        let mut seen_regs = FxHashSet::default();
        // start from the owned value but traverse by reference when possible
        self.drop_runtime_value_inner_ref(&value, &mut seen, &mut seen_regs);
    }

    fn drop_runtime_value_inner_ref(
        &mut self,
        value: &RuntimeValue,
        seen: &mut FxHashSet<String>,
        seen_regs: &mut FxHashSet<usize>,
    ) {
        match value {
            RuntimeValue::Ref(name) => {
                if !seen.insert(name.clone()) {
                    return;
                }
                if let Some(inner) = self.variables.remove(name) {
                    self.drop_runtime_value_inner_ref(&inner, seen, seen_regs);
                }
            }
            RuntimeValue::VarRef(id) => {
                let key = format!("#{}", id);
                if !seen.insert(key) {
                    return;
                }
                if let Some(inner) = self.variables.remove_by_id(*id) {
                    self.drop_runtime_value_inner_ref(&inner, seen, seen_regs);
                }
            }
            RuntimeValue::RegRef { frame, reg } => {
                let key = (frame << 16) ^ *reg as usize;
                if !seen_regs.insert(key) {
                    return;
                }
                let inner = self.get_reg_value_in_frame(*frame, *reg);
                self.set_reg_value_in_frame(*frame, *reg, RuntimeValue::Null);
                self.drop_runtime_value_inner_ref(&inner, seen, seen_regs);
            }
            RuntimeValue::List(list) => {
                for item in list.as_ref().0.iter() {
                    self.drop_runtime_value_inner_ref(item, seen, seen_regs);
                }
            }
            RuntimeValue::Aggregate(_, data) => {
                for (_, value) in data.as_ref().0.0.iter() {
                    self.drop_runtime_value_inner_ref(value, seen, seen_regs);
                }
            }
            RuntimeValue::HashMap(map) => {
                if let Ok(guard) = map.lock() {
                    for (_, value) in guard.iter() {
                        self.drop_runtime_value_inner_ref(value, seen, seen_regs);
                    }
                }
            }
            RuntimeValue::HashSet(_) => {}
            RuntimeValue::Option(Some(x)) => {
                self.drop_runtime_value_inner_ref(x.as_ref(), seen, seen_regs);
            }
            RuntimeValue::Result(Ok(x)) => {
                self.drop_runtime_value_inner_ref(x.as_ref(), seen, seen_regs);
            }
            RuntimeValue::Result(Err(x)) => {
                self.drop_runtime_value_inner_ref(x.as_ref(), seen, seen_regs);
            }
            RuntimeValue::Enum(_, _, payload) => {
                if let Some(val) = payload {
                    self.drop_runtime_value_inner_ref(val.as_ref(), seen, seen_regs);
                }
            }
            RuntimeValue::Generator { .. } => {}
            RuntimeValue::Channel(ch) => {
                if let Ok(mut queue) = ch.queue.lock() {
                    while let Some(item) = queue.pop_front() {
                        self.drop_runtime_value_inner_ref(&item, seen, seen_regs);
                    }
                }
            }
            _ => {}
        }
    }
}

fn build_function_suffix_index(
    registry: &VMRegistry,
) -> FxHashMap<String, Option<Arc<VMFunction>>> {
    let mut out: FxHashMap<String, Option<Arc<VMFunction>>> =
        FxHashMap::with_capacity_and_hasher(registry.functions.len(), Default::default());
    for (name, func) in registry.functions.iter() {
        let Some((_, short)) = name.rsplit_once(':') else {
            continue;
        };
        match out.get(short) {
            None => {
                out.insert(short.to_string(), Some(func.clone()));
            }
            Some(Some(_)) => {
                out.insert(short.to_string(), None);
            }
            Some(None) => {}
        }
    }
    out
}

fn build_globals_direct_cache(registry: &VMRegistry) -> FxHashMap<String, RuntimeValue> {
    let mut out: FxHashMap<String, RuntimeValue> =
        FxHashMap::with_capacity_and_hasher(registry.functions.len(), Default::default());
    for (name, _func) in registry.functions.iter() {
        out.insert(
            name.clone(),
            RuntimeValue::Function {
                name: Arc::new(name.clone()),
                captures: std::sync::Arc::new(Vec::new()),
            },
        );
    }
    out
}

#[derive(Debug, Clone)]
enum VarName {
    Var(String),
    Func(String),
    Global,
}

impl VM {
    #[inline]
    pub(crate) fn is_gen_type_name(type_name: &str) -> bool {
        let short = type_name.rsplit(':').next().unwrap_or(type_name);
        short == "gen" || short.starts_with("gen->")
    }

    pub(crate) fn resolve_aggregate_member_slot(
        &mut self,
        type_name: &str,
        map: &GcMap,
        name: &str,
        short_name: Option<&str>,
    ) -> Option<usize> {
        if map.0.0.len() <= 3 {
            return map.0.0.iter().enumerate().find_map(|(idx, (field, _))| {
                if field == name || short_name.is_some_and(|short| field == short) {
                    Some(idx)
                } else {
                    None
                }
            });
        }

        if let Some(slots) = self.caches.aggregate_member_slots.get(type_name) {
            if let Some(idx) = slots.get(name).copied() {
                return Some(idx);
            }
            if let Some(short) = short_name
                && let Some(idx) = slots.get(short).copied()
            {
                return Some(idx);
            }
        }

        let idx = map.0.0.iter().enumerate().find_map(|(idx, (field, _))| {
            if field == name || short_name.is_some_and(|short| field == short) {
                Some(idx)
            } else {
                None
            }
        })?;

        let slots = self
            .caches
            .aggregate_member_slots
            .entry(type_name.to_string())
            .or_default();
        slots.insert(name.to_string(), idx);
        if let Some(short) = short_name {
            let _ = slots.entry(short.to_string()).or_insert(idx);
        }
        Some(idx)
    }

    fn resolve_var_name(&self, name: &str) -> VarName {
        if self.get_function(name).is_some() {
            return VarName::Func(name.to_string());
        }
        if self.variables.contains_key(name) {
            return VarName::Var(name.to_string());
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
