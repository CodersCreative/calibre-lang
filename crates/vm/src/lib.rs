use crate::{
    config::VMConfig,
    conversion::{Reg, VMBlock, VMFunction, VMRegistry},
    error::RuntimeError,
    native::NativeFunction,
    value::{GcMap, GcVec, RuntimeValue, WaitGroupInner},
    variables::VariableStore,
};
use astro_float::Consts;
use calibre_lir::ast::BlockId;
use dumpster::sync::Gc;
use rustc_hash::{FxHashMap, FxHashSet};
use std::{
    fmt::Debug,
    sync::{
        Arc,
        atomic::{AtomicBool, Ordering},
    },
};
use std::{fmt::Display, sync::OnceLock};
use tracing::instrument;

static NULL_RUNTIME_VALUE: RuntimeValue = RuntimeValue::Null;
static EMPTY_FRAME: OnceLock<VMFrame> = OnceLock::new();
static EMPTY_CAPTURES: OnceLock<Arc<Vec<(String, RuntimeValue)>>> = OnceLock::new();

pub mod config;
pub mod conversion;
pub mod error;
pub mod evaluate;
pub mod native;
pub mod scheduler;
pub mod serialization;
pub mod value;
pub mod variables;
mod vm_lookup;

pub(crate) use vm_lookup::VarName;

#[derive(Debug, Clone)]
pub struct VMFrame {
    pub reg_start: usize,
    pub reg_count: usize,
    pub member_sources: FxHashMap<Reg, (Reg, String)>,
    pub func_ptr: usize,
    pub func_name: Option<String>,
    pub acc: RuntimeValue,
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
            member_sources: FxHashMap::default(),
            func_ptr: 0,
            func_name: None,
            acc: RuntimeValue::Null,
        }
    }
}

#[derive(Debug)]
pub struct VM {
    pub variables: VariableStore,
    pub registry: Arc<VMRegistry>,
    pub mappings: Arc<Vec<String>>,
    pub program_args: Arc<Vec<String>>,
    pub counter: u64,
    pub ptr_heap: FxHashMap<u64, RuntimeValue>,
    pub config: VMConfig,
    source_file_override: Option<Arc<String>>,
    reg_arena: Vec<RuntimeValue>,
    reg_top: usize,
    pub frames: Vec<VMFrame>,
    frame_pool: Vec<VMFrame>,
    caches: VMCaches,
    gc: VMGC,
    scheduler: Option<scheduler::SchedulerHandle>,
    task_state: TaskState,
    pub(crate) moved_functions: FxHashSet<String>,
    pub suppress_output: bool,
    pub captured_output: String,
    pub big_consts: Consts,
}

impl Clone for VM {
    fn clone(&self) -> Self {
        VM {
            variables: self.variables.clone(),
            registry: self.registry.clone(),
            mappings: self.mappings.clone(),
            program_args: self.program_args.clone(),
            counter: self.counter,
            ptr_heap: self.ptr_heap.clone(),
            config: self.config.clone(),
            source_file_override: self.source_file_override.clone(),
            reg_arena: self.reg_arena.clone(),
            reg_top: self.reg_top,
            frames: self.frames.clone(),
            frame_pool: self.frame_pool.clone(),
            caches: self.caches.clone(),
            gc: self.gc.clone(),
            scheduler: self.scheduler.clone(),
            task_state: self.task_state.clone(),
            moved_functions: self.moved_functions.clone(),
            suppress_output: self.suppress_output,
            captured_output: self.captured_output.clone(),
            big_consts: Consts::new().unwrap(),
        }
    }
}

#[derive(Debug, Clone, Default)]
pub struct VMCaches {
    call: FxHashMap<String, Arc<VMFunction>>,
    callsite: FxHashMap<(usize, usize, u32), Arc<VMFunction>>,
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
        Self::from_shared_parts(
            Arc::new(value),
            Arc::new(Vec::new()),
            VMConfig::default(),
            false,
        )
    }
}

impl VM {
    #[inline]
    fn list_identity_eq(a: &Gc<GcVec>, b: &Gc<GcVec>) -> bool {
        std::ptr::eq(a.as_ref(), b.as_ref())
    }

    #[instrument(skip_all)]
    fn replace_list_aliases_in_runtime_value(
        value: &mut RuntimeValue,
        old_list: &Gc<GcVec>,
        new_list: &Gc<GcVec>,
    ) {
        match value {
            RuntimeValue::List(list) => {
                if Self::list_identity_eq(list, old_list) {
                    *list = new_list.clone();
                }
            }
            RuntimeValue::Aggregate(_, map) => {
                let entries = &mut Gc::make_mut(map).0.0;
                for (_, field) in entries.iter_mut() {
                    Self::replace_list_aliases_in_runtime_value(field, old_list, new_list);
                }
            }
            RuntimeValue::Option(Some(inner))
            | RuntimeValue::Result(Ok(inner))
            | RuntimeValue::Result(Err(inner))
            | RuntimeValue::Enum(_, _, Some(inner)) => {
                Self::replace_list_aliases_in_runtime_value(
                    Gc::make_mut(inner),
                    old_list,
                    new_list,
                );
            }
            RuntimeValue::DynObject { value: inner, .. } => {
                Self::replace_list_aliases_in_runtime_value(
                    Gc::make_mut(inner),
                    old_list,
                    new_list,
                );
            }
            _ => {}
        }
    }

    pub(crate) fn propagate_list_aliases(&mut self, old_list: &Gc<GcVec>, new_list: &Gc<GcVec>) {
        let frame_count = self.frames.len();
        for frame_idx in 0..frame_count {
            let reg_count = self.frames[frame_idx].reg_count as u16;
            for reg in 0..reg_count {
                let mut value = self.get_reg_value_in_frame(frame_idx, reg).clone();
                Self::replace_list_aliases_in_runtime_value(&mut value, old_list, new_list);
                self.set_reg_value_in_frame(frame_idx, reg, value);
            }
        }

        let slot_len = self.variables.slot_len();
        for id in 0..slot_len {
            let Some(current) = self.variables.get_by_id(id).cloned() else {
                continue;
            };
            let mut value = current;
            Self::replace_list_aliases_in_runtime_value(&mut value, old_list, new_list);
            let _ = self.variables.set_by_id(id, value);
        }
    }

    fn from_shared_parts(
        registry: Arc<VMRegistry>,
        mappings: Arc<Vec<String>>,
        config: VMConfig,
        install_builtins: bool,
    ) -> Self {
        let mut vm = Self {
            registry,
            mappings,
            program_args: Arc::new(Vec::new()),
            variables: VariableStore::default(),
            counter: 0,
            ptr_heap: FxHashMap::default(),
            config,
            source_file_override: None,
            reg_arena: Vec::new(),
            reg_top: 0,
            frames: vec![VMFrame::default()],
            frame_pool: Vec::new(),
            caches: VMCaches {
                ..VMCaches::default()
            },
            gc: VMGC::default(),
            scheduler: None,
            task_state: TaskState::default(),
            moved_functions: FxHashSet::default(),
            suppress_output: false,
            captured_output: String::new(),
            big_consts: Consts::new().unwrap(),
        };

        if let Some(interval) = vm.config.gc_interval {
            vm.gc.interval = interval;
        }

        vm.preallocate_execution_buffers();
        if install_builtins {
            vm.setup_stdlib();
        }
        vm
    }

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
    pub(crate) fn empty_captures() -> Arc<Vec<(String, RuntimeValue)>> {
        EMPTY_CAPTURES.get_or_init(|| Arc::new(Vec::new())).clone()
    }

    #[inline]
    pub(crate) fn get_function_ref(&self, name: &str) -> Option<&VMFunction> {
        if self.moved_functions.contains(name) {
            return None;
        }
        self.registry.functions.get(name).map(Arc::as_ref)
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

    pub fn new(registry: VMRegistry, mappings: Vec<String>, config: VMConfig) -> Self {
        Self::from_shared_parts(Arc::new(registry), Arc::new(mappings), config, true)
    }

    pub fn new_shared(
        registry: Arc<VMRegistry>,
        mappings: Arc<Vec<String>>,
        config: VMConfig,
    ) -> Self {
        Self::from_shared_parts(registry, mappings, config, true)
    }

    #[instrument(skip_all)]
    pub fn spawn_async_task(
        &mut self,
        mut func: RuntimeValue,
        wait_group: Option<Arc<WaitGroupInner>>,
    ) {
        if self.scheduler.is_none() {
            self.scheduler = Some(scheduler::SchedulerHandle::new(&self.config));
        }
        if let RuntimeValue::Function { name: _, captures } = &mut func {
            let resolved: Vec<(String, RuntimeValue)> = captures
                .as_ref()
                .iter()
                .map(|(key, value)| {
                    let resolved = self
                        .resolve_value_for_op_ref(value)
                        .unwrap_or_else(|_| RuntimeValue::Null);
                    let resolved = self.resolve_saveable_runtime_value_ref(
                        &self.convert_runtime_var_into_saveable(resolved),
                    );
                    (key.clone(), resolved)
                })
                .collect();
            *captures = Arc::new(resolved);
        }
        if let Some(scheduler) = &self.scheduler {
            scheduler.spawn(self, func, wait_group);
        }
    }

    pub fn set_program_args(&mut self, args: Vec<String>) {
        self.program_args = Arc::new(args);
    }

    pub fn program_args(&self) -> &[String] {
        self.program_args.as_ref()
    }

    pub fn set_source_file_override(&mut self, path: &std::path::Path) {
        let path = path.canonicalize().unwrap_or_else(|_| path.to_path_buf());
        self.source_file_override = Some(Arc::new(path.to_string_lossy().to_string()));
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

    fn push_frame(&mut self, reg_count: usize, func_ptr: usize, func_name: Option<String>) {
        let start = self.reg_top;
        self.reg_top = self.reg_top.saturating_add(reg_count);
        if self.reg_top > self.reg_arena.len() {
            self.reg_arena.resize(self.reg_top, RuntimeValue::Null);
        }
        if let Some(mut frame) = self.frame_pool.pop() {
            frame.reg_start = start;
            frame.reg_count = reg_count;
            frame.member_sources.clear();
            frame.func_ptr = func_ptr;
            frame.func_name = func_name;
            frame.acc = RuntimeValue::Null;
            self.frames.push(frame);
        } else {
            self.frames.push(VMFrame {
                reg_start: start,
                reg_count,
                member_sources: FxHashMap::default(),
                func_ptr,
                func_name,
                acc: RuntimeValue::Null,
            });
        }
    }

    fn pop_frame(&mut self) {
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
    #[instrument(skip_all)]
    pub(crate) fn get_reg_value(&self, reg: Reg) -> &RuntimeValue {
        let frame = self.current_frame();
        let idx = reg as usize;
        if idx < frame.reg_count {
            unsafe { self.reg_arena.get_unchecked(frame.reg_start + idx) }
        } else {
            &NULL_RUNTIME_VALUE
        }
    }

    #[inline(always)]
    pub(crate) fn get_reg_value_in_frame(&self, frame_idx: usize, reg: Reg) -> &RuntimeValue {
        if let Some(frame) = self.frames.get(frame_idx) {
            let idx = reg as usize;
            if idx < frame.reg_count {
                return &self.reg_arena[frame.reg_start + idx];
            }
        }
        &NULL_RUNTIME_VALUE
    }

    #[inline(always)]
    pub(crate) fn set_reg_value(&mut self, reg: Reg, value: RuntimeValue) {
        if let RuntimeValue::Null = value {
            let frame = self.current_frame();
            let idx = reg as usize;
            if idx < frame.reg_count && self.reg_arena[frame.reg_start + idx].is_null() {
                return;
            }
        }
        let _ = self.replace_reg_value(reg, value);
        self.current_frame_mut().member_sources.remove(&reg);
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
                frame.member_sources.remove(&reg);
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
    #[instrument(skip_all)]
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

    #[instrument(skip_all)]
    pub(crate) fn resolve_value_for_op_ref(
        &self,
        value: &RuntimeValue,
    ) -> Result<RuntimeValue, RuntimeError> {
        let mut owned: Option<RuntimeValue> = None;
        let mut seen_refs: FxHashSet<String> = FxHashSet::default();
        let mut seen_var_refs: FxHashSet<usize> = FxHashSet::default();
        let mut seen_reg_refs: FxHashSet<(usize, u16)> = FxHashSet::default();

        for _ in 0..64 {
            let (current, from_owned) = match &owned {
                Some(v) => (v, true),
                None => (value, false),
            };

            match current {
                RuntimeValue::Ref(pointer) => {
                    if !seen_refs.insert(pointer.clone()) {
                        return Err(RuntimeError::DanglingRef(format!("ref-cycle({})", pointer)));
                    }

                    let v = if let Some(v) = self.variables.get(pointer).cloned() {
                        if matches!(&v, RuntimeValue::Ref(next) if next == pointer) {
                            return Err(RuntimeError::DanglingRef(pointer.to_string()));
                        } else {
                            v
                        }
                    } else {
                        return Err(RuntimeError::DanglingRef(pointer.to_string()));
                    };
                    owned = Some(v);
                }
                RuntimeValue::VarRef(id) => {
                    if !seen_var_refs.insert(*id) {
                        return Err(RuntimeError::DanglingRef(format!(
                            "varref-cycle(id = #{} name = '{}')",
                            id,
                            self.variables.name_of(*id).unwrap_or_default()
                        )));
                    }

                    let v = self
                        .variables
                        .get_by_id(*id)
                        .cloned()
                        .ok_or(RuntimeError::DanglingRef(format!("#{}", id)))?;

                    if matches!(&v, RuntimeValue::VarRef(next) if next == id) {
                        if let Some(name) = self.variables.name_of(*id)
                            && let Ok(local) =
                                self.resolve_value_for_op_ref(&RuntimeValue::Ref(name.to_string()))
                            && !matches!(&local, RuntimeValue::VarRef(next) if next == id)
                        {
                            owned = Some(local);
                            continue;
                        }
                        return Err(RuntimeError::DanglingRef(format!("#{}", id)));
                    }

                    owned = Some(v);
                }
                RuntimeValue::RegRef { frame, reg } => {
                    if !seen_reg_refs.insert((*frame, *reg)) {
                        return Err(RuntimeError::DanglingRef(format!(
                            "regref-cycle({}:{})",
                            frame, reg
                        )));
                    }
                    owned = Some(self.get_reg_value_in_frame(*frame, *reg).clone());
                }
                RuntimeValue::MutexGuard(guard) => {
                    owned = Some(guard.get_clone());
                }
                _ => {
                    if from_owned {
                        if let Some(value) = owned.take() {
                            return Ok(value);
                        }
                        return Err(RuntimeError::DanglingRef(String::from("<owned-missing>")));
                    }
                    return Ok(current.clone());
                }
            }
        }

        Err(RuntimeError::DanglingRef(String::from("<ref-depth-limit>")))
    }

    fn drop_runtime_value(&mut self, value: RuntimeValue) {
        let mut seen = FxHashSet::default();
        let mut seen_regs = FxHashSet::default();
        self.drop_runtime_value_inner_ref(&value, &mut seen, &mut seen_regs);
    }

    fn drop_runtime_value_inner_ref(
        &mut self,
        value: &RuntimeValue,
        seen: &mut FxHashSet<String>,
        seen_regs: &mut FxHashSet<usize>,
    ) {
        let _ = self.call_trait_for_type(value, "drop", Vec::new(), Some(0));

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
                let inner = self.get_reg_value_in_frame(*frame, *reg).clone();
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
                if let Ok(guard) = map.try_lock() {
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
            RuntimeValue::Enum(_, _, Some(val)) => {
                self.drop_runtime_value_inner_ref(val.as_ref(), seen, seen_regs);
            }
            RuntimeValue::Generator { .. } => {}
            RuntimeValue::Channel(ch) => {
                if let Ok(mut queue) = ch.queue.try_lock() {
                    while let Some(item) = queue.pop_front() {
                        self.drop_runtime_value_inner_ref(&item, seen, seen_regs);
                    }
                }
            }
            _ => {}
        }
    }

    // TODO Make an impl_name function for RuntimeValue
    pub fn call_trait_for_type(
        &mut self,
        value: &RuntimeValue,
        method: impl Display,
        mut args: Vec<RuntimeValue>,
        value_pos: Option<usize>,
    ) -> Result<RuntimeValue, RuntimeError> {
        let type_name = match value {
            RuntimeValue::Aggregate(Some(x), _) | RuntimeValue::Enum(x, _, _) => x.as_str(),
            _ => return Ok(RuntimeValue::Null),
        };

        let drop_method_name = format!("{type_name}.{method}",);

        if let Some(_drop_func) = self.registry.functions.get(&drop_method_name) {
            if let Some(x) = value_pos {
                args.insert(x, value.clone());
            }

            self.call_runtime_callable_at(
                RuntimeValue::Function {
                    name: Arc::new(drop_method_name.clone()),
                    captures: Arc::new(Vec::new()),
                },
                args,
                0,
                0,
            )
        } else {
            Err(RuntimeError::InvalidFunctionCall)
        }
    }
}
