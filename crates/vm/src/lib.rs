use crate::{
    config::VMConfig,
    conversion::{Reg, VMBlock, VMFunction, VMRegistry},
    error::RuntimeError,
    native::NativeFunction,
    value::{ExternFunction, GcMap, RuntimeValue, WaitGroupInner},
    variables::VariableStore,
};
use calibre_lir::BlockId;
use dumpster::sync::Gc;
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
mod vm_lookup;

pub(crate) use vm_lookup::VarName;

#[derive(Debug, Clone)]
struct VMFrame {
    reg_start: usize,
    reg_count: usize,
    local_map_base: Option<Arc<FxHashMap<Arc<str>, Reg>>>,
    local_map: FxHashMap<Arc<str>, Reg>,
    member_sources: FxHashMap<Reg, (Reg, String)>,
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
            member_sources: FxHashMap::default(),
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
    pub program_args: Arc<Vec<String>>,
    pub counter: u64,
    pub ptr_heap: FxHashMap<u64, RuntimeValue>,
    pub config: VMConfig,
    source_file_override: Option<Arc<String>>,
    reg_arena: Vec<RuntimeValue>,
    reg_top: usize,
    name_arena: FxHashMap<Arc<str>, Arc<str>>,
    frames: Vec<VMFrame>,
    frame_pool: Vec<VMFrame>,
    caches: VMCaches,
    func_suffix: FxHashMap<String, Option<Arc<VMFunction>>>,
    gc: VMGC,
    scheduler: Option<scheduler::SchedulerHandle>,
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
    fn list_identity_eq(a: &Gc<crate::value::GcVec>, b: &Gc<crate::value::GcVec>) -> bool {
        std::ptr::eq(a.as_ref(), b.as_ref())
    }

    fn replace_list_aliases_in_runtime_value(
        value: &mut RuntimeValue,
        old_list: &Gc<crate::value::GcVec>,
        new_list: &Gc<crate::value::GcVec>,
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

    pub(crate) fn propagate_list_aliases(
        &mut self,
        old_list: &Gc<crate::value::GcVec>,
        new_list: &Gc<crate::value::GcVec>,
    ) {
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

    #[inline]
    pub(crate) fn is_magic_binding_name(name: &str, binding: &str) -> bool {
        name == binding || name.ends_with(&format!(":{binding}"))
    }

    #[inline]
    pub(crate) fn is_magic_file_binding(name: &str) -> bool {
        Self::is_magic_binding_name(name, "__file__")
    }

    fn from_shared_parts(
        registry: Arc<VMRegistry>,
        mappings: Arc<Vec<String>>,
        config: VMConfig,
        install_builtins: bool,
    ) -> Self {
        let func_suffix = build_function_suffix_index(registry.as_ref());
        let globals_direct = build_globals_direct_cache(registry.as_ref());
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
            name_arena: FxHashMap::default(),
            frames: vec![VMFrame::default()],
            frame_pool: Vec::new(),
            caches: VMCaches {
                globals_direct,
                ..VMCaches::default()
            },
            func_suffix,
            gc: VMGC::default(),
            scheduler: None,
            task_state: TaskState::default(),
            moved_functions: FxHashSet::default(),
            suppress_output: false,
            captured_output: String::new(),
        };

        if let Some(interval) = vm.config.gc_interval {
            vm.gc.interval = interval;
        }

        vm.preallocate_execution_buffers();
        if install_builtins {
            vm.setup_global();
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

    pub fn spawn_async_task(
        &mut self,
        func: RuntimeValue,
        wait_group: Option<Arc<WaitGroupInner>>,
    ) {
        if self.scheduler.is_none() {
            self.scheduler = Some(scheduler::SchedulerHandle::new(&self.config));
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

    pub fn normalize_magic_file_bindings(&mut self, path: &std::path::Path) {
        let path = path.canonicalize().unwrap_or_else(|_| path.to_path_buf());
        let value = RuntimeValue::Str(Arc::new(path.to_string_lossy().to_string()));
        let keys: Vec<String> = self
            .variables
            .keys()
            .filter(|name| Self::is_magic_file_binding(name))
            .map(ToString::to_string)
            .collect();
        for key in keys {
            let _ = self.variables.insert(&key, value.clone());
        }
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
            frame.member_sources.clear();
            frame.func_ptr = func_ptr;
            frame.acc = RuntimeValue::Null;
            self.frames.push(frame);
        } else {
            self.frames.push(VMFrame {
                reg_start: start,
                reg_count,
                local_map_base: None,
                local_map: FxHashMap::default(),
                member_sources: FxHashMap::default(),
                func_ptr,
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
            if idx < frame.reg_count {
                if let RuntimeValue::Null = self.reg_arena[frame.reg_start + idx] {
                    return;
                }
            }
        }
        let _ = self.replace_reg_value(reg, value);
        self.current_frame_mut().member_sources.remove(&reg);
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

    pub(crate) fn resolve_value_for_op_ref(
        &self,
        value: &RuntimeValue,
    ) -> Result<RuntimeValue, RuntimeError> {
        let resolve_local_ref = |pointer: &str| -> Option<RuntimeValue> {
            for (frame_idx, frame) in self.frames.iter().enumerate().rev() {
                if let Some(reg) = frame.local_map.get(pointer) {
                    let value = self.get_reg_value_in_frame(frame_idx, *reg).clone();
                    if !matches!(&value, RuntimeValue::Ref(next) if next == pointer) {
                        return Some(value);
                    }
                }
                if let Some(base) = frame.local_map_base.as_ref()
                    && let Some(reg) = base.get(pointer)
                {
                    let value = self.get_reg_value_in_frame(frame_idx, *reg).clone();
                    if !matches!(&value, RuntimeValue::Ref(next) if next == pointer) {
                        return Some(value);
                    }
                }
            }
            None
        };

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
                    let local_style = pointer.contains(':') || pointer.contains("->");
                    let v = if local_style {
                        if let Some(local) = resolve_local_ref(pointer) {
                            local
                        } else if let Some(v) = self.variables.get(pointer).cloned() {
                            if matches!(&v, RuntimeValue::Ref(next) if next == pointer) {
                                return Err(RuntimeError::DanglingRef(pointer.to_string()));
                            }
                            v
                        } else {
                            return Err(RuntimeError::DanglingRef(pointer.to_string()));
                        }
                    } else if let Some(v) = self.variables.get(pointer).cloned() {
                        if matches!(&v, RuntimeValue::Ref(next) if next == pointer) {
                            if let Some(local) = resolve_local_ref(pointer) {
                                local
                            } else {
                                return Err(RuntimeError::DanglingRef(pointer.to_string()));
                            }
                        } else {
                            v
                        }
                    } else if let Some(v) = resolve_local_ref(pointer) {
                        v
                    } else {
                        return Err(RuntimeError::DanglingRef(pointer.to_string()));
                    };
                    owned = Some(v);
                }
                RuntimeValue::VarRef(id) => {
                    if !seen_var_refs.insert(*id) {
                        return Err(RuntimeError::DanglingRef(format!("varref-cycle(#{} )", id)));
                    }
                    let v = self
                        .variables
                        .get_by_id(*id)
                        .cloned()
                        .ok_or(RuntimeError::DanglingRef(format!("#{}", id)))?;
                    if matches!(&v, RuntimeValue::VarRef(next) if next == id) {
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
        let short = calibre_parser::qualified_name_tail(name);
        if short == name {
            continue;
        }
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
    for name in registry.functions.keys() {
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
