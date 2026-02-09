use crate::{
    config::VMConfig,
    conversion::{VMBlock, VMFunction, VMInstruction, VMRegistry},
    error::RuntimeError,
    value::RuntimeValue,
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
pub mod serialization;
pub mod value;

pub struct VMStack {
    items: Vec<RuntimeValue>,
    top: Option<RuntimeValue>,
}

impl VMStack {
    fn new() -> Self {
        Self {
            items: Vec::new(),
            top: None,
        }
    }

    fn len(&self) -> usize {
        self.items.len() + if self.top.is_some() { 1 } else { 0 }
    }

    fn push(&mut self, value: RuntimeValue) {
        if let Some(top) = self.top.take() {
            self.items.push(top);
        }
        self.top = Some(value);
    }

    fn pop(&mut self) -> Option<RuntimeValue> {
        if let Some(top) = self.top.take() {
            return Some(top);
        }
        self.items.pop()
    }

    fn pop_n(&mut self, count: usize) -> Option<Vec<RuntimeValue>> {
        if self.len() < count {
            return None;
        }
        let mut values = Vec::with_capacity(count);
        for _ in 0..count {
            values.push(self.pop()?);
        }
        values.reverse();
        Some(values)
    }
}

#[derive(Debug, Clone, Default)]
struct VMFrame {
    slots: Vec<RuntimeValue>,
    slot_map: FxHashMap<String, usize>,
    slot_names: Vec<String>,
}

#[derive(Copy, Clone, Debug)]
enum ResolvedName {
    Slot(usize),
    GlobalVar,
    Func,
    Unknown,
}

#[derive(Debug, Clone, Default)]
struct ResolvedFunctionCache {
    slots_len: usize,
    param_slots: FxHashMap<String, usize>,
    slot_names: Vec<String>,
    block_resolved: FxHashMap<BlockId, Vec<ResolvedName>>,
}

#[derive(Debug, Clone, Default)]
pub struct VM {
    pub variables: FxHashMap<String, RuntimeValue>,
    pub registry: VMRegistry,
    pub mappings: Vec<String>,
    pub counter: u64,
    pub ptr_heap: FxHashMap<u64, RuntimeValue>,
    frames: Vec<VMFrame>,
    caches: VMCaches,
    gc: VMGC,
}

#[derive(Debug, Clone, Default)]
pub struct VMCaches {
    call: FxHashMap<String, Arc<VMFunction>>,
    lowered: FxHashMap<String, Arc<VMFunction>>,
    resolved: FxHashMap<String, Arc<ResolvedFunctionCache>>,
    current_resolved: Option<Arc<ResolvedFunctionCache>>,
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
        Self {
            variables: FxHashMap::default(),
            mappings: Vec::new(),
            registry: value,
            counter: 0,
            ptr_heap: FxHashMap::default(),
            frames: vec![VMFrame::default()],
            caches: VMCaches::default(),
            gc: VMGC::default(),
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

    fn find_unique_slot_by_suffix(&self, short_name: &str) -> Option<usize> {
        let suffix = format!(":{}", short_name);
        for (name, slot) in self.current_frame().slot_map.iter() {
            if name.ends_with(&suffix) {
                return Some(*slot);
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
        match self.resolve_var_name(name) {
            VarName::Var(var) => self
                .variables
                .get(&var)
                .cloned()
                .map(|v| self.copy_saveable_into_runtime_var(v))
                .unwrap_or(RuntimeValue::Null),
            VarName::Slot(slot) => self.get_slot_value(slot),
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
        let mut vm = Self {
            registry,
            mappings,
            variables: FxHashMap::default(),
            counter: 0,
            ptr_heap: FxHashMap::default(),
            frames: vec![VMFrame::default()],
            caches: VMCaches::default(),
            gc: VMGC::default(),
        };

        if let Some(interval) = config.gc_interval {
            vm.gc.interval = interval;
        }

        vm.setup_global();
        vm.setup_stdlib();

        vm
    }

    pub fn get_ref_id(&mut self) -> u64 {
        let id = self.counter;
        self.counter = self.counter.wrapping_add(1).max(1);
        id
    }

    fn push_frame(&mut self) {
        self.frames.push(VMFrame::default());
    }

    fn pop_frame(&mut self) {
        if self.frames.len() <= 1 {
            let _ = self.frames.pop();
            return;
        }

        if let Some(frame) = self.frames.pop() {
            for name in frame.slot_names {
                let _ = self.variables.remove(&name);
            }
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

    fn build_resolved_cache(&self, function: &VMFunction) -> ResolvedFunctionCache {
        let mut slot_map: FxHashMap<String, usize> = FxHashMap::default();
        let captures: FxHashSet<String> = function.captures.iter().cloned().collect();
        let mut slot_names: Vec<String> = Vec::new();
        let mut block_resolved: FxHashMap<BlockId, Vec<ResolvedName>> = FxHashMap::default();

        for (i, param) in function.params.iter().enumerate() {
            slot_map.insert(param.clone(), i);
            if slot_names.len() <= i {
                slot_names.resize(i + 1, String::new());
            }
            slot_names[i] = param.clone();
        }

        let mut next_slot = slot_map.len();

        for block in &function.blocks {
            for instr in &block.instructions {
                match instr {
                    VMInstruction::DeclareVar(idx) | VMInstruction::SetVar(idx) => {
                        if let Some(name) = block.local_strings.get(*idx as usize) {
                            if captures.contains(name) {
                                continue;
                            }
                            if !slot_map.contains_key(name) {
                                slot_map.insert(name.clone(), next_slot);
                                if slot_names.len() <= next_slot {
                                    slot_names.resize(next_slot + 1, String::new());
                                }
                                slot_names[next_slot] = name.clone();
                                next_slot += 1;
                            }
                        }
                    }
                    _ => {}
                }
            }

            let mut resolved = Vec::with_capacity(block.local_strings.len());

            for name in &block.local_strings {
                let kind = if captures.contains(name) {
                    ResolvedName::GlobalVar
                } else if let Some(slot) = slot_map.get(name).copied() {
                    ResolvedName::Slot(slot)
                } else if self.variables.contains_key(name) {
                    ResolvedName::GlobalVar
                } else if self.registry.functions.contains_key(name) {
                    ResolvedName::Func
                } else {
                    ResolvedName::Unknown
                };
                resolved.push(kind);
            }

            block_resolved.insert(block.id, resolved);
        }

        ResolvedFunctionCache {
            slots_len: next_slot,
            param_slots: slot_map,
            slot_names,
            block_resolved,
        }
    }

    fn lowered_function(&mut self, function: &VMFunction) -> Arc<VMFunction> {
        if let Some(existing) = self.caches.lowered.get(&function.name) {
            return existing.clone();
        }
        let lowered = std::sync::Arc::new(function.lower_slots());
        self.caches
            .lowered
            .insert(function.name.clone(), lowered.clone());
        lowered
    }

    fn resolved_name_for(&self, block: &VMBlock, idx: u8) -> Option<ResolvedName> {
        self.caches
            .current_resolved
            .as_ref()
            .and_then(|cache| cache.block_resolved.get(&block.id))
            .and_then(|v| v.get(idx as usize).copied())
    }

    #[inline]
    fn local_string<'a>(&self, block: &'a VMBlock, idx: u8) -> Result<&'a str, RuntimeError> {
        let idx = idx as usize;
        if idx >= block.local_strings.len() {
            return Err(RuntimeError::InvalidBytecode(format!(
                "missing string {}",
                idx
            )));
        }
        Ok(unsafe { block.local_strings.get_unchecked(idx).as_str() })
    }

    #[inline]
    fn local_string_owned<'a>(
        &self,
        block: &'a VMBlock,
        idx: u8,
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

    fn ensure_slot(&mut self, name: &str) -> usize {
        let frame = self.current_frame_mut();
        if let Some(slot) = frame.slot_map.get(name) {
            return *slot;
        }
        let slot = frame.slots.len();
        frame.slots.push(RuntimeValue::Null);
        frame.slot_names.push(name.to_string());
        frame.slot_map.insert(name.to_string(), slot);
        slot
    }

    pub(crate) fn get_slot_value(&self, slot: usize) -> RuntimeValue {
        self.current_frame()
            .slots
            .get(slot)
            .cloned()
            .unwrap_or(RuntimeValue::Null)
    }

    pub(crate) fn set_slot_value(&mut self, slot: usize, value: RuntimeValue) {
        let _ = self.replace_slot_value(slot, value);
    }

    pub(crate) fn replace_slot_value(&mut self, slot: usize, value: RuntimeValue) -> RuntimeValue {
        let (name, stored, old) = {
            let frame = self.current_frame_mut();
            if slot >= frame.slots.len() {
                frame.slots.resize(slot + 1, RuntimeValue::Null);
                frame.slot_names.resize(slot + 1, String::from("<slot>"));
            }
            let old = frame.slots[slot].clone();
            frame.slots[slot] = value;
            let name = frame.slot_names.get(slot).cloned();
            let stored = frame.slots[slot].clone();
            (name, stored, old)
        };
        if let Some(name) = name {
            self.variables.insert(name, stored);
        }
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

    pub(crate) fn drop_runtime_value(&mut self, value: RuntimeValue) {
        let mut seen = FxHashSet::default();
        let mut seen_slots = FxHashSet::default();
        self.drop_runtime_value_inner(value, &mut seen, &mut seen_slots);
    }

    fn drop_runtime_value_inner(
        &mut self,
        value: RuntimeValue,
        seen: &mut FxHashSet<String>,
        seen_slots: &mut FxHashSet<usize>,
    ) {
        match value {
            RuntimeValue::Ref(name) => {
                if !seen.insert(name.clone()) {
                    return;
                }
                if let Some(inner) = self.variables.remove(&name) {
                    self.drop_runtime_value_inner(inner, seen, seen_slots);
                }
            }
            RuntimeValue::SlotRef(slot) => {
                if !seen_slots.insert(slot) {
                    return;
                }
                let inner = self.get_slot_value(slot);
                self.set_slot_value(slot, RuntimeValue::Null);
                self.drop_runtime_value_inner(inner, seen, seen_slots);
            }
            RuntimeValue::Aggregate(_, map) => {
                for (_, value) in map.as_ref().0.0.iter().cloned() {
                    self.drop_runtime_value_inner(value, seen, seen_slots);
                }
            }
            RuntimeValue::List(list) => {
                for value in list.as_ref().0.iter().cloned() {
                    self.drop_runtime_value_inner(value, seen, seen_slots);
                }
            }
            RuntimeValue::Enum(_, _, Some(value)) => {
                self.drop_runtime_value_inner(value.as_ref().clone(), seen, seen_slots);
            }
            RuntimeValue::Option(Some(value)) => {
                self.drop_runtime_value_inner(value.as_ref().clone(), seen, seen_slots);
            }
            RuntimeValue::Result(Ok(value)) => {
                self.drop_runtime_value_inner(value.as_ref().clone(), seen, seen_slots);
            }
            RuntimeValue::Result(Err(value)) => {
                self.drop_runtime_value_inner(value.as_ref().clone(), seen, seen_slots);
            }
            RuntimeValue::Function { captures, .. } => {
                for (_, value) in captures {
                    self.drop_runtime_value_inner(value, seen, seen_slots);
                }
            }
            _ => {}
        }
    }

    fn resolve_value_for_op(&self, value: RuntimeValue) -> Result<RuntimeValue, RuntimeError> {
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
                RuntimeValue::SlotRef(slot) => {
                    current = self.get_slot_value(slot);
                }
                other => return Ok(other),
            }
        }

        Err(RuntimeError::DanglingRef("<ref-depth-limit>".to_string()))
    }
}

pub enum VarName {
    Func(String),
    Var(String),
    Slot(usize),
    Global,
}
