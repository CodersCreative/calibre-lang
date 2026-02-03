use calibre_parser::ast::ObjectMap;
use rustc_hash::FxHashMap;

use crate::{
    conversion::{VMBlock, VMFunction, VMGlobal, VMInstruction, VMRegistry},
    error::RuntimeError,
    value::{
        RuntimeValue, TerminateValue,
        operation::{binary, boolean, comparison},
    },
};
use calibre_parser::ast::binary::BinaryOperator;
use calibre_parser::ast::comparison::{BooleanOperator, ComparisonOperator};
use dumpster::sync::Gc;

pub mod conversion;
pub mod error;
pub mod native;
pub mod serde_fxhashmap;
pub mod serde_fxhashmap_rc;
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
    block_resolved: FxHashMap<calibre_lir::BlockId, Vec<ResolvedName>>,
}

#[derive(Debug, Clone, Default)]
pub struct VM {
    pub variables: FxHashMap<String, RuntimeValue>,
    pub registry: VMRegistry,
    pub mappings: Vec<String>,
    pub next_ref_id: u64,
    frames: Vec<VMFrame>,
    call_cache: FxHashMap<String, std::sync::Arc<VMFunction>>,
    resolved_cache: FxHashMap<String, std::sync::Arc<ResolvedFunctionCache>>,
    current_resolved: Option<std::sync::Arc<ResolvedFunctionCache>>,
}

impl From<VMRegistry> for VM {
    fn from(value: VMRegistry) -> Self {
        Self {
            variables: FxHashMap::default(),
            mappings: Vec::new(),
            registry: value,
            next_ref_id: 0,
            frames: vec![VMFrame::default()],
            call_cache: FxHashMap::default(),
            resolved_cache: FxHashMap::default(),
            current_resolved: None,
        }
    }
}

impl VM {
    pub fn new(registry: VMRegistry, mappings: Vec<String>) -> Self {
        let mut vm = Self {
            registry,
            mappings,
            variables: FxHashMap::default(),
            next_ref_id: 0,
            frames: vec![VMFrame::default()],
            call_cache: FxHashMap::default(),
            resolved_cache: FxHashMap::default(),
            current_resolved: None,
        };

        vm.setup_global();
        vm.setup_stdlib();

        vm
    }

    pub fn alloc_ref_id(&mut self) -> String {
        let id = self.next_ref_id;
        self.next_ref_id = self.next_ref_id.wrapping_add(1);
        id.to_string()
    }

    fn push_frame(&mut self) {
        self.frames.push(VMFrame::default());
    }

    fn pop_frame(&mut self) {
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
        let captures: std::collections::HashSet<String> =
            function.captures.iter().cloned().collect();
        let mut slot_names: Vec<String> = Vec::new();
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
        }

        let mut block_resolved: FxHashMap<calibre_lir::BlockId, Vec<ResolvedName>> =
            FxHashMap::default();
        for block in &function.blocks {
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

    fn resolved_name_for(&self, block: &VMBlock, idx: u8) -> Option<ResolvedName> {
        self.current_resolved
            .as_ref()
            .and_then(|cache| cache.block_resolved.get(&block.id))
            .and_then(|v| v.get(idx as usize).copied())
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
        let (name, stored) = {
            let frame = self.current_frame_mut();
            if slot >= frame.slots.len() {
                frame.slots.resize(slot + 1, RuntimeValue::Null);
                frame.slot_names.resize(slot + 1, String::from("<slot>"));
            }
            frame.slots[slot] = value;
            let name = frame.slot_names.get(slot).cloned();
            let stored = frame.slots[slot].clone();
            (name, stored)
        };
        if let Some(name) = name {
            self.variables.insert(name, stored);
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

impl VM {
    pub fn run(
        &mut self,
        function: &VMFunction,
        args: Vec<RuntimeValue>,
    ) -> Result<RuntimeValue, RuntimeError> {
        let _ = self.run_globals()?;

        self.run_function(function, args)
    }

    pub fn run_globals(&mut self) -> Result<(), RuntimeError> {
        for global in self.registry.globals.clone().into_iter() {
            let _ = self.run_global(&global.1)?;
        }

        Ok(())
    }

    pub fn run_global(&mut self, global: &VMGlobal) -> Result<RuntimeValue, RuntimeError> {
        let mut stack = VMStack::new();
        let mut block = global
            .blocks
            .first()
            .ok_or_else(|| RuntimeError::InvalidBytecode("global has no blocks".to_string()))?;

        loop {
            match self.run_block(block, &mut stack)? {
                TerminateValue::Jump(x) => {
                    block = global.blocks.get(x.0 as usize).ok_or_else(|| {
                        RuntimeError::InvalidBytecode(format!("invalid global block {}", x.0))
                    })?
                }
                TerminateValue::Return(x) => match x {
                    RuntimeValue::Null => break,
                    x => return Ok(x),
                },
                TerminateValue::None => break,
            }
        }

        loop {
            match stack.pop() {
                None => break,
                Some(RuntimeValue::Null) => {}
                Some(x) => return Ok(x),
            }
        }

        Ok(RuntimeValue::Null)
    }

    pub fn run_function(
        &mut self,
        function: &VMFunction,
        mut args: Vec<RuntimeValue>,
    ) -> Result<RuntimeValue, RuntimeError> {
        let cache = if let Some(existing) = self.resolved_cache.get(&function.name) {
            existing.clone()
        } else {
            let built = std::sync::Arc::new(self.build_resolved_cache(function));
            self.resolved_cache
                .insert(function.name.clone(), built.clone());
            built
        };
        let prev_resolved = self.current_resolved.take();
        self.current_resolved = Some(cache.clone());
        self.push_frame();
        let result = (|| {
            let mut stack = VMStack::new();
            let mut block = function.blocks.first().ok_or_else(|| {
                RuntimeError::InvalidBytecode("function has no blocks".to_string())
            })?;

            let mut arg_iter = args.drain(..);
            self.current_frame_mut()
                .slots
                .resize(cache.slots_len, RuntimeValue::Null);
            self.current_frame_mut().slot_map = cache.param_slots.clone();
            self.current_frame_mut()
                .slot_names
                .resize(cache.slots_len, String::new());
            for (idx, name) in cache.slot_names.iter().enumerate() {
                if let Some(slot_name) = self.current_frame_mut().slot_names.get_mut(idx) {
                    *slot_name = name.clone();
                }
            }
            for param in function.params.iter() {
                let arg = arg_iter.next().unwrap_or(RuntimeValue::Null);
                if let Some(slot) = cache.param_slots.get(param) {
                    self.set_slot_value(*slot, arg);
                } else {
                    let slot = self.ensure_slot(param);
                    self.set_slot_value(slot, arg);
                }
            }

            let mut result = RuntimeValue::Null;
            let mut returned = false;
            loop {
                match self.run_block(block, &mut stack)? {
                    TerminateValue::Jump(x) => {
                        block = function.blocks.get(x.0 as usize).ok_or_else(|| {
                            RuntimeError::InvalidBytecode(format!("invalid function block {}", x.0))
                        })?
                    }
                    TerminateValue::Return(x) => match x {
                        RuntimeValue::Null if function.returns_value => {
                            returned = true;
                            break;
                        }
                        x => {
                            result = x;
                            returned = true;
                            break;
                        }
                    },
                    TerminateValue::None => break,
                }
            }
            if function.returns_value {
                loop {
                    match stack.pop() {
                        None => break,
                        Some(RuntimeValue::Null) => {}
                        Some(x) => {
                            result = x;
                            returned = true;
                            break;
                        }
                    }
                }
            }
            if returned {
                Ok(result)
            } else {
                Ok(RuntimeValue::Null)
            }
        })();
        self.pop_frame();
        self.current_resolved = prev_resolved;
        result
    }

    pub fn resolve_var_name(&self, name: &str) -> VarName {
        if let Some(slot) = self.current_frame().slot_map.get(name) {
            return VarName::Slot(*slot);
        }
        if self.variables.contains_key(name) {
            return VarName::Var(name.to_string());
        } else if self.registry.functions.contains_key(name) {
            return VarName::Func(name.to_string());
        } else if let Some((prefix, _)) = name.split_once("->") {
            if let Some(slot) = self.current_frame().slot_map.get(prefix) {
                return VarName::Slot(*slot);
            } else if self.variables.contains_key(prefix) {
                return VarName::Var(prefix.to_string());
            } else if self.registry.functions.contains_key(prefix) {
                return VarName::Func(prefix.to_string());
            } else {
                // TODO handle globals
            }
        } else {
            // TODO handle globals
        }

        VarName::Global
    }

    pub fn run_block(
        &mut self,
        block: &VMBlock,
        stack: &mut VMStack,
    ) -> Result<TerminateValue, RuntimeError> {
        for (ip, instruction) in block.instructions.iter().enumerate() {
            let span = block.instruction_spans.get(ip).cloned().unwrap_or_default();

            let step = self
                .run_instruction(instruction, block, stack)
                .map_err(|e| RuntimeError::At(span, Box::new(e)))?;

            match step {
                TerminateValue::None => {}
                x => return Ok(x),
            }
        }

        Ok(TerminateValue::None)
    }

    fn run_instruction(
        &mut self,
        instruction: &VMInstruction,
        block: &VMBlock,
        stack: &mut VMStack,
    ) -> Result<TerminateValue, RuntimeError> {
        match instruction {
            VMInstruction::Binary(x) => {
                let right =
                    self.resolve_value_for_op(stack.pop().ok_or(RuntimeError::StackUnderflow)?)?;
                let left =
                    self.resolve_value_for_op(stack.pop().ok_or(RuntimeError::StackUnderflow)?)?;

                stack.push(binary(x, left, right)?);
            }
            VMInstruction::Comparison(x) => {
                let right =
                    self.resolve_value_for_op(stack.pop().ok_or(RuntimeError::StackUnderflow)?)?;
                let left =
                    self.resolve_value_for_op(stack.pop().ok_or(RuntimeError::StackUnderflow)?)?;

                stack.push(comparison(x, left, right)?);
            }
            VMInstruction::Boolean(x) => {
                let right =
                    self.resolve_value_for_op(stack.pop().ok_or(RuntimeError::StackUnderflow)?)?;
                let left =
                    self.resolve_value_for_op(stack.pop().ok_or(RuntimeError::StackUnderflow)?)?;

                stack.push(boolean(x, left, right)?);
            }
            VMInstruction::Jump(x) => return Ok(TerminateValue::Jump(*x)),
            VMInstruction::Branch(x, y) => {
                let RuntimeValue::Bool(comparison) =
                    stack.pop().ok_or(RuntimeError::StackUnderflow)?
                else {
                    return Err(RuntimeError::UnexpectedType(RuntimeValue::Null));
                };

                if comparison {
                    return Ok(TerminateValue::Jump(*x));
                } else {
                    return Ok(TerminateValue::Jump(*y));
                }
            }
            VMInstruction::Return(x) => {
                let value = if *x {
                    stack.pop().ok_or(RuntimeError::StackUnderflow)?
                } else {
                    RuntimeValue::Null
                };
                return Ok(TerminateValue::Return(value));
            }
            VMInstruction::LoadLiteral(x) => {
                stack.push(RuntimeValue::from(
                    block
                        .local_literals
                        .get(*x as usize)
                        .ok_or(RuntimeError::StackUnderflow)?
                        .clone(),
                ));
            }
            VMInstruction::Drop(x) => {
                let name = block
                    .local_strings
                    .get(*x as usize)
                    .ok_or_else(|| RuntimeError::InvalidBytecode(format!("missing string {}", x)))?
                    .as_str();
                if let Some(kind) = self.resolved_name_for(block, *x) {
                    match kind {
                        ResolvedName::Slot(slot) => {
                            self.set_slot_value(slot, RuntimeValue::Null);
                            return Ok(TerminateValue::None);
                        }
                        ResolvedName::GlobalVar => {
                            let _ = self.variables.remove(name);
                            return Ok(TerminateValue::None);
                        }
                        ResolvedName::Func => {
                            let _ = self.registry.functions.remove(name);
                            return Ok(TerminateValue::None);
                        }
                        ResolvedName::Unknown => {}
                    }
                }
                match self.resolve_var_name(name) {
                    VarName::Func(func) => {
                        let _ = self.registry.functions.remove(&func);
                    }
                    VarName::Var(var) => {
                        if let Some(var) = self.variables.remove(&var) {
                            let _ = self.move_saveable_into_runtime_var(var);
                        }
                    }
                    _ => {}
                }
            }
            VMInstruction::Move(x) => {
                let name = block
                    .local_strings
                    .get(*x as usize)
                    .ok_or_else(|| RuntimeError::InvalidBytecode(format!("missing string {}", x)))?
                    .as_str();
                if let Some(kind) = self.resolved_name_for(block, *x) {
                    match kind {
                        ResolvedName::Slot(slot) => {
                            let value = self.get_slot_value(slot);
                            self.set_slot_value(slot, RuntimeValue::Null);
                            stack.push(value);
                            return Ok(TerminateValue::None);
                        }
                        ResolvedName::GlobalVar => {
                            if let Some(var) = self.variables.remove(name) {
                                stack.push(self.move_saveable_into_runtime_var(var));
                            }
                            return Ok(TerminateValue::None);
                        }
                        ResolvedName::Func => {
                            if let Some(func) = self.registry.functions.remove(name) {
                                stack.push(RuntimeValue::Function {
                                    name: func.name.clone(),
                                    captures: func.captures.clone(),
                                });
                            }
                            return Ok(TerminateValue::None);
                        }
                        ResolvedName::Unknown => {}
                    }
                }
                match self.resolve_var_name(name) {
                    VarName::Func(func) => {
                        if let Some(func) = self.registry.functions.remove(&func) {
                            stack.push(RuntimeValue::Function {
                                name: func.name.clone(),
                                captures: func.captures.clone(),
                            });
                        }
                    }
                    VarName::Var(var) => {
                        if let Some(var) = self.variables.remove(&var) {
                            stack.push(self.move_saveable_into_runtime_var(var));
                        }
                    }
                    _ => {}
                }
            }
            VMInstruction::LoadVar(x) => {
                let name = block
                    .local_strings
                    .get(*x as usize)
                    .ok_or_else(|| RuntimeError::InvalidBytecode(format!("missing string {}", x)))?
                    .as_str();
                if let Some(kind) = self.resolved_name_for(block, *x) {
                    match kind {
                        ResolvedName::Slot(slot) => {
                            stack.push(self.get_slot_value(slot));
                            return Ok(TerminateValue::None);
                        }
                        ResolvedName::GlobalVar => {
                            if let Some(func) = self.registry.functions.get(name) {
                                stack.push(RuntimeValue::Function {
                                    name: func.name.clone(),
                                    captures: func.captures.clone(),
                                });
                            } else if let Some(var) = self.variables.get(name) {
                                stack.push(self.copy_saveable_into_runtime_var(var.clone()));
                            } else if let Some((_, short_name)) = name.rsplit_once(':') {
                                if let Some(func) = self
                                    .registry
                                    .functions
                                    .iter()
                                    .find(|(k, _)| k.ends_with(&format!(":{}", short_name)))
                                {
                                    stack.push(RuntimeValue::Function {
                                        name: func.1.name.clone(),
                                        captures: func.1.captures.clone(),
                                    });
                                }
                            }
                            return Ok(TerminateValue::None);
                        }
                        ResolvedName::Func => {
                            if let Some(func) = self.registry.functions.get(name) {
                                stack.push(RuntimeValue::Function {
                                    name: func.name.clone(),
                                    captures: func.captures.clone(),
                                });
                            }
                            return Ok(TerminateValue::None);
                        }
                        ResolvedName::Unknown => {}
                    }
                }
                match self.resolve_var_name(name) {
                    VarName::Func(func) => {
                        if let Some(func) = self.registry.functions.get(&func) {
                            stack.push(RuntimeValue::Function {
                                name: func.name.clone(),
                                captures: func.captures.clone(),
                            });
                        } else if let Some((_, short_name)) = name.rsplit_once(':') {
                            if let Some(func) = self
                                .registry
                                .functions
                                .iter()
                                .find(|(k, _)| k.ends_with(&format!(":{}", short_name)))
                            {
                                stack.push(RuntimeValue::Function {
                                    name: func.1.name.clone(),
                                    captures: func.1.captures.clone(),
                                });
                            }
                        }
                    }
                    VarName::Var(var) => {
                        if let Some(var) = self.variables.get(&var) {
                            stack.push(self.copy_saveable_into_runtime_var(var.clone()));
                        }
                    }
                    VarName::Slot(slot) => {
                        stack.push(self.get_slot_value(slot));
                    }
                    VarName::Global => {
                        if let Some((_, short_name)) = name.rsplit_once(':') {
                            if let Some(func) = self
                                .registry
                                .functions
                                .iter()
                                .find(|(k, _)| k.ends_with(&format!(":{}", short_name)))
                            {
                                stack.push(RuntimeValue::Function {
                                    name: func.1.name.clone(),
                                    captures: func.1.captures.clone(),
                                });
                            }
                        }
                    }
                }
            }
            VMInstruction::LoadVarRef(x) => {
                let name = block.local_strings.get(*x as usize).ok_or_else(|| {
                    RuntimeError::InvalidBytecode(format!("missing string {}", x))
                })?;
                if let Some(kind) = self.resolved_name_for(block, *x) {
                    match kind {
                        ResolvedName::Slot(slot) => {
                            stack.push(RuntimeValue::SlotRef(slot));
                            return Ok(TerminateValue::None);
                        }
                        ResolvedName::GlobalVar => {
                            stack.push(RuntimeValue::Ref(name.clone()));
                            return Ok(TerminateValue::None);
                        }
                        ResolvedName::Func => {
                            stack.push(RuntimeValue::Ref(name.clone()));
                            return Ok(TerminateValue::None);
                        }
                        ResolvedName::Unknown => {}
                    }
                }
                if let Some(slot) = self.current_frame().slot_map.get(name) {
                    stack.push(RuntimeValue::SlotRef(*slot));
                } else {
                    stack.push(RuntimeValue::Ref(name.clone()));
                }
            }
            VMInstruction::Deref => {
                let value = match stack.pop().ok_or(RuntimeError::StackUnderflow)? {
                    RuntimeValue::Ref(x) => self
                        .variables
                        .get(&x)
                        .cloned()
                        .ok_or(RuntimeError::DanglingRef(x))?,
                    RuntimeValue::SlotRef(slot) => self.get_slot_value(slot),
                    x => x,
                };
                stack.push(value);
            }
            VMInstruction::MakeRef => {
                let value = stack.pop().ok_or(RuntimeError::StackUnderflow)?;
                match value {
                    RuntimeValue::Ref(name) => {
                        stack.push(RuntimeValue::Ref(name));
                    }
                    RuntimeValue::SlotRef(slot) => {
                        let value = self.get_slot_value(slot);
                        let name = self.alloc_ref_id();
                        self.variables.insert(name.clone(), value);
                        stack.push(RuntimeValue::Ref(name));
                    }
                    other => {
                        let name = self.alloc_ref_id();
                        self.variables.insert(name.clone(), other);
                        stack.push(RuntimeValue::Ref(name));
                    }
                }
            }
            VMInstruction::DeclareVar(x) | VMInstruction::SetVar(x) => {
                let name = block.local_strings.get(*x as usize).ok_or_else(|| {
                    RuntimeError::InvalidBytecode(format!("missing string {}", x))
                })?;

                let value = self.convert_runtime_var_into_saveable(
                    stack.pop().ok_or(RuntimeError::StackUnderflow)?,
                );
                let slot = match self.resolved_name_for(block, *x) {
                    Some(ResolvedName::Slot(slot)) => slot,
                    _ => self.ensure_slot(name),
                };
                self.set_slot_value(slot, value);
            }
            VMInstruction::List(count) => {
                let count = *count as usize;
                if stack.len() < count {
                    return Err(RuntimeError::StackUnderflow);
                }
                let values = stack.pop_n(count).ok_or(RuntimeError::StackUnderflow)?;
                stack.push(RuntimeValue::List(Gc::new(crate::value::GcVec(values))));
            }
            VMInstruction::Call(count) => {
                let func = stack.pop().ok_or(RuntimeError::StackUnderflow)?;
                let count = *count as usize;
                if stack.len() < count {
                    return Err(RuntimeError::StackUnderflow);
                }
                let args = stack.pop_n(count).ok_or(RuntimeError::StackUnderflow)?;

                match func {
                    RuntimeValue::Function { name, captures: _ } => {
                        let func_opt = if let Some(cached) = self.call_cache.get(&name) {
                            Some(cached.clone())
                        } else if let Some(x) = self.registry.functions.get(&name) {
                            self.call_cache.insert(name.clone(), x.clone());
                            Some(x.clone())
                        } else if let Some((prefix, _)) = name.split_once("->") {
                            let found = self
                                .registry
                                .functions
                                .iter()
                                .find(|(k, _)| k.starts_with(prefix))
                                .map(|(_, v)| v.clone());
                            if let Some(ref func) = found {
                                self.call_cache.insert(name.clone(), func.clone());
                            }
                            found
                        } else if let Some((_, short_name)) = name.rsplit_once(':') {
                            let found = self
                                .registry
                                .functions
                                .iter()
                                .find(|(k, _)| k.ends_with(&format!(":{}", short_name)))
                                .map(|(_, v)| v.clone());
                            if let Some(ref func) = found {
                                self.call_cache.insert(name.clone(), func.clone());
                            }
                            found
                        } else {
                            None
                        };

                        if let Some(func) = func_opt {
                            let value = self.run_function(func.as_ref(), args)?;
                            stack.push(value);
                        } else {
                            return Err(RuntimeError::FunctionNotFound(name));
                        }
                    }
                    RuntimeValue::NativeFunction(func) => {
                        stack.push(func.run(self, args)?);
                    }
                    _ => return Err(RuntimeError::InvalidFunctionCall),
                }
            }
            VMInstruction::Index => {
                let value = stack.pop().ok_or(RuntimeError::StackUnderflow)?;
                let RuntimeValue::Int(index) = stack.pop().ok_or(RuntimeError::StackUnderflow)?
                else {
                    return Err(RuntimeError::UnexpectedType(RuntimeValue::Null));
                };

                stack.push(match value {
                    RuntimeValue::Ref(x) => match self
                        .variables
                        .get(&x)
                        .ok_or(RuntimeError::DanglingRef(x.clone()))?
                    {
                        RuntimeValue::List(x) => x
                            .as_ref()
                            .0
                            .get(index as usize)
                            .ok_or(RuntimeError::StackUnderflow)?
                            .clone(),
                        other => return Err(RuntimeError::UnexpectedType(other.clone())),
                    },
                    RuntimeValue::SlotRef(slot) => match self.get_slot_value(slot) {
                        RuntimeValue::List(x) => x
                            .as_ref()
                            .0
                            .get(index as usize)
                            .ok_or(RuntimeError::StackUnderflow)?
                            .clone(),
                        other => return Err(RuntimeError::UnexpectedType(other)),
                    },
                    RuntimeValue::List(x) => x
                        .as_ref()
                        .0
                        .get(index as usize)
                        .ok_or(RuntimeError::StackUnderflow)?
                        .clone(),
                    other => return Err(RuntimeError::UnexpectedType(other)),
                });
            }
            VMInstruction::LoadMember(x) => {
                let name = block.local_strings.get(*x as usize).ok_or_else(|| {
                    RuntimeError::InvalidBytecode(format!("missing string {}", x))
                })?;
                let value = stack.pop().ok_or(RuntimeError::StackUnderflow)?;

                let tuple_index = name.parse::<usize>().ok();

                stack.push(match value {
                    RuntimeValue::Ref(x) => match self
                        .variables
                        .get(&x)
                        .ok_or(RuntimeError::DanglingRef(x.clone()))?
                    {
                        RuntimeValue::Aggregate(None, map) => {
                            let idx = tuple_index.ok_or(RuntimeError::UnexpectedType(
                                RuntimeValue::Ref(x.clone()),
                            ))?;
                            map.as_ref()
                                .0
                                .0
                                .get(idx)
                                .ok_or(RuntimeError::StackUnderflow)?
                                .1
                                .clone()
                        }
                        RuntimeValue::Aggregate(Some(_key), map) => map
                            .as_ref()
                            .0
                            .0
                            .iter()
                            .find(|x| &x.0 == name)
                            .ok_or(RuntimeError::StackUnderflow)?
                            .1
                            .clone(),
                        RuntimeValue::Enum(_, _, Some(x)) if name == "next" => x.as_ref().clone(),
                        RuntimeValue::Enum(_, _, None) if name == "next" => RuntimeValue::Null,
                        RuntimeValue::Option(Some(x)) if name == "next" => x.as_ref().clone(),
                        RuntimeValue::Option(None) if name == "next" => RuntimeValue::Null,
                        RuntimeValue::Result(Ok(x)) if name == "next" => x.as_ref().clone(),
                        RuntimeValue::Result(Err(x)) if name == "next" => x.as_ref().clone(),
                        other => return Err(RuntimeError::UnexpectedType(other.clone())),
                    },
                    RuntimeValue::SlotRef(slot) => match self.get_slot_value(slot) {
                        RuntimeValue::Aggregate(None, map) => {
                            let idx = tuple_index
                                .ok_or(RuntimeError::UnexpectedType(RuntimeValue::SlotRef(slot)))?;
                            map.as_ref()
                                .0
                                .0
                                .get(idx)
                                .ok_or(RuntimeError::StackUnderflow)?
                                .1
                                .clone()
                        }
                        RuntimeValue::Aggregate(Some(_key), map) => map
                            .as_ref()
                            .0
                            .0
                            .iter()
                            .find(|x| &x.0 == name)
                            .ok_or(RuntimeError::StackUnderflow)?
                            .1
                            .clone(),
                        RuntimeValue::Enum(_, _, Some(x)) if name == "next" => x.as_ref().clone(),
                        RuntimeValue::Enum(_, _, None) if name == "next" => RuntimeValue::Null,
                        RuntimeValue::Option(Some(x)) if name == "next" => x.as_ref().clone(),
                        RuntimeValue::Option(None) if name == "next" => RuntimeValue::Null,
                        RuntimeValue::Result(Ok(x)) if name == "next" => x.as_ref().clone(),
                        RuntimeValue::Result(Err(x)) if name == "next" => x.as_ref().clone(),
                        other => return Err(RuntimeError::UnexpectedType(other)),
                    },
                    RuntimeValue::Aggregate(None, map) => {
                        let idx = tuple_index.ok_or(RuntimeError::UnexpectedType(
                            RuntimeValue::Aggregate(None, map.clone()),
                        ))?;
                        map.as_ref()
                            .0
                            .0
                            .get(idx)
                            .ok_or(RuntimeError::StackUnderflow)?
                            .1
                            .clone()
                    }
                    RuntimeValue::Aggregate(Some(_key), map) => map
                        .as_ref()
                        .0
                        .0
                        .iter()
                        .find(|x| &x.0 == name)
                        .ok_or(RuntimeError::StackUnderflow)?
                        .1
                        .clone(),
                    RuntimeValue::Enum(_, _, Some(x)) if name == "next" => x.as_ref().clone(),
                    RuntimeValue::Enum(_, _, None) if name == "next" => RuntimeValue::Null,
                    RuntimeValue::Option(Some(x)) if name == "next" => x.as_ref().clone(),
                    RuntimeValue::Option(None) if name == "next" => RuntimeValue::Null,
                    RuntimeValue::Result(Ok(x)) if name == "next" => x.as_ref().clone(),
                    RuntimeValue::Result(Err(x)) if name == "next" => x.as_ref().clone(),
                    other => return Err(RuntimeError::UnexpectedType(other)),
                });
            }
            VMInstruction::SetMember(_x) => {
                let name = block.local_strings.get(*_x as usize).ok_or_else(|| {
                    RuntimeError::InvalidBytecode(format!("missing string {}", _x))
                })?;
                let value = stack.pop().ok_or(RuntimeError::StackUnderflow)?;
                let target = stack.pop().ok_or(RuntimeError::StackUnderflow)?;

                let tuple_index = name.parse::<usize>().ok();

                let update_aggregate =
                    |agg_name: &Option<String>, map: &Gc<crate::value::GcMap>| {
                        let mut entries = map.as_ref().0.0.clone();
                        match (agg_name.as_ref(), tuple_index) {
                            (None, Some(idx)) => {
                                if idx >= entries.len() {
                                    return Err(RuntimeError::StackUnderflow);
                                }
                                entries[idx].1 = value.clone();
                            }
                            (Some(_), _) => {
                                if let Some(entry) =
                                    entries.iter_mut().find(|entry| entry.0 == *name)
                                {
                                    entry.1 = value.clone();
                                } else {
                                    return Err(RuntimeError::StackUnderflow);
                                }
                            }
                            _ => return Err(RuntimeError::UnexpectedType(RuntimeValue::Null)),
                        }
                        Ok(Gc::new(crate::value::GcMap(ObjectMap(entries))))
                    };

                match target {
                    RuntimeValue::Ref(ref_name) => {
                        let current = self
                            .variables
                            .get(&ref_name)
                            .cloned()
                            .ok_or(RuntimeError::DanglingRef(ref_name.clone()))?;
                        let updated = match current {
                            RuntimeValue::Aggregate(name, map) => RuntimeValue::Aggregate(
                                name.clone(),
                                update_aggregate(&name, &map)?,
                            ),
                            other => return Err(RuntimeError::UnexpectedType(other)),
                        };
                        self.variables.insert(ref_name.clone(), updated.clone());
                    }
                    RuntimeValue::SlotRef(slot) => {
                        let current = self.get_slot_value(slot);
                        let updated = match current {
                            RuntimeValue::Aggregate(name, map) => RuntimeValue::Aggregate(
                                name.clone(),
                                update_aggregate(&name, &map)?,
                            ),
                            other => return Err(RuntimeError::UnexpectedType(other)),
                        };
                        self.set_slot_value(slot, updated);
                    }
                    other => return Err(RuntimeError::UnexpectedType(other)),
                }
            }
            VMInstruction::SetIndex => {
                let value = stack.pop().ok_or(RuntimeError::StackUnderflow)?;
                let target = stack.pop().ok_or(RuntimeError::StackUnderflow)?;
                let RuntimeValue::Int(index) = stack.pop().ok_or(RuntimeError::StackUnderflow)?
                else {
                    return Err(RuntimeError::UnexpectedType(RuntimeValue::Null));
                };
                let index = index as usize;

                let update_list = |list: &Gc<crate::value::GcVec>| {
                    let mut items = list.as_ref().0.clone();
                    if index >= items.len() {
                        return Err(RuntimeError::StackUnderflow);
                    }
                    items[index] = value.clone();
                    Ok(Gc::new(crate::value::GcVec(items)))
                };

                match target {
                    RuntimeValue::Ref(ref_name) => {
                        let current = self
                            .variables
                            .get(&ref_name)
                            .cloned()
                            .ok_or(RuntimeError::DanglingRef(ref_name.clone()))?;
                        let updated = match current {
                            RuntimeValue::List(list) => RuntimeValue::List(update_list(&list)?),
                            other => return Err(RuntimeError::UnexpectedType(other)),
                        };
                        self.variables.insert(ref_name.clone(), updated.clone());
                    }
                    RuntimeValue::SlotRef(slot) => {
                        let current = self.get_slot_value(slot);
                        let updated = match current {
                            RuntimeValue::List(list) => RuntimeValue::List(update_list(&list)?),
                            other => return Err(RuntimeError::UnexpectedType(other)),
                        };
                        self.set_slot_value(slot, updated);
                    }
                    other => return Err(RuntimeError::UnexpectedType(other)),
                }
            }
            VMInstruction::SetRef => {
                let value = stack.pop().ok_or(RuntimeError::StackUnderflow)?;
                let target = stack.pop().ok_or(RuntimeError::StackUnderflow)?;
                match target {
                    RuntimeValue::Ref(name) => {
                        self.variables.insert(name, value);
                    }
                    RuntimeValue::SlotRef(slot) => {
                        self.set_slot_value(slot, value);
                    }
                    other => return Err(RuntimeError::UnexpectedType(other)),
                }
            }
            VMInstruction::Aggregate(x) => {
                let layout = block.aggregate_layouts.get(*x as usize).ok_or_else(|| {
                    RuntimeError::InvalidBytecode(format!("missing aggregate layout {}", x))
                })?;
                let mut values = Vec::new();
                for _ in 0..layout.members.len() {
                    values.push(stack.pop().ok_or(RuntimeError::StackUnderflow)?);
                }
                values.reverse();

                stack.push(RuntimeValue::Aggregate(
                    layout.name.clone(),
                    Gc::new(crate::value::GcMap({
                        let mut map = Vec::new();

                        for (i, value) in values.into_iter().enumerate() {
                            let member = layout.members.get(i).ok_or_else(|| {
                                RuntimeError::InvalidBytecode(format!(
                                    "missing aggregate member {} in {:?}",
                                    i, layout.name
                                ))
                            })?;
                            map.push((member.to_string(), value));
                        }

                        ObjectMap(map)
                    })),
                ));
            }
            VMInstruction::Range(inclusive) => {
                let (RuntimeValue::Int(mut to), RuntimeValue::Int(from)) = (
                    stack.pop().ok_or(RuntimeError::StackUnderflow)?,
                    stack.pop().ok_or(RuntimeError::StackUnderflow)?,
                ) else {
                    return Err(RuntimeError::UnexpectedType(RuntimeValue::Null));
                };

                to += if *inclusive { 1 } else { 0 };
                stack.push(RuntimeValue::Range(from, to));
            }
            VMInstruction::Enum {
                name,
                variant,
                has_payload,
            } => {
                let value = RuntimeValue::Enum(
                    block
                        .local_strings
                        .get(*name as usize)
                        .ok_or_else(|| {
                            RuntimeError::InvalidBytecode(format!("missing string {}", name))
                        })?
                        .to_string(),
                    *variant as usize,
                    if *has_payload {
                        Some(Gc::new(stack.pop().ok_or(RuntimeError::StackUnderflow)?))
                    } else {
                        None
                    },
                );
                stack.push(value);
            }
            VMInstruction::As(data_type) => {
                let value = stack.pop().ok_or(RuntimeError::StackUnderflow)?;
                let value = value.convert(self, &data_type.data_type);
                stack.push(match value {
                    Ok(x) => RuntimeValue::Result(Ok(Gc::new(x))),
                    Err(e) => {
                        RuntimeValue::Result(Err(Gc::new(RuntimeValue::Str(format!("{:?}", e)))))
                    }
                });
            }
        }

        Ok(TerminateValue::None)
    }
}
