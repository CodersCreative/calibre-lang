use std::sync::Arc;

use calibre_parser::ast::ObjectMap;
use dumpster::sync::Gc;
use rustc_hash::FxHashSet;

use crate::{
    ResolvedName, VM, VMStack, VarName,
    conversion::{VMBlock, VMFunction, VMGlobal, VMInstruction, VMLiteral},
    error::RuntimeError,
    value::{
        RuntimeValue, TerminateValue,
        operation::{binary, boolean, comparison},
    },
};

impl VM {
    pub fn run(
        &mut self,
        function: &VMFunction,
        args: Vec<RuntimeValue>,
    ) -> Result<RuntimeValue, RuntimeError> {
        let _ = self.run_globals()?;
        self.run_function(function, args, Vec::new())
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
        captures: Vec<(String, RuntimeValue)>,
    ) -> Result<RuntimeValue, RuntimeError> {
        let lowered = self.lowered_function(function);

        let cache = {
            let built = Arc::new(self.build_resolved_cache(function));
            self.caches
                .resolved
                .insert(function.name.clone(), built.clone());
            built
        };

        let prev_resolved = self.caches.current_resolved.take();
        self.caches.current_resolved = Some(cache.clone());
        self.push_frame();
        let mut restore_vars = Vec::new();

        for (name, value) in captures {
            let old = self.variables.get(&name).cloned();
            restore_vars.push((name.clone(), old));
            self.variables.insert(name, value);
        }

        let result = {
            let mut stack = VMStack::new();
            let mut block = lowered.blocks.first().ok_or_else(|| {
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
                        block = lowered.blocks.get(x.0 as usize).ok_or_else(|| {
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
        };

        for (name, value) in restore_vars {
            if let Some(value) = value {
                self.variables.insert(name, value);
            } else {
                self.variables.remove(&name);
            }
        }

        self.pop_frame();
        self.caches.current_resolved = prev_resolved;

        result
    }

    pub fn resolve_var_name(&self, name: &str) -> VarName {
        if let Some(slot) = self.current_frame().slot_map.get(name) {
            return VarName::Slot(*slot);
        } else if self.variables.contains_key(name) {
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
            }
        } else if let Some((_, short_name)) = name.rsplit_once(':') {
            if let Some(slot) = self.find_unique_slot_by_suffix(short_name) {
                return VarName::Slot(slot);
            }
            if let Some(var) = self.find_unique_var_by_suffix(short_name) {
                return VarName::Var(var);
            }
            if let Some(func) = self.find_unique_function_by_suffix(short_name) {
                return VarName::Func(func.name.clone());
            }
        }

        VarName::Global
    }

    pub fn run_block(
        &mut self,
        block: &VMBlock,
        stack: &mut VMStack,
    ) -> Result<TerminateValue, RuntimeError> {
        for (ip, instruction) in block.instructions.iter().enumerate() {
            self.maybe_collect_garbage();
            let step = match self.run_instruction(instruction, block, stack) {
                Ok(step) => step,
                Err(e) => {
                    let span = block.instruction_spans.get(ip).cloned().unwrap_or_default();
                    return Err(RuntimeError::At(span, Box::new(e)));
                }
            };

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
                let idx = *x as usize;
                if idx >= block.local_literals.len() {
                    return Err(RuntimeError::InvalidBytecode(format!(
                        "missing literal {}",
                        x
                    )));
                }
                let literal = unsafe { block.local_literals.get_unchecked(idx).clone() };
                match literal {
                    VMLiteral::Closure { label, captures } => {
                        let mut seen = FxHashSet::default();
                        let captured = self.capture_values(&captures, &mut seen);
                        stack.push(RuntimeValue::Function {
                            name: label,
                            captures: captured,
                        });
                    }
                    VMLiteral::ExternFunction {
                        abi,
                        library,
                        symbol,
                        parameters,
                        return_type,
                    } => {
                        let abi_lower = abi.to_ascii_lowercase();
                        if abi_lower != "c" && abi_lower != "zig" {
                            return Err(RuntimeError::Ffi(format!("unsupported ABI \"{}\"", abi)));
                        }

                        let mut last_err = None;
                        let mut handle_opt = None;
                        for candidate in Self::resolve_library_candidates(&library) {
                            match unsafe { libloading::Library::new(candidate.clone()) } {
                                Ok(h) => {
                                    handle_opt = Some(h);
                                    break;
                                }
                                Err(e) => {
                                    last_err = Some(e.to_string());
                                }
                            }
                        }
                        let handle = handle_opt.ok_or_else(|| {
                            RuntimeError::Ffi(format!(
                                "failed to load library {} ({})",
                                library,
                                last_err.unwrap_or_else(|| "no candidates".to_string())
                            ))
                        })?;
                        let func = crate::value::ExternFunction {
                            abi,
                            library,
                            symbol,
                            parameters,
                            return_type,
                            handle: std::sync::Arc::new(handle),
                        };
                        stack.push(RuntimeValue::ExternFunction(std::sync::Arc::new(func)));
                    }
                    other => {
                        stack.push(RuntimeValue::from(other));
                    }
                }
            }
            VMInstruction::LoadSlot(slot) => {
                stack.push(self.get_slot_value(*slot as usize));
            }
            VMInstruction::LoadSlotRef(slot) => {
                stack.push(RuntimeValue::SlotRef(*slot as usize));
            }
            VMInstruction::Drop(x) => {
                let name = self.local_string(block, *x)?;
                if let Some(kind) = self.resolved_name_for(block, *x) {
                    match kind {
                        ResolvedName::Slot(slot) => {
                            let value = self.get_slot_value(slot);
                            self.set_slot_value(slot, RuntimeValue::Null);
                            self.drop_runtime_value(value);
                            return Ok(TerminateValue::None);
                        }
                        ResolvedName::GlobalVar => {
                            if let Some(value) = self.variables.remove(name) {
                                self.drop_runtime_value(value);
                            }
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
                            self.drop_runtime_value(var);
                        }
                    }
                    _ => {}
                }
            }
            VMInstruction::DropSlot(slot) => {
                let value = self.get_slot_value(*slot as usize);
                self.set_slot_value(*slot as usize, RuntimeValue::Null);
                self.drop_runtime_value(value);
            }
            VMInstruction::Move(x) => {
                let name = self.local_string(block, *x)?;
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
                            } else if let Some(slot) = self.current_frame().slot_map.get(name) {
                                let value = self.get_slot_value(*slot);
                                self.set_slot_value(*slot, RuntimeValue::Null);
                                stack.push(value);
                            }

                            return Ok(TerminateValue::None);
                        }
                        ResolvedName::Func => {
                            if let Some(func) = self.registry.functions.remove(name) {
                                stack.push(self.make_runtime_function(&func));
                            }
                            return Ok(TerminateValue::None);
                        }
                        ResolvedName::Unknown => {}
                    }
                }
                match self.resolve_var_name(name) {
                    VarName::Func(func) => {
                        if let Some(func) = self.registry.functions.remove(&func) {
                            stack.push(self.make_runtime_function(&func));
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
            VMInstruction::MoveSlot(slot) => {
                let value = self.get_slot_value(*slot as usize);
                self.set_slot_value(*slot as usize, RuntimeValue::Null);
                stack.push(value);
            }
            VMInstruction::LoadVar(x) => {
                let name = self.local_string(block, *x)?;
                if let Some(kind) = self.resolved_name_for(block, *x) {
                    match kind {
                        ResolvedName::Slot(slot) => {
                            stack.push(self.get_slot_value(slot));
                            return Ok(TerminateValue::None);
                        }
                        ResolvedName::GlobalVar => {
                            let mut pushed = false;
                            if let Some(func) = self.registry.functions.get(name) {
                                stack.push(self.make_runtime_function(func));
                                pushed = true;
                            } else if let Some(var) = self.variables.get(name) {
                                stack.push(self.copy_saveable_into_runtime_var(var.clone()));
                                pushed = true;
                            } else if let Some((_, short_name)) = name.rsplit_once(':') {
                                if let Some(var_name) = self.find_unique_var_by_suffix(short_name) {
                                    if let Some(var) = self.variables.get(&var_name) {
                                        stack
                                            .push(self.copy_saveable_into_runtime_var(var.clone()));
                                        pushed = true;
                                    }
                                }
                            } else if let Some(slot) = self.current_frame().slot_map.get(name) {
                                stack.push(self.get_slot_value(*slot));
                                pushed = true;
                            } else if let Some((_, short_name)) = name.rsplit_once(':') {
                                if let Some(func) = self.find_unique_function_by_suffix(short_name)
                                {
                                    stack.push(self.make_runtime_function(func.as_ref()));
                                    pushed = true;
                                }
                            }
                            if !pushed {
                                match self.resolve_var_name(name) {
                                    VarName::Slot(slot) => {
                                        stack.push(self.get_slot_value(slot));
                                    }
                                    VarName::Var(var) => {
                                        if let Some(var) = self.variables.get(&var) {
                                            stack.push(
                                                self.copy_saveable_into_runtime_var(var.clone()),
                                            );
                                        } else {
                                            stack.push(RuntimeValue::Null);
                                        }
                                    }
                                    VarName::Func(func) => {
                                        if let Some(func) = self.registry.functions.get(&func) {
                                            stack.push(self.make_runtime_function(func));
                                        } else {
                                            stack.push(RuntimeValue::Null);
                                        }
                                    }
                                    VarName::Global => stack.push(RuntimeValue::Null),
                                }
                            }
                            return Ok(TerminateValue::None);
                        }
                        ResolvedName::Func => {
                            if let Some(func) = self.registry.functions.get(name) {
                                stack.push(self.make_runtime_function(func));
                            }
                            return Ok(TerminateValue::None);
                        }
                        ResolvedName::Unknown => {}
                    }
                }
                match self.resolve_var_name(name) {
                    VarName::Func(func) => {
                        if let Some(func) = self.registry.functions.get(&func) {
                            stack.push(self.make_runtime_function(func));
                        } else if let Some((_, short_name)) = name.rsplit_once(':') {
                            if let Some(func) = self.find_unique_function_by_suffix(short_name) {
                                stack.push(self.make_runtime_function(func.as_ref()));
                            } else {
                                stack.push(RuntimeValue::Null);
                            }
                        } else {
                            stack.push(RuntimeValue::Null);
                        }
                    }
                    VarName::Var(var) => {
                        if let Some(var) = self.variables.get(&var) {
                            stack.push(self.copy_saveable_into_runtime_var(var.clone()));
                        } else {
                            stack.push(RuntimeValue::Null);
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
                                stack.push(self.make_runtime_function(func.1));
                            } else {
                                stack.push(RuntimeValue::Null);
                            }
                        } else {
                            stack.push(RuntimeValue::Null);
                        }
                    }
                }
            }
            VMInstruction::LoadVarRef(x) => {
                let name = self.local_string_owned(block, *x)?;
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
                if let Some(slot) = self.current_frame().slot_map.get(name.as_str()) {
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
                        let name = self.get_ref_id().to_string();
                        self.variables.insert(name.clone(), value);
                        stack.push(RuntimeValue::Ref(name));
                    }
                    other => {
                        let name = self.get_ref_id().to_string();
                        self.variables.insert(name.clone(), other);
                        stack.push(RuntimeValue::Ref(name));
                    }
                }
            }
            VMInstruction::DeclareVar(x) | VMInstruction::SetVar(x) => {
                let name = self.local_string(block, *x)?;

                let value = self.convert_runtime_var_into_saveable(
                    stack.pop().ok_or(RuntimeError::StackUnderflow)?,
                );
                match self.resolved_name_for(block, *x) {
                    Some(ResolvedName::Slot(slot)) => {
                        let old = self.replace_slot_value(slot, value);
                        self.drop_runtime_value(old);
                    }
                    Some(ResolvedName::GlobalVar) => {
                        if let Some(old) = self.variables.insert(name.to_string(), value) {
                            self.drop_runtime_value(old);
                        }
                    }
                    Some(ResolvedName::Func) => {}
                    _ => {
                        let slot = self.ensure_slot(name);
                        let old = self.replace_slot_value(slot, value);
                        self.drop_runtime_value(old);
                    }
                }
            }
            VMInstruction::StoreSlot(slot) => {
                let value = self.convert_runtime_var_into_saveable(
                    stack.pop().ok_or(RuntimeError::StackUnderflow)?,
                );
                let old = self.replace_slot_value(*slot as usize, value);
                self.drop_runtime_value(old);
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
                    RuntimeValue::Function { name, captures } => {
                        let func_opt = if let Some(cached) = self.caches.call.get(&name) {
                            Some(cached.clone())
                        } else if let Some(x) = self.registry.functions.get(&name) {
                            self.caches.call.insert(name.clone(), x.clone());
                            Some(x.clone())
                        } else if let Some((prefix, _)) = name.split_once("->") {
                            let found = self
                                .registry
                                .functions
                                .iter()
                                .find(|(k, _)| k.starts_with(prefix))
                                .map(|(_, v)| v.clone());
                            if let Some(ref func) = found {
                                self.caches.call.insert(name.clone(), func.clone());
                            }
                            found
                        } else if let Some((_, short_name)) = name.rsplit_once(':') {
                            if let Some(found) = self.find_unique_function_by_suffix(short_name) {
                                self.caches.call.insert(name.clone(), found.clone());
                                Some(found)
                            } else {
                                None
                            }
                        } else {
                            None
                        };

                        if let Some(func) = func_opt {
                            let value = self.run_function(func.as_ref(), args, captures)?;
                            stack.push(value);
                        } else {
                            return Err(RuntimeError::FunctionNotFound(name));
                        }
                    }
                    RuntimeValue::NativeFunction(func) => {
                        stack.push(func.run(self, args)?);
                    }
                    RuntimeValue::ExternFunction(func) => {
                        let value = func.call(self, args)?;
                        stack.push(value);
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
                    RuntimeValue::Aggregate(None, map) => map
                        .as_ref()
                        .0
                        .0
                        .get(index as usize)
                        .ok_or(RuntimeError::StackUnderflow)?
                        .1
                        .clone(),
                    other => return Err(RuntimeError::UnexpectedType(other)),
                });
            }
            VMInstruction::LoadMember(x) => {
                let name = self.local_string(block, *x)?;
                let value = stack.pop().ok_or(RuntimeError::StackUnderflow)?;
                let short_name = name.rsplit_once(':').map(|(_, short)| short);

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
                            .find(|x| {
                                &x.0 == name || short_name.map_or(false, |short| x.0 == short)
                            })
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
                            .find(|x| {
                                &x.0 == name || short_name.map_or(false, |short| x.0 == short)
                            })
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
                        .find(|x| &x.0 == name || short_name.map_or(false, |short| x.0 == short))
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
                let name = self.local_string(block, *_x)?;
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
