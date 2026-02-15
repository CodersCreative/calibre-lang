use calibre_lir::BlockId;
use calibre_parser::ast::ObjectMap;
use dumpster::sync::Gc;
use rustc_hash::FxHashSet;
use smallvec::SmallVec;
use std::sync::Arc;

use crate::{
    VM, VarName,
    conversion::{VMBlock, VMFunction, VMGlobal, VMInstruction, VMLiteral},
    error::RuntimeError,
    value::{
        RuntimeValue, TerminateValue, WaitGroupInner,
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
        self.run_function(function, args, std::sync::Arc::new(Vec::new()))
    }

    pub fn run_globals(&mut self) -> Result<(), RuntimeError> {
        if self.registry.globals.is_empty() {
            return Ok(());
        }
        let globals: Vec<VMGlobal> = self.registry.globals.values().cloned().collect();
        for global in globals {
            let _ = self.run_global(&global)?;
        }
        Ok(())
    }

    pub fn run_global(&mut self, global: &VMGlobal) -> Result<RuntimeValue, RuntimeError> {
        let mut block = global
            .blocks
            .get(*global.block_map.get(&global.entry).unwrap_or(&0))
            .ok_or_else(|| RuntimeError::InvalidBytecode("global has no blocks".to_string()))?;

        let mut prev_block: Option<BlockId> = None;
        loop {
            match self.run_block(block, prev_block)? {
                TerminateValue::Jump(target) => {
                    prev_block = Some(block.id);
                    block = global
                        .blocks
                        .get(*global.block_map.get(&target).unwrap_or(&0))
                        .ok_or_else(|| {
                            RuntimeError::InvalidBytecode(format!(
                                "invalid global block {}",
                                target.0
                            ))
                        })?;
                }
                TerminateValue::Return(x) => match x {
                    RuntimeValue::Null => break,
                    x => return Ok(x),
                },
                TerminateValue::Yield { .. } => break,
                TerminateValue::None => break,
            }
        }

        Ok(RuntimeValue::Null)
    }

    pub fn run_function<I>(
        &mut self,
        function: &VMFunction,
        args: I,
        captures: std::sync::Arc<Vec<(String, RuntimeValue)>>,
    ) -> Result<RuntimeValue, RuntimeError>
    where
        I: IntoIterator<Item = RuntimeValue>,
    {
        let mut state = crate::TaskState::default();
        match self.run_function_with_budget(function, args, captures, usize::MAX, &mut state)? {
            Some(value) => Ok(value),
            None => Ok(RuntimeValue::Null),
        }
    }

    pub fn run_function_with_budget<I>(
        &mut self,
        function: &VMFunction,
        args: I,
        captures: std::sync::Arc<Vec<(String, RuntimeValue)>>,
        budget: usize,
        state: &mut crate::TaskState,
    ) -> Result<Option<RuntimeValue>, RuntimeError>
    where
        I: IntoIterator<Item = RuntimeValue>,
    {
        let mut prev_vars: Vec<(String, Option<RuntimeValue>)> = Vec::new();
        if !captures.as_ref().is_empty() {
            prev_vars = Vec::with_capacity(captures.as_ref().len());
            prev_vars.extend(
                captures
                    .as_ref()
                    .iter()
                    .map(|(name, _)| (name.clone(), self.variables.get(name).cloned())),
            );
            for (name, value) in captures.as_ref().iter() {
                self.variables.insert(name.clone(), value.clone());
            }
        }

        if state.block.is_none() {
            let func_ptr = function as *const VMFunction as usize;
            self.push_frame(function.reg_count as usize, func_ptr);
            for (reg, arg) in function.param_regs.iter().zip(args) {
                self.set_reg_value(*reg, arg);
            }
            let base = self.local_map_base_for(function);
            let frame = self.current_frame_mut();
            frame.local_map_base = Some(base);
            frame.local_map.clear();
            state.block = Some(function.entry);
            state.ip = 0;
            state.prev_block = None;
        }

        let mut block_id = state.block.unwrap_or(function.entry);
        let mut block = function
            .blocks
            .get(*function.block_map.get(&block_id).unwrap_or(&0))
            .ok_or_else(|| RuntimeError::InvalidBytecode("function has no blocks".to_string()))?;
        let mut prev_block: Option<BlockId> = state.prev_block;
        let mut result = RuntimeValue::Null;
        let mut returned = false;
        loop {
            let slice_budget = if budget == usize::MAX {
                None
            } else {
                Some(budget.max(1))
            };

            match self.run_block_with_budget(block, prev_block, state.ip, slice_budget)? {
                TerminateValue::Jump(target) => {
                    prev_block = Some(block.id);
                    block_id = target;
                    block = function
                        .blocks
                        .get(*function.block_map.get(&block_id).unwrap_or(&0))
                        .ok_or_else(|| {
                            RuntimeError::InvalidBytecode(format!(
                                "invalid function block {}",
                                target.0
                            ))
                        })?;
                    state.ip = 0;
                    state.block = Some(block_id);
                    state.prev_block = prev_block;
                }
                TerminateValue::Return(x) => {
                    result = x;
                    returned = true;
                    break;
                }
                TerminateValue::Yield {
                    block,
                    ip,
                    prev_block,
                } => {
                    state.block = Some(block);
                    state.ip = ip;
                    state.prev_block = prev_block;
                    return Ok(None);
                }
                TerminateValue::None => break,
            }
        }

        if function.returns_value && !returned {
            result = self.get_reg_value(function.ret_reg);
        }

        if let Ok(filter) = std::env::var("CAL_DEBUG_FUNC_RESULT") {
            if function.name.contains(&filter) {
                eprintln!("{} result = {:?}", function.name, result);
            }
        }

        if let RuntimeValue::RegRef { frame, reg } = result {
            result = self.get_reg_value_in_frame(frame, reg);
        }

        self.pop_frame();
        if !prev_vars.is_empty() {
            for (name, old) in prev_vars {
                match old {
                    Some(v) => {
                        self.variables.insert(name, v);
                    }
                    None => {
                        self.variables.remove(&name);
                    }
                }
            }
        }

        Ok(Some(result))
    }

    fn apply_phis(&mut self, block: &VMBlock, prev: Option<BlockId>) -> Result<(), RuntimeError> {
        if block.phis.is_empty() {
            return Ok(());
        }
        let Some(prev) = prev else {
            return Ok(());
        };
        for phi in &block.phis {
            let mut selected = None;
            for (pred, reg) in &phi.sources {
                if *pred == prev {
                    selected = Some(*reg);
                    break;
                }
            }
            let reg = selected.unwrap_or_else(|| phi.sources.first().map(|x| x.1).unwrap_or(0));
            let value = self.get_reg_value(reg);
            self.set_reg_value(phi.dest, value);
        }
        Ok(())
    }

    pub fn run_block(
        &mut self,
        block: &VMBlock,
        prev: Option<BlockId>,
    ) -> Result<TerminateValue, RuntimeError> {
        self.run_block_with_budget(block, prev, 0, None)
    }

    pub fn run_block_with_budget(
        &mut self,
        block: &VMBlock,
        prev: Option<BlockId>,
        start_ip: usize,
        budget: Option<usize>,
    ) -> Result<TerminateValue, RuntimeError> {
        self.apply_phis(block, prev)?;
        let mut instr_exec_count: u64 = 0;
        let mut fuel = budget.unwrap_or(usize::MAX);
        for (ip, instruction) in block.instructions.iter().enumerate().skip(start_ip) {
            if (ip & 0x3f) == 0 {
                self.maybe_collect_garbage();
            }
            instr_exec_count = instr_exec_count.wrapping_add(1);
            let step = match self.run_instruction(instruction, block, ip as u32) {
                Ok(step) => step,
                Err(e) => {
                    let span = block.instruction_spans.get(ip).cloned().unwrap_or_default();
                    return Err(RuntimeError::at(span, e));
                }
            };

            match step {
                TerminateValue::None => {}
                x => return Ok(x),
            }

            if fuel != usize::MAX {
                fuel = fuel.saturating_sub(1);
                if fuel == 0 {
                    return Ok(TerminateValue::Yield {
                        block: block.id,
                        ip: ip + 1,
                        prev_block: prev,
                    });
                }
            }
        }

        Ok(TerminateValue::None)
    }

    fn run_instruction(
        &mut self,
        instruction: &VMInstruction,
        block: &VMBlock,
        ip: u32,
    ) -> Result<TerminateValue, RuntimeError> {
        match instruction {
            VMInstruction::LoadLiteral { dst, literal } => {
                let lit = block
                    .local_literals
                    .get(*literal as usize)
                    .ok_or_else(|| RuntimeError::InvalidBytecode("missing literal".to_string()))
                    .cloned()?;
                match lit {
                    VMLiteral::Closure { label, captures } => {
                        let mut seen = FxHashSet::default();
                        let caps = self.capture_values(&captures, &mut seen);
                        self.set_reg_value(
                            *dst,
                            RuntimeValue::Function {
                                name: label.into(),
                                captures: std::sync::Arc::new(caps),
                            },
                        );
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
                                Err(e) => last_err = Some(e.to_string()),
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
                            handle: Arc::new(handle),
                        };
                        self.set_reg_value(*dst, RuntimeValue::ExternFunction(Arc::new(func)));
                    }
                    other => {
                        self.set_reg_value(*dst, RuntimeValue::from(other));
                    }
                }
            }
            VMInstruction::LoadGlobal { dst, name } => {
                let name_idx = *name;
                let name = self.local_string(block, name_idx)?;
                if let Some(cache) = self.caches.globals_direct.get(name) {
                    self.set_reg_value(*dst, cache.clone());
                    return Ok(TerminateValue::None);
                }
                if let Some(cache) = self.caches.globals.get(name) {
                    self.set_reg_value(*dst, cache.clone());
                    return Ok(TerminateValue::None);
                }

                let mut value = None;
                let mut resolved_name: Option<String> = None;
                if let Some(func) = self.get_function(name) {
                    value = Some(self.make_runtime_function(func.as_ref()));
                    resolved_name = Some(name.to_string());
                } else if let Some(var) = self.variables.get(name) {
                    value = Some(self.copy_saveable_into_runtime_var(var.clone()));
                    resolved_name = Some(name.to_string());
                } else if let Some((_, short_name)) = name.rsplit_once(':') {
                    if let Some((base, _)) = short_name.split_once("->") {
                        if let Some(func) = self.find_unique_function_by_suffix(base) {
                            value = Some(self.make_runtime_function(func.as_ref()));
                            resolved_name = Some(func.name.clone());
                        } else if let Some(var_name) = self.find_unique_var_by_suffix(base) {
                            if let Some(var) = self.variables.get(&var_name) {
                                value = Some(self.copy_saveable_into_runtime_var(var.clone()));
                                resolved_name = Some(var_name);
                            }
                        }
                    }
                    if value.is_none() {
                        if let Some(func) = self.find_unique_function_by_suffix(short_name) {
                            value = Some(self.make_runtime_function(func.as_ref()));
                            resolved_name = Some(func.name.clone());
                        } else if let Some(var_name) = self.find_unique_var_by_suffix(short_name) {
                            if let Some(var) = self.variables.get(&var_name) {
                                value = Some(self.copy_saveable_into_runtime_var(var.clone()));
                                resolved_name = Some(var_name);
                            }
                        }
                    }
                }

                if matches!(value, Some(RuntimeValue::Null)) && name.contains("->") {
                    if let Some((_, short_name)) = name.rsplit_once(':') {
                        let base = short_name
                            .split_once("->")
                            .map(|(b, _)| b)
                            .unwrap_or(short_name);
                        if let Some(func) = self.find_unique_function_by_suffix(base) {
                            value = Some(self.make_runtime_function(func.as_ref()));
                            resolved_name = Some(func.name.clone());
                        } else if let Some(var_name) = self.find_unique_var_by_suffix(base) {
                            if let Some(var) = self.variables.get(&var_name) {
                                value = Some(self.copy_saveable_into_runtime_var(var.clone()));
                                resolved_name = Some(var_name);
                            }
                        }
                    }
                }

                let value = value.unwrap_or_else(|| {
                    let resolved = self.resolve_var_name(name);
                    match resolved {
                        VarName::Func(func) => self
                            .get_function(&func)
                            .map(|f| {
                                resolved_name = Some(func.clone());
                                self.make_runtime_function(f.as_ref())
                            })
                            .unwrap_or(RuntimeValue::Null),
                        VarName::Var(var) => self
                            .variables
                            .get(&var)
                            .cloned()
                            .map(|v| {
                                resolved_name = Some(var.clone());
                                self.copy_saveable_into_runtime_var(v)
                            })
                            .unwrap_or(RuntimeValue::Null),
                        VarName::Global => RuntimeValue::Null,
                    }
                });

                if let Some(resolved_name) = resolved_name {
                    self.caches.globals.insert(resolved_name, value.clone());
                }
                self.set_reg_value(*dst, value);
            }
            VMInstruction::MoveGlobal { dst, name } => {
                let name = self.local_string(block, *name)?;
                let resolved = self.resolve_var_name(name);
                let mut value = None;
                if let Some(func) = self.take_function(name) {
                    value = Some(self.make_runtime_function(&func));
                } else if let Some(var) = self.variables.remove(name) {
                    value = Some(self.move_saveable_into_runtime_var(var));
                } else if let Some((_, short_name)) = name.rsplit_once(':') {
                    if let Some(func) = self.find_unique_function_by_suffix(short_name) {
                        self.moved_functions.insert(func.name.clone());
                        value = Some(self.make_runtime_function(func.as_ref()));
                    } else if let Some(var_name) = self.find_unique_var_by_suffix(short_name) {
                        if let Some(var) = self.variables.remove(&var_name) {
                            value = Some(self.move_saveable_into_runtime_var(var));
                        }
                    }
                }

                let value = value.unwrap_or_else(|| match resolved.clone() {
                    VarName::Func(func) => {
                        if let Some(func) = self.take_function(&func) {
                            self.make_runtime_function(&func)
                        } else {
                            RuntimeValue::Null
                        }
                    }
                    VarName::Var(var) => {
                        if let Some(var) = self.variables.remove(&var) {
                            self.move_saveable_into_runtime_var(var)
                        } else {
                            RuntimeValue::Null
                        }
                    }
                    VarName::Global => RuntimeValue::Null,
                });

                self.set_reg_value(*dst, value);
            }
            VMInstruction::DropGlobal { name } => {
                let name = self.local_string(block, *name)?;
                match self.resolve_var_name(name) {
                    VarName::Var(var) => {
                        self.caches.globals_id.remove(var.as_str());
                        if let Some(val) = self.variables.remove(&var) {
                            self.drop_runtime_value(val);
                        }
                    }
                    VarName::Func(func) => {
                        self.moved_functions.insert(func);
                    }
                    VarName::Global => {}
                }
            }
            VMInstruction::StoreGlobal { name, src } => {
                let name = self.local_string(block, *name)?;
                let value = self.get_reg_value(*src);
                if let Some(id) = self.caches.globals_id.get(name).copied() {
                    let _ = self.variables.set_by_id(id, value);
                } else {
                    let id = self.variables.insert_str_with_id(name, value);
                    self.caches.globals_id.insert(name.to_string(), id);
                }
            }
            VMInstruction::SetLocalName { name, src } => {
                let interned = self.intern_local_string(block, *name)?;
                self.current_frame_mut().local_map.insert(interned, *src);
            }
            VMInstruction::LoadGlobalRef { dst, name } => {
                let name = self.local_string_owned(block, *name)?;
                if let Some(id) = self.caches.globals_id.get(name.as_str()).copied() {
                    self.set_reg_value(*dst, RuntimeValue::VarRef(id));
                } else if let Some(id) = self.variables.id_of(name.as_str()) {
                    self.caches.globals_id.insert(name.to_string(), id);
                    self.set_reg_value(*dst, RuntimeValue::VarRef(id));
                } else {
                    self.set_reg_value(*dst, RuntimeValue::Ref(name.clone()));
                }
            }
            VMInstruction::LoadRegRef { dst, src } => {
                let src_val = self.get_reg_value(*src);
                match src_val {
                    RuntimeValue::RegRef { frame, reg } => {
                        self.set_reg_value(*dst, RuntimeValue::RegRef { frame, reg });
                    }
                    RuntimeValue::Ref(name) => {
                        self.set_reg_value(*dst, RuntimeValue::Ref(name));
                    }
                    RuntimeValue::VarRef(id) => {
                        self.set_reg_value(*dst, RuntimeValue::VarRef(id));
                    }
                    _ => {
                        let frame = self.frames.len().saturating_sub(1);
                        self.set_reg_value(*dst, RuntimeValue::RegRef { frame, reg: *src });
                    }
                }
            }
            VMInstruction::Copy { dst, src } => {
                if dst == src {
                    return Ok(TerminateValue::None);
                }
                let value = self.get_reg_value(*src);
                self.set_reg_value(*dst, value);
            }
            VMInstruction::As {
                dst,
                src,
                data_type,
            } => {
                let value = self.get_reg_value(*src);
                let converted = value.convert(self, &data_type.data_type)?;
                self.set_reg_value(*dst, converted);
            }
            VMInstruction::Binary {
                dst,
                op,
                left,
                right,
            } => {
                let left_ref = self.get_reg_value_ref(*left);
                let right_ref = self.get_reg_value_ref(*right);
                let left = match left_ref {
                    RuntimeValue::Ref(_)
                    | RuntimeValue::VarRef(_)
                    | RuntimeValue::RegRef { .. }
                    | RuntimeValue::MutexGuard(_) => self.resolve_value_for_op_ref(left_ref)?,
                    other => other.clone(),
                };
                let right = match right_ref {
                    RuntimeValue::Ref(_)
                    | RuntimeValue::VarRef(_)
                    | RuntimeValue::RegRef { .. }
                    | RuntimeValue::MutexGuard(_) => self.resolve_value_for_op_ref(right_ref)?,
                    other => other.clone(),
                };
                self.set_reg_value(*dst, binary(self, op, left, right)?);
            }
            VMInstruction::AccLoad { src } => {
                let value = self.get_reg_value(*src);
                self.current_frame_mut().acc = value;
            }
            VMInstruction::AccStore { dst } => {
                let value = self.current_frame().acc.clone();
                self.set_reg_value(*dst, value);
            }
            VMInstruction::AccBinary { op, right } => {
                let right_ref = self.get_reg_value_ref(*right);
                let right = match right_ref {
                    RuntimeValue::Ref(_)
                    | RuntimeValue::VarRef(_)
                    | RuntimeValue::RegRef { .. }
                    | RuntimeValue::MutexGuard(_) => self.resolve_value_for_op_ref(right_ref)?,
                    other => other.clone(),
                };
                let left_ref = &self.current_frame().acc;
                let left = match left_ref {
                    RuntimeValue::Ref(_)
                    | RuntimeValue::VarRef(_)
                    | RuntimeValue::RegRef { .. }
                    | RuntimeValue::MutexGuard(_) => self.resolve_value_for_op_ref(left_ref)?,
                    other => other.clone(),
                };
                self.current_frame_mut().acc = binary(self, op, left, right)?;
            }
            VMInstruction::Comparison {
                dst,
                op,
                left,
                right,
            } => {
                let right_ref = self.get_reg_value_ref(*right);
                let right = match right_ref {
                    RuntimeValue::Ref(_)
                    | RuntimeValue::VarRef(_)
                    | RuntimeValue::RegRef { .. }
                    | RuntimeValue::MutexGuard(_) => self.resolve_value_for_op_ref(right_ref)?,
                    other => other.clone(),
                };
                let left_ref = self.get_reg_value_ref(*left);
                let left = match left_ref {
                    RuntimeValue::Ref(_)
                    | RuntimeValue::VarRef(_)
                    | RuntimeValue::RegRef { .. }
                    | RuntimeValue::MutexGuard(_) => self.resolve_value_for_op_ref(left_ref)?,
                    other => other.clone(),
                };
                let cmp_val = comparison(op, left, right)?;
                self.set_reg_value(*dst, cmp_val);
            }
            VMInstruction::Boolean {
                dst,
                op,
                left,
                right,
            } => {
                let right_ref = self.get_reg_value_ref(*right);
                let right = match right_ref {
                    RuntimeValue::Ref(_)
                    | RuntimeValue::VarRef(_)
                    | RuntimeValue::RegRef { .. }
                    | RuntimeValue::MutexGuard(_) => self.resolve_value_for_op_ref(right_ref)?,
                    other => other.clone(),
                };
                let left_ref = self.get_reg_value_ref(*left);
                let left = match left_ref {
                    RuntimeValue::Ref(_)
                    | RuntimeValue::VarRef(_)
                    | RuntimeValue::RegRef { .. }
                    | RuntimeValue::MutexGuard(_) => self.resolve_value_for_op_ref(left_ref)?,
                    other => other.clone(),
                };
                self.set_reg_value(*dst, boolean(op, left, right)?);
            }
            VMInstruction::Range {
                dst,
                from,
                to,
                inclusive,
            } => {
                let from = self.resolve_value_for_op_ref(self.get_reg_value_ref(*from))?;
                let to = self.resolve_value_for_op_ref(self.get_reg_value_ref(*to))?;
                let (RuntimeValue::Int(from), RuntimeValue::Int(to)) = (from, to) else {
                    return Err(RuntimeError::UnexpectedType(RuntimeValue::Null));
                };
                let end = if *inclusive { to + 1 } else { to };
                self.set_reg_value(*dst, RuntimeValue::Range(from, end));
            }
            VMInstruction::List { dst, items } => {
                let mut values = Vec::with_capacity(items.len());
                for item in items {
                    values.push(self.get_reg_value(*item));
                }
                self.set_reg_value(
                    *dst,
                    RuntimeValue::List(Gc::new(crate::value::GcVec(values))),
                );
            }
            VMInstruction::Aggregate {
                dst,
                layout,
                fields,
            } => {
                let layout = block
                    .aggregate_layouts
                    .get(*layout as usize)
                    .ok_or_else(|| {
                        RuntimeError::InvalidBytecode("invalid aggregate layout".to_string())
                    })?;
                let mut entries = Vec::with_capacity(layout.members.len());
                for (name, reg) in layout.members.iter().zip(fields.iter()) {
                    entries.push((name.clone(), self.get_reg_value(*reg)));
                }
                self.set_reg_value(
                    *dst,
                    RuntimeValue::Aggregate(
                        layout.name.clone(),
                        Gc::new(crate::value::GcMap(ObjectMap(entries))),
                    ),
                );
            }
            VMInstruction::Enum {
                dst,
                name,
                variant,
                payload,
            } => {
                let name = self.local_string(block, *name)?;
                let payload = payload.map(|reg| Gc::new(self.get_reg_value(reg)));
                self.set_reg_value(
                    *dst,
                    RuntimeValue::Enum(name.to_string(), *variant as usize, payload),
                );
            }
            VMInstruction::Call { dst, callee, args } => {
                let func = self.resolve_value_for_op_ref(self.get_reg_value_ref(*callee))?;
                let mut call_args: SmallVec<[RuntimeValue; 4]> =
                    SmallVec::with_capacity(args.len());
                for reg in args {
                    let frame = self.frames.len().saturating_sub(1);
                    match self.get_reg_value_ref(*reg) {
                        RuntimeValue::Aggregate(_, _)
                        | RuntimeValue::List(_)
                        | RuntimeValue::Enum(_, _, _)
                        | RuntimeValue::Option(_)
                        | RuntimeValue::Result(_)
                        | RuntimeValue::Ptr(_) => {
                            call_args.push(RuntimeValue::RegRef { frame, reg: *reg });
                        }
                        other => {
                            call_args.push(other.clone_for_arg());
                        }
                    }
                }
                match func {
                    RuntimeValue::Function { name, captures } => {
                        let callsite = (block.id.0 as usize, ip);
                        let func_opt = if let Some(cached) = self.caches.callsite.get(&callsite) {
                            Some(cached.clone())
                        } else if let Some(cached) = self.caches.call.get(name.as_str()) {
                            Some(cached.clone())
                        } else if let Some(x) = self.get_function(name.as_str()) {
                            self.caches
                                .call
                                .insert(name.as_str().to_string(), x.clone());
                            self.caches.callsite.insert(callsite, x.clone());
                            Some(x)
                        } else if let Some((prefix, _)) = name.split_once("->") {
                            let found = self
                                .registry
                                .functions
                                .iter()
                                .filter(|(k, _)| !self.moved_functions.contains(*k))
                                .find(|(k, _)| k.starts_with(prefix))
                                .map(|(_, v)| v.clone());
                            if let Some(ref func) = found {
                                self.caches
                                    .call
                                    .insert(name.as_str().to_string(), func.clone());
                                self.caches.callsite.insert(callsite, func.clone());
                            }
                            found
                        } else if let Some((_, short_name)) = name.rsplit_once(':') {
                            if let Some(found) = self.find_unique_function_by_suffix(short_name) {
                                self.caches
                                    .call
                                    .insert(name.as_str().to_string(), found.clone());
                                self.caches.callsite.insert(callsite, found.clone());
                                Some(found)
                            } else {
                                None
                            }
                        } else {
                            None
                        };

                        if let Some(func) = func_opt {
                            if captures.as_ref().is_empty()
                                && func.as_ref() as *const VMFunction
                                    == self.current_frame().func_ptr as *const VMFunction
                            {
                                if let Some(VMInstruction::Return {
                                    value: Some(ret_reg),
                                }) = block.instructions.get((ip as usize).saturating_add(1))
                                {
                                    if *ret_reg == *dst && call_args.len() == func.param_regs.len()
                                    {
                                        let base = self.local_map_base_for(func.as_ref());
                                        let start = self.current_frame().reg_start;
                                        let reg_count = func.reg_count as usize;
                                        self.reg_arena.truncate(start);
                                        self.reg_arena
                                            .resize(start + reg_count, RuntimeValue::Null);
                                        {
                                            let frame = self.current_frame_mut();
                                            frame.reg_count = reg_count;
                                            frame.local_map_base = Some(base);
                                            frame.local_map.clear();
                                            frame.acc = RuntimeValue::Null;
                                            frame.func_ptr =
                                                func.as_ref() as *const VMFunction as usize;
                                        }
                                        for (reg, arg) in func.param_regs.iter().zip(call_args) {
                                            let idx = *reg as usize;
                                            if idx < reg_count {
                                                self.reg_arena[start + idx] = arg;
                                            }
                                        }
                                        return Ok(TerminateValue::Jump(func.entry));
                                    }
                                }
                            }
                            let value =
                                self.run_function(func.as_ref(), call_args, captures.clone())?;
                            self.set_reg_value(*dst, value);
                        } else {
                            return Err(RuntimeError::FunctionNotFound(name.as_str().to_string()));
                        }
                    }
                    RuntimeValue::NativeFunction(func) => {
                        let result = func.run(self, call_args.into_vec())?;
                        self.set_reg_value(*dst, result);
                    }
                    RuntimeValue::ExternFunction(func) => {
                        let value = func.call(self, call_args.into_vec())?;
                        self.set_reg_value(*dst, value);
                    }
                    other => {
                        eprintln!(
                            "Invalid function call: {:?} at blk {:?} ip {} locals {:?}",
                            other, block.id, ip, block.local_strings
                        );
                        return Err(RuntimeError::InvalidFunctionCall);
                    }
                }
            }
            VMInstruction::Spawn { dst, callee } => {
                let resolved = self.resolve_value_for_op_ref(self.get_reg_value_ref(*callee))?;
                let to_spawn = match resolved {
                    RuntimeValue::Function { name, captures } => {
                        let resolved_caps: Vec<(String, RuntimeValue)> = captures
                            .as_ref()
                            .iter()
                            .map(|(k, v)| {
                                let resolved = self
                                    .resolve_value_for_op_ref(&v)
                                    .unwrap_or(RuntimeValue::Null);
                                (k.clone(), resolved)
                            })
                            .collect();
                        RuntimeValue::Function {
                            name,
                            captures: std::sync::Arc::new(resolved_caps),
                        }
                    }
                    other => other,
                };
                let wg = Arc::new(WaitGroupInner::new());
                wg.count.store(1, std::sync::atomic::Ordering::Release);
                self.spawn_async_task(to_spawn, Some(wg.clone()));
                self.set_reg_value(*dst, RuntimeValue::WaitGroup(wg));
            }
            VMInstruction::LoadMember { dst, value, member } => {
                let name = self.local_string(block, *member)?;
                let short_name = name.rsplit_once(':').map(|(_, short)| short);
                let tuple_index = name.parse::<usize>().ok();
                let value_ref = self.get_reg_value_ref(*value);

                let load_from = |val: RuntimeValue| -> Result<RuntimeValue, RuntimeError> {
                    match val {
                        RuntimeValue::Aggregate(None, map) => {
                            let idx = tuple_index
                                .ok_or(RuntimeError::UnexpectedType(RuntimeValue::Null))?;
                            Ok(map
                                .as_ref()
                                .0
                                .0
                                .get(idx)
                                .ok_or_else(|| RuntimeError::MissingMember {
                                    target: RuntimeValue::Aggregate(None, map.clone()),
                                    member: name.to_string(),
                                })?
                                .1
                                .clone())
                        }
                        RuntimeValue::Aggregate(Some(type_name), map) => {
                            if let Some(found) = map.as_ref().0.0.iter().find(|x| {
                                &x.0 == name || short_name.map_or(false, |short| x.0 == short)
                            }) {
                                return Ok(found.1.clone());
                            }

                            let mut candidates = Vec::with_capacity(2);
                            candidates.push(format!("{type_name}::{}", name));
                            if let Some(short) = short_name {
                                candidates.push(format!("{type_name}::{}", short));
                            }

                            for cand in candidates {
                                if let Some(func) = self.get_function(&cand) {
                                    return Ok(self.make_runtime_function(func.as_ref()));
                                }
                            }

                            let short_type = type_name.rsplit_once(':').map(|(_, short)| short);
                            if let Some(short_type) = short_type {
                                if let Some(func) =
                                    self.get_function(&format!("{short_type}::{}", name))
                                {
                                    return Ok(self.make_runtime_function(func.as_ref()));
                                }
                                if let Some(short) = short_name {
                                    if let Some(func) =
                                        self.get_function(&format!("{short_type}::{short}"))
                                    {
                                        return Ok(self.make_runtime_function(func.as_ref()));
                                    }
                                }
                            }

                            Err(RuntimeError::MissingMember {
                                target: RuntimeValue::Aggregate(Some(type_name), map),
                                member: name.to_string(),
                            })
                        }
                        RuntimeValue::Enum(_, _, Some(x)) if name == "next" || name == "0" => {
                            Ok(x.as_ref().clone())
                        }
                        RuntimeValue::Enum(_, _, Some(x)) => Ok(x.as_ref().clone()),
                        RuntimeValue::Enum(_, _, None) if name == "next" || name == "0" => {
                            Ok(RuntimeValue::Null)
                        }
                        RuntimeValue::Option(Some(x)) if name == "next" || name == "0" => {
                            Ok(x.as_ref().clone())
                        }
                        RuntimeValue::Option(None) if name == "next" || name == "0" => {
                            Ok(RuntimeValue::Null)
                        }
                        RuntimeValue::Result(Ok(x)) if name == "next" || name == "0" => {
                            Ok(x.as_ref().clone())
                        }
                        RuntimeValue::Result(Err(x)) if name == "next" || name == "0" => {
                            Ok(x.as_ref().clone())
                        }
                        RuntimeValue::Ptr(id) if name == "next" || name == "0" => Ok(self
                            .ptr_heap
                            .get(&id)
                            .cloned()
                            .unwrap_or(RuntimeValue::Null)),
                        other => Err(RuntimeError::UnexpectedType(other)),
                    }
                };

                let resolved = self.resolve_value_for_op_ref(value_ref)?;
                let val = load_from(resolved)?;
                self.set_reg_value(*dst, val);
            }
            VMInstruction::SetMember {
                target,
                member,
                value,
            } => {
                let name = self.local_string(block, *member)?;
                let value = self.get_reg_value(*value);
                let tuple_index = name.parse::<usize>().ok();

                let update_aggregate =
                    |agg_name: &Option<String>, mut map: Gc<crate::value::GcMap>| {
                        let entries = &mut Gc::make_mut(&mut map).0.0;
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
                        Ok(map)
                    };

                match self.get_reg_value(*target) {
                    RuntimeValue::Ref(ref_name) => {
                        let current = self
                            .variables
                            .get(&ref_name)
                            .cloned()
                            .ok_or(RuntimeError::DanglingRef(ref_name.clone()))?;

                        self.variables.insert(
                            ref_name,
                            match current {
                                RuntimeValue::Aggregate(name, map) => RuntimeValue::Aggregate(
                                    name.clone(),
                                    update_aggregate(&name, map.clone())?,
                                ),
                                other => return Err(RuntimeError::UnexpectedType(other)),
                            },
                        );
                    }
                    RuntimeValue::VarRef(id) => {
                        let current = self
                            .variables
                            .get_by_id(id)
                            .ok_or(RuntimeError::DanglingRef(format!("#{}", id)))?;
                        let updated = match current {
                            RuntimeValue::Aggregate(name, map) => RuntimeValue::Aggregate(
                                name.clone(),
                                update_aggregate(&name, map.clone())?,
                            ),
                            other => return Err(RuntimeError::UnexpectedType(other)),
                        };
                        let _ = self.variables.set_by_id(id, updated);
                    }
                    RuntimeValue::RegRef { frame, reg } => {
                        self.set_reg_value_in_frame(
                            frame,
                            reg,
                            match self.resolve_value_for_op_ref(
                                self.get_reg_value_in_frame_ref(frame, reg),
                            )? {
                                RuntimeValue::Aggregate(name, map) => RuntimeValue::Aggregate(
                                    name.clone(),
                                    update_aggregate(&name, map.clone())?,
                                ),
                                other => return Err(RuntimeError::UnexpectedType(other)),
                            },
                        );
                    }
                    RuntimeValue::Aggregate(name, map) => {
                        self.set_reg_value(
                            *target,
                            RuntimeValue::Aggregate(
                                name.clone(),
                                update_aggregate(&name, map.clone())?,
                            ),
                        );
                    }
                    other => return Err(RuntimeError::UnexpectedType(other)),
                }
            }
            VMInstruction::Index { dst, value, index } => {
                let resolve_idx = |len: usize, idx: i64| -> Option<usize> {
                    if len == 0 {
                        return None;
                    }
                    let mut i = idx;
                    if i < 0 {
                        i = len as i64 + i;
                    }
                    if i < 0 || i as usize >= len {
                        None
                    } else {
                        Some(i as usize)
                    }
                };
                let resolve_range = |len: usize, start: i64, end: i64| -> (usize, usize) {
                    let mut s = start;
                    let mut e = end;
                    if s < 0 {
                        s = len as i64 + s;
                    }
                    if e < 0 {
                        e = len as i64 + e;
                    }
                    if s < 0 {
                        s = 0;
                    }
                    if e < 0 {
                        e = 0;
                    }
                    let s = s.min(len as i64) as usize;
                    let e = e.min(len as i64) as usize;
                    if e < s { (s, s) } else { (s, e) }
                };
                let value_resolved =
                    self.resolve_value_for_op_ref(self.get_reg_value_ref(*value))?;
                let index_val = self.get_reg_value(*index);
                let val = match value_resolved {
                    RuntimeValue::List(x) => match index_val {
                        RuntimeValue::Int(index) => resolve_idx(x.as_ref().0.len(), index)
                            .and_then(|i| x.as_ref().0.get(i).cloned())
                            .unwrap_or(RuntimeValue::Null),
                        RuntimeValue::Range(start, end) => {
                            let (s, e) = resolve_range(x.as_ref().0.len(), start, end);
                            let slice = x.as_ref().0[s..e].to_vec();
                            RuntimeValue::List(Gc::new(crate::value::GcVec(slice)))
                        }
                        _ => return Err(RuntimeError::UnexpectedType(RuntimeValue::Null)),
                    },
                    RuntimeValue::Ref(x) => match self
                        .variables
                        .get(&x)
                        .cloned()
                        .unwrap_or(RuntimeValue::Null)
                    {
                        RuntimeValue::List(list) => match index_val {
                            RuntimeValue::Int(index) => resolve_idx(list.as_ref().0.len(), index)
                                .and_then(|i| list.as_ref().0.get(i).cloned())
                                .unwrap_or(RuntimeValue::Null),
                            RuntimeValue::Range(start, end) => {
                                let (s, e) = resolve_range(list.as_ref().0.len(), start, end);
                                let slice = list.as_ref().0[s..e].to_vec();
                                RuntimeValue::List(Gc::new(crate::value::GcVec(slice)))
                            }
                            _ => return Err(RuntimeError::UnexpectedType(RuntimeValue::Null)),
                        },
                        other => return Err(RuntimeError::UnexpectedType(other)),
                    },
                    RuntimeValue::VarRef(id) => {
                        match self.variables.get_by_id(id).unwrap_or(RuntimeValue::Null) {
                            RuntimeValue::List(list) => match index_val {
                                RuntimeValue::Int(index) => {
                                    resolve_idx(list.as_ref().0.len(), index)
                                        .and_then(|i| list.as_ref().0.get(i).cloned())
                                        .unwrap_or(RuntimeValue::Null)
                                }
                                RuntimeValue::Range(start, end) => {
                                    let (s, e) = resolve_range(list.as_ref().0.len(), start, end);
                                    let slice = list.as_ref().0[s..e].to_vec();
                                    RuntimeValue::List(Gc::new(crate::value::GcVec(slice)))
                                }
                                _ => return Err(RuntimeError::UnexpectedType(RuntimeValue::Null)),
                            },
                            other => return Err(RuntimeError::UnexpectedType(other)),
                        }
                    }
                    RuntimeValue::RegRef { frame, reg } => {
                        match self.get_reg_value_in_frame(frame, reg) {
                            RuntimeValue::List(list) => match index_val {
                                RuntimeValue::Int(index) => {
                                    resolve_idx(list.as_ref().0.len(), index)
                                        .and_then(|i| list.as_ref().0.get(i).cloned())
                                        .unwrap_or(RuntimeValue::Null)
                                }
                                RuntimeValue::Range(start, end) => {
                                    let (s, e) = resolve_range(list.as_ref().0.len(), start, end);
                                    let slice = list.as_ref().0[s..e].to_vec();
                                    RuntimeValue::List(Gc::new(crate::value::GcVec(slice)))
                                }
                                _ => return Err(RuntimeError::UnexpectedType(RuntimeValue::Null)),
                            },
                            other => return Err(RuntimeError::UnexpectedType(other)),
                        }
                    }
                    RuntimeValue::Str(s) => {
                        let v = s.chars().collect::<Vec<char>>();
                        match index_val {
                            RuntimeValue::Int(index) => resolve_idx(v.len(), index)
                                .and_then(|i| v.get(i).copied())
                                .map(RuntimeValue::Char)
                                .unwrap_or(RuntimeValue::Null),
                            RuntimeValue::Range(start, end) => {
                                let (s, e) = resolve_range(v.len(), start, end);
                                let slice: String = v[s..e].iter().collect();
                                RuntimeValue::Str(Arc::new(slice))
                            }
                            _ => return Err(RuntimeError::UnexpectedType(RuntimeValue::Null)),
                        }
                    }
                    RuntimeValue::Enum(_, _, Some(x)) => x.as_ref().clone(),
                    RuntimeValue::Option(Some(x)) => x.as_ref().clone(),
                    RuntimeValue::Result(Ok(x)) => x.as_ref().clone(),
                    RuntimeValue::Result(Err(x)) => x.as_ref().clone(),
                    other => return Err(RuntimeError::UnexpectedType(other)),
                };
                self.set_reg_value(*dst, val);
            }
            VMInstruction::SetIndex {
                target,
                index,
                value,
            } => {
                let index_val = self.get_reg_value(*index);
                let value = self.get_reg_value(*value);
                let RuntimeValue::Int(index) = index_val else {
                    return Err(RuntimeError::UnexpectedType(RuntimeValue::Null));
                };
                let resolve_idx = |len: usize, idx: i64| -> Result<usize, RuntimeError> {
                    let resolved = if idx < 0 { len as i64 + idx } else { idx };
                    if resolved < 0 || resolved as usize >= len {
                        return Err(RuntimeError::StackUnderflow);
                    }
                    Ok(resolved as usize)
                };
                let target_val_ref = self.get_reg_value_ref(*target);
                match target_val_ref {
                    RuntimeValue::List(list) => {
                        let mut list = list.clone();
                        let vec = &mut Gc::make_mut(&mut list).0;
                        let idx = resolve_idx(vec.len(), index)?;
                        vec[idx] = value;
                        self.set_reg_value(*target, RuntimeValue::List(list));
                    }
                    RuntimeValue::Ref(id) => {
                        if let Some(RuntimeValue::List(list)) =
                            self.variables.get(id.as_str()).cloned()
                        {
                            let mut list = list.clone();
                            let vec = &mut Gc::make_mut(&mut list).0;
                            let idx = resolve_idx(vec.len(), index)?;
                            vec[idx] = value;
                            self.variables.insert(id.clone(), RuntimeValue::List(list));
                        }
                    }
                    RuntimeValue::VarRef(id) => {
                        if let Some(RuntimeValue::List(list)) = self.variables.get_by_id(*id) {
                            let mut list = list.clone();
                            let vec = &mut Gc::make_mut(&mut list).0;
                            let idx = resolve_idx(vec.len(), index)?;
                            vec[idx] = value;
                            let _ = self.variables.set_by_id(*id, RuntimeValue::List(list));
                        }
                    }
                    RuntimeValue::RegRef { frame, reg } => {
                        if let RuntimeValue::List(list) = self.get_reg_value_in_frame(*frame, *reg)
                        {
                            let mut list = list.clone();
                            let vec = &mut Gc::make_mut(&mut list).0;
                            let idx = resolve_idx(vec.len(), index)?;
                            vec[idx] = value;
                            self.set_reg_value_in_frame(*frame, *reg, RuntimeValue::List(list));
                        }
                    }
                    _ => return Err(RuntimeError::UnexpectedType(RuntimeValue::Null)),
                }
            }
            VMInstruction::Ref { dst, value } => {
                let out = match self.get_reg_value(*value) {
                    RuntimeValue::Ref(name) => RuntimeValue::Ref(name),
                    RuntimeValue::VarRef(id) => RuntimeValue::VarRef(id),
                    RuntimeValue::RegRef { frame, reg } => RuntimeValue::RegRef { frame, reg },
                    other => {
                        let name = self.get_ref_id().to_string();
                        let id = self.variables.insert_str_with_id(&name, other);
                        RuntimeValue::VarRef(id)
                    }
                };
                self.set_reg_value(*dst, out);
            }
            VMInstruction::Deref { dst, value } => {
                let out = match self.get_reg_value(*value) {
                    RuntimeValue::Ref(x) => self
                        .variables
                        .get(&x)
                        .cloned()
                        .ok_or(RuntimeError::DanglingRef(x))?,
                    RuntimeValue::VarRef(id) => self
                        .variables
                        .get_by_id(id)
                        .ok_or(RuntimeError::DanglingRef(format!("#{}", id)))?,
                    RuntimeValue::RegRef { frame, reg } => self.get_reg_value_in_frame(frame, reg),
                    RuntimeValue::MutexGuard(guard) => guard.get_clone(),
                    other => other,
                };
                self.set_reg_value(*dst, out);
            }
            VMInstruction::SetRef { target, value } => {
                let target = self.get_reg_value(*target);
                let value = self.get_reg_value(*value);
                match target {
                    RuntimeValue::Ref(name) => {
                        self.variables.insert(name, value);
                    }
                    RuntimeValue::VarRef(id) => {
                        let _ = self.variables.set_by_id(id, value);
                    }
                    RuntimeValue::RegRef { frame, reg } => {
                        self.set_reg_value_in_frame(frame, reg, value);
                    }
                    RuntimeValue::MutexGuard(guard) => {
                        guard.set_value(value);
                    }
                    _ => return Err(RuntimeError::InvalidBytecode("invalid ref".to_string())),
                }
            }
            VMInstruction::Jump(target) => return Ok(TerminateValue::Jump(*target)),
            VMInstruction::Branch {
                cond,
                then_block,
                else_block,
            } => {
                let cond_val = self.resolve_value_for_op_ref(self.get_reg_value_ref(*cond))?;
                match cond_val {
                    RuntimeValue::Bool(true) => return Ok(TerminateValue::Jump(*then_block)),
                    RuntimeValue::Bool(false) => return Ok(TerminateValue::Jump(*else_block)),
                    x => return Err(RuntimeError::UnexpectedType(x)),
                }
            }
            VMInstruction::Return { value } => {
                return if let Some(reg) = value {
                    Ok(TerminateValue::Return(self.get_reg_value(*reg)))
                } else {
                    Ok(TerminateValue::Return(RuntimeValue::Null))
                };
            }
            VMInstruction::Noop => {}
        }

        Ok(TerminateValue::None)
    }
}
