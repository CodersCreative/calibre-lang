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
        RuntimeValue, TerminateValue,
        operation::{binary, boolean, comparison},
    },
};

impl VM {
    #[inline(always)]
    fn materialize_arg(&mut self, value: RuntimeValue) -> RuntimeValue {
        match value {
            RuntimeValue::RegRef { frame, reg } => RuntimeValue::RegRef { frame, reg },
            other => other,
        }
    }

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
        captures: Vec<(String, RuntimeValue)>,
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
        captures: Vec<(String, RuntimeValue)>,
        budget: usize,
        state: &mut crate::TaskState,
    ) -> Result<Option<RuntimeValue>, RuntimeError>
    where
        I: IntoIterator<Item = RuntimeValue>,
    {
        let prev_vars: Vec<(String, Option<RuntimeValue>)> = captures
            .iter()
            .map(|(name, _)| (name.clone(), self.variables.get(name).cloned()))
            .collect();
        for (name, value) in captures {
            self.variables.insert(name, value);
        }

        if state.block.is_none() {
            let func_ptr = function as *const VMFunction as usize;
            self.push_frame(function.reg_count as usize, func_ptr);
            for (reg, arg) in function.param_regs.iter().zip(args) {
                self.set_reg_value(*reg, arg);
            }
            for (name, reg) in function
                .params
                .iter()
                .zip(function.param_regs.iter().copied())
            {
                self.current_frame_mut().local_map.insert(name.clone(), reg);
            }
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

        self.pop_frame();
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
                    return Err(RuntimeError::At(span, Box::new(e)));
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
                        let captures = self.capture_values(&captures, &mut seen);
                        self.set_reg_value(
                            *dst,
                            RuntimeValue::Function {
                                name: label,
                                captures,
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
                let resolved = self.resolve_var_name(name);
                let resolved_name = match resolved.clone() {
                    VarName::Func(func) => func,
                    VarName::Var(var) => var,
                    VarName::Global => name.to_string(),
                };

                if let Some(cache) = self.caches.globals.get(&resolved_name) {
                    self.set_reg_value(*dst, cache.clone());
                    return Ok(TerminateValue::None);
                }

                let mut value = None;
                if let Some(func) = self.registry.functions.get(name) {
                    value = Some(self.make_runtime_function(func));
                } else if let Some(var) = self.variables.get(name) {
                    value = Some(self.copy_saveable_into_runtime_var(var.clone()));
                } else if let Some((_, short_name)) = name.rsplit_once(':') {
                    if let Some((base, _)) = short_name.split_once("->") {
                        if let Some(func) = self.find_unique_function_by_suffix(base) {
                            value = Some(self.make_runtime_function(func.as_ref()));
                        } else if let Some(var_name) = self.find_unique_var_by_suffix(base) {
                            if let Some(var) = self.variables.get(&var_name) {
                                value = Some(self.copy_saveable_into_runtime_var(var.clone()));
                            }
                        }
                    }
                    if value.is_none() {
                        if let Some(func) = self.find_unique_function_by_suffix(short_name) {
                            value = Some(self.make_runtime_function(func.as_ref()));
                        } else if let Some(var_name) = self.find_unique_var_by_suffix(short_name) {
                            if let Some(var) = self.variables.get(&var_name) {
                                value = Some(self.copy_saveable_into_runtime_var(var.clone()));
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
                        } else if let Some(var_name) = self.find_unique_var_by_suffix(base) {
                            if let Some(var) = self.variables.get(&var_name) {
                                value = Some(self.copy_saveable_into_runtime_var(var.clone()));
                            }
                        }
                    }
                }

                let value = value.unwrap_or_else(|| match resolved {
                    VarName::Func(func) => self
                        .registry
                        .functions
                        .get(&func)
                        .map(|f| self.make_runtime_function(f))
                        .unwrap_or(RuntimeValue::Null),
                    VarName::Var(var) => self
                        .variables
                        .get(&var)
                        .cloned()
                        .map(|v| self.copy_saveable_into_runtime_var(v))
                        .unwrap_or(RuntimeValue::Null),
                    VarName::Global => RuntimeValue::Null,
                });

                self.caches.globals.insert(resolved_name, value.clone());
                self.set_reg_value(*dst, value);
            }
            VMInstruction::MoveGlobal { dst, name } => {
                let name = self.local_string(block, *name)?;
                let resolved = self.resolve_var_name(name);
                let mut value = None;
                if let Some(func) = self.registry.functions.remove(name) {
                    value = Some(self.make_runtime_function(&func));
                } else if let Some(var) = self.variables.remove(name) {
                    value = Some(self.move_saveable_into_runtime_var(var));
                } else if let Some((_, short_name)) = name.rsplit_once(':') {
                    if let Some(func) = self.find_unique_function_by_suffix(short_name) {
                        let _ = self.registry.functions.remove(&func.name);
                        value = Some(self.make_runtime_function(func.as_ref()));
                    } else if let Some(var_name) = self.find_unique_var_by_suffix(short_name) {
                        if let Some(var) = self.variables.remove(&var_name) {
                            value = Some(self.move_saveable_into_runtime_var(var));
                        }
                    }
                }

                let value = value.unwrap_or_else(|| match resolved.clone() {
                    VarName::Func(func) => {
                        if let Some(func) = self.registry.functions.remove(&func) {
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
                        if let Some(val) = self.variables.remove(&var) {
                            self.drop_runtime_value(val);
                        }
                    }
                    VarName::Func(func) => {
                        let _ = self.registry.functions.remove(&func);
                    }
                    VarName::Global => {}
                }
            }
            VMInstruction::StoreGlobal { name, src } => {
                let name = self.local_string(block, *name)?;
                let value = self.get_reg_value(*src);
                self.variables.insert(name.to_string(), value);
            }
            VMInstruction::SetLocalName { name, src } => {
                let name = self.local_string(block, *name)?;
                self.current_frame_mut()
                    .local_map
                    .insert(name.to_string(), *src);
            }
            VMInstruction::LoadGlobalRef { dst, name } => {
                let name = self.local_string_owned(block, *name)?;
                self.set_reg_value(*dst, RuntimeValue::Ref(name.clone()));
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
                    _ => {
                        let frame = self.frames.len().saturating_sub(1);
                        self.set_reg_value(*dst, RuntimeValue::RegRef { frame, reg: *src });
                    }
                }
            }
            VMInstruction::Copy { dst, src } => {
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
                let right = self.resolve_value_for_op(self.get_reg_value(*right))?;
                let left = self.resolve_value_for_op(self.get_reg_value(*left))?;
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
                let right = self.resolve_value_for_op(self.get_reg_value(*right))?;
                let left = self.resolve_value_for_op(self.current_frame().acc.clone())?;
                self.current_frame_mut().acc = binary(self, op, left, right)?;
            }
            VMInstruction::Comparison {
                dst,
                op,
                left,
                right,
            } => {
                let right = self.resolve_value_for_op(self.get_reg_value(*right))?;
                let left = self.resolve_value_for_op(self.get_reg_value(*left))?;
                let cmp_val = comparison(op, left, right)?;
                self.set_reg_value(*dst, cmp_val);
            }
            VMInstruction::Boolean {
                dst,
                op,
                left,
                right,
            } => {
                let right = self.resolve_value_for_op(self.get_reg_value(*right))?;
                let left = self.resolve_value_for_op(self.get_reg_value(*left))?;
                self.set_reg_value(*dst, boolean(op, left, right)?);
            }
            VMInstruction::Range {
                dst,
                from,
                to,
                inclusive,
            } => {
                let from = self.resolve_value_for_op(self.get_reg_value(*from))?;
                let to = self.resolve_value_for_op(self.get_reg_value(*to))?;
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
                let mut entries = Vec::new();
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
                let func = self.get_reg_value(*callee);
                let mut call_args: SmallVec<[RuntimeValue; 4]> =
                    SmallVec::with_capacity(args.len());
                for reg in args {
                    let arg_val = self.get_reg_value(*reg);
                    let arg_to_pass = match &arg_val {
                        RuntimeValue::Aggregate(_, _)
                        | RuntimeValue::List(_)
                        | RuntimeValue::Enum(_, _, _)
                        | RuntimeValue::Option(_)
                        | RuntimeValue::Result(_)
                        | RuntimeValue::Ptr(_) => {
                            let frame = self.frames.len().saturating_sub(1);
                            RuntimeValue::RegRef { frame, reg: *reg }
                        }
                        _ => self.materialize_arg(arg_val),
                    };
                    call_args.push(arg_to_pass);
                }
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
                            let value = self.run_function(func.as_ref(), call_args, captures)?;
                            self.set_reg_value(*dst, value);
                        } else {
                            return Err(RuntimeError::FunctionNotFound(name));
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
            VMInstruction::Spawn { callee } => {
                let func = self.get_reg_value(*callee);
                let resolved = self.resolve_value_for_op(func)?;
                let to_spawn = match resolved {
                    RuntimeValue::Function { name, captures } => {
                        let resolved_caps = captures
                            .into_iter()
                            .map(|(k, v)| {
                                let resolved =
                                    self.resolve_value_for_op(v).unwrap_or(RuntimeValue::Null);
                                (k, resolved)
                            })
                            .collect();
                        RuntimeValue::Function {
                            name,
                            captures: resolved_caps,
                        }
                    }
                    other => other,
                };
                self.spawn_async_task(to_spawn);
            }
            VMInstruction::LoadMember { dst, value, member } => {
                let name = self.local_string(block, *member)?;
                let short_name = name.rsplit_once(':').map(|(_, short)| short);
                let tuple_index = name.parse::<usize>().ok();
                let value = self.get_reg_value(*value);

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
                                .ok_or(RuntimeError::StackUnderflow)?
                                .1
                                .clone())
                        }
                        RuntimeValue::Aggregate(Some(_), map) => Ok(map
                            .as_ref()
                            .0
                            .0
                            .iter()
                            .find(|x| {
                                &x.0 == name || short_name.map_or(false, |short| x.0 == short)
                            })
                            .ok_or(RuntimeError::StackUnderflow)?
                            .1
                            .clone()),
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

                let resolved = self.resolve_value_for_op(value)?;
                let val = load_from(resolved.clone())?;
                // debug logging removed
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
                                    update_aggregate(&name, &map)?,
                                ),
                                other => return Err(RuntimeError::UnexpectedType(other)),
                            },
                        );
                    }
                    RuntimeValue::RegRef { frame, reg } => {
                        let current = self.get_reg_value_in_frame(frame, reg);

                        self.set_reg_value_in_frame(
                            frame,
                            reg,
                            match self.resolve_value_for_op(current)? {
                                RuntimeValue::Aggregate(name, map) => RuntimeValue::Aggregate(
                                    name.clone(),
                                    update_aggregate(&name, &map)?,
                                ),
                                other => return Err(RuntimeError::UnexpectedType(other)),
                            },
                        );
                    }
                    RuntimeValue::Aggregate(name, map) => {
                        self.set_reg_value(
                            *target,
                            RuntimeValue::Aggregate(name.clone(), update_aggregate(&name, &map)?),
                        );
                    }
                    other => return Err(RuntimeError::UnexpectedType(other)),
                }
            }
            VMInstruction::Index { dst, value, index } => {
                let value = self.get_reg_value(*value);
                let RuntimeValue::Int(index) = self.get_reg_value(*index) else {
                    return Err(RuntimeError::UnexpectedType(RuntimeValue::Null));
                };
                let val = match value {
                    RuntimeValue::List(x) => x
                        .as_ref()
                        .0
                        .get(index as usize)
                        .cloned()
                        .unwrap_or(RuntimeValue::Null),
                    RuntimeValue::Ref(x) => match self
                        .variables
                        .get(&x)
                        .cloned()
                        .unwrap_or(RuntimeValue::Null)
                    {
                        RuntimeValue::List(list) => list
                            .as_ref()
                            .0
                            .get(index as usize)
                            .cloned()
                            .unwrap_or(RuntimeValue::Null),
                        RuntimeValue::Aggregate(None, map) => map
                            .as_ref()
                            .0
                            .0
                            .get(index as usize)
                            .ok_or(RuntimeError::StackUnderflow)?
                            .1
                            .clone(),
                        other => other,
                    },
                    RuntimeValue::RegRef { frame, reg } => {
                        match self.get_reg_value_in_frame(frame, reg) {
                            RuntimeValue::List(list) => list
                                .as_ref()
                                .0
                                .get(index as usize)
                                .cloned()
                                .unwrap_or(RuntimeValue::Null),
                            RuntimeValue::Aggregate(None, map) => map
                                .as_ref()
                                .0
                                .0
                                .get(index as usize)
                                .ok_or(RuntimeError::StackUnderflow)?
                                .1
                                .clone(),
                            other => other,
                        }
                    }
                    RuntimeValue::Aggregate(None, map) => map
                        .as_ref()
                        .0
                        .0
                        .get(index as usize)
                        .ok_or(RuntimeError::StackUnderflow)?
                        .1
                        .clone(),
                    other => other,
                };
                self.set_reg_value(*dst, val);
            }
            VMInstruction::SetIndex {
                target,
                index,
                value,
            } => {
                let index = self.get_reg_value(*index);
                let value = self.get_reg_value(*value);
                let RuntimeValue::Int(index) = index else {
                    return Err(RuntimeError::UnexpectedType(RuntimeValue::Null));
                };
                let target_val = self.get_reg_value(*target);
                match target_val {
                    RuntimeValue::List(list) => {
                        let mut vec = list.as_ref().0.clone();
                        if index as usize >= vec.len() {
                            return Err(RuntimeError::StackUnderflow);
                        }
                        vec[index as usize] = value;
                        self.set_reg_value(
                            *target,
                            RuntimeValue::List(Gc::new(crate::value::GcVec(vec))),
                        );
                    }
                    RuntimeValue::Aggregate(None, map) => {
                        let mut entries = map.as_ref().0.0.clone();
                        if index as usize >= entries.len() {
                            return Err(RuntimeError::StackUnderflow);
                        }
                        entries[index as usize].1 = value;
                        self.set_reg_value(
                            *target,
                            RuntimeValue::Aggregate(
                                None,
                                Gc::new(crate::value::GcMap(ObjectMap(entries))),
                            ),
                        );
                    }
                    RuntimeValue::Ref(id) => {
                        if let Some(RuntimeValue::List(list)) = self.variables.get(&id).cloned() {
                            let mut vec = list.as_ref().0.clone();
                            if index as usize >= vec.len() {
                                return Err(RuntimeError::StackUnderflow);
                            }
                            vec[index as usize] = value;
                            self.variables
                                .insert(id, RuntimeValue::List(Gc::new(crate::value::GcVec(vec))));
                        } else if let Some(RuntimeValue::Aggregate(None, map)) =
                            self.variables.get(&id).cloned()
                        {
                            let mut entries = map.as_ref().0.0.clone();
                            if index as usize >= entries.len() {
                                return Err(RuntimeError::StackUnderflow);
                            }
                            entries[index as usize].1 = value;
                            self.variables.insert(
                                id,
                                RuntimeValue::Aggregate(
                                    None,
                                    Gc::new(crate::value::GcMap(ObjectMap(entries))),
                                ),
                            );
                        }
                    }
                    RuntimeValue::RegRef { frame, reg } => {
                        if let RuntimeValue::List(list) = self.get_reg_value_in_frame(frame, reg) {
                            let mut vec = list.as_ref().0.clone();
                            if index as usize >= vec.len() {
                                return Err(RuntimeError::StackUnderflow);
                            }
                            vec[index as usize] = value;
                            self.set_reg_value_in_frame(
                                frame,
                                reg,
                                RuntimeValue::List(Gc::new(crate::value::GcVec(vec))),
                            );
                        } else if let RuntimeValue::Aggregate(None, map) =
                            self.get_reg_value_in_frame(frame, reg)
                        {
                            let mut entries = map.as_ref().0.0.clone();
                            if index as usize >= entries.len() {
                                return Err(RuntimeError::StackUnderflow);
                            }
                            entries[index as usize].1 = value;
                            self.set_reg_value_in_frame(
                                frame,
                                reg,
                                RuntimeValue::Aggregate(
                                    None,
                                    Gc::new(crate::value::GcMap(ObjectMap(entries))),
                                ),
                            );
                        }
                    }
                    _ => return Err(RuntimeError::UnexpectedType(RuntimeValue::Null)),
                }
            }
            VMInstruction::Ref { dst, value } => {
                let out = match self.get_reg_value(*value) {
                    RuntimeValue::Ref(name) => RuntimeValue::Ref(name),
                    RuntimeValue::RegRef { frame, reg } => RuntimeValue::RegRef { frame, reg },
                    other => {
                        let name = self.get_ref_id().to_string();
                        self.variables.insert(name.clone(), other);
                        RuntimeValue::Ref(name)
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
                let cond_val = self.resolve_value_for_op(self.get_reg_value(*cond))?;
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
