use super::*;

impl VM {
    pub(super) fn run_instruction(
        &mut self,
        instruction: &VMInstruction,
        block: &VMBlock,
        ip: u32,
        prev_block: Option<BlockId>,
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
                            match unsafe { libloading::Library::new(&candidate) } {
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
                let cached = self
                    .caches
                    .globals_direct
                    .get(name)
                    .or_else(|| self.caches.globals.get(name));
                if let Some(cache) = cached {
                    self.set_reg_value(*dst, cache.clone());
                    return Ok(TerminateValue::None);
                }

                let mut resolved_name: Option<String> = None;
                let value = if let Some((v, n)) = self.try_resolve_global_runtime_value(name) {
                    resolved_name = Some(n);
                    v
                } else {
                    let resolved = self.resolve_var_name(name);
                    match resolved {
                        VarName::Func(func) => {
                            if let Some(f) = self.get_function_ref(&func) {
                                resolved_name = Some(func);
                                self.make_runtime_function(f)
                            } else {
                                RuntimeValue::Null
                            }
                        }
                        VarName::Var(var) => {
                            if let Some(v) = self.variables.get(&var) {
                                resolved_name = Some(var);
                                self.resolve_saveable_runtime_value_ref(v)
                            } else {
                                RuntimeValue::Null
                            }
                        }
                        VarName::Global => RuntimeValue::Null,
                    }
                };
                if let Some(resolved_name) = resolved_name
                    && !matches!(value, RuntimeValue::Null)
                {
                    self.caches.globals.insert(resolved_name, value.clone());
                }
                self.set_reg_value(*dst, value);
            }
            VMInstruction::MoveGlobal { dst, name } => {
                let name = self.local_string(block, *name)?;
                let resolved = self.resolve_var_name(name);
                let value =
                    self.try_move_global_runtime_value(name)
                        .unwrap_or_else(|| match &resolved {
                            VarName::Func(func) => {
                                if let Some(func) = self.take_function(func) {
                                    self.make_runtime_function(&func)
                                } else {
                                    RuntimeValue::Null
                                }
                            }
                            VarName::Var(var) => {
                                if let Some(var) = self.variables.remove(var) {
                                    self.resolve_saveable_runtime_value_ref(&var)
                                } else {
                                    RuntimeValue::Null
                                }
                            }
                            VarName::Global => RuntimeValue::Null,
                        });

                self.set_reg_value(*dst, value);
                self.invalidate_name_resolution_caches();
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
                self.invalidate_name_resolution_caches();
            }
            VMInstruction::StoreGlobal { name, src } => {
                let name = self.local_string(block, *name)?;
                let value = self.get_reg_value(*src);
                let existed = self.variables.contains_key(name);
                if let Some(id) = self.global_id_cached(name) {
                    let _ = self.variables.set_by_id(id, value);
                } else {
                    let id = self.variables.insert_str_with_id(name, value);
                    self.caches.globals_id.insert(name.to_string(), id);
                }
                if !existed {
                    self.invalidate_name_resolution_caches();
                }
            }
            VMInstruction::SetLocalName { name, src } => {
                let interned = self.intern_local_string(block, *name)?;
                self.current_frame_mut().local_map.insert(interned, *src);
            }
            VMInstruction::LoadGlobalRef { dst, name } => {
                let name = self.local_string(block, *name)?;
                if let Some(id) = self.global_id_cached(name) {
                    self.set_reg_value(*dst, RuntimeValue::VarRef(id));
                } else {
                    self.set_reg_value(*dst, RuntimeValue::Ref(name.to_string()));
                }
            }
            VMInstruction::LoadRegRef { dst, src } => {
                let value = match self.get_reg_value(*src) {
                    RuntimeValue::RegRef { frame, reg } => RuntimeValue::RegRef { frame, reg },
                    RuntimeValue::Ref(name) => RuntimeValue::Ref(name),
                    RuntimeValue::VarRef(id) => RuntimeValue::VarRef(id),
                    _ => RuntimeValue::RegRef {
                        frame: self.frames.len().saturating_sub(1),
                        reg: *src,
                    },
                };
                self.set_reg_value(*dst, value);
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
                failure_mode,
            } => {
                let value = self.get_reg_value(*src);
                let conversion = value.convert(self, &data_type.data_type);
                let converted = match failure_mode {
                    calibre_parser::ast::AsFailureMode::Panic => match conversion {
                        Ok(value) => value,
                        Err(err) => {
                            return Err(RuntimeError::Panic(Some(format!(
                                "failed `as!` conversion to {}: {}",
                                data_type, err
                            ))));
                        }
                    },
                    calibre_parser::ast::AsFailureMode::Option => match conversion {
                        Ok(value) => RuntimeValue::Option(Some(Gc::new(value))),
                        Err(_) => RuntimeValue::Option(None),
                    },
                    calibre_parser::ast::AsFailureMode::Result => match conversion {
                        Ok(value) => RuntimeValue::Result(Ok(Gc::new(value))),
                        Err(err) => RuntimeValue::Result(Err(Gc::new(RuntimeValue::Str(
                            Arc::new(err.to_string()),
                        )))),
                    },
                };
                self.set_reg_value(*dst, converted);
            }
            VMInstruction::Binary {
                dst,
                op,
                left,
                right,
            } => {
                if let Some(value) = self.try_fast_int_binary(*op, *left, *right) {
                    self.set_reg_value(*dst, value);
                    return Ok(TerminateValue::None);
                }
                let left = self.resolve_operand_value(self.get_reg_value(*left))?;
                let right = self.resolve_operand_value(self.get_reg_value(*right))?;
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
                if let RuntimeValue::Int(left) = self.current_frame().acc.clone()
                    && let RuntimeValue::Int(right) = self.get_reg_value(*right)
                    && let Some(value) = {
                        let out = match op {
                            BinaryOperator::Add => {
                                Some(RuntimeValue::Int(left.wrapping_add(right)))
                            }
                            BinaryOperator::Sub => {
                                Some(RuntimeValue::Int(left.wrapping_sub(right)))
                            }
                            BinaryOperator::Mul => {
                                Some(RuntimeValue::Int(left.wrapping_mul(right)))
                            }
                            BinaryOperator::Div => {
                                if right == 0 {
                                    None
                                } else {
                                    Some(RuntimeValue::Int(left / right))
                                }
                            }
                            BinaryOperator::Mod => {
                                if right == 0 {
                                    None
                                } else {
                                    Some(RuntimeValue::Int(left % right))
                                }
                            }
                            BinaryOperator::BitAnd => Some(RuntimeValue::Int(left & right)),
                            BinaryOperator::BitOr => Some(RuntimeValue::Int(left | right)),
                            BinaryOperator::BitXor => Some(RuntimeValue::Int(left ^ right)),
                            BinaryOperator::Shl => {
                                Some(RuntimeValue::Int(left.wrapping_shl(right as u32)))
                            }
                            BinaryOperator::Shr => {
                                Some(RuntimeValue::Int(left.wrapping_shr(right as u32)))
                            }
                            BinaryOperator::Pow => None,
                        };
                        out
                    }
                {
                    self.current_frame_mut().acc = value;
                    return Ok(TerminateValue::None);
                }
                let right = self.resolve_operand_value(self.get_reg_value(*right))?;
                let left_raw = {
                    let frame = self.current_frame_mut();
                    std::mem::replace(&mut frame.acc, RuntimeValue::Null)
                };
                let left = self.resolve_operand_value(left_raw)?;
                self.current_frame_mut().acc = binary(self, op, left, right)?;
            }
            VMInstruction::Comparison {
                dst,
                op,
                left,
                right,
            } => {
                if let Some(cmp_val) = self.try_fast_int_comparison(*op, *left, *right) {
                    self.set_reg_value(*dst, cmp_val);
                    return Ok(TerminateValue::None);
                }
                let right = self.resolve_operand_value(self.get_reg_value(*right))?;
                let left = self.resolve_operand_value(self.get_reg_value(*left))?;
                let cmp_val = comparison(op, left, right)?;
                self.set_reg_value(*dst, cmp_val);
            }
            VMInstruction::Boolean {
                dst,
                op,
                left,
                right,
            } => {
                let right = self.resolve_operand_value(self.get_reg_value(*right))?;
                let left = self.resolve_operand_value(self.get_reg_value(*left))?;
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
                let as_range_bound = |value: RuntimeValue| -> Result<i64, RuntimeError> {
                    match value {
                        RuntimeValue::Int(v) => Ok(v),
                        RuntimeValue::UInt(v) => Ok(v as i64),
                        RuntimeValue::Float(v) => Ok(v as i64),
                        RuntimeValue::Bool(v) => Ok(v as i64),
                        RuntimeValue::Char(v) => Ok(v as i64),
                        RuntimeValue::List(v) => Ok(v.as_ref().0.len() as i64),
                        RuntimeValue::Aggregate(_, v) => Ok(v.as_ref().0.0.len() as i64),
                        RuntimeValue::Str(v) => Ok(v.len() as i64),
                        RuntimeValue::Range(from, to) => Ok((to - from).max(0)),
                        other => Err(RuntimeError::UnexpectedType(other)),
                    }
                };
                let from = as_range_bound(from)?;
                let to = as_range_bound(to)?;
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
                if let Some(type_name) = layout.name.clone()
                    && Self::is_gen_type_name(&type_name)
                {
                    let mut next_fn: Option<RuntimeValue> = None;
                    for (field, value) in entries.iter() {
                        let short = field.rsplit("::").next().unwrap_or(field.as_str());
                        match short {
                            "data" => next_fn = Some(value.clone()),
                            _ => {}
                        }
                    }
                    if let Some(RuntimeValue::Function { name, captures }) = next_fn {
                        let mut gen_vm = VM::new_shared(
                            self.registry.clone(),
                            self.mappings.clone(),
                            self.config.clone(),
                        );
                        gen_vm.variables = self.variables.clone();
                        gen_vm.ptr_heap = self.ptr_heap.clone();
                        gen_vm.moved_functions = self.moved_functions.clone();

                        self.set_reg_value(
                            *dst,
                            RuntimeValue::Generator {
                                type_name: Arc::new(type_name),
                                state: Arc::new(std::sync::Mutex::new(
                                    crate::value::GeneratorState {
                                        vm: gen_vm,
                                        function_name: name,
                                        captures,
                                        task_state: crate::TaskState::default(),
                                        index: 0,
                                        completed: false,
                                    },
                                )),
                            },
                        );
                        return Ok(TerminateValue::None);
                    }
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
            VMInstruction::CallDirect { dst, name, args } => {
                match self.resolve_direct_callsite_cached(block, ip, *name)? {
                    Some(PreparedDirectCall::Vm(func)) => {
                        let value = self.run_function_from_regs(
                            func.as_ref(),
                            args,
                            Self::empty_captures(),
                        )?;
                        self.set_reg_value(*dst, value);
                    }
                    Some(PreparedDirectCall::Native(func)) => {
                        let result = func.run(self, self.collect_call_args_vec(args))?;
                        self.set_reg_value(*dst, result);
                    }
                    Some(PreparedDirectCall::Extern(func)) => {
                        let value = func.call(self, self.collect_call_args_vec(args))?;
                        self.set_reg_value(*dst, value);
                    }
                    None => {
                        let func_name = self.local_string(block, *name)?;
                        return Err(RuntimeError::FunctionNotFound(func_name.to_string()));
                    }
                }
            }
            VMInstruction::CallSelf { dst, args } => {
                let func_ptr = self.current_frame().func_ptr as *const VMFunction;
                if func_ptr.is_null() {
                    return Err(RuntimeError::InvalidBytecode(
                        "missing current function frame".to_string(),
                    ));
                }
                let func = unsafe { &*func_ptr };
                if let Some(step) = self.try_trampoline_self_tail_call(block, ip, *dst, args, func)
                {
                    return Ok(step);
                }
                let value = self.run_function_from_regs(func, args, Self::empty_captures())?;
                self.set_reg_value(*dst, value);
            }
            VMInstruction::Call { dst, callee, args } => {
                let func = match self.get_reg_value_ref(*callee) {
                    RuntimeValue::Function { name, captures } => RuntimeValue::Function {
                        name: name.clone(),
                        captures: captures.clone(),
                    },
                    RuntimeValue::NativeFunction(func) => {
                        RuntimeValue::NativeFunction(func.clone())
                    }
                    RuntimeValue::ExternFunction(func) => {
                        RuntimeValue::ExternFunction(func.clone())
                    }
                    other => self.resolve_value_for_op_ref(other)?,
                };
                match func {
                    RuntimeValue::Function { name, captures } => {
                        let callsite = (self.current_frame().func_ptr, block.id.0 as usize, ip);
                        let func_opt = self.resolve_callable_cached(name.as_str(), callsite);

                        if let Some(func) = func_opt {
                            if captures.as_ref().is_empty()
                                && func.as_ref() as *const VMFunction
                                    == self.current_frame().func_ptr as *const VMFunction
                            {
                                if let Some(step) = self.try_trampoline_self_tail_call(
                                    block,
                                    ip,
                                    *dst,
                                    args,
                                    func.as_ref(),
                                ) {
                                    return Ok(step);
                                }
                            }
                            let value =
                                self.run_function_from_regs(func.as_ref(), args, captures.clone())?;
                            self.set_reg_value(*dst, value);
                        } else {
                            return Err(RuntimeError::FunctionNotFound(name.as_str().to_string()));
                        }
                    }
                    RuntimeValue::NativeFunction(func) => {
                        let result = func.run(self, self.collect_call_args_vec(args))?;
                        if let RuntimeValue::GeneratorSuspend(value) = result {
                            let yielded = *value;
                            self.set_reg_value(*dst, yielded.clone());
                            return Ok(TerminateValue::Yield {
                                block: block.id,
                                ip: ip as usize + 1,
                                prev_block,
                                yielded: Some(yielded),
                            });
                        }
                        self.set_reg_value(*dst, result);
                    }
                    RuntimeValue::ExternFunction(func) => {
                        let value = func.call(self, self.collect_call_args_vec(args))?;
                        self.set_reg_value(*dst, value);
                    }
                    _other => return Err(RuntimeError::InvalidFunctionCall),
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
                let short_name = if let Some(pos) = name.rfind("::") {
                    Some(&name[pos + 2..])
                } else {
                    None
                };
                let tuple_index = name.parse::<usize>().ok();
                let resolved = self.resolve_value_for_op_ref(self.get_reg_value_ref(*value))?;
                let val = match resolved {
                    RuntimeValue::Generator { type_name, state } => {
                        let member_short = short_name.unwrap_or(name);
                        match member_short {
                            "data" => RuntimeValue::NativeFunction(Arc::new(
                                crate::value::GeneratorResumeFn {
                                    state: state.clone(),
                                },
                            )),
                            "index" => {
                                let guard = state.lock().map_err(|_| {
                                    RuntimeError::UnexpectedType(RuntimeValue::Null)
                                })?;
                                RuntimeValue::Int(guard.index)
                            }
                            "done" => {
                                let guard = state.lock().map_err(|_| {
                                    RuntimeError::UnexpectedType(RuntimeValue::Null)
                                })?;
                                RuntimeValue::Bool(guard.completed)
                            }
                            _ => {
                                let mut path =
                                    String::with_capacity(type_name.len() + name.len() + 2);
                                path.push_str(type_name.as_str());
                                path.push_str("::");
                                path.push_str(name);
                                if let Some(func) = self.get_function_ref(&path) {
                                    self.make_runtime_function(func)
                                } else {
                                    let found = if let Some(short) = short_name {
                                        path.clear();
                                        path.push_str(type_name.as_str());
                                        path.push_str("::");
                                        path.push_str(short);
                                        self.get_function_ref(&path)
                                    } else {
                                        None
                                    }
                                    .or_else(|| {
                                        let short_type =
                                            type_name.rsplit_once(':').map(|(_, short)| short)?;
                                        path.clear();
                                        path.push_str(short_type);
                                        path.push_str("::");
                                        path.push_str(name);
                                        self.get_function_ref(&path).or_else(|| {
                                            let short = short_name?;
                                            path.clear();
                                            path.push_str(short_type);
                                            path.push_str("::");
                                            path.push_str(short);
                                            self.get_function_ref(&path)
                                        })
                                    });

                                    if let Some(func) = found {
                                        self.make_runtime_function(func)
                                    } else {
                                        return Err(RuntimeError::MissingMember {
                                            target: RuntimeValue::Generator { type_name, state },
                                            member: name.to_string(),
                                        });
                                    }
                                }
                            }
                        }
                    }
                    RuntimeValue::Aggregate(None, map) => {
                        let idx =
                            tuple_index.ok_or(RuntimeError::UnexpectedType(RuntimeValue::Null))?;
                        if let Some((_, value)) = map.as_ref().0.0.get(idx) {
                            value.clone()
                        } else {
                            return Err(RuntimeError::MissingMember {
                                target: RuntimeValue::Aggregate(None, map),
                                member: name.to_string(),
                            });
                        }
                    }
                    RuntimeValue::Aggregate(Some(type_name), map) => {
                        if let Some(idx) =
                            self.resolve_aggregate_member_slot(&type_name, &map, name, short_name)
                        {
                            map.0.0[idx].1.clone()
                        } else {
                            let mut path = String::with_capacity(type_name.len() + name.len() + 2);
                            path.push_str(&type_name);
                            path.push_str("::");
                            path.push_str(name);
                            if let Some(func) = self.get_function_ref(&path) {
                                self.make_runtime_function(func)
                            } else {
                                let found = if let Some(short) = short_name {
                                    path.clear();
                                    path.push_str(&type_name);
                                    path.push_str("::");
                                    path.push_str(short);
                                    self.get_function_ref(&path)
                                } else {
                                    None
                                }
                                .or_else(|| {
                                    let short_type =
                                        type_name.rsplit_once(':').map(|(_, short)| short)?;
                                    path.clear();
                                    path.push_str(short_type);
                                    path.push_str("::");
                                    path.push_str(name);
                                    self.get_function_ref(&path).or_else(|| {
                                        let short = short_name?;
                                        path.clear();
                                        path.push_str(short_type);
                                        path.push_str("::");
                                        path.push_str(short);
                                        self.get_function_ref(&path)
                                    })
                                });
                                if let Some(func) = found {
                                    self.make_runtime_function(func)
                                } else {
                                    return Err(RuntimeError::MissingMember {
                                        target: RuntimeValue::Aggregate(Some(type_name), map),
                                        member: name.to_string(),
                                    });
                                }
                            }
                        }
                    }
                    RuntimeValue::Enum(_, _, Some(x)) if name == "next" || name == "0" => {
                        x.as_ref().clone()
                    }
                    RuntimeValue::Enum(_, _, Some(x)) => x.as_ref().clone(),
                    RuntimeValue::Enum(_, _, None) if name == "next" || name == "0" => {
                        RuntimeValue::Null
                    }
                    RuntimeValue::Option(Some(x)) if name == "next" || name == "0" => {
                        x.as_ref().clone()
                    }
                    RuntimeValue::Option(None) if name == "next" || name == "0" => {
                        RuntimeValue::Null
                    }
                    RuntimeValue::Result(Ok(x)) if name == "next" || name == "0" => {
                        x.as_ref().clone()
                    }
                    RuntimeValue::Result(Err(x)) if name == "next" || name == "0" => {
                        x.as_ref().clone()
                    }
                    RuntimeValue::Ptr(id) if name == "next" || name == "0" => self
                        .ptr_heap
                        .get(&id)
                        .cloned()
                        .unwrap_or(RuntimeValue::Null),
                    other => return Err(RuntimeError::UnexpectedType(other)),
                };
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
                let short_name = if let Some(pos) = name.rfind("::") {
                    Some(&name[pos + 2..])
                } else {
                    None
                };

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
                                if let Some(entry) = entries.iter_mut().find(|entry| {
                                    entry.0 == *name
                                        || short_name.is_some_and(|short| entry.0 == short)
                                }) {
                                    entry.1 = value.clone();
                                } else {
                                    return Err(RuntimeError::StackUnderflow);
                                }
                            }
                            _ => return Err(RuntimeError::UnexpectedType(RuntimeValue::Null)),
                        }
                        Ok(map)
                    };
                let update_generator =
                    |generator_value: RuntimeValue| -> Result<RuntimeValue, RuntimeError> {
                        let RuntimeValue::Generator { type_name, state } = generator_value else {
                            return Err(RuntimeError::UnexpectedType(generator_value));
                        };

                        if !matches!(short_name.unwrap_or(name), "done" | "index") {
                            return Err(RuntimeError::MissingMember {
                                target: RuntimeValue::Generator { type_name, state },
                                member: name.to_string(),
                            });
                        }

                        let mut guard = state
                            .lock()
                            .map_err(|_| RuntimeError::UnexpectedType(RuntimeValue::Null))?;
                        match short_name.unwrap_or(name) {
                            "index" => match &value {
                                RuntimeValue::Int(x) => guard.index = (*x).max(0),
                                RuntimeValue::UInt(x) => guard.index = *x as i64,
                                other => return Err(RuntimeError::UnexpectedType(other.clone())),
                            },
                            "done" => match &value {
                                RuntimeValue::Bool(x) => guard.completed = *x,
                                other => return Err(RuntimeError::UnexpectedType(other.clone())),
                            },
                            _ => {}
                        }
                        drop(guard);

                        Ok(RuntimeValue::Generator { type_name, state })
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
                                RuntimeValue::Aggregate(name, map) => {
                                    let updated = update_aggregate(&name, map)?;
                                    RuntimeValue::Aggregate(name, updated)
                                }
                                RuntimeValue::Generator { .. } => update_generator(current)?,
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
                            RuntimeValue::Aggregate(name, map) => {
                                let updated = update_aggregate(&name, map)?;
                                RuntimeValue::Aggregate(name, updated)
                            }
                            RuntimeValue::Generator { .. } => update_generator(current)?,
                            other => return Err(RuntimeError::UnexpectedType(other)),
                        };
                        let _ = self.variables.set_by_id(id, updated);
                    }
                    RuntimeValue::RegRef { frame, reg } => {
                        let slot = self
                            .get_reg_value_in_frame_mut(frame, reg)
                            .ok_or(RuntimeError::StackUnderflow)?;
                        match slot {
                            RuntimeValue::Aggregate(name, map) => {
                                let updated = update_aggregate(name, map.clone())?;
                                *slot = RuntimeValue::Aggregate(name.clone(), updated);
                            }
                            RuntimeValue::Generator { .. } => {
                                let updated = update_generator(slot.clone())?;
                                *slot = updated;
                            }
                            other => {
                                return Err(RuntimeError::UnexpectedType(other.clone()));
                            }
                        }
                    }
                    RuntimeValue::Aggregate(name, map) => {
                        let updated = update_aggregate(&name, map)?;
                        self.set_reg_value(*target, RuntimeValue::Aggregate(name, updated));
                    }
                    current @ RuntimeValue::Generator { .. } => {
                        self.set_reg_value(*target, update_generator(current)?);
                    }
                    other => return Err(RuntimeError::UnexpectedType(other)),
                }
            }
            VMInstruction::Index { dst, value, index } => {
                let value_ref = self.get_reg_value_ref(*value);
                let index_ref = self.get_reg_value_ref(*index);
                if let RuntimeValue::List(list) = value_ref {
                    match index_ref {
                        RuntimeValue::UInt(i) => {
                            let out = list
                                .as_ref()
                                .0
                                .get(*i as usize)
                                .cloned()
                                .unwrap_or(RuntimeValue::Null);
                            self.set_reg_value(*dst, out);
                            return Ok(TerminateValue::None);
                        }
                        RuntimeValue::Int(i) if *i >= 0 => {
                            let out = list
                                .as_ref()
                                .0
                                .get(*i as usize)
                                .cloned()
                                .unwrap_or(RuntimeValue::Null);
                            self.set_reg_value(*dst, out);
                            return Ok(TerminateValue::None);
                        }
                        _ => {}
                    }
                }

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
                let index_val = self.get_reg_value(*index);
                let index_list =
                    |list: &Gc<crate::value::GcVec>| -> Result<RuntimeValue, RuntimeError> {
                        match &index_val {
                            RuntimeValue::Int(index) => {
                                Ok(resolve_idx(list.as_ref().0.len(), *index)
                                    .and_then(|i| list.as_ref().0.get(i).cloned())
                                    .unwrap_or(RuntimeValue::Null))
                            }
                            RuntimeValue::UInt(index) => Ok(list
                                .as_ref()
                                .0
                                .get(*index as usize)
                                .cloned()
                                .unwrap_or(RuntimeValue::Null)),
                            RuntimeValue::Range(start, end) => {
                                let (s, e) = resolve_range(list.as_ref().0.len(), *start, *end);
                                let slice = list.as_ref().0[s..e].to_vec();
                                Ok(RuntimeValue::List(Gc::new(crate::value::GcVec(slice))))
                            }
                            _ => Err(RuntimeError::UnexpectedType(RuntimeValue::Null)),
                        }
                    };
                let resolved = self.resolve_value_for_op_ref(self.get_reg_value_ref(*value))?;
                let val = match resolved {
                    RuntimeValue::List(list) => index_list(&list)?,
                    RuntimeValue::Range(start, end) => match &index_val {
                        RuntimeValue::Int(index) => {
                            let len = (end - start).max(0) as usize;
                            resolve_idx(len, *index)
                                .map(|i| RuntimeValue::Int(start + i as i64))
                                .unwrap_or(RuntimeValue::Null)
                        }
                        RuntimeValue::UInt(index) => {
                            let len = (end - start).max(0) as usize;
                            if (*index as usize) < len {
                                RuntimeValue::Int(start + *index as i64)
                            } else {
                                RuntimeValue::Null
                            }
                        }
                        RuntimeValue::Range(slice_start, slice_end) => {
                            let len = (end - start).max(0) as usize;
                            let (s, e) = resolve_range(len, *slice_start, *slice_end);
                            RuntimeValue::Range(start + s as i64, start + e as i64)
                        }
                        _ => return Err(RuntimeError::UnexpectedType(RuntimeValue::Null)),
                    },
                    RuntimeValue::Aggregate(None, tuple) => match &index_val {
                        RuntimeValue::Int(index) => resolve_idx(tuple.as_ref().0.0.len(), *index)
                            .and_then(|i| tuple.as_ref().0.0.get(i).map(|(_, v)| v.clone()))
                            .unwrap_or(RuntimeValue::Null),
                        RuntimeValue::UInt(index) => tuple
                            .as_ref()
                            .0
                            .0
                            .get(*index as usize)
                            .map(|(_, v)| v.clone())
                            .unwrap_or(RuntimeValue::Null),
                        RuntimeValue::Range(start, end) => {
                            let (s, e) = resolve_range(tuple.as_ref().0.0.len(), *start, *end);
                            let slice = tuple.as_ref().0.0[s..e].to_vec();
                            RuntimeValue::Aggregate(
                                None,
                                Gc::new(crate::value::GcMap(ObjectMap(slice))),
                            )
                        }
                        _ => return Err(RuntimeError::UnexpectedType(RuntimeValue::Null)),
                    },
                    RuntimeValue::Str(s) => match &index_val {
                        RuntimeValue::Int(index) => {
                            let resolved = if *index < 0 {
                                let len = s.chars().count();
                                resolve_idx(len, *index)
                            } else {
                                Some(*index as usize)
                            };
                            resolved
                                .and_then(|i| s.chars().nth(i))
                                .map(RuntimeValue::Char)
                                .unwrap_or(RuntimeValue::Null)
                        }
                        RuntimeValue::UInt(index) => s
                            .chars()
                            .nth(*index as usize)
                            .map(RuntimeValue::Char)
                            .unwrap_or(RuntimeValue::Null),
                        RuntimeValue::Range(start, end) => {
                            let v = s.chars().collect::<Vec<char>>();
                            let (s, e) = resolve_range(v.len(), *start, *end);
                            let slice: String = v[s..e].iter().collect();
                            RuntimeValue::Str(Arc::new(slice))
                        }
                        _ => return Err(RuntimeError::UnexpectedType(RuntimeValue::Null)),
                    },
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
                let index = match index_val {
                    RuntimeValue::Int(index) => index,
                    RuntimeValue::UInt(index) => index as i64,
                    _ => {
                        return Err(RuntimeError::UnexpectedType(RuntimeValue::Null));
                    }
                };
                let resolve_idx = |len: usize, idx: i64| -> Result<usize, RuntimeError> {
                    let resolved = if idx < 0 { len as i64 + idx } else { idx };
                    if resolved < 0 || resolved as usize >= len {
                        return Err(RuntimeError::StackUnderflow);
                    }
                    Ok(resolved as usize)
                };
                let target_val_ref = self.get_reg_value_ref(*target);
                if index < 0 && !matches!(target_val_ref, RuntimeValue::List(_)) {
                    return Err(RuntimeError::UnexpectedType(RuntimeValue::Null));
                }
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
                            let mut list = list;
                            let vec = &mut Gc::make_mut(&mut list).0;
                            let idx = resolve_idx(vec.len(), index)?;
                            vec[idx] = value;
                            self.variables.insert(id.clone(), RuntimeValue::List(list));
                        }
                    }
                    RuntimeValue::VarRef(id) => {
                        if let Some(RuntimeValue::List(list)) = self.variables.get_by_id(*id) {
                            let mut list = list;
                            let vec = &mut Gc::make_mut(&mut list).0;
                            let idx = resolve_idx(vec.len(), index)?;
                            vec[idx] = value;
                            let _ = self.variables.set_by_id(*id, RuntimeValue::List(list));
                        }
                    }
                    RuntimeValue::RegRef { frame, reg } => {
                        if let RuntimeValue::List(list) = self.get_reg_value_in_frame(*frame, *reg)
                        {
                            let mut list = list;
                            let vec = &mut Gc::make_mut(&mut list).0;
                            let idx = resolve_idx(vec.len(), index)?;
                            vec[idx] = value;
                            self.set_reg_value_in_frame(*frame, *reg, RuntimeValue::List(list));
                        } else {
                            return Err(RuntimeError::UnexpectedType(RuntimeValue::Null));
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
                        let existed = self.variables.contains_key(&name);
                        self.variables.insert(name, value);
                        if !existed {
                            self.invalidate_name_resolution_caches();
                        }
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
                if let RuntimeValue::Bool(v) = self.get_reg_value_ref(*cond) {
                    return if *v {
                        Ok(TerminateValue::Jump(*then_block))
                    } else {
                        Ok(TerminateValue::Jump(*else_block))
                    };
                }
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
