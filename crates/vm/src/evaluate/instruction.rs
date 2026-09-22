use super::write_back::Propagation;
use super::*;
use crate::{
    evaluate::calling::{CallSite, RegisterCall},
    native::stdlib::generator::{GeneratorResumeFn, GeneratorState},
    value::{GcMap, GcVec, HashKey},
};
use calibre_parser::ast::{comparison::BooleanOperator, nodes::binary::AsFailureMode};
use wasm_sync::Mutex;

pub trait VMEvaluation {
    fn run(
        &self,
        vm: &mut VM,
        block: &VMBlock,
        ip: u32,
        prev_block: Option<BlockId>,
    ) -> Result<TerminateValue, RuntimeError>;
}

impl VM {
    fn eval_branch_condition(
        &mut self,
        cond: u16,
        block: &VMBlock,
        ip: u32,
    ) -> Result<bool, RuntimeError> {
        if let RuntimeValue::Bool(v) = self.get_reg_value(cond) {
            return Ok(*v);
        }

        let resolved = self.resolve_value_ref(self.get_reg_value(cond))?;
        let value = if resolved.is_callable() {
            let mut callee = resolved;
            if let Some((source_reg, member_name)) =
                self.current_frame().member_sources.get(&cond).cloned()
            {
                let raw_receiver = self.get_reg_value(source_reg).clone();
                let resolved_receiver = self.resolve_value_ref(&raw_receiver)?;
                callee = self.bind_member_receiver_if_callable(
                    callee,
                    &member_name,
                    &raw_receiver,
                    resolved_receiver,
                );
            }
            self.call_runtime_callable_at(
                callee,
                Vec::new(),
                CallSite {
                    block: block.id.0 as usize,
                    tag: ip,
                },
                true,
            )?
        } else {
            resolved
        };

        match value {
            RuntimeValue::Bool(v) => Ok(v),
            other => Err(RuntimeError::ExpectedBoolFound {
                found: Box::new(other),
            }),
        }
    }

    #[inline]
    fn resolve_index(len: usize, idx: i64) -> Option<usize> {
        if len == 0 {
            return None;
        }

        let resolved = if idx < 0 { len as i64 + idx } else { idx };

        if resolved < 0 || resolved as usize >= len {
            None
        } else {
            Some(resolved as usize)
        }
    }

    #[inline]
    fn resolve_index_or_err(len: usize, idx: i64) -> Result<usize, RuntimeError> {
        Self::resolve_index(len, idx).ok_or(RuntimeError::StackUnderflow)
    }

    #[inline]
    fn resolve_slice_range(len: usize, start: i64, end: i64) -> (usize, usize) {
        let mut s = start;
        let mut e = end;
        if s < 0 {
            s += len as i64;
        }
        if e < 0 {
            e += len as i64;
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
    }

    #[instrument(skip_all)]
    pub(super) fn run_instruction(
        &mut self,
        instruction: &VMInstruction,
        block: &VMBlock,
        ip: u32,
        prev_block: Option<BlockId>,
    ) -> Result<TerminateValue, RuntimeError> {
        match instruction {
            // Literals
            VMInstruction::LoadLiteral(x) => x.run(self, block, ip, prev_block),

            // Variables
            VMInstruction::LoadVar(x) => x.run(self, block, ip, prev_block),
            VMInstruction::MoveVar(x) => x.run(self, block, ip, prev_block),
            VMInstruction::DropVar(x) => x.run(self, block, ip, prev_block),
            VMInstruction::StoreVar { dst, name, src } => {
                let name = self.local_string(block, *name)?;
                let old = self.variables.insert(
                    *name,
                    if self.in_global {
                        self.get_reg_value(*src).clone()
                    } else {
                        RuntimeValue::RegRef {
                            frame: self.frames.len().saturating_sub(1),
                            reg: *src,
                        }
                    },
                );

                if let Some(old) = old
                    && let Some(dst) = dst
                {
                    self.set_reg_value(*dst, old);
                }

                Ok(TerminateValue::None)
            }
            VMInstruction::LoadVarRef { dst, name } => {
                let name = self.local_string(block, *name)?;
                if let Some(RuntimeValue::RegRef { frame, reg }) = self.variables.get(name) {
                    self.set_reg_value(
                        *dst,
                        RuntimeValue::RegRef {
                            frame: *frame,
                            reg: *reg,
                        },
                    );
                } else {
                    self.set_reg_value(*dst, RuntimeValue::Ref(*name));
                }

                Ok(TerminateValue::None)
            }
            VMInstruction::LoadRegRef { dst, src } => {
                let value = match self.get_reg_value(*src) {
                    RuntimeValue::RegRef { frame, reg } => RuntimeValue::RegRef {
                        frame: *frame,
                        reg: *reg,
                    },
                    RuntimeValue::Ref(name) => RuntimeValue::Ref(*name),
                    RuntimeValue::VarRef(id) => RuntimeValue::VarRef(*id),
                    other => other.clone(),
                };

                self.set_reg_value(*dst, value);
                Ok(TerminateValue::None)
            }

            VMInstruction::Copy { dst, src } => {
                if dst == src {
                    return Ok(TerminateValue::None);
                }
                let value = self.get_reg_value(*src).clone();
                self.set_reg_value(*dst, value);
                self.propagate_member_source_alias(*src, *dst);
                Ok(TerminateValue::None)
            }
            VMInstruction::As {
                dst,
                src,
                data_type,
                failure_mode,
            } => {
                let value = self.get_reg_value(*src).clone();
                let conversion = value.convert(self, &data_type.data_type);
                let converted = match failure_mode {
                    AsFailureMode::Panic => match conversion {
                        Ok(value) => value,
                        Err(err) => {
                            return Err(RuntimeError::Panic(Some(format!(
                                "failed `as!` conversion to {}: {}",
                                data_type, err
                            ))));
                        }
                    },
                    AsFailureMode::Option => match conversion {
                        Ok(value) => RuntimeValue::Option(Some(Gc::new(value))),
                        Err(_) => RuntimeValue::Option(None),
                    },
                    AsFailureMode::Result => match conversion {
                        Ok(value) => RuntimeValue::Result(Ok(Gc::new(value))),
                        Err(err) => RuntimeValue::Result(Err(Gc::new(RuntimeValue::Str(
                            Ustr::from(&err.to_string()),
                        )))),
                    },
                };

                self.set_reg_value(*dst, converted);
                Ok(TerminateValue::None)
            }
            VMInstruction::Is {
                dst,
                src,
                data_type,
            } => {
                let resolved = self.resolve_value(self.get_reg_value(*src).clone())?;
                let out = self.runtime_matches_type(&resolved, &data_type.data_type);
                self.set_reg_value(*dst, RuntimeValue::Bool(out));
                Ok(TerminateValue::None)
            }
            VMInstruction::Binary {
                dst,
                op,
                left,
                right,
            } => {
                let left = self.resolve_value(self.get_reg_value(*left).clone())?;
                let right = self.resolve_value(self.get_reg_value(*right).clone())?;
                let value = binary(self, op, left, right)?;
                self.set_reg_value(*dst, value);
                Ok(TerminateValue::None)
            }
            VMInstruction::Comparison {
                dst,
                op,
                left,
                right,
            } => {
                let right = self.resolve_value(self.get_reg_value(*right).clone())?;
                let left = self.resolve_value(self.get_reg_value(*left).clone())?;
                let cmp_val = comparison(op, left, right)?;
                self.set_reg_value(*dst, cmp_val);
                Ok(TerminateValue::None)
            }
            VMInstruction::Boolean {
                dst,
                op,
                left,
                right,
            } => {
                let left = self.resolve_value(self.get_reg_value(*left).clone())?;

                if let RuntimeValue::Bool(x) = &left {
                    if &BooleanOperator::And == op && !*x {
                        self.set_reg_value(*dst, RuntimeValue::Bool(false));
                        return Ok(TerminateValue::None);
                    } else if &BooleanOperator::Or == op && *x {
                        self.set_reg_value(*dst, RuntimeValue::Bool(true));
                        return Ok(TerminateValue::None);
                    }
                }

                let right = self.resolve_value(self.get_reg_value(*right).clone())?;
                self.set_reg_value(*dst, boolean(op, left, right)?);
                Ok(TerminateValue::None)
            }
            VMInstruction::Range {
                dst,
                from,
                to,
                inclusive,
            } => {
                let from = self.resolve_value_ref(self.get_reg_value(*from))?;
                let to = self.resolve_value_ref(self.get_reg_value(*to))?;
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
                        other => Err(RuntimeError::ExpectedNumericFound {
                            found: Box::new(other),
                        }),
                    }
                };
                let from = as_range_bound(from)?;
                let to = as_range_bound(to)?;
                let range = if *inclusive {
                    RuntimeValue::Range(from, to + 1)
                } else {
                    RuntimeValue::Range(from, to)
                };
                self.set_reg_value(*dst, range);
                Ok(TerminateValue::None)
            }
            VMInstruction::List { dst, items } => {
                let values = items
                    .iter()
                    .map(|item| self.get_reg_value(*item).clone())
                    .collect();
                self.set_reg_value(*dst, RuntimeValue::List(Gc::new(GcVec(values))));
                Ok(TerminateValue::None)
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
                    let mut value = self.get_reg_value(*reg).clone();
                    if value.is_ref_like()
                        && let Ok(resolved) = self.resolve_value_ref(&value)
                    {
                        value = resolved;
                    }
                    entries.push((name, value));
                }

                if let Some(type_name) = layout.name
                    && Self::is_gen_type_name(&type_name)
                {
                    let next_fn = entries.iter().find_map(|(field, value)| {
                        let short = field.rsplit(".").next().unwrap_or(field.as_str());
                        (short == "data").then(|| value.clone())
                    });
                    if let Some(RuntimeValue::Function { name, captures }) = next_fn {
                        let resolved_caps: Vec<(Ustr, RuntimeValue)> = captures
                            .iter()
                            .map(|(k, v)| {
                                let resolved =
                                    self.resolve_value_ref(v).unwrap_or_else(|_| v.clone());
                                (*k, resolved)
                            })
                            .collect();

                        let mut gen_vm = VM::new_shared(
                            self.registry.clone(),
                            self.mappings.clone(),
                            self.config.clone(),
                        );

                        for (k, v) in &resolved_caps {
                            gen_vm.variables.insert(*k, v.clone());
                        }

                        if !self.ptr_heap.is_empty() {
                            gen_vm.ptr_heap = self.ptr_heap.clone();
                        }

                        self.set_reg_value(
                            *dst,
                            RuntimeValue::Generator {
                                type_name,
                                state: Arc::new(Mutex::new(GeneratorState {
                                    vm: gen_vm,
                                    function_name: name,
                                    captures: Arc::new(resolved_caps),
                                    task_state: crate::TaskState::default(),
                                    index: 0,
                                    completed: false,
                                })),
                            },
                        );
                        return Ok(TerminateValue::None);
                    }
                }

                self.set_reg_value(
                    *dst,
                    RuntimeValue::Aggregate(
                        layout.name,
                        Gc::new(GcMap(ObjectMap(
                            entries.into_iter().map(|x| (*x.0, x.1)).collect(),
                        ))),
                    ),
                );

                Ok(TerminateValue::None)
            }
            VMInstruction::Enum {
                dst,
                name,
                variant,
                payload,
            } => {
                let name = self.local_string(block, *name)?;
                let payload = payload.map(|reg| Gc::new(self.get_reg_value(reg).clone()));
                self.set_reg_value(*dst, RuntimeValue::Enum(*name, *variant as usize, payload));
                Ok(TerminateValue::None)
            }
            VMInstruction::CallSelf { dst, args } => {
                let func_ptr = self.current_frame().func_ptr as *const VMFunction;
                if func_ptr.is_null() {
                    return Err(RuntimeError::InvalidBytecode(
                        "missing current function frame".to_string(),
                    ));
                }
                let func = unsafe { &*func_ptr };

                if func.pure && (dst.is_none() || !func.returns_value) {
                    return Ok(TerminateValue::None);
                }

                if let Some(dst) = dst
                    && func.memo
                {
                    let caller_frame = self.frames.len().saturating_sub(1);
                    let mut key: Option<Vec<HashKey>> = Some(Vec::with_capacity(args.len()));

                    for (i, reg) in args.iter().enumerate() {
                        if func.memo_params == 0 || func.memo_params & (1 << i) != 0 {
                            let val = self.get_reg_value_in_frame(caller_frame, *reg).clone();
                            match HashKey::try_from(val) {
                                Ok(k) => key.as_mut().unwrap().push(k),
                                Err(_) => {
                                    key = None;
                                    break;
                                }
                            }
                        }
                    }

                    if let Some(k) = key {
                        if let Some(val) = {
                            let cache_entry = self
                                .caches
                                .memo
                                .entry(func.name)
                                .or_insert_with(|| Arc::new(Mutex::new(FxHashMap::default())));

                            let guard = cache_entry.lock().unwrap();
                            guard.get(&k).cloned()
                        } {
                            self.set_reg_value(*dst, val);
                            return Ok(TerminateValue::None);
                        }

                        let value = self.run_function_from_regs(
                            func,
                            args.iter().copied(),
                            Self::empty_captures(),
                            true,
                        )?;

                        {
                            let cache_entry = self
                                .caches
                                .memo
                                .entry(func.name)
                                .or_insert_with(|| Arc::new(Mutex::new(FxHashMap::default())));

                            cache_entry.lock().unwrap().insert(k, value.clone());
                        }

                        self.set_reg_value(*dst, value);
                        return Ok(TerminateValue::None);
                    }
                }

                let value = self.run_function_from_regs(
                    func,
                    args.iter().copied(),
                    Self::empty_captures(),
                    dst.is_some(),
                )?;

                if let Some(dst) = dst {
                    self.set_reg_value(*dst, value);
                }

                Ok(TerminateValue::None)
            }
            VMInstruction::Call { dst, callee, args } => {
                if let Some(step) = self.call_registers(RegisterCall {
                    dst: *dst,
                    callee: *callee,
                    args,
                    block,
                    ip,
                    prev_block,
                })? {
                    return Ok(step);
                }

                Ok(TerminateValue::None)
            }
            VMInstruction::Spawn { dst, callee } => {
                let resolved = self.resolve_value_ref(self.get_reg_value(*callee))?;
                let to_spawn = match resolved {
                    RuntimeValue::Function { name, captures } => {
                        let resolved_caps: Vec<(Ustr, RuntimeValue)> = captures
                            .as_ref()
                            .iter()
                            .map(|(k, v)| {
                                let resolved = self
                                    .resolve_value_ref(v)
                                    .unwrap_or_else(|_| RuntimeValue::Null);
                                let resolved = self.convert_runtime_var_into_saveable(resolved);
                                (*k, resolved)
                            })
                            .collect();
                        RuntimeValue::Function {
                            name,
                            captures: Arc::new(resolved_caps),
                        }
                    }
                    other => other,
                };

                let wg = Arc::new(WaitGroupInner::default());
                wg.count.store(1, std::sync::atomic::Ordering::Release);
                self.spawn_async_task(to_spawn, Some(wg.clone()));
                self.set_reg_value(*dst, RuntimeValue::WaitGroup(wg));

                Ok(TerminateValue::None)
            }
            VMInstruction::LoadMember { dst, value, member } => {
                let source_reg = *value;
                let name = self.local_string(block, *member)?;
                let raw_receiver = self.get_reg_value(*value).clone();
                let (short_name, tuple_index) = Self::member_parts(name);

                let mut resolved = self.resolve_value_ref(&raw_receiver)?;
                if resolved.is_null()
                    && let RuntimeValue::Ref(owner) = &raw_receiver
                    && let Some(callee) =
                        self.resolve_associated_member_value(owner, name, short_name)
                {
                    self.set_reg_value(*dst, callee);
                    self.current_frame_mut()
                        .member_sources
                        .insert(*dst, (source_reg, *name));
                    return Ok(TerminateValue::None);
                }

                let member_short = short_name.unwrap_or(name);
                let bind_assoc = |vm: &mut VM,
                                  type_name: &str,
                                  value: RuntimeValue|
                 -> Result<RuntimeValue, RuntimeError> {
                    if let Some(callee) =
                        vm.resolve_associated_member_value(type_name, name, short_name)
                    {
                        Ok(vm.bind_member_receiver_if_callable(callee, name, &raw_receiver, value))
                    } else {
                        Err(RuntimeError::MissingMember {
                            target: Box::new(value),
                            member: name.to_string(),
                        })
                    }
                };
                for _ in 0..4 {
                    match &resolved {
                        RuntimeValue::Result(Ok(inner)) if member_short == "next" => {
                            self.set_reg_value(*dst, inner.as_ref().clone());
                            return Ok(TerminateValue::None);
                        }
                        RuntimeValue::Result(Ok(inner)) => {
                            resolved = inner.as_ref().clone();
                        }
                        _ => break,
                    }
                }

                let mut member_source: Option<(u16, Ustr)> = None;

                let val = match resolved {
                    RuntimeValue::Generator { type_name, state } => match member_short {
                        "data" | "next" => {
                            RuntimeValue::NativeFunction(Arc::new(GeneratorResumeFn {
                                state: state.clone(),
                            }))
                        }
                        "index" => {
                            let guard = state.lock().unwrap();
                            RuntimeValue::Int(guard.index)
                        }
                        "done" => {
                            let guard = state.lock().unwrap();
                            RuntimeValue::Bool(guard.completed)
                        }
                        _ => match self.resolve_associated_member_value(
                            type_name.as_str(),
                            name,
                            short_name,
                        ) {
                            Some(value) => value,
                            None => {
                                return Err(RuntimeError::MissingMember {
                                    target: Box::new(RuntimeValue::Generator { type_name, state }),
                                    member: name.to_string(),
                                });
                            }
                        },
                    },
                    RuntimeValue::DynObject {
                        type_name,
                        value,
                        vtable,
                        constraints,
                    } => {
                        let member_short = Ustr::from(short_name.unwrap_or(name));
                        if let Some(callee_name) =
                            vtable.get(&member_short).or_else(|| vtable.get(name))
                        {
                            if let Some(callee) = self.resolve_dyn_method_callable(
                                type_name.as_str(),
                                member_short.as_str(),
                                Some(callee_name.as_str()),
                            ) {
                                callee.bind_if_callable(value.as_ref().clone())
                            } else if let Some(x) = self.get_value(callee_name) {
                                x
                            } else {
                                return Err(RuntimeError::FunctionNotFound(
                                    callee_name.to_string(),
                                ));
                            }
                        } else if let Some(callee) = self.resolve_dyn_method_callable(
                            type_name.as_str(),
                            member_short.as_str(),
                            None,
                        ) {
                            callee.bind_if_callable(value.as_ref().clone())
                        } else if member_short == "type" {
                            RuntimeValue::Str(type_name)
                        } else if member_short == "traits" {
                            RuntimeValue::List(Gc::new(GcVec(
                                constraints.iter().map(|x| RuntimeValue::Str(*x)).collect(),
                            )))
                        } else if let Some(x) =
                            self.get_value(&Ustr::from(&format!("{}.{}", type_name, member_short)))
                        {
                            x
                        } else {
                            return Err(RuntimeError::MissingMember {
                                target: Box::new(RuntimeValue::DynObject {
                                    type_name,
                                    constraints,
                                    value,
                                    vtable,
                                }),
                                member: name.to_string(),
                            });
                        }
                    }
                    RuntimeValue::Aggregate(None, map) => {
                        let idx = tuple_index.ok_or(RuntimeError::ExpectedIntIndexFound {
                            found: Box::new(RuntimeValue::Null),
                        })?;
                        if let Some((_, value)) = map.as_ref().0.0.get(idx) {
                            value.clone()
                        } else {
                            return Err(RuntimeError::MissingMember {
                                target: Box::new(RuntimeValue::Aggregate(None, map)),
                                member: name.to_string(),
                            });
                        }
                    }
                    RuntimeValue::Aggregate(Some(type_name), map) => {
                        if let Some(idx) =
                            self.resolve_aggregate_member_slot(&type_name, &map, name, short_name)
                        {
                            member_source = Some(
                                self.current_frame()
                                    .member_sources
                                    .get(&source_reg)
                                    .map(|(parent, path)| {
                                        (
                                            parent.to_owned(),
                                            Ustr::from(&format!("{path}.{}", map.0.0[idx].0)),
                                        )
                                    })
                                    .unwrap_or((source_reg, Ustr::from(&map.0.0[idx].0))),
                            );

                            map.0.0[idx].1.clone()
                        } else if let Some((_, wrapped)) =
                            map.0.0.iter().find(|(field, _)| field == "0")
                        {
                            let wrapped = self.resolve_value_ref(wrapped)?;
                            if tuple_index.is_some() {
                                member_source = Some(
                                    self.current_frame()
                                        .member_sources
                                        .get(&source_reg)
                                        .map(|(parent, path)| {
                                            (parent.to_owned(), Ustr::from(&format!("{path}.0")))
                                        })
                                        .unwrap_or((source_reg, Ustr::from("0"))),
                                );
                                wrapped
                            } else {
                                let RuntimeValue::Aggregate(inner_type, inner_map) =
                                    wrapped.clone()
                                else {
                                    return Err(RuntimeError::MissingMember {
                                        target: Box::new(RuntimeValue::Aggregate(
                                            Some(type_name),
                                            map,
                                        )),
                                        member: name.to_string(),
                                    });
                                };
                                let inner_name = inner_type.as_deref().unwrap_or_default();
                                if let Some(idx) = self.resolve_aggregate_member_slot(
                                    inner_name, &inner_map, name, short_name,
                                ) {
                                    inner_map.0.0[idx].1.clone()
                                } else {
                                    return Err(RuntimeError::MissingMember {
                                        target: Box::new(RuntimeValue::Aggregate(
                                            Some(type_name),
                                            map,
                                        )),
                                        member: name.to_string(),
                                    });
                                }
                            }
                        } else {
                            match self.resolve_associated_member_value(
                                type_name.as_str(),
                                name,
                                short_name,
                            ) {
                                Some(value) => {
                                    let resolved_receiver =
                                        RuntimeValue::Aggregate(Some(type_name), map.clone());
                                    self.bind_member_receiver_if_callable(
                                        value,
                                        name,
                                        &raw_receiver,
                                        resolved_receiver,
                                    )
                                }
                                None => {
                                    return Err(RuntimeError::MissingMember {
                                        target: Box::new(RuntimeValue::Aggregate(
                                            Some(type_name),
                                            map,
                                        )),
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
                    RuntimeValue::Option(Some(inner)) if !(name == "next" || name == "0") => {
                        if let Some(callee) =
                            self.resolve_associated_member_value("option", name, short_name)
                        {
                            self.bind_member_receiver_if_callable(
                                callee,
                                name,
                                &raw_receiver,
                                RuntimeValue::Option(Some(inner.clone())),
                            )
                        } else {
                            let mut inner_value =
                                self.resolve_value_ref(&inner.as_ref().clone())?;

                            while let RuntimeValue::Option(Some(nested)) = inner_value.clone() {
                                inner_value = self.resolve_value_ref(&nested.as_ref().clone())?;
                            }

                            match inner_value.clone() {
                                RuntimeValue::Aggregate(type_name, map) => {
                                    if let Some(idx) = self.resolve_aggregate_member_slot(
                                        type_name.as_deref().unwrap_or_default(),
                                        &map,
                                        name,
                                        short_name,
                                    ) {
                                        map.0.0[idx].1.clone()
                                    } else if let Some(callee) = self
                                        .resolve_associated_member_value(
                                            type_name.as_deref().unwrap_or("T"),
                                            name,
                                            short_name,
                                        )
                                    {
                                        self.bind_member_receiver_if_callable(
                                            callee,
                                            name,
                                            &inner_value,
                                            inner_value.clone(),
                                        )
                                    } else {
                                        return Err(RuntimeError::MissingMember {
                                            target: Box::new(RuntimeValue::Option(Some(inner))),
                                            member: name.to_string(),
                                        });
                                    }
                                }
                                other => {
                                    return Err(RuntimeError::MissingMember {
                                        target: Box::new(RuntimeValue::Option(Some(Gc::new(
                                            other,
                                        )))),
                                        member: name.to_string(),
                                    });
                                }
                            }
                        }
                    }
                    RuntimeValue::Option(None) if name == "next" || name == "0" => {
                        RuntimeValue::Null
                    }
                    option @ RuntimeValue::Option(_) => {
                        if let Some(callee) =
                            self.resolve_associated_member_value("T?", name, short_name)
                        {
                            self.bind_member_receiver_if_callable(
                                callee,
                                name,
                                &raw_receiver,
                                option,
                            )
                        } else {
                            return Err(RuntimeError::MissingMember {
                                target: Box::new(option),
                                member: name.to_string(),
                            });
                        }
                    }
                    RuntimeValue::Result(Ok(x)) if name == "next" || name == "0" => {
                        x.as_ref().clone()
                    }
                    RuntimeValue::Result(Err(x)) if name == "next" || name == "0" => {
                        x.as_ref().clone()
                    }
                    result @ RuntimeValue::Result(_) => {
                        if let Some(callee) =
                            self.resolve_associated_member_value("result", name, short_name)
                        {
                            self.bind_member_receiver_if_callable(
                                callee,
                                name,
                                &raw_receiver,
                                result,
                            )
                        } else {
                            return Err(RuntimeError::MissingMember {
                                target: Box::new(result),
                                member: name.to_string(),
                            });
                        }
                    }
                    RuntimeValue::Ptr(id) if name == "next" || name == "0" => {
                        self.ptr_heap.get(&id).cloned().unwrap_or_default()
                    }
                    RuntimeValue::Char(value) => {
                        bind_assoc(self, "char", RuntimeValue::Char(value))?
                    }
                    RuntimeValue::Str(value) => bind_assoc(self, "str", RuntimeValue::Str(value))?,
                    RuntimeValue::List(value) => {
                        if let Some(index) = tuple_index {
                            value
                                .as_ref()
                                .0
                                .get(index)
                                .cloned()
                                .unwrap_or_else(|| RuntimeValue::Null)
                        } else {
                            bind_assoc(self, "list", RuntimeValue::List(value))?
                        }
                    }
                    RuntimeValue::Int(value) => bind_assoc(self, "int", RuntimeValue::Int(value))?,
                    RuntimeValue::UInt(value) => {
                        bind_assoc(self, "uint", RuntimeValue::UInt(value))?
                    }
                    RuntimeValue::Float(value) => {
                        bind_assoc(self, "float", RuntimeValue::Float(value))?
                    }
                    RuntimeValue::Bool(value) => {
                        bind_assoc(self, "bool", RuntimeValue::Bool(value))?
                    }
                    RuntimeValue::Null => {
                        return Err(RuntimeError::MissingMember {
                            target: Box::new(RuntimeValue::Null),
                            member: name.to_string(),
                        });
                    }
                    other => {
                        if let Some(type_name) = other.impl_name() {
                            bind_assoc(self, type_name.as_str(), other)?
                        } else {
                            return Err(RuntimeError::ExpectedStructOrAggregateFound {
                                found: Box::new(other),
                            });
                        }
                    }
                };

                self.set_reg_value(*dst, val);

                match member_source {
                    Some((parent, field)) => {
                        self.current_frame_mut()
                            .member_sources
                            .insert(*dst, (parent, Ustr::from(&field)));
                    }
                    None => {
                        let source = self
                            .current_frame()
                            .member_sources
                            .get(&source_reg)
                            .cloned();
                        self.current_frame_mut().member_sources.insert(
                            *dst,
                            source
                                .map(|(parent, path)| {
                                    (parent, Ustr::from(&format!("{path}.{name}")))
                                })
                                .unwrap_or((source_reg, Ustr::from(name))),
                        );
                    }
                }

                Ok(TerminateValue::None)
            }
            VMInstruction::SetMember {
                dst,
                target,
                member,
                value,
            } => {
                let name = self.local_string(block, *member)?;
                let value = self.get_reg_value(*value).clone();
                let (short_name, tuple_index) = Self::member_parts(name);

                let update_aggregate = |agg_name: &Option<Ustr>, mut map: Gc<GcMap>| {
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
                                entry.0 == *name || short_name.is_some_and(|short| entry.0 == short)
                            }) {
                                entry.1 = value.clone();
                            } else {
                                return Err(RuntimeError::StackUnderflow);
                            }
                        }
                        _ => {
                            return Err(RuntimeError::ExpectedAggregateFound {
                                found: Box::new(RuntimeValue::Null),
                            });
                        }
                    }
                    Ok(map)
                };

                let update_generator =
                    |generator_value: RuntimeValue| -> Result<RuntimeValue, RuntimeError> {
                        let RuntimeValue::Generator { type_name, state } = generator_value else {
                            return Err(RuntimeError::ExpectedGeneratorFound {
                                found: Box::new(generator_value),
                            });
                        };

                        let member_key = short_name.unwrap_or(name);
                        if !matches!(member_key, "done" | "index") {
                            return Err(RuntimeError::MissingMember {
                                target: Box::new(RuntimeValue::Generator { type_name, state }),
                                member: name.to_string(),
                            });
                        }

                        let mut guard = state.lock().unwrap();
                        match member_key {
                            "index" => match &value {
                                RuntimeValue::Int(x) => guard.index = (*x).max(0),
                                RuntimeValue::UInt(x) => guard.index = *x as i64,
                                other => {
                                    return Err(RuntimeError::ExpectedIntForGeneratorIndex {
                                        found: Box::new((*other).clone()),
                                    });
                                }
                            },
                            "done" => match &value {
                                RuntimeValue::Bool(x) => guard.completed = *x,
                                other => {
                                    return Err(RuntimeError::ExpectedBoolForGeneratorDone {
                                        found: Box::new((*other).clone()),
                                    });
                                }
                            },
                            _ => {}
                        }
                        drop(guard);

                        Ok(RuntimeValue::Generator { type_name, state })
                    };

                let mut target_value = self.get_reg_value(*target).clone();
                let mut handled = false;

                for _ in 0..64 {
                    match target_value {
                        RuntimeValue::Ref(ref_name) => {
                            let current =
                                if let Some(value) = self.variables.get(&ref_name).cloned() {
                                    value
                                } else if let Some(value) = self.get_function_ref(&ref_name) {
                                    self.make_runtime_function(value)
                                } else {
                                    return Err(RuntimeError::DanglingRef(ref_name.to_string()));
                                };
                            let old = match current {
                                RuntimeValue::Ref(_)
                                | RuntimeValue::VarRef(_)
                                | RuntimeValue::RegRef { .. } => {
                                    target_value = current;
                                    continue;
                                }
                                RuntimeValue::Aggregate(name, map) => {
                                    let updated = update_aggregate(&name, map)?;
                                    self.variables
                                        .insert(ref_name, RuntimeValue::Aggregate(name, updated))
                                }
                                RuntimeValue::List(_list) => self.variables.insert(ref_name, value),
                                RuntimeValue::Generator { .. } => {
                                    self.variables.insert(ref_name, update_generator(current)?)
                                }
                                other => {
                                    return Err(RuntimeError::ExpectedGeneratorFound {
                                        found: Box::new(other),
                                    });
                                }
                            };

                            if let Some(old) = old {
                                let _ = self.set_reg_value(*dst, old);
                            }

                            handled = true;
                            break;
                        }
                        RuntimeValue::VarRef(id) => {
                            let current = self
                                .variables
                                .get_by_id(id)
                                .cloned()
                                .ok_or(RuntimeError::DanglingRef(format!("#{}", id)))?;

                            let old = match current {
                                RuntimeValue::Ref(_)
                                | RuntimeValue::VarRef(_)
                                | RuntimeValue::RegRef { .. } => {
                                    target_value = current;
                                    continue;
                                }
                                RuntimeValue::Aggregate(name, map) => {
                                    let updated = update_aggregate(&name, map)?;
                                    self.variables
                                        .set_by_id(id, RuntimeValue::Aggregate(name, updated))
                                }
                                RuntimeValue::List(_list) => self.variables.set_by_id(id, value),
                                RuntimeValue::Generator { .. } => {
                                    self.variables.set_by_id(id, update_generator(current)?)
                                }
                                other => {
                                    return Err(RuntimeError::ExpectedGeneratorFound {
                                        found: Box::new(other),
                                    });
                                }
                            };

                            if let Some(old) = old {
                                let _ = self.set_reg_value(*dst, old);
                            }

                            handled = true;
                            break;
                        }
                        RuntimeValue::RegRef { frame, reg } => {
                            let current = self.get_reg_value_in_frame(frame, reg).clone();
                            match current {
                                RuntimeValue::Ref(_)
                                | RuntimeValue::VarRef(_)
                                | RuntimeValue::RegRef { .. } => {
                                    target_value = current;
                                    continue;
                                }
                                RuntimeValue::Aggregate(name, map) => {
                                    let updated = update_aggregate(&name, map)?;
                                    let member_source = self
                                        .frames
                                        .get(frame)
                                        .and_then(|vm_frame| vm_frame.member_sources.get(&reg))
                                        .cloned();

                                    let _ = self.set_reg_value_in_frame(
                                        frame,
                                        reg,
                                        RuntimeValue::Aggregate(name, updated),
                                    );

                                    if let Some(source) = member_source
                                        && let Some(vm_frame) = self.frames.get_mut(frame)
                                    {
                                        vm_frame.member_sources.insert(reg, source);
                                    }

                                    let old = self.propagate_member_source_reg(reg, frame)?;

                                    if let Some(old) = old {
                                        let _ = self.set_reg_value(*dst, old);
                                    }
                                }
                                RuntimeValue::List(_) => {
                                    if let Some((parent_reg, field_name)) =
                                        self.current_frame().member_sources.get(&reg).cloned()
                                    {
                                        let old = self.write_back_member_field_update(
                                            frame,
                                            reg,
                                            parent_reg,
                                            &field_name,
                                        )?;

                                        if let Some(old) = old {
                                            let _ = self.set_reg_value(*dst, old);
                                        }
                                    }
                                }
                                RuntimeValue::Generator { .. } => {
                                    let old = self.set_reg_value_in_frame(
                                        frame,
                                        reg,
                                        update_generator(current)?,
                                    );

                                    let _ = self.set_reg_value(*dst, old);
                                }
                                other => {
                                    return Err(RuntimeError::ExpectedGeneratorFound {
                                        found: Box::new(other),
                                    });
                                }
                            }
                            handled = true;
                            break;
                        }
                        RuntimeValue::Aggregate(name, map) => {
                            let updated = update_aggregate(&name, map)?;
                            let member_source =
                                self.current_frame().member_sources.get(target).cloned();

                            let _ =
                                self.set_reg_value(*target, RuntimeValue::Aggregate(name, updated));

                            if let Some(source) = member_source {
                                self.current_frame_mut()
                                    .member_sources
                                    .insert(*target, source);
                            }

                            let old = self.propagate_member_source_reg(
                                *target,
                                self.frames.len().saturating_sub(1),
                            )?;

                            if let Some(old) = old {
                                let _ = self.set_reg_value(*dst, old);
                            }

                            handled = true;
                            break;
                        }
                        RuntimeValue::List(_) => {
                            if let Some((parent_reg, field_name)) =
                                self.current_frame().member_sources.get(target).cloned()
                            {
                                let old = self.write_back_member_field_update(
                                    self.frames.len().saturating_sub(1),
                                    *target,
                                    parent_reg,
                                    &field_name,
                                )?;

                                if let Some(old) = old {
                                    let _ = self.set_reg_value(*dst, old);
                                }
                            }

                            handled = true;
                            break;
                        }
                        current @ RuntimeValue::Generator { .. } => {
                            let old = self.set_reg_value(*target, update_generator(current)?);
                            let _ = self.set_reg_value(*dst, old);
                            handled = true;
                            break;
                        }
                        other => {
                            return Err(RuntimeError::ExpectedGeneratorFound {
                                found: Box::new(other),
                            });
                        }
                    }
                }

                if !handled {
                    return Err(RuntimeError::DanglingRef(
                        "<set-member-depth-limit>".to_string(),
                    ));
                }

                Ok(TerminateValue::None)
            }
            VMInstruction::Index { dst, value, index } => {
                let value_ref = self.get_reg_value(*value);
                let mut index_val = self.get_reg_value(*index).clone();

                if index_val.is_ref_like() {
                    index_val = self.resolve_value_ref(&index_val)?;
                }

                if let RuntimeValue::List(list) = value_ref {
                    let idx = match &index_val {
                        RuntimeValue::UInt(i) => Some(*i as usize),
                        RuntimeValue::Int(i) if *i >= 0 => Some(*i as usize),
                        _ => None,
                    };

                    if let Some(idx) = idx {
                        let out = list.as_ref().0.get(idx).cloned();
                        let out = out.unwrap_or_default();
                        self.set_reg_value(*dst, out);
                        if let Some(source) =
                            self.current_frame().member_sources.get(value).cloned()
                        {
                            self.current_frame_mut().member_sources.insert(*dst, source);
                        }
                        return Ok(TerminateValue::None);
                    }
                }

                let index_list = |list: &Gc<GcVec>| -> Result<RuntimeValue, RuntimeError> {
                    match &index_val {
                        RuntimeValue::Int(index) => {
                            Ok(Self::resolve_index(list.as_ref().0.len(), *index)
                                .and_then(|i| list.as_ref().0.get(i).cloned())
                                .unwrap_or_else(|| RuntimeValue::Null))
                        }
                        RuntimeValue::UInt(index) => Ok(list
                            .as_ref()
                            .0
                            .get(*index as usize)
                            .cloned()
                            .unwrap_or_else(|| RuntimeValue::Null)),
                        RuntimeValue::Range(start, end) => {
                            let (s, e) =
                                Self::resolve_slice_range(list.as_ref().0.len(), *start, *end);
                            let slice = list.as_ref().0[s..e].to_vec();
                            Ok(RuntimeValue::List(Gc::new(GcVec(slice))))
                        }
                        _ => Err(RuntimeError::ExpectedListOrStrFound {
                            found: Box::new(RuntimeValue::Null),
                        }),
                    }
                };

                let index_map = |map: &Arc<
                    Mutex<rustc_hash::FxHashMap<crate::value::HashKey, RuntimeValue>>,
                >|
                 -> Result<RuntimeValue, RuntimeError> {
                    let key = crate::value::HashKey::try_from(index_val.clone())?;
                    let guard = map.lock().unwrap();
                    Ok(guard.get(&key).cloned().unwrap_or(RuntimeValue::Null))
                };

                let resolved = self.resolve_value_ref(self.get_reg_value(*value))?;
                let val = match resolved {
                    RuntimeValue::List(list) => index_list(&list)?,
                    RuntimeValue::HashMap(map) => index_map(&map)?,
                    RuntimeValue::Range(start, end) => match &index_val {
                        RuntimeValue::Int(index) => {
                            let len = (end - start).max(0) as usize;
                            Self::resolve_index(len, *index)
                                .map(|i| RuntimeValue::Int(start + i as i64))
                                .unwrap_or_else(|| RuntimeValue::Null)
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
                            let (s, e) = Self::resolve_slice_range(len, *slice_start, *slice_end);
                            RuntimeValue::Range(start + s as i64, start + e as i64)
                        }
                        _ => {
                            return Err(RuntimeError::ExpectedIntIndexFound {
                                found: Box::new(RuntimeValue::Null),
                            });
                        }
                    },
                    RuntimeValue::Aggregate(None, tuple) => match &index_val {
                        RuntimeValue::Int(index) => {
                            Self::resolve_index(tuple.as_ref().0.0.len(), *index)
                                .and_then(|i| tuple.as_ref().0.0.get(i).map(|(_, v)| v.clone()))
                                .unwrap_or_else(|| RuntimeValue::Null)
                        }
                        RuntimeValue::UInt(index) => tuple
                            .as_ref()
                            .0
                            .0
                            .get(*index as usize)
                            .map(|(_, v)| v.clone())
                            .unwrap_or_else(|| RuntimeValue::Null),
                        RuntimeValue::Range(start, end) => {
                            let (s, e) =
                                Self::resolve_slice_range(tuple.as_ref().0.0.len(), *start, *end);
                            let slice = tuple.as_ref().0.0[s..e].to_vec();
                            RuntimeValue::Aggregate(None, Gc::new(GcMap(ObjectMap(slice))))
                        }
                        _ => {
                            return Err(RuntimeError::ExpectedIntIndexFound {
                                found: Box::new(RuntimeValue::Null),
                            });
                        }
                    },
                    RuntimeValue::Aggregate(Some(_), tuple) => match &index_val {
                        RuntimeValue::Int(0) | RuntimeValue::UInt(0)
                            if tuple.as_ref().0.0.len() == 1
                                && matches!(tuple.as_ref().0.0[0].1, RuntimeValue::List(_)) =>
                        {
                            let RuntimeValue::List(list) = &tuple.as_ref().0.0[0].1 else {
                                unreachable!()
                            };
                            list.as_ref()
                                .0
                                .first()
                                .cloned()
                                .unwrap_or(RuntimeValue::Null)
                        }
                        RuntimeValue::Int(index) => {
                            Self::resolve_index(tuple.as_ref().0.0.len(), *index)
                                .and_then(|i| tuple.as_ref().0.0.get(i).map(|(_, v)| v.clone()))
                                .unwrap_or_else(|| RuntimeValue::Null)
                        }
                        RuntimeValue::UInt(index) => tuple
                            .as_ref()
                            .0
                            .0
                            .get(*index as usize)
                            .map(|(_, v)| v.clone())
                            .unwrap_or_else(|| RuntimeValue::Null),
                        _ => {
                            return Err(RuntimeError::ExpectedIntIndexFound {
                                found: Box::new(RuntimeValue::Null),
                            });
                        }
                    },
                    RuntimeValue::Str(s) => match &index_val {
                        RuntimeValue::Int(index) => {
                            let resolved = if *index < 0 {
                                let len = s.chars().count();
                                Self::resolve_index(len, *index)
                            } else {
                                Some(*index as usize)
                            };
                            resolved
                                .and_then(|i| s.chars().nth(i))
                                .map(RuntimeValue::Char)
                                .unwrap_or_else(|| RuntimeValue::Null)
                        }
                        RuntimeValue::UInt(index) => s
                            .chars()
                            .nth(*index as usize)
                            .map(RuntimeValue::Char)
                            .unwrap_or_else(|| RuntimeValue::Null),
                        RuntimeValue::Range(start, end) => {
                            let v = s.chars().collect::<Vec<char>>();
                            let (s, e) = Self::resolve_slice_range(v.len(), *start, *end);
                            let slice: String = v[s..e].iter().collect();
                            RuntimeValue::Str(Ustr::from(&slice))
                        }
                        _ => {
                            return Err(RuntimeError::ExpectedIntIndexFound {
                                found: Box::new(RuntimeValue::Null),
                            });
                        }
                    },
                    RuntimeValue::Enum(_, _, Some(x)) => x.as_ref().clone(),
                    RuntimeValue::Option(Some(x)) => x.as_ref().clone(),
                    RuntimeValue::Result(Ok(x)) => x.as_ref().clone(),
                    RuntimeValue::Result(Err(x)) => x.as_ref().clone(),
                    other => {
                        return Err(RuntimeError::UnexpectedTypeInIndexAccess {
                            target: Box::new(other),
                            index: Box::new(index_val.clone()),
                        });
                    }
                };

                self.set_reg_value(*dst, val);
                if !self.current_frame().member_sources.contains_key(dst) {
                    self.propagate_member_source_alias(*value, *dst);
                }

                Ok(TerminateValue::None)
            }
            VMInstruction::SetIndex {
                dst,
                target,
                index,
                value,
            } => {
                let mut index_val = self.get_reg_value(*index).clone();

                if index_val.is_ref_like() {
                    index_val = self.resolve_value_ref(&index_val)?;
                }

                let value = self.get_reg_value(*value).clone();
                let numeric_index = || match index_val.clone() {
                    RuntimeValue::Int(index) => Ok(index),
                    RuntimeValue::UInt(index) => Ok(index as i64),
                    _ => Err(RuntimeError::ExpectedIntIndexFound {
                        found: Box::new(RuntimeValue::Null),
                    }),
                };

                let hash_index = || crate::value::HashKey::try_from(index_val.clone());
                let mut target_value = self.get_reg_value(*target).clone();
                let mut handled = false;

                for _ in 0..64 {
                    match target_value {
                        RuntimeValue::Ref(ref_name) => {
                            let current =
                                if let Some(value) = self.variables.get(&ref_name).cloned() {
                                    value
                                } else if let Some(value) = self.get_function_ref(&ref_name) {
                                    self.make_runtime_function(value)
                                } else {
                                    return Err(RuntimeError::DanglingRef(ref_name.to_string()));
                                };

                            match current {
                                RuntimeValue::Ref(_)
                                | RuntimeValue::VarRef(_)
                                | RuntimeValue::RegRef { .. } => {
                                    target_value = current;
                                    continue;
                                }
                                RuntimeValue::List(mut list) => {
                                    let index = numeric_index()?;
                                    if index < 0 {
                                        return Err(RuntimeError::ExpectedListOrStrFound {
                                            found: Box::new(RuntimeValue::Null),
                                        });
                                    }

                                    let vec = &mut Gc::make_mut(&mut list).0;
                                    let idx = Self::resolve_index_or_err(vec.len(), index)?;
                                    let old = std::mem::replace(&mut vec[idx], value);
                                    let _ = self.set_reg_value(*dst, old);

                                    self.variables.insert(ref_name, RuntimeValue::List(list));
                                    self.propagate_member_source_reg(
                                        *target,
                                        self.frames.len().saturating_sub(1),
                                    )?;
                                }
                                RuntimeValue::HashMap(map) => {
                                    let key = hash_index()?;

                                    let mut guard = map.lock().unwrap();

                                    if let Some(old) = guard.insert(key, value) {
                                        let _ = self.set_reg_value(*dst, old);
                                    }
                                }
                                _ => {
                                    return Err(RuntimeError::ExpectedListOrStrFound {
                                        found: Box::new(RuntimeValue::Null),
                                    });
                                }
                            }
                            handled = true;
                            break;
                        }
                        RuntimeValue::VarRef(id) => {
                            let current = self
                                .variables
                                .get_by_id(id)
                                .cloned()
                                .ok_or(RuntimeError::DanglingRef(format!("#{}", id)))?;

                            match current {
                                RuntimeValue::Ref(_)
                                | RuntimeValue::VarRef(_)
                                | RuntimeValue::RegRef { .. } => {
                                    target_value = current;
                                    continue;
                                }
                                RuntimeValue::List(mut list) => {
                                    let index = numeric_index()?;
                                    if index < 0 {
                                        return Err(RuntimeError::ExpectedListOrStrFound {
                                            found: Box::new(RuntimeValue::Null),
                                        });
                                    }

                                    let vec = &mut Gc::make_mut(&mut list).0;
                                    let idx = Self::resolve_index_or_err(vec.len(), index)?;
                                    let old = std::mem::replace(&mut vec[idx], value);
                                    let _ = self.set_reg_value(*dst, old);

                                    let _ = self.variables.set_by_id(id, RuntimeValue::List(list));
                                    self.propagate_member_source_reg(
                                        *target,
                                        self.frames.len().saturating_sub(1),
                                    )?;
                                }
                                RuntimeValue::HashMap(map) => {
                                    let key = hash_index()?;

                                    let mut guard = map.lock().unwrap();

                                    if let Some(old) = guard.insert(key, value) {
                                        let _ = self.set_reg_value(*dst, old);
                                    }
                                }
                                _ => {
                                    return Err(RuntimeError::ExpectedListOrStrFound {
                                        found: Box::new(RuntimeValue::Null),
                                    });
                                }
                            }
                            handled = true;
                            break;
                        }
                        RuntimeValue::RegRef { frame, reg } => {
                            let current = self.get_reg_value_in_frame(frame, reg).clone();
                            match current {
                                RuntimeValue::Ref(_)
                                | RuntimeValue::VarRef(_)
                                | RuntimeValue::RegRef { .. } => {
                                    target_value = current;
                                    continue;
                                }
                                RuntimeValue::List(mut list) => {
                                    let index = numeric_index()?;

                                    if index < 0 {
                                        return Err(RuntimeError::ExpectedListOrStrFound {
                                            found: Box::new(RuntimeValue::Null),
                                        });
                                    }

                                    let vec = &mut Gc::make_mut(&mut list).0;
                                    let idx = Self::resolve_index_or_err(vec.len(), index)?;
                                    let old = std::mem::replace(&mut vec[idx], value);
                                    let _ = self.set_reg_value(*dst, old);

                                    let member_source = self
                                        .frames
                                        .get(frame)
                                        .and_then(|vm_frame| vm_frame.member_sources.get(&reg))
                                        .cloned();

                                    self.set_reg_value_in_frame(
                                        frame,
                                        reg,
                                        RuntimeValue::List(list.clone()),
                                    );

                                    if let Some(source) = member_source
                                        && let Some(vm_frame) = self.frames.get_mut(frame)
                                    {
                                        vm_frame.member_sources.insert(reg, source);
                                    }

                                    self.propagate_member_source_reg(reg, frame)?;
                                }
                                RuntimeValue::HashMap(map) => {
                                    let key = hash_index()?;
                                    let guard = map.lock().unwrap();

                                    let mut guard = guard;
                                    if let Some(old) = guard.insert(key, value) {
                                        let _ = self.set_reg_value(*dst, old);
                                    }
                                }
                                _ => {
                                    return Err(RuntimeError::ExpectedListOrStrFound {
                                        found: Box::new(RuntimeValue::Null),
                                    });
                                }
                            }
                            handled = true;
                            break;
                        }
                        RuntimeValue::List(mut list) => {
                            let index = numeric_index()?;

                            if index < 0 {
                                return Err(RuntimeError::ExpectedListOrStrFound {
                                    found: Box::new(RuntimeValue::Null),
                                });
                            }

                            let vec = &mut Gc::make_mut(&mut list).0;
                            let idx = Self::resolve_index_or_err(vec.len(), index)?;
                            let old = std::mem::replace(&mut vec[idx], value);
                            let _ = self.set_reg_value(*dst, old);

                            let member_source =
                                self.current_frame().member_sources.get(target).cloned();
                            self.set_reg_value(*target, RuntimeValue::List(list));

                            if let Some(source) = member_source {
                                self.current_frame_mut()
                                    .member_sources
                                    .insert(*target, source);
                            }

                            self.propagate_member_source_reg(
                                *target,
                                self.frames.len().saturating_sub(1),
                            )?;

                            handled = true;
                            break;
                        }
                        RuntimeValue::HashMap(map) => {
                            let key = hash_index()?;

                            let mut guard = map.lock().unwrap();
                            if let Some(old) = guard.insert(key, value) {
                                let _ = self.set_reg_value(*dst, old);
                            }

                            handled = true;
                            break;
                        }
                        other => {
                            return Err(RuntimeError::ExpectedListOrStrFound {
                                found: Box::new(other),
                            });
                        }
                    }
                }

                if !handled {
                    return Err(RuntimeError::DanglingRef(
                        "<set-index-depth-limit>".to_string(),
                    ));
                }

                Ok(TerminateValue::None)
            }
            VMInstruction::Ref { dst, value } => {
                let out = match self.get_reg_value(*value).clone() {
                    RuntimeValue::Ref(name) => RuntimeValue::Ref(name),
                    RuntimeValue::VarRef(id) => RuntimeValue::VarRef(id),
                    RuntimeValue::RegRef { frame, reg } => RuntimeValue::RegRef { frame, reg },
                    other => if let Some(id) = (0..self.variables.slot_len()).find(|id| {
                        matches!(
                            self.variables.get_by_id(*id),
                            Some(RuntimeValue::RegRef { frame, reg })
                                if *frame == self.frames.len().saturating_sub(1) && *reg == *value
                        )
                    }) {
                        RuntimeValue::VarRef(id)
                    } else if let RuntimeValue::List(list) = &other
                        && let Some(id) = (0..self.variables.slot_len()).find(|id| {
                            matches!(
                                self.variables.get_by_id(*id),
                                Some(RuntimeValue::List(other_list))
                                    if std::ptr::eq(list.as_ref(), other_list.as_ref())
                            )
                        })
                    {
                        RuntimeValue::VarRef(id)
                    } else {
                        let name = Ustr::from(&self.get_ref_id().to_string());
                        let id = self.variables.insert_with_id(name, other);
                        RuntimeValue::VarRef(id)
                    },
                };

                self.set_reg_value(*dst, out);
                self.propagate_member_source_alias(*value, *dst);
                Ok(TerminateValue::None)
            }
            VMInstruction::Deref { dst, value } => {
                let out = self.resolve_value_ref(self.get_reg_value(*value))?;
                self.set_reg_value(*dst, out);
                Ok(TerminateValue::None)
            }
            VMInstruction::SetRef { dst, target, value } => {
                let target = self.get_reg_value(*target).clone();
                let value = self.get_reg_value(*value).clone();

                match target {
                    RuntimeValue::Ref(name) => {
                        if let Some(old) = self.variables.insert(name, value) {
                            let _ = self.set_reg_value(*dst, old);
                        }
                    }
                    RuntimeValue::VarRef(id) => {
                        if let Some(old) = self.variables.set_by_id(id, value) {
                            let _ = self.set_reg_value(*dst, old);
                        }
                    }
                    RuntimeValue::RegRef { frame, reg } => {
                        let old = self.set_reg_value_in_frame(frame, reg, value);
                        let _ = self.set_reg_value(*dst, old);
                    }
                    RuntimeValue::MutexGuard(guard) => {
                        let old = guard.set_value(value);
                        let _ = self.set_reg_value(*dst, old);
                    }
                    _ => return Err(RuntimeError::InvalidBytecode("invalid ref".to_string())),
                }

                Ok(TerminateValue::None)
            }
            VMInstruction::Jump(target) => Ok(TerminateValue::Jump(*target)),
            VMInstruction::Branch {
                cond,
                then_block,
                else_block,
            } => {
                if self.eval_branch_condition(*cond, block, ip)? {
                    Ok(TerminateValue::Jump(*then_block))
                } else {
                    Ok(TerminateValue::Jump(*else_block))
                }
            }
            VMInstruction::Return { value } => {
                if let Some(reg) = value {
                    Ok(TerminateValue::Return(self.get_reg_value(*reg).clone()))
                } else {
                    Ok(TerminateValue::Return(RuntimeValue::Null))
                }
            }
            VMInstruction::Noop => Ok(TerminateValue::None),
        }
    }
}
