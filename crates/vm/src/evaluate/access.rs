use std::sync::Arc;

use crate::{
    VM,
    conversion::{
        VMBlock,
        instructions::access::{VMIndex, VMLoadMember, VMSetIndex, VMSetMember},
    },
    error::RuntimeError,
    evaluate::{
        instruction::{VMEvaluation, resolve_index, resolve_slice_range},
        write_back::Propagation,
    },
    native::stdlib::generator::GeneratorResumeFn,
    value::{GcMap, GcVec, HashKey, RuntimeValue, TerminateValue},
};
use calibre_lir::ast::BlockId;
use calibre_parser::ast::ObjectMap;
use dumpster::sync::Gc;
use ustr::Ustr;
use wasm_sync::Mutex;

impl VMEvaluation for VMLoadMember {
    fn run(
        &self,
        vm: &mut VM,
        block: &VMBlock,
        _ip: u32,
        _prev_block: Option<BlockId>,
    ) -> Result<TerminateValue, RuntimeError> {
        let source_reg = self.value;
        let name = vm.local_string(block, self.member)?;
        let raw_receiver = vm.get_reg_value(self.value).clone();
        let (short_name, tuple_index) = VM::member_parts(name);

        let mut resolved = vm.resolve_value_ref(&raw_receiver)?;
        if resolved.is_null()
            && let RuntimeValue::Ref(owner) = &raw_receiver
            && let Some(callee) = vm.resolve_associated_member_value(owner, name, short_name)
        {
            vm.set_reg_value(self.dst, callee);
            vm.current_frame_mut()
                .member_sources
                .insert(self.dst, (source_reg, *name));
            return Ok(TerminateValue::None);
        }

        let member_short = short_name.unwrap_or(name);
        let bind_assoc = |vm: &mut VM,
                          type_name: &str,
                          value: RuntimeValue|
         -> Result<RuntimeValue, RuntimeError> {
            if let Some(callee) = vm.resolve_associated_member_value(type_name, name, short_name) {
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
                    vm.set_reg_value(self.dst, inner.as_ref().clone());
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
                "data" | "next" => RuntimeValue::NativeFunction(Arc::new(GeneratorResumeFn {
                    state: state.clone(),
                })),
                "index" => {
                    let guard = state.lock().unwrap();
                    RuntimeValue::Int(guard.index)
                }
                "done" => {
                    let guard = state.lock().unwrap();
                    RuntimeValue::Bool(guard.completed)
                }
                _ => {
                    match vm.resolve_associated_member_value(type_name.as_str(), name, short_name) {
                        Some(value) => value,
                        None => {
                            return Err(RuntimeError::MissingMember {
                                target: Box::new(RuntimeValue::Generator { type_name, state }),
                                member: name.to_string(),
                            });
                        }
                    }
                }
            },
            RuntimeValue::DynObject {
                type_name,
                value,
                vtable,
                constraints,
            } => {
                let member_short = Ustr::from(short_name.unwrap_or(name));
                if let Some(callee_name) = vtable.get(&member_short).or_else(|| vtable.get(name)) {
                    if let Some(callee) = vm.resolve_dyn_method_callable(
                        type_name.as_str(),
                        member_short.as_str(),
                        Some(callee_name.as_str()),
                    ) {
                        callee.bind_if_callable(value.as_ref().clone())
                    } else if let Some(x) = vm.get_value(callee_name) {
                        x
                    } else {
                        return Err(RuntimeError::FunctionNotFound(callee_name.to_string()));
                    }
                } else if let Some(callee) =
                    vm.resolve_dyn_method_callable(type_name.as_str(), member_short.as_str(), None)
                {
                    callee.bind_if_callable(value.as_ref().clone())
                } else if member_short == "type" {
                    RuntimeValue::Str(type_name)
                } else if member_short == "traits" {
                    RuntimeValue::List(Gc::new(GcVec(
                        constraints.iter().map(|x| RuntimeValue::Str(*x)).collect(),
                    )))
                } else if let Some(x) =
                    vm.get_value(&Ustr::from(&format!("{}.{}", type_name, member_short)))
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
                    vm.resolve_aggregate_member_slot(&type_name, &map, name, short_name)
                {
                    member_source = Some(
                        vm.current_frame()
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
                } else if let Some((_, wrapped)) = map.0.0.iter().find(|(field, _)| field == "0") {
                    let wrapped = vm.resolve_value_ref(wrapped)?;
                    if tuple_index.is_some() {
                        member_source = Some(
                            vm.current_frame()
                                .member_sources
                                .get(&source_reg)
                                .map(|(parent, path)| {
                                    (parent.to_owned(), Ustr::from(&format!("{path}.0")))
                                })
                                .unwrap_or((source_reg, Ustr::from("0"))),
                        );
                        wrapped
                    } else {
                        let RuntimeValue::Aggregate(inner_type, inner_map) = wrapped.clone() else {
                            return Err(RuntimeError::MissingMember {
                                target: Box::new(RuntimeValue::Aggregate(Some(type_name), map)),
                                member: name.to_string(),
                            });
                        };
                        let inner_name = inner_type.as_deref().unwrap_or_default();
                        if let Some(idx) = vm
                            .resolve_aggregate_member_slot(inner_name, &inner_map, name, short_name)
                        {
                            inner_map.0.0[idx].1.clone()
                        } else {
                            return Err(RuntimeError::MissingMember {
                                target: Box::new(RuntimeValue::Aggregate(Some(type_name), map)),
                                member: name.to_string(),
                            });
                        }
                    }
                } else {
                    match vm.resolve_associated_member_value(type_name.as_str(), name, short_name) {
                        Some(value) => {
                            let resolved_receiver =
                                RuntimeValue::Aggregate(Some(type_name), map.clone());

                            vm.bind_member_receiver_if_callable(
                                value,
                                name,
                                &raw_receiver,
                                resolved_receiver,
                            )
                        }
                        None => {
                            return Err(RuntimeError::MissingMember {
                                target: Box::new(RuntimeValue::Aggregate(Some(type_name), map)),
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
            RuntimeValue::Enum(_, _, None) if name == "next" || name == "0" => RuntimeValue::Null,
            RuntimeValue::Option(Some(x)) if name == "next" || name == "0" => x.as_ref().clone(),
            RuntimeValue::Option(Some(inner)) if !(name == "next" || name == "0") => {
                if let Some(callee) = vm.resolve_associated_member_value("option", name, short_name)
                {
                    vm.bind_member_receiver_if_callable(
                        callee,
                        name,
                        &raw_receiver,
                        RuntimeValue::Option(Some(inner.clone())),
                    )
                } else {
                    let mut inner_value = vm.resolve_value_ref(&inner.as_ref().clone())?;

                    while let RuntimeValue::Option(Some(nested)) = inner_value.clone() {
                        inner_value = vm.resolve_value_ref(&nested.as_ref().clone())?;
                    }

                    match inner_value.clone() {
                        RuntimeValue::Aggregate(type_name, map) => {
                            if let Some(idx) = vm.resolve_aggregate_member_slot(
                                type_name.as_deref().unwrap_or_default(),
                                &map,
                                name,
                                short_name,
                            ) {
                                map.0.0[idx].1.clone()
                            } else if let Some(callee) = vm.resolve_associated_member_value(
                                type_name.as_deref().unwrap_or("T"),
                                name,
                                short_name,
                            ) {
                                vm.bind_member_receiver_if_callable(
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
                                target: Box::new(RuntimeValue::Option(Some(Gc::new(other)))),
                                member: name.to_string(),
                            });
                        }
                    }
                }
            }
            RuntimeValue::Option(None) if name == "next" || name == "0" => RuntimeValue::Null,
            option @ RuntimeValue::Option(_) => {
                if let Some(callee) = vm.resolve_associated_member_value("T?", name, short_name) {
                    vm.bind_member_receiver_if_callable(callee, name, &raw_receiver, option)
                } else {
                    return Err(RuntimeError::MissingMember {
                        target: Box::new(option),
                        member: name.to_string(),
                    });
                }
            }
            RuntimeValue::Result(Ok(x)) if name == "next" || name == "0" => x.as_ref().clone(),
            RuntimeValue::Result(Err(x)) if name == "next" || name == "0" => x.as_ref().clone(),
            result @ RuntimeValue::Result(_) => {
                if let Some(callee) = vm.resolve_associated_member_value("result", name, short_name)
                {
                    vm.bind_member_receiver_if_callable(callee, name, &raw_receiver, result)
                } else {
                    return Err(RuntimeError::MissingMember {
                        target: Box::new(result),
                        member: name.to_string(),
                    });
                }
            }
            RuntimeValue::Ptr(id) if name == "next" || name == "0" => {
                vm.ptr_heap.get(&id).cloned().unwrap_or_default()
            }
            RuntimeValue::Char(value) => bind_assoc(vm, "char", RuntimeValue::Char(value))?,
            RuntimeValue::Str(value) => bind_assoc(vm, "str", RuntimeValue::Str(value))?,
            RuntimeValue::List(value) => {
                if let Some(index) = tuple_index {
                    value
                        .as_ref()
                        .0
                        .get(index)
                        .cloned()
                        .unwrap_or_else(|| RuntimeValue::Null)
                } else {
                    bind_assoc(vm, "list", RuntimeValue::List(value))?
                }
            }
            RuntimeValue::Int(value) => bind_assoc(vm, "int", RuntimeValue::Int(value))?,
            RuntimeValue::UInt(value) => bind_assoc(vm, "uint", RuntimeValue::UInt(value))?,
            RuntimeValue::Float(value) => bind_assoc(vm, "float", RuntimeValue::Float(value))?,
            RuntimeValue::Bool(value) => bind_assoc(vm, "bool", RuntimeValue::Bool(value))?,
            RuntimeValue::Null => {
                return Err(RuntimeError::MissingMember {
                    target: Box::new(RuntimeValue::Null),
                    member: name.to_string(),
                });
            }
            other => {
                if let Some(type_name) = other.impl_name() {
                    bind_assoc(vm, type_name.as_str(), other)?
                } else {
                    return Err(RuntimeError::ExpectedStructOrAggregateFound {
                        found: Box::new(other),
                    });
                }
            }
        };

        vm.set_reg_value(self.dst, val);

        match member_source {
            Some((parent, field)) => {
                vm.current_frame_mut()
                    .member_sources
                    .insert(self.dst, (parent, Ustr::from(&field)));
            }
            None => {
                let source = vm.current_frame().member_sources.get(&source_reg).cloned();
                vm.current_frame_mut().member_sources.insert(
                    self.dst,
                    source
                        .map(|(parent, path)| (parent, Ustr::from(&format!("{path}.{name}"))))
                        .unwrap_or((source_reg, Ustr::from(name))),
                );
            }
        }

        Ok(TerminateValue::None)
    }
}

impl VMEvaluation for VMSetMember {
    fn run(
        &self,
        vm: &mut VM,
        block: &VMBlock,
        _ip: u32,
        _prev_block: Option<BlockId>,
    ) -> Result<TerminateValue, RuntimeError> {
        let name = vm.local_string(block, self.member)?;
        let value = vm.get_reg_value(self.value).clone();
        let (short_name, tuple_index) = VM::member_parts(name);

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

        let mut target_value = vm.get_reg_value(self.target).clone();
        let mut handled = false;

        for _ in 0..64 {
            match target_value {
                RuntimeValue::Ref(ref_name) => {
                    let current = if let Some(value) = vm.variables.get(&ref_name).cloned() {
                        value
                    } else if let Some(value) = vm.get_function_ref(&ref_name) {
                        vm.make_runtime_function(value)
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
                            vm.variables
                                .insert(ref_name, RuntimeValue::Aggregate(name, updated))
                        }
                        RuntimeValue::List(_list) => vm.variables.insert(ref_name, value),
                        RuntimeValue::Generator { .. } => {
                            vm.variables.insert(ref_name, update_generator(current)?)
                        }
                        other => {
                            return Err(RuntimeError::ExpectedGeneratorFound {
                                found: Box::new(other),
                            });
                        }
                    };

                    if let Some(old) = old {
                        let _ = vm.set_reg_value(self.dst, old);
                    }

                    handled = true;
                    break;
                }
                RuntimeValue::VarRef(id) => {
                    let current = vm
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
                            vm.variables
                                .set_by_id(id, RuntimeValue::Aggregate(name, updated))
                        }
                        RuntimeValue::List(_list) => vm.variables.set_by_id(id, value),
                        RuntimeValue::Generator { .. } => {
                            vm.variables.set_by_id(id, update_generator(current)?)
                        }
                        other => {
                            return Err(RuntimeError::ExpectedGeneratorFound {
                                found: Box::new(other),
                            });
                        }
                    };

                    if let Some(old) = old {
                        let _ = vm.set_reg_value(self.dst, old);
                    }

                    handled = true;
                    break;
                }
                RuntimeValue::RegRef { frame, reg } => {
                    let current = vm.get_reg_value_in_frame(frame, reg).clone();
                    match current {
                        RuntimeValue::Ref(_)
                        | RuntimeValue::VarRef(_)
                        | RuntimeValue::RegRef { .. } => {
                            target_value = current;
                            continue;
                        }
                        RuntimeValue::Aggregate(name, map) => {
                            let updated = update_aggregate(&name, map)?;
                            let member_source = vm
                                .frames
                                .get(frame)
                                .and_then(|vm_frame| vm_frame.member_sources.get(&reg))
                                .cloned();

                            let _ = vm.set_reg_value_in_frame(
                                frame,
                                reg,
                                RuntimeValue::Aggregate(name, updated),
                            );

                            if let Some(source) = member_source
                                && let Some(vm_frame) = vm.frames.get_mut(frame)
                            {
                                vm_frame.member_sources.insert(reg, source);
                            }

                            let old = vm.propagate_member_source_reg(reg, frame)?;

                            if let Some(old) = old {
                                let _ = vm.set_reg_value(self.dst, old);
                            }
                        }
                        RuntimeValue::List(_) => {
                            if let Some((parent_reg, field_name)) =
                                vm.current_frame().member_sources.get(&reg).cloned()
                            {
                                let old = vm.write_back_member_field_update(
                                    frame,
                                    reg,
                                    parent_reg,
                                    &field_name,
                                )?;

                                if let Some(old) = old {
                                    let _ = vm.set_reg_value(self.dst, old);
                                }
                            }
                        }
                        RuntimeValue::Generator { .. } => {
                            let old =
                                vm.set_reg_value_in_frame(frame, reg, update_generator(current)?);

                            let _ = vm.set_reg_value(self.dst, old);
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
                        vm.current_frame().member_sources.get(&self.target).cloned();

                    let _ = vm.set_reg_value(self.target, RuntimeValue::Aggregate(name, updated));

                    if let Some(source) = member_source {
                        vm.current_frame_mut()
                            .member_sources
                            .insert(self.target, source);
                    }

                    let old = vm.propagate_member_source_reg(
                        self.target,
                        vm.frames.len().saturating_sub(1),
                    )?;

                    if let Some(old) = old {
                        let _ = vm.set_reg_value(self.dst, old);
                    }

                    handled = true;
                    break;
                }
                RuntimeValue::List(_) => {
                    if let Some((parent_reg, field_name)) =
                        vm.current_frame().member_sources.get(&self.target).cloned()
                    {
                        let old = vm.write_back_member_field_update(
                            vm.frames.len().saturating_sub(1),
                            self.target,
                            parent_reg,
                            &field_name,
                        )?;

                        if let Some(old) = old {
                            let _ = vm.set_reg_value(self.dst, old);
                        }
                    }

                    handled = true;
                    break;
                }
                current @ RuntimeValue::Generator { .. } => {
                    let old = vm.set_reg_value(self.target, update_generator(current)?);
                    let _ = vm.set_reg_value(self.dst, old);
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
}

impl VMEvaluation for VMIndex {
    fn run(
        &self,
        vm: &mut VM,
        _block: &VMBlock,
        _ip: u32,
        _prev_block: Option<BlockId>,
    ) -> Result<TerminateValue, RuntimeError> {
        let value_ref = vm.get_reg_value(self.value);
        let mut index_val = vm.get_reg_value(self.index).clone();

        if index_val.is_ref_like() {
            index_val = vm.resolve_value_ref(&index_val)?;
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
                vm.set_reg_value(self.dst, out);
                if let Some(source) = vm.current_frame().member_sources.get(&self.value).cloned() {
                    vm.current_frame_mut()
                        .member_sources
                        .insert(self.dst, source);
                }
                return Ok(TerminateValue::None);
            }
        }

        let index_list = |list: &Gc<GcVec>| -> Result<RuntimeValue, RuntimeError> {
            match &index_val {
                RuntimeValue::Int(index) => Ok(resolve_index(list.as_ref().0.len(), *index)
                    .ok()
                    .and_then(|i| list.as_ref().0.get(i).cloned())
                    .unwrap_or_else(|| RuntimeValue::Null)),
                RuntimeValue::UInt(index) => Ok(list
                    .as_ref()
                    .0
                    .get(*index as usize)
                    .cloned()
                    .unwrap_or_else(|| RuntimeValue::Null)),
                RuntimeValue::Range(start, end) => {
                    let (s, e) = resolve_slice_range(list.as_ref().0.len(), *start, *end);
                    let slice = list.as_ref().0[s..e].to_vec();
                    Ok(RuntimeValue::List(Gc::new(GcVec(slice))))
                }
                _ => Err(RuntimeError::ExpectedListOrStrFound {
                    found: Box::new(RuntimeValue::Null),
                }),
            }
        };

        let index_map = |map: &Arc<
            Mutex<rustc_hash::FxHashMap<HashKey, RuntimeValue>>,
        >|
         -> Result<RuntimeValue, RuntimeError> {
            let key = HashKey::try_from(index_val.clone())?;
            let guard = map.lock().unwrap();
            Ok(guard.get(&key).cloned().unwrap_or(RuntimeValue::Null))
        };

        let resolved = vm.resolve_value_ref(vm.get_reg_value(self.value))?;
        let val = match resolved {
            RuntimeValue::List(list) => index_list(&list)?,
            RuntimeValue::HashMap(map) => index_map(&map)?,
            RuntimeValue::Range(start, end) => match &index_val {
                RuntimeValue::Int(index) => {
                    let len = (end - start).max(0) as usize;
                    resolve_index(len, *index)
                        .map(|i| RuntimeValue::Int(start + i as i64))
                        .unwrap_or_else(|_| RuntimeValue::Null)
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
                    let (s, e) = resolve_slice_range(len, *slice_start, *slice_end);
                    RuntimeValue::Range(start + s as i64, start + e as i64)
                }
                _ => {
                    return Err(RuntimeError::ExpectedIntIndexFound {
                        found: Box::new(RuntimeValue::Null),
                    });
                }
            },
            RuntimeValue::Aggregate(None, tuple) => match &index_val {
                RuntimeValue::Int(index) => resolve_index(tuple.as_ref().0.0.len(), *index)
                    .ok()
                    .and_then(|i| tuple.as_ref().0.0.get(i).map(|(_, v)| v.clone()))
                    .unwrap_or_else(|| RuntimeValue::Null),
                RuntimeValue::UInt(index) => tuple
                    .as_ref()
                    .0
                    .0
                    .get(*index as usize)
                    .map(|(_, v)| v.clone())
                    .unwrap_or_else(|| RuntimeValue::Null),
                RuntimeValue::Range(start, end) => {
                    let (s, e) = resolve_slice_range(tuple.as_ref().0.0.len(), *start, *end);
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
                RuntimeValue::Int(index) => resolve_index(tuple.as_ref().0.0.len(), *index)
                    .ok()
                    .and_then(|i| tuple.as_ref().0.0.get(i).map(|(_, v)| v.clone()))
                    .unwrap_or_else(|| RuntimeValue::Null),
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
                        resolve_index(len, *index).ok()
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
                    let (s, e) = resolve_slice_range(v.len(), *start, *end);
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

        vm.set_reg_value(self.dst, val);
        if !vm.current_frame().member_sources.contains_key(&self.dst) {
            vm.propagate_member_source_alias(self.value, self.dst);
        }

        Ok(TerminateValue::None)
    }
}

impl VMEvaluation for VMSetIndex {
    fn run(
        &self,
        vm: &mut VM,
        _block: &VMBlock,
        _ip: u32,
        _prev_block: Option<BlockId>,
    ) -> Result<TerminateValue, RuntimeError> {
        let mut index_val = vm.get_reg_value(self.index).clone();

        if index_val.is_ref_like() {
            index_val = vm.resolve_value_ref(&index_val)?;
        }

        let value = vm.get_reg_value(self.value).clone();
        let numeric_index = || match index_val.clone() {
            RuntimeValue::Int(index) => Ok(index),
            RuntimeValue::UInt(index) => Ok(index as i64),
            _ => Err(RuntimeError::ExpectedIntIndexFound {
                found: Box::new(RuntimeValue::Null),
            }),
        };

        let hash_index = || HashKey::try_from(index_val.clone());
        let mut target_value = vm.get_reg_value(self.target).clone();
        let mut handled = false;

        for _ in 0..64 {
            match target_value {
                RuntimeValue::Ref(ref_name) => {
                    let current = if let Some(value) = vm.variables.get(&ref_name).cloned() {
                        value
                    } else if let Some(value) = vm.get_function_ref(&ref_name) {
                        vm.make_runtime_function(value)
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

                            let vec: &mut Vec<RuntimeValue> = &mut Gc::make_mut(&mut list).0;
                            let idx = resolve_index(vec.len(), index)?;
                            let old = std::mem::replace(&mut vec[idx], value);
                            let _ = vm.set_reg_value(self.dst, old);

                            vm.variables.insert(ref_name, RuntimeValue::List(list));
                            vm.propagate_member_source_reg(
                                self.target,
                                vm.frames.len().saturating_sub(1),
                            )?;
                        }
                        RuntimeValue::HashMap(map) => {
                            let key = hash_index()?;

                            let mut guard = map.lock().unwrap();

                            if let Some(old) = guard.insert(key, value) {
                                let _ = vm.set_reg_value(self.dst, old);
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
                    let current = vm
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
                            let idx = resolve_index(vec.len(), index)?;
                            let old = std::mem::replace(&mut vec[idx], value);
                            let _ = vm.set_reg_value(self.dst, old);

                            let _ = vm.variables.set_by_id(id, RuntimeValue::List(list));
                            vm.propagate_member_source_reg(
                                self.target,
                                vm.frames.len().saturating_sub(1),
                            )?;
                        }
                        RuntimeValue::HashMap(map) => {
                            let key = hash_index()?;

                            let mut guard = map.lock().unwrap();

                            if let Some(old) = guard.insert(key, value) {
                                let _ = vm.set_reg_value(self.dst, old);
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
                    let current = vm.get_reg_value_in_frame(frame, reg).clone();
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
                            let idx = resolve_index(vec.len(), index)?;
                            let old = std::mem::replace(&mut vec[idx], value);
                            let _ = vm.set_reg_value(self.dst, old);

                            let member_source = vm
                                .frames
                                .get(frame)
                                .and_then(|vm_frame| vm_frame.member_sources.get(&reg))
                                .cloned();

                            vm.set_reg_value_in_frame(frame, reg, RuntimeValue::List(list.clone()));

                            if let Some(source) = member_source
                                && let Some(vm_frame) = vm.frames.get_mut(frame)
                            {
                                vm_frame.member_sources.insert(reg, source);
                            }

                            vm.propagate_member_source_reg(reg, frame)?;
                        }
                        RuntimeValue::HashMap(map) => {
                            let key = hash_index()?;
                            let guard = map.lock().unwrap();

                            let mut guard = guard;
                            if let Some(old) = guard.insert(key, value) {
                                let _ = vm.set_reg_value(self.dst, old);
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
                    let idx = resolve_index(vec.len(), index)?;
                    let old = std::mem::replace(&mut vec[idx], value);
                    let _ = vm.set_reg_value(self.dst, old);

                    let member_source =
                        vm.current_frame().member_sources.get(&self.target).cloned();
                    vm.set_reg_value(self.target, RuntimeValue::List(list));

                    if let Some(source) = member_source {
                        vm.current_frame_mut()
                            .member_sources
                            .insert(self.target, source);
                    }

                    vm.propagate_member_source_reg(self.target, vm.frames.len().saturating_sub(1))?;

                    handled = true;
                    break;
                }
                RuntimeValue::HashMap(map) => {
                    let key = hash_index()?;

                    let mut guard = map.lock().unwrap();
                    if let Some(old) = guard.insert(key, value) {
                        let _ = vm.set_reg_value(self.dst, old);
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
}
