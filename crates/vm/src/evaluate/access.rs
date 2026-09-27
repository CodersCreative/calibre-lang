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
    value::{GcMap, GcVec, RuntimeValue, TerminateValue, hashable::HashKey},
};
use calibre_lir::ast::BlockId;
use dumpster::sync::Gc;
use tracing::instrument;
use ustr::Ustr;

impl VMEvaluation for VMLoadMember {
    #[instrument(skip_all)]
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
        let is_next_or_zero = name == "next" || name == "0";
        let member_short = short_name.unwrap_or(name);

        let missing = |target: RuntimeValue| RuntimeError::MissingMember {
            target: Box::new(target),
            member: name.to_string(),
        };

        let bind_assoc = |vm: &mut VM, type_name: &str, value: RuntimeValue| {
            if let Some(callee) = vm.resolve_associated_member_value(type_name, name, short_name) {
                Ok(vm.bind_member_receiver_if_callable(callee, name, &raw_receiver, value))
            } else {
                Err(missing(value))
            }
        };

        let resolved = vm.resolve_value_ref(&raw_receiver)?;
        let mut member_source: Option<(u16, Ustr)> = None;

        let val = match resolved {
            RuntimeValue::Null => {
                if let RuntimeValue::Ref(owner) = &raw_receiver
                    && let Some(callee) =
                        vm.resolve_associated_member_value(owner, name, short_name)
                {
                    vm.set_reg_value(self.dst, callee);
                    vm.current_frame_mut()
                        .member_sources
                        .insert(self.dst, (source_reg, *name));
                    return Ok(TerminateValue::None);
                } else {
                    return Err(missing(RuntimeValue::Null));
                }
            }
            RuntimeValue::Generator { type_name, state } => match member_short {
                "data" | "next" => RuntimeValue::NativeFunction(Arc::new(GeneratorResumeFn {
                    state: state.clone(),
                })),
                "index" => RuntimeValue::Int(state.lock().unwrap().index),
                "done" => RuntimeValue::Bool(state.lock().unwrap().completed),
                _ => vm
                    .resolve_associated_member_value(type_name.as_str(), name, short_name)
                    .ok_or_else(|| missing(RuntimeValue::Generator { type_name, state }))?,
            },
            RuntimeValue::DynObject {
                type_name,
                value,
                vtable,
                constraints,
            } => {
                let member_short_ustr = Ustr::from(member_short);
                if let Some(callee_name) =
                    vtable.get(&member_short_ustr).or_else(|| vtable.get(name))
                {
                    if let Some(callee) = vm.resolve_dyn_method_callable(
                        type_name.as_str(),
                        member_short_ustr.as_str(),
                        Some(callee_name.as_str()),
                    ) {
                        callee.bind_if_callable(value.as_ref().clone())
                    } else {
                        vm.get_value(callee_name).ok_or_else(|| {
                            RuntimeError::FunctionNotFound(callee_name.to_string())
                        })?
                    }
                } else if let Some(callee) = vm.resolve_dyn_method_callable(
                    type_name.as_str(),
                    member_short_ustr.as_str(),
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
                    vm.get_value(&Ustr::from(&format!("{type_name}.{member_short}")))
                {
                    x
                } else {
                    return Err(missing(RuntimeValue::DynObject {
                        type_name,
                        constraints,
                        value,
                        vtable,
                    }));
                }
            }
            RuntimeValue::Aggregate(None, map) => {
                let idx = tuple_index.ok_or(RuntimeError::ExpectedIntIndexFound {
                    found: Box::new(RuntimeValue::Null),
                })?;
                map.as_ref()
                    .0
                    .0
                    .get(idx)
                    .map(|(_, val)| val.clone())
                    .ok_or_else(|| missing(RuntimeValue::Aggregate(None, map)))?
            }
            RuntimeValue::Aggregate(Some(type_name), map) => {
                if let Some(idx) =
                    vm.resolve_aggregate_member_slot(&type_name, &map, name, short_name)
                {
                    let field_name = &map.0.0[idx].0;
                    member_source = Some(
                        vm.current_frame()
                            .member_sources
                            .get(&source_reg)
                            .map(|(parent, path)| {
                                (*parent, Ustr::from(&format!("{path}.{field_name}")))
                            })
                            .unwrap_or((source_reg, Ustr::from(field_name))),
                    );
                    map.0.0[idx].1.clone()
                } else if let Some((_, wrapped)) = map.0.0.iter().find(|(field, _)| field == "0") {
                    let wrapped_val = vm.resolve_value_ref(wrapped)?;
                    if tuple_index.is_some() {
                        member_source = Some(
                            vm.current_frame()
                                .member_sources
                                .get(&source_reg)
                                .map(|(parent, path)| (*parent, Ustr::from(&format!("{path}.0"))))
                                .unwrap_or((source_reg, Ustr::from("0"))),
                        );
                        wrapped_val
                    } else {
                        let RuntimeValue::Aggregate(inner_type, inner_map) = wrapped_val else {
                            return Err(missing(RuntimeValue::Aggregate(Some(type_name), map)));
                        };
                        let inner_name = inner_type.as_deref().unwrap_or_default();
                        let idx = vm
                            .resolve_aggregate_member_slot(inner_name, &inner_map, name, short_name)
                            .ok_or_else(|| {
                                missing(RuntimeValue::Aggregate(Some(type_name), map.clone()))
                            })?;
                        inner_map.0.0[idx].1.clone()
                    }
                } else {
                    let value = vm
                        .resolve_associated_member_value(type_name.as_str(), name, short_name)
                        .ok_or_else(|| {
                            missing(RuntimeValue::Aggregate(Some(type_name), map.clone()))
                        })?;

                    vm.bind_member_receiver_if_callable(
                        value,
                        name,
                        &raw_receiver,
                        RuntimeValue::Aggregate(Some(type_name), map),
                    )
                }
            }
            RuntimeValue::Enum(_, _, Some(x)) if is_next_or_zero => x.as_ref().clone(),
            RuntimeValue::Enum(_, _, Some(x)) => x.as_ref().clone(),
            RuntimeValue::Enum(_, _, None) if is_next_or_zero => RuntimeValue::Null,
            RuntimeValue::Option(Some(x)) if is_next_or_zero => x.as_ref().clone(),
            RuntimeValue::Option(Some(inner)) => {
                if let Some(callee) = vm.resolve_associated_member_value("option", name, short_name)
                {
                    vm.bind_member_receiver_if_callable(
                        callee,
                        name,
                        &raw_receiver,
                        RuntimeValue::Option(Some(inner)),
                    )
                } else {
                    return Err(missing(RuntimeValue::Option(Some(inner))));
                }
            }
            RuntimeValue::Option(None) if is_next_or_zero => RuntimeValue::Null,
            option @ RuntimeValue::Option(_) => {
                let callee = vm
                    .resolve_associated_member_value("T?", name, short_name)
                    .ok_or_else(|| missing(option.clone()))?;
                vm.bind_member_receiver_if_callable(callee, name, &raw_receiver, option)
            }

            RuntimeValue::Result(Ok(x)) | RuntimeValue::Result(Err(x)) if is_next_or_zero => {
                x.as_ref().clone()
            }
            result @ RuntimeValue::Result(_) => {
                let callee = vm
                    .resolve_associated_member_value("result", name, short_name)
                    .ok_or_else(|| missing(result.clone()))?;
                vm.bind_member_receiver_if_callable(callee, name, &raw_receiver, result)
            }

            RuntimeValue::Ptr(id) if is_next_or_zero => {
                vm.ptr_heap.get(&id).cloned().unwrap_or_default()
            }

            RuntimeValue::Char(v) => bind_assoc(vm, "char", RuntimeValue::Char(v))?,
            RuntimeValue::Str(v) => bind_assoc(vm, "str", RuntimeValue::Str(v))?,
            RuntimeValue::List(v) => {
                if let Some(index) = tuple_index {
                    v.as_ref()
                        .0
                        .get(index)
                        .cloned()
                        .unwrap_or(RuntimeValue::Null)
                } else {
                    bind_assoc(vm, "list", RuntimeValue::List(v))?
                }
            }
            RuntimeValue::Int(v) => bind_assoc(vm, "int", RuntimeValue::Int(v))?,
            RuntimeValue::UInt(v) => bind_assoc(vm, "uint", RuntimeValue::UInt(v))?,
            RuntimeValue::Float(v) => bind_assoc(vm, "float", RuntimeValue::Float(v))?,
            RuntimeValue::Bool(v) => bind_assoc(vm, "bool", RuntimeValue::Bool(v))?,
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

        let frame = vm.current_frame_mut();
        let final_source = match member_source {
            Some(source) => source,
            None => match frame.member_sources.get(&source_reg).cloned() {
                Some((parent, path)) => (parent, Ustr::from(&format!("{path}.{name}"))),
                None => (source_reg, Ustr::from(name)),
            },
        };
        frame.member_sources.insert(self.dst, final_source);

        Ok(TerminateValue::None)
    }
}

impl VMEvaluation for VMSetMember {
    #[instrument(skip_all)]
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
    #[instrument(skip_all)]
    fn run(
        &self,
        vm: &mut VM,
        _block: &VMBlock,
        _ip: u32,
        _prev_block: Option<BlockId>,
    ) -> Result<TerminateValue, RuntimeError> {
        let target_val = vm.resolve_and_unwrap_values_ref(vm.get_reg_value(self.value))?;

        let index_val = vm.resolve_value_ref(vm.get_reg_value(self.index))?;

        let resolve_single_index =
            |len: usize, idx: &RuntimeValue| -> Result<Option<usize>, RuntimeError> {
                match idx {
                    RuntimeValue::Int(i) => Ok(resolve_index(len, *i).ok()),
                    RuntimeValue::UInt(u) => {
                        let idx_usize = *u as usize;
                        Ok((idx_usize < len).then_some(idx_usize))
                    }
                    other => Err(RuntimeError::ExpectedIntIndexFound {
                        found: Box::new(other.clone()),
                    }),
                }
            };

        let val = match target_val {
            RuntimeValue::List(list) => {
                let items = &list.as_ref().0;
                match &index_val {
                    RuntimeValue::Range(start, end) => {
                        resolve_slice_range(items.len(), *start, *end)
                            .and_then(|(s, e)| items.get(s..e))
                            .map(|x| {
                                RuntimeValue::Option(Some(Gc::new(RuntimeValue::List(Gc::new(
                                    GcVec(x.to_vec()),
                                )))))
                            })
                            .unwrap_or_else(|| RuntimeValue::Option(None))
                    }
                    other => {
                        let resolved = resolve_single_index(items.len(), other)?;
                        match resolved {
                            Some(i) => RuntimeValue::Option(Some(Gc::new(items[i].clone()))),
                            None => RuntimeValue::Option(None),
                        }
                    }
                }
            }
            RuntimeValue::Range(start, end) => {
                let len = (end - start).max(0) as usize;
                match &index_val {
                    RuntimeValue::Range(slice_start, slice_end) => {
                        resolve_slice_range(len, *slice_start, *slice_end)
                            .map(|(s, e)| {
                                RuntimeValue::Option(Some(Gc::new(RuntimeValue::Range(
                                    start + s as i64,
                                    start + e as i64,
                                ))))
                            })
                            .unwrap_or_else(|| RuntimeValue::Option(None))
                    }
                    other => {
                        let resolved = resolve_single_index(len, other)?;
                        match resolved {
                            Some(i) => {
                                let num = start + i as i64;
                                if num > end {
                                    RuntimeValue::Option(None)
                                } else {
                                    RuntimeValue::Option(Some(Gc::new(RuntimeValue::Int(num))))
                                }
                            }
                            None => RuntimeValue::Option(None),
                        }
                    }
                }
            }
            RuntimeValue::Str(s) => {
                let chars: Vec<char> = s.chars().collect();
                match &index_val {
                    RuntimeValue::Range(start, end) => {
                        resolve_slice_range(chars.len(), *start, *end)
                            .and_then(|(s, e)| chars.get(s..e))
                            .map(|x| {
                                RuntimeValue::Option(Some(Gc::new(RuntimeValue::Str(Ustr::from(
                                    &x.iter().collect::<String>(),
                                )))))
                            })
                            .unwrap_or_else(|| RuntimeValue::Option(None))
                    }
                    other => {
                        let resolved = match other {
                            RuntimeValue::Int(i) => {
                                if *i < 0 {
                                    resolve_index(chars.len(), *i).ok()
                                } else {
                                    Some(*i as usize)
                                }
                            }
                            RuntimeValue::UInt(u) => Some(*u as usize),
                            x => {
                                return Err(RuntimeError::ExpectedIntIndexFound {
                                    found: Box::new(x.clone()),
                                });
                            }
                        };

                        match resolved.and_then(|i| chars.get(i)) {
                            Some(&ch) => {
                                RuntimeValue::Option(Some(Gc::new(RuntimeValue::Char(ch))))
                            }
                            None => RuntimeValue::Option(None),
                        }
                    }
                }
            }
            other => match &index_val {
                RuntimeValue::Range(..) => {
                    return Err(RuntimeError::ExpectedListOrStrFound {
                        found: Box::new(other),
                    });
                }
                _ => {
                    return Err(RuntimeError::UnexpectedTypeInIndexAccess {
                        target: Box::new(other),
                        index: Box::new(index_val),
                    });
                }
            },
        };

        vm.set_reg_value(self.dst, val);
        if !vm.current_frame().member_sources.contains_key(&self.dst) {
            vm.propagate_member_source_alias(self.value, self.dst);
        }

        Ok(TerminateValue::None)
    }
}

impl VMEvaluation for VMSetIndex {
    #[instrument(skip_all)]
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

                            let mut guard = map.map.lock().unwrap();

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

                            let mut guard = map.map.lock().unwrap();

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
                            let guard = map.map.lock().unwrap();

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

                    let mut guard = map.map.lock().unwrap();
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
