use crate::{
    MutationHandle, PathSegment, VM,
    error::RuntimeError,
    evaluate::{
        instruction::{VMEvaluation, resolve_slice_range},
        write_back::Propagation,
    },
    native::stdlib::generator::GeneratorResumeFn,
    value::{GcMap, GcVec, RuntimeValue, TerminateValue, hashable::HashKey},
};
use calibre_bytecode::{
    VMBlock,
    instructions::access::{VMDiscriminant, VMIndex, VMLoadMember, VMSetIndex, VMSetMember},
};
use calibre_lir::{FullyQualifiedPath, TypeImplKey, TypeKey, ast::BlockId};
use dumpster::sync::Gc;
use std::sync::Arc;
use tracing::instrument;
use ustr::Ustr;

impl VMEvaluation for VMDiscriminant {
    fn run(
        &self,
        vm: &mut VM,
        _block: &VMBlock,
        _ip: u32,
        _prev_block: Option<BlockId>,
    ) -> Result<TerminateValue, RuntimeError> {
        let value = match vm.get_reg_value(self.value) {
            RuntimeValue::Enum(_, index, _) => *index as i64,
            RuntimeValue::Option(Some(_)) | RuntimeValue::Result(Ok(_)) => 0,
            RuntimeValue::Option(None) | RuntimeValue::Result(Err(_)) => 1,
            _ => 0,
        };

        vm.set_reg_value(self.dst, RuntimeValue::Int(value));
        Ok(TerminateValue::None)
    }
}

#[inline]
fn normalize_index(len: usize, index: i64) -> Option<usize> {
    let len_i64 = len as i64;
    let actual = if index < 0 { len_i64 + index } else { index };

    if actual >= 0 && actual < len_i64 {
        Some(actual as usize)
    } else {
        None
    }
}

#[inline]
fn resolve_element_index(len: usize, index: &RuntimeValue) -> Result<Option<usize>, RuntimeError> {
    match index {
        RuntimeValue::Int(idx) => Ok(normalize_index(len, *idx)),
        RuntimeValue::UInt(idx) => {
            let idx = *idx as usize;
            Ok((idx < len).then_some(idx))
        }
        RuntimeValue::Str(s) => {
            if let Ok(idx) = s.parse::<i64>() {
                Ok(normalize_index(len, idx))
            } else {
                Err(RuntimeError::ExpectedIntIndexFound {
                    found: Box::new(index.clone()),
                })
            }
        }
        other => Err(RuntimeError::ExpectedIntIndexFound {
            found: Box::new(other.clone()),
        }),
    }
}

#[inline]
fn resolve_numeric_index(index: &RuntimeValue) -> Result<i64, RuntimeError> {
    match index {
        RuntimeValue::Int(idx) => Ok(*idx),
        RuntimeValue::UInt(idx) => Ok(*idx as i64),
        RuntimeValue::Str(s) => s
            .parse::<i64>()
            .map_err(|_| RuntimeError::ExpectedIntIndexFound {
                found: Box::new(index.clone()),
            }),
        _ => Err(RuntimeError::ExpectedIntIndexFound {
            found: Box::new(RuntimeValue::Null),
        }),
    }
}

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
        let member = vm.local_string(block, self.member)?;
        let tuple_index = member.parse::<i64>().ok();
        let raw_receiver = vm.get_reg_value(self.value).clone();
        let is_next_or_zero = member == "next" || member == "0";

        let missing = |target: RuntimeValue| RuntimeError::MissingMember {
            target: Box::new(target),
            member: member.to_string(),
        };

        let resolved = vm.resolve_value_ref(&raw_receiver)?;
        let mut member_source: Option<Arc<MutationHandle>> = None;

        let val = match resolved {
            RuntimeValue::Null if member == "done" => RuntimeValue::Bool(true),
            RuntimeValue::Generator {
                type_name: TypeImplKey::Nominal(type_name),
                state,
            } => match member.as_str() {
                "data" | "next" | "0" => {
                    RuntimeValue::NativeFunction(Arc::new(GeneratorResumeFn {
                        state: state.clone(),
                    }))
                }
                "index" => RuntimeValue::Int(state.lock().unwrap().index),
                "done" => RuntimeValue::Bool(state.lock().unwrap().completed),
                _ => {
                    return Err(missing(RuntimeValue::Generator {
                        type_name: TypeImplKey::Nominal(type_name.clone()),
                        state,
                    }));
                }
            },
            RuntimeValue::Reader(x) if member == "gen_next" => {
                vm.set_reg_value(
                    self.dst,
                    RuntimeValue::Option(x.0.lock().ok().and_then(|mut x| {
                        let mut txt = String::new();
                        x.read_line(&mut txt)
                            .ok()
                            .map(|_| Gc::new(RuntimeValue::Str(Ustr::from(&txt))))
                    })),
                );
                return Ok(TerminateValue::None);
            }
            RuntimeValue::Aggregate(None, map) => {
                let idx = tuple_index
                    .and_then(|i| normalize_index(map.as_ref().0.0.len(), i))
                    .ok_or_else(|| RuntimeError::ExpectedIntIndexFound {
                        found: Box::new(RuntimeValue::Null),
                    })?;

                map.as_ref()
                    .0
                    .0
                    .get(idx)
                    .map(|(_, v)| RuntimeValue::from(v.clone()))
                    .ok_or_else(|| missing(RuntimeValue::Aggregate(None, map)))?
            }
            RuntimeValue::Aggregate(Some(type_name), map) => {
                if let Some(idx) = vm.resolve_aggregate_member_slot(&type_name, &map, member) {
                    let field_name = &map.0.0[idx].0;
                    member_source = Some(vm.new_mutation_handle(
                        source_reg,
                        PathSegment::Field(Ustr::from(field_name)),
                    ));
                    RuntimeValue::from(map.0.0[idx].1.clone())
                } else if let Some((_, wrapped)) = map.0.0.iter().find(|(field, _)| field == "0") {
                    let wrapped_val = vm.resolve_value_ref(wrapped.as_ref())?;
                    if let Some(i) = tuple_index {
                        let resolved_idx = normalize_index(map.0.0.len(), i).unwrap_or_default();
                        member_source = Some(
                            vm.new_mutation_handle(source_reg, PathSegment::Index(resolved_idx)),
                        );
                        wrapped_val
                    } else {
                        let RuntimeValue::Aggregate(inner_type, inner_map) = wrapped_val else {
                            return Err(missing(RuntimeValue::Aggregate(Some(type_name), map)));
                        };

                        let inner_name = inner_type
                            .as_ref()
                            .map(|k| *k.name())
                            .unwrap_or_else(|| Ustr::from(""));

                        let inner_type_key = TypeKey {
                            fully_qualified_path: inner_type
                                .as_ref()
                                .map(|k| k.fully_qualified_path.clone())
                                .unwrap_or_else(|| {
                                    Arc::new(FullyQualifiedPath {
                                        name: Some(inner_name),
                                        parent: None,
                                    })
                                }),
                        };

                        let idx = vm
                            .resolve_aggregate_member_slot(&inner_type_key, &inner_map, member)
                            .ok_or_else(|| {
                                missing(RuntimeValue::Aggregate(Some(type_name), map.clone()))
                            })?;

                        member_source = Some(vm.extend_mutation_handle(
                            source_reg,
                            &[
                                PathSegment::Field(Ustr::from("0")),
                                PathSegment::Field(Ustr::from(inner_map.0.0[idx].0.as_str())),
                            ],
                        ));

                        RuntimeValue::from(inner_map.0.0[idx].1.clone())
                    }
                } else {
                    return Err(missing(RuntimeValue::Aggregate(
                        Some(type_name.clone()),
                        map.clone(),
                    )));
                }
            }
            RuntimeValue::Enum(_, _, Some(x)) if is_next_or_zero => {
                member_source = Some(vm.unwrap_mutation_handle(source_reg));
                x.as_ref().clone()
            }
            RuntimeValue::Enum(_, _, Some(x)) => x.as_ref().clone(),
            RuntimeValue::Enum(_, _, None) if is_next_or_zero => RuntimeValue::Null,
            RuntimeValue::Option(Some(x)) if is_next_or_zero => {
                member_source = Some(vm.unwrap_mutation_handle(source_reg));
                x.as_ref().clone()
            }
            RuntimeValue::Option(None) if is_next_or_zero => RuntimeValue::Null,
            RuntimeValue::Result(Ok(x)) | RuntimeValue::Result(Err(x))
                if is_next_or_zero || member.as_str() == "ok" || member.as_str() == "err" =>
            {
                member_source = Some(vm.unwrap_mutation_handle(source_reg));
                x.as_ref().clone()
            }
            RuntimeValue::Ptr(id) if is_next_or_zero => {
                vm.ptr_heap.get(&id).cloned().unwrap_or_default()
            }
            other => {
                return Err(RuntimeError::ExpectedStructOrAggregateFound {
                    found: Box::new(other),
                });
            }
        };

        vm.set_reg_value(self.dst, val);

        let final_source = member_source.unwrap_or_else(|| {
            if let Some(i) = tuple_index {
                let resolved_idx = normalize_index(usize::MAX, i).unwrap_or_default();
                vm.new_mutation_handle(source_reg, PathSegment::Index(resolved_idx))
            } else {
                vm.new_mutation_handle(source_reg, PathSegment::Field(*member))
            }
        });

        vm.current_frame_mut()
            .set_shared_mutation_handle(self.dst, final_source);

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
        let member = vm.local_string(block, self.member)?;
        let tuple_index = member.parse::<i64>().ok();
        let value = vm.get_reg_value(self.value).clone();

        let update_aggregate = |agg_name: &Option<TypeKey>, mut map: Arc<GcMap>| {
            let entries = &mut Arc::make_mut(&mut map);

            match (agg_name.as_ref(), tuple_index) {
                (None, Some(idx)) => {
                    let resolved_idx =
                        normalize_index(entries.len(), idx).ok_or(RuntimeError::StackUnderflow)?;
                    entries[resolved_idx].1 = value.clone().into();
                }
                (Some(_), _) => {
                    if let Some(entry) = entries.iter_mut().find(|entry| entry.0 == *member) {
                        entry.1 = value.clone().into();
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

                if !matches!(member.as_str(), "done" | "index") {
                    return Err(RuntimeError::MissingMember {
                        target: Box::new(RuntimeValue::Generator { type_name, state }),
                        member: member.to_string(),
                    });
                }

                let mut guard = state.lock().unwrap();
                match member.as_str() {
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
                    let current = if let Some(val) = vm.variables.get(&ref_name).cloned() {
                        val
                    } else if let Some(val) = vm.get_function_ref(&ref_name) {
                        vm.make_runtime_function(val)
                    } else {
                        return Err(RuntimeError::DanglingRef(ref_name.to_string()));
                    };

                    match &current {
                        RuntimeValue::Ref(_)
                        | RuntimeValue::VarRef(_)
                        | RuntimeValue::RegRef { .. } => {
                            target_value = current;
                            continue;
                        }
                        RuntimeValue::Aggregate(name, map) => {
                            let updated = update_aggregate(name, map.clone())?;
                            if let Some(old) = vm
                                .variables
                                .insert(ref_name, RuntimeValue::Aggregate(name.clone(), updated))
                            {
                                let _ = vm.set_reg_value(self.dst, old);
                            }
                        }
                        RuntimeValue::List(_) => {
                            if let Some(old) = vm.variables.insert(ref_name, current) {
                                let _ = vm.set_reg_value(self.dst, old);
                            }
                        }
                        RuntimeValue::Generator { .. } => {
                            let updated_gen = update_generator(current)?;
                            if let Some(old) = vm.variables.insert(ref_name, updated_gen) {
                                let _ = vm.set_reg_value(self.dst, old);
                            }
                        }
                        other => {
                            return Err(RuntimeError::ExpectedGeneratorFound {
                                found: Box::new(other.clone()),
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
                        .ok_or_else(|| RuntimeError::DanglingRef(format!("#{}", id)))?;

                    match &current {
                        RuntimeValue::Ref(_)
                        | RuntimeValue::VarRef(_)
                        | RuntimeValue::RegRef { .. } => {
                            target_value = current;
                            continue;
                        }
                        RuntimeValue::Aggregate(name, map) => {
                            let updated = update_aggregate(name, map.clone())?;
                            if let Some(old) = vm
                                .variables
                                .set_by_id(id, RuntimeValue::Aggregate(name.clone(), updated))
                            {
                                let _ = vm.set_reg_value(self.dst, old);
                            }
                        }
                        RuntimeValue::List(_) => {
                            if let Some(old) = vm.variables.set_by_id(id, value.clone()) {
                                let _ = vm.set_reg_value(self.dst, old);
                            }
                        }
                        RuntimeValue::Generator { .. } => {
                            let updated_gen = update_generator(current)?;
                            if let Some(old) = vm.variables.set_by_id(id, updated_gen) {
                                let _ = vm.set_reg_value(self.dst, old);
                            }
                        }
                        other => {
                            return Err(RuntimeError::ExpectedGeneratorFound {
                                found: Box::new(other.clone()),
                            });
                        }
                    }

                    handled = true;
                    break;
                }
                RuntimeValue::RegRef { frame, reg } => {
                    let current = vm.get_reg_value_in_frame(frame, reg).clone();
                    match &current {
                        RuntimeValue::Ref(_)
                        | RuntimeValue::VarRef(_)
                        | RuntimeValue::RegRef { .. } => {
                            target_value = current;
                            continue;
                        }
                        RuntimeValue::Aggregate(name, map) => {
                            let updated = update_aggregate(name, map.clone())?;
                            let member_source = vm
                                .frames
                                .get(frame)
                                .and_then(|vm_frame| vm_frame.get_mutation_handle(reg));

                            let _ = vm.set_reg_value_in_frame(
                                frame,
                                reg,
                                RuntimeValue::Aggregate(name.clone(), updated),
                            );

                            if let Some(source) = member_source
                                && let Some(vm_frame) = vm.frames.get_mut(frame)
                            {
                                vm_frame.set_shared_mutation_handle(reg, source);
                            }

                            if let Some(old) = vm.propagate_member_source_reg(reg, frame)? {
                                let _ = vm.set_reg_value(self.dst, old);
                            }
                        }
                        RuntimeValue::List(_) => {
                            if let Some(handle) = vm
                                .frames
                                .get(frame)
                                .and_then(|frame| frame.get_mutation_handle(reg))
                            {
                                let field = vm.get_reg_value_in_frame(frame, reg).clone();
                                if let Some(old) = vm.replace_mutation_handle(&handle, field) {
                                    let _ = vm.set_reg_value(self.dst, old);
                                }
                            }
                        }
                        RuntimeValue::Generator { .. } => {
                            let updated_gen = update_generator(current)?;
                            let old = vm.set_reg_value_in_frame(frame, reg, updated_gen);
                            let _ = vm.set_reg_value(self.dst, old);
                        }
                        other => {
                            return Err(RuntimeError::ExpectedGeneratorFound {
                                found: Box::new(other.clone()),
                            });
                        }
                    }
                    handled = true;
                    break;
                }
                RuntimeValue::Aggregate(name, map) => {
                    let updated = update_aggregate(&name, map)?;
                    let member_source = vm.current_frame().get_mutation_handle(self.target);

                    let _ = vm.set_reg_value(self.target, RuntimeValue::Aggregate(name, updated));

                    if let Some(source) = member_source {
                        vm.current_frame_mut()
                            .set_shared_mutation_handle(self.target, source);
                    }

                    if let Some(old) = vm.propagate_member_source_reg(
                        self.target,
                        vm.frames.len().saturating_sub(1),
                    )? {
                        let _ = vm.set_reg_value(self.dst, old);
                    }

                    handled = true;
                    break;
                }
                RuntimeValue::List(_) => {
                    if let Some(handle) = vm.get_mutation_handle(self.target) {
                        let field = vm.get_reg_value(self.target).clone();
                        if let Some(old) = vm.replace_mutation_handle(&handle, field) {
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

        let indexed_segment = match &target_val {
            RuntimeValue::List(list) => match &index_val {
                RuntimeValue::Range(..) => None,
                other => {
                    resolve_element_index(list.as_ref().0.len(), other)?.map(PathSegment::Index)
                }
            },
            RuntimeValue::Str(s) => match &index_val {
                RuntimeValue::Range(..) => None,
                other => {
                    let chars_len = s.chars().count();
                    resolve_element_index(chars_len, other)?.map(PathSegment::Index)
                }
            },
            RuntimeValue::HashMap(_) => {
                Some(PathSegment::MapKey(HashKey::try_from(index_val.clone())?))
            }
            _ => None,
        };

        let val = match target_val {
            RuntimeValue::List(list) => {
                let items = &list.as_ref().0;
                match &index_val {
                    RuntimeValue::Range(start, end) => {
                        resolve_slice_range(items.len(), *start, *end)
                            .and_then(|(s, e)| items.get(s..e))
                            .map(|x| {
                                RuntimeValue::Option(Some(Gc::new(RuntimeValue::List(Arc::new(
                                    GcVec::from(x.to_vec()),
                                )))))
                            })
                            .unwrap_or_else(|| RuntimeValue::Option(None))
                    }
                    other => {
                        let resolved = resolve_element_index(items.len(), other)?;
                        match resolved {
                            Some(i) => RuntimeValue::Option(Some(Gc::new(RuntimeValue::from(
                                items[i].clone(),
                            )))),
                            None => RuntimeValue::Option(None),
                        }
                    }
                }
            }
            RuntimeValue::HashMap(map) => {
                let key = HashKey::try_from(index_val.clone()).map_err(|_| {
                    RuntimeError::UnexpectedTypeInIndexAccess {
                        target: Box::new(RuntimeValue::HashMap(map.clone())),
                        index: Box::new(index_val.clone()),
                    }
                })?;

                RuntimeValue::Option(
                    map.map
                        .get(&key)
                        .map(|slot| Gc::new(RuntimeValue::from(slot.clone()))),
                )
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
                        let resolved = resolve_element_index(len, other)?;
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
                        let resolved = resolve_element_index(chars.len(), other)?;
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

        if let Some(segment) = indexed_segment {
            let handle = vm.new_mutation_handle(self.value, segment);

            vm.current_frame_mut()
                .set_shared_mutation_handle(self.dst, handle);
        } else if vm.current_frame().get_mutation_handle(self.dst).is_none() {
            vm.propagate_member_source_alias(self.value, self.dst);
        }

        Ok(TerminateValue::None)
    }
}

impl VMEvaluation for VMSetIndex {
    #[allow(clippy::mutable_key_type)]
    #[instrument(skip_all)]
    fn run(
        &self,
        vm: &mut VM,
        _block: &VMBlock,
        _ip: u32,
        _prev_block: Option<BlockId>,
    ) -> Result<TerminateValue, RuntimeError> {
        let index_val = vm.resolve_value_ref(vm.get_reg_value(self.index))?;
        let value = vm.get_reg_value(self.value).clone();
        let index_i64 = resolve_numeric_index(&index_val)?;

        let update_container = |vm: &mut VM,
                                current: RuntimeValue|
         -> Result<(RuntimeValue, RuntimeValue), RuntimeError> {
            match current {
                RuntimeValue::List(mut list) => {
                    let vec = Arc::make_mut(&mut list);
                    let idx = normalize_index(vec.len(), index_i64)
                        .ok_or(RuntimeError::StackUnderflow)?;
                    let old = std::mem::replace(&mut vec[idx], value.clone().into());
                    Ok((RuntimeValue::List(list), RuntimeValue::from(old)))
                }
                RuntimeValue::HashMap(mut map) => {
                    let key = HashKey::try_from(index_val.clone()).map_err(|_| {
                        RuntimeError::UnexpectedTypeInIndexAccess {
                            target: Box::new(RuntimeValue::HashMap(map.clone())),
                            index: Box::new(index_val.clone()),
                        }
                    })?;
                    let table = Arc::make_mut(&mut map.map);
                    let old = table
                        .get(&key)
                        .map(|v| RuntimeValue::from(v.clone()))
                        .unwrap_or(RuntimeValue::Null);
                    table.insert(key, value.clone().into());
                    Ok((RuntimeValue::HashMap(map), old))
                }
                RuntimeValue::Str(txt) => {
                    let chars: Vec<char> = txt.chars().collect();
                    let idx = normalize_index(chars.len(), index_i64)
                        .ok_or(RuntimeError::StackUnderflow)?;

                    let mut txt_str = txt.to_string();
                    let byte_offset = txt_str
                        .char_indices()
                        .nth(idx)
                        .map(|(b_idx, _)| b_idx)
                        .ok_or(RuntimeError::StackUnderflow)?;
                    let char_len = txt_str[byte_offset..]
                        .chars()
                        .next()
                        .map(|c| c.len_utf8())
                        .unwrap_or(1);

                    let replacement = value.display(vm);
                    let old_char = chars[idx];
                    txt_str.replace_range(byte_offset..byte_offset + char_len, &replacement);

                    Ok((
                        RuntimeValue::Str(Ustr::from(&txt_str)),
                        RuntimeValue::Char(old_char),
                    ))
                }
                other => Err(RuntimeError::ExpectedListOrStrFound {
                    found: Box::new(other),
                }),
            }
        };

        let mut target_value = vm.get_reg_value(self.target).clone();
        let mut handled = false;

        for _ in 0..64 {
            match target_value {
                RuntimeValue::Ref(ref_name) => {
                    let current = if let Some(val) = vm.variables.get(&ref_name).cloned() {
                        val
                    } else if let Some(val) = vm.get_function_ref(&ref_name) {
                        vm.make_runtime_function(val)
                    } else {
                        return Err(RuntimeError::DanglingRef(ref_name.to_string()));
                    };

                    if matches!(
                        current,
                        RuntimeValue::Ref(_)
                            | RuntimeValue::VarRef(_)
                            | RuntimeValue::RegRef { .. }
                    ) {
                        target_value = current;
                        continue;
                    }

                    let (new_val, old_val) = update_container(vm, current)?;
                    vm.variables.insert(ref_name, new_val);
                    vm.set_reg_value(self.dst, old_val);
                    vm.propagate_member_source_reg(self.target, vm.frames.len().saturating_sub(1))?;
                    handled = true;
                    break;
                }
                RuntimeValue::VarRef(id) => {
                    let current = vm
                        .variables
                        .get_by_id(id)
                        .cloned()
                        .ok_or_else(|| RuntimeError::DanglingRef(format!("#{}", id)))?;

                    if matches!(
                        current,
                        RuntimeValue::Ref(_)
                            | RuntimeValue::VarRef(_)
                            | RuntimeValue::RegRef { .. }
                    ) {
                        target_value = current;
                        continue;
                    }

                    let (new_val, old_val) = update_container(vm, current)?;
                    vm.variables.set_by_id(id, new_val);
                    vm.set_reg_value(self.dst, old_val);
                    vm.propagate_member_source_reg(self.target, vm.frames.len().saturating_sub(1))?;
                    handled = true;
                    break;
                }
                RuntimeValue::RegRef { frame, reg } => {
                    let current = vm.get_reg_value_in_frame(frame, reg).clone();
                    if matches!(
                        current,
                        RuntimeValue::Ref(_)
                            | RuntimeValue::VarRef(_)
                            | RuntimeValue::RegRef { .. }
                    ) {
                        target_value = current;
                        continue;
                    }

                    let (new_val, old_val) = update_container(vm, current)?;
                    let member_source = vm
                        .frames
                        .get(frame)
                        .and_then(|vm_frame| vm_frame.get_mutation_handle(reg));

                    vm.set_reg_value_in_frame(frame, reg, new_val);

                    if let Some(source) = member_source
                        && let Some(vm_frame) = vm.frames.get_mut(frame)
                    {
                        vm_frame.set_shared_mutation_handle(reg, source);
                    }

                    vm.set_reg_value(self.dst, old_val);
                    vm.propagate_member_source_reg(reg, frame)?;
                    handled = true;
                    break;
                }
                other => {
                    if matches!(
                        other,
                        RuntimeValue::Ref(_)
                            | RuntimeValue::VarRef(_)
                            | RuntimeValue::RegRef { .. }
                    ) {
                        target_value = other;
                        continue;
                    }

                    let (new_val, old_val) = update_container(vm, other)?;
                    let member_source = vm.current_frame().get_mutation_handle(self.target);
                    vm.set_reg_value(self.target, new_val);

                    if let Some(source) = member_source {
                        vm.current_frame_mut()
                            .set_shared_mutation_handle(self.target, source);
                    }

                    vm.set_reg_value(self.dst, old_val);
                    vm.propagate_member_source_reg(self.target, vm.frames.len().saturating_sub(1))?;
                    handled = true;
                    break;
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
