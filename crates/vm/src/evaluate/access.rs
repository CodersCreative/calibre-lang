use crate::{
    VM, error::RuntimeError, evaluate::instruction::{VMEvaluation, resolve_slice_range}, native::stdlib::generator::GeneratorResumeFn, value::{GcVec, RuntimeValue, TerminateValue, ValueSlot, hashable::HashKey},
};
use calibre_bytecode::{
    VMBlock,
    instructions::access::{VMDiscriminant, VMIndex, VMLoadMember},
};
use calibre_lir::{FullyQualifiedPath, TypeImplKey, TypeKey, ast::BlockId};
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
            RuntimeValue::Enum(_, index, _) => *index,
            RuntimeValue::Option(Some(_)) | RuntimeValue::Result(Ok(_)) => 0,
            RuntimeValue::Option(None) | RuntimeValue::Result(Err(_)) => 1,
            _ => 0,
        };

        vm.set_reg_value(self.dst, RuntimeValue::UInt(value));
        Ok(TerminateValue::None)
    }
}

#[inline]
fn normalize_index(len: usize, index: isize) -> Option<usize> {
    let len_i64 = len as isize;
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
        RuntimeValue::UInt(idx) => Ok((*idx < len).then_some(*idx)),
        RuntimeValue::Str(s) => {
            if let Ok(idx) = s.parse::<isize>() {
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

impl VMEvaluation for VMLoadMember {
    #[instrument(skip_all)]
    fn run(
        &self,
        vm: &mut VM,
        block: &VMBlock,
        _ip: u32,
        _prev_block: Option<BlockId>,
    ) -> Result<TerminateValue, RuntimeError> {
        let member = vm.local_string(block, self.member)?;
        let tuple_index = member.parse::<isize>().ok();
        let raw_receiver = vm.get_reg_value(self.value).clone();
        let is_next_or_zero = member == "next" || member == "0";

        let missing = |target: RuntimeValue| RuntimeError::MissingMember {
            target: Box::new(target),
            member: member.to_string(),
        };

        let resolved = vm.resolve_value_ref(&raw_receiver)?;

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
                            .map(|_| ValueSlot::new(RuntimeValue::Str(Ustr::from(&txt))))
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
                    .map(|(_, v)| RuntimeValue::GcRef(v.clone()))
                    .ok_or_else(|| missing(RuntimeValue::Aggregate(None, map)))?
            }
            RuntimeValue::Aggregate(Some(type_name), map) => {
                if let Some(idx) = vm.resolve_aggregate_member_slot(&type_name, &map, member) {
                    RuntimeValue::GcRef(map.0.0[idx].1.clone())
                } else if let Some((_, wrapped)) = map.0.0.iter().find(|(field, _)| field == "0") {
                    if tuple_index.is_some() {
                        RuntimeValue::GcRef(wrapped.clone())
                    } else {
                        let wrapped_val = vm.resolve_value_ref(&wrapped.get())?;

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

                        RuntimeValue::GcRef(inner_map.0.0[idx].1.clone())
                    }
                } else {
                    return Err(missing(RuntimeValue::Aggregate(
                        Some(type_name.clone()),
                        map.clone(),
                    )));
                }
            }
            RuntimeValue::Enum(_, _, Some(x)) if is_next_or_zero => {
                RuntimeValue::GcRef(x)
            }
            RuntimeValue::Enum(_, _, Some(x)) => 
                RuntimeValue::GcRef(x),
            RuntimeValue::Enum(_, _, None) if is_next_or_zero => RuntimeValue::Null,
            RuntimeValue::Option(Some(x)) if is_next_or_zero => {
                RuntimeValue::GcRef(x)
            }
            RuntimeValue::Option(None) if is_next_or_zero => RuntimeValue::Null,
            RuntimeValue::Result(Ok(x)) | RuntimeValue::Result(Err(x))
                if is_next_or_zero || member.as_str() == "ok" || member.as_str() == "err" =>
            {
                RuntimeValue::GcRef(x)
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

        let val = match target_val {
            RuntimeValue::List(list) => {
                let items = &list.as_ref().0;
                match &index_val {
                    RuntimeValue::Range(start, end) => {
                        resolve_slice_range(items.len(), *start, *end)
                            .and_then(|(s, e)| items.get(s..e))
                            .map(|x| {
                                RuntimeValue::Option(Some(ValueSlot::new(RuntimeValue::List(Arc::new(
                                    GcVec::from(x.to_vec()),
                                )))))
                            })
                            .unwrap_or_else(|| RuntimeValue::Option(None))
                    }
                    other => {
                        let resolved = resolve_element_index(items.len(), other)?;
                        match resolved {
                            Some(i) => RuntimeValue::Option(Some(ValueSlot::new(RuntimeValue::GcRef(
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
                        .map(|slot| ValueSlot::new(RuntimeValue::GcRef(slot.clone()))),
                )
            }
            RuntimeValue::Range(start, end) => {
                let len = (end - start).max(0) as usize;
                match &index_val {
                    RuntimeValue::Range(slice_start, slice_end) => {
                        resolve_slice_range(len, *slice_start, *slice_end)
                            .map(|(s, e)| {
                                RuntimeValue::Option(Some(ValueSlot::new(RuntimeValue::Range(
                                    start + s as isize,
                                    start + e as isize,
                                ))))
                            })
                            .unwrap_or_else(|| RuntimeValue::Option(None))
                    }
                    other => {
                        let resolved = resolve_element_index(len, other)?;
                        match resolved {
                            Some(i) => {
                                let num = start + i as isize;
                                if num > end {
                                    RuntimeValue::Option(None)
                                } else {
                                    RuntimeValue::Option(Some(ValueSlot::new(RuntimeValue::Int(num))))
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
                                RuntimeValue::Option(Some(ValueSlot::new(RuntimeValue::Str(Ustr::from(
                                    &x.iter().collect::<String>(),
                                )))))
                            })
                            .unwrap_or_else(|| RuntimeValue::Option(None))
                    }
                    other => {
                        let resolved = resolve_element_index(chars.len(), other)?;
                        match resolved.and_then(|i| chars.get(i)) {
                            Some(&ch) => {
                                RuntimeValue::Option(Some(ValueSlot::new(RuntimeValue::Char(ch))))
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
        Ok(TerminateValue::None)
    }
}
