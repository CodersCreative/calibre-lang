use crate::{
    MutationHandle, PathSegment, VM,
    error::RuntimeError,
    evaluate::{
        instruction::{VMEvaluation, resolve_slice_range},
    },
    value::{RuntimeValue, TerminateValue, hashable::HashKey},
};
use calibre_bytecode::{
    VMBlock,
    instructions::access::{VMDiscriminant, VMIndex, VMLoadMember},
};
use calibre_lir::{FullyQualifiedPath, TypeKey, ast::BlockId};
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

#[inline]
fn resolve_numeric_index(index: &RuntimeValue) -> Result<isize, RuntimeError> {
    match index {
        RuntimeValue::Int(idx) => Ok(*idx),
        RuntimeValue::UInt(idx) => Ok(*idx as isize),
        RuntimeValue::Str(s) => {
            s.parse::<isize>()
                .map_err(|_| RuntimeError::ExpectedIntIndexFound {
                    found: Box::new(index.clone()),
                })
        }
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
        let tuple_index = member.parse::<isize>().ok();
        let raw_receiver = vm.get_reg_value(self.value).clone();
        let is_next_or_zero = member == "next" || member == "0";

        let missing = |target: RuntimeValue| RuntimeError::MissingMember {
            target: Box::new(target),
            member: member.to_string(),
        };

        let resolved = vm.resolve_value_ref(&raw_receiver)?;
        let mut member_source: Option<Arc<MutationHandle>> = None;

        if let RuntimeValue::Aggregate(Some(type_name), map) = &resolved {
            if let Some(idx) = vm.resolve_aggregate_member_slot(type_name, map, member) {
                let field_name = &map.0.0[idx].0;
                member_source = Some(vm.new_mutation_handle(
                    source_reg,
                    PathSegment::Field(Ustr::from(field_name)),
                ));
            } else if let Some((_, wrapped)) = map.0.0.iter().find(|(field, _)| field == "0") {
                if let Some(i) = tuple_index {
                    let resolved_idx = normalize_index(map.0.0.len(), i).unwrap_or_default();
                    member_source = Some(
                        vm.new_mutation_handle(source_reg, PathSegment::Index(resolved_idx)),
                    );
                } else {
                    let wrapped_val = vm.resolve_value_ref(wrapped.as_ref())?;
                    if let RuntimeValue::Aggregate(inner_type, inner_map) = wrapped_val {
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

                        if let Some(idx) = vm.resolve_aggregate_member_slot(&inner_type_key, &inner_map, member) {
                            member_source = Some(vm.extend_mutation_handle(
                                source_reg,
                                &[
                                    PathSegment::Field(Ustr::from("0")),
                                    PathSegment::Field(Ustr::from(inner_map.0.0[idx].0.as_str())),
                                ],
                            ));
                        }
                    }
                }
            }
        } else if is_next_or_zero {
            member_source = Some(vm.unwrap_mutation_handle(source_reg));
        }

        let final_source = member_source.unwrap_or_else(|| {
            if let Some(i) = tuple_index {
                let resolved_idx = normalize_index(usize::MAX, i).unwrap_or_default();
                vm.new_mutation_handle(source_reg, PathSegment::Index(resolved_idx))
            } else {
                vm.new_mutation_handle(source_reg, PathSegment::Field(*member))
            }
        });

        vm.set_reg_value(self.dst, RuntimeValue::MutationHandle(final_source));

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
                RuntimeValue::Range(start, end) => {
                    let items = &list.as_ref().0;
                    resolve_slice_range(items.len(), *start, *end)
                        .map(|(s, e)| PathSegment::Slice(s, e))
                }
                other => {
                    resolve_element_index(list.as_ref().0.len(), other)?.map(PathSegment::Index)
                }
            },
            RuntimeValue::Str(s) => match &index_val {
                RuntimeValue::Range(start, end) => {
                    let chars_len = s.chars().count();
                    resolve_slice_range(chars_len, *start, *end)
                        .map(|(s, e)| PathSegment::Slice(s, e))
                }
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

        if let Some(segment) = indexed_segment {
            let handle = vm.new_mutation_handle(self.value, segment);
            vm.set_reg_value(self.dst, RuntimeValue::MutationHandle(handle));
        }

        Ok(TerminateValue::None)
    }
}
