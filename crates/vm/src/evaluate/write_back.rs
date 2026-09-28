use super::super::VM;
use crate::{
    MutationHandle, PathSegment, RootBinding,
    conversion::Reg,
    error::RuntimeError,
    value::{RuntimeValue, ValueSlot},
};
use dumpster::sync::Gc;
use std::sync::Arc;
use tracing::{instrument, trace};

pub(crate) trait WriteBack {
    fn write_back(&mut self, target: RuntimeValue, value: RuntimeValue) -> Option<RuntimeValue>;
}

pub(crate) trait Propagation {
    fn propagate_member_source_alias(&mut self, src: Reg, dst: Reg);

    fn propagate_member_source_args(
        &mut self,
        args: &[Reg],
        caller_frame: usize,
    ) -> Result<(), RuntimeError>;

    fn propagate_member_source_reg(
        &mut self,
        reg: Reg,
        frame_idx: usize,
    ) -> Result<Option<RuntimeValue>, RuntimeError>;
}

#[derive(Debug, Clone)]
pub(crate) struct PropagatedValue {
    pub value: RuntimeValue,
    pub handle: Option<MutationHandle>,
}

impl VM {
    pub(crate) fn get_propagated_value(&self, reg: Reg) -> PropagatedValue {
        PropagatedValue {
            value: self.get_reg_value(reg).clone(),
            handle: self.get_mutation_handle(reg),
        }
    }

    pub(crate) fn set_propagated_value(
        &mut self,
        reg: Reg,
        propagated: PropagatedValue,
    ) -> RuntimeValue {
        let old = self.set_reg_value(reg, propagated.value);

        if let Some(handle) = propagated.handle {
            self.current_frame_mut()
                .mutation_handles
                .insert(reg, handle);
        }

        old
    }

    pub(crate) fn propagate_member_source_args_into(
        &mut self,
        args: &[Reg],
        caller_frame: usize,
        params: &[Reg],
    ) {
        if self
            .frames
            .get(caller_frame)
            .is_none_or(|frame| frame.mutation_handles.is_empty())
        {
            return;
        }

        for (arg, param) in args.iter().copied().zip(params.iter().copied()) {
            let handle = self
                .frames
                .get(caller_frame)
                .and_then(|frame| frame.mutation_handles.get(&arg))
                .cloned();
            if let Some(handle) = handle {
                self.current_frame_mut()
                    .mutation_handles
                    .insert(param, handle);
            }
        }
    }

    fn get_root_binding(&self, source: Reg) -> RootBinding {
        match self.get_reg_value(source) {
            RuntimeValue::Ref(name) => RootBinding::Ref(*name),
            RuntimeValue::VarRef(id) => RootBinding::VarRef(*id),
            RuntimeValue::RegRef { frame, reg } => RootBinding::RegRef {
                frame: *frame,
                reg: *reg,
            },
            _ => RootBinding::FrameReg {
                frame: self.frames.len().saturating_sub(1),
                reg: source,
            },
        }
    }

    pub(crate) fn new_mutation_handle(&self, source: Reg, segment: PathSegment) -> MutationHandle {
        let mut handle = self
            .current_frame()
            .mutation_handles
            .get(&source)
            .cloned()
            .unwrap_or_else(|| MutationHandle {
                root: self.get_root_binding(source),
                path: Vec::new(),
            });

        handle.path.push(segment);
        handle
    }

    pub(crate) fn extend_mutation_handle(
        &self,
        source: Reg,
        segments: &[PathSegment],
    ) -> MutationHandle {
        let mut handle = self
            .current_frame()
            .mutation_handles
            .get(&source)
            .cloned()
            .unwrap_or_else(|| MutationHandle {
                root: self.get_root_binding(source),
                path: Vec::new(),
            });

        handle.path.extend_from_slice(segments);
        handle
    }

    pub(crate) fn get_mutation_handle(&self, reg: Reg) -> Option<MutationHandle> {
        self.current_frame().mutation_handles.get(&reg).cloned()
    }

    pub(crate) fn unwrap_mutation_handle(&self, source: Reg) -> MutationHandle {
        self.get_mutation_handle(source)
            .unwrap_or_else(|| self.new_mutation_handle(source, PathSegment::Payload))
    }

    fn get_root(&self, root: &RootBinding) -> RuntimeValue {
        match root {
            RootBinding::FrameReg { frame, reg } | RootBinding::RegRef { frame, reg } => {
                RuntimeValue::RegRef {
                    frame: *frame,
                    reg: *reg,
                }
            }
            RootBinding::Ref(name) => RuntimeValue::Ref(*name),
            RootBinding::VarRef(id) => RuntimeValue::VarRef(*id),
        }
    }

    fn replace_path(
        value: &mut RuntimeValue,
        path: &[PathSegment],
        replacement: RuntimeValue,
    ) -> Option<RuntimeValue> {
        let Some((segment, rest)) = path.split_first() else {
            return Some(std::mem::replace(value, replacement));
        };

        match segment {
            PathSegment::Field(field) => match value {
                RuntimeValue::Aggregate(_, map) => {
                    let entries = &mut Arc::make_mut(map);
                    let slot = entries
                        .iter_mut()
                        .find(|(name, _)| name == field)
                        .map(|(_, slot)| slot)?;
                    Self::replace_slot(slot, rest, replacement)
                }
                _ => None,
            },
            PathSegment::Index(index) => match value {
                RuntimeValue::List(list) => {
                    let values = Arc::make_mut(list);
                    Self::replace_slot(values.get_mut(*index)?, rest, replacement)
                }
                RuntimeValue::Aggregate(_, map) => {
                    let entries = &mut Arc::make_mut(map);
                    Self::replace_slot(&mut entries.get_mut(*index)?.1, rest, replacement)
                }
                _ => None,
            },
            PathSegment::MapKey(key) => match value {
                RuntimeValue::HashMap(map) => {
                    let slot = Arc::make_mut(&mut map.map).get_mut(key)?;
                    Self::replace_slot(slot, rest, replacement)
                }
                _ => None,
            },
            PathSegment::Payload => match value {
                RuntimeValue::Option(Some(inner)) => Self::replace_gc(inner, rest, replacement),
                RuntimeValue::Result(Ok(inner)) | RuntimeValue::Result(Err(inner)) => {
                    Self::replace_gc(inner, rest, replacement)
                }
                RuntimeValue::Enum(_, _, Some(inner)) => Self::replace_gc(inner, rest, replacement),
                RuntimeValue::DynObject { value: inner, .. } => {
                    Self::replace_gc(inner, rest, replacement)
                }
                _ => None,
            },
        }
    }

    fn read_path(value: &RuntimeValue, path: &[PathSegment]) -> Option<RuntimeValue> {
        let Some((segment, rest)) = path.split_first() else {
            return Some(value.clone());
        };
        match segment {
            PathSegment::Field(field) => match value {
                RuntimeValue::Aggregate(_, map) => map
                    .as_ref()
                    .0
                    .0
                    .iter()
                    .find(|(name, _)| name == field)
                    .and_then(|(_, slot)| Self::read_path(slot.as_ref(), rest)),
                _ => None,
            },
            PathSegment::Index(index) => match value {
                RuntimeValue::List(list) => list
                    .as_ref()
                    .0
                    .get(*index)
                    .and_then(|slot| Self::read_path(slot.as_ref(), rest)),
                RuntimeValue::Aggregate(_, map) => map
                    .as_ref()
                    .0
                    .0
                    .get(*index)
                    .and_then(|(_, slot)| Self::read_path(slot.as_ref(), rest)),
                _ => None,
            },
            PathSegment::MapKey(key) => match value {
                RuntimeValue::HashMap(map) => map
                    .map
                    .get(key)
                    .and_then(|slot| Self::read_path(slot.as_ref(), rest)),
                _ => None,
            },
            PathSegment::Payload => match value {
                RuntimeValue::Option(Some(inner)) => Self::read_path(inner.as_ref(), rest),
                RuntimeValue::Result(Ok(inner)) | RuntimeValue::Result(Err(inner)) => {
                    Self::read_path(inner.as_ref(), rest)
                }
                RuntimeValue::Enum(_, _, Some(inner)) => Self::read_path(inner.as_ref(), rest),
                RuntimeValue::DynObject { value: inner, .. } => {
                    Self::read_path(inner.as_ref(), rest)
                }
                _ => None,
            },
        }
    }

    fn replace_gc(
        inner: &mut Gc<RuntimeValue>,
        path: &[PathSegment],
        replacement: RuntimeValue,
    ) -> Option<RuntimeValue> {
        Self::replace_path(Gc::make_mut(inner), path, replacement)
    }

    fn replace_slot(
        slot: &mut ValueSlot,
        path: &[PathSegment],
        replacement: RuntimeValue,
    ) -> Option<RuntimeValue> {
        Self::replace_path(slot.make_mut(), path, replacement)
    }

    pub(crate) fn replace_mutation_handle(
        &mut self,
        handle: &MutationHandle,
        replacement: RuntimeValue,
    ) -> Option<RuntimeValue> {
        let target = self.get_root(&handle.root);
        let mut root = self.resolve_value_ref(&target).ok()?;

        let old = Self::replace_path(&mut root, &handle.path, replacement)?;
        let _ = self.write_back(target, root);
        Some(old)
    }

    pub(crate) fn read_mutation_handle(&self, handle: &MutationHandle) -> Option<RuntimeValue> {
        let target = self.get_root(&handle.root);
        let root = self.resolve_value_ref(&target).ok()?;
        Self::read_path(&root, &handle.path)
    }
}

impl Propagation for VM {
    #[instrument(skip_all)]
    fn propagate_member_source_alias(&mut self, src: Reg, dst: Reg) {
        let source = self.current_frame().mutation_handles.get(&src).cloned();

        match source {
            Some(source) => {
                self.current_frame_mut()
                    .mutation_handles
                    .insert(dst, source);
            }
            None => {
                self.current_frame_mut().mutation_handles.remove(&dst);
            }
        }
    }

    #[instrument(skip_all)]
    fn propagate_member_source_args(
        &mut self,
        args: &[Reg],
        caller_frame: usize,
    ) -> Result<(), RuntimeError> {
        let handles = args
            .iter()
            .filter_map(|arg| {
                let RuntimeValue::RegRef { frame, reg } =
                    self.get_reg_value_in_frame(caller_frame, *arg)
                else {
                    return None;
                };

                if *frame != caller_frame {
                    return None;
                }

                Some((
                    self.frames
                        .get(caller_frame)?
                        .mutation_handles
                        .get(reg)
                        .cloned()?,
                    *frame,
                    *reg,
                ))
            })
            .collect::<Vec<_>>();

        for (handle, frame, reg) in handles {
            let field = self.get_reg_value_in_frame(frame, reg).clone();
            let _ = self.replace_mutation_handle(&handle, field);
        }
        Ok(())
    }

    #[instrument(skip_all)]
    fn propagate_member_source_reg(
        &mut self,
        reg: Reg,
        frame_idx: usize,
    ) -> Result<Option<RuntimeValue>, RuntimeError> {
        let Some(handle) = self
            .frames
            .get(frame_idx)
            .and_then(|frame| frame.mutation_handles.get(&reg))
            .cloned()
        else {
            return Ok(None);
        };

        let field = self.get_reg_value_in_frame(frame_idx, reg).clone();
        Ok(self.replace_mutation_handle(&handle, field))
    }
}

impl WriteBack for VM {
    #[instrument(skip_all)]
    fn write_back(&mut self, target: RuntimeValue, value: RuntimeValue) -> Option<RuntimeValue> {
        self.write_back_at_depth(target, value, 32)
    }
}

impl VM {
    fn write_back_at_depth(
        &mut self,
        target: RuntimeValue,
        value: RuntimeValue,
        depth: usize,
    ) -> Option<RuntimeValue> {
        if depth == 0 {
            trace!("write-back reference depth exceeded; forcing the write");
            return self.force_write_back(target, value);
        }
        match target {
            RuntimeValue::Ref(name) => match self.variables.get(&name).cloned() {
                Some(current) if current.is_ref_like() => {
                    self.write_back_at_depth(current, value, depth - 1)
                }
                _ => self.variables.insert(name, value),
            },
            RuntimeValue::VarRef(id) => match self.variables.get_by_id(id).cloned() {
                Some(current) if current.is_ref_like() => {
                    self.write_back_at_depth(current, value, depth - 1)
                }
                _ => self.variables.set_by_id(id, value),
            },
            RuntimeValue::RegRef { frame, reg } => {
                let current = self.get_reg_value_in_frame(frame, reg).clone();

                if current.is_ref_like() {
                    self.write_back_at_depth(current, value, depth - 1)
                } else {
                    Some(self.set_reg_value_in_frame(frame, reg, value))
                }
            }
            RuntimeValue::MutexGuard(x) => Some(x.set_value(value)),
            _ => None,
        }
    }

    fn force_write_back(
        &mut self,
        target: RuntimeValue,
        value: RuntimeValue,
    ) -> Option<RuntimeValue> {
        match target {
            RuntimeValue::Ref(name) => self.variables.insert(name, value),
            RuntimeValue::VarRef(id) => self.variables.set_by_id(id, value),
            RuntimeValue::RegRef { frame, reg } => {
                Some(self.set_reg_value_in_frame(frame, reg, value))
            }
            _ => None,
        }
    }
}
