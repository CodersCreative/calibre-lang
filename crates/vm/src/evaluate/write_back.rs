use super::super::VM;
use crate::{
    MutationHandle, PathSegment, RootBinding, conversion::Reg, error::RuntimeError,
    value::RuntimeValue,
};
use smallvec::SmallVec;
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
    pub handle: Option<Arc<MutationHandle>>,
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
                .set_shared_mutation_handle(reg, handle);
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
                .and_then(|frame| frame.get_mutation_handle(arg));
            if let Some(handle) = handle {
                self.current_frame_mut()
                    .set_shared_mutation_handle(param, handle);
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

    pub(crate) fn new_mutation_handle(
        &self,
        source: Reg,
        segment: PathSegment,
    ) -> Arc<MutationHandle> {
        let mut handle = self
            .current_frame()
            .get_mutation_handle(source)
            .unwrap_or_else(|| {
                Arc::new(MutationHandle {
                    root: self.get_root_binding(source),
                    path: SmallVec::new(),
                })
            });

        Arc::make_mut(&mut handle).path.push(segment);
        handle
    }

    pub(crate) fn extend_mutation_handle(
        &self,
        source: Reg,
        segments: &[PathSegment],
    ) -> Arc<MutationHandle> {
        let mut handle = self
            .current_frame()
            .get_mutation_handle(source)
            .unwrap_or_else(|| {
                Arc::new(MutationHandle {
                    root: self.get_root_binding(source),
                    path: SmallVec::new(),
                })
            });

        Arc::make_mut(&mut handle)
            .path
            .extend(segments.iter().cloned());
        handle
    }

    pub(crate) fn get_mutation_handle(&self, reg: Reg) -> Option<Arc<MutationHandle>> {
        self.current_frame().get_mutation_handle(reg)
    }

    pub(crate) fn unwrap_mutation_handle(&self, source: Reg) -> Arc<MutationHandle> {
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

    pub(crate) fn mutate_handle<F, R>(&mut self, handle: &MutationHandle, mutation: F) -> Option<R>
    where
        F: FnOnce(&mut RuntimeValue) -> Option<R>,
    {
        let target = self.get_root(&handle.root);
        let mut root = self.resolve_value_ref(&target).ok()?;

        if !root.path_exists(&handle.path) {
            return None;
        }

        let mut mutation = Some(mutation);
        let mut apply = |value: &mut RuntimeValue| {
            let mutation = mutation.take()?;
            mutation(value)
        };
        let result = root.update_path(&handle.path, &mut apply)?;
        let _ = self.write_back(target, root);
        Some(result)
    }

    pub(crate) fn replace_mutation_handle(
        &mut self,
        handle: &MutationHandle,
        replacement: RuntimeValue,
    ) -> Option<RuntimeValue> {
        self.mutate_handle(handle, |value| Some(std::mem::replace(value, replacement)))
    }

    pub(crate) fn read_mutation_handle(&self, handle: &MutationHandle) -> Option<RuntimeValue> {
        let target = self.get_root(&handle.root);
        let root = self.resolve_value_ref(&target).ok()?;
        root.read_path(&handle.path)
    }
}

impl Propagation for VM {
    #[instrument(skip_all)]
    fn propagate_member_source_alias(&mut self, src: Reg, dst: Reg) {
        let source = self.current_frame().get_mutation_handle(src);

        match source {
            Some(source) => {
                self.current_frame_mut()
                    .set_shared_mutation_handle(dst, source);
            }
            None => {
                self.current_frame_mut().remove_mutation_handle(dst);
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
                    self.frames.get(caller_frame)?.get_mutation_handle(*reg)?,
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
            .and_then(|frame| frame.get_mutation_handle(reg))
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
