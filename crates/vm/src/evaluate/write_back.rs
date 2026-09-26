use super::super::VM;
use crate::{
    conversion::Reg,
    error::RuntimeError,
    value::{GcVec, RuntimeValue},
};
use dumpster::sync::Gc;
use tracing::{instrument, trace};

pub(crate) trait WriteBack {
    fn write_back(&mut self, target: RuntimeValue, value: RuntimeValue) -> Option<RuntimeValue>;
}

pub(crate) trait Propagation {
    fn propagate_list_aliases(&mut self, old_list: &Gc<GcVec>, new_list: &Gc<GcVec>);

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

    fn write_back_member_field_update(
        &mut self,
        frame_idx: usize,
        field_reg: Reg,
        parent_reg: Reg,
        field_name: &str,
    ) -> Result<Option<RuntimeValue>, RuntimeError>;
}

impl Propagation for VM {
    #[instrument(skip_all)]
    fn propagate_list_aliases(&mut self, old_list: &Gc<GcVec>, new_list: &Gc<GcVec>) {
        let frame_count = self.frames.len();
        for frame_idx in 0..frame_count {
            let reg_count = self.frames[frame_idx].reg_count as u16;
            for reg in 0..reg_count {
                let Some(value) = self.get_reg_value_in_frame_mut(frame_idx, reg) else {
                    continue;
                };

                if !value.might_contain_list() {
                    continue;
                }

                value.replace_list_aliases(old_list, new_list);
            }
        }

        let slot_len = self.variables.slot_len();
        for id in 0..slot_len {
            let Some(current) = self.variables.get_mut_by_id(id) else {
                continue;
            };

            if !current.might_contain_list() {
                continue;
            }

            current.replace_list_aliases(old_list, new_list);
        }
    }

    #[instrument(skip_all)]
    fn propagate_member_source_alias(&mut self, src: Reg, dst: Reg) {
        let source = self.current_frame().member_sources.get(&src).cloned();
        match source {
            Some(source) => {
                self.current_frame_mut().member_sources.insert(dst, source);
            }
            None => {
                self.current_frame_mut().member_sources.remove(&dst);
            }
        }
    }

    #[instrument(skip_all)]
    fn propagate_member_source_args(
        &mut self,
        args: &[Reg],
        caller_frame: usize,
    ) -> Result<(), RuntimeError> {
        let args = args
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

                let (parent, field) = self
                    .frames
                    .get(caller_frame)?
                    .member_sources
                    .get(reg)
                    .cloned()?;

                Some((caller_frame, *reg, parent, field))
            })
            .collect::<Vec<_>>();

        for (frame, field, parent, name) in args {
            self.write_back_member_field_update(frame, field, parent, &name)?;
        }

        Ok(())
    }

    #[instrument(skip_all)]
    fn propagate_member_source_reg(
        &mut self,
        reg: Reg,
        frame_idx: usize,
    ) -> Result<Option<RuntimeValue>, RuntimeError> {
        let Some((parent, field)) = self.recover_member_source_for_list(reg, frame_idx) else {
            return Ok(None);
        };

        self.write_back_member_field_update(frame_idx, reg, parent, &field)
    }

    #[instrument(skip_all)]
    fn write_back_member_field_update(
        &mut self,
        frame_idx: usize,
        field_reg: Reg,
        parent_reg: Reg,
        field_name: &str,
    ) -> Result<Option<RuntimeValue>, RuntimeError> {
        let field = self.get_reg_value_in_frame(frame_idx, field_reg).clone();

        let parent_raw = self.get_reg_value_in_frame(frame_idx, parent_reg).clone();
        let parent = self.resolve_value_ref(&parent_raw)?;

        fn update(
            value: RuntimeValue,
            field: &str,
            replacement: &RuntimeValue,
        ) -> Option<RuntimeValue> {
            match value {
                RuntimeValue::Aggregate(name, mut map) => {
                    let entries = &mut dumpster::sync::Gc::make_mut(&mut map).0.0;

                    if let Some(entry) = entries.iter_mut().find(|(name, _)| name == field) {
                        entry.1 = replacement.clone();
                        return Some(RuntimeValue::Aggregate(name, map));
                    }

                    let (_, wrapped) = entries.iter_mut().find(|(name, _)| name == "0")?;
                    *wrapped = update(wrapped.clone(), field, replacement)?;
                    Some(RuntimeValue::Aggregate(name, map))
                }
                RuntimeValue::List(mut list) => {
                    let values = &mut dumpster::sync::Gc::make_mut(&mut list).0;
                    let mut changed = false;

                    for item in values.iter_mut() {
                        if let Some(nested) = update(item.clone(), field, replacement) {
                            *item = nested;
                            changed = true;
                        }
                    }

                    changed.then_some(RuntimeValue::List(list))
                }
                _ => None,
            }
        }

        let leaf = field_name
            .rsplit('.')
            .next()
            .unwrap_or(field_name)
            .rsplit_once(']')
            .map(|(_, name)| name)
            .unwrap_or(field_name);

        if let Some(updated) = update(parent, leaf, &field) {
            let source = self
                .frames
                .get(frame_idx)
                .and_then(|frame| frame.member_sources.get(&parent_reg))
                .cloned();

            match parent_raw {
                RuntimeValue::Ref(_) | RuntimeValue::VarRef(_) | RuntimeValue::RegRef { .. } => {
                    Ok(self.write_back(parent_raw, updated))
                }
                _ => {
                    let old = self.set_reg_value_in_frame(frame_idx, parent_reg, updated);
                    if let Some(source) = source {
                        self.frames
                            .get_mut(frame_idx)
                            .unwrap()
                            .member_sources
                            .insert(parent_reg, source);
                        self.propagate_member_source_reg(parent_reg, frame_idx)
                    } else {
                        Ok(Some(old))
                    }
                }
            }
        } else {
            Ok(None)
        }
    }
}

impl WriteBack for VM {
    #[instrument(skip_all)]
    fn write_back(&mut self, target: RuntimeValue, value: RuntimeValue) -> Option<RuntimeValue> {
        self.write_back_at_depth(target, value, 32)
    }
}

impl VM {
    fn recover_member_source_for_list(
        &self,
        reg: Reg,
        frame_idx: usize,
    ) -> Option<(Reg, ustr::Ustr)> {
        if let Some(source) = self
            .frames
            .get(frame_idx)?
            .member_sources
            .get(&reg)
            .cloned()
        {
            return Some(source);
        }

        let RuntimeValue::List(target) = self.get_reg_value_in_frame(frame_idx, reg) else {
            return None;
        };

        self.frames
            .get(frame_idx)?
            .member_sources
            .iter()
            .find_map(|(candidate, source)| {
                if *candidate == reg {
                    return None;
                }

                if let RuntimeValue::List(other) =
                    self.get_reg_value_in_frame(frame_idx, *candidate)
                    && std::ptr::eq(other.as_ref(), target.as_ref())
                {
                    Some(*source)
                } else {
                    None
                }
            })
    }

    fn write_back_at_depth(
        &mut self,
        target: RuntimeValue,
        value: RuntimeValue,
        depth: usize,
    ) -> Option<RuntimeValue> {
        if depth == 0 {
            trace!("write-back alias depth exceeded; forcing the write");
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
