use super::write_back::Propagation;
use super::*;
use crate::evaluate::calling::CallSite;

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
    pub(crate) fn resolve_index(len: usize, idx: i64) -> Option<usize> {
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
    pub(crate) fn resolve_index_or_err(len: usize, idx: i64) -> Result<usize, RuntimeError> {
        Self::resolve_index(len, idx).ok_or(RuntimeError::StackUnderflow)
    }

    #[inline]
    pub(crate) fn resolve_slice_range(len: usize, start: i64, end: i64) -> (usize, usize) {
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
            VMInstruction::Range(x) => x.run(self, block, ip, prev_block),
            VMInstruction::List(x) => x.run(self, block, ip, prev_block),
            VMInstruction::Aggregate(x) => x.run(self, block, ip, prev_block),
            VMInstruction::Enum(x) => x.run(self, block, ip, prev_block),

            // Variables
            VMInstruction::LoadVar(x) => x.run(self, block, ip, prev_block),
            VMInstruction::MoveVar(x) => x.run(self, block, ip, prev_block),
            VMInstruction::DropVar(x) => x.run(self, block, ip, prev_block),
            VMInstruction::StoreVar(x) => x.run(self, block, ip, prev_block),
            VMInstruction::LoadVarRef(x) => x.run(self, block, ip, prev_block),

            // Registers
            VMInstruction::LoadRegRef(x) => x.run(self, block, ip, prev_block),
            VMInstruction::Copy(x) => x.run(self, block, ip, prev_block),

            // Binary
            VMInstruction::As(x) => x.run(self, block, ip, prev_block),
            VMInstruction::Is(x) => x.run(self, block, ip, prev_block),
            VMInstruction::Binary(x) => x.run(self, block, ip, prev_block),
            VMInstruction::Comparison(x) => x.run(self, block, ip, prev_block),
            VMInstruction::Boolean(x) => x.run(self, block, ip, prev_block),

            // Functions
            VMInstruction::CallSelf(x) => x.run(self, block, ip, prev_block),
            VMInstruction::Call(x) => x.run(self, block, ip, prev_block),
            VMInstruction::Spawn(x) => x.run(self, block, ip, prev_block),

            // Access
            VMInstruction::LoadMember(x) => x.run(self, block, ip, prev_block),
            VMInstruction::SetMember(x) => x.run(self, block, ip, prev_block),
            VMInstruction::Index(x) => x.run(self, block, ip, prev_block),
            VMInstruction::SetIndex(x) => x.run(self, block, ip, prev_block),

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
