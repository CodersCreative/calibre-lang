use crate::{
    VM,
    conversion::{VMBlock, instructions::VMInstruction},
    error::RuntimeError,
    evaluate::calling::CallSite,
    value::{RuntimeValue, TerminateValue},
};
use calibre_lir::ast::BlockId;
use tracing::instrument;

pub trait VMEvaluation {
    fn run(
        &self,
        vm: &mut VM,
        block: &VMBlock,
        ip: u32,
        prev_block: Option<BlockId>,
    ) -> Result<TerminateValue, RuntimeError>;
}

impl VMEvaluation for VMInstruction {
    #[instrument(skip_all)]
    fn run(
        &self,
        vm: &mut VM,
        block: &VMBlock,
        ip: u32,
        prev_block: Option<BlockId>,
    ) -> Result<TerminateValue, RuntimeError> {
        match self {
            VMInstruction::Noop => Ok(TerminateValue::None),

            // Literals
            VMInstruction::LoadLiteral(x) => x.run(vm, block, ip, prev_block),
            VMInstruction::Range(x) => x.run(vm, block, ip, prev_block),
            VMInstruction::List(x) => x.run(vm, block, ip, prev_block),
            VMInstruction::Aggregate(x) => x.run(vm, block, ip, prev_block),
            VMInstruction::Enum(x) => x.run(vm, block, ip, prev_block),

            // Variables
            VMInstruction::LoadVar(x) => x.run(vm, block, ip, prev_block),
            VMInstruction::MoveVar(x) => x.run(vm, block, ip, prev_block),
            VMInstruction::DropVar(x) => x.run(vm, block, ip, prev_block),
            VMInstruction::StoreVar(x) => x.run(vm, block, ip, prev_block),
            VMInstruction::LoadVarRef(x) => x.run(vm, block, ip, prev_block),

            // Registers
            VMInstruction::LoadRegRef(x) => x.run(vm, block, ip, prev_block),
            VMInstruction::Copy(x) => x.run(vm, block, ip, prev_block),

            // Binary
            VMInstruction::As(x) => x.run(vm, block, ip, prev_block),
            VMInstruction::Is(x) => x.run(vm, block, ip, prev_block),
            VMInstruction::Binary(x) => x.run(vm, block, ip, prev_block),
            VMInstruction::Comparison(x) => x.run(vm, block, ip, prev_block),
            VMInstruction::Boolean(x) => x.run(vm, block, ip, prev_block),

            // Functions
            VMInstruction::CallSelf(x) => x.run(vm, block, ip, prev_block),
            VMInstruction::Call(x) => x.run(vm, block, ip, prev_block),
            VMInstruction::Spawn(x) => x.run(vm, block, ip, prev_block),

            // Access
            VMInstruction::LoadMember(x) => x.run(vm, block, ip, prev_block),
            VMInstruction::SetMember(x) => x.run(vm, block, ip, prev_block),
            VMInstruction::Index(x) => x.run(vm, block, ip, prev_block),
            VMInstruction::SetIndex(x) => x.run(vm, block, ip, prev_block),

            // Memory
            VMInstruction::Ref(x) => x.run(vm, block, ip, prev_block),
            VMInstruction::Deref(x) => x.run(vm, block, ip, prev_block),
            VMInstruction::SetRef(x) => x.run(vm, block, ip, prev_block),

            // Termination
            VMInstruction::Jump(x) => x.run(vm, block, ip, prev_block),
            VMInstruction::Branch(x) => x.run(vm, block, ip, prev_block),
            VMInstruction::Return(x) => x.run(vm, block, ip, prev_block),
        }
    }
}

impl VM {
    pub(crate) fn eval_branch_condition(
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
}
