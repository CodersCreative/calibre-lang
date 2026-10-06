use crate::{VM, error::RuntimeError, value::TerminateValue};
use calibre_bytecode::{VMBlock, instructions::VMInstruction};
use calibre_lir::ast::BlockId;

pub trait VMEvaluation {
    fn run(
        &self,
        vm: &mut VM,
        block: &VMBlock,
        ip: u32,
        prev_block: Option<BlockId>,
    ) -> Result<TerminateValue, RuntimeError>;

    fn run_inner(&self, _vm: &mut VM) {
        unimplemented!()
    }
}

impl VMEvaluation for VMInstruction {
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
            VMInstruction::Discriminant(x) => x.run(vm, block, ip, prev_block),

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

#[inline]
pub(crate) fn resolve_index(len: usize, idx: i64) -> Result<usize, RuntimeError> {
    if len == 0 {
        return Err(RuntimeError::StackUnderflow);
    }

    let resolved = if idx < 0 { len as i64 + idx } else { idx };

    if resolved < 0 || resolved as usize >= len {
        Err(RuntimeError::StackUnderflow)
    } else {
        Ok(resolved as usize)
    }
}

#[inline]
pub(crate) fn resolve_slice_range(len: usize, start: i64, end: i64) -> Option<(usize, usize)> {
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

    if s > len as i64 || e > len as i64 {
        return None;
    }

    Some(if e < s {
        (e as usize, s as usize)
    } else {
        (s as usize, e as usize)
    })
}
