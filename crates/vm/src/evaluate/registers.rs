use crate::{
    VM,
    conversion::{
        VMBlock,
        instructions::registers::{VMCopy, VMLoadRegRef},
    },
    error::RuntimeError,
    evaluate::{instruction::VMEvaluation, write_back::Propagation},
    value::{RuntimeValue, TerminateValue},
};
use calibre_lir::ast::BlockId;
use tracing::instrument;

impl VMEvaluation for VMLoadRegRef {
    #[instrument(skip_all)]
    fn run(
        &self,
        vm: &mut VM,
        _block: &VMBlock,
        _ip: u32,
        _prev_block: Option<BlockId>,
    ) -> Result<TerminateValue, RuntimeError> {
        let value = match vm.get_reg_value(self.src) {
            RuntimeValue::RegRef { frame, reg } => RuntimeValue::RegRef {
                frame: *frame,
                reg: *reg,
            },
            RuntimeValue::Ref(name) => RuntimeValue::Ref(*name),
            RuntimeValue::VarRef(id) => RuntimeValue::VarRef(*id),
            other => other.clone(),
        };

        vm.set_reg_value(self.dst, value);
        Ok(TerminateValue::None)
    }
}

impl VMEvaluation for VMCopy {
    #[instrument(skip_all)]
    fn run(
        &self,
        vm: &mut VM,
        _block: &VMBlock,
        _ip: u32,
        _prev_block: Option<BlockId>,
    ) -> Result<TerminateValue, RuntimeError> {
        if self.dst == self.src {
            return Ok(TerminateValue::None);
        }

        let value = vm.get_reg_value(self.src).clone();
        vm.set_reg_value(self.dst, value);
        vm.propagate_member_source_alias(self.src, self.dst);

        Ok(TerminateValue::None)
    }
}
