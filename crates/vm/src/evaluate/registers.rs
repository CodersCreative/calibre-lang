use crate::{
    VM,
    error::RuntimeError,
    evaluate::instruction::VMEvaluation,
    value::{RuntimeValue, TerminateValue},
};
use calibre_bytecode::{
    VMBlock,
    instructions::registers::{VMCopy, VMLoadRegRef},
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
            RuntimeValue::Ref(name) => RuntimeValue::Ref(name.clone()),
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
        self.run_inner(vm);
        Ok(TerminateValue::None)
    }

    fn run_inner(&self, vm: &mut VM) {
        if self.src == self.dst {
            return;
        }

        let value = vm.get_reg_value(self.src).clone();
        vm.set_reg_value(self.dst, value);
    }
}
