use crate::{
    VM,
    conversion::{
        VMBlock,
        instructions::termination::{VMBranch, VMJump, VMReturn},
    },
    error::RuntimeError,
    evaluate::instruction::VMEvaluation,
    value::{RuntimeValue, TerminateValue},
};
use calibre_lir::ast::BlockId;
use tracing::instrument;

impl VMEvaluation for VMJump {
    #[instrument(skip_all)]
    fn run(
        &self,
        _vm: &mut VM,
        _block: &VMBlock,
        _ip: u32,
        _prev_block: Option<BlockId>,
    ) -> Result<TerminateValue, RuntimeError> {
        Ok(TerminateValue::Jump(self.target))
    }
}

impl VMEvaluation for VMBranch {
    #[instrument(skip_all)]
    fn run(
        &self,
        vm: &mut VM,
        _block: &VMBlock,
        _ip: u32,
        _prev_block: Option<BlockId>,
    ) -> Result<TerminateValue, RuntimeError> {
        let is_true = match vm.get_reg_value(self.cond).clone() {
            RuntimeValue::Bool(x) => x,
            other => {
                return Err(RuntimeError::ExpectedBoolFound {
                    found: Box::new(other),
                });
            }
        };

        if is_true {
            Ok(TerminateValue::Jump(self.then_block))
        } else {
            Ok(TerminateValue::Jump(self.else_block))
        }
    }
}

impl VMEvaluation for VMReturn {
    #[instrument(skip_all)]
    fn run(
        &self,
        vm: &mut VM,
        _block: &VMBlock,
        _ip: u32,
        _prev_block: Option<BlockId>,
    ) -> Result<TerminateValue, RuntimeError> {
        if let Some(reg) = &self.value {
            Ok(TerminateValue::Return(vm.get_reg_value(*reg).clone()))
        } else {
            Ok(TerminateValue::Return(RuntimeValue::Null))
        }
    }
}
