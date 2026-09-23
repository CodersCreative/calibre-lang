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

impl VMEvaluation for VMJump {
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
    fn run(
        &self,
        vm: &mut VM,
        block: &VMBlock,
        ip: u32,
        _prev_block: Option<BlockId>,
    ) -> Result<TerminateValue, RuntimeError> {
        if vm.eval_branch_condition(self.cond, block, ip)? {
            Ok(TerminateValue::Jump(self.then_block))
        } else {
            Ok(TerminateValue::Jump(self.else_block))
        }
    }
}

impl VMEvaluation for VMReturn {
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
