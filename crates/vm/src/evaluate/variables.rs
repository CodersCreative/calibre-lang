use crate::{
    VM,
    conversion::{
        VMBlock,
        instructions::variables::{VMDropVar, VMLoadVar, VMLoadVarRef, VMMoveVar, VMStoreVar},
    },
    error::RuntimeError,
    evaluate::instruction::VMEvaluation,
    value::{RuntimeValue, TerminateValue},
};
use calibre_lir::ast::BlockId;
use tracing::instrument;

impl VMEvaluation for VMDropVar {
    #[instrument(skip_all)]
    fn run(
        &self,
        vm: &mut VM,
        block: &VMBlock,
        _ip: u32,
        _prev_block: Option<BlockId>,
    ) -> Result<TerminateValue, RuntimeError> {
        let name = vm.local_variable(block, self.name)?;

        if let Some(val) = vm.variables.remove(name) {
            vm.drop_runtime_value(val);
        }

        Ok(TerminateValue::None)
    }
}

impl VMEvaluation for VMLoadVar {
    #[instrument(skip_all)]
    fn run(
        &self,
        vm: &mut VM,
        block: &VMBlock,
        _ip: u32,
        _prev_block: Option<BlockId>,
    ) -> Result<TerminateValue, RuntimeError> {
        let name = vm.local_variable(block, self.name)?;

        if let Some(value) = vm.get_value(name) {
            vm.set_reg_value(self.dst, value);
        }

        Ok(TerminateValue::None)
    }
}

impl VMEvaluation for VMMoveVar {
    #[instrument(skip_all)]
    fn run(
        &self,
        vm: &mut VM,
        block: &VMBlock,
        _ip: u32,
        _prev_block: Option<BlockId>,
    ) -> Result<TerminateValue, RuntimeError> {
        let name = vm.local_variable(block, self.name)?;

        if let Some(value) = vm.remove_value(name) {
            vm.set_reg_value(self.dst, value);
        }

        Ok(TerminateValue::None)
    }
}

impl VMEvaluation for VMStoreVar {
    #[instrument(skip_all)]
    fn run(
        &self,
        vm: &mut VM,
        block: &VMBlock,
        _ip: u32,
        _prev_block: Option<BlockId>,
    ) -> Result<TerminateValue, RuntimeError> {
        let name = vm.local_variable(block, self.name)?;
        let stored = vm.get_reg_value(self.src).clone();
        let old = vm.variables.insert(name.clone(), stored);

        if let Some(old) = old
            && let Some(dst) = &self.dst
        {
            vm.set_reg_value(*dst, old);
        }

        Ok(TerminateValue::None)
    }
}

impl VMEvaluation for VMLoadVarRef {
    #[instrument(skip_all)]
    fn run(
        &self,
        vm: &mut VM,
        block: &VMBlock,
        _ip: u32,
        _prev_block: Option<BlockId>,
    ) -> Result<TerminateValue, RuntimeError> {
        let name = vm.local_variable(block, self.name)?;

        if let Some(RuntimeValue::RegRef { frame, reg }) = vm.variables.get(name) {
            vm.set_reg_value(
                self.dst,
                RuntimeValue::RegRef {
                    frame: *frame,
                    reg: *reg,
                },
            );
        } else {
            vm.set_reg_value(self.dst, RuntimeValue::Ref(name.clone()));
        }

        Ok(TerminateValue::None)
    }
}
