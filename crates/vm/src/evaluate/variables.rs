use crate::{
    VM, VarName,
    conversion::{
        VMBlock,
        instructions::variables::{VMDropVar, VMLoadVar, VMLoadVarRef, VMMoveVar, VMStoreVar},
    },
    error::RuntimeError,
    evaluate::instruction::VMEvaluation,
    value::{RuntimeValue, TerminateValue},
};
use calibre_lir::ast::BlockId;

impl VMEvaluation for VMDropVar {
    fn run(
        &self,
        vm: &mut VM,
        block: &VMBlock,
        _ip: u32,
        _prev_block: Option<BlockId>,
    ) -> Result<TerminateValue, RuntimeError> {
        let name = vm.local_string(block, self.name)?;

        if let Some(val) = vm.variables.remove(name) {
            vm.drop_runtime_value(val);
        }

        Ok(TerminateValue::None)
    }
}

impl VMEvaluation for VMLoadVar {
    fn run(
        &self,
        vm: &mut VM,
        block: &VMBlock,
        _ip: u32,
        _prev_block: Option<BlockId>,
    ) -> Result<TerminateValue, RuntimeError> {
        let name = vm.local_string(block, self.name)?;

        if let Some(value) = vm.get_value(name) {
            vm.set_reg_value(self.dst, value);
        }

        Ok(TerminateValue::None)
    }
}

impl VMEvaluation for VMMoveVar {
    fn run(
        &self,
        vm: &mut VM,
        block: &VMBlock,
        _ip: u32,
        _prev_block: Option<BlockId>,
    ) -> Result<TerminateValue, RuntimeError> {
        let name = vm.local_string(block, self.name)?;
        let resolved = vm.resolve_var_name(*name);

        let value = vm.remove_value(name).unwrap_or_else(|| match &resolved {
            Some(VarName::Func(func)) => {
                if let Some(func) = vm.get_function_ref(func) {
                    vm.make_runtime_function(func)
                } else {
                    RuntimeValue::Null
                }
            }
            Some(VarName::Var(var)) => {
                if let Some(var) = vm.variables.remove(var) {
                    vm.resolve_saveable_runtime_value(var)
                } else {
                    RuntimeValue::Null
                }
            }
            _ => RuntimeValue::Null,
        });

        vm.set_reg_value(self.dst, value);
        Ok(TerminateValue::None)
    }
}

impl VMEvaluation for VMStoreVar {
    fn run(
        &self,
        vm: &mut VM,
        block: &VMBlock,
        _ip: u32,
        _prev_block: Option<BlockId>,
    ) -> Result<TerminateValue, RuntimeError> {
        let name = vm.local_string(block, self.name)?;
        let stored = vm.resolve_value_ref(vm.get_reg_value(self.src))?;
        let old = vm.variables.insert(*name, stored);

        if let Some(old) = old
            && let Some(dst) = &self.dst
        {
            vm.set_reg_value(*dst, old);
        }

        Ok(TerminateValue::None)
    }
}

impl VMEvaluation for VMLoadVarRef {
    fn run(
        &self,
        vm: &mut VM,
        block: &VMBlock,
        _ip: u32,
        _prev_block: Option<BlockId>,
    ) -> Result<TerminateValue, RuntimeError> {
        let name = vm.local_string(block, self.name)?;

        if let Some(RuntimeValue::RegRef { frame, reg }) = vm.variables.get(name) {
            vm.set_reg_value(
                self.dst,
                RuntimeValue::RegRef {
                    frame: *frame,
                    reg: *reg,
                },
            );
        } else {
            vm.set_reg_value(self.dst, RuntimeValue::Ref(*name));
        }

        Ok(TerminateValue::None)
    }
}
