use crate::{
    VM,
    error::RuntimeError,
    evaluate::{instruction::VMEvaluation, write_back::Propagation},
    value::{RuntimeValue, TerminateValue},
};
use calibre_bytecode::{
    VMBlock,
    instructions::memory::{VMDeref, VMRef, VMSetRef},
};
use calibre_lir::{FullyQualifiedPath, VariableKey, ast::BlockId};
use tracing::instrument;
use ustr::Ustr;

impl VMEvaluation for VMRef {
    #[instrument(skip_all)]
    fn run(
        &self,
        vm: &mut VM,
        _block: &VMBlock,
        _ip: u32,
        _prev_block: Option<BlockId>,
    ) -> Result<TerminateValue, RuntimeError> {
        let out = match vm.get_reg_value(self.value).clone() {
            RuntimeValue::Ref(name) => RuntimeValue::Ref(name),
            RuntimeValue::RegRef { frame, reg } => RuntimeValue::RegRef { frame, reg },
            other => {
                    let name = VariableKey {
                        fully_qualified_path: FullyQualifiedPath::combine(
                            None,
                            Ustr::from(&vm.get_ref_id().to_string()),
                        ),
                        shadow_counter: None,
                    };
                    
                    let _ = vm.variables.insert(name.clone(), other);
                    RuntimeValue::Ref(name)
            }
        };

        vm.set_reg_value(self.dst, out);
        vm.propagate_member_source_alias(self.value, self.dst);
        Ok(TerminateValue::None)
    }
}

impl VMEvaluation for VMDeref {
    #[instrument(skip_all)]
    fn run(
        &self,
        vm: &mut VM,
        _block: &VMBlock,
        _ip: u32,
        _prev_block: Option<BlockId>,
    ) -> Result<TerminateValue, RuntimeError> {
        let out = vm.resolve_value_ref(vm.get_reg_value(self.value))?;
        vm.set_reg_value(self.dst, out);
        Ok(TerminateValue::None)
    }
}

impl VMEvaluation for VMSetRef {
    #[instrument(skip_all)]
    fn run(
        &self,
        vm: &mut VM,
        _block: &VMBlock,
        _ip: u32,
        _prev_block: Option<BlockId>,
    ) -> Result<TerminateValue, RuntimeError> {
        let target = vm.get_reg_value(self.target).clone();
        let value = vm.get_reg_value(self.value).clone();

        match target {
            RuntimeValue::Ref(name) => {
                if let Some(old) = vm.variables.insert(name, value) {
                    let _ = vm.set_reg_value(self.dst, old);
                }
            }
            RuntimeValue::RegRef { frame, reg } => {
                let old = vm.set_reg_value_in_frame(frame, reg, value);
                let _ = vm.set_reg_value(self.dst, old);
            }
            RuntimeValue::MutexGuard(guard) => {
                let old = guard.set_value(value);
                let _ = vm.set_reg_value(self.dst, old);
            }
            x => return Err(RuntimeError::InvalidBytecode(format!("invalid ref {}", x))),
        }

        Ok(TerminateValue::None)
    }
}
