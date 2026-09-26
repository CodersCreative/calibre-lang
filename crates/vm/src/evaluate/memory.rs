use crate::{
    VM,
    conversion::{
        VMBlock,
        instructions::memory::{VMDeref, VMRef, VMSetRef},
    },
    error::RuntimeError,
    evaluate::{instruction::VMEvaluation, write_back::Propagation},
    value::{RuntimeValue, TerminateValue},
};
use calibre_lir::ast::BlockId;
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
            RuntimeValue::VarRef(id) => RuntimeValue::VarRef(id),
            RuntimeValue::RegRef { frame, reg } => RuntimeValue::RegRef { frame, reg },
            other => {
                if let Some(id) = (0..vm.variables.slot_len()).find(|id| {
                    matches!(
                        vm.variables.get_by_id(*id),
                        Some(RuntimeValue::RegRef { frame, reg })
                            if *frame == vm.frames.len().saturating_sub(1) && *reg == self.value
                    )
                }) {
                    RuntimeValue::VarRef(id)
                } else if let RuntimeValue::List(list) = &other
                    && let Some(id) = (0..vm.variables.slot_len()).find(|id| {
                        matches!(
                            vm.variables.get_by_id(*id),
                            Some(RuntimeValue::List(other_list))
                                if std::ptr::eq(list.as_ref(), other_list.as_ref())
                        )
                    })
                {
                    RuntimeValue::VarRef(id)
                } else {
                    let name = Ustr::from(&vm.get_ref_id().to_string());
                    let id = vm.variables.insert_with_id(name, other);
                    RuntimeValue::VarRef(id)
                }
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
            RuntimeValue::VarRef(id) => {
                if let Some(old) = vm.variables.set_by_id(id, value) {
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
            _ => return Err(RuntimeError::InvalidBytecode("invalid ref".to_string())),
        }

        Ok(TerminateValue::None)
    }
}
