use crate::{
    VM,
    error::RuntimeError,
    evaluate::instruction::VMEvaluation,
    value::{
        RuntimeValue, TerminateValue, ValueSlot,
        operation::{binary, boolean, comparison},
    },
};
use calibre_bytecode::{
    VMBlock,
    instructions::binary::{VMAs, VMBinary, VMBoolean, VMComparison, VMIs},
};
use calibre_lir::ast::BlockId;
use calibre_parser::ast::{comparison::BooleanOperator, nodes::binary::AsFailureMode};
use tracing::instrument;
use ustr::Ustr;

impl VMEvaluation for VMAs {
    #[instrument(skip_all)]
    fn run(
        &self,
        vm: &mut VM,
        _block: &VMBlock,
        _ip: u32,
        _prev_block: Option<BlockId>,
    ) -> Result<TerminateValue, RuntimeError> {
        let value = vm.get_reg_value(self.src).clone();
        let conversion = value.convert(vm, &self.data_type);
        let converted = match self.failure_mode {
            AsFailureMode::Panic => match conversion {
                Ok(value) => value,
                Err(err) => {
                    return Err(RuntimeError::Panic(Some(format!(
                        "failed `as!` conversion to {}: {}",
                        self.data_type, err
                    ))));
                }
            },
            AsFailureMode::Option => match conversion {
                Ok(value) => RuntimeValue::Option(Some(ValueSlot::new(value))),
                Err(_) => RuntimeValue::Option(None),
            },
            AsFailureMode::Result => match conversion {
                Ok(value) => RuntimeValue::Result(Ok(ValueSlot::new(value))),
                Err(err) => RuntimeValue::Result(Err(ValueSlot::new(RuntimeValue::Str(
                    Ustr::from(&err.to_string()),
                )))),
            },
        };

        vm.set_reg_value(self.dst, converted);
        Ok(TerminateValue::None)
    }
}

impl VMEvaluation for VMIs {
    #[instrument(skip_all)]
    fn run(
        &self,
        vm: &mut VM,
        _block: &VMBlock,
        _ip: u32,
        _prev_block: Option<BlockId>,
    ) -> Result<TerminateValue, RuntimeError> {
        let resolved = vm.resolve_value(vm.get_reg_value(self.src).clone())?;
        let out = vm.runtime_matches_type(&resolved, &self.data_type);

        vm.set_reg_value(self.dst, RuntimeValue::Bool(out));
        Ok(TerminateValue::None)
    }
}

impl VMEvaluation for VMBinary {
    #[instrument(skip_all)]
    fn run(
        &self,
        vm: &mut VM,
        _block: &VMBlock,
        _ip: u32,
        _prev_block: Option<BlockId>,
    ) -> Result<TerminateValue, RuntimeError> {
        let left = vm
            .resolve_value_ref(vm.get_reg_value(self.left))
            .map(RuntimeValue::from)?;
        let right = vm
            .resolve_value_ref(vm.get_reg_value(self.right))
            .map(RuntimeValue::from)?;

        let value = binary(vm, &self.op, left, right)?;
        vm.set_reg_value(self.dst, value);

        Ok(TerminateValue::None)
    }
}

impl VMEvaluation for VMComparison {
    #[instrument(skip_all)]
    fn run(
        &self,
        vm: &mut VM,
        _block: &VMBlock,
        _ip: u32,
        _prev_block: Option<BlockId>,
    ) -> Result<TerminateValue, RuntimeError> {
        let left = vm
            .resolve_value_ref(vm.get_reg_value(self.left))
            .map(RuntimeValue::from)?;
        let right = vm
            .resolve_value_ref(vm.get_reg_value(self.right))
            .map(RuntimeValue::from)?;

        let value = comparison(&self.op, left, right)?;
        vm.set_reg_value(self.dst, value);

        Ok(TerminateValue::None)
    }
}

impl VMEvaluation for VMBoolean {
    #[instrument(skip_all)]
    fn run(
        &self,
        vm: &mut VM,
        _block: &VMBlock,
        _ip: u32,
        _prev_block: Option<BlockId>,
    ) -> Result<TerminateValue, RuntimeError> {
        let left = vm
            .resolve_value_ref(vm.get_reg_value(self.left))
            .map(RuntimeValue::from)?;

        if let RuntimeValue::Bool(x) = &left {
            if BooleanOperator::And == self.op && !*x {
                vm.set_reg_value(self.dst, RuntimeValue::Bool(false));
                return Ok(TerminateValue::None);
            } else if BooleanOperator::Or == self.op && *x {
                vm.set_reg_value(self.dst, RuntimeValue::Bool(true));
                return Ok(TerminateValue::None);
            }
        }

        let right = vm
            .resolve_value_ref(vm.get_reg_value(self.right))
            .map(RuntimeValue::from)?;
        vm.set_reg_value(self.dst, boolean(&self.op, left, right)?);

        Ok(TerminateValue::None)
    }
}
