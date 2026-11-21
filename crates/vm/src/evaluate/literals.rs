use crate::{
    VM,
    error::RuntimeError,
    evaluate::instruction::VMEvaluation,
    value::{GcMap, GcVec, RuntimeValue, TerminateValue, ValueSlot},
};
use calibre_bytecode::{
    VMBlock, VMLiteral,
    instructions::literals::{VMAggregate, VMEnum, VMList, VMLoadLiteral, VMRange},
};
use calibre_lir::ast::BlockId;
use calibre_parser::ast::ObjectMap;
use rustc_hash::FxHashSet;
use std::sync::Arc;
use tracing::instrument;

impl VMEvaluation for VMLoadLiteral {
    #[instrument(skip_all)]
    fn run(
        &self,
        vm: &mut VM,
        block: &VMBlock,
        _ip: u32,
        _prev_block: Option<BlockId>,
    ) -> Result<TerminateValue, RuntimeError> {
        let lit = block
            .local_literals
            .get(self.literal as usize)
            .cloned()
            .ok_or_else(|| RuntimeError::InvalidBytecode("missing literal".to_string()))?;

        match lit {
            VMLiteral::Closure { label, captures } => {
                let mut seen = FxHashSet::default();
                let caps = vm.capture_values(&captures, &mut seen);

                vm.set_reg_value(
                    self.dst,
                    RuntimeValue::Function {
                        name: label,
                        captures: Arc::new(caps),
                    },
                );
            }
            #[cfg(feature = "native")]
            VMLiteral::ExternFunction {
                abi,
                library,
                symbol,
                parameters,
                return_type,
                pure,
                memo,
                memo_params,
            } => {
                let func = vm.load_extern_function(
                    abi,
                    library,
                    symbol,
                    parameters,
                    return_type,
                    pure,
                    memo,
                    memo_params,
                )?;

                vm.set_reg_value(self.dst, RuntimeValue::ExternFunction(Arc::new(func)));
            }
            #[cfg(all(feature = "wasm", not(feature = "native")))]
            VMLiteral::ExternFunction { .. } => {}
            other => {
                let literal = RuntimeValue::from_vm_literal(other, &mut vm.big_consts);
                vm.set_reg_value(self.dst, literal);
            }
        }

        Ok(TerminateValue::None)
    }
}

impl VMEvaluation for VMRange {
    #[instrument(skip_all)]
    fn run(
        &self,
        vm: &mut VM,
        _block: &VMBlock,
        _ip: u32,
        _prev_block: Option<BlockId>,
    ) -> Result<TerminateValue, RuntimeError> {
        let from = vm.resolve_value_from_ref(vm.get_reg_value(self.from))?;

        let to = vm.resolve_value_from_ref(vm.get_reg_value(self.to))?;

        let as_range_bound = |value: RuntimeValue| -> Result<isize, RuntimeError> {
            match value {
                RuntimeValue::Int(v) => Ok(v),
                RuntimeValue::UInt(v) => Ok(v as isize),
                RuntimeValue::Float(v) => Ok(v as isize),
                RuntimeValue::Bool(v) => Ok(v as isize),
                RuntimeValue::Char(v) => Ok(v as isize),
                RuntimeValue::List(v) => Ok(v.0.len() as isize),
                RuntimeValue::Aggregate(_, v) => Ok(v.as_ref().0.0.len() as isize),
                RuntimeValue::Str(v) => Ok(v.len() as isize),
                RuntimeValue::Range(from, to) => Ok((to - from).max(0)),
                other => Err(RuntimeError::ExpectedNumericFound {
                    found: Box::new(other),
                }),
            }
        };

        let from = as_range_bound(from)?;
        let to = as_range_bound(to)?;

        let range = if self.inclusive {
            RuntimeValue::Range(from, to + 1)
        } else {
            RuntimeValue::Range(from, to)
        };

        vm.set_reg_value(self.dst, range);
        Ok(TerminateValue::None)
    }
}

impl VMEvaluation for VMList {
    #[instrument(skip_all)]
    fn run(
        &self,
        vm: &mut VM,
        _block: &VMBlock,
        _ip: u32,
        _prev_block: Option<BlockId>,
    ) -> Result<TerminateValue, RuntimeError> {
        let values = self
            .items
            .iter()
            .map(|item| vm.get_reg_value(*item).clone())
            .collect();

        vm.set_reg_value(self.dst, RuntimeValue::List(GcVec::new(values)));
        Ok(TerminateValue::None)
    }
}

impl VMEvaluation for VMAggregate {
    #[instrument(skip_all)]
    fn run(
        &self,
        vm: &mut VM,
        block: &VMBlock,
        _ip: u32,
        _prev_block: Option<BlockId>,
    ) -> Result<TerminateValue, RuntimeError> {
        let layout = block
            .aggregate_layouts
            .get(self.layout as usize)
            .ok_or_else(|| RuntimeError::InvalidBytecode("invalid aggregate layout".to_string()))?;

        let mut entries = Vec::with_capacity(layout.members.len());
        for (name, reg) in layout.members.iter().zip(self.fields.iter()) {
            let value = vm.resolve_value_from_ref(vm.get_reg_value(*reg))?;
            entries.push((name, value));
        }

        vm.set_reg_value(
            self.dst,
            RuntimeValue::Aggregate(
                layout.name.clone(),
                Arc::new(GcMap::new(ObjectMap(
                    entries.into_iter().map(|x| (*x.0, x.1)).collect(),
                ))),
            ),
        );

        Ok(TerminateValue::None)
    }
}

impl VMEvaluation for VMEnum {
    #[instrument(skip_all)]
    fn run(
        &self,
        vm: &mut VM,
        block: &VMBlock,
        _ip: u32,
        _prev_block: Option<BlockId>,
    ) -> Result<TerminateValue, RuntimeError> {
        let name = vm.local_type(block, self.name)?;
        let payload = self
            .payload
            .map(|reg| ValueSlot::new(vm.get_reg_value(reg).clone()));

        vm.set_reg_value(
            self.dst,
            RuntimeValue::Enum(name.clone(), self.variant as usize, payload),
        );
        Ok(TerminateValue::None)
    }
}
