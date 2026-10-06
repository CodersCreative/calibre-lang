use crate::{
    VM,
    error::RuntimeError,
    evaluate::instruction::VMEvaluation,
    native::stdlib::generator::GeneratorState,
    value::{GcMap, GcVec, RuntimeValue, TerminateValue},
};
use calibre_bytecode::{
    VMBlock, VMLiteral,
    instructions::literals::{VMAggregate, VMEnum, VMList, VMLoadLiteral, VMRange},
};
use calibre_lir::{TypeImplKey, VariableKey, ast::BlockId};
use calibre_parser::ast::ObjectMap;
use dumpster::sync::Gc;
use rustc_hash::FxHashSet;
use std::sync::Arc;
use tracing::instrument;
use wasm_sync::Mutex;

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
                use crate::value::ffi::ExternFunction;

                let abi_lower = abi.to_ascii_lowercase();
                if abi_lower != "c" && abi_lower != "zig" {
                    return Err(RuntimeError::Ffi(format!("unsupported ABI \"{}\"", abi)));
                }

                let mut last_err = None;
                let mut handle_opt = None;

                for candidate in VM::resolve_library_candidates(&library) {
                    match unsafe { libloading::Library::new(&candidate) } {
                        Ok(h) => {
                            handle_opt = Some(h);
                            break;
                        }
                        Err(e) => last_err = Some(e.to_string()),
                    }
                }

                let handle = handle_opt.ok_or_else(|| {
                    RuntimeError::Ffi(format!(
                        "failed to load library {} ({})",
                        library,
                        last_err.unwrap_or_else(|| "no candidates".to_string())
                    ))
                })?;

                let func = ExternFunction {
                    abi,
                    library,
                    symbol,
                    parameters,
                    return_type,
                    pure,
                    memo,
                    memo_params,
                    handle: Arc::new(handle),
                };

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
        let from = vm.resolve_value_ref(vm.get_reg_value(self.from))?;
        let to = vm.resolve_value_ref(vm.get_reg_value(self.to))?;

        let as_range_bound = |value: RuntimeValue| -> Result<i64, RuntimeError> {
            match value {
                RuntimeValue::Int(v) => Ok(v),
                RuntimeValue::UInt(v) => Ok(v as i64),
                RuntimeValue::Float(v) => Ok(v as i64),
                RuntimeValue::Bool(v) => Ok(v as i64),
                RuntimeValue::Char(v) => Ok(v as i64),
                RuntimeValue::List(v) => Ok(v.as_ref().0.len() as i64),
                RuntimeValue::Aggregate(_, v) => Ok(v.as_ref().0.0.len() as i64),
                RuntimeValue::Str(v) => Ok(v.len() as i64),
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

        vm.set_reg_value(self.dst, RuntimeValue::List(Arc::new(GcVec::new(values))));
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
            let mut value = vm.get_reg_value(*reg).clone();

            if value.is_ref_like()
                && let Ok(resolved) = vm.resolve_value_ref(&value)
            {
                value = resolved;
            }

            entries.push((name, value));
        }

        if let Some(type_name) = &layout.name
            && type_name.name() == "gen"
        {
            let next_fn = entries.iter().find_map(|(field, value)| {
                let short = field.rsplit(".").next().unwrap_or(field.as_str());
                (short == "data").then(|| value.clone())
            });

            if let Some(RuntimeValue::Function { name, captures }) = next_fn {
                let resolved_caps: Vec<(VariableKey, RuntimeValue)> = captures
                    .iter()
                    .map(|(k, v)| {
                        let resolved = vm.resolve_value_ref(v).unwrap_or_else(|_| v.clone());
                        (k.clone(), resolved)
                    })
                    .collect();

                let mut gen_vm =
                    VM::new_shared(vm.registry.clone(), vm.mappings.clone(), vm.config.clone());

                for (k, v) in &resolved_caps {
                    gen_vm.variables.insert(k.clone(), v.clone());
                }

                if !vm.ptr_heap.is_empty() {
                    gen_vm.ptr_heap = vm.ptr_heap.clone();
                }

                vm.set_reg_value(
                    self.dst,
                    RuntimeValue::Generator {
                        type_name: TypeImplKey::from(type_name.clone()),
                        state: Arc::new(Mutex::new(GeneratorState {
                            vm: gen_vm,
                            function_name: name,
                            captures: Arc::new(resolved_caps),
                            task_state: crate::TaskState::default(),
                            index: 0,
                            completed: false,
                        })),
                    },
                );

                return Ok(TerminateValue::None);
            }
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
            .map(|reg| Gc::new(vm.get_reg_value(reg).clone()));

        vm.set_reg_value(
            self.dst,
            RuntimeValue::Enum(name.clone(), self.variant as usize, payload),
        );
        Ok(TerminateValue::None)
    }
}
