use crate::{
    VM,
    conversion::{VMBlock, VMLiteral, instructions::literals::VMLoadLiteral},
    error::RuntimeError,
    evaluate::instruction::VMEvaluation,
    value::{RuntimeValue, TerminateValue},
};
use calibre_lir::ast::BlockId;
use std::sync::Arc;
use ustr::UstrSet;

impl VMEvaluation for VMLoadLiteral {
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
                let mut seen = UstrSet::default();
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
                use crate::value::ExternFunction;

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
            #[cfg(feature = "wasm")]
            VMLiteral::ExternFunction { .. } => {}
            other => {
                vm.set_reg_value(self.dst, RuntimeValue::from(other));
            }
        }

        Ok(TerminateValue::None)
    }
}
