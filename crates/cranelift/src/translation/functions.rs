use crate::{
    natives::{ArgumentKind, runtime_functions},
    translation::{CraneliftTranslation, FunctionTranslator},
    values::CompiledValue,
};
use calibre_lir::MirDataType;
use calibre_lir::ast::{LirCall, LirLoad, LirNodeType};
use cranelift::codegen::ir::{InstBuilder, types};
use cranelift_module::Module;
use std::error::Error;

impl CraneliftTranslation for LirCall {
    fn translate<'a, M: Module>(
        self,
        translator: &mut FunctionTranslator<'a, M>,
        data_type: Option<MirDataType>,
    ) -> Result<CompiledValue, Box<dyn Error>> {
        let callee_value = match *self.caller {
            LirNodeType::Load(LirLoad { value }) => value,
            x => {
                return Err(format!("Call callee must be a Load, found {}", x).into());
            }
        };

        let callee_str = callee_value.name().as_str();

        let mut arg_types = Vec::new();

        if let Some(&func_id) = translator.functions.get(&callee_value) {
            let func_ref = translator
                .module
                .declare_func_in_func(func_id, translator.builder.func);
            let function = translator.registry.functions.get(&callee_value);

            let args = self
                .args
                .into_iter()
                .map(|arg| {
                    let arg = arg.translate(translator, None)?;
                    arg_types.push(arg.data_type);
                    Ok(arg.value)
                })
                .collect::<Result<Box<[_]>, Box<dyn Error>>>()?;

            let call = translator.builder.ins().call(func_ref, &args);
            let return_type = function
                .map(|x| x.return_type.clone())
                .or_else(|| data_type);

            if let Some(return_type) = return_type
                && self.returns_value
            {
                let results = translator.builder.inst_results(call);
                if let Some(&result) = results.first() {
                    Ok(CompiledValue::new(result, return_type))
                } else {
                    Err("Expected return value but found none".into())
                }
            } else {
                Ok(CompiledValue::new(
                    translator.builder.ins().iconst(translator.types.int(), 0),
                    MirDataType::Null,
                ))
            }
        } else if let Some(runtime_name) = {
            if let Some(x) = translator.natives.get(&callee_value) {
                Some(*x)
            } else if runtime_functions().contains_key(callee_str) {
                Some(callee_str)
            } else {
                None
            }
        } {
            let func_id = translator
                .runtime_functions
                .get(runtime_name)
                .ok_or(format!("{} not found in runtime_functions", runtime_name))?;

            let sig = runtime_functions().get(runtime_name).ok_or(format!(
                "Signature not found for runtime function {}",
                runtime_name
            ))?;

            let func_ref = translator
                .module
                .declare_func_in_func(*func_id, translator.builder.func);

            let mut args = Vec::new();
            let mut arg_index = 0;

            for (param_type, arg_kind) in sig.params.iter() {
                let arg = self.args.get(arg_index).ok_or(format!(
                    "Missing argument {} for runtime function {} (signature has {} params, have {} args)",
                    arg_index, runtime_name, sig.params.len(), self.args.len()
                ))?;

                let value = arg.clone().translate(translator, None)?;

                arg_types.push(value.data_type.clone());

                match arg_kind {
                    ArgumentKind::Direct => {
                        let value = if *param_type == types::I64 {
                            translator.convert_to_i64(value)?
                        } else {
                            value.value
                        };

                        args.push(value);
                        arg_index += 1;
                    }
                    ArgumentKind::TagPayload => {
                        let tag = value.data_type.type_tag(translator.registry.enums()) as i64;

                        let tag = translator.builder.ins().iconst(translator.types.int(), tag);

                        let value = if *param_type == types::I64 {
                            translator.convert_to_i64(value)?
                        } else {
                            value.value
                        };

                        args.push(tag);
                        args.push(value);

                        arg_index += 1;
                    }
                }
            }

            let call = translator.builder.ins().call(func_ref, &args);

            let result = if sig.returns.is_some() {
                translator.builder.inst_results(call)[0]
            } else {
                translator.builder.ins().iconst(translator.types.int(), 0)
            };

            let mut return_type = if let Some(t) = &sig.typed_return {
                t.clone()
            } else if let Some(t) = &sig.returns {
                match t {
                    t if t == &translator.types.int() => MirDataType::Int,
                    t if t == &translator.types.uint() => MirDataType::UInt,
                    t if t == &translator.types.float() => MirDataType::Float,
                    t if t == &translator.types.bool() => MirDataType::Bool,
                    t if t == &translator.types.byte() => MirDataType::Byte,
                    t if t == &translator.types.char() => MirDataType::Char,
                    _ => MirDataType::Null,
                }
            } else {
                MirDataType::Null
            };

            if let Some(t) = data_type
                && return_type.is_dyn()
            {
                return_type = t;
            }

            let subst = arg_types
                .into_iter()
                .enumerate()
                .map(|x| (x.0.to_string(), x.1))
                .collect();

            return_type.substitute(&subst);

            Ok(CompiledValue::new(result, return_type))
        } else {
            Err(format!("Unable to find function: {}", callee_value).into())
        }
    }
}
