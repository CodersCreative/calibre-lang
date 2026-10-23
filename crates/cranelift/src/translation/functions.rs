use crate::{
    natives::{ArgumentKind, runtime_functions},
    translation::{CraneliftTranslation, FunctionTranslator},
    values::CompiledValue,
};
use calibre_lir::MirDataType;
use calibre_lir::ast::{LirCall, LirLoad, LirNodeType};
use cranelift::codegen::ir::InstBuilder;
use cranelift_module::Module;
use std::error::Error;

impl CraneliftTranslation for LirCall {
    fn translate<'a, M: Module>(
        self,
        translator: &mut FunctionTranslator<'a, M>,
    ) -> Result<CompiledValue, Box<dyn Error>> {
        let callee_value = match *self.caller {
            LirNodeType::Load(LirLoad { value }) => value,
            _ => {
                return Err("Call callee must be a Load".into());
            }
        };

        if let Some(&func_id) = translator.functions.get(&callee_value) {
            let func_ref = translator
                .module
                .declare_func_in_func(func_id, translator.builder.func);
            let function = translator.registry.functions.get(&callee_value);

            let args = self
                .args
                .into_iter()
                .map(|arg| Ok(arg.translate(translator)?.value))
                .collect::<Result<Box<[_]>, Box<dyn Error>>>()?;

            let call = translator.builder.ins().call(func_ref, &args);
            let return_type = function
                .map(|x| x.return_type.clone())
                .unwrap_or(MirDataType::Int);

            if self.returns_value || return_type.is_null() {
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
        } else if let Some(&runtime_name) = translator.natives.get(&callee_value) {
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

            for (_, arg_kind) in sig.params.iter() {
                let arg = self.args.get(arg_index).ok_or(format!(
                    "Missing argument {} for runtime function {}",
                    arg_index, runtime_name
                ))?;

                let value = arg.clone().translate(translator)?;

                match arg_kind {
                    ArgumentKind::Direct => {
                        args.push(value.value);
                        arg_index += 1;
                    }
                    ArgumentKind::TagPayload => {
                        let tag = value.data_type.type_tag() as i64;

                        let tag_value =
                            translator.builder.ins().iconst(translator.types.int(), tag);

                        let payload_value = value.value;

                        args.push(tag_value);
                        args.push(payload_value);

                        arg_index += 1;
                    }
                }
            }

            let call = translator.builder.ins().call(func_ref, &args);
            let result = translator.builder.inst_results(call)[0];

            if let Some(t) = &sig.returns
                && t == &translator.types.ptr()
            {
                let root_func_id = translator
                    .runtime_functions
                    .get("calibre_gc_add_root")
                    .ok_or("calibre_gc_add_root not found in runtime_functions")?;

                let root_func_ref = translator
                    .module
                    .declare_func_in_func(*root_func_id, translator.builder.func);

                translator.builder.ins().call(root_func_ref, &[result]);

                Ok(CompiledValue::new(
                    result,
                    MirDataType::Ptr(Box::new(MirDataType::Null)),
                ))
            } else {
                // TODO Probably swap this for an MirDataType return type and just remove all of this hassle
                let return_type = if let Some(t) = &sig.returns {
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

                Ok(CompiledValue::new(result, return_type))
            }
        } else {
            Err(format!("Unable to find function: {}", callee_value).into())
        }
    }
}
