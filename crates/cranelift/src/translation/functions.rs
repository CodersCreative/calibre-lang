use crate::{
    natives::{ArgumentKind, runtime_functions},
    translation::{CraneliftTranslation, FunctionTranslator},
    values::CompiledValue,
};
use calibre_lir::MirDataType;
use calibre_lir::ast::{LirCall, LirLoad, LirNodeType};
use cranelift::codegen::ir::{InstBuilder, Signature, Type, Value, types};
use cranelift::codegen::isa::CallConv;
use cranelift::prelude::AbiParam;
use cranelift_module::Module;
use std::error::Error;

impl CraneliftTranslation for LirCall {
    fn translate<'a, M: Module>(
        self,
        translator: &mut FunctionTranslator<'a, M>,
        data_type: Option<MirDataType>,
    ) -> Result<CompiledValue, Box<dyn Error>> {
        let key = match &*self.caller {
            LirNodeType::Load(LirLoad { value }) => Some(value.clone()),
            _ => None,
        };

        let Some(key) = key else {
            let mut callee = self.caller.translate(translator, None)?;

            while let MirDataType::Ref(inner, _) = callee.data_type.clone() {
                let payload = translator.runtime_call("calibre_deref", &[callee.value])?;
                callee = CompiledValue::new(payload, *inner);
            }

            let mut arg_types = Vec::new();

            match callee.data_type.unwrap_all_refs().clone() {
                MirDataType::Function {
                    parameters,
                    return_type,
                } => {
                    return emit_indirect_call(
                        translator,
                        callee.value,
                        parameters,
                        *return_type,
                        &self.args,
                        &mut arg_types,
                    );
                }
                MirDataType::Dynamic => {
                    return emit_indirect_call(
                        translator,
                        callee.value,
                        Vec::new(),
                        MirDataType::Dynamic,
                        &self.args,
                        &mut arg_types,
                    );
                }
                other => {
                    return Err(
                        format!("Call callee must be a Load or function, found {}", other).into(),
                    );
                }
            }
        };

        let callee_str = key.name().as_str();

        let mut arg_types = Vec::new();

        if let Some((data_type, var)) = translator.variables.get(&key).cloned()
            && let MirDataType::Function {
                parameters,
                return_type,
            } = data_type.unwrap_all_refs().clone()
        {
            let closure = translator.builder.use_var(var);

            return emit_indirect_call(
                translator,
                closure,
                parameters,
                *return_type,
                &self.args,
                &mut arg_types,
            );
        }

        if let Some(&func_id) = translator.functions.get(&key) {
            let func_ref = translator
                .module
                .declare_func_in_func(func_id, translator.builder.func);
            let function = translator.registry.functions.get(&key);

            let param_types: Vec<Type> = function
                .map(|f| {
                    f.params
                        .iter()
                        .map(|p| translator.types.to_cranelift_type(&p.data_type))
                        .collect()
                })
                .unwrap_or_default();

            let mut arg_values = self
                .args
                .into_iter()
                .enumerate()
                .map(|(i, arg)| {
                    let arg = arg.translate(translator, None)?;
                    arg_types.push(arg.data_type.clone());

                    let target = param_types.get(i).copied().unwrap_or(types::I64);
                    Ok(translator.cast_to(target, arg))
                })
                .collect::<Result<Vec<_>, Box<dyn Error>>>()?;

            if function.is_some_and(|f| !f.captures.is_empty()) {
                let value = if translator.current_function.as_ref() == Some(&key)
                    && let Some(value) = translator.self_env
                {
                    value
                } else {
                    translator.builder.ins().iconst(types::I64, 0)
                };

                arg_values.insert(0, value);
            }

            let call = translator.builder.ins().call(func_ref, &arg_values);
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
            if let Some(x) = translator.natives.get(&key) {
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

                let mut value = arg.clone().translate(translator, None)?;

                while let MirDataType::Ref(inner, _) = value.data_type.clone() {
                    let payload = translator.runtime_call("calibre_deref", &[value.value])?;
                    value = CompiledValue::new(payload, *inner);
                }

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

            let mut return_type = if let Some(t) = &self.return_type {
                t.clone()
            } else if let Some(t) = &sig.typed_return {
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

            if let Some(t) = data_type {
                return_type = t.replace_dyn_from(&return_type);
            }

            let subst = arg_types
                .into_iter()
                .enumerate()
                .map(|x| (x.0.to_string(), x.1))
                .collect();

            return_type.substitute(&subst);

            Ok(CompiledValue::new(result, return_type))
        } else {
            Err(format!("Unable to find function: {}", key).into())
        }
    }
}

fn emit_indirect_call<'a, M: Module>(
    translator: &mut FunctionTranslator<'a, M>,
    closure: Value,
    parameters: Vec<MirDataType>,
    return_type: MirDataType,
    args: &[LirNodeType],
    arg_types: &mut Vec<MirDataType>,
) -> Result<CompiledValue, Box<dyn Error>> {
    let entry = translator.runtime_call("calibre_fn_entry", &[closure])?;

    let field_count = translator.int_const(args.len() as i64);
    let args_agg = translator.runtime_call("calibre_gc_alloc_aggregate", &[field_count])?;

    for (i, arg) in args.iter().enumerate() {
        let value = arg
            .clone()
            .translate(translator, parameters.get(i).cloned())?;
        arg_types.push(value.data_type.clone());

        let tag = value.data_type.type_tag(translator.registry.enums()) as i64;
        let tag = translator.int_const(tag);

        let value = translator.convert_to_i64(value)?;
        let index = translator.int_const(i as i64);

        translator.runtime_call_void(
            "calibre_aggregate_set_field",
            &[args_agg, index, tag, value],
        )?;
    }

    let sig = if let Some(sig) = translator.indirect_call_sig {
        sig
    } else {
        let mut sig = Signature::new(CallConv::SystemV);
        sig.params.push(AbiParam::new(types::I64));
        sig.params.push(AbiParam::new(types::I64));
        sig.returns.push(AbiParam::new(types::I64));

        let sig = translator.builder.import_signature(sig);
        translator.indirect_call_sig = Some(sig);
        sig
    };

    let call = translator
        .builder
        .ins()
        .call_indirect(sig, entry, &[closure, args_agg]);

    let return_type = return_type.unwrap_ffi();

    let value = translator.builder.inst_results(call)[0];
    let value = translator.convert_from_i64(value, &return_type);

    Ok(CompiledValue::new(value, return_type))
}
