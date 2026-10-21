use crate::{
    translation::{CraneliftTranslation, FunctionTranslator},
    values::CompiledValue,
};
use calibre_lir::MirDataType;
use calibre_lir::ast::{LirCall, LirLoad, LirNodeType};
use cranelift::prelude::*;
use cranelift_module::Module;
use std::error::Error;

impl CraneliftTranslation for LirCall {
    fn translate<'a, M: Module>(
        self,
        translator: &mut FunctionTranslator<'a, M>,
    ) -> Result<CompiledValue, Box<dyn Error>> {
        let (func_ref, function) = match *self.caller {
            LirNodeType::Load(LirLoad { value }) => {
                if let Some(&func_id) = translator.functions.get(&value) {
                    (
                        translator
                            .module
                            .declare_func_in_func(func_id, translator.builder.func),
                        translator.registry.functions.get(&value),
                    )
                } else {
                    return Err(format!("Unable to find function: {}", value).into());
                }
            }
            _ => {
                unimplemented!()
            }
        };

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
    }
}
