use crate::{
    translation::{CraneliftTranslation, FunctionTranslator},
    values::CompiledValue,
};
use calibre_lir::{
    MirDataType,
    ast::{LirClosure, LirLoad},
};
use cranelift::codegen::ir::InstBuilder;
use cranelift_module::Module;
use std::error::Error;

impl CraneliftTranslation for LirClosure {
    fn translate<'a, M: Module>(
        self,
        translator: &mut FunctionTranslator<'a, M>,
        data_type: Option<MirDataType>,
    ) -> Result<CompiledValue, Box<dyn Error>> {
        let func_id = translator
            .function_ids
            .get(&self.label)
            .copied()
            .ok_or(format!("no uniform entry for closure {}", self.label))?;

        let func_ref = translator
            .module
            .declare_func_in_func(func_id, translator.builder.func);

        let func_ptr = translator
            .builder
            .ins()
            .func_addr(translator.types.ptr(), func_ref);

        let field_count = translator.int_const(self.captures.len() as i64 + 1);
        let closure =
            translator.runtime_call("calibre_fn_closure_new", &[field_count, func_ptr])?;

        for (i, capture) in self.captures.iter().enumerate() {
            let (data_type, payload) = if capture.mutable
                && let Some(value) = translator.boxed_vars.get(&capture.name).cloned()
            {
                (value.data_type.clone(), translator.convert_to_i64(value)?)
            } else {
                let value = LirLoad {
                    value: capture.name.clone(),
                }
                .translate(translator, Some(capture.data_type.clone()))?;

                (value.data_type.clone(), translator.convert_to_i64(value)?)
            };

            let tag = data_type.type_tag(translator.registry.enums()) as i64;
            let tag = translator.builder.ins().iconst(translator.types.int(), tag);

            let index = translator.int_const(i as i64 + 1);
            translator.runtime_call_void(
                "calibre_aggregate_set_field",
                &[closure, index, tag, payload],
            )?;
        }

        Ok(CompiledValue::new(
            closure,
            data_type.unwrap_or(MirDataType::Dynamic),
        ))
    }
}
