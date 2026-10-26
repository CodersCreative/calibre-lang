use crate::{
    translation::{CraneliftTranslation, FunctionTranslator},
    values::CompiledValue,
};
use calibre_lir::Key;
use calibre_lir::{
    MirDataType,
    ast::{LirDrop, LirLoad, LirMove},
};
use cranelift::codegen::ir::{InstBuilder, MemFlags};
use cranelift_module::Module;
use std::error::Error;

impl CraneliftTranslation for LirLoad {
    fn translate<'a, M: Module>(
        self,
        translator: &mut FunctionTranslator<'a, M>,
        _data_type: Option<MirDataType>,
    ) -> Result<CompiledValue, Box<dyn Error>> {
        // TODO Find a way to handle this more gracefully
        if let Some(Key::VariableKey(_)) = translator.registry.natives.get(&self.value.name()) {
            let name_str = self.value.name().as_str();

            match name_str {
                "none" => {
                    let func_id = translator
                        .runtime_functions
                        .get("calibre_option_none")
                        .ok_or("calibre_option_none not found in runtime functions")?;

                    let func_ref = translator
                        .module
                        .declare_func_in_func(*func_id, translator.builder.func);

                    let call = translator.builder.ins().call(func_ref, &[]);
                    let result = translator.builder.inst_results(call)[0];

                    Ok(CompiledValue::new(
                        result,
                        MirDataType::Option(Box::new(MirDataType::Dynamic)),
                    ))
                }
                "true" => {
                    let value = translator.builder.ins().iconst(translator.types.bool(), 1);

                    Ok(CompiledValue::new(value, MirDataType::Bool))
                }
                "false" => {
                    let value = translator.builder.ins().iconst(translator.types.bool(), 0);

                    Ok(CompiledValue::new(value, MirDataType::Bool))
                }
                "INT_MIN" => {
                    let value = translator
                        .builder
                        .ins()
                        .iconst(translator.types.int(), isize::MIN as i64);

                    Ok(CompiledValue::new(value, MirDataType::Int))
                }
                "INT_MAX" => {
                    let value = translator
                        .builder
                        .ins()
                        .iconst(translator.types.int(), isize::MAX as i64);

                    Ok(CompiledValue::new(value, MirDataType::Int))
                }
                _ => {
                    unreachable!()
                }
            }
        } else if let Some((ty, var)) = translator.variables.get(&self.value).cloned() {
            let value = translator.builder.use_var(var);

            if translator.gc_managed_vars.contains(&self.value) {
                let func_ref = translator.get_add_root()?;
                translator.builder.ins().call(func_ref, &[value]);
            }

            Ok(CompiledValue::new(value, ty))
        } else if let Some((data_type, data_id)) = translator.globals.get(&self.value) {
            let global_handle = translator
                .module
                .declare_data_in_func(*data_id, translator.builder.func);

            let addr = translator
                .builder
                .ins()
                .global_value(translator.types.ptr(), global_handle);

            let cl_type = translator.types.to_cranelift_type(data_type);

            let value = translator
                .builder
                .ins()
                .load(cl_type, MemFlags::new(), addr, 0);

            Ok(CompiledValue::new(value, data_type.clone()))
        } else {
            Err(format!("Unable to find variable: {}", self.value).into())
        }
    }
}

impl CraneliftTranslation for LirMove {
    fn translate<'a, M: Module>(
        self,
        translator: &mut FunctionTranslator<'a, M>,
        _data_type: Option<MirDataType>,
    ) -> Result<CompiledValue, Box<dyn Error>> {
        // TODO Dropping
        if let Some((data_type, var)) = translator.variables.get(&self.value) {
            let value = translator.builder.use_var(*var);
            Ok(CompiledValue::new(value, data_type.clone()))
        } else {
            Err(format!("Unable to find variable for move: {}", self.value).into())
        }
    }
}

impl CraneliftTranslation for LirDrop {
    fn translate<'a, M: Module>(
        self,
        translator: &mut FunctionTranslator<'a, M>,
        _data_type: Option<MirDataType>,
    ) -> Result<CompiledValue, Box<dyn Error>> {
        // TODO Dropping
        let value = translator.builder.ins().iconst(translator.types.int(), 0);
        Ok(CompiledValue::new(value, MirDataType::Null))
    }
}
