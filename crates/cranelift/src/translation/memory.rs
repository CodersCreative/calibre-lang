use crate::{
    translation::{CraneliftTranslation, FunctionTranslator},
    values::CompiledValue,
};
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
    ) -> Result<CompiledValue, Box<dyn Error>> {
        if let Some((ty, var)) = translator.variables.get(&self.value) {
            let value = translator.builder.use_var(*var);
            Ok(CompiledValue::new(value, ty.clone()))
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
    ) -> Result<CompiledValue, Box<dyn Error>> {
        // TODO Dropping
        if let Some((_, var)) = translator.variables.get(&self.value) {
            let value = translator.builder.use_var(*var);
            Ok(CompiledValue::new(value, MirDataType::Null))
        } else {
            Err(format!("Unable to find variable for move: {}", self.value).into())
        }
    }
}

impl CraneliftTranslation for LirDrop {
    fn translate<'a, M: Module>(
        self,
        translator: &mut FunctionTranslator<'a, M>,
    ) -> Result<CompiledValue, Box<dyn Error>> {
        // TODO Dropping
        let value = translator.builder.ins().iconst(translator.types.int(), 0);
        Ok(CompiledValue::new(value, MirDataType::Null))
    }
}
