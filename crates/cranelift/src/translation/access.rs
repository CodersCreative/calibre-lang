use crate::{
    translation::{CraneliftTranslation, FunctionTranslator},
    values::CompiledValue,
};
use calibre_lir::{
    MirDataType,
    ast::{LirDeref, LirDiscriminant, LirRef, LirRefLoad},
};
use cranelift::codegen::ir::{InstBuilder, MemFlags};
use cranelift_module::Module;
use std::error::Error;

impl CraneliftTranslation for LirDiscriminant {
    fn translate<'a, M: Module>(
        self,
        translator: &mut FunctionTranslator<'a, M>,
    ) -> Result<CompiledValue, Box<dyn Error>> {
        // TODO Get Disciminant
        let value = translator.builder.ins().iconst(translator.types.int(), 0);
        Ok(CompiledValue::new(value, MirDataType::Int))
    }
}

impl CraneliftTranslation for LirRef {
    fn translate<'a, M: Module>(
        self,
        translator: &mut FunctionTranslator<'a, M>,
    ) -> Result<CompiledValue, Box<dyn Error>> {
        // TODO Get a pointer
        let value = self.value.translate(translator)?;
        Ok(value)
    }
}

impl CraneliftTranslation for LirRefLoad {
    fn translate<'a, M: Module>(
        self,
        translator: &mut FunctionTranslator<'a, M>,
    ) -> Result<CompiledValue, Box<dyn Error>> {
        // TODO Get a pointer
        if let Some((_, var)) = translator.variables.get(&self.value) {
            let value = translator.builder.use_var(*var);
            Ok(CompiledValue::new(value, MirDataType::Null))
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
            Err(format!("Unable to find variable for ref load: {}", self.value).into())
        }
    }
}

impl CraneliftTranslation for LirDeref {
    fn translate<'a, M: Module>(
        self,
        translator: &mut FunctionTranslator<'a, M>,
    ) -> Result<CompiledValue, Box<dyn Error>> {
        // TODO Dereference the pointer
        let ptr = self.value.translate(translator)?;
        Ok(ptr)
    }
}
