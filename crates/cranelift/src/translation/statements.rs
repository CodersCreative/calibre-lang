use crate::{
    translation::{CraneliftTranslation, FunctionTranslator},
    values::CompiledValue,
};
use calibre_lir::{
    MirDataType,
    ast::{LirAssign, LirDeclare, LirLValue, LirMember, LirNodeType},
};
use cranelift::prelude::*;
use cranelift_module::Module;
use std::error::Error;

impl CraneliftTranslation for LirDeclare {
    fn translate<'a, M: Module>(
        self,
        translator: &mut FunctionTranslator<'a, M>,
        _data_type: Option<MirDataType>,
    ) -> Result<CompiledValue, Box<dyn Error>> {
        let value = self
            .value
            .translate(translator, Some(self.data_type.clone()))?;

        let ty = translator.types.to_cranelift_type(&self.data_type);
        let var = translator.builder.declare_var(ty);

        translator.builder.def_var(var, value.value);
        translator
            .variables
            .insert(self.dest.clone(), (self.data_type.clone(), var));

        if matches!(
            &self.data_type,
            MirDataType::Ptr(_) | MirDataType::Option(_) | MirDataType::Result { .. }
        ) {
            translator.gc_managed_vars.insert(self.dest.clone());
        }

        Ok(value)
    }
}

impl CraneliftTranslation for LirAssign {
    fn translate<'a, M: Module>(
        self,
        translator: &mut FunctionTranslator<'a, M>,
        _data_type: Option<MirDataType>,
    ) -> Result<CompiledValue, Box<dyn Error>> {
        match self.dest {
            LirLValue::Var(key) => {
                if let Some((data_type, var)) = translator.variables.get(&key).cloned() {
                    let value = self.value.translate(translator, Some(data_type.clone()))?;
                    translator.builder.def_var(var, value.value);
                    Ok(value)
                } else if let Some((data_type, data_id)) = translator.globals.get(&key).cloned() {
                    let value = self.value.translate(translator, Some(data_type.clone()))?;

                    let global = translator
                        .module
                        .declare_data_in_func(data_id, translator.builder.func);

                    let addr = translator
                        .builder
                        .ins()
                        .global_value(translator.types.ptr(), global);

                    translator
                        .builder
                        .ins()
                        .store(MemFlags::new(), value.value, addr, 0);

                    Ok(value)
                } else {
                    Err(format!("Unable to find variable: {key}").into())
                }
            }
            LirLValue::Ptr(ptr_expr) => {
                let value = self.value.translate(translator, None)?;

                // TODO
                Ok(value)
            }
        }
    }
}
