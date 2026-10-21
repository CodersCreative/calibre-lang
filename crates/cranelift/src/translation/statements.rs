use crate::{
    translation::{CraneliftTranslation, FunctionTranslator},
    values::CompiledValue,
};
use calibre_lir::ast::{LirAssign, LirDeclare, LirLValue};
use cranelift_module::Module;
use std::error::Error;

impl CraneliftTranslation for LirDeclare {
    fn translate<'a, M: Module>(
        self,
        translator: &mut FunctionTranslator<'a, M>,
    ) -> Result<CompiledValue, Box<dyn Error>> {
        let value = self.value.translate(translator)?;

        let ty = translator.types.to_cranelift_type(&self.data_type);
        let var = translator.builder.declare_var(ty);

        translator.builder.def_var(var, value.value);
        translator
            .variables
            .insert(self.dest, (self.data_type.clone(), var));

        Ok(value)
    }
}

impl CraneliftTranslation for LirAssign {
    fn translate<'a, M: Module>(
        self,
        translator: &mut FunctionTranslator<'a, M>,
    ) -> Result<CompiledValue, Box<dyn Error>> {
        let value = self.value.translate(translator)?;

        match self.dest {
            LirLValue::Var(key) => {
                if let Some((_, var)) = translator.variables.get(&key) {
                    translator.builder.def_var(*var, value.value);
                    Ok(value)
                } else {
                    Err(format!("Unable to find variable: {key}").into())
                }
            }
            LirLValue::Ptr(_) => {
                // TODO
                Ok(value)
            }
        }
    }
}
