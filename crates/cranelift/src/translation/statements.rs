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
        let name = self.dest.to_string();

        let ty = translator.types.to_cranelift_type(&self.data_type);
        let var = translator.builder.declare_var(ty);

        translator.builder.def_var(var, value.value);
        translator
            .variables
            .insert(name, (self.data_type.clone(), var));

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
                let name = key.to_string();

                if let Some((_, var)) = translator.variables.get(&name) {
                    translator.builder.def_var(*var, value.value);
                    Ok(value)
                } else {
                    Err(format!("Unable to find variable: {name}").into())
                }
            }
            LirLValue::Ptr(_) => {
                // TODO
                Ok(value)
            }
        }
    }
}
