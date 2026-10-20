use crate::{
    translation::{CraneliftTranslation, FunctionTranslator},
    values::CompiledValue,
};
use calibre_lir::ast::LirLoad;
use cranelift_module::Module;
use std::error::Error;

impl CraneliftTranslation for LirLoad {
    fn translate<'a, M: Module>(
        self,
        translator: &mut FunctionTranslator<'a, M>,
    ) -> Result<CompiledValue, Box<dyn Error>> {
        let name = self.value.to_string();

        if let Some((ty, var)) = translator.variables.get(&name) {
            let value = translator.builder.use_var(*var);
            Ok(CompiledValue::new(value, ty.clone()))
        } else {
            Err(format!("Unable to find variable: {name}").into())
        }
    }
}
