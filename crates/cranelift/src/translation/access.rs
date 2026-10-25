use crate::{
    translation::{CraneliftTranslation, FunctionTranslator},
    values::CompiledValue,
};
use calibre_lir::{
    MirDataType,
    ast::{LirDeref, LirDiscriminant, LirMember, LirRef, LirRefLoad},
};
use cranelift::codegen::ir::{InstBuilder, MemFlags, StackSlotData, StackSlotKind};
use cranelift_module::Module;
use std::error::Error;

impl CraneliftTranslation for LirMember {
    fn translate<'a, M: Module>(
        self,
        translator: &mut FunctionTranslator<'a, M>,
        data_type: Option<MirDataType>,
    ) -> Result<CompiledValue, Box<dyn Error>> {
        let base = self.base.translate(translator, None)?;
        let member = self.field.as_str();
        let is_next_or_zero = member == "next" || member == "0";

        // TODO Get if enum
        match &base.data_type {
            MirDataType::Option(_) | MirDataType::Result { .. }
                if is_next_or_zero
                    || base.data_type.is_result() && (member == "ok" || member == "err") =>
            {
                let stack_slot = translator.builder.create_sized_stack_slot(StackSlotData {
                    kind: StackSlotKind::ExplicitSlot,
                    size: 8,
                    align_shift: 3,
                });

                translator
                    .builder
                    .ins()
                    .stack_store(base.value, stack_slot, 0);

                let ptr =
                    translator
                        .builder
                        .ins()
                        .stack_load(translator.types.int(), stack_slot, 0);

                let ptr = translator.builder.ins().iadd_imm(ptr, 8);

                let runtime_name = "calibre_enum_get";

                let func_id = translator
                    .runtime_functions
                    .get(runtime_name)
                    .ok_or(format!("{} not found in runtime_functions", runtime_name))?;

                let func_ref = translator
                    .module
                    .declare_func_in_func(*func_id, translator.builder.func);

                let call = translator.builder.ins().call(func_ref, &[ptr]);
                let result = translator.builder.inst_results(call)[0];

                Ok(CompiledValue::new(
                    result,
                    data_type.unwrap_or(MirDataType::Int),
                ))
            }
            _ => {
                // TODO
                Ok(base)
            }
        }
    }
}

impl CraneliftTranslation for LirDiscriminant {
    fn translate<'a, M: Module>(
        self,
        translator: &mut FunctionTranslator<'a, M>,
        _data_type: Option<MirDataType>,
    ) -> Result<CompiledValue, Box<dyn Error>> {
        let value = self.value.translate(translator, None)?;

        // TODO Get if enum
        match &value.data_type {
            MirDataType::Option(_) | MirDataType::Result { .. } => {
                let stack_slot = translator.builder.create_sized_stack_slot(StackSlotData {
                    kind: StackSlotKind::ExplicitSlot,
                    size: 8,
                    align_shift: 3,
                });

                translator
                    .builder
                    .ins()
                    .stack_store(value.value, stack_slot, 0);

                let ptr =
                    translator
                        .builder
                        .ins()
                        .stack_load(translator.types.int(), stack_slot, 0);

                let ptr = translator.builder.ins().iadd_imm(ptr, 8);

                let runtime_name = "calibre_enum_discriminant";

                let func_id = translator
                    .runtime_functions
                    .get(runtime_name)
                    .ok_or(format!("{} not found in runtime_functions", runtime_name))?;

                let func_ref = translator
                    .module
                    .declare_func_in_func(*func_id, translator.builder.func);

                let call = translator.builder.ins().call(func_ref, &[ptr]);
                let result = translator.builder.inst_results(call)[0];

                Ok(CompiledValue::new(result, MirDataType::UInt))
            }
            _ => {
                let value = translator.builder.ins().iconst(translator.types.uint(), 0);
                Ok(CompiledValue::new(value, MirDataType::UInt))
            }
        }
    }
}

impl CraneliftTranslation for LirRef {
    fn translate<'a, M: Module>(
        self,
        translator: &mut FunctionTranslator<'a, M>,
        data_type: Option<MirDataType>,
    ) -> Result<CompiledValue, Box<dyn Error>> {
        // TODO Get a pointer
        let value = self
            .value
            .translate(translator, data_type.map(|x| x.unwrap_all_refs().clone()))?;
        Ok(value)
    }
}

impl CraneliftTranslation for LirRefLoad {
    fn translate<'a, M: Module>(
        self,
        translator: &mut FunctionTranslator<'a, M>,
        _data_type: Option<MirDataType>,
    ) -> Result<CompiledValue, Box<dyn Error>> {
        // TODO Get a pointer
        if let Some((data_type, var)) = translator.variables.get(&self.value) {
            let value = translator.builder.use_var(*var);
            Ok(CompiledValue::new(value, data_type.clone()))
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
        data_type: Option<MirDataType>,
    ) -> Result<CompiledValue, Box<dyn Error>> {
        // TODO Dereference the pointer
        let ptr = self
            .value
            .translate(translator, data_type.map(|x| x.unwrap_all_refs().clone()))?;
        Ok(ptr)
    }
}
