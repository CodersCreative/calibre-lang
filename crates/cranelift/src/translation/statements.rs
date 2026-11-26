use crate::{
    translation::{CraneliftTranslation, FunctionTranslator},
    values::CompiledValue,
};
use calibre_lir::{
    MirDataType,
    ast::{LirAssign, LirDeclare, LirLValue},
};
use cranelift::codegen::ir::MachMemFlags;
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

        if self.is_referenced || translator.mutable_captures.contains(&self.dest) {
            if let Some(existing) = translator.boxed_vars.get(&self.dest).cloned() {
                translator.store_boxed(existing.clone(), value)?;
                return Ok(CompiledValue::new(existing.value, self.data_type.clone()));
            }

            let value = translator.box_value(&self.data_type, value)?;

            translator.boxed_vars.insert(
                self.dest.clone(),
                CompiledValue::new(value, self.data_type.clone()),
            );

            return Ok(CompiledValue::new(value, self.data_type.clone()));
        }

        let value = if self.data_type.is_dyn() {
            translator.coerce_to_dyn(value)?
        } else {
            value
        };

        let ty = translator.types.to_cranelift_type(&self.data_type);
        let var = translator.builder.declare_var(ty);

        translator.builder.def_var(var, value.value);
        translator
            .variables
            .insert(self.dest.clone(), (self.data_type.clone(), var));

        if matches!(
            &self.data_type,
            MirDataType::Ptr(_)
                | MirDataType::Option(_)
                | MirDataType::Result { .. }
                | MirDataType::Str
                | MirDataType::Struct { .. }
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
                if let Some(boxed) = translator.boxed_vars.get(&key).cloned() {
                    let value = self
                        .value
                        .translate(translator, Some(boxed.data_type.clone()))?;

                    let data_type = boxed.data_type.clone().unwrap_ffi();
                    let old = translator.store_boxed(boxed, value)?;
                    let old = translator.convert_from_i64(old, &data_type);

                    return Ok(CompiledValue::new(old, data_type));
                }

                if let Some((data_type, var)) = translator.variables.get(&key).cloned() {
                    let value = self.value.translate(translator, Some(data_type.clone()))?;

                    if data_type.is_ref() && !value.data_type.is_ref() {
                        let ptr = translator.builder.use_var(var);

                        let tag = translator.builder.ins().iconst(
                            translator.types.int(),
                            data_type
                                .unwrap_all_refs()
                                .type_tag(translator.registry.enums())
                                as i64,
                        );

                        let payload = translator.convert_to_i64(value)?;

                        let func_id = translator
                            .runtime_functions
                            .get("calibre_set_ref")
                            .ok_or("calibre_set_ref not found in runtime_functions")?;

                        let func_ref = translator
                            .module
                            .declare_func_in_func(*func_id, translator.builder.func);

                        let call = translator
                            .builder
                            .ins()
                            .call(func_ref, &[ptr, tag, payload]);

                        let old_payload = translator.builder.inst_results(call)[0];

                        let pointee = match &data_type {
                            MirDataType::Ref(inner, _) => (**inner).clone(),
                            _ => data_type.clone(),
                        }
                        .unwrap_ffi();

                        let old = translator.convert_from_i64(old_payload, &pointee);

                        return Ok(CompiledValue::new(old, pointee));
                    }

                    let value = if data_type.is_dyn() {
                        translator.coerce_to_dyn(value)?
                    } else {
                        value
                    };

                    let old = translator.builder.use_var(var);

                    if translator.gc_managed_vars.contains(&key) {
                        let func_ref = translator.get_add_root()?;
                        translator.builder.ins().call(func_ref, &[old]);
                    }

                    translator.builder.def_var(var, value.value);
                    Ok(CompiledValue::new(old, data_type))
                } else if let Some((data_type, data_id)) = translator.globals.get(&key).cloned() {
                    let value = self.value.translate(translator, Some(data_type.clone()))?;

                    let global = translator
                        .module
                        .declare_data_in_func(data_id, translator.builder.func);

                    let addr = translator
                        .builder
                        .ins()
                        .symbol_value(translator.types.ptr(), global);

                    let cl_type = translator.types.to_cranelift_type(&data_type);

                    let old = translator
                        .builder
                        .ins()
                        .load(cl_type, MachMemFlags::new(), addr, 0);

                    translator
                        .builder
                        .ins()
                        .store(MachMemFlags::new(), value.value, addr, 0);

                    Ok(CompiledValue::new(old, data_type))
                } else {
                    Err(format!("Unable to find variable: {key}").into())
                }
            }
            LirLValue::Ptr(ptr) => {
                let value = self.value.translate(translator, None)?;
                let value_type = value.data_type.clone();
                let ptr = ptr.translate(translator, None)?;

                let tag = value_type.type_tag(translator.registry.enums()) as i64;
                let tag = translator.builder.ins().iconst(translator.types.int(), tag);

                let runtime_name = "calibre_set_ref";

                let func_id = translator
                    .runtime_functions
                    .get(runtime_name)
                    .ok_or(format!("{} not found in runtime_functions", runtime_name))?;

                let func_ref = translator
                    .module
                    .declare_func_in_func(*func_id, translator.builder.func);

                let payload = translator.convert_to_i64(value)?;

                let call = translator
                    .builder
                    .ins()
                    .call(func_ref, &[ptr.value, tag, payload]);
                let result = translator.builder.inst_results(call)[0];

                let old_data_type = match &ptr.data_type {
                    MirDataType::Ref(inner, _) => (**inner).clone(),
                    other if other.is_dyn() => value_type,
                    other => other.clone(),
                }
                .unwrap_ffi();

                let old = translator.convert_from_i64(result, &old_data_type);

                Ok(CompiledValue::new(old, old_data_type))
            }
        }
    }
}
