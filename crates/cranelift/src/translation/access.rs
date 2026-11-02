use crate::{
    translation::{CraneliftTranslation, FunctionTranslator},
    values::CompiledValue,
};
use calibre_lir::{
    MirDataType,
    ast::{LirDeref, LirDiscriminant, LirMember, LirRef, LirRefLoad},
    environment::LirType,
};
use calibre_parser::ast::RefMutability;
use cranelift::codegen::ir::{InstBuilder, MemFlags};
use cranelift_module::Module;
use std::error::Error;

impl CraneliftTranslation for LirMember {
    fn translate<'a, M: Module>(
        self,
        translator: &mut FunctionTranslator<'a, M>,
        mut data_type: Option<MirDataType>,
    ) -> Result<CompiledValue, Box<dyn Error>> {
        let base = self.base.translate(translator, None)?;
        let member = self.field.as_str();
        let mut index = 0;

        let is_enum = match base.data_type.unwrap_all_refs() {
            MirDataType::Struct { identifier, .. } => {
                if let Some(lir_type) = translator.registry.types.get(identifier) {
                    match lir_type {
                        LirType::Enum(x) => {
                            data_type = x
                                .iter()
                                .find(|x| x.0 == member)
                                .and_then(|x| x.1.clone())
                                .map(|x| MirDataType::Ref(Box::new(x), RefMutability::MutRef))
                                .or(data_type);
                            true
                        }
                        LirType::Struct(x) => {
                            data_type = x
                                .iter()
                                .enumerate()
                                .find(|x| x.1.0 == member)
                                .map(|x| {
                                    index = x.0;
                                    MirDataType::Ref(Box::new(x.1.1.clone()), RefMutability::MutRef)
                                })
                                .or(data_type);
                            false
                        }
                    }
                } else {
                    false
                }
            }
            MirDataType::Option(x) => {
                data_type = if x.is_dyn() {
                    data_type
                } else {
                    Some(MirDataType::Ref(x.clone(), RefMutability::MutRef))
                };

                false
            }
            MirDataType::Result { err, .. } if member == "Err" => {
                data_type = if err.is_dyn() {
                    data_type
                } else {
                    Some(MirDataType::Ref(err.clone(), RefMutability::MutRef))
                };

                false
            }
            MirDataType::Result { ok, .. } => {
                data_type = if ok.is_dyn() {
                    data_type
                } else {
                    Some(MirDataType::Ref(ok.clone(), RefMutability::MutRef))
                };

                false
            }
            _ => false,
        };

        if matches!(&base.data_type.unwrap_all_refs(), MirDataType::Option(_) if member == "Some" || member == "None")
            || matches!(&base.data_type.unwrap_all_refs(), MirDataType::Result { .. } if member == "Ok" || member == "Err")
            || is_enum
        {
            let runtime_name = if matches!(base.data_type, MirDataType::Ref(_, _)) {
                "calibre_enum_ref_get"
            } else {
                "calibre_enum_get"
            };

            let func_id = translator
                .runtime_functions
                .get(runtime_name)
                .ok_or(format!("{} not found in runtime_functions", runtime_name))?;

            let func_ref = translator
                .module
                .declare_func_in_func(*func_id, translator.builder.func);

            let call = translator.builder.ins().call(func_ref, &[base.value]);
            let result = translator.builder.inst_results(call)[0];

            Ok(CompiledValue::new(
                result,
                data_type.unwrap_or(MirDataType::Ref(
                    Box::new(MirDataType::Dynamic),
                    RefMutability::MutRef,
                )),
            ))
        } else {
            let runtime_name = if matches!(base.data_type, MirDataType::Ref(_, _)) {
                "calibre_enum_ref_get"
            } else {
                "calibre_enum_get"
            };

            let func_id = translator
                .runtime_functions
                .get(runtime_name)
                .ok_or(format!("{} not found in runtime_functions", runtime_name))?;

            let func_ref = translator
                .module
                .declare_func_in_func(*func_id, translator.builder.func);

            let call = translator.builder.ins().call(func_ref, &[base.value]);
            let result = translator.builder.inst_results(call)[0];

            Ok(CompiledValue::new(
                result,
                data_type.unwrap_or(MirDataType::Ref(
                    Box::new(MirDataType::Dynamic),
                    RefMutability::MutRef,
                )),
            ))
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

        let is_enum = if let MirDataType::Struct { identifier, .. } = &value.data_type
            && let Some(lir_type) = translator.registry.types.get(identifier)
        {
            matches!(lir_type, LirType::Enum(_))
        } else {
            false
        };

        if matches!(
            &value.data_type,
            MirDataType::Option(_) | MirDataType::Result { .. }
        ) || is_enum
        {
            let runtime_name = "calibre_enum_discriminant";

            let func_id = translator
                .runtime_functions
                .get(runtime_name)
                .ok_or(format!("{} not found in runtime_functions", runtime_name))?;

            let func_ref = translator
                .module
                .declare_func_in_func(*func_id, translator.builder.func);

            let call = translator.builder.ins().call(func_ref, &[value.value]);
            let result = translator.builder.inst_results(call)[0];

            Ok(CompiledValue::new(result, MirDataType::UInt))
        } else {
            let value = translator.builder.ins().iconst(translator.types.uint(), 0);
            Ok(CompiledValue::new(value, MirDataType::UInt))
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
        let ptr = self
            .value
            .translate(translator, data_type.map(|x| x.unwrap_all_refs().clone()))?;

        if matches!(&ptr.data_type, MirDataType::Ref(_, _)) {
            let runtime_name = "calibre_deref";

            let func_id = translator
                .runtime_functions
                .get(runtime_name)
                .ok_or(format!("{} not found in runtime_functions", runtime_name))?;

            let func_ref = translator
                .module
                .declare_func_in_func(*func_id, translator.builder.func);

            let call = translator.builder.ins().call(func_ref, &[ptr.value]);
            let result = translator.builder.inst_results(call)[0];

            Ok(CompiledValue::new(
                result,
                ptr.data_type.unwrap_all_refs().clone(),
            ))
        } else {
            Ok(ptr)
        }
    }
}
