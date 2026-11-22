use crate::{
    translation::{CraneliftTranslation, FunctionTranslator},
    values::CompiledValue,
};
use calibre_lir::{
    MirDataType, TypeKey,
    ast::{LirAggregate, LirEnum, LirList, LirLiteral, LirRange},
    environment::LirType,
};
use cranelift::codegen::{
    FxHashMap,
    ir::{InstBuilder, MachMemFlags, types},
};
use cranelift_module::Module;
use std::error::Error;

impl CraneliftTranslation for LirLiteral {
    fn translate<'a, M: Module>(
        self,
        translator: &mut FunctionTranslator<'a, M>,
        data_type: Option<MirDataType>,
    ) -> Result<CompiledValue, Box<dyn Error>> {
        match self {
            LirLiteral::Int(i) => {
                let value = translator
                    .builder
                    .ins()
                    .iconst(translator.types.int(), i as i64);

                Ok(CompiledValue::new(value, MirDataType::Int))
            }
            LirLiteral::UInt(u) => {
                let value = translator
                    .builder
                    .ins()
                    .iconst(translator.types.int(), u as i64);

                Ok(CompiledValue::new(value, MirDataType::UInt))
            }
            LirLiteral::Byte(b) => {
                let value = translator
                    .builder
                    .ins()
                    .iconst(translator.types.byte(), b as i64);

                Ok(CompiledValue::new(value, MirDataType::Byte))
            }
            LirLiteral::Float(f) => {
                let value = translator.builder.ins().f64const(f);

                Ok(CompiledValue::new(value, MirDataType::Float))
            }
            LirLiteral::Bool(b) => {
                let value = translator
                    .builder
                    .ins()
                    .iconst(translator.types.bool(), if b { 1 } else { 0 });

                Ok(CompiledValue::new(value, MirDataType::Bool))
            }
            LirLiteral::Char(c) => {
                let value = translator
                    .builder
                    .ins()
                    .iconst(translator.types.char(), c as u32 as i64);

                Ok(CompiledValue::new(value, MirDataType::Char))
            }
            LirLiteral::String(s) => {
                let len = translator
                    .builder
                    .ins()
                    .iconst(translator.types.int(), s.len() as i64);

                let alloc_func_id = translator
                    .runtime_functions
                    .get("calibre_gc_alloc_string")
                    .ok_or("calibre_gc_alloc_string not found in runtime_functions")?;

                let alloc_func_ref = translator
                    .module
                    .declare_func_in_func(*alloc_func_id, translator.builder.func);

                let ptr = translator.builder.ins().call(alloc_func_ref, &[len]);
                let ptr = translator.builder.inst_results(ptr)[0];

                let data_ptr = translator.builder.ins().load(
                    translator.types.ptr(),
                    MachMemFlags::new(),
                    ptr,
                    translator.types.pointer_bytes() as i32,
                );

                for (i, &byte) in s.as_bytes().iter().enumerate() {
                    let value = translator
                        .builder
                        .ins()
                        .iconst(translator.types.byte(), byte as i64);

                    translator
                        .builder
                        .ins()
                        .store(MachMemFlags::new(), value, data_ptr, i as i32);
                }

                Ok(CompiledValue::new(ptr, MirDataType::Str))
            }
            LirLiteral::Null => {
                let data_type = data_type.unwrap_or(MirDataType::Null);
                let ty = translator.types.to_cranelift_type(&data_type);

                let value = if ty == types::F64 {
                    translator.builder.ins().f64const(0.0)
                } else {
                    translator.builder.ins().iconst(ty, 0)
                };

                Ok(CompiledValue::new(value, data_type))
            }
            LirLiteral::Big(x) => {
                if let Ok(x) = x.parse::<f64>() {
                    let value = translator.builder.ins().f64const(x);

                    let alloc_func_id = translator
                        .runtime_functions
                        .get("calibre_gc_alloc_big_from_float")
                        .ok_or("calibre_gc_alloc_big_from_float not found in runtime_functions")?;

                    let alloc_func_ref = translator
                        .module
                        .declare_func_in_func(*alloc_func_id, translator.builder.func);

                    let ptr = translator.builder.ins().call(alloc_func_ref, &[value]);
                    let ptr = translator.builder.inst_results(ptr)[0];

                    Ok(CompiledValue::new(ptr, MirDataType::Big))
                } else {
                    let txt = LirLiteral::String(x).translate(translator, data_type)?;

                    let alloc_func_id = translator
                        .runtime_functions
                        .get("calibre_gc_alloc_big")
                        .ok_or("calibre_gc_alloc_big not found in runtime_functions")?;

                    let alloc_func_ref = translator
                        .module
                        .declare_func_in_func(*alloc_func_id, translator.builder.func);

                    let ptr = translator.builder.ins().call(alloc_func_ref, &[txt.value]);
                    let ptr = translator.builder.inst_results(ptr)[0];

                    Ok(CompiledValue::new(ptr, MirDataType::Big))
                }
            }
        }
    }
}

impl CraneliftTranslation for LirAggregate {
    fn translate<'a, M: Module>(
        mut self,
        translator: &mut FunctionTranslator<'a, M>,
        _data_type: Option<MirDataType>,
    ) -> Result<CompiledValue, Box<dyn Error>> {
        let field_count = translator
            .builder
            .ins()
            .iconst(translator.types.int(), self.fields.len() as i64);

        let alloc_func_id = translator
            .runtime_functions
            .get("calibre_gc_alloc_aggregate")
            .ok_or("calibre_gc_alloc_aggregate not found in runtime_functions")?;

        let alloc_func_ref = translator
            .module
            .declare_func_in_func(*alloc_func_id, translator.builder.func);

        let ptr = translator
            .builder
            .ins()
            .call(alloc_func_ref, &[field_count]);
        let ptr = translator.builder.inst_results(ptr)[0];

        let set_field_func_id = translator
            .runtime_functions
            .get("calibre_aggregate_set_field")
            .ok_or("calibre_aggregate_set_field not found in runtime_functions")?;

        let set_field_func_ref = translator
            .module
            .declare_func_in_func(*set_field_func_id, translator.builder.func);

        let order = self
            .name
            .as_ref()
            .and_then(|name| match translator.registry.types.get(name) {
                Some(LirType::Struct(fields)) => Some(
                    fields
                        .0
                        .iter()
                        .enumerate()
                        .map(|(i, (field, _))| (*field, i))
                        .collect::<FxHashMap<_, _>>(),
                ),
                _ => None,
            });

        if let Some(order) = order {
            self.fields
                .0
                .sort_by_key(|(name, _)| order.get(name).copied().unwrap_or(usize::MAX));
        } else {
            self.fields
                .0
                .sort_by_key(|(name, _)| name.as_str().parse::<usize>().unwrap_or(usize::MAX));
        }

        let mut field_types = Vec::new();

        for (i, (_, value)) in std::mem::take(&mut self.fields.0).into_iter().enumerate() {
            let field = value.translate(translator, None)?;

            let tag =
                translator.int_const(field.data_type.type_tag(translator.registry.enums()) as i64);

            let index = translator
                .builder
                .ins()
                .iconst(translator.types.int(), i as i64);

            let field_data_type = field.data_type.clone();
            let payload = translator.convert_to_i64(field)?;

            translator
                .builder
                .ins()
                .call(set_field_func_ref, &[ptr, index, tag, payload]);

            field_types.push(field_data_type);
        }

        let data_type = if let Some(name) = self.name {
            MirDataType::Struct {
                identifier: name,
                generic_types: Vec::new(),
            }
        } else {
            MirDataType::Tuple(field_types)
        };

        translator.set_type_for_ptr(ptr, &data_type)?;

        Ok(CompiledValue::new(ptr, data_type))
    }
}

impl CraneliftTranslation for LirEnum {
    fn translate<'a, M: Module>(
        self,
        translator: &mut FunctionTranslator<'a, M>,
        _data_type: Option<MirDataType>,
    ) -> Result<CompiledValue, Box<dyn Error>> {
        let variant = translator
            .builder
            .ins()
            .iconst(translator.types.int(), self.variant as i64);

        let (tag, payload) = if let Some(payload) = self.payload {
            let compiled = payload.translate(translator, None)?;

            let tag = translator
                .int_const(compiled.data_type.type_tag(translator.registry.enums()) as i64);

            let payload = translator.convert_to_i64(compiled)?;

            (tag, payload)
        } else {
            let tag = translator.int_const(0);
            let payload = translator.int_const(0);

            (tag, payload)
        };

        let alloc_func_id = translator
            .runtime_functions
            .get("calibre_gc_alloc_enum")
            .ok_or("calibre_gc_alloc_enum not found in runtime_functions")?;

        let alloc_func_ref = translator
            .module
            .declare_func_in_func(*alloc_func_id, translator.builder.func);

        let ptr = translator
            .builder
            .ins()
            .call(alloc_func_ref, &[variant, tag, payload]);
        let ptr = translator.builder.inst_results(ptr)[0];

        let data_type = if let Some(name) = self.name {
            MirDataType::Struct {
                identifier: name,
                generic_types: Vec::new(),
            }
        } else {
            MirDataType::Struct {
                identifier: TypeKey::from("unknown"),
                generic_types: Vec::new(),
            }
        };

        translator.set_type_for_ptr(ptr, &data_type)?;

        Ok(CompiledValue::new(ptr, data_type))
    }
}

impl CraneliftTranslation for LirList {
    fn translate<'a, M: Module>(
        mut self,
        translator: &mut FunctionTranslator<'a, M>,
        _data_type: Option<MirDataType>,
    ) -> Result<CompiledValue, Box<dyn Error>> {
        let len = translator
            .builder
            .ins()
            .iconst(translator.types.int(), self.values.len() as i64);

        let alloc_func_id = translator
            .runtime_functions
            .get("calibre_gc_alloc_list")
            .ok_or("calibre_gc_alloc_list not found in runtime_functions")?;

        let alloc_func_ref = translator
            .module
            .declare_func_in_func(*alloc_func_id, translator.builder.func);

        let ptr = translator.builder.ins().call(alloc_func_ref, &[len]);
        let ptr = translator.builder.inst_results(ptr)[0];

        let data_type = MirDataType::List(Box::new(self.data_type.clone()));
        translator.set_type_for_ptr(ptr, &data_type)?;

        let push_func_id = translator
            .runtime_functions
            .get("calibre_list_push")
            .ok_or("calibre_list_push not found in runtime_functions")?;

        let push_func_ref = translator
            .module
            .declare_func_in_func(*push_func_id, translator.builder.func);

        for value in std::mem::take(&mut self.values) {
            let value = value.translate(translator, Some(self.data_type.clone()))?;

            let tag =
                translator.int_const(value.data_type.type_tag(translator.registry.enums()) as i64);

            let payload = translator.convert_to_i64(value)?;

            translator
                .builder
                .ins()
                .call(push_func_ref, &[ptr, tag, payload]);
        }

        Ok(CompiledValue::new(ptr, data_type))
    }
}

impl CraneliftTranslation for LirRange {
    fn translate<'a, M: Module>(
        self,
        translator: &mut FunctionTranslator<'a, M>,
        _data_type: Option<MirDataType>,
    ) -> Result<CompiledValue, Box<dyn Error>> {
        let from = (*self.from).translate(translator, Some(MirDataType::Int))?;
        let to = (*self.to).translate(translator, Some(MirDataType::Int))?;

        let from = translator.convert_to_i64(from)?;

        let to = translator.convert_to_i64(to)?;
        let to = translator.builder.ins().ireduce(types::I32, to);
        let to = translator.builder.ins().sextend(translator.types.int(), to);
        let to = translator.builder.ins().ishl_imm_u(to, 32);

        let value = translator.builder.ins().iadd(from, to);
        Ok(CompiledValue::new(value, MirDataType::Range))
    }
}
