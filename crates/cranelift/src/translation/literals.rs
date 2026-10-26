use crate::{
    translation::{CraneliftTranslation, FunctionTranslator},
    values::CompiledValue,
};
use calibre_lir::{
    MirDataType,
    ast::{LirAggregate, LirLiteral},
};
use cranelift::codegen::ir::{InstBuilder, MemFlags};
use cranelift_module::Module;
use std::error::Error;

impl CraneliftTranslation for LirLiteral {
    fn translate<'a, M: Module>(
        self,
        translator: &mut FunctionTranslator<'a, M>,
        _data_type: Option<MirDataType>,
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

                let string = translator.builder.ins().call(alloc_func_ref, &[len]);
                let string = translator.builder.inst_results(string)[0];

                let ptr = translator.builder.ins().iadd_imm(string, 8);

                let data_ptr =
                    translator
                        .builder
                        .ins()
                        .load(translator.types.ptr(), MemFlags::new(), ptr, 16);

                for (i, &byte) in s.as_bytes().iter().enumerate() {
                    let value = translator
                        .builder
                        .ins()
                        .iconst(translator.types.byte(), byte as i64);

                    translator
                        .builder
                        .ins()
                        .store(MemFlags::new(), value, data_ptr, i as i32);
                }

                let root_func_id = translator
                    .runtime_functions
                    .get("calibre_gc_add_root")
                    .ok_or("calibre_gc_add_root not found in runtime_functions")?;

                let root_func_ref = translator
                    .module
                    .declare_func_in_func(*root_func_id, translator.builder.func);

                translator.builder.ins().call(root_func_ref, &[string]);

                Ok(CompiledValue::new(ptr, MirDataType::Str))
            }
            LirLiteral::Null => {
                let value = translator.builder.ins().iconst(translator.types.int(), 0);

                Ok(CompiledValue::new(value, MirDataType::Null))
            }
            _ => {
                let value = translator.builder.ins().iconst(translator.types.int(), 0);

                Ok(CompiledValue::new(value, MirDataType::Int))
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

        let aggregate_ptr = translator
            .builder
            .ins()
            .call(alloc_func_ref, &[field_count]);
        let aggregate_ptr = translator.builder.inst_results(aggregate_ptr)[0];

        let ptr = translator.builder.ins().iadd_imm(aggregate_ptr, 8);

        let set_field_func_id = translator
            .runtime_functions
            .get("calibre_aggregate_set_field")
            .ok_or("calibre_aggregate_set_field not found in runtime_functions")?;

        let set_field_func_ref = translator
            .module
            .declare_func_in_func(*set_field_func_id, translator.builder.func);

        self.fields.sort_by_key(|(k, _)| *k);

        for (i, (_, value)) in std::mem::take(&mut self.fields.0).into_iter().enumerate() {
            let field = value.translate(translator, None)?;

            let tag = translator
                .builder
                .ins()
                .iconst(translator.types.int(), field.data_type.type_tag() as i64);
            let index = translator
                .builder
                .ins()
                .iconst(translator.types.int(), i as i64);

            translator
                .builder
                .ins()
                .call(set_field_func_ref, &[ptr, index, tag, field.value]);
        }

        let root_func_id = translator
            .runtime_functions
            .get("calibre_gc_add_root")
            .ok_or("calibre_gc_add_root not found in runtime_functions")?;

        let root_func_ref = translator
            .module
            .declare_func_in_func(*root_func_id, translator.builder.func);

        translator
            .builder
            .ins()
            .call(root_func_ref, &[aggregate_ptr]);

        let data_type = if let Some(name) = self.name {
            MirDataType::Struct {
                identifier: name,
                generic_types: Vec::new(),
            }
        } else {
            MirDataType::Tuple(Vec::new())
        };

        Ok(CompiledValue::new(ptr, data_type))
    }
}
