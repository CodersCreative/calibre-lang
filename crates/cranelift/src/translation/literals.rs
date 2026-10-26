use crate::{
    translation::{CraneliftTranslation, FunctionTranslator},
    values::CompiledValue,
};
use calibre_lir::{MirDataType, ast::LirLiteral};
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
