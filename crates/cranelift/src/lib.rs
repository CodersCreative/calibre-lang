use crate::layout::Layouts;
use crate::translation::FunctionTranslator;
use crate::values::CraneliftTypes;
use calibre_lir::MirDataType;
use calibre_lir::environment::LirRegistry;
use calibre_mir::symbols::VariableKey;
use cranelift::codegen::isa::CallConv;
use cranelift::frontend::FunctionBuilderContext;
use cranelift::prelude::*;
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{DataDescription, DataId, FuncId, Linkage, Module};
use cranelift_object::{ObjectBuilder, ObjectModule};
use rustc_hash::FxHashMap;
use std::error::Error;
use std::sync::Arc;

pub mod layout;
pub mod translation;
pub mod values;

pub type Globals = FxHashMap<VariableKey, (MirDataType, DataId)>;
pub type Functions = FxHashMap<VariableKey, FuncId>;

pub struct CompilerData {
    context: codegen::Context,
    registry: LirRegistry,
    builder_context: FunctionBuilderContext,
    types: CraneliftTypes,
    functions: Functions,
    globals: Globals,
    layouts: Layouts,
}

fn create_target_isa() -> Result<Arc<dyn isa::TargetIsa>, Box<dyn Error>> {
    let mut flag_builder = settings::builder();
    flag_builder.set("use_colocated_libcalls", "false")?;
    flag_builder.set("is_pic", "false")?;

    let isa_builder = cranelift_native::builder()
        .map_err(|msg| format!("host machine is not supported: {msg}"))?;
    let isa = isa_builder.finish(settings::Flags::new(flag_builder))?;

    Ok(isa)
}

impl CompilerData {
    fn compile<M: Module>(&mut self, module: &mut M) -> Result<(), Box<dyn Error>> {
        for symbol in calibre_runtime::get_runtime_symbols() {
            let mut sig = module.make_signature();
            sig.call_conv = CallConv::SystemV;

            let _ = module.declare_function(symbol.name, Linkage::Import, &sig)?;
        }

        for (key, global) in &self.registry.globals {
            let data_id =
                module.declare_data(&key.to_string(), Linkage::Export, global.mutable, false)?;

            self.globals
                .insert(key.clone(), (global.data_type.clone(), data_id));

            let mut data = DataDescription::new();
            data.define_zeroinit(8);
            module.define_data(data_id, &data)?;
        }

        for (key, func) in &self.registry.functions {
            let mut signature = module.make_signature();

            for (_, param_ty) in func.params.iter() {
                signature
                    .params
                    .push(AbiParam::new(self.types.to_cranelift_type(param_ty)));
            }

            if !func.return_type.is_null() && func.return_type != MirDataType::Never {
                signature.returns.push(AbiParam::new(
                    self.types.to_cranelift_type(&func.return_type),
                ));
            }

            let symbol_name = key.to_string();
            let function = module.declare_function(&symbol_name, Linkage::Export, &signature)?;
            self.functions.insert(key.clone(), function);
        }

        for (key, func) in self.registry.functions.clone() {
            let Some(&function) = self.functions.get(&key) else {
                continue;
            };

            self.context.func.signature.params.clear();
            self.context.func.signature.returns.clear();

            for (_, param_ty) in func.params.iter() {
                self.context
                    .func
                    .signature
                    .params
                    .push(AbiParam::new(self.types.to_cranelift_type(param_ty)));
            }

            if !func.return_type.is_null() && func.return_type != MirDataType::Never {
                self.context.func.signature.returns.push(AbiParam::new(
                    self.types.to_cranelift_type(&func.return_type),
                ));
            }

            {
                let builder =
                    FunctionBuilder::new(&mut self.context.func, &mut self.builder_context);

                let translator = FunctionTranslator::new(
                    builder,
                    module,
                    &self.registry,
                    &self.types,
                    &mut self.layouts,
                    &self.globals,
                    &self.functions,
                );

                translator.translate_function(func)?;
            }

            module.define_function(function, &mut self.context)?;
            module.clear_context(&mut self.context);
        }

        Ok(())
    }
}

pub struct JitCompiler {
    module: JITModule,
    data: CompilerData,
}

impl JitCompiler {
    pub fn new(registry: LirRegistry) -> Result<Self, Box<dyn Error>> {
        let isa = create_target_isa()?;
        let mut builder = JITBuilder::with_isa(isa, cranelift_module::default_libcall_names());

        for sym in calibre_runtime::get_runtime_symbols() {
            builder.symbol(sym.name, sym.ptr);
        }

        let mut module = JITModule::new(builder);

        let ptr_type = module.target_config().pointer_type();
        let layouts = Layouts::new(Some(&registry), ptr_type.bits());

        let ctx = module.make_context();

        let mut data = CompilerData {
            context: ctx,
            builder_context: FunctionBuilderContext::new(),
            types: CraneliftTypes::new(ptr_type),
            functions: FxHashMap::default(),
            globals: FxHashMap::default(),
            layouts,
            registry,
        };

        data.compile(&mut module)?;

        module.finalize_definitions()?;

        Ok(Self { module, data })
    }

    pub fn get_function(&self, key: &VariableKey) -> Option<*const u8> {
        let func_id = self.data.functions.get(key)?;
        Some(self.module.get_finalized_function(*func_id))
    }
}

pub struct AotCompiler {
    module: ObjectModule,
    data: CompilerData,
}

impl AotCompiler {
    pub fn new(module_name: &str, registry: LirRegistry) -> Result<Self, Box<dyn Error>> {
        let isa = create_target_isa()?;

        let builder = ObjectBuilder::new(
            isa,
            module_name.to_string(),
            cranelift_module::default_libcall_names(),
        )?;

        let mut module = ObjectModule::new(builder);

        let ptr_type = module.target_config().pointer_type();
        let layouts = Layouts::new(Some(&registry), ptr_type.bits());

        let context = module.make_context();

        let mut data = CompilerData {
            context,
            builder_context: FunctionBuilderContext::new(),
            types: CraneliftTypes::new(ptr_type),
            functions: FxHashMap::default(),
            globals: FxHashMap::default(),
            layouts,
            registry,
        };

        data.compile(&mut module)?;

        Ok(Self { module, data })
    }

    pub fn finish(self) -> Result<Vec<u8>, Box<dyn Error>> {
        let product = self.module.finish();
        let bytes = product.emit()?;
        Ok(bytes)
    }
}
