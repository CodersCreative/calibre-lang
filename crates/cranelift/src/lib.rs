use crate::layout::Layouts;
use crate::natives::{ArgumentKind, runtime_functions};
use crate::translation::FunctionTranslator;
use crate::values::CraneliftTypes;
use calibre_lir::ast::{
    BlockId, LirAssign, LirBlock, LirLValue, LirNode, LirNodeType, LirTerminator,
};
use calibre_lir::environment::{LirFunction, LirRegistry};
use calibre_lir::{FullyQualifiedPath, MirDataType};
use calibre_mir::symbols::VariableKey;
use calibre_parser::Span;
use cranelift::codegen::isa::CallConv;
use cranelift::codegen::print_errors::pretty_verifier_error;
use cranelift::codegen::settings;
use cranelift::codegen::{CodegenError, isa};
use cranelift::frontend::FunctionBuilderContext;
use cranelift::prelude::*;
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{DataDescription, DataId, FuncId, Linkage, Module, ModuleError};
use cranelift_object::{ObjectBuilder, ObjectModule};
use rustc_hash::FxHashMap;
use std::error::Error;
use std::sync::Arc;

pub mod layout;
pub mod natives;
pub mod translation;
pub mod values;

pub type Globals = FxHashMap<VariableKey, (MirDataType, DataId)>;
pub type Functions = FxHashMap<VariableKey, FuncId>;
pub type RuntimeFunctions = FxHashMap<&'static str, FuncId>;

pub struct CompilerData {
    context: codegen::Context,
    pub registry: LirRegistry,
    builder_context: FunctionBuilderContext,
    types: CraneliftTypes,
    functions: Functions,
    globals: Globals,
    layouts: Layouts,
    runtime_functions: RuntimeFunctions,
}

fn create_target_isa() -> Result<Arc<dyn isa::TargetIsa>, Box<dyn Error>> {
    let mut flag_builder = settings::builder();
    flag_builder.set("use_colocated_libcalls", "false")?;
    flag_builder.set("is_pic", "false")?;
    flag_builder.set("enable_verifier", "true")?;

    let isa_builder = cranelift_native::builder()
        .map_err(|msg| format!("host machine is not supported: {msg}"))?;
    let isa = isa_builder.finish(settings::Flags::new(flag_builder))?;

    Ok(isa)
}

impl CompilerData {
    fn compile<M: Module>(&mut self, module: &mut M) -> Result<(), Box<dyn Error>> {
        for (name, sig) in runtime_functions() {
            let mut signature = module.make_signature();
            signature.call_conv = CallConv::SystemV;

            for (ty, arg_kind) in &sig.params {
                match arg_kind {
                    ArgumentKind::Direct => {
                        signature.params.push(AbiParam::new(*ty));
                    }
                    ArgumentKind::TagPayload => {
                        signature.params.push(AbiParam::new(*ty));
                        signature.params.push(AbiParam::new(*ty));
                    }
                }
            }

            if let Some(ty) = &sig.returns {
                signature.returns.push(AbiParam::new(*ty));
            }

            let func_id = module.declare_function(*name, Linkage::Import, &signature)?;
            self.runtime_functions.insert(*name, func_id);
        }

        let mut blocks: Vec<Option<LirBlock>> = Vec::new();
        let mut block_id = 0;

        for (key, global) in &self.registry.globals {
            let mut transformed_blocks = Vec::with_capacity(global.blocks.len());

            for block in global.blocks.iter().flatten() {
                let mut block = block.clone();
                block.id = BlockId(block_id);
                block_id += 1;

                block.instructions = block
                    .instructions
                    .into_iter()
                    .map(|instr| match instr.node_type {
                        LirNodeType::Declare(decl) if decl.dest == *key => LirNode::new(
                            instr.span,
                            LirNodeType::Assign(LirAssign {
                                dest: LirLValue::Var(decl.dest.clone()),
                                value: decl.value.clone(),
                            }),
                        ),
                        _ => instr,
                    })
                    .collect();

                blocks.push(Some(block.clone()));
                transformed_blocks.push(Some(block));
            }
        }

        if !blocks.is_empty() {
            if let Some(Some(block)) = blocks.last_mut() {
                block.terminator = Some(LirTerminator::Return {
                    span: Span::default(),
                    value: None,
                });
            }

            let name = VariableKey {
                fully_qualified_path: FullyQualifiedPath::combine(
                    None,
                    ustr::Ustr::from("__init_globals"),
                ),
                shadow_counter: None,
            };

            let func = LirFunction {
                name: name.clone(),
                params: Box::default(),
                captures: Box::default(),
                return_type: MirDataType::Null,
                blocks: blocks.into_boxed_slice(),
                pure: false,
                memo: false,
                referenced_params: 0,
                memo_params: 0,
            };

            self.registry.functions.insert(name, func);
        }

        for (key, global) in &self.registry.globals {
            let data_id = module.declare_data(&key.to_string(), Linkage::Export, true, false)?;

            self.globals
                .insert(key.clone(), (global.data_type.clone(), data_id));

            if let Some(layout) = self.layouts.get_layout(&global.data_type) {
                let mut data = DataDescription::new();
                data.define_zeroinit(layout.size as usize);
                module.define_data(data_id, &data)?;
            }
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
                    &self.runtime_functions,
                    func.return_type.clone(),
                );

                translator.translate_function(func)?;
            }

            match module.define_function(function, &mut self.context) {
                Ok(_) => {}
                Err(ModuleError::Compilation(CodegenError::Verifier(verifier_errors))) => {
                    let formatted_error =
                        pretty_verifier_error(&self.context.func, None, verifier_errors);
                    return Err(format!("Verifier error:\n{}", formatted_error).into());
                }
                Err(x) => return Err(Box::new(x)),
            };

            module.clear_context(&mut self.context);
        }

        Ok(())
    }
}

pub struct JitCompiler {
    module: JITModule,
    pub data: CompilerData,
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
            runtime_functions: FxHashMap::default(),
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
            runtime_functions: FxHashMap::default(),
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
