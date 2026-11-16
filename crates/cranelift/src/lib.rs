use crate::layout::Layouts;
use crate::natives::{ArgumentKind, runtime_functions};
use crate::translation::FunctionTranslator;
use crate::values::{CompiledValue, CraneliftTypes};
use calibre_lir::MirDataType;
use calibre_lir::ast::{
    BlockId, LirAssign, LirBlock, LirLValue, LirNode, LirNodeType, LirTerminator,
};
use calibre_lir::environment::{LirFunction, LirRegistry};
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
    function_ids: FxHashMap<VariableKey, FuncId>,
    counter: i64,
    isa: Arc<dyn isa::TargetIsa>,
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
    fn compile<M: Module>(
        &mut self,
        module: &mut M,
        init_functions: Vec<(i32, VariableKey)>,
        fin_functions: Vec<(i32, VariableKey)>,
    ) -> Result<(), Box<dyn Error>> {
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

            let name = VariableKey::from("calibre_init_globals");

            let func = LirFunction {
                name: name.clone(),
                params: Box::default(),
                captures: Box::default(),
                return_type: MirDataType::Null,
                blocks: blocks.into_boxed_slice(),
                pure: false,
                memo: false,
                referenced_params: 0,
                captured_params: 0,
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

        for (index, (key, func)) in self.registry.functions.iter().enumerate() {
            let mut signature = module.make_signature();

            if !func.captures.is_empty() {
                signature.params.push(AbiParam::new(types::I64));
            }

            for param in func.params.iter() {
                signature.params.push(AbiParam::new(
                    self.types.to_cranelift_type(&param.data_type),
                ));
            }

            if !func.return_type.is_null() && func.return_type != MirDataType::Never {
                signature.returns.push(AbiParam::new(
                    self.types.to_cranelift_type(&func.return_type),
                ));
            }

            let symbol_name = format!("calibre_fn_{}_{}", index, key);
            let function = module.declare_function(&symbol_name, Linkage::Export, &signature)?;
            self.functions.insert(key.clone(), function);
        }

        let mut closure_signature = module.make_signature();
        closure_signature.call_conv = CallConv::SystemV;
        closure_signature.params.push(AbiParam::new(types::I64));
        closure_signature.params.push(AbiParam::new(types::I64));
        closure_signature.returns.push(AbiParam::new(types::I64));

        for (index, key) in self.registry.functions.keys().enumerate() {
            let entry_name = format!("calibre_closure_{}_{}", index, key);
            let entry = module.declare_function(&entry_name, Linkage::Local, &closure_signature)?;
            self.function_ids.insert(key.clone(), entry);
        }

        for (key, func) in self.registry.functions.clone() {
            let Some(&function) = self.functions.get(&key) else {
                continue;
            };

            self.context.func.signature.params.clear();
            self.context.func.signature.returns.clear();

            if !func.captures.is_empty() {
                self.context
                    .func
                    .signature
                    .params
                    .push(AbiParam::new(types::I64));
            }

            for param in func.params.iter() {
                self.context.func.signature.params.push(AbiParam::new(
                    self.types.to_cranelift_type(&param.data_type),
                ));
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
                    &mut self.counter,
                    func.return_type.clone(),
                    self.isa.clone(),
                    &self.function_ids,
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

        for (key, func) in self.registry.functions.clone() {
            let Some(&entry_id) = self.function_ids.get(&key) else {
                continue;
            };
            let Some(&real_id) = self.functions.get(&key) else {
                continue;
            };

            self.context.func.signature.params.clear();
            self.context.func.signature.returns.clear();

            self.context
                .func
                .signature
                .params
                .push(AbiParam::new(types::I64));

            self.context
                .func
                .signature
                .params
                .push(AbiParam::new(types::I64));

            self.context
                .func
                .signature
                .returns
                .push(AbiParam::new(types::I64));

            self.context.func.signature.call_conv = CallConv::SystemV;

            let func_ref = module.declare_func_in_func(real_id, &mut self.context.func);

            {
                let builder =
                    FunctionBuilder::new(&mut self.context.func, &mut self.builder_context);

                let mut translator = FunctionTranslator::new(
                    builder,
                    module,
                    &self.registry,
                    &self.types,
                    &mut self.layouts,
                    &self.globals,
                    &self.functions,
                    &self.runtime_functions,
                    &mut self.counter,
                    func.return_type.clone(),
                    self.isa.clone(),
                    &self.function_ids,
                );

                let entry_block = translator.builder.create_block();

                translator
                    .builder
                    .append_block_params_for_function_params(entry_block);

                translator.builder.switch_to_block(entry_block);

                let block_params = translator.builder.block_params(entry_block).to_vec();

                let env = block_params
                    .first()
                    .copied()
                    .unwrap_or_else(|| translator.int_const(0));

                let args = block_params.get(1).copied().unwrap_or_else(|| env);

                let mut call_args = Vec::with_capacity(func.params.len() + 1);
                if !func.captures.is_empty() {
                    call_args.push(env);
                }

                for (i, param) in func.params.iter().enumerate() {
                    let index = translator.int_const(i as i64);
                    let payload = translator
                        .runtime_call("calibre_aggregate_get_field_and_deref", &[args, index])?;

                    call_args.push(translator.convert_from_i64(payload, &param.data_type));
                }

                let call = translator.builder.ins().call(func_ref, &call_args);

                let payload =
                    if func.return_type.is_null() || func.return_type == MirDataType::Never {
                        translator.int_const(0)
                    } else {
                        let results = translator.builder.inst_results(call);
                        let result = results
                            .first()
                            .copied()
                            .ok_or("expected return value from function entry".to_string())?;
                        translator
                            .convert_to_i64(CompiledValue::new(result, func.return_type.clone()))?
                    };

                translator.builder.ins().return_(&[payload]);
                translator.builder.seal_all_blocks();

                let frontend_config = translator.isa.frontend_config();
                translator.builder.finalize(frontend_config);
            }

            match module.define_function(entry_id, &mut self.context) {
                Ok(_) => {}
                Err(ModuleError::Compilation(CodegenError::Verifier(verifier_errors))) => {
                    let error = pretty_verifier_error(&self.context.func, None, verifier_errors);

                    return Err(format!("Verifier error in entry {}:\n{}", key, error).into());
                }
                Err(x) => return Err(Box::new(x)),
            };

            module.clear_context(&mut self.context);
        }

        let mut start_signature = module.make_signature();
        start_signature.call_conv = CallConv::SystemV;
        let start_func_id = module.declare_function("_start", Linkage::Export, &start_signature)?;

        let start_name = VariableKey::from("_start");
        self.functions.insert(start_name.clone(), start_func_id);

        self.context.func.signature.params.clear();
        self.context.func.signature.returns.clear();

        {
            let mut builder =
                FunctionBuilder::new(&mut self.context.func, &mut self.builder_context);

            let entry_block = builder.create_block();
            builder.append_block_params_for_function_params(entry_block);
            builder.switch_to_block(entry_block);

            let init_globals_key = VariableKey::from("calibre_init_globals");

            let mut startup_functions = vec![init_globals_key];
            startup_functions.extend(init_functions.into_iter().map(|x| x.1));
            startup_functions.extend(fin_functions.into_iter().map(|x| x.1));

            for func_key in startup_functions {
                if let Some(&func_id) = self.functions.get(&func_key) {
                    let func_ref = module.declare_func_in_func(func_id, builder.func);

                    let args = if self
                        .registry
                        .functions
                        .get(&func_key)
                        .is_some_and(|f| !f.captures.is_empty())
                    {
                        vec![builder.ins().iconst(types::I64, 0)]
                    } else {
                        Vec::new()
                    };

                    builder.ins().call(func_ref, &args);
                }
            }

            builder.ins().return_(&[]);
        }

        match module.define_function(start_func_id, &mut self.context) {
            Ok(_) => {}
            Err(ModuleError::Compilation(CodegenError::Verifier(verifier_errors))) => {
                let formatted_error =
                    pretty_verifier_error(&self.context.func, None, verifier_errors);
                return Err(format!("Verifier error in _start:\n{}", formatted_error).into());
            }
            Err(x) => return Err(Box::new(x)),
        };

        module.clear_context(&mut self.context);

        Ok(())
    }
}

pub struct JitCompiler {
    module: JITModule,
    pub data: CompilerData,
}

impl JitCompiler {
    pub fn new(
        registry: LirRegistry,
        init_functions: Vec<(i32, VariableKey)>,
        fin_functions: Vec<(i32, VariableKey)>,
        args: Vec<String>,
    ) -> Result<Self, Box<dyn Error>> {
        let isa = create_target_isa()?;
        let mut builder =
            JITBuilder::with_isa(isa.clone(), cranelift_module::default_libcall_names());

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
            function_ids: FxHashMap::default(),
            counter: 1,
            isa,
        };

        if let Ok(mut x) = calibre_runtime::native::env::PROGRAM_ARGS.lock() {
            *x = args;
        }

        data.compile(&mut module, init_functions, fin_functions)?;

        module.finalize_definitions()?;

        Ok(Self { module, data })
    }

    pub fn get_function(&self, key: &VariableKey) -> Option<*const u8> {
        let func_id = self.data.functions.get(key)?;
        Some(self.module.get_finalized_function(*func_id))
    }
}

#[allow(unused)]
pub struct AotCompiler {
    module: ObjectModule,
    data: CompilerData,
}

impl AotCompiler {
    pub fn new(
        module_name: &str,
        registry: LirRegistry,
        init_functions: Vec<(i32, VariableKey)>,
        fin_functions: Vec<(i32, VariableKey)>,
    ) -> Result<Self, Box<dyn Error>> {
        let isa = create_target_isa()?;

        let builder = ObjectBuilder::new(
            isa.clone(),
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
            function_ids: FxHashMap::default(),
            counter: 1,
            isa,
        };

        data.compile(&mut module, init_functions, fin_functions)?;

        Ok(Self { module, data })
    }

    pub fn finish(self) -> Result<Vec<u8>, Box<dyn Error>> {
        let product = self.module.finish();
        let bytes = product.emit()?;
        Ok(bytes)
    }
}
