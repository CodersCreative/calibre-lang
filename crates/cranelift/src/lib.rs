use crate::natives::{ArgumentKind, runtime_functions};
use crate::translation::FunctionTranslator;
use crate::values::{CompiledValue, CraneliftTypes};
use calibre_lir::MirDataType;
use calibre_lir::ast::{
    BlockId, LirAssign, LirBlock, LirExtern, LirLValue, LirNode, LirNodeType, LirTerminator,
};
use calibre_lir::environment::{LirFunction, LirRegistry};
use calibre_mir::symbols::VariableKey;
use calibre_parser::Span;
use cranelift::codegen::ir::MachMemFlags;
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

pub mod natives;
pub mod translation;
pub mod values;

#[derive(Clone, Copy, Debug, Default)]
pub struct CraneliftConfig {
    pub max_per_thread: Option<u64>,
    pub stack_size: Option<u64>,
}

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
    runtime_functions: RuntimeFunctions,
    function_ids: FxHashMap<VariableKey, FuncId>,
    extern_functions: FxHashMap<VariableKey, (LirExtern, Vec<MirDataType>, MirDataType)>,
    counter: i64,
    isa: Arc<dyn isa::TargetIsa>,
    source_dir: Option<String>,
    config: CraneliftConfig,
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
        if let Ok(mut dir) = calibre_runtime::native::env::SOURCE_DIR.lock() {
            *dir = self.source_dir.clone();
        }

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

        let extern_globals: Vec<VariableKey> = self
            .registry
            .globals
            .iter()
            .filter(|(_, global)| global.detect_extern().is_some())
            .map(|(key, _)| key.clone())
            .collect();

        for key in extern_globals {
            let Some(global) = self.registry.globals.remove(&key) else {
                continue;
            };

            let Some((func, ty)) = global.detect_extern() else {
                continue;
            };

            let (params, ret) = ty.signature();

            self.registry.functions.insert(
                key.clone(),
                LirFunction {
                    name: key.clone(),
                    params: Box::default(),
                    captures: Box::default(),
                    return_type: ret.clone(),
                    blocks: global.blocks,
                    pure: func.pure,
                    memo: func.memo,
                    referenced_params: 0,
                    captured_params: 0,
                    memo_params: 0,
                },
            );

            self.extern_functions.insert(key, (func, params, ret));
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

            let mut data = DataDescription::new();
            data.define_zeroinit(8);
            module.define_data(data_id, &data)?;
        }

        for (index, (key, func)) in self.registry.functions.iter().enumerate() {
            let mut signature = module.make_signature();

            if let Some((_, params, return_type)) = self.extern_functions.get(key) {
                for param in params.iter() {
                    signature
                        .params
                        .push(AbiParam::new(self.types.to_cranelift_type(param)));
                }

                if !return_type.is_null() && func.return_type != MirDataType::Never {
                    signature
                        .returns
                        .push(AbiParam::new(self.types.to_cranelift_type(return_type)));
                }
            } else {
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

        for (key, func) in &self.registry.functions {
            if let Some((extern_fn, ty)) = func.detect_extern() {
                let (params, ret) = ty.signature();

                self.extern_functions
                    .insert(key.clone(), (extern_fn, params, ret));
            }
        }

        for (key, func) in self.registry.functions.clone() {
            let Some(&function) = self.functions.get(&key) else {
                continue;
            };

            if let Some((extern_fn, params, ret)) = self.extern_functions.get(&key).cloned() {
                self.compile_extern_function(module, function, &extern_fn, &params, &ret)?;
                continue;
            }

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

                let (param_types, return_type) = self
                    .extern_functions
                    .get(&key)
                    .map(|(_, p, r)| (p.clone(), r.clone()))
                    .unwrap_or_else(|| {
                        (
                            func.params.iter().map(|p| p.data_type.clone()).collect(),
                            func.return_type.clone(),
                        )
                    });

                let mut call_args = Vec::with_capacity(param_types.len() + 1);
                if !func.captures.is_empty() {
                    call_args.push(env);
                }

                for (i, param_type) in param_types.into_iter().enumerate() {
                    let index = translator.int_const(i as i64);
                    let payload = translator
                        .runtime_call("calibre_aggregate_get_field_and_deref", &[args, index])?;

                    call_args.push(translator.convert_from_i64(payload, &param_type.unwrap_ffi()));
                }

                let call = translator.builder.ins().call(func_ref, &call_args);

                let payload = if return_type.is_null() || return_type == MirDataType::Never {
                    translator.int_const(0)
                } else {
                    let result = translator.builder.inst_results(call);
                    let result = result
                        .first()
                        .copied()
                        .ok_or("expected return value from function entry".to_string())?;

                    translator.convert_to_i64(CompiledValue::new(result, return_type.clone()))?
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
            let builder = FunctionBuilder::new(&mut self.context.func, &mut self.builder_context);

            let mut translator = FunctionTranslator::new(
                builder,
                module,
                &self.registry,
                &self.types,
                &self.globals,
                &self.functions,
                &self.runtime_functions,
                &mut self.counter,
                MirDataType::Null,
                self.isa.clone(),
                &self.function_ids,
            );

            let entry_block = translator.builder.create_block();

            translator
                .builder
                .append_block_params_for_function_params(entry_block);

            translator.builder.switch_to_block(entry_block);

            let init_globals_key = VariableKey::from("calibre_init_globals");
            if let Some(&func_id) = translator.functions.get(&init_globals_key) {
                let func_ref = translator
                    .module
                    .declare_func_in_func(func_id, translator.builder.func);

                let args = if translator
                    .registry
                    .functions
                    .get(&init_globals_key)
                    .is_some_and(|f| !f.captures.is_empty())
                {
                    vec![translator.int_const(0)]
                } else {
                    Vec::new()
                };

                translator.builder.ins().call(func_ref, &args);
            }

            for (_, func_key) in init_functions {
                translator.runtime_lifecycle_call(&func_key, 0)?;
            }

            for (_, func_key) in fin_functions {
                translator.runtime_lifecycle_call(&func_key, 1)?;
            }

            let max_per_thread =
                translator.int_const(self.config.max_per_thread.unwrap_or(16) as i64);

            let stack_size = translator.int_const(self.config.stack_size.unwrap_or(0) as i64);

            translator
                .runtime_call_void("calibre_scheduler_configure", &[max_per_thread, stack_size])?;

            translator.runtime_call_void("calibre_scheduler_drain", &[])?;
            translator.runtime_call_void("calibre_exit_flush", &[])?;

            translator.builder.ins().return_(&[]);
            translator.builder.seal_all_blocks();

            let frontend_config = translator.isa.frontend_config();
            translator.builder.finalize(frontend_config);
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

    fn compile_extern_function<M: Module>(
        &mut self,
        module: &mut M,
        function: FuncId,
        extern_fn: &LirExtern,
        params: &[MirDataType],
        return_type: &MirDataType,
    ) -> Result<(), Box<dyn Error>> {
        use translation::ffi;

        self.context.func.signature.params.clear();
        self.context.func.signature.returns.clear();
        self.context.func.signature.call_conv = CallConv::SystemV;

        for param in params {
            self.context
                .func
                .signature
                .params
                .push(AbiParam::new(self.types.to_cranelift_type(param)));
        }

        let return_cl_ty = if !return_type.is_null() && !matches!(return_type, MirDataType::Never) {
            let ty = self.types.to_cranelift_type(return_type);
            self.context.func.signature.returns.push(AbiParam::new(ty));
            Some(ty)
        } else {
            None
        };

        let ffi_id = *self
            .runtime_functions
            .get("calibre_ffi_call")
            .ok_or("calibre_ffi_call not found in runtime_functions")?;

        let memo_funcs = if extern_fn.memo {
            let get = |name: &str| -> Result<FuncId, Box<dyn Error>> {
                self.runtime_functions
                    .get(name)
                    .copied()
                    .ok_or_else(|| format!("{} not found in runtime_functions", name).into())
            };

            Some((
                get("calibre_new_hasher")?,
                get("calibre_hash_add")?,
                get("calibre_hash_finish")?,
                get("calibre_memo_contains")?,
                get("calibre_memo_get")?,
                get("calibre_memo_set")?,
            ))
        } else {
            None
        };

        {
            let mut builder =
                FunctionBuilder::new(&mut self.context.func, &mut self.builder_context);

            let entry_block = builder.create_block();
            builder.append_block_params_for_function_params(entry_block);
            builder.switch_to_block(entry_block);

            let block_params = builder.block_params(entry_block).to_vec();

            let memo_hash = if let Some((
                new_hasher_id,
                hash_add_id,
                hash_finish_id,
                memo_contains_id,
                memo_get_id,
                _,
            )) = memo_funcs
            {
                let hasher_ref = module.declare_func_in_func(new_hasher_id, builder.func);
                let hasher_call = builder.ins().call(hasher_ref, &[]);
                let hasher = builder.inst_results(hasher_call)[0];

                let counter = builder.ins().iconst(types::I64, self.counter);
                self.counter += 1;

                let tag = builder.ins().iconst(
                    types::I64,
                    MirDataType::Int.type_tag(self.registry.enums()) as i64,
                );

                let hash_add_ref = module.declare_func_in_func(hash_add_id, builder.func);

                builder.ins().call(hash_add_ref, &[hasher, tag, counter]);

                for (i, param) in params.iter().enumerate() {
                    if extern_fn.memo_params != 0 && (extern_fn.memo_params & (1 << i)) == 0 {
                        continue;
                    }

                    let value = block_params[i];

                    let payload = match builder.func.dfg.value_type(value) {
                        types::F64 => builder
                            .ins()
                            .bitcast(types::I64, MachMemFlags::new(), value),
                        types::I8 => builder.ins().uextend(types::I64, value),
                        types::I32 => builder.ins().sextend(types::I64, value),
                        _ => value,
                    };

                    let tag = builder
                        .ins()
                        .iconst(types::I64, param.type_tag(self.registry.enums()) as i64);

                    builder.ins().call(hash_add_ref, &[hasher, tag, payload]);
                }

                let hash_finish_ref = module.declare_func_in_func(hash_finish_id, builder.func);
                let hash_call = builder.ins().call(hash_finish_ref, &[hasher]);
                let hash = builder.inst_results(hash_call)[0];

                let contains_ref = module.declare_func_in_func(memo_contains_id, builder.func);
                let contains_call = builder.ins().call(contains_ref, &[hash]);
                let contains = builder.inst_results(contains_call)[0];

                let hit_block = builder.create_block();
                let miss_block = builder.create_block();

                builder
                    .ins()
                    .brif(contains, hit_block, &[], miss_block, &[]);
                builder.switch_to_block(hit_block);

                let get_ref = module.declare_func_in_func(memo_get_id, builder.func);
                let get_call = builder.ins().call(get_ref, &[hash]);
                let cached = builder.inst_results(get_call)[0];

                if let Some(ty) = return_cl_ty {
                    let value = if ty == types::F64 {
                        builder
                            .ins()
                            .bitcast(types::F64, MachMemFlags::new(), cached)
                    } else if ty == types::I8 {
                        builder.ins().ireduce(types::I8, cached)
                    } else if ty == types::I32 {
                        builder.ins().ireduce(types::I32, cached)
                    } else {
                        cached
                    };

                    builder.ins().return_(&[value]);
                } else {
                    builder.ins().return_(&[]);
                }

                builder.switch_to_block(miss_block);
                Some(hash)
            } else {
                None
            };

            let slot = builder.create_sized_stack_slot(StackSlotData::new(
                StackSlotKind::ExplicitSlot,
                ((params.len() * 2).max(2) * 8) as u32,
                8,
            ));

            let buf_addr = builder.ins().stack_addr(types::I64, slot, 0);

            for (i, param) in params.iter().enumerate() {
                let value = block_params[i];
                let cl_ty = builder.func.dfg.value_type(value);

                // TODO Try to tie it back into my convert to i64 system
                let payload = match cl_ty {
                    types::F64 => builder
                        .ins()
                        .bitcast(types::I64, MachMemFlags::new(), value),
                    types::I8 => builder.ins().uextend(types::I64, value),
                    types::I32 => builder.ins().sextend(types::I64, value),
                    _ => value,
                };

                let kind = extern_fn
                    .parameters
                    .get(i)
                    .map(|x| ffi::ffi_kind(x))
                    .unwrap_or_else(|| ffi::ffi_kind(param));

                let kind_val = builder.ins().iconst(types::I64, kind as i64);

                builder
                    .ins()
                    .stack_store(types::I64, kind_val, slot, (i * 16) as i32);

                builder
                    .ins()
                    .stack_store(types::I64, payload, slot, (i * 16 + 8) as i32);
            }

            let ptr_type = self.types.ptr();

            macro_rules! cstr_ptr {
                ($text:expr) => {{
                    let mut bytes = $text.as_bytes().to_vec();
                    bytes.push(0);

                    let name = format!("calibre_ffi_str_{}", self.counter);
                    self.counter += 1;

                    let data_id = module.declare_data(&name, Linkage::Local, false, false)?;

                    let mut data = DataDescription::new();
                    data.define(bytes.into_boxed_slice());
                    module.define_data(data_id, &data)?;

                    let handle = module.declare_data_in_func(data_id, builder.func);
                    builder.ins().symbol_value(ptr_type, handle)
                }};
            }

            let library_ptr = cstr_ptr!(&extern_fn.library.to_string());
            let symbol_ptr = cstr_ptr!(&extern_fn.symbol.to_string());

            let param_len = builder.ins().iconst(types::I64, params.len() as i64);
            let return_kind = builder
                .ins()
                .iconst(types::I64, ffi::ffi_kind(return_type) as i64);

            let func_ref = module.declare_func_in_func(ffi_id, builder.func);

            let call = builder.ins().call(
                func_ref,
                &[library_ptr, symbol_ptr, buf_addr, param_len, return_kind],
            );

            let result = builder.inst_results(call)[0];

            if let (Some(hash), Some((_, _, _, _, _, memo_set_id))) = (memo_hash, memo_funcs) {
                let memo_set_ref = module.declare_func_in_func(memo_set_id, builder.func);
                builder.ins().call(memo_set_ref, &[hash, result]);
            }

            if let Some(ty) = return_cl_ty {
                let value = if ty == types::F64 {
                    builder
                        .ins()
                        .bitcast(types::F64, MachMemFlags::new(), result)
                } else if ty == types::I8 {
                    builder.ins().ireduce(types::I8, result)
                } else if ty == types::I32 {
                    builder.ins().ireduce(types::I32, result)
                } else {
                    result
                };

                builder.ins().return_(&[value]);
            } else {
                builder.ins().return_(&[]);
            }

            builder.seal_all_blocks();
            let frontend_config = self.isa.frontend_config();
            builder.finalize(frontend_config);
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
        source_dir: Option<String>,
        config: CraneliftConfig,
    ) -> Result<Self, Box<dyn Error>> {
        let isa = create_target_isa()?;
        let mut builder =
            JITBuilder::with_isa(isa.clone(), cranelift_module::default_libcall_names());

        for sym in calibre_runtime::get_runtime_symbols() {
            builder.symbol(sym.name, sym.ptr);
        }

        let mut module = JITModule::new(builder);

        let ptr_type = module.target_config().pointer_type();

        let ctx = module.make_context();

        let mut data = CompilerData {
            context: ctx,
            builder_context: FunctionBuilderContext::new(),
            types: CraneliftTypes::new(ptr_type),
            functions: FxHashMap::default(),
            globals: FxHashMap::default(),
            registry,
            runtime_functions: FxHashMap::default(),
            function_ids: FxHashMap::default(),
            extern_functions: FxHashMap::default(),
            counter: 1,
            isa,
            source_dir,
            config,
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
        source_dir: Option<String>,
        config: CraneliftConfig,
    ) -> Result<Self, Box<dyn Error>> {
        let isa = create_target_isa()?;

        let builder = ObjectBuilder::new(
            isa.clone(),
            module_name.to_string(),
            cranelift_module::default_libcall_names(),
        )?;

        let mut module = ObjectModule::new(builder);

        let ptr_type = module.target_config().pointer_type();

        let context = module.make_context();

        let mut data = CompilerData {
            context,
            builder_context: FunctionBuilderContext::new(),
            types: CraneliftTypes::new(ptr_type),
            functions: FxHashMap::default(),
            globals: FxHashMap::default(),
            registry,
            runtime_functions: FxHashMap::default(),
            function_ids: FxHashMap::default(),
            extern_functions: FxHashMap::default(),
            counter: 1,
            isa,
            source_dir,
            config,
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
