use crate::layout::Layouts;
use crate::natives::{Natives, build_native_mapping};
use crate::values::{CompiledValue, CraneliftTypes};
use crate::{Functions, Globals, RuntimeFunctions};
use calibre_lir::MirDataType;
use calibre_lir::ast::{BlockId, LirNodeType, LirTerminator};
use calibre_lir::environment::{LirFunction, LirRegistry};
use calibre_mir::symbols::VariableKey;
use calibre_parser::ast::nodes::VarType;
use cranelift::codegen::ir::FuncRef;
use cranelift::codegen::isa::TargetIsa;
use cranelift::prelude::*;
use cranelift_module::Module;
use rustc_hash::{FxHashMap, FxHashSet};
use std::error::Error;
use std::sync::Arc;

pub mod access;
pub mod expression;
pub mod functions;
pub mod literals;
pub mod memory;
pub mod statements;

pub trait CraneliftTranslation {
    fn translate<'a, M: Module>(
        self,
        translator: &mut FunctionTranslator<'a, M>,
        data_type: Option<MirDataType>,
    ) -> Result<CompiledValue, Box<dyn Error>>;
}

pub type Variables = FxHashMap<VariableKey, (MirDataType, Variable)>;

pub struct FunctionTranslator<'a, M: Module> {
    pub types: &'a CraneliftTypes,
    pub layouts: &'a mut Layouts,
    pub builder: FunctionBuilder<'a>,
    pub module: &'a mut M,
    pub variables: Variables,
    pub globals: &'a Globals,
    pub functions: &'a Functions,
    pub registry: &'a LirRegistry,
    pub natives: Natives,
    pub runtime_functions: &'a RuntimeFunctions,
    pub counter: &'a mut i64,
    pub memo_hash: Option<Value>,
    pub gc_managed_vars: FxHashSet<VariableKey>,
    pub return_type: MirDataType,
    pub isa: Arc<dyn TargetIsa>,
}

impl<'a, M: Module> FunctionTranslator<'a, M> {
    pub fn new(
        builder: FunctionBuilder<'a>,
        module: &'a mut M,
        registry: &'a LirRegistry,
        types: &'a CraneliftTypes,
        layouts: &'a mut Layouts,
        globals: &'a Globals,
        functions: &'a Functions,
        runtime_functions: &'a RuntimeFunctions,
        counter: &'a mut i64,
        return_type: MirDataType,
        isa: Arc<dyn TargetIsa>,
    ) -> Self {
        let natives = build_native_mapping(registry);

        Self {
            types,
            builder,
            module,
            registry,
            layouts,
            variables: FxHashMap::default(),
            globals,
            functions,
            natives,
            runtime_functions,
            gc_managed_vars: FxHashSet::default(),
            counter,
            memo_hash: None,
            return_type,
            isa,
        }
    }

    pub fn translate_function(mut self, function: LirFunction) -> Result<(), Box<dyn Error>> {
        let block_map: FxHashMap<BlockId, Block> = function
            .blocks
            .iter()
            .flatten()
            .map(|b| {
                let block = self.builder.create_block();
                (b.id, block)
            })
            .collect();

        let entry_block = self.builder.create_block();

        self.builder
            .append_block_params_for_function_params(entry_block);
        self.builder.switch_to_block(entry_block);

        let block_params = self.builder.block_params(entry_block).to_vec();

        let memo_hasher = if function.memo {
            let new_hasher_func = *self
                .runtime_functions
                .get("calibre_new_hasher")
                .ok_or("calibre_new_hasher not found in runtime_functions")?;
            let new_hasher_ref = self
                .module
                .declare_func_in_func(new_hasher_func, self.builder.func);
            let hasher = self.builder.ins().call(new_hasher_ref, &[]);
            let hasher = self.builder.inst_results(hasher)[0];

            let counter = self.builder.ins().iconst(self.types.int(), *self.counter);
            *self.counter += 1;

            let tag = MirDataType::Int.type_tag(self.registry.enums()) as i64;
            let tag = self.builder.ins().iconst(self.types.int(), tag);

            let hash_add_func = *self
                .runtime_functions
                .get("calibre_hash_add")
                .ok_or("calibre_hash_add not found in runtime_functions")?;
            let hash_add_ref = self
                .module
                .declare_func_in_func(hash_add_func, self.builder.func);
            self.builder
                .ins()
                .call(hash_add_ref, &[hasher, tag, counter]);

            Some(hasher)
        } else {
            None
        };

        for (i, param) in function.params.iter().enumerate() {
            let cl_ty = self.types.to_cranelift_type(&param.data_type);
            let var = self.builder.declare_var(cl_ty);

            if let Some(&value) = block_params.get(i) {
                let value = CompiledValue::new(value, param.data_type.clone());

                if function.memo
                    && let Some(hasher_ptr) = memo_hasher
                    && (function.memo_params == 0 || (function.memo_params & (1 << i) != 0))
                {
                    let tag = param.data_type.type_tag(self.registry.enums()) as i64;
                    let tag = self.builder.ins().iconst(self.types.int(), tag);

                    let value = self.convert_to_i64(value.clone())?;

                    let hash_add_func = *self
                        .runtime_functions
                        .get("calibre_hash_add")
                        .ok_or("calibre_hash_add not found in runtime_functions")?;

                    let hash_add_ref = self
                        .module
                        .declare_func_in_func(hash_add_func, self.builder.func);

                    self.builder
                        .ins()
                        .call(hash_add_ref, &[hasher_ptr, tag, value]);
                }

                let value =
                    if param.var_type == VarType::Mutable && !param.data_type.is_ref() {
                        let func_id = self.runtime_functions.get("calibre_deep_clone").ok_or(
                            String::from("calibre_deep_clone not found in runtime_functions"),
                        )?;

                        let func_ref = self
                            .module
                            .declare_func_in_func(*func_id, self.builder.func);

                        let tag = value.data_type.type_tag(self.registry.enums()) as i64;
                        let tag = self.builder.ins().iconst(self.types.int(), tag);

                        let value = self.convert_to_i64(value)?;

                        let call = self.builder.ins().call(func_ref, &[tag, value]);

                        self.builder.inst_results(call)[0]
                    } else {
                        value.value
                    };

                self.builder.def_var(var, value);
            }

            self.variables
                .insert(param.identifier.clone(), (param.data_type.clone(), var));
        }

        if function.memo
            && let Some(hasher_ptr) = memo_hasher
        {
            let hash_finish_func = *self
                .runtime_functions
                .get("calibre_hash_finish")
                .ok_or("calibre_hash_finish not found in runtime_functions")?;
            let hash_finish_ref = self
                .module
                .declare_func_in_func(hash_finish_func, self.builder.func);
            let hash = self.builder.ins().call(hash_finish_ref, &[hasher_ptr]);
            let hash = self.builder.inst_results(hash)[0];

            let memo_contains_func = *self
                .runtime_functions
                .get("calibre_memo_contains")
                .ok_or("calibre_memo_contains not found in runtime_functions")?;
            let memo_contains_ref = self
                .module
                .declare_func_in_func(memo_contains_func, self.builder.func);
            let contains = self.builder.ins().call(memo_contains_ref, &[hash]);
            let contains = self.builder.inst_results(contains)[0];

            let cache_hit_block = self.builder.create_block();
            let cache_miss_block = self.builder.create_block();

            self.builder
                .ins()
                .brif(contains, cache_hit_block, &[], cache_miss_block, &[]);
            self.builder.switch_to_block(cache_hit_block);

            let memo_get_func = *self
                .runtime_functions
                .get("calibre_memo_get")
                .ok_or("calibre_memo_get not found in runtime_functions")?;
            let memo_get_ref = self
                .module
                .declare_func_in_func(memo_get_func, self.builder.func);
            let value = self.builder.ins().call(memo_get_ref, &[hash]);
            let value = self.builder.inst_results(value)[0];

            self.builder.ins().return_(&[value]);
            self.builder.switch_to_block(cache_miss_block);

            self.memo_hash = Some(hash);
        }

        let first_id = function.blocks.iter().flatten().next().map(|b| b.id);
        if let Some(first_id) = first_id {
            if let Some(&target) = block_map.get(&first_id) {
                self.builder.ins().jump(target, &[]);
            }
        } else {
            // Function has no blocks
            if function.return_type.is_null() || function.return_type == MirDataType::Never {
                self.builder.ins().return_(&[]);
            } else if function.return_type == MirDataType::Float {
                let zero = self.builder.ins().f64const(0.0);
                self.builder.ins().return_(&[zero]);
            } else {
                let zero = self.builder.ins().iconst(self.types.int(), 0);
                self.builder.ins().return_(&[zero]);
            }
        }

        self.builder.seal_block(entry_block);

        for b in function.blocks.into_iter().flatten() {
            let Some(&cl_block) = block_map.get(&b.id) else {
                continue;
            };
            self.builder.switch_to_block(cl_block);

            for x in b.instructions {
                x.node_type.translate(&mut self, None)?;
            }

            if let Some(terminator) = b.terminator {
                self.translate_terminator(terminator, &block_map)?;
            } else {
                // If block has no terminator
                if function.return_type.is_null() || function.return_type == MirDataType::Never {
                    self.builder.ins().return_(&[]);
                } else if function.return_type == MirDataType::Float {
                    let zero = self.builder.ins().f64const(0.0);
                    self.builder.ins().return_(&[zero]);
                } else {
                    let zero = self.builder.ins().iconst(self.types.int(), 0);
                    self.builder.ins().return_(&[zero]);
                }
            }
        }

        self.builder.seal_all_blocks();
        let frontend_config = self.isa.frontend_config();
        self.builder.finalize(frontend_config);

        Ok(())
    }

    fn translate_terminator(
        &mut self,
        term: LirTerminator,
        block_map: &FxHashMap<BlockId, Block>,
    ) -> Result<(), Box<dyn Error>> {
        match term {
            LirTerminator::Jump { target, .. } => {
                let dest = block_map[&target];
                self.builder.ins().jump(dest, &[]);
            }
            LirTerminator::Branch {
                condition,
                then_block,
                else_block,
                ..
            } => {
                let condition_value = condition.translate(self, Some(MirDataType::Bool))?;
                let then_dest = block_map[&then_block];

                if let Some(else_block) = else_block {
                    let else_dest = block_map[&else_block];

                    self.builder
                        .ins()
                        .brif(condition_value.value, then_dest, &[], else_dest, &[]);
                } else {
                    let fallthrough = self.builder.create_block();
                    self.builder.ins().brif(
                        condition_value.value,
                        then_dest,
                        &[],
                        fallthrough,
                        &[],
                    );
                    self.builder.switch_to_block(fallthrough);
                }
            }
            LirTerminator::Return { value, .. } => {
                if let Some(value) = value {
                    let value = value.translate(self, Some(self.return_type.clone()))?;

                    if let Some(hash) = self.memo_hash {
                        let memo_set_func = *self
                            .runtime_functions
                            .get("calibre_memo_set")
                            .ok_or("calibre_memo_set not found in runtime_functions")?;
                        let memo_set_ref = self
                            .module
                            .declare_func_in_func(memo_set_func, self.builder.func);
                        self.builder.ins().call(memo_set_ref, &[hash, value.value]);
                    }

                    self.builder.ins().return_(&[value.value]);
                } else {
                    self.builder.ins().return_(&[]);
                }
            }
        }
        Ok(())
    }

    pub fn get_add_root(&mut self) -> Result<FuncRef, Box<dyn Error>> {
        let id = self
            .runtime_functions
            .get("calibre_gc_add_root")
            .ok_or("calibre_gc_add_root not found in runtime_functions")?;

        Ok(self.module.declare_func_in_func(*id, self.builder.func))
    }
}

impl CraneliftTranslation for LirNodeType {
    fn translate<'a, M: Module>(
        self,
        translator: &mut FunctionTranslator<'a, M>,
        data_type: Option<MirDataType>,
    ) -> Result<CompiledValue, Box<dyn Error>> {
        match self {
            LirNodeType::Noop => {
                let val = translator.builder.ins().iconst(translator.types.int(), 0);
                Ok(CompiledValue::new(val, MirDataType::Null))
            }
            LirNodeType::Literal(x) => x.translate(translator, data_type),
            LirNodeType::Load(x) => x.translate(translator, data_type),
            LirNodeType::Declare(x) => x.translate(translator, data_type),
            LirNodeType::Assign(x) => x.translate(translator, data_type),
            LirNodeType::Binary(x) => x.translate(translator, data_type),
            LirNodeType::Comparison(x) => x.translate(translator, data_type),
            LirNodeType::Call(x) => x.translate(translator, data_type),
            LirNodeType::As(x) => x.translate(translator, data_type),
            LirNodeType::Is(x) => x.translate(translator, data_type),
            LirNodeType::Ref(x) => x.translate(translator, data_type),
            LirNodeType::RefLoad(x) => x.translate(translator, data_type),
            LirNodeType::Deref(x) => x.translate(translator, data_type),
            LirNodeType::Discriminant(x) => x.translate(translator, data_type),
            LirNodeType::Move(x) => x.translate(translator, data_type),
            LirNodeType::Clone(x) => x.translate(translator, data_type),
            LirNodeType::Drop(x) => x.translate(translator, data_type),
            LirNodeType::Member(x) => x.translate(translator, data_type),
            LirNodeType::Aggregate(x) => x.translate(translator, data_type),
            LirNodeType::Enum(x) => x.translate(translator, data_type),
            LirNodeType::List(x) => x.translate(translator, data_type),
            LirNodeType::Range(x) => x.translate(translator, data_type),
            LirNodeType::Index(x) => x.translate(translator, data_type),
            x => {
                unimplemented!("{}", x)
            }
        }
    }
}
