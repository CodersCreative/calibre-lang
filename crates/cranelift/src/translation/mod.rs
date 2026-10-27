use crate::layout::Layouts;
use crate::natives::{Natives, build_native_mapping};
use crate::values::{CompiledValue, CraneliftTypes};
use crate::{Functions, Globals, RuntimeFunctions};
use calibre_lir::MirDataType;
use calibre_lir::ast::{BlockId, LirNodeType, LirTerminator};
use calibre_lir::environment::{LirFunction, LirRegistry};
use calibre_mir::symbols::VariableKey;
use cranelift::codegen::ir::FuncRef;
use cranelift::prelude::*;
use cranelift_module::Module;
use rustc_hash::{FxHashMap, FxHashSet};
use std::error::Error;

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
    pub gc_managed_vars: FxHashSet<VariableKey>,
    pub return_type: MirDataType,
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
        return_type: MirDataType,
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
            return_type,
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
        for (i, (name, data_type)) in function.params.iter().enumerate() {
            let cl_ty = self.types.to_cranelift_type(data_type);
            let var = self.builder.declare_var(cl_ty);

            if let Some(&value) = block_params.get(i) {
                self.builder.def_var(var, value);
            }

            self.variables
                .insert(name.clone(), (data_type.clone(), var));
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
        self.builder.finalize();

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
                    let compiled = value.translate(self, Some(self.return_type.clone()))?;
                    self.builder.ins().return_(&[compiled.value]);
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
            LirNodeType::Boolean(x) => x.translate(translator, data_type),
            LirNodeType::As(x) => x.translate(translator, data_type),
            LirNodeType::Is(x) => x.translate(translator, data_type),
            LirNodeType::Ref(x) => x.translate(translator, data_type),
            LirNodeType::RefLoad(x) => x.translate(translator, data_type),
            LirNodeType::Deref(x) => x.translate(translator, data_type),
            LirNodeType::Discriminant(x) => x.translate(translator, data_type),
            LirNodeType::Move(x) => x.translate(translator, data_type),
            LirNodeType::Drop(x) => x.translate(translator, data_type),
            LirNodeType::Member(x) => x.translate(translator, data_type),
            LirNodeType::Aggregate(x) => x.translate(translator, data_type),
            LirNodeType::Enum(x) => x.translate(translator, data_type),
            LirNodeType::List(x) => x.translate(translator, data_type),
            LirNodeType::Range(x) => x.translate(translator, data_type),
            x => {
                unimplemented!("{}", x)
            }
        }
    }
}
