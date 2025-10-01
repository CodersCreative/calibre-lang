use calibre_parser::ast::{Node, NodeType, ParserDataType, VarType};
use cranelift::prelude::*;
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{DataDescription, Linkage, Module};
use std::{collections::HashMap, error::Error};

use crate::translator::FunctionTranslator;

pub struct JIT {
    /// The function builder context, which is reused across multiple
    /// FunctionBuilder instances.
    builder_context: FunctionBuilderContext,

    /// The main Cranelift context, which holds the state for codegen. Cranelift
    /// separates this from `Module` to allow for parallel compilation, with a
    /// context per thread, though this isn't in the simple demo here.
    ctx: codegen::Context,

    /// The data description, which is to data objects what `ctx` is to functions.
    data_description: DataDescription,

    /// The module, with the jit backend, which manages the JIT'd
    /// functions.
    module: JITModule,
}

impl Default for JIT {
    fn default() -> Self {
        let mut flag_builder = settings::builder();
        flag_builder.set("use_colocated_libcalls", "false").unwrap();
        flag_builder.set("is_pic", "false").unwrap();
        let isa_builder = cranelift_native::builder().unwrap_or_else(|msg| {
            panic!("host machine is not supported: {}", msg);
        });
        let isa = isa_builder
            .finish(settings::Flags::new(flag_builder))
            .unwrap();
        let builder = JITBuilder::with_isa(isa, cranelift_module::default_libcall_names());

        let module = JITModule::new(builder);
        Self {
            builder_context: FunctionBuilderContext::new(),
            ctx: module.make_context(),
            data_description: DataDescription::new(),
            module,
        }
    }
}

impl JIT {
    pub fn compile(&mut self, program: Node) -> Result<*const u8, Box<dyn Error>> {
        let mut main = None;

        if let NodeType::ScopeDeclaration { body, is_temp } = program.node_type {
            for func in body {
                let val = self.compile_const_fns(func.clone())?;
                if let NodeType::VariableDeclaration { identifier, .. } = func.node_type {
                    if identifier == "main" {
                        main = Some(val);
                    }
                }
            }
        }

        Ok(main.unwrap())
    }

    pub fn compile_const_fns(&mut self, node: Node) -> Result<*const u8, Box<dyn Error>> {
        if let NodeType::VariableDeclaration {
            var_type,
            mut identifier,
            value,
            data_type: _,
        } = node.node_type
        {
            if identifier == "main" {
                identifier = "_start".to_string();
            }

            if var_type != VarType::Constant {
                unimplemented!()
            }

            if let NodeType::FunctionDeclaration {
                parameters,
                body,
                return_type,
                is_async: _,
            } = value.node_type
            {
                self.translate(
                    parameters
                        .into_iter()
                        .map(|x| (x.0.clone(), x.1.clone()))
                        .collect(),
                    return_type,
                    *body,
                )?;
                let id = self
                    .module
                    .declare_function(&identifier, Linkage::Export, &self.ctx.func.signature)
                    .map_err(|e| e.to_string())?;

                // Define the function to jit. This finishes compilation, although
                // there may be outstanding relocations to perform. Currently, jit
                // cannot finish relocations until all functions to be called are
                // defined. For this toy demo for now, we'll just finalize the
                // function below.
                self.module
                    .define_function(id, &mut self.ctx)
                    .map_err(|e| e.to_string())?;

                // Now that compilation is finished, we can clear out the context state.
                self.module.clear_context(&mut self.ctx);

                // Finalize the functions which we just defined, which resolves any
                // outstanding relocations (patching in addresses, now that they're
                // available).
                self.module.finalize_definitions().unwrap();

                // We can now retrieve a pointer to the machine code.
                let code = self.module.get_finalized_function(id);

                Ok(code)
            } else {
                todo!()
            }
        } else {
            todo!()
        }
    }

    fn translate(
        &mut self,
        params: Vec<(String, ParserDataType)>,
        return_type: Option<ParserDataType>,
        node: Node,
    ) -> Result<(), Box<dyn Error>> {
        let ptr = self.module.target_config().pointer_type();
        let types = crate::translator::Types::new(ptr);
        for p in &params {
            self.ctx
                .func
                .signature
                .params
                .push(AbiParam::new(types.get_type_from_parser_type(&p.1)));
        }

        if let Some(t) = return_type {
            self.ctx
                .func
                .signature
                .returns
                .push(AbiParam::new(types.get_type_from_parser_type(&t)));
        }

        // Create the builder to build a function.
        let mut builder = FunctionBuilder::new(&mut self.ctx.func, &mut self.builder_context);

        // Create the entry block, to start emitting code in.
        let entry_block = builder.create_block();
        builder.append_block_params_for_function_params(entry_block);

        // Tell the builder to emit code in this block.
        builder.switch_to_block(entry_block);

        // And, tell the builder that this block will have no further
        // predecessors. Since it's the entry block, it won't have any
        // predecessors.
        builder.seal_block(entry_block);

        // Now translate the statements of the function body.
        let mut trans = FunctionTranslator {
            variables: HashMap::new(),
            types,
            builder,
            module: &mut self.module,
            description: &mut self.data_description,
            stop: None,
        };
        let value = trans.translate(node);

        // Emit the return instruction.
        trans.builder.ins().return_(&[value.value]);

        // Tell the builder we're done with this function.
        trans.builder.finalize();
        Ok(())
    }
}
