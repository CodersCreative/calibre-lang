use crate::layout::Layouts;
use calibre_lir::environment::LirRegistry;
use cranelift::frontend::FunctionBuilderContext;
use cranelift::prelude::*;
use cranelift_module::DataDescription;

pub mod layout;

pub struct Compiler {
    /// The function builder context, which is reused across multiple
    /// FunctionBuilder instances.
    builder_context: FunctionBuilderContext,

    /// The main Cranelift context, which holds the state for codegen. Cranelift
    /// separates this from `Module` to allow for parallel compilation, with a
    /// context per thread, though this isn't in the simple demo here.
    ctx: codegen::Context,

    /// The data description, which is to data objects what `ctx` is to functions.
    data_description: DataDescription,

    lir_registry: LirRegistry,

    layouts: Layouts,
}

impl From<LirRegistry> for Compiler {
    fn from(value: LirRegistry) -> Self {
        // TODO Go through types in the LirRegistry and initialize them in the layouts
        Self {
            builder_context: FunctionBuilderContext::new(),
            ctx: codegen::Context::new(),
            data_description: DataDescription::new(),
            lir_registry: value,
            layouts: Layouts::default(),
        }
    }
}
