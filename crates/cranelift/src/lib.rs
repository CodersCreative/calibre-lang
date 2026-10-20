use std::collections::VecDeque;

use crate::layout::{Layout, Layouts};
use calibre_lir::MirDataType;
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
        let mut layouts = Layouts::default();
        let mut work_list = VecDeque::new();

        for (key, lir_type) in &value.types {
            if let Some(layout) = Layout::from_lir_type(&layouts, lir_type) {
                layouts.layouts.insert(
                    MirDataType::Struct {
                        identifier: key.clone(),
                        generic_types: Vec::new(),
                    },
                    layout,
                );
            } else {
                work_list.push_back((key, lir_type));
            }
        }

        let mut depth = 64;
        while let Some((key, lir_type)) = work_list.pop_front()
            && depth > 0
        {
            if let Some(layout) = Layout::from_lir_type(&layouts, lir_type) {
                layouts.layouts.insert(
                    MirDataType::Struct {
                        identifier: key.clone(),
                        generic_types: Vec::new(),
                    },
                    layout,
                );
            } else {
                work_list.push_back((key, lir_type));
            }

            depth -= 1;
        }

        if work_list.is_empty() {
            eprintln!("something went wrong...........");
        }

        Self {
            builder_context: FunctionBuilderContext::new(),
            ctx: codegen::Context::new(),
            data_description: DataDescription::new(),
            lir_registry: value,
            layouts,
        }
    }
}
