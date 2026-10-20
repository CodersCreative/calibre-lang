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
    pub builder_context: FunctionBuilderContext,

    /// The main Cranelift context, which holds the state for codegen.
    pub ctx: codegen::Context,

    /// The data description, which is to data objects what `ctx` is to functions.
    pub data_description: DataDescription,

    pub lir_registry: LirRegistry,

    pub layouts: Layouts,
}

const PRIMITIVE_TYPES: [MirDataType; 9] = [
    MirDataType::Int,
    MirDataType::UInt,
    MirDataType::Byte,
    MirDataType::Float,
    MirDataType::Bool,
    MirDataType::Char,
    MirDataType::Str,
    MirDataType::Range,
    MirDataType::Dynamic,
];

impl From<LirRegistry> for Compiler {
    fn from(value: LirRegistry) -> Self {
        let mut layouts = Layouts::default();
        let mut work_list = VecDeque::new();

        for data_type in &PRIMITIVE_TYPES {
            layouts.ensure_layout(data_type);
        }

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

        if !work_list.is_empty() {
            eprintln!("Could not resolve layouts for {} types", work_list.len());
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
