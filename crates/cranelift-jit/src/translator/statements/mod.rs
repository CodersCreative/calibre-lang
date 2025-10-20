pub mod loops;

use crate::{
    translator::FunctionTranslator,
    values::{RuntimeType, RuntimeValue},
};
use calibre_parser::{
    ast::{
        IfComparisonType, LoopType, Node, NodeType, VarType,
        binary::BinaryOperator,
        comparison::{self, Comparison},
    },
    lexer::StopValue,
};
use cranelift::{codegen::ir::BlockArg, prelude::*};

impl<'a> FunctionTranslator<'a> {
    pub fn translate_if_statement(&mut self, node: Node) -> RuntimeValue {
        if let NodeType::IfStatement {
            comparison,
            then,
            otherwise,
        } = node.node_type
        {
            let IfComparisonType::If(comparison) = *comparison else {
                panic!()
            };
            let condition_value = self.translate(comparison);

            let then_block = self.builder.create_block();
            let else_block = self.builder.create_block();
            let merge_block = self.builder.create_block();

            self.builder
                .append_block_param(merge_block, self.types.int());

            // Test the if condition and conditionally branch.
            self.builder
                .ins()
                .brif(condition_value.value, then_block, &[], else_block, &[]);

            self.builder.switch_to_block(then_block);
            self.builder.seal_block(then_block);
            let then_return = self.translate(*then);

            // Jump to the merge block, passing it the block return value.
            self.builder
                .ins()
                .jump(merge_block, &[BlockArg::Value(then_return.value)]);

            self.builder.switch_to_block(else_block);
            self.builder.seal_block(else_block);

            let mut else_return = self.translate_null();
            let mut use_else = false;
            if let Some(otherwise) = otherwise {
                else_return = self.translate(*otherwise);
                use_else = true;
            }

            // Jump to the merge block, passing it the block return value.
            self.builder
                .ins()
                .jump(merge_block, &[BlockArg::Value(else_return.value)]);

            // Switch to the merge block for subsequent statements.
            self.builder.switch_to_block(merge_block);

            // We've now seen all the predecessors of the merge block.
            self.builder.seal_block(merge_block);

            // Read the value of the if-else by reading the merge block
            // parameter.
            let phi = self.builder.block_params(merge_block)[0];

            RuntimeValue::new(
                phi,
                if use_else {
                    else_return.data_type
                } else {
                    then_return.data_type
                },
            )
        } else {
            todo!()
        }
    }
}
