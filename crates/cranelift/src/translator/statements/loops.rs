use crate::{
    translator::{BlockType, FunctionTranslator},
    values::{RuntimeType, RuntimeValue},
};
use calibre_mir::ast::{MiddleNode, MiddleNodeType};
use calibre_parser::ast::{
    LoopType, Node, NodeType, ParserText, VarType, binary::BinaryOperator, comparison::Comparison,
};
use cranelift::prelude::*;

macro_rules! pre_loop {
    ($func:expr) => {{
        let header_block = $func.builder.create_block();
        let body_block = $func.builder.create_block();
        let exit_block = $func.builder.create_block();
        $func.builder.ins().jump(header_block, &[]);
        $func.builder.switch_to_block(header_block);
        (header_block, body_block, exit_block)
    }};
}

macro_rules! mid_loop {
    ($func:expr, $condition:expr, $body_block:expr, $exit_block:expr) => {
        $func
            .builder
            .ins()
            .brif($condition, $body_block, &[], $exit_block, &[]);
        $func.builder.switch_to_block($body_block);
        $func.builder.seal_block($body_block);
    };
}

macro_rules! end_loop {
    ($func:expr, $header_block:expr, $exit_block:expr) => {
        $func.builder.ins().jump($header_block, &[]);
        $func.builder.switch_to_block($exit_block);
        $func.builder.seal_block($header_block);
        $func.builder.seal_block($exit_block);
    };
}

impl<'a> FunctionTranslator<'a> {
    pub fn get_int_value(&mut self, value: RuntimeValue) -> u32 {
        let mut result = 0;
        let index = self.builder.declare_var(self.types.ptr());
        let one = self.builder.ins().iconst(self.types.ptr(), 1);
        self.builder.def_var(index, value.value);
        let (header_block, body_block, exit_block) = pre_loop!(self);

        let index_val = self.builder.use_var(index);
        let condition = self.translate_comparisson_expression(
            RuntimeValue::new(index_val, RuntimeType::Int),
            RuntimeValue::new(Value::with_number(0).unwrap(), RuntimeType::Int),
            Comparison::Greater,
        );
        mid_loop!(self, condition.value, body_block, exit_block);

        let index_val = self.builder.use_var(index);
        let idx_sub_one = self.translate_binary_expression(
            RuntimeValue::new(index_val, RuntimeType::Int),
            RuntimeValue::new(one, RuntimeType::Int),
            BinaryOperator::Sub,
        );
        result += 1;
        self.builder.def_var(index, idx_sub_one.value);

        end_loop!(self, header_block, exit_block);
        result
    }

    pub fn translate_loop_statement(&mut self, node: MiddleNode) -> RuntimeValue {
        if let MiddleNodeType::LoopDeclaration { body } = node.node_type {
            let (header_block, body_block, exit_block) = pre_loop!(self);
            self.break_stack
                .push(BlockType::Continue(header_block.clone()));
            self.break_stack.push(BlockType::Break(exit_block.clone()));
            self.builder.ins().jump(body_block, &[]);
            self.builder.switch_to_block(body_block);
            let value = self.translate(*body);
            self.builder.seal_block(body_block);
            end_loop!(self, header_block, exit_block);
            self.break_stack.pop();
            self.break_stack.pop();
            value
        } else {
            todo!()
        }
    }
}
