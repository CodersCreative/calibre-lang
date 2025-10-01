use crate::{
    translator::FunctionTranslator,
    values::{RuntimeType, RuntimeValue},
};
use calibre_parser::ast::{
    binary::BinaryOperator, comparison::Comparison, LoopType, Node, NodeType, VarType
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

    pub fn translate_loop_statement(&mut self, node: Node) -> RuntimeValue {
        if let NodeType::LoopDeclaration { loop_type, body } = node.node_type {
            let value = match *loop_type {
                LoopType::While(x) => {
                    if self.translate(x.clone()).data_type != RuntimeType::Bool {
                        return self.translate_loop_statement(Node::new(NodeType::LoopDeclaration {
                            loop_type: Box::new(LoopType::For(
                                format!("__counter_{}__", rand::random_range(0..100000)),
                                x,
                            )),
                            body,
                        }, node.line, node.col));
                    }

                    let (header_block, body_block, exit_block) = pre_loop!(self);

                    let condition = self.translate(x.clone());
                    mid_loop!(self, condition.value, body_block, exit_block);

                    let value = self.translate(*body);
                    end_loop!(self, header_block, exit_block);
                    value
                }
                LoopType::For(var_name, x) => {
                    let index = self.builder.declare_var(self.types.ptr());

                    let zero = self.builder.ins().iconst(self.types.ptr(), 0);
                    let value = self.translate(x);
                    let mut compare_value = RuntimeValue::new(zero.clone(), RuntimeType::Int);

                    let prev = match value.data_type.clone() {
                        RuntimeType::List(x) => {
                            if let Some(x) = *x {
                                let member = self.get_array_member(value.clone(), zero);
                                let member_var = self
                                    .builder
                                    .declare_var(self.types.get_type_from_runtime_type(&x));
                                self.builder.def_var(member_var, member.value);
                                compare_value = RuntimeValue::new(
                                    self.builder.ins().load(
                                        self.types.ptr(),
                                        MemFlags::trusted(),
                                        value.value,
                                        0,
                                    ),
                                    RuntimeType::Int,
                                );
                                self.variables
                                    .insert(var_name.clone(), (VarType::Mutable, x, member_var))
                            } else {
                                todo!()
                            }
                        }
                        RuntimeType::Tuple(x) => None,
                        _ => {
                            compare_value = value;
                            self.variables.insert(
                                var_name.clone(),
                                (VarType::Mutable, RuntimeType::Int, index),
                            )
                        }
                    };

                    self.builder.def_var(index, zero);

                    let (header_block, body_block, exit_block) = pre_loop!(self);

                    let index_val = self.builder.use_var(index);
                    let condition = self.translate_comparisson_expression(
                        RuntimeValue::new(index_val, RuntimeType::Int),
                        compare_value.clone(),
                        Comparison::Lesser,
                    );

                    mid_loop!(self, condition.value, body_block, exit_block);

                    match compare_value.data_type.clone() {
                        RuntimeType::List(_) => {
                            let member = self.get_array_member(compare_value.clone(), index_val);
                            if let Some((_, _, var)) = self.variables.get(&var_name) {
                                self.builder.def_var(var.clone(), member.value);
                            }
                        }
                        _ => {}
                    }

                    let value = self.translate(*body);

                    let index_val = self.builder.use_var(index);
                    let idx_plus_one = self.builder.ins().iadd_imm(index_val, 1);
                    self.builder.def_var(index, idx_plus_one);

                    end_loop!(self, header_block, exit_block);

                    if let Some(prev) = prev {
                        self.variables.insert(var_name, prev);
                    }
                    value
                }
                LoopType::ForEach(_, _) => todo!(),
            };

            // Just return 0 for now.
            value
        } else {
            todo!()
        }
    }
}
