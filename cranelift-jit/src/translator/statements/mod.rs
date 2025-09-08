use crate::{
    translator::FunctionTranslator,
    values::{RuntimeType, RuntimeValue},
};
use calibre_parser::{
    ast::{
        IfComparisonType, LoopType, NodeType, VarType,
        binary::BinaryOperator,
        comparison::{self, Comparison},
    },
    lexer::StopValue,
};
use cranelift::{codegen::ir::BlockArg, prelude::*};

impl<'a> FunctionTranslator<'a> {
    pub fn translate_if_statement(&mut self, node: NodeType) -> RuntimeValue {
        if let NodeType::IfStatement {
            comparison,
            then,
            otherwise,
        } = node
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
    pub fn translate_loop_statement(&mut self, node: NodeType) -> RuntimeValue {
        if let NodeType::LoopDeclaration { loop_type, body } = node {
            let value = match *loop_type {
                LoopType::While(x) => {
                    if self.translate(x.clone()).data_type != RuntimeType::Bool {
                        return self.translate_loop_statement(NodeType::LoopDeclaration {
                            loop_type: Box::new(LoopType::For(
                                format!("__counter_{}__", rand::random_range(0..100000)),
                                x,
                            )),
                            body,
                        });
                    }

                    let header_block = self.builder.create_block();
                    let body_block = self.builder.create_block();
                    let exit_block = self.builder.create_block();
                    self.builder.ins().jump(header_block, &[]);
                    self.builder.switch_to_block(header_block);
                    let condition = self.translate(x.clone());
                    self.builder
                        .ins()
                        .brif(condition.value, body_block, &[], exit_block, &[]);
                    self.builder.switch_to_block(body_block);
                    self.builder.seal_block(body_block);

                    let value = self.translate(*body);

                    self.builder.ins().jump(header_block, &[]);
                    self.builder.switch_to_block(exit_block);
                    self.builder.seal_block(header_block);
                    self.builder.seal_block(exit_block);
                    value
                }
                LoopType::For(var_name, x) => {
                    let index = self.builder.declare_var(self.types.ptr());

                    let zero = self.builder.ins().iconst(self.types.ptr(), 0);
                    let value = self.translate(x);
                    let mut compare_value = RuntimeValue::new(zero.clone(), RuntimeType::Int(64));

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
                                    RuntimeType::Int(64),
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
                                (VarType::Mutable, RuntimeType::Int(64), index),
                            )
                        }
                    };

                    self.builder.def_var(index, zero);

                    let header_block = self.builder.create_block();
                    let body_block = self.builder.create_block();
                    let exit_block = self.builder.create_block();

                    self.builder.ins().jump(header_block, &[]);

                    self.builder.switch_to_block(header_block);

                    let index_val = self.builder.use_var(index);
                    let condition = self.translate_comparisson_expression(
                        RuntimeValue::new(index_val, RuntimeType::Int(64)),
                        compare_value.clone(),
                        Comparison::Lesser,
                    );

                    self.builder
                        .ins()
                        .brif(condition.value, body_block, &[], exit_block, &[]);

                    self.builder.switch_to_block(body_block);
                    self.builder.seal_block(body_block);

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

                    self.builder.ins().jump(header_block, &[]);

                    self.builder.switch_to_block(exit_block);
                    self.builder.seal_block(header_block);
                    self.builder.seal_block(exit_block);
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
