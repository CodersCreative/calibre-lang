use crate::{
    translator::FunctionTranslator,
    values::{RuntimeType, RuntimeValue},
};
use calibre_parser::ast::{
    NodeType, ParserDataType, binary::BinaryOperator, comparison::Comparison,
};
use cranelift::{codegen::ir::BlockArg, prelude::*};

impl<'a> FunctionTranslator<'a> {
    pub fn translate_binary_expression(
        &mut self,
        left: RuntimeValue,
        right: RuntimeValue,
        operator: BinaryOperator,
    ) -> RuntimeValue {
        let (left, right) = left.make_similar(&right, self);
        let data_type = left.data_type;
        let left = left.value;
        let right = right.value;
        match data_type.clone() {
            RuntimeType::Int(_) | RuntimeType::UInt(_) | RuntimeType::Bool => {
                let signed = match data_type {
                    RuntimeType::Int(_) => true,
                    _ => false,
                };
                RuntimeValue::new(
                    match operator {
                        BinaryOperator::Add => self.builder.ins().iadd(left, right),
                        BinaryOperator::Sub => self.builder.ins().isub(left, right),
                        BinaryOperator::Mul => self.builder.ins().imul(left, right),
                        BinaryOperator::Div if signed => self.builder.ins().sdiv(left, right),
                        BinaryOperator::Div => self.builder.ins().udiv(left, right),
                        BinaryOperator::Mod if signed => self.builder.ins().srem(left, right),
                        BinaryOperator::Mod => self.builder.ins().urem(left, right),
                        BinaryOperator::Shl => self.builder.ins().ishl(left, right),
                        BinaryOperator::Shr if signed => self.builder.ins().sshr(left, right),
                        BinaryOperator::Shr => self.builder.ins().ushr(left, right),
                        BinaryOperator::BitOr => self.builder.ins().bor(left, right),
                        BinaryOperator::BitXor => self.builder.ins().bxor(left, right),
                        BinaryOperator::BitAnd => self.builder.ins().band(left, right),
                        BinaryOperator::Pow => {
                            let amt = right.as_u32();
                            if amt == 0 {
                                self.builder.ins().iconst(self.types.int(), 1)
                            } else if amt == 1 {
                                left
                            } else {
                                let mut value = left;
                                for _ in 1..amt {
                                    value = self.builder.ins().imul(value, left)
                                }
                                value
                            }
                        }
                    },
                    data_type.clone(),
                )
            }
            RuntimeType::Float(_) => RuntimeValue::new(
                match operator {
                    BinaryOperator::Add => self.builder.ins().fadd(left, right),
                    BinaryOperator::Sub => self.builder.ins().fsub(left, right),
                    BinaryOperator::Mul => self.builder.ins().fmul(left, right),
                    BinaryOperator::Div => self.builder.ins().fdiv(left, right),
                    BinaryOperator::Pow => {
                        let amt = right.as_u32();
                        if amt == 0 {
                            self.builder.ins().iconst(self.types.int(), 1)
                        } else if amt == 1 {
                            left
                        } else {
                            let mut value = left;
                            for _ in 1..amt {
                                value = self.builder.ins().fmul(value, left)
                            }
                            value
                        }
                    }
                    _ => unimplemented!(),
                },
                data_type,
            ),
            _ => unimplemented!(),
        }
    }
    fn translate_array_compare(
        &mut self,
        left: RuntimeValue,
        right: RuntimeValue,
        operator: Comparison,
    ) -> Value {
        let RuntimeType::List(left_r_type) = left.data_type else {
            panic!()
        };
        let RuntimeType::List(right_r_type) = right.data_type else {
            panic!()
        };
        let Some(left_r_type) = *left_r_type else {
            panic!()
        };
        let Some(right_r_type) = *right_r_type else {
            panic!()
        };
        let lhs_len = self
            .builder
            .ins()
            .load(self.types.ptr(), MemFlags::trusted(), left.value, 0);
        let rhs_len =
            self.builder
                .ins()
                .load(self.types.ptr(), MemFlags::trusted(), right.value, 0);

        let lhs = self.builder.ins().load(
            self.types.ptr(),
            MemFlags::trusted(),
            left.value,
            self.types.ptr().bytes() as i32,
        );

        let rhs = self.builder.ins().load(
            self.types.ptr(),
            MemFlags::trusted(),
            right.value,
            self.types.ptr().bytes() as i32,
        );

        let false_val = self.builder.ins().iconst(types::I8, 0);
        let true_val = self.builder.ins().iconst(types::I8, 1);

        let idx = self.builder.declare_var(self.types.ptr());
        let zero = self.builder.ins().iconst(self.types.ptr(), 0);
        self.builder.def_var(idx, zero);

        let header = self.builder.create_block();
        let header_idx = self.builder.append_block_param(header, self.types.ptr());
        let body = self.builder.create_block();
        let body_idx = self.builder.append_block_param(body, self.types.ptr());
        let exit = self.builder.create_block();
        let exit_bool = self.builder.append_block_param(exit, types::I8);

        self.builder.ins().jump(header, &[BlockArg::Value(zero)]);

        self.builder.switch_to_block(header);
        let cond = self
            .builder
            .ins()
            .icmp(IntCC::UnsignedLessThan, header_idx, lhs_len);
        self.builder.ins().brif(
            cond,
            body,
            &[BlockArg::Value(header_idx)],
            exit,
            &[match operator {
                Comparison::Equal => BlockArg::Value(true_val),
                Comparison::NotEqual => BlockArg::Value(false_val),
                _ => unimplemented!(),
            }],
        );

        self.builder.switch_to_block(body);
        self.builder.seal_block(body);
        let left_ty = self.types.get_type_from_runtime_type(&left_r_type);
        let right_ty = self.types.get_type_from_runtime_type(&right_r_type);
        let lhs_offset = self
            .builder
            .ins()
            .imul_imm(body_idx, left_ty.bytes() as i64);
        let rhs_offset = self
            .builder
            .ins()
            .imul_imm(body_idx, right_ty.bytes() as i64);
        let lhs_addr = self.builder.ins().iadd(lhs, lhs_offset);
        let rhs_addr = self.builder.ins().iadd(rhs, rhs_offset);

        let res = if false {
            // Add complex type comparisons
            // self.compile_complex_compare(lhs_addr, rhs_addr, sub_ty, hir_op)
            unreachable!()
        } else {
            let lhs_item = self
                .builder
                .ins()
                .load(left_ty, MemFlags::trusted(), lhs_addr, 0);
            let rhs_item = self
                .builder
                .ins()
                .load(right_ty, MemFlags::trusted(), rhs_addr, 0);

            self.translate_comparisson_expression(
                RuntimeValue::new(lhs_item, left_r_type),
                RuntimeValue::new(rhs_item, right_r_type),
                operator.clone(),
            )
        };

        let idx_plus_one = self.builder.ins().iadd_imm(body_idx, 1);

        // if the check was successful, check the next item of the array, otherwise exit
        // with false

        match operator {
            Comparison::Equal => {
                self.builder.ins().brif(
                    res.value,
                    header,
                    &[BlockArg::Value(idx_plus_one)],
                    exit,
                    &[BlockArg::Value(false_val)],
                );
            }
            Comparison::NotEqual => {
                self.builder.ins().brif(
                    res.value,
                    exit,
                    &[BlockArg::Value(true_val)],
                    header,
                    &[BlockArg::Value(idx_plus_one)],
                );
            }
            _ => unimplemented!(),
        }

        self.builder.switch_to_block(exit);
        self.builder.seal_block(header);
        self.builder.seal_block(exit);

        exit_bool
    }

    pub fn get_array_member(&mut self, left: RuntimeValue, index: Value) -> RuntimeValue {
        let RuntimeType::List(left_r_type) = left.data_type else {
            panic!()
        };
        let Some(left_r_type) = *left_r_type else {
            panic!()
        };
        let lhs = self.builder.ins().load(
            self.types.ptr(),
            MemFlags::trusted(),
            left.value,
            self.types.ptr().bytes() as i32,
        );
        let left_ty = self.types.get_type_from_runtime_type(&left_r_type);
        let lhs_offset = self.builder.ins().imul_imm(index, left_ty.bytes() as i64);
        let lhs_addr = self.builder.ins().iadd(lhs, lhs_offset);
        RuntimeValue::new(
            self.builder
                .ins()
                .load(left_ty, MemFlags::new().with_aligned(), lhs_addr, 0),
            left_r_type,
        )
    }

    pub fn translate_comparisson_expression(
        &mut self,
        left: RuntimeValue,
        right: RuntimeValue,
        operator: Comparison,
    ) -> RuntimeValue {
        let (left, right) = left.make_similar(&right, self);
        let data_type = left.data_type.clone();
        let value = match data_type.clone() {
            RuntimeType::Int(_) | RuntimeType::UInt(_) | RuntimeType::Bool => {
                let left = left.value;
                let right = right.value;
                match operator {
                    Comparison::Equal => self.builder.ins().icmp(IntCC::Equal, left, right),
                    Comparison::NotEqual => self.builder.ins().icmp(IntCC::NotEqual, left, right),
                    Comparison::Lesser => {
                        self.builder.ins().icmp(IntCC::SignedLessThan, left, right)
                    }
                    Comparison::LesserEqual => {
                        self.builder
                            .ins()
                            .icmp(IntCC::SignedLessThanOrEqual, left, right)
                    }
                    Comparison::Greater => {
                        self.builder
                            .ins()
                            .icmp(IntCC::SignedGreaterThan, left, right)
                    }
                    Comparison::GreaterEqual => {
                        self.builder
                            .ins()
                            .icmp(IntCC::SignedGreaterThan, left, right)
                    }
                }
            }
            RuntimeType::Float(_) => {
                let left = left.value;
                let right = right.value;
                match operator {
                    Comparison::Equal => self.builder.ins().fcmp(FloatCC::Equal, left, right),
                    Comparison::NotEqual => self.builder.ins().fcmp(FloatCC::NotEqual, left, right),
                    Comparison::Lesser => self.builder.ins().fcmp(FloatCC::LessThan, left, right),
                    Comparison::LesserEqual => {
                        self.builder
                            .ins()
                            .fcmp(FloatCC::LessThanOrEqual, left, right)
                    }
                    Comparison::Greater => {
                        self.builder.ins().fcmp(FloatCC::GreaterThan, left, right)
                    }
                    Comparison::GreaterEqual => {
                        self.builder.ins().fcmp(FloatCC::GreaterThan, left, right)
                    }
                }
            }
            RuntimeType::List(x) => self.translate_array_compare(left, right, operator),
            _ => unimplemented!(),
        };

        RuntimeValue::new(value, RuntimeType::Bool)
    }

    pub fn translate_identifier(&mut self, identifier: &str) -> RuntimeValue {
        let variable = self
            .variables
            .get(identifier)
            .expect("variable not defined");
        RuntimeValue::new(self.builder.use_var(variable.2), variable.1.clone())
    }
}
