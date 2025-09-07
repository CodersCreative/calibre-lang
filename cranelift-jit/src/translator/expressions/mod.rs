use crate::{
    translator::FunctionTranslator,
    values::{RuntimeType, RuntimeValue},
};
use calibre_parser::ast::{
    NodeType, ParserDataType, binary::BinaryOperator, comparison::Comparison,
};
use cranelift::prelude::*;
use cranelift_module::Module;

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

    pub fn translate_comparisson_expression(&mut self, node: NodeType) -> RuntimeValue {
        if let NodeType::ComparisonExpression {
            left,
            right,
            operator,
        } = node
        {
            let left = self.translate(*left);
            let right = self.translate(*right);
            let (left, right) = left.make_similar(&right, self);
            let data_type = left.data_type;
            let left = left.value;
            let right = right.value;
            let value = match data_type.clone() {
                RuntimeType::Int(_) | RuntimeType::UInt(_) | RuntimeType::Bool => match operator {
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
                },
                RuntimeType::Float(_) => match operator {
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
                },
                _ => unimplemented!(),
            };

            RuntimeValue::new(value, RuntimeType::Bool)
        } else {
            todo!()
        }
    }

    pub fn translate_identifier(&mut self, identifier: &str) -> RuntimeValue {
        let variable = self
            .variables
            .get(identifier)
            .expect("variable not defined");
        RuntimeValue::new(self.builder.use_var(variable.2), variable.1.clone())
    }
}
