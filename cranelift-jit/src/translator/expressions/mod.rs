use crate::translator::FunctionTranslator;
use calibre_parser::ast::{NodeType, binary::BinaryOperator, comparison::Comparison};
use cranelift::prelude::*;

impl<'a> FunctionTranslator<'a> {
    pub fn translate_binary_expression(&mut self, node: NodeType) -> Value {
        if let NodeType::BinaryExpression {
            left,
            right,
            operator,
        } = node
        {
            let left = self.translate(*left);
            let right = self.translate(*right);
            match operator {
                BinaryOperator::Add => self.builder.ins().iadd(left, right),
                BinaryOperator::Sub => self.builder.ins().isub(left, right),
                BinaryOperator::Mul => self.builder.ins().imul(left, right),
                BinaryOperator::Div => self.builder.ins().udiv(left, right),
                BinaryOperator::Shl => self.builder.ins().ishl(left, right),
                BinaryOperator::Shr => self.builder.ins().ushr(left, right),
                BinaryOperator::BitOr => self.builder.ins().bor(left, right),
                BinaryOperator::BitXor => self.builder.ins().bxor(left, right),
                BinaryOperator::BitAnd => self.builder.ins().band(left, right),
                BinaryOperator::Pow => {
                    let amt = right.as_u32();
                    if amt == 0 {
                        self.builder.ins().iconst(self.types.int, 1)
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
                BinaryOperator::Mod => unimplemented!(),
            }
        } else {
            todo!()
        }
    }

    pub fn translate_comparisson_expression(&mut self, node: NodeType) -> Value {
        if let NodeType::ComparisonExpression {
            left,
            right,
            operator,
        } = node
        {
            let left = self.translate(*left);
            let right = self.translate(*right);
            match operator {
                Comparison::Equal => self.builder.ins().icmp(IntCC::Equal, left, right),
                Comparison::NotEqual => self.builder.ins().icmp(IntCC::NotEqual, left, right),
                Comparison::Lesser => self.builder.ins().icmp(IntCC::SignedLessThan, left, right),
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
        } else {
            todo!()
        }
    }

    pub fn translate_identifier(&mut self, identifier: &str) -> Value {
        let variable = self
            .variables
            .get(identifier)
            .expect("variable not defined");
        self.builder.use_var(*variable)
    }
}
