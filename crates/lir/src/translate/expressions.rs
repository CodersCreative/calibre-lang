/*
This file handles :
BinaryExpression,
ComparisonExpression,
BooleanExpression,
NegExpression,
AsExpression,
IsExpression
*/

use crate::{
    ast::{
        LirAs, LirBinary, LirBoolean, LirComparison, LirIs, LirLiteral, LirLoad, LirNodeType,
        LirTerminator,
    },
    environment::LirEnvironment,
    translate::LirLowering,
};
use calibre_mir::ast::{MirAs, MirBinary, MirBoolean, MirComparison, MirIs, MirNeg};
use calibre_parser::{
    Span,
    ast::{binary::BinaryOperator, comparison::BooleanOperator},
};

impl LirLowering for MirBinary {
    #[inline(always)]
    fn lower<'a>(self, env: &mut LirEnvironment<'a>, _span: Span) -> LirNodeType {
        LirNodeType::Binary(LirBinary {
            left: Box::new(env.lower_node(*self.left)),
            right: Box::new(env.lower_node(*self.right)),
            operator: self.operator,
        })
    }
}

impl LirLowering for MirComparison {
    #[inline(always)]
    fn lower<'a>(self, env: &mut LirEnvironment<'a>, _span: Span) -> LirNodeType {
        LirNodeType::Comparison(LirComparison {
            left: Box::new(env.lower_node(*self.left)),
            right: Box::new(env.lower_node(*self.right)),
            operator: self.operator,
        })
    }
}

impl LirLowering for MirBoolean {
    #[inline(always)]
    fn lower<'a>(self, env: &mut LirEnvironment<'a>, span: Span) -> LirNodeType {
        let then_id = env.create_block();
        let else_id = env.create_block();
        let merge_id = env.create_block();

        let temp = env.get_temp();
        env.declare_temp_null(span, temp);

        let cond = env.lower_node(*self.left);
        env.set_terminator(LirTerminator::Branch {
            span,
            condition: cond,
            then_block: then_id,
            else_block: else_id,
        });

        match self.operator {
            BooleanOperator::And => {
                env.switch_to(then_id);
                let right_val = env.lower_node(*self.right);
                let checked = LirNodeType::Boolean(LirBoolean {
                    left: Box::new(right_val),
                    right: Box::new(LirNodeType::bool(true)),
                    operator: self.operator,
                });
                if env.current_block_open() {
                    env.assign_var(span, temp, checked);
                    env.jump_if_open(span, merge_id);
                }

                env.switch_to(else_id);
                if env.current_block_open() {
                    env.assign_var(span, temp, LirNodeType::bool(false));
                    env.jump_if_open(span, merge_id);
                }
            }
            BooleanOperator::Or => {
                env.switch_to(then_id);
                if env.current_block_open() {
                    env.assign_var(span, temp, LirNodeType::bool(true));
                    env.jump_if_open(span, merge_id);
                }

                env.switch_to(else_id);
                let right_val = env.lower_node(*self.right);
                let checked = LirNodeType::Boolean(LirBoolean {
                    left: Box::new(right_val),
                    right: Box::new(LirNodeType::bool(false)),
                    operator: self.operator,
                });
                if env.current_block_open() {
                    env.assign_var(span, temp, checked);
                    env.jump_if_open(span, merge_id);
                }
            }
        }

        env.switch_to(merge_id);
        LirNodeType::Load(LirLoad {
            value: temp,
        })
    }
}

impl LirLowering for MirNeg {
    #[inline(always)]
    fn lower<'a>(self, env: &mut LirEnvironment<'a>, _span: Span) -> LirNodeType {
        LirNodeType::Binary(LirBinary {
            left: Box::new(LirNodeType::Literal(LirLiteral::Int(0))),
            right: Box::new(env.lower_node(*self.value)),
            operator: BinaryOperator::Sub,
        })
    }
}

impl LirLowering for MirAs {
    #[inline(always)]
    fn lower<'a>(self, env: &mut LirEnvironment<'a>, _span: Span) -> LirNodeType {
        LirNodeType::As(LirAs {
            value: Box::new(env.lower_node(*self.value)),
            data_type: self.data_type,
            failure_mode: self.failure_mode,
        })
    }
}

impl LirLowering for MirIs {
    #[inline(always)]
    fn lower<'a>(self, env: &mut LirEnvironment<'a>, _span: Span) -> LirNodeType {
        LirNodeType::Is(LirIs {
            value: Box::new(env.lower_node(*self.value)),
            data_type: self.data_type,
        })
    }
}
