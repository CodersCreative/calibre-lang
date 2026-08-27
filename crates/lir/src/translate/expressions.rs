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
    environment::{LirEnvironment, LirId},
    translate::LirLowering,
};
use calibre_mir::ast::{MirAs, MirBinary, MirBoolean, MirComparison, MirIs, MirNeg};
use calibre_parser::{
    Span,
    ast::{binary::BinaryOperator, comparison::BooleanOperator},
};

impl LirLowering for MirBinary {
    #[inline(always)]
    fn lower<'a>(self, env: &mut LirEnvironment<'a>, span: Span) -> LirId {
        let left = env.lower_node(*self.left);
        let right = env.lower_node(*self.right);

        env.add_with_children(
            LirNodeType::Binary(LirBinary {
                left,
                right,
                operator: self.operator,
            }),
            [left, right].into_iter(),
            span,
        )
    }
}

impl LirLowering for MirComparison {
    #[inline(always)]
    fn lower<'a>(self, env: &mut LirEnvironment<'a>, span: Span) -> LirId {
        let left = env.lower_node(*self.left);
        let right = env.lower_node(*self.right);

        env.add_with_children(
            LirNodeType::Comparison(LirComparison {
                left,
                right,
                operator: self.operator,
            }),
            [left, right].into_iter(),
            span,
        )
    }
}

impl LirLowering for MirBoolean {
    #[inline(always)]
    fn lower<'a>(self, env: &mut LirEnvironment<'a>, span: Span) -> LirId {
        let then_id = env.create_block();
        let else_id = env.create_block();
        let merge_id = env.create_block();

        let temp = env.get_temp();
        env.declare_temp_null(span, temp.as_str());

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
                let true_val = env.add(LirNodeType::bool(true), span);

                let checked = env.add_with_children(
                    LirNodeType::Boolean(LirBoolean {
                        left: right_val,
                        right: true_val,
                        operator: self.operator,
                    }),
                    [right_val, true_val].into_iter(),
                    span,
                );

                if env.current_block_open() {
                    env.assign_var(span, temp.as_str(), checked);
                    env.jump_if_open(span, merge_id);
                }

                env.switch_to(else_id);
                let false_val = env.add(LirNodeType::bool(false), span);

                if env.current_block_open() {
                    env.assign_var(span, temp.as_str(), false_val);
                    env.jump_if_open(span, merge_id);
                }
            }
            BooleanOperator::Or => {
                env.switch_to(then_id);
                let true_val = env.add(LirNodeType::bool(true), span);

                if env.current_block_open() {
                    env.assign_var(span, temp.as_str(), true_val);
                    env.jump_if_open(span, merge_id);
                }

                env.switch_to(else_id);

                let right_val = env.lower_node(*self.right);
                let false_val = env.add(LirNodeType::bool(false), span);

                let checked = env.add_with_children(
                    LirNodeType::Boolean(LirBoolean {
                        left: right_val,
                        right: false_val,
                        operator: self.operator,
                    }),
                    [right_val, false_val].into_iter(),
                    span,
                );

                if env.current_block_open() {
                    env.assign_var(span, temp.as_str(), checked);
                    env.jump_if_open(span, merge_id);
                }
            }
        }

        env.switch_to(merge_id);
        env.add(
            LirNodeType::Load(LirLoad {
                value: temp.into_boxed_str(),
            }),
            span,
        )
    }
}

impl LirLowering for MirNeg {
    #[inline(always)]
    fn lower<'a>(self, env: &mut LirEnvironment<'a>, span: Span) -> LirId {
        let left = env.add(LirNodeType::Literal(LirLiteral::Int(0)), span);
        let right = env.lower_node(*self.value);

        env.add_with_children(
            LirNodeType::Binary(LirBinary {
                left,
                right,
                operator: BinaryOperator::Sub,
            }),
            [left, right].into_iter(),
            span,
        )
    }
}

impl LirLowering for MirAs {
    #[inline(always)]
    fn lower<'a>(self, env: &mut LirEnvironment<'a>, span: Span) -> LirId {
        let value = env.lower_node(*self.value);

        env.add_with_children(
            LirNodeType::As(LirAs {
                value,
                data_type: self.data_type,
                failure_mode: self.failure_mode,
            }),
            std::iter::once(value),
            span,
        )
    }
}

impl LirLowering for MirIs {
    #[inline(always)]
    fn lower<'a>(self, env: &mut LirEnvironment<'a>, span: Span) -> LirId {
        let value = env.lower_node(*self.value);
        env.add_with_children(
            LirNodeType::Is(LirIs {
                value,
                data_type: self.data_type,
            }),
            std::iter::once(value),
            span,
        )
    }
}
