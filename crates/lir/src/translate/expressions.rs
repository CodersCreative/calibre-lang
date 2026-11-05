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
        LirAs, LirBinary, LirComparison, LirIs, LirLiteral, LirLoad, LirNodeType, LirTerminator,
    },
    environment::LirEnvironment,
    translate::LirLowering,
};
use calibre_mir::ast::{
    MirAs, MirBinary, MirBoolean, MirComparison, MirIs, MirNeg, types::MirDataType,
};
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
        let temp = env.get_temp();
        env.declare_temp_null(span, temp.clone(), MirDataType::Bool);

        let left_val = env.lower_node(*self.left);

        match self.operator {
            BooleanOperator::And => {
                let right_block = env.create_block();
                let merge_block = env.create_block();

                env.set_terminator(LirTerminator::Branch {
                    span,
                    condition: left_val,
                    then_block: right_block,
                    else_block: Some(merge_block),
                });

                env.switch_to(right_block);

                let right_val = env.lower_node(*self.right);
                env.assign_temp_if_non_null(span, temp.clone(), right_val);

                env.jump_if_open(span, merge_block);

                env.switch_to(merge_block);
                env.assign_temp_if_non_null(span, temp.clone(), LirNodeType::bool(false));
            }
            BooleanOperator::Or => {
                let right_block = env.create_block();
                let merge_block = env.create_block();

                env.set_terminator(LirTerminator::Branch {
                    span,
                    condition: left_val,
                    then_block: merge_block,
                    else_block: Some(right_block),
                });

                env.switch_to(right_block);

                let right_val = env.lower_node(*self.right);
                env.assign_temp_if_non_null(span, temp.clone(), right_val);

                env.jump_if_open(span, merge_block);

                env.switch_to(merge_block);
                env.assign_temp_if_non_null(span, temp.clone(), LirNodeType::bool(true));
            }
        }

        LirNodeType::Load(LirLoad { value: temp })
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
