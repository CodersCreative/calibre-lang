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
    ast::{LirAs, LirBinary, LirBoolean, LirComparison, LirIs, LirLiteral, LirNodeType},
    environment::LirEnvironment,
    translate::LirLowering,
};
use calibre_mir::ast::{MirAs, MirBinary, MirBoolean, MirComparison, MirIs, MirNeg};
use calibre_parser::{Span, ast::binary::BinaryOperator};

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
    fn lower<'a>(self, env: &mut LirEnvironment<'a>, _span: Span) -> LirNodeType {
        LirNodeType::Boolean(LirBoolean {
            left: Box::new(env.lower_node(*self.left)),
            right: Box::new(env.lower_node(*self.right)),
            operator: self.operator,
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
