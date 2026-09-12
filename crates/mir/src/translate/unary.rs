use crate::{
    ast::{MiddleNode, MiddleNodeType, MirNeg},
    environment::MiddleEnvironment,
    errors::MiddleErr,
    scoping::ScopeId,
    translate::MirLowering,
};
use calibre_parser::{
    Span,
    ast::{
        comparison::ComparisonOperator,
        nodes::{
            AstNode, AstNodeType,
            binary::AstComparison,
            unary::{AstNeg, AstNot},
        },
        types::{ParserDataType, ParserInnerType},
    },
};

impl MirLowering for AstNeg {
    fn lower(
        self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        span: Span,
    ) -> Result<MiddleNode, MiddleErr> {
        Ok(MiddleNode {
            node_type: MiddleNodeType::NegExpression(MirNeg {
                value: Box::new(self.value.lower(env, scope, span)?),
            }),
            span,
        })
    }

    fn type_of(
        &self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        span: Span,
    ) -> Option<ParserDataType> {
        self.value.type_of(env, scope, span)
    }
}

impl MirLowering for AstNot {
    fn lower(
        self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        span: Span,
    ) -> Result<MiddleNode, MiddleErr> {
        AstNode {
            node_type: AstNodeType::ComparisonExpression(AstComparison {
                left: self.value,
                right: Box::new(AstNode::bool(env.context.current_span(), false)),
                operator: ComparisonOperator::Equal,
            }),
            span,
        }
        .lower(env, scope, span)
    }

    fn type_of(
        &self,
        _env: &mut MiddleEnvironment,
        _scope: ScopeId,
        span: Span,
    ) -> Option<ParserDataType> {
        Some(ParserDataType {
            data_type: ParserInnerType::Bool,
            span,
        })
    }
}
