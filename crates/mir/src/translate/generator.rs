use crate::{
    ast::MiddleNode, environment::MiddleEnvironment, errors::MiddleErr, scoping::ScopeId,
    translate::MirLowering,
};
use calibre_parser::{
    Span,
    ast::{
        comparison::BooleanOperator,
        nodes::{
            AstNode, AstNodeType,
            binary::AstBoolean,
            conditionals::{AstIf, IfComparisonType},
            flow::{AstContinue, AstReturn},
            generator::AstGenerator,
            loops::AstLoop,
        },
        types::{ParserDataType, ParserInnerType},
    },
};

impl MirLowering for AstGenerator {
    fn lower(
        self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        span: Span,
    ) -> Result<MiddleNode, MiddleErr> {
        let guard = self.conditionals.into_iter().reduce(|left, right| {
            AstNode::new(
                span,
                AstNodeType::BooleanExpression(AstBoolean {
                    left: Box::new(left),
                    right: Box::new(right),
                    operator: BooleanOperator::And,
                }),
            )
        });

        let mut loop_body_items = Vec::new();
        let yield_node = AstNode::new(
            span,
            AstNodeType::Return(AstReturn {
                value: Some(self.map),
            }),
        );

        if let Some(guard) = guard {
            loop_body_items.push(AstNode::new(
                span,
                AstNodeType::IfStatement(AstIf {
                    comparison: Box::new(IfComparisonType::If(guard)),
                    then: Box::new(yield_node),
                    otherwise: Some(Box::new(AstNode::new(
                        span,
                        AstNodeType::Continue(AstContinue { label: None }),
                    ))),
                }),
            ));
        } else {
            loop_body_items.push(yield_node);
        }

        let loop_node = AstNode::new(
            span,
            AstNodeType::LoopDeclaration(AstLoop {
                loop_type: self.loop_type,
                body: Box::new(AstNode::new_temp_scope(loop_body_items)),
                until: self.until,
                label: None,
                else_body: None,
            }),
        );

        MiddleEnvironment::wrap_generator_body(
            AstNode::new_temp_scope_with_create(vec![loop_node], Some(false)),
            self.data_type.unwrap_or(ParserDataType::auto(span)),
            span,
        )
        .lower(env, scope, span)
    }

    fn type_of(
        &self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        span: Span,
    ) -> Option<ParserDataType> {
        let elem = match &self.data_type {
            Some(dt) => dt.clone(),
            _ => self.map.type_of(env, scope, span)?,
        };

        Some(ParserDataType::new(
            span,
            ParserInnerType::Gen(Box::new(elem)),
        ))
    }
}
