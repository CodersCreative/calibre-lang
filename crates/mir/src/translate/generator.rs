use crate::{
    ast::{MiddleNode, types::MirDataType},
    environment::MiddleEnvironment,
    errors::MiddleErr,
    scoping::ScopeId,
    symbols::resolve::ResolutionOptions,
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
            functions::{AstFunction, FunctionHeader},
            generator::AstGenerator,
            loops::AstLoop,
        },
    },
};
use tracing::instrument;

impl MirLowering for AstGenerator {
    #[instrument(skip_all)]
    fn lower(
        self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        span: Span,
    ) -> Result<MiddleNode, MiddleErr> {
        let return_type = self.type_of(env, scope, span).unwrap();
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

        AstNode::call(
            span,
            AstNode::new(
                span,
                AstNodeType::FunctionDeclaration(AstFunction {
                    header: FunctionHeader {
                        return_type: return_type.into(),
                        ..Default::default()
                    },
                    body: Box::new(AstNode::new_temp_scope(vec![loop_node])),
                }),
            ),
            Vec::new(),
        )
        .lower(env, scope, span)
    }

    fn type_of(
        &self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        span: Span,
    ) -> Option<MirDataType> {
        let elem = match &self.data_type {
            Some(dt) => env
                .resolve_data_type(scope, dt, ResolutionOptions::typing())
                .ok()?,
            _ => self.map.type_of(env, scope, span)?,
        };

        Some(MirDataType::Gen(Box::new(elem)))
    }
}
