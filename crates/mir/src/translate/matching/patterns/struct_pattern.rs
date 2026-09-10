use super::{BindingDeclaration, PatternTranslation, PatternTranslator};
use crate::{
    environment::MiddleEnvironment, errors::MiddleErr, scoping::ScopeId,
    symbols::resolve::ResolutionOptions,
};
use calibre_parser::ast::{
    comparison::{BooleanOperator, ComparisonOperator},
    matching::{MatchArmType, MatchStructFieldPattern},
    nodes::{AstNode, AstNodeType},
};

pub struct StructPatternTranslator;

impl PatternTranslator for StructPatternTranslator {
    fn translate(
        &self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        pattern: &MatchArmType,
        value: &AstNode,
    ) -> Result<PatternTranslation, MiddleErr> {
        let (inner_pattern, aliases) = pattern.clone().alias_bindings();

        let MatchArmType::StructPattern(fields) = inner_pattern else {
            return Err(env.context.err_at_current(MiddleErr::Internal(
                "StructPatternCompiler called with non-struct pattern".to_string(),
            )));
        };

        let mut condition = AstNode::bool(env.context.current_span(), true);

        let mut bindings = aliases
            .into_iter()
            .map(|(var_type, name)| {
                Ok(BindingDeclaration {
                    name: env.resolve(scope, &name, ResolutionOptions::default().with_dollar())?,
                    value: value.clone(),
                    var_type,
                    data_type: None,
                })
            })
            .collect::<Result<Vec<_>, MiddleErr>>()?;

        for field in fields {
            match field {
                MatchStructFieldPattern::Value {
                    field: field_name,
                    value: expected,
                } => {
                    let current = AstNode::member(
                        env.context.current_span(),
                        value.clone(),
                        field_name.clone(),
                    );

                    condition = env.bool_and_nodes(
                        condition,
                        AstNode::new(
                            env.context.current_span(),
                            AstNodeType::ComparisonExpression {
                                left: Box::new(current),
                                right: Box::new(expected),
                                operator: ComparisonOperator::Equal,
                            },
                        ),
                    );
                }
                MatchStructFieldPattern::AlternativeValues {
                    field: field_name,
                    values,
                } => {
                    let current = AstNode::member(
                        env.context.current_span(),
                        value.clone(),
                        field_name.clone(),
                    );

                    let cond = if values.is_empty() {
                        AstNode::bool(env.context.current_span(), true)
                    } else {
                        let mut iter = values.into_iter();

                        let first = iter.next().unwrap();
                        let mut cond = AstNode::new(
                            env.context.current_span(),
                            AstNodeType::ComparisonExpression {
                                left: Box::new(current.clone()),
                                right: Box::new(first),
                                operator: ComparisonOperator::Equal,
                            },
                        );

                        for value in iter {
                            cond = AstNode::new(
                                env.context.current_span(),
                                AstNodeType::BooleanExpression {
                                    left: Box::new(cond),
                                    right: Box::new(AstNode::new(
                                        env.context.current_span(),
                                        AstNodeType::ComparisonExpression {
                                            left: Box::new(current.clone()),
                                            right: Box::new(value),
                                            operator: ComparisonOperator::Equal,
                                        },
                                    )),
                                    operator: BooleanOperator::Or,
                                },
                            );
                        }

                        cond
                    };

                    condition = env.bool_and_nodes(condition, cond);
                }
                MatchStructFieldPattern::Binding {
                    field: field_name,
                    var_type,
                    name,
                } => {
                    let current = AstNode::member(
                        env.context.current_span(),
                        value.clone(),
                        field_name.clone(),
                    );

                    bindings.push(BindingDeclaration {
                        name: env.resolve(
                            scope,
                            &name,
                            ResolutionOptions::default().with_dollar(),
                        )?,
                        value: current,
                        var_type,
                        data_type: None,
                    });
                }
            }
        }

        Ok(PatternTranslation {
            condition,
            bindings,
        })
    }
}
