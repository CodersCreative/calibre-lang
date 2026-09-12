use super::{BindingDeclaration, PatternTranslation, PatternTranslator};
use crate::{
    environment::MiddleEnvironment, errors::MiddleErr, scoping::ScopeId,
    symbols::resolve::ResolutionOptions,
};
use calibre_parser::ast::{
    comparison::ComparisonOperator,
    matching::{MatchArmType, MatchTupleItem},
    nodes::{AstNode, AstNodeType, binary::AstComparison},
};

pub struct ListPatternTranslator;

impl PatternTranslator for ListPatternTranslator {
    fn translate(
        &self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        pattern: &MatchArmType,
        value: &AstNode,
    ) -> Result<PatternTranslation, MiddleErr> {
        let (inner_pattern, aliases) = pattern.clone().alias_bindings();

        let MatchArmType::ListPattern(items) = inner_pattern else {
            return Err(env.context.err_at_current(MiddleErr::Internal(
                "ListPatternCompiler called with non-list pattern".to_string(),
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

        // TODO Eventually allow for multiple rests
        let rest_index = items
            .iter()
            .position(|item| matches!(item, MatchTupleItem::Rest(_)));
        let min_len = rest_index.unwrap_or(items.len());
        let has_rest = rest_index.is_some();

        condition = env.bool_and_nodes(
            condition,
            AstNode::new(
                env.context.current_span(),
                AstNodeType::ComparisonExpression(AstComparison {
                    left: Box::new(AstNode::len(env.context.current_span(), value.clone())),
                    right: Box::new(AstNode::int(env.context.current_span(), min_len)),
                    operator: if has_rest {
                        ComparisonOperator::GreaterEqual
                    } else {
                        ComparisonOperator::Equal
                    },
                }),
            ),
        );

        let mut idx = 0;
        for item in items {
            let (inner_item, item_aliases) = item.clone().alias_bindings();

            match inner_item {
                MatchTupleItem::Rest(_) => break,
                MatchTupleItem::Wildcard(_) => {
                    idx += 1;
                }
                MatchTupleItem::Binding { var_type, name } => {
                    let current = env.match_index_access(value.clone(), idx);

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

                    bindings.append(
                        &mut item_aliases
                            .into_iter()
                            .map(|(var_type, name)| {
                                Ok(BindingDeclaration {
                                    name: env.resolve(
                                        scope,
                                        &name,
                                        ResolutionOptions::default().with_dollar(),
                                    )?,
                                    value: env.match_index_access(value.clone(), idx),
                                    var_type,
                                    data_type: None,
                                })
                            })
                            .collect::<Result<_, MiddleErr>>()?,
                    );

                    idx += 1;
                }
                MatchTupleItem::Value(expected) => {
                    let current = env.match_index_access(value.clone(), idx);

                    condition = env.bool_and_nodes(
                        condition,
                        AstNode::new(
                            env.context.current_span(),
                            AstNodeType::ComparisonExpression(AstComparison {
                                left: Box::new(current),
                                right: Box::new(expected),
                                operator: ComparisonOperator::Equal,
                            }),
                        ),
                    );

                    bindings.append(
                        &mut item_aliases
                            .into_iter()
                            .map(|(var_type, name)| {
                                Ok(BindingDeclaration {
                                    name: env.resolve(
                                        scope,
                                        &name,
                                        ResolutionOptions::default().with_dollar(),
                                    )?,
                                    value: env.match_index_access(value.clone(), idx),
                                    var_type,
                                    data_type: None,
                                })
                            })
                            .collect::<Result<_, MiddleErr>>()?,
                    );

                    idx += 1;
                }
                _ => {
                    idx += 1;
                }
            }
        }

        Ok(PatternTranslation {
            condition,
            bindings,
        })
    }
}
