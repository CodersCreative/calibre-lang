use super::{BindingDeclaration, PatternTranslation, PatternTranslator};
use crate::{
    ast::types::MirDataType, environment::MiddleEnvironment, errors::MiddleErr, scoping::ScopeId,
    symbols::resolve::ResolutionOptions,
};
use calibre_parser::{
    Span,
    ast::{
        comparison::ComparisonOperator,
        nodes::{
            AstNode, AstNodeType, access::AstIndex, binary::AstComparison, matching::MatchArmType,
        },
    },
};

pub struct ListPatternTranslator;

impl PatternTranslator for ListPatternTranslator {
    fn translate(
        &self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        pattern: &MatchArmType,
        value: &AstNode,
        _value_type: Option<&MirDataType>,
    ) -> Result<PatternTranslation, MiddleErr> {
        let (inner_pattern, aliases) = pattern.clone().alias_bindings();

        let MatchArmType::ListPattern(items) = inner_pattern else {
            return Err(env.context.err_at_current(MiddleErr::Internal(
                "ListPatternTranslator called with non-list pattern".to_string(),
            )));
        };

        let span = env.context.current_span();
        let mut condition = AstNode::bool(span, true);

        let mut bindings = aliases
            .into_iter()
            .map(|(var_type, name)| {
                Ok(BindingDeclaration {
                    name: env
                        .resolve(scope, &name, ResolutionOptions::default().with_dollar())?
                        .unwrap_dollar(),
                    value: value.clone(),
                    var_type,
                    data_type: None,
                })
            })
            .collect::<Result<Vec<_>, MiddleErr>>()?;

        let rest_index = items
            .iter()
            .position(|item| matches!(item.clone().alias_bindings().0, MatchArmType::Rest(_)));

        let has_rest = rest_index.is_some();
        let rest_index = rest_index.unwrap_or(items.len());

        let min_len = if has_rest {
            items.len().saturating_sub(1)
        } else {
            items.len()
        };

        let len_check = AstNode::new(
            span,
            AstNodeType::ComparisonExpression(AstComparison {
                left: Box::new(AstNode::len(span, value.clone())),
                right: Box::new(AstNode::int(span, min_len as i64)),
                operator: if has_rest {
                    ComparisonOperator::GreaterEqual
                } else {
                    ComparisonOperator::Equal
                },
            }),
        );

        condition = env.bool_and_nodes(condition, len_check);

        fn translate_item(
            env: &mut MiddleEnvironment,
            scope: ScopeId,
            span: Span,
            item: &MatchArmType,
            current: AstNode,
            condition: &mut AstNode,
            bindings: &mut Vec<BindingDeclaration>,
        ) -> Result<(), MiddleErr> {
            let (arm, aliases) = item.clone().alias_bindings();

            match arm {
                MatchArmType::Rest(_) => {}
                MatchArmType::Wildcard(_) => {}
                MatchArmType::Let { var_type, name } => {
                    let resolved_name = env
                        .resolve(scope, &name, ResolutionOptions::default().with_dollar())?
                        .unwrap_dollar();
                    bindings.push(BindingDeclaration {
                        name: resolved_name,
                        value: current.clone(),
                        var_type,
                        data_type: None,
                    });
                }
                MatchArmType::Value(expected) => {
                    let eq_cond = AstNode::new(
                        span,
                        AstNodeType::ComparisonExpression(AstComparison {
                            left: Box::new(current.clone()),
                            right: Box::new(expected),
                            operator: ComparisonOperator::Equal,
                        }),
                    );
                    let old_cond = std::mem::replace(condition, AstNode::bool(span, true));
                    *condition = env.bool_and_nodes(old_cond, eq_cond);
                }
                _ => {}
            }

            for (var_type, name) in aliases {
                let name = env
                    .resolve(scope, &name, ResolutionOptions::default().with_dollar())?
                    .unwrap_dollar();

                bindings.push(BindingDeclaration {
                    name,
                    value: current.clone(),
                    var_type,
                    data_type: None,
                });
            }

            Ok(())
        }

        for (i, item) in items[..rest_index].iter().enumerate() {
            let current = AstNode::new(
                span,
                AstNodeType::IndexAccess(AstIndex {
                    base: Box::new(value.clone()),
                    index: Box::new(AstNode::int(span, i as i64)),
                    panic: true,
                }),
            );

            translate_item(
                env,
                scope,
                span,
                item,
                current,
                &mut condition,
                &mut bindings,
            )?;
        }

        if has_rest {
            let suffix = &items[rest_index + 1..];
            let suffix_len = suffix.len() as i64;

            for (s_idx, item) in suffix.iter().enumerate() {
                let index = -(suffix_len - s_idx as i64);

                let current = AstNode::new(
                    span,
                    AstNodeType::IndexAccess(AstIndex {
                        base: Box::new(value.clone()),
                        index: Box::new(AstNode::int(span, index)),
                        panic: true,
                    }),
                );

                translate_item(
                    env,
                    scope,
                    span,
                    item,
                    current,
                    &mut condition,
                    &mut bindings,
                )?;
            }
        }

        Ok(PatternTranslation {
            condition,
            bindings,
        })
    }
}
