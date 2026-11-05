use super::{BindingDeclaration, PatternTranslation, PatternTranslator};
use crate::{
    ast::types::MirDataType, environment::MiddleEnvironment, errors::MiddleErr, scoping::ScopeId,
    symbols::resolve::ResolutionOptions,
};
use calibre_parser::ast::{
    comparison::ComparisonOperator,
    idents::ParserText,
    nodes::{
        AstNode, AstNodeType,
        binary::AstComparison,
        functions::CallArg,
        literals::AstString,
        matching::{MatchArmType, MatchStringPatternPart},
    },
    types::{ParserDataType, ParserInnerType},
};

pub struct StringPatternTranslator;

impl PatternTranslator for StringPatternTranslator {
    fn translate(
        &self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        pattern: &MatchArmType,
        value: &AstNode,
        _value_type: Option<&MirDataType>,
    ) -> Result<PatternTranslation, MiddleErr> {
        let (inner_pattern, aliases) = pattern.clone().alias_bindings();

        let MatchArmType::StringPattern(parts) = inner_pattern else {
            return Err(env.context.err_at_current(MiddleErr::Internal(
                "StringPatternCompiler called with non-string pattern".to_string(),
            )));
        };

        let span = env.context.current_span();
        let mut bindings = Vec::with_capacity(aliases.len() + parts.len());

        for (var_type, name) in aliases {
            let name = env
                .resolve(scope, &name, ResolutionOptions::default().with_dollar())?
                .unwrap_dollar();

            bindings.push(BindingDeclaration {
                name,
                value: value.clone(),
                var_type,
                data_type: None,
            });
        }

        if parts.len() == 1
            && let MatchStringPatternPart::Literal(text) = &parts[0]
        {
            let literal_node = AstNode::new(
                text.span,
                AstNodeType::StringLiteral(AstString {
                    value: text.clone(),
                }),
            );

            let condition = AstNode::new(
                span,
                AstNodeType::ComparisonExpression(AstComparison {
                    left: Box::new(value.clone()),
                    right: Box::new(literal_node),
                    operator: ComparisonOperator::Equal,
                }),
            );

            return Ok(PatternTranslation {
                condition,
                bindings,
            });
        }

        let mut condition = AstNode::bool(span, true);
        let mut current = value.clone();

        let starts_with_binding = matches!(
            parts.first(),
            Some(MatchStringPatternPart::Wildcard(_))
                | Some(MatchStringPatternPart::Binding { .. })
        );

        let mut parts = parts.as_slice();

        if starts_with_binding && let Some(MatchStringPatternPart::Literal(text)) = parts.last() {
            let literal_node = AstNode::new(
                text.span,
                AstNodeType::StringLiteral(AstString {
                    value: text.clone(),
                }),
            );

            let ends_with_call = AstNode::call(
                span,
                AstNode::member(
                    text.span,
                    AstNode::identifier(text.span, "str"),
                    "ends_with",
                ),
                vec![CallArg::Value(value.clone()), CallArg::Value(literal_node)],
            );

            condition = env.bool_and_nodes(condition, ends_with_call);
            parts = &parts[..parts.len() - 1];
        }

        let mut ends_with_capture = false;

        for (i, part) in parts.iter().enumerate() {
            match part {
                MatchStringPatternPart::Literal(text) => {
                    let literal_node = AstNode::new(
                        text.span,
                        AstNodeType::StringLiteral(AstString {
                            value: text.clone(),
                        }),
                    );

                    let starts_with_call = AstNode::call(
                        span,
                        AstNode::member(
                            text.span,
                            AstNode::identifier(text.span, "str"),
                            "starts_with",
                        ),
                        vec![
                            CallArg::Value(current.clone()),
                            CallArg::Value(literal_node.clone()),
                        ],
                    );

                    condition = env.bool_and_nodes(condition, starts_with_call);

                    if i < parts.len() - 1 {
                        let strip_prefix_call = AstNode::call(
                            span,
                            AstNode::member(
                                text.span,
                                AstNode::identifier(text.span, "str"),
                                "strip_prefix",
                            ),
                            vec![
                                CallArg::Value(current.clone()),
                                CallArg::Value(literal_node),
                            ],
                        );

                        current = AstNode::member(span, strip_prefix_call, "Some");
                        ends_with_capture = false;
                    } else {
                        ends_with_capture = true;
                    }
                }
                MatchStringPatternPart::Binding { var_type, name } => {
                    let name = env
                        .resolve(scope, name, ResolutionOptions::default().with_dollar())?
                        .unwrap_dollar();

                    bindings.push(BindingDeclaration {
                        name,
                        value: current.clone(),
                        var_type: *var_type,
                        data_type: Some(ParserDataType::new(span, ParserInnerType::Str)),
                    });

                    ends_with_capture = true;
                }
                MatchStringPatternPart::Wildcard(_) => {
                    ends_with_capture = true;
                }
            }
        }

        if !ends_with_capture && !starts_with_binding {
            let node = AstNode::new(
                span,
                AstNodeType::ComparisonExpression(AstComparison {
                    left: Box::new(current),
                    right: Box::new(AstNode::new(
                        span,
                        AstNodeType::StringLiteral(AstString {
                            value: ParserText::from(String::new()),
                        }),
                    )),
                    operator: ComparisonOperator::Equal,
                }),
            );

            condition = env.bool_and_nodes(condition, node);
        }

        Ok(PatternTranslation {
            condition,
            bindings,
        })
    }
}
