use crate::ast::idents::ParserText;
use crate::ast::idents::PotentialDollarIdentifier;
use crate::ast::nodes::DestructurePattern;
use crate::ast::nodes::VarType;
use crate::ast::nodes::functions::FunctionHeader;
use crate::ast::nodes::literals::AstTuple;
use crate::ast::nodes::matching::{
    AstFnMatch, AstMatch, MatchArmType, MatchBody, MatchStringPatternPart, MatchStructFieldPattern,
    MatchTupleItem,
};
use crate::ast::nodes::scopes::AstScopeDef;
use crate::ast::types::{GenericTypes, ParserDataType};
use crate::parse::MapWithSpanExt;
use crate::parse::RecurseAstNode;
use crate::parse::potential_new_line;
use crate::{
    Span,
    ast::nodes::AstNode,
    ast::nodes::AstNodeType,
    lexer::Token,
    parse::{AstParser, AstParserErr, TokenStream},
};
use chumsky::prelude::*;
use chumsky::{Boxed, Parser, select};

impl<'a> AstParser<'a> for VarType {
    type Data = ();

    fn parser(_data : Self::Data) -> Boxed<'a, 'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        choice((
            select! { Token::Mut => () }.map(|_| VarType::Mutable),
            select! { Token::Const => () }.map(|_| VarType::Constant),
            select! { Token::Let => () }.map(|_| VarType::Immutable),
        ))
        .or_not()
        .map(|x| x.unwrap_or(VarType::Immutable))
        .boxed()
    }
}

impl<'a> DestructurePattern {
    pub fn no_bracket_parser() -> Boxed<'a, 'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        choice((
            Self::parser(()),
            choice((
                // rest
                select! { Token::Range => () }.map(|_| None),
                // binding
                choice((
                    select! { Token::Mut => () }.map(|_| VarType::Mutable),
                    select! { Token::Const => () }.map(|_| VarType::Constant),
                ))
                .or_not()
                .then(PotentialDollarIdentifier::parser(()))
                .map_with_span(|(var_type, name), span| {
                    Some((
                        var_type.unwrap_or(VarType::Immutable),
                        PotentialDollarIdentifier::Identifier(ParserText::new(
                            span,
                            name.text().clone(),
                        )),
                    ))
                }),
            ))
            .padded_by(potential_new_line())
            .separated_by(select! { Token::Comma => () })
            .allow_trailing()
            .collect::<Vec<_>>()
            .or_not()
            .map(|x| x.unwrap_or_default())
            .map(DestructurePattern::Tuple),
        ))
        .boxed()
    }
}

impl<'a> AstParser<'a> for DestructurePattern {
    type Data = ();
    
    fn parser(_data : Self::Data) -> Boxed<'a, 'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        choice((
            // tuple
            select! { Token::LeftParen => () }
                .ignore_then(
                    choice((
                        // rest
                        select! { Token::Range => () }.map(|_| None),
                        // binding
                        choice((
                            select! { Token::Mut => () }.map(|_| VarType::Mutable),
                            select! { Token::Const => () }.map(|_| VarType::Constant),
                        ))
                        .or_not()
                        .then(PotentialDollarIdentifier::parser(()))
                        .map_with_span(|(var_type, name), span| {
                            Some((
                                var_type.unwrap_or(VarType::Immutable),
                                PotentialDollarIdentifier::Identifier(ParserText::new(
                                    span,
                                    name.text().clone(),
                                )),
                            ))
                        }),
                    ))
                    .padded_by(potential_new_line())
                    .separated_by(select! { Token::Comma => () })
                    .allow_trailing()
                    .collect::<Vec<_>>()
                    .or_not()
                    .map(|x| x.unwrap_or_default()),
                )
                .then_ignore(select! { Token::RightParen => () })
                .map(DestructurePattern::Tuple),
            // struct
            select! { Token::LeftBracket => () }
                .ignore_then(
                    select! { Token::Identifier(field) => field }
                        .then(
                            select! { Token::Colon => () }
                                .ignore_then(
                                    VarType::parser(()).then(PotentialDollarIdentifier::parser(())),
                                )
                                .or_not(),
                        )
                        .map_with_span(|(field, alias), span| {
                            if let Some((var_type, name)) = alias {
                                (field.to_string(), var_type, name)
                            } else {
                                (
                                    field.to_string(),
                                    VarType::Immutable,
                                    PotentialDollarIdentifier::Identifier(ParserText::new(
                                        span,
                                        field.to_string(),
                                    )),
                                )
                            }
                        })
                        .separated_by(select! { Token::Comma => () })
                        .allow_trailing()
                        .collect::<Vec<_>>(),
                )
                .then_ignore(select! { Token::RightBracket => () })
                .map(DestructurePattern::Struct),
        ))
        .boxed()
    }
}

impl<'a> AstParser<'a> for MatchStringPatternPart {
    type Data = ();

    fn parser(_data : Self::Data) -> Boxed<'a, 'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        choice((
            // string
            select! { Token::StringLiteral(s) => s }.map_with_span(|s, span| {
                MatchStringPatternPart::Literal(ParserText::new(span, s.to_string()))
            }),
            // binding
            VarType::parser(())
                .then(PotentialDollarIdentifier::parser(()))
                .map(|(var_type, name)| MatchStringPatternPart::Binding { var_type, name }),
            // wildcard
            select! { Token::Identifier(x) if x == "_" => () }
                .map_with_span(|_, span| MatchStringPatternPart::Wildcard(span)),
        ))
        .boxed()
    }
}

impl<'a> AstParser<'a> for MatchTupleItem {
    type Data = RecurseAstNode<'a>;

    fn parser(data : Self::Data) -> Boxed<'a, 'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        recursive(|tuple_item| {
            choice((
                // rest
                select! { Token::Range => () }
                    .map_with_span(move |_, span| MatchTupleItem::Rest(span)),
                // wildcard
                select! { Token::Identifier(x) if x == "_" => () }
                    .map_with_span(move |_, span| MatchTupleItem::Wildcard(span)),
                // value
                data.node.clone().map(MatchTupleItem::Value),
                // is
                select! { Token::Is => () }
                    .ignore_then(ParserDataType::parser(()))
                    .map(MatchTupleItem::IsType),
                // in
                select! { Token::In => () }
                    .ignore_then(data.node.clone())
                    .map(MatchTupleItem::In),
                // string
                select! { Token::StringLiteral(s) => s }
                    .map_with_span(|s, span| {
                        MatchStringPatternPart::Literal(ParserText::new(span, s.to_string()))
                    })
                    .then(
                        select! { Token::BitAnd => () }
                            .ignore_then(MatchStringPatternPart::parser(()))
                            .repeated()
                            .collect::<Vec<_>>(),
                    )
                    .map(|(head, mut tail)| {
                        let mut parts = vec![head];
                        parts.append(&mut tail);
                        MatchTupleItem::StringPattern(parts)
                    }),
                // @
                VarType::parser(())
                    .then(PotentialDollarIdentifier::parser(()))
                    .then_ignore(select! { Token::At => () })
                    .then(tuple_item.clone())
                    .map(|((var_type, name), pattern)| MatchTupleItem::At {
                        var_type,
                        name,
                        pattern: Box::new(pattern),
                    }),
                // .enum @
                select! { Token::Dot => () }
                    .ignore_then(PotentialDollarIdentifier::parser(()))
                    .then(
                        select! { Token::Colon => () }
                            .ignore_then(choice((
                                // tuple
                                select! { Token::LeftParen => () }
                                    .ignore_then(
                                        PotentialDollarIdentifier::parser(())
                                            .map_with_span(|name, span| {
                                                Some((
                                                    VarType::Immutable,
                                                    PotentialDollarIdentifier::Identifier(
                                                        ParserText::new(span, name.text().clone()),
                                                    ),
                                                ))
                                            })
                                            .separated_by(select! { Token::Comma => () })
                                            .allow_trailing()
                                            .collect::<Vec<_>>(),
                                    )
                                    .then_ignore(select! { Token::RightParen => () })
                                    .map(|items| if items.is_empty() { vec![None] } else { items })
                                    .map(DestructurePattern::Tuple)
                                    .map(|d| (None, None, Some(d))),
                                // struct
                                select! { Token::LeftBracket => () }
                                    .ignore_then(
                                        PotentialDollarIdentifier::parser(())
                                            .map_with_span(|name, span| {
                                                (
                                                    name.to_string(),
                                                    VarType::Immutable,
                                                    PotentialDollarIdentifier::Identifier(
                                                        ParserText::new(span, name.text().clone()),
                                                    ),
                                                )
                                            })
                                            .separated_by(select! { Token::Comma => () })
                                            .allow_trailing()
                                            .collect::<Vec<_>>(),
                                    )
                                    .then_ignore(select! { Token::RightBracket => () })
                                    .map(DestructurePattern::Struct)
                                    .map(|d| (None, None, Some(d))),
                                // binding
                                VarType::parser(())
                                    .then(PotentialDollarIdentifier::parser(()))
                                    .map(|(var_type, name)| (Some(var_type), Some(name), None)),
                            )))
                            .or_not(),
                    )
                    .map(|(value, bind)| {
                        let (var_type, name, destructure) = bind.unwrap_or((None, None, None));
                        let (var_type, name) = if let (Some(vt), Some(n)) = (var_type, name) {
                            (vt, Some(n))
                        } else {
                            (VarType::Immutable, None)
                        };
                        MatchTupleItem::Enum {
                            value,
                            var_type,
                            name,
                            destructure,
                            pattern: None,
                        }
                    }),
                // struct
                select! { Token::LeftBracket => () }
                    .ignore_then(
                        MatchStructFieldPattern::parser(data)
                            .separated_by(select! { Token::Comma => () })
                            .allow_trailing()
                            .collect::<Vec<_>>()
                            .or_not()
                            .map(|x| x.unwrap_or_default()),
                    )
                    .then_ignore(select! { Token::RightBracket => () })
                    .map(MatchTupleItem::StructPattern),
                // binding
                VarType::parser(())
                    .then(PotentialDollarIdentifier::parser(()))
                    .map(|(var_type, name)| MatchTupleItem::Binding { var_type, name }),
            ))
            .boxed()
        })
        .boxed()
    }
}

impl<'a> AstParser<'a> for MatchStructFieldPattern {
    type Data = RecurseAstNode<'a>;

    fn parser(data : Self::Data) -> Boxed<'a, 'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        select! { Token::Identifier(field) => field }
            .map_with_span(|field, span| (field, span))
            .then(
                select! { Token::Colon => () }
                    .ignore_then(choice((
                        // ... | ...
                        data.node.clone()
                            .then(
                                select! { Token::BitOr => () }
                                    .ignore_then(data.node.clone())
                                    .repeated()
                                    .collect::<Vec<_>>(),
                            )
                            .map(|(first, rest)| {
                                let mut values = vec![first];
                                values.extend(rest);
                                (Some(values), None, None)
                            }),
                        // binding
                        VarType::parser(())
                            .then(PotentialDollarIdentifier::parser(()))
                            .map(|(var_type, name)| (None, Some(var_type), Some(name))),
                        // value
                        data.node.map(|value| (Some(vec![value]), None, None)),
                    )))
                    .or_not()
                    .map(|x| x.unwrap_or((None, Some(VarType::Immutable), None))),
            )
            .map(|((field, span), (values, var_type, name))| {
                if let Some(values) = values {
                    if values.len() == 1 {
                        let value = values.into_iter().next().unwrap();
                        let mut unwrapped = value.unwrap_bit_ors();
                        if unwrapped.len() == 1 {
                            MatchStructFieldPattern::Value {
                                field: field.to_string(),
                                value: unwrapped.pop().unwrap(),
                            }
                        } else {
                            MatchStructFieldPattern::AlternativeValues {
                                field: field.to_string(),
                                values: unwrapped,
                            }
                        }
                    } else {
                        MatchStructFieldPattern::AlternativeValues {
                            field: field.to_string(),
                            values,
                        }
                    }
                } else if let (Some(var_type), Some(name)) = (var_type, name) {
                    MatchStructFieldPattern::Binding {
                        field: field.to_string(),
                        var_type,
                        name,
                    }
                } else if let Some(var_type) = var_type {
                    MatchStructFieldPattern::Binding {
                        field: field.to_string(),
                        var_type,
                        name: PotentialDollarIdentifier::Identifier(ParserText::new(
                            span,
                            field.to_string(),
                        )),
                    }
                } else {
                    unreachable!()
                }
            })
            .boxed()
    }
}

impl<'a> AstParser<'a> for MatchArmType {
    type Data = RecurseAstNode<'a>;

    fn parser(data : Self::Data) -> Boxed<'a, 'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        recursive(|arm_type| {
            choice((
                // wildcard
                select! { Token::Identifier(x) if x == "_" => () }
                    .map_with_span(|_, span| MatchArmType::Wildcard(span)),
                // value
                data.node.clone().map(MatchArmType::Value),
                // is
                select! { Token::Is => () }
                    .ignore_then(ParserDataType::parser(()))
                    .map(MatchArmType::IsType),
                // in
                select! { Token::In => () }
                    .ignore_then(data.node.clone())
                    .map(MatchArmType::In),
                // string
                select! { Token::StringLiteral(s) => s }
                    .map_with_span(|s, span| {
                        MatchStringPatternPart::Literal(ParserText::new(span, s.to_string()))
                    })
                    .then(
                        select! { Token::BitAnd => () }
                            .ignore_then(MatchStringPatternPart::parser(()))
                            .repeated()
                            .collect::<Vec<_>>(),
                    )
                    .map(|(head, mut tail)| {
                        let mut parts = vec![head];
                        parts.append(&mut tail);
                        MatchArmType::StringPattern(parts)
                    }),
                // .enum @
                select! { Token::Dot => () }
                    .ignore_then(PotentialDollarIdentifier::parser(()))
                    .then(
                        select! { Token::Colon => () }
                            .ignore_then(choice((
                                // tuple
                                DestructurePattern::parser(()).map(|d| (None, None, Some(d))),
                                // struct
                                DestructurePattern::parser(()).map(|d| (None, None, Some(d))),
                                // binding
                                VarType::parser(())
                                    .then(PotentialDollarIdentifier::parser(()))
                                    .map(|(var_type, name)| (Some(var_type), Some(name), None)),
                            )))
                            .or_not(),
                    )
                    .then(
                        select! { Token::At => () }
                            .ignore_then(arm_type.clone())
                            .or_not(),
                    )
                    .map(|((value, bind), pattern)| {
                        let (var_type, name, destructure) = bind.unwrap_or((None, None, None));
                        let (var_type, name) = if let (Some(vt), Some(n)) = (var_type, name) {
                            (vt, Some(n))
                        } else {
                            (VarType::Immutable, None)
                        };
                        MatchArmType::Enum {
                            value,
                            var_type,
                            name,
                            destructure,
                            pattern: pattern.map(Box::new),
                        }
                    }),
                // rest
                select! { Token::Range => () }.map_with_span(|_, span| {
                    MatchArmType::TuplePattern(vec![MatchTupleItem::Rest(span)])
                }),
                // (...)
                select! { Token::LeftParen => () }
                    .ignore_then(
                        MatchTupleItem::parser(data.clone())
                            .separated_by(select! { Token::Comma => () })
                            .allow_trailing()
                            .collect::<Vec<_>>()
                            .or_not()
                            .map(|x| x.unwrap_or_default()),
                    )
                    .then_ignore(select! { Token::RightParen => () })
                    .map(MatchArmType::TuplePattern),
                // [...]
                select! { Token::LeftSquare => () }
                    .ignore_then(
                        MatchTupleItem::parser(data.clone())
                            .separated_by(select! { Token::Comma => () })
                            .allow_trailing()
                            .collect::<Vec<_>>()
                            .or_not()
                            .map(|x| x.unwrap_or_default()),
                    )
                    .then_ignore(select! { Token::RightSquare => () })
                    .map(MatchArmType::ListPattern),
                // struct
                select! { Token::LeftBracket => () }
                    .ignore_then(
                        MatchStructFieldPattern::parser(data.into())
                            .separated_by(select! { Token::Comma => () })
                            .allow_trailing()
                            .collect::<Vec<_>>()
                            .or_not()
                            .map(|x| x.unwrap_or_default()),
                    )
                    .then_ignore(select! { Token::RightBracket => () })
                    .map(MatchArmType::StructPattern),
                // binding
                VarType::parser(())
                    .then(PotentialDollarIdentifier::parser(()))
                    .map(|(var_type, name)| MatchArmType::Let { var_type, name }),
                // @
                VarType::parser(())
                    .then(PotentialDollarIdentifier::parser(()))
                    .then_ignore(select! { Token::At => () })
                    .then(arm_type.clone())
                    .map(|((var_type, name), pattern)| MatchArmType::At {
                        var_type,
                        name,
                        pattern: Box::new(pattern),
                    }),
            ))
            .boxed()
        })
        .boxed()
    }
}

pub fn parse_pattern_list<'a>(data : RecurseAstNode<'a>)
-> Boxed<'a, 'a, TokenStream<'a>, (Vec<MatchArmType>, Vec<AstNode>), AstParserErr<'a>> {
    MatchArmType::parser(data.clone())
        .then(
            select! { Token::BitOr => () }
                .ignore_then(MatchArmType::parser(data))
                .repeated()
                .collect::<Vec<_>>(),
        )
        .map(|(first, rest)| {
            let mut all = vec![first];
            all.extend(rest);
            (all, Vec::new())
        })
        .boxed()
}

impl<'a> AstParser<'a> for MatchBody {
    type Data = RecurseAstNode<'a>;

    fn parser(data : Self::Data) -> Boxed<'a, 'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        let match_arm = MatchArmType::parser(data.clone())
            .padded_by(potential_new_line())
            .then(
                choice((
                    select! { Token::BitOr => () }.map(|_| '|'),
                    select! { Token::Comma => () }.map(|_| ','),
                ))
                .then(MatchArmType::parser(data.clone()))
                .repeated()
                .collect::<Vec<(char, MatchArmType)>>(),
            )
            .then(
                select! { Token::If => () }
                    .padded_by(potential_new_line())
                    .ignore_then(data.node.clone())
                    .repeated()
                    .collect::<Vec<_>>(),
            )
            .then(
                select! { Token::FatArrow => () }
                    .padded_by(potential_new_line())
                    .ignore_then(choice((
                        AstScopeDef::parser(data.clone().into()).map_with_span(|scope, span| {
                            AstNode::new(span, AstNodeType::from(scope))
                        }),
                        data.node,
                    ))),
            )
            .map(|(((first, rest), conditions), body)| {
                let mut values = vec![first.clone()];
                values.extend(rest.iter().map(|(_, v)| v.clone()));
                let has_comma = rest.iter().any(|(sep, _)| *sep == ',');

                if has_comma {
                    let mut slots: Vec<Vec<MatchArmType>> = vec![vec![first.clone()]];
                    for (sep, arm) in rest {
                        if sep == ',' {
                            slots.push(vec![arm]);
                        } else if let Some(last) = slots.last_mut() {
                            last.push(arm);
                        }
                    }

                    let mut tuple_item_variants: Vec<Vec<Vec<MatchTupleItem>>> = Vec::new();
                    for slot in slots {
                        let mut variants = Vec::new();
                        for arm in slot {
                            let mut items = Vec::new();
                            if let Some(mapped) = arm.into_tuple_items() {
                                items.extend(mapped);
                            } else {
                                continue;
                            }
                            variants.push(items);
                        }
                        tuple_item_variants.push(variants);
                    }

                    let mut combos: Vec<Vec<MatchTupleItem>> = vec![Vec::new()];
                    for slot_variants in tuple_item_variants {
                        let mut next = Vec::new();
                        for prefix in &combos {
                            for variant in &slot_variants {
                                let mut merged = prefix.clone();
                                merged.extend(variant.clone());
                                next.push(merged);
                            }
                        }
                        combos = next;
                    }

                    combos
                        .into_iter()
                        .map(|items| {
                            (
                                MatchArmType::TuplePattern(items),
                                conditions.clone(),
                                Box::new(body.clone()),
                            )
                        })
                        .collect()
                } else {
                    let shared_enum_payload = values.iter().rev().find_map(|v| match &v {
                        MatchArmType::Enum {
                            var_type,
                            name,
                            destructure,
                            pattern,
                            ..
                        } if var_type != &VarType::Immutable
                            || name.is_some()
                            || destructure.is_some()
                            || pattern.is_some() =>
                        {
                            Some((
                                *var_type,
                                name.clone(),
                                destructure.clone(),
                                pattern.clone(),
                            ))
                        }
                        _ => None,
                    });

                    if let Some((shared_vt, shared_name, shared_destructure, shared_pattern)) =
                        shared_enum_payload
                    {
                        for value in values.iter_mut() {
                            if let MatchArmType::Enum {
                                var_type,
                                name,
                                destructure,
                                pattern,
                                ..
                            } = value
                                && var_type == &VarType::Immutable
                                && name.is_none()
                                && destructure.is_none()
                                && pattern.is_none()
                            {
                                *var_type = shared_vt;
                                *name = shared_name.clone();
                                *destructure = shared_destructure.clone();
                                *pattern = shared_pattern.clone();
                            }
                        }
                    }

                    let mut out = Vec::with_capacity(values.len());
                    for value in values {
                        out.push((value, conditions.clone(), Box::new(body.clone())));
                    }
                    out
                }
            });

        match_arm
            .padded_by(potential_new_line())
            .separated_by(select! { Token::Comma => () })
            .allow_trailing()
            .collect::<Vec<_>>()
            .map(|arms| MatchBody {
                values: arms.into_iter().flatten().collect(),
            })
            .boxed()
    }
}

impl<'a> AstParser<'a> for AstMatch {
    type Data = RecurseAstNode<'a>;

    fn parser(data : Self::Data) -> Boxed<'a, 'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        select! { Token::Match => () }
            .ignore_then(
                data.node.clone()
                    .then(
                        select! { Token::Comma => () }
                            .ignore_then(data.node.clone())
                            .repeated()
                            .collect::<Vec<_>>(),
                    )
                    .map(|(first, rest)| {
                        if rest.is_empty() {
                            first
                        } else {
                            let span =
                                Span::new(first.span.from, rest.last().unwrap_or(&first).span.to);
                            let mut values = Vec::with_capacity(rest.len() + 1);
                            values.push(first);
                            values.extend(rest);
                            AstNode::new(span, AstNodeType::TupleLiteral(AstTuple { values }))
                        }
                    })
                    .or_not()
                    .map(|x| x.map(Box::new)),
            )
            .then_ignore(select! { Token::LeftBracket => () })
            .then(MatchBody::parser(data))
            .then_ignore(select! { Token::RightBracket => () })
            .map(|(value, body)| AstMatch { value, body })
            .boxed()
    }
}

impl<'a> AstParser<'a> for AstFnMatch {
    type Data = RecurseAstNode<'a>;

    fn parser(data : Self::Data) -> Boxed<'a, 'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        select! { Token::Fn => () }
            .ignore_then(select! { Token::Match => () })
            .ignore_then(GenericTypes::parser(()).or_not())
            .then(ParserDataType::parser(()).or_not())
            .then(
                select! { Token::Eq => () }
                    .ignore_then(data.node.clone())
                    .or_not(),
            )
            .then(
                select! { Token::RightArrow => () }
                    .ignore_then(ParserDataType::parser(()))
                    .or_not(),
            )
            .then_ignore(select! { Token::LeftBracket => () })
            .then(MatchBody::parser(data))
            .then_ignore(select! { Token::RightBracket => () })
            .map_with_span(
                |((((generics, param_ty), default), return_ty), body), span| {
                    let header = FunctionHeader {
                        generics: generics.unwrap_or_default(),
                        parameters: vec![(
                            PotentialDollarIdentifier::Identifier(ParserText::new(
                                span,
                                "match_value".to_string(),
                            )),
                            param_ty,
                            default.map(Box::new),
                        )],
                        return_type: return_ty.unwrap_or_else(|| ParserDataType::null(span)),
                        param_destructures: Vec::new(),
                    };

                    AstFnMatch { header, body }
                },
            )
            .boxed()
    }
}
