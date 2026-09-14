use crate::ast::idents::ParserText;
use crate::ast::idents::PotentialDollarIdentifier;
use crate::ast::nodes::AstNodeType;
use crate::ast::nodes::VarType;
use crate::ast::nodes::functions::FunctionHeader;
use crate::ast::nodes::matching::{
    AstFnMatch, AstMatch, MatchArmType, MatchBody, MatchStringPatternPart, MatchStructFieldPattern,
    MatchTupleItem,
};
use crate::ast::nodes::scopes::AstScopeDef;
use crate::ast::types::{ GenericTypes, ParserDataType};
use crate::parse::MapWithSpanExt;
use crate::{
    ast::nodes::AstNode,
    lexer::Token,
    parse::{AstParser, AstParserErr, TokenStream},
};
use chumsky::prelude::*;
use chumsky::{Boxed, Parser, select};

impl<'a> AstParser<'a> for VarType {
    fn parser() -> Boxed<'a, 'a, TokenStream<'a>, Self, AstParserErr<'a>> {
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

impl<'a> AstParser<'a> for MatchStringPatternPart {
    fn parser() -> Boxed<'a, 'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        choice((
            // string
            select! { Token::StringLiteral(s) => s }.map_with_span(|s, span| {
                MatchStringPatternPart::Literal(ParserText::new(span, s.to_string()))
            }),
            // binding
            VarType::parser()
                .then(PotentialDollarIdentifier::parser())
                .map(|(var_type, name)| MatchStringPatternPart::Binding { var_type, name }),
            // wildcard
            select! { Token::Identifier(x) if x == "_" => () }
                .map_with_span(|_, span| MatchStringPatternPart::Wildcard(span)),
        ))
        .boxed()
    }
}

impl<'a> AstParser<'a> for MatchTupleItem {
    fn parser() -> Boxed<'a, 'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        recursive(|tuple_item| {
            choice((
                // rest
                select! { Token::Range => () }
                    .map_with_span(move |_, span| MatchTupleItem::Rest(span)),
                // wildcard
                select! { Token::Identifier(x) if x == "_" => () }
                    .map_with_span(move |_, span| MatchTupleItem::Wildcard(span)),
                // value
                AstNode::parser().map(MatchTupleItem::Value),
                // is
                select! { Token::Is => () }
                    .ignore_then(ParserDataType::parser())
                    .map(MatchTupleItem::IsType),
                // in
                select! { Token::In => () }
                    .ignore_then(AstNode::parser())
                    .map(MatchTupleItem::In),
                // string
                select! { Token::StringLiteral(s) => s }
                    .map_with_span(|s, span| {
                        MatchStringPatternPart::Literal(ParserText::new(span, s.to_string()))
                    })
                    .then(
                        select! { Token::BitAnd => () }
                            .ignore_then(MatchStringPatternPart::parser())
                            .repeated()
                            .collect::<Vec<_>>(),
                    )
                    .map(|(head, mut tail)| {
                        let mut parts = vec![head];
                        parts.append(&mut tail);
                        MatchTupleItem::StringPattern(parts)
                    }),
                // @
                VarType::parser()
                    .then(PotentialDollarIdentifier::parser())
                    .then_ignore(select! { Token::At => () })
                    .then(tuple_item.clone())
                    .map(|((var_type, name), pattern)| MatchTupleItem::At {
                        var_type,
                        name,
                        pattern: Box::new(pattern),
                    }),
                // .enum @
                select! { Token::Dot => () }
                    .ignore_then(PotentialDollarIdentifier::parser())
                    .then(
                        VarType::parser()
                            .then(PotentialDollarIdentifier::parser())
                            .or_not(),
                    )
                    .map_with_span(|(value, bind), span| {
                        let (var_type, name) = bind.unwrap_or((
                            VarType::Immutable,
                            PotentialDollarIdentifier::Identifier(ParserText::new(
                                span,
                                String::new(),
                            )),
                        ));
                        MatchTupleItem::Enum {
                            value,
                            var_type,
                            name: Some(name),
                            destructure: None,
                            pattern: None,
                        }
                    }),
                // struct
                select! { Token::LeftBracket => () }
                    .ignore_then(
                        MatchStructFieldPattern::parser()
                            .separated_by(select! { Token::Comma => () })
                            .allow_trailing()
                            .collect::<Vec<_>>()
                            .or_not()
                            .map(|x| x.unwrap_or_default()),
                    )
                    .then_ignore(select! { Token::RightBracket => () })
                    .map(MatchTupleItem::StructPattern),
                // binding
                VarType::parser()
                    .then(PotentialDollarIdentifier::parser())
                    .map(|(var_type, name)| MatchTupleItem::Binding { var_type, name }),
            ))
            .boxed()
        })
        .boxed()
    }
}

impl<'a> AstParser<'a> for MatchStructFieldPattern {
    fn parser() -> Boxed<'a, 'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        select! { Token::Identifier(field) => field }
            .then_ignore(select! { Token::Colon => () })
            .then(choice((
                // ... | ...
                AstNode::parser()
                    .then(
                        select! { Token::BitOr => () }
                            .ignore_then(AstNode::parser())
                            .repeated()
                            .collect::<Vec<_>>(),
                    )
                    .map(|(first, rest)| {
                        let mut values = vec![first];
                        values.extend(rest);
                        (Some(values), None, None)
                    }),
                // binding
                VarType::parser()
                    .then(PotentialDollarIdentifier::parser())
                    .map(|(var_type, name)| (None, Some(var_type), Some(name))),
                // value
                AstNode::parser().map(|value| (Some(vec![value]), None, None)),
            )))
            .map(|(field, (values, var_type, name))| {
                if let Some(values) = values {
                    if values.len() == 1 {
                        MatchStructFieldPattern::Value {
                            field: field.to_string(),
                            value: values.into_iter().next().unwrap(),
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
                } else {
                    unreachable!()
                }
            })
            .boxed()
    }
}

impl<'a> AstParser<'a> for MatchArmType {
    fn parser() -> Boxed<'a, 'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        recursive(|arm_type| {
            choice((
                // wildcard
                select! { Token::Identifier(x) if x == "_" => () }
                    .map_with_span(|_, span| MatchArmType::Wildcard(span)),
                // value
                AstNode::parser().map(MatchArmType::Value),
                // is
                select! { Token::Is => () }
                    .ignore_then(ParserDataType::parser())
                    .map(MatchArmType::IsType),
                // in
                select! { Token::In => () }
                    .ignore_then(AstNode::parser())
                    .map(MatchArmType::In),
                // string
                select! { Token::StringLiteral(s) => s }
                    .map_with_span(|s, span| {
                        MatchStringPatternPart::Literal(ParserText::new(span, s.to_string()))
                    })
                    .then(
                        select! { Token::BitAnd => () }
                            .ignore_then(MatchStringPatternPart::parser())
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
                    .ignore_then(PotentialDollarIdentifier::parser())
                    .then(
                        VarType::parser()
                            .then(PotentialDollarIdentifier::parser())
                            .or_not(),
                    )
                    .then(
                        select! { Token::At => () }
                            .ignore_then(arm_type.clone())
                            .or_not(),
                    )
                    .map_with_span(|((value, bind), pattern), span| {
                        let (var_type, name) = bind.unwrap_or((
                            VarType::Immutable,
                            PotentialDollarIdentifier::Identifier(ParserText::new(
                                span,
                                String::new(),
                            )),
                        ));
                        MatchArmType::Enum {
                            value,
                            var_type,
                            name: Some(name),
                            destructure: None,
                            pattern: pattern.map(Box::new),
                        }
                    }),
                // (...)
                select! { Token::LeftParen => () }
                    .ignore_then(
                        MatchTupleItem::parser()
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
                        MatchTupleItem::parser()
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
                        MatchStructFieldPattern::parser()
                            .separated_by(select! { Token::Comma => () })
                            .allow_trailing()
                            .collect::<Vec<_>>()
                            .or_not()
                            .map(|x| x.unwrap_or_default()),
                    )
                    .then_ignore(select! { Token::RightBracket => () })
                    .map(MatchArmType::StructPattern),
                // binding
                VarType::parser()
                    .then(PotentialDollarIdentifier::parser())
                    .map(|(var_type, name)| MatchArmType::Let { var_type, name }),
                // @
                VarType::parser()
                    .then(PotentialDollarIdentifier::parser())
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

pub fn parse_pattern_list<'a>()
-> Boxed<'a, 'a, TokenStream<'a>, (Vec<MatchArmType>, Vec<AstNode>), AstParserErr<'a>> {
    MatchArmType::parser()
        .then(
            select! { Token::BitOr => () }
                .ignore_then(MatchArmType::parser())
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
    fn parser() -> Boxed<'a, 'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        let match_arm =
            MatchArmType::parser()
                .then(
                    select! { Token::If => () }
                        .ignore_then(AstNode::parser())
                        .repeated()
                        .collect::<Vec<_>>(),
                )
                .then(
                        AstScopeDef::parser().map_with_span(|scope, span| {
                            AstNode::new(span, AstNodeType::from(scope))
                        }))
                .map(|((pattern, conditionals), body)| {
                    (pattern, conditionals, Box::new(body))
                });

        match_arm
            .separated_by(select! { Token::Comma => () })
            .allow_trailing()
            .collect::<Vec<_>>()
            .map(|arms| MatchBody { values: arms })
            .boxed()
    }
}

impl<'a> AstParser<'a> for AstMatch {
    fn parser() -> Boxed<'a, 'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        select! { Token::Match => () }
            .ignore_then(AstNode::parser().or_not())
            .then_ignore(select! { Token::LeftBracket => () })
            .then(MatchBody::parser())
            .then_ignore(select! { Token::RightBracket => () })
            .map(|(value, body)| AstMatch {
                value: value.map(Box::new),
                body,
            })
            .boxed()
    }
}

impl<'a> AstParser<'a> for AstFnMatch {
    fn parser() -> Boxed<'a, 'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        select! { Token::Fn => () }
            .ignore_then(select! { Token::Match => () })
            .ignore_then(
                GenericTypes::parser()
                    .or_not()
            )
            .then(ParserDataType::parser().or_not())
            .then(
                select! { Token::Eq => () }
                    .ignore_then(AstNode::parser())
                    .or_not(),
            )
            .then(
                select! { Token::RightArrow => () }
                    .ignore_then(ParserDataType::parser())
                    .or_not(),
            )
            .then_ignore(select! { Token::LeftBracket => () })
            .then(MatchBody::parser())
            .then_ignore(select! { Token::RightBracket => () })
            .map_with_span(|((((generics, param_ty), default), return_ty), body), span| {
                let header = FunctionHeader {
                    generics : generics.unwrap_or_default(),
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

                AstFnMatch {
                    header,
                    body,
                }
            })
            .boxed()
    }
}
