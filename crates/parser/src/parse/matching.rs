use crate::ast::idents::ParserText;
use crate::ast::idents::PotentialDollarIdentifier;
use crate::ast::nodes::DestructurePattern;
use crate::ast::nodes::VarType;
use crate::ast::nodes::functions::FunctionHeader;
use crate::ast::nodes::matching::{
    AstFnMatch, AstMatch, MatchArmType, MatchBody, MatchStringPatternPart, MatchStructFieldPattern,
};
use crate::ast::types::{GenericTypes, ParserDataType};
use crate::parse::MapWithSpanExt;
use crate::parse::StatementData;
use crate::parse::potential_new_line;
use crate::{
    ast::nodes::AstNode,
    lexer::Token,
    parse::{AstParser, AstParserErr},
};
use chumsky::input::ValueInput;
use chumsky::prelude::*;
use chumsky::{Parser, select};

impl<'a, I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>> AstParser<'a, I> for VarType {
    type Data = ();

    #[inline(always)]
    fn parser(_data: Self::Data) -> impl Parser<'a, I, Self, AstParserErr<'a>> {
        choice((
            just(Token::Mut).map(|_| VarType::Mutable),
            just(Token::Const).map(|_| VarType::Constant),
            just(Token::Let).map(|_| VarType::Immutable),
        ))
        .or_not()
        .map(|x| x.unwrap_or(VarType::Immutable))
    }
}

impl<'a, I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>> AstParser<'a, I>
    for DestructurePattern
{
    type Data = StatementData<'a, I>;

    fn parser(data: Self::Data) -> impl Parser<'a, I, Self, AstParserErr<'a>> {
        choice((
            // tuple
            choice((
                // rest
                just(Token::Range).map(|_| None),
                // binding
                choice((
                    just(Token::Mut).map(|_| VarType::Mutable),
                    just(Token::Const).map(|_| VarType::Constant),
                ))
                .or_not()
                .then(data.dollar_ident.clone())
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
            .separated_by(just(Token::Comma).padded_by(potential_new_line()))
            .allow_trailing()
            .collect::<Vec<_>>()
            .padded_by(potential_new_line())
            .delimited_by(just(Token::LeftParen), just(Token::RightParen))
            .map(DestructurePattern::Tuple),
            // struct
            select! { Token::Identifier(field) => field }
                .then(
                    just(Token::Colon)
                        .ignore_then(VarType::parser(()).then(data.dollar_ident.clone()))
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
                .separated_by(just(Token::Comma).padded_by(potential_new_line()))
                .allow_trailing()
                .collect::<Vec<_>>()
                .padded_by(potential_new_line())
                .delimited_by(just(Token::LeftBracket), just(Token::RightBracket))
                .map(DestructurePattern::Struct),
        ))
    }
}

impl<'a, I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>> AstParser<'a, I>
    for MatchStringPatternPart
{
    type Data = StatementData<'a, I>;

    #[inline(always)]
    fn parser(data: Self::Data) -> impl Parser<'a, I, Self, AstParserErr<'a>> {
        choice((
            // string
            select! { Token::StringLiteral(s) => s }.map_with_span(|s, span| {
                MatchStringPatternPart::Literal(ParserText::new(
                    span,
                    ParserText::decode_literal(s),
                ))
            }),
            // binding
            VarType::parser(())
                .then(data.dollar_ident.clone())
                .map(|(var_type, name)| MatchStringPatternPart::Binding { var_type, name }),
            // wildcard
            select! { Token::Identifier(x) if x == "_" => () }
                .map_with_span(|_, span| MatchStringPatternPart::Wildcard(span)),
        ))
    }
}

impl<'a, I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>> AstParser<'a, I>
    for MatchStructFieldPattern
{
    type Data = StatementData<'a, I>;

    fn parser(data: Self::Data) -> impl Parser<'a, I, Self, AstParserErr<'a>> {
        select! { Token::Identifier(field) => field }
            .map_with_span(|field, span| (field, span))
            .then(
                just(Token::Colon)
                    .ignore_then(choice((
                        // ... | ...
                        data.node
                            .clone()
                            .then(
                                just(Token::BitOr)
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
                            .then(data.dollar_ident.clone())
                            .map(|(var_type, name)| (None, Some(var_type), Some(name))),
                        // value
                        data.node
                            .clone()
                            .map(|value| (Some(vec![value]), None, None)),
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
    }
}

impl<'a, I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>> AstParser<'a, I>
    for MatchArmType
{
    type Data = StatementData<'a, I>;

    fn parser(data: Self::Data) -> impl Parser<'a, I, Self, AstParserErr<'a>> {
        recursive(|arm_type| {
            choice((
                // wildcard
                select! { Token::Identifier(x) if x == "_" => () }
                    .map_with_span(|_, span| MatchArmType::Wildcard(span)),
                // .enum @
                just(Token::Dot)
                    .ignore_then(data.dollar_ident.clone())
                    .then(
                        just(Token::Colon)
                            .ignore_then(choice((
                                // tuple
                                DestructurePattern::parser(data.clone())
                                    .map(|d| (None, None, Some(d))),
                                // struct
                                DestructurePattern::parser(data.clone())
                                    .map(|d| (None, None, Some(d))),
                                // binding
                                VarType::parser(())
                                    .then(data.dollar_ident.clone())
                                    .map(|(var_type, name)| (Some(var_type), Some(name), None)),
                            )))
                            .or_not(),
                    )
                    .then(just(Token::At).ignore_then(arm_type.clone()).or_not())
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
                // is
                just(Token::Is)
                    .ignore_then(data.data_type.clone())
                    .map(MatchArmType::IsType),
                // in
                just(Token::In)
                    .ignore_then(data.node.clone())
                    .map(MatchArmType::In),
                // string
                select! { Token::StringLiteral(s) => s }
                    .map_with_span(|s, span| {
                        MatchStringPatternPart::Literal(ParserText::new(
                            span,
                            ParserText::decode_literal(s),
                        ))
                    })
                    .then(
                        just(Token::BitAnd)
                            .ignore_then(MatchStringPatternPart::parser(data.clone()))
                            .repeated()
                            .collect::<Vec<_>>(),
                    )
                    .map(|(head, mut tail)| {
                        let mut parts = vec![head];
                        parts.append(&mut tail);
                        MatchArmType::StringPattern(parts)
                    }),
                // rest
                just(Token::Range).map_with_span(|_, span| MatchArmType::Rest(span)),
                // (...)
                arm_type
                    .clone()
                    .separated_by(just(Token::Comma).padded_by(potential_new_line()))
                    .allow_trailing()
                    .collect::<Vec<_>>()
                    .padded_by(potential_new_line())
                    .delimited_by(just(Token::LeftParen), just(Token::RightParen))
                    .map(MatchArmType::TuplePattern),
                // [...]
                arm_type
                    .clone()
                    .separated_by(just(Token::Comma).padded_by(potential_new_line()))
                    .allow_trailing()
                    .collect::<Vec<_>>()
                    .padded_by(potential_new_line())
                    .delimited_by(just(Token::LeftSquare), just(Token::RightSquare))
                    .map(MatchArmType::ListPattern),
                // struct
                MatchStructFieldPattern::parser(data.clone())
                    .separated_by(just(Token::Comma).padded_by(potential_new_line()))
                    .allow_trailing()
                    .collect::<Vec<_>>()
                    .padded_by(potential_new_line())
                    .delimited_by(just(Token::LeftBracket), just(Token::RightBracket))
                    .map(MatchArmType::StructPattern),
                // @
                VarType::parser(())
                    .then(data.dollar_ident.clone())
                    .then_ignore(just(Token::At))
                    .then(arm_type.clone())
                    .map(|((var_type, name), pattern)| MatchArmType::At {
                        var_type,
                        name,
                        pattern: Box::new(pattern),
                    }),
                // value
                data.node.clone().map(MatchArmType::Value),
                // binding
                VarType::parser(())
                    .then(data.dollar_ident.clone())
                    .map(|(var_type, name)| MatchArmType::Let { var_type, name }),
            ))
            .boxed()
        })
    }
}

pub fn parse_pattern_list<'a, I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>>(
    data: StatementData<'a, I>,
) -> impl Parser<'a, I, (Vec<MatchArmType>, Vec<AstNode>), AstParserErr<'a>> {
    MatchArmType::parser(data.clone())
        .then(
            just(Token::BitOr)
                .ignore_then(MatchArmType::parser(data))
                .repeated()
                .collect::<Vec<_>>(),
        )
        .map(|(first, rest)| {
            let mut all = vec![first];
            all.extend(rest);
            (all, Vec::new())
        })
}

impl<'a, I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>> AstParser<'a, I> for MatchBody {
    type Data = StatementData<'a, I>;

    fn parser(data: Self::Data) -> impl Parser<'a, I, Self, AstParserErr<'a>> {
        let match_arm = MatchArmType::parser(data.clone())
            .padded_by(potential_new_line())
            .then(
                choice((just(Token::BitOr).map(|_| '|'),))
                    .then(MatchArmType::parser(data.clone()))
                    .repeated()
                    .collect::<Vec<(char, MatchArmType)>>(),
            )
            .then(
                just(Token::If)
                    .padded_by(potential_new_line())
                    .ignore_then(data.node.clone())
                    .repeated()
                    .collect::<Vec<_>>(),
            )
            .then(data.scope)
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

                    // TODO
                    let mut tuple_item_variants: Vec<Vec<Vec<MatchArmType>>> = Vec::new();
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

                    let mut combos: Vec<Vec<MatchArmType>> = vec![Vec::new()];
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
            .separated_by(just(Token::Comma).padded_by(potential_new_line()))
            .allow_trailing()
            .collect::<Vec<_>>()
            .map(|arms| MatchBody {
                values: arms.into_iter().flatten().collect(),
            })
    }
}

impl<'a, I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>> AstParser<'a, I> for AstMatch {
    type Data = StatementData<'a, I>;

    #[inline(always)]
    fn parser(data: Self::Data) -> impl Parser<'a, I, Self, AstParserErr<'a>> {
        just(Token::Match)
            .ignore_then(
                data.node
                    .clone()
                    .padded_by(potential_new_line())
                    .or_not()
                    .map(|x| x.map(Box::new)),
            )
            .then(
                MatchBody::parser(data)
                    .padded_by(potential_new_line())
                    .delimited_by(just(Token::LeftBracket), just(Token::RightBracket)),
            )
            .map(|(value, body)| AstMatch { value, body })
    }
}

impl<'a, I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>> AstParser<'a, I> for AstFnMatch {
    type Data = StatementData<'a, I>;

    #[inline(always)]
    fn parser(data: Self::Data) -> impl Parser<'a, I, Self, AstParserErr<'a>> {
        just(Token::Fn)
            .ignore_then(just(Token::Match))
            .ignore_then(GenericTypes::parser(data.clone()).or_not())
            .then(data.data_type.clone().or_not())
            .then(just(Token::Eq).ignore_then(data.node.clone()).or_not())
            .then(
                just(Token::RightArrow)
                    .ignore_then(data.data_type.clone())
                    .or_not(),
            )
            .then(
                MatchBody::parser(data)
                    .padded_by(potential_new_line())
                    .delimited_by(just(Token::LeftBracket), just(Token::RightBracket)),
            )
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
    }
}
