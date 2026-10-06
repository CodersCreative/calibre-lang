use crate::{
    ast::{
        ObjectType,
        idents::{ParsedIntLiteral, ParserText, PotentialDollarIdentifier},
        nodes::{
            AstNode,
            literals::{
                AstBig, AstChar, AstDataType, AstEnum, AstFloat, AstInt, AstString, AstStruct,
                AstTuple,
            },
        },
    },
    lexer::Token,
    parse::{AstParser, AstParserErr, MapWithSpanExt, StatementData, potential_new_line},
};
use chumsky::{Parser, select};
use chumsky::{input::ValueInput, prelude::*};
use ustr::Ustr;

impl<'a, I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>> AstParser<'a, I> for AstFloat {
    type Data = ();

    #[inline(always)]
    fn parser(_data: Self::Data) -> impl Parser<'a, I, Self, AstParserErr<'a>> {
        select! {
            Token::FloatLiteral(x) => x
        }
        .map_with_span(|value, sp| AstFloat {
            value: value
                .replace('_', "")
                .replace("f", "")
                .trim()
                .parse::<f64>()
                .unwrap_or_default(),
            format: Some(ParserText::new(sp, value)),
        })
    }
}

impl<'a, I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>> AstParser<'a, I> for AstBig {
    type Data = ();

    #[inline(always)]
    fn parser(_data: Self::Data) -> impl Parser<'a, I, Self, AstParserErr<'a>> {
        select! {
            Token::BigLiteral(x) => x
        }
        .map_with_span(|value, sp| AstBig {
            value: ParserText::new(sp, value.replace('_', "").replace("g", "").trim()),
            format: Some(ParserText::new(sp, value)),
        })
    }
}

impl<'a, I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>> AstParser<'a, I> for AstInt {
    type Data = ();

    #[inline(always)]
    fn parser(_data: Self::Data) -> impl Parser<'a, I, Self, AstParserErr<'a>> {
        select! {
            Token::IntLiteral(x) => x
        }
        .map_with_span(|value, sp| AstInt {
            format: Some(ParserText::new(sp, &value)),
            value: ParsedIntLiteral::parse(value).unwrap_or_default(),
        })
    }
}

impl<'a, I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>> AstParser<'a, I> for AstChar {
    type Data = ();

    #[inline(always)]
    fn parser(_data: Self::Data) -> impl Parser<'a, I, Self, AstParserErr<'a>> {
        select! {
            Token::CharLiteral(x) => AstChar {
                value: ParserText::decode_literal(x).chars().next().unwrap_or_default(),
            }
        }
    }
}

impl<'a, I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>> AstParser<'a, I> for AstString {
    type Data = ();

    #[inline(always)]
    fn parser(_data: Self::Data) -> impl Parser<'a, I, Self, AstParserErr<'a>> {
        select! {
            Token::StringLiteral(x) => x
        }
        .map_with_span(|value, span| AstString {
            value: ParserText {
                text: ParserText::decode_literal(value),
                span,
            },
        })
    }
}

impl<'a, I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>> AstParser<'a, I> for AstTuple {
    type Data = StatementData<'a, I>;

    #[inline(always)]
    fn parser(data: Self::Data) -> impl Parser<'a, I, Self, AstParserErr<'a>> {
        data.node
            .clone()
            .separated_by(just(Token::Comma).padded_by(potential_new_line()))
            .allow_trailing()
            .collect::<Vec<_>>()
            .padded_by(potential_new_line())
            .delimited_by(just(Token::LeftParen), just(Token::RightParen))
            .map(|values| AstTuple { values })
    }
}

impl<'a, I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>> AstParser<'a, I> for AstStruct {
    type Data = StatementData<'a, I>;

    fn parser(data: Self::Data) -> impl Parser<'a, I, Self, AstParserErr<'a>> {
        choice((
            just(Token::Dot)
                .ignore_then(
                    data.node
                        .clone()
                        .separated_by(just(Token::Comma).padded_by(potential_new_line()))
                        .allow_trailing()
                        .collect::<Vec<_>>()
                        .padded_by(potential_new_line())
                        .delimited_by(just(Token::LeftParen), just(Token::RightParen)),
                )
                .map(|fields| AstStruct {
                    identifier: None,
                    value: ObjectType::Tuple(fields),
                }),
            choice((
                data.generic_ident.clone().map(Some),
                just(Token::Dot).to(None),
            ))
            .then(
                select! { Token::Identifier(x) => x }
                    .then(just(Token::Colon).ignore_then(data.node.clone()).or_not())
                    .map_with_span(|(field, value), span| {
                        (
                            Ustr::from(field),
                            value.unwrap_or_else(|| AstNode::identifier(span, field)),
                        )
                    })
                    .separated_by(just(Token::Comma).padded_by(potential_new_line()))
                    .allow_trailing()
                    .collect::<Vec<_>>()
                    .padded_by(potential_new_line())
                    .delimited_by(just(Token::LeftBracket), just(Token::RightBracket)),
            )
            .map(|(identifier, fields)| AstStruct {
                identifier,
                value: ObjectType::Map(fields),
            }),
        ))
    }
}

impl<'a, I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>> AstParser<'a, I> for AstEnum {
    type Data = StatementData<'a, I>;

    fn parser(data: Self::Data) -> impl Parser<'a, I, Self, AstParserErr<'a>> {
        just(Token::Dot)
            .ignore_then(
                data.dollar_ident
                    .clone()
                    .or(select! { Token::IntLiteral(value) => value }.map_with_span(
                        |value, span| {
                            PotentialDollarIdentifier::Identifier(ParserText::new(
                                span,
                                value.to_string(),
                            ))
                        },
                    ))
                    .then(
                        just(Token::Colon)
                            .padded_by(potential_new_line())
                            .ignore_then(data.node.clone().or_not())
                            .or_not(),
                    ),
            )
            .map(|(value, fields)| AstEnum {
                identifier: None,
                value,
                data: fields.flatten().map(Box::new),
            })
    }
}

impl<'a, I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>> AstParser<'a, I> for AstDataType {
    type Data = StatementData<'a, I>;

    #[inline(always)]
    fn parser(data: Self::Data) -> impl Parser<'a, I, Self, AstParserErr<'a>> {
        just(Token::Type)
            .then(just(Token::Colon))
            .ignore_then(data.data_type.clone())
            .map(|data_type| AstDataType { data_type })
    }
}
