use crate::{
    ast::{
        ObjectType,
        idents::{ParsedIntLiteral, ParserText},
        nodes::{
            AstNode,
            literals::{
                AstBig, AstChar, AstDataType, AstFloat, AstInt, AstString, AstStruct, AstTuple,
            },
        },
    },
    lexer::Token,
    parse::{
        AstParser, AstParserErr, MapWithSpanExt, StatementData, TokenStream, potential_new_line,
    },
};
use chumsky::prelude::*;
use chumsky::{Parser, select};
use ustr::Ustr;

impl<'a> AstParser<'a> for AstFloat {
    type Data = ();

    #[inline(always)]
    fn parser(_data: Self::Data) -> impl Parser<'a, TokenStream<'a>, Self, AstParserErr<'a>> {
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

impl<'a> AstParser<'a> for AstBig {
    type Data = ();

    #[inline(always)]
    fn parser(_data: Self::Data) -> impl Parser<'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        select! {
            Token::BigLiteral(x) => x
        }
        .map_with_span(|value, sp| AstBig {
            value: ParserText::new(sp, value.replace('_', "").replace("g", "").trim()),
            format: Some(ParserText::new(sp, value)),
        })
    }
}

impl<'a> AstParser<'a> for AstInt {
    type Data = ();

    #[inline(always)]
    fn parser(_data: Self::Data) -> impl Parser<'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        select! {
            Token::IntLiteral(x) => x
        }
        .map_with_span(|value, sp| AstInt {
            format: Some(ParserText::new(sp, &value)),
            value: ParsedIntLiteral::parse(value).unwrap_or_default(),
        })
    }
}

impl<'a> AstParser<'a> for AstChar {
    type Data = ();

    #[inline(always)]
    fn parser(_data: Self::Data) -> impl Parser<'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        select! {
            Token::CharLiteral(x) => AstChar {
                value: ParserText::decode_literal(x).chars().next().unwrap_or_default(),
            }
        }
    }
}

impl<'a> AstParser<'a> for AstString {
    type Data = ();

    #[inline(always)]
    fn parser(_data: Self::Data) -> impl Parser<'a, TokenStream<'a>, Self, AstParserErr<'a>> {
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

impl<'a> AstParser<'a> for AstTuple {
    type Data = StatementData<'a>;

    #[inline(always)]
    fn parser(data: Self::Data) -> impl Parser<'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        select! { Token::LeftParen => () }
            .ignore_then(
                data.node
                    .clone()
                    .padded_by(potential_new_line())
                    .separated_by(select! { Token::Comma => () })
                    .allow_trailing()
                    .collect::<Vec<_>>()
                    .or_not()
                    .map(|x| x.unwrap_or_default()),
            )
            .then_ignore(select! { Token::RightParen => () })
            .map(|values| AstTuple { values })
    }
}

impl<'a> AstParser<'a> for AstStruct {
    type Data = StatementData<'a>;

    fn parser(data: Self::Data) -> impl Parser<'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        data.generic_ident
            .clone()
            .then(
                select! { Token::LeftBracket => () }
                    .ignore_then(
                        select! { Token::Identifier(x) => x }
                            .then(
                                select! { Token::Colon => () }
                                    .ignore_then(data.node.clone())
                                    .or_not(),
                            )
                            .map_with_span(|(field, value), span| {
                                (
                                    Ustr::from(field),
                                    value.unwrap_or_else(|| AstNode::identifier(span, field)),
                                )
                            })
                            .padded_by(potential_new_line())
                            .separated_by(select! { Token::Comma => () })
                            .allow_trailing()
                            .collect::<Vec<_>>()
                            .or_not()
                            .map(|x| x.unwrap_or_default()),
                    )
                    .then_ignore(select! { Token::RightBracket => () }),
            )
            .map(|(identifier, fields)| AstStruct {
                identifier,
                value: ObjectType::Map(fields),
            })
    }
}

impl<'a> AstParser<'a> for AstDataType {
    type Data = StatementData<'a>;

    #[inline(always)]
    fn parser(data: Self::Data) -> impl Parser<'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        select! {Token::Type => ()}
            .then(select! {Token::Colon => ()})
            .ignore_then(data.data_type.clone())
            .map(|data_type| AstDataType { data_type })
    }
}
