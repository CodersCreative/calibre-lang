use chumsky::prelude::*;
use chumsky::{Boxed, Parser, select};

use crate::{
    ast::{
        ObjectType,
        idents::{ParserText, PotentialDollarIdentifier, PotentialGenericTypeIdentifier},
        nodes::{
            AstNode,
            literals::{
                AstBig, AstChar, AstEnum, AstFloat, AstInt, AstRange, AstString, AstStruct,
                AstTuple,
            },
        },
    },
    lexer::Token,
    parse::{AstParser, AstParserErr, MapWithSpanExt, TokenStream},
};
use ustr::Ustr;

impl<'a> AstParser<'a> for AstFloat {
    fn parser() -> Boxed<'a, 'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        select! {
            Token::FloatLiteral(x) => x
        }
        .map(|value| AstFloat {
            value: value
                .replace('_', "")
                .replace("f", "")
                .trim()
                .parse::<f64>()
                .unwrap_or_default(),
        })
        .boxed()
    }
}

impl<'a> AstParser<'a> for AstBig {
    fn parser() -> Boxed<'a, 'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        select! {
            Token::BigLiteral(x) => x
        }
        .map_with_span(|value, sp| AstBig {
            value: ParserText::new(sp, value.replace('_', "").replace("g", "").trim()),
        })
        .boxed()
    }
}

impl<'a> AstParser<'a> for AstInt {
    fn parser() -> Boxed<'a, 'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        select! {
            Token::IntLiteral(x) => x
        }
        .map(|value| AstInt { value })
        .boxed()
    }
}

// TODO Rewrite char handling
impl<'a> AstParser<'a> for AstChar {
    fn parser() -> Boxed<'a, 'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        select! {
            Token::CharLiteral(x) => AstChar{value : x.to_string().chars().next().unwrap_or_default()}
        }
        .boxed()
    }
}

impl<'a> AstParser<'a> for AstString {
    fn parser() -> Boxed<'a, 'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        select! {
            Token::StringLiteral(x) => x
        }
        .map_with_span(|value, span| AstString {
            value: ParserText {
                text: value.to_string(),
                span,
            },
        })
        .boxed()
    }
}

impl<'a> AstParser<'a> for AstRange {
    fn parser() -> Boxed<'a, 'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        choice((
            AstNode::parser()
                .then_ignore(select! { Token::InclusiveRange => () })
                .then(AstNode::parser())
                .map(|(from, to)| AstRange {
                    from: Box::new(from),
                    to: Box::new(to),
                    inclusive: true,
                }),
            AstNode::parser()
                .then_ignore(select! { Token::Range => () })
                .then(AstNode::parser())
                .map(|(from, to)| AstRange {
                    from: Box::new(from),
                    to: Box::new(to),
                    inclusive: false,
                }),
        ))
        .boxed()
    }
}

impl<'a> AstParser<'a> for AstTuple {
    fn parser() -> Boxed<'a, 'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        select! { Token::LeftParen => () }
            .ignore_then(
                AstNode::parser()
                    .separated_by(select! { Token::Comma => () })
                    .allow_trailing()
                    .collect::<Vec<_>>()
                    .or_not()
                    .map(|x| x.unwrap_or_default()),
            )
            .then_ignore(select! { Token::RightParen => () })
            .map(|values| AstTuple { values })
            .boxed()
    }
}

impl<'a> AstParser<'a> for AstStruct {
    fn parser() -> Boxed<'a, 'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        PotentialGenericTypeIdentifier::parser()
            .then(
                select! { Token::LeftBracket => () }
                    .ignore_then(
                        select! { Token::Identifier(x) => x }
                            .then(
                                select! { Token::Colon => () }
                                    .ignore_then(AstNode::parser())
                                    .or_not(),
                            )
                            .map_with_span(|(field, value), span| {
                                (
                                    Ustr::from(field),
                                    value.unwrap_or_else(|| AstNode::identifier(span, field)),
                                )
                            })
                            .separated_by(select! { Token::Comma => () })
                            .allow_trailing()
                            .collect::<Vec<_>>()
                            .or_not()
                            .map(|x| x.unwrap_or_default()),
                    )
                    .then_ignore(select! { Token::RightBracket => () })
                    .or_not(),
            )
            .map(|(identifier, fields)| AstStruct {
                identifier,
                value: ObjectType::Map(fields.unwrap_or_default()),
            })
            .boxed()
    }
}

impl<'a> AstParser<'a> for AstEnum {
    fn parser() -> Boxed<'a, 'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        PotentialGenericTypeIdentifier::parser()
            .then_ignore(select! { Token::Dot => () })
            .then(PotentialDollarIdentifier::parser())
            .then_ignore(select! { Token::Colon => () })
            .then(AstNode::parser().or_not())
            .map(|((identifier, value), data)| AstEnum {
                identifier,
                value,
                data: data.map(Box::new),
            })
            .boxed()
    }
}
