use crate::{
    ast::{
        ObjectType,
        idents::ParserText,
        nodes::{
            AstNode,
            literals::{
                AstBig, AstChar, AstDataType, AstEnum, AstFloat, AstInt, AstRange, AstString,
                AstStruct, AstTuple,
            },
        },
    },
    lexer::Token,
    parse::{
        AstParser, AstParserErr, MapWithSpanExt, RecursiveData, TokenStream, potential_new_line,
    },
};
use chumsky::prelude::*;
use chumsky::{Boxed, Parser, select};
use ustr::Ustr;

impl<'a> AstParser<'a> for AstFloat {
    type Data = ();

    #[inline(always)]
    fn parser(_data: &Self::Data) -> Boxed<'a, 'a, TokenStream<'a>, Self, AstParserErr<'a>> {
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
    type Data = ();

    #[inline(always)]
    fn parser(_data: &Self::Data) -> Boxed<'a, 'a, TokenStream<'a>, Self, AstParserErr<'a>> {
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
    type Data = ();

    #[inline(always)]
    fn parser(_data: &Self::Data) -> Boxed<'a, 'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        select! {
            Token::IntLiteral(x) => x
        }
        .map(|value| AstInt { value })
        .boxed()
    }
}

// TODO Rewrite char handling
impl<'a> AstParser<'a> for AstChar {
    type Data = ();

    #[inline(always)]
    fn parser(_data: &Self::Data) -> Boxed<'a, 'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        select! {
            Token::CharLiteral(x) => AstChar{value : x.to_string().chars().next().unwrap_or_default()}
        }
        .boxed()
    }
}

impl<'a> AstParser<'a> for AstString {
    type Data = ();

    #[inline(always)]
    fn parser(_data: &Self::Data) -> Boxed<'a, 'a, TokenStream<'a>, Self, AstParserErr<'a>> {
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
    type Data = RecursiveData<'a>;

    #[inline(always)]
    fn parser(data: &Self::Data) -> Boxed<'a, 'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        choice((
            data.node
                .clone()
                .then_ignore(
                    select! { Token::InclusiveRange => () }.padded_by(potential_new_line()),
                )
                .then(data.node.clone())
                .map(|(from, to)| AstRange {
                    from: Box::new(from),
                    to: Box::new(to),
                    inclusive: true,
                }),
            data.node
                .clone()
                .then_ignore(select! { Token::Range => () }.padded_by(potential_new_line()))
                .then(data.node.clone())
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
    type Data = RecursiveData<'a>;

    #[inline(always)]
    fn parser(data: &Self::Data) -> Boxed<'a, 'a, TokenStream<'a>, Self, AstParserErr<'a>> {
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
            .boxed()
    }
}

impl<'a> AstParser<'a> for AstStruct {
    type Data = RecursiveData<'a>;

    #[inline(always)]
    fn parser(data: &Self::Data) -> Boxed<'a, 'a, TokenStream<'a>, Self, AstParserErr<'a>> {
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
    type Data = RecursiveData<'a>;

    #[inline(always)]
    fn parser(data: &Self::Data) -> Boxed<'a, 'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        data.generic_ident
            .clone()
            .then_ignore(select! { Token::Dot => () })
            .then(data.dollar_ident.clone())
            .then_ignore(select! { Token::Colon => () }.padded_by(potential_new_line()))
            .then(data.node.clone().or_not())
            .map(|((identifier, value), data)| AstEnum {
                identifier,
                value,
                data: data.map(Box::new),
            })
            .boxed()
    }
}

impl<'a> AstParser<'a> for AstDataType {
    type Data = RecursiveData<'a>;

    #[inline(always)]
    fn parser(data: &Self::Data) -> Boxed<'a, 'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        select! {Token::Type => ()}
            .then(select! {Token::Colon => ()})
            .ignore_then(data.data_type.clone())
            .map(|data_type| AstDataType { data_type })
            .boxed()
    }
}
