use chumsky::{Parser, select};

use crate::{
    ast::{
        idents::ParserText,
        nodes::literals::{AstBig, AstChar, AstFloat, AstInt, AstString},
    },
    lexer::Token,
    parse::{AstParser, MapWithSpanExt},
};

impl<'a> AstParser<'a> for AstFloat {
    fn parser()
    -> chumsky::prelude::Boxed<'a, 'a, super::TokenStream<'a>, Self, super::AstParserErr<'a>> {
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
    fn parser()
    -> chumsky::prelude::Boxed<'a, 'a, super::TokenStream<'a>, Self, super::AstParserErr<'a>> {
        select! {
            Token::BigLiteral(x) => x
        }
        .map_with_span(|value, sp| AstBig {
            value: ParserText::new(sp, value.replace('_', "").replace("f", "").trim()),
        })
        .boxed()
    }
}

impl<'a> AstParser<'a> for AstInt {
    fn parser()
    -> chumsky::prelude::Boxed<'a, 'a, super::TokenStream<'a>, Self, super::AstParserErr<'a>> {
        select! {
            Token::IntLiteral(x) => x
        }
        .map(|value| AstInt { value })
        .boxed()
    }
}

// TODO Rewrite char handling
impl<'a> AstParser<'a> for AstChar {
    fn parser()
    -> chumsky::prelude::Boxed<'a, 'a, super::TokenStream<'a>, Self, super::AstParserErr<'a>> {
        select! {
            Token::CharLiteral(x) => AstChar{value : x.to_string().chars().next().unwrap_or_default()}
        }
        .boxed()
    }
}

impl<'a> AstParser<'a> for AstString {
    fn parser()
    -> chumsky::prelude::Boxed<'a, 'a, super::TokenStream<'a>, Self, super::AstParserErr<'a>> {
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
