use chumsky::{Parser, select};

use crate::{
    ast::nodes::literals::{AstFloat, AstInt},
    lexer::Token,
    parse::AstParser,
};

impl<'a> AstParser<'a> for AstFloat {
    fn parser()
    -> chumsky::prelude::Boxed<'a, 'a, super::TokenStream<'a>, Self, super::AstParserErr<'a>> {
        select! {
            Token::FloatLiteral(x) => x
        }
        .map(|value| AstFloat {
            value: value.replace('_', "").parse::<f64>().unwrap_or_default(),
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
