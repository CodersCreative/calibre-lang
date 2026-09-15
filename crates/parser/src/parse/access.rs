use crate::ast::nodes::access::{AstField, AstIndex, AstScope};
use crate::parse::{StatementData, potential_new_line};
use crate::{
    lexer::Token,
    parse::{AstParser, AstParserErr, TokenStream},
};
use chumsky::{Parser, select};

impl<'a> AstParser<'a> for AstField {
    type Data = StatementData<'a>;

    #[inline(always)]
    fn parser(data: Self::Data) -> impl Parser<'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        data.node
            .clone()
            .then_ignore(select! { Token::Dot => () }.padded_by(potential_new_line()))
            .then(data.dollar_ident.clone())
            .map(|(base, field)| AstField {
                base: Box::new(base),
                field,
            })
    }
}

impl<'a> AstParser<'a> for AstScope {
    type Data = StatementData<'a>;

    #[inline(always)]
    fn parser(data: Self::Data) -> impl Parser<'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        data.node
            .clone()
            .then_ignore(select! { Token::Scope => () }.padded_by(potential_new_line()))
            .then(data.dollar_ident.clone())
            .map(|(base, field)| AstScope {
                base: Box::new(base),
                field,
            })
    }
}

impl<'a> AstParser<'a> for AstIndex {
    type Data = StatementData<'a>;

    #[inline(always)]
    fn parser(data: Self::Data) -> impl Parser<'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        data.node
            .clone()
            .then_ignore(select! { Token::LeftSquare => () }.padded_by(potential_new_line()))
            .then(data.node.clone())
            .then_ignore(select! { Token::RightSquare => () }.padded_by(potential_new_line()))
            .map(|(base, index)| AstIndex {
                base: Box::new(base),
                index: Box::new(index),
            })
    }
}
