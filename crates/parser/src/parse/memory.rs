use crate::ast::RefMutability;
use crate::ast::nodes::memory::{AstDrop, AstMove};
use crate::parse::StatementData;
use crate::{
    lexer::Token,
    parse::{AstParser, AstParserErr, TokenStream},
};
use chumsky::prelude::*;
use chumsky::{Parser, select};

impl<'a> AstParser<'a> for RefMutability {
    type Data = ();

    #[inline(always)]
    fn parser(_data: Self::Data) -> impl Parser<'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        choice((
            select! { Token::MutRef => () }.map(|_| RefMutability::MutRef),
            select! { Token::Mut => () }.map(|_| RefMutability::MutValue),
            select! { Token::BitAnd => () }.map(|_| RefMutability::Ref),
        ))
        .or_not()
        .map(|x| x.unwrap_or(RefMutability::Value))
    }
}

impl<'a> AstParser<'a> for AstDrop {
    type Data = StatementData<'a>;

    #[inline(always)]
    fn parser(data: Self::Data) -> impl Parser<'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        select! { Token::Identifier(x) if x == "drop" => () }
            .ignore_then(data.dollar_ident.clone())
            .map(|value| AstDrop { value })
    }
}

impl<'a> AstParser<'a> for AstMove {
    type Data = StatementData<'a>;

    #[inline(always)]
    fn parser(data: Self::Data) -> impl Parser<'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        select! { Token::Move => () }
            .ignore_then(data.node.clone())
            .map(|value| AstMove {
                value: Box::new(value),
            })
    }
}
