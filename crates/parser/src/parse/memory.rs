use crate::ast::RefMutability;
use crate::ast::idents::PotentialDollarIdentifier;
use crate::ast::nodes::memory::{AstDeref, AstDrop, AstMove, AstRef};
use crate::parse::RecurseAstNode;
use crate::{
    lexer::Token,
    parse::{AstParser, AstParserErr, TokenStream},
};
use chumsky::prelude::*;
use chumsky::{Boxed, Parser, select};

impl<'a> AstParser<'a> for RefMutability {
    type Data = ();

    fn parser(_data : Self::Data) -> Boxed<'a, 'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        choice((
            select! { Token::MutRef => () }.map(|_| RefMutability::MutRef),
            select! { Token::Mut => () }.map(|_| RefMutability::MutValue),
            select! { Token::BitAnd => () }.map(|_| RefMutability::Ref),
        ))
        .or_not()
        .map(|x| x.unwrap_or(RefMutability::Value))
        .boxed()
    }
}

impl<'a> AstParser<'a> for AstRef {
    type Data = RecurseAstNode<'a>;

    fn parser(data : Self::Data) -> Boxed<'a, 'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        data.node
            .then_ignore(select! {Token::Dot => ()})
            .then(choice((
                select! { Token::MutRef => () }.map(|_| RefMutability::MutRef),
                select! { Token::BitAnd => () }.map(|_| RefMutability::Ref),
            )))
            .map(|(value, mutability)| AstRef {
                mutability,
                value: Box::new(value),
            })
            .boxed()
    }
}

impl<'a> AstParser<'a> for AstDeref {
    type Data = RecurseAstNode<'a>;

    fn parser(data : Self::Data) -> Boxed<'a, 'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        data.node
            .then_ignore(select! {Token::Dot => ()})
            .then_ignore(select! {Token::Mul => ()})
            .map(|value| AstDeref {
                value: Box::new(value),
            })
            .boxed()
    }
}

impl<'a> AstParser<'a> for AstDrop {
    type Data = ();

    fn parser(_data : Self::Data) -> Boxed<'a, 'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        select! { Token::Identifier(x) if x == "drop" => () }
            .ignore_then(PotentialDollarIdentifier::parser(()))
            .map(|value| AstDrop { value })
            .boxed()
    }
}

impl<'a> AstParser<'a> for AstMove {
    type Data = RecurseAstNode<'a>;

    fn parser(data : Self::Data) -> Boxed<'a, 'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        select! { Token::Move => () }
            .ignore_then(data.node)
            .map(|value| AstMove {
                value: Box::new(value),
            })
            .boxed()
    }
}
