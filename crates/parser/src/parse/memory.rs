use crate::ast::RefMutability;
use crate::ast::idents::PotentialDollarIdentifier;
use crate::ast::nodes::memory::{AstDeref, AstDrop, AstMove, AstRef};
use crate::{
    ast::nodes::AstNode,
    lexer::Token,
    parse::{AstParser, AstParserErr, TokenStream},
};
use chumsky::prelude::*;
use chumsky::{Boxed, Parser, select};

impl<'a> AstParser<'a> for RefMutability {
    fn parser() -> Boxed<'a, 'a, TokenStream<'a>, Self, AstParserErr<'a>> {
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
    fn parser() -> Boxed<'a, 'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        AstNode::parser()
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
    fn parser() -> Boxed<'a, 'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        AstNode::parser()
            .then_ignore(select! {Token::Dot => ()})
            .then_ignore(select! {Token::Mul => ()})
            .map(|value| AstDeref {
                value: Box::new(value),
            })
            .boxed()
    }
}

impl<'a> AstParser<'a> for AstDrop {
    fn parser() -> Boxed<'a, 'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        select! { Token::Identifier(x) if x == "drop" => () }
            .ignore_then(PotentialDollarIdentifier::parser())
            .map(|value| AstDrop { value })
            .boxed()
    }
}

impl<'a> AstParser<'a> for AstMove {
    fn parser() -> Boxed<'a, 'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        select! { Token::Move => () }
            .ignore_then(AstNode::parser())
            .map(|value| AstMove {
                value: Box::new(value),
            })
            .boxed()
    }
}
