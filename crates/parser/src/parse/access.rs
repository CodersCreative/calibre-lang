use crate::ast::nodes::access::{AstField, AstIndex, AstScope};
use crate::{
    ast::{idents::PotentialDollarIdentifier, nodes::AstNode},
    lexer::Token,
    parse::{AstParser, AstParserErr, TokenStream},
};
use chumsky::{Boxed, Parser, select};

impl<'a> AstParser<'a> for AstField {
    fn parser() -> Boxed<'a, 'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        AstNode::parser()
            .then_ignore(select! { Token::Dot => () })
            .then(PotentialDollarIdentifier::parser())
            .map(|(base, field)| AstField {
                base: Box::new(base),
                field,
            })
            .boxed()
    }
}

impl<'a> AstParser<'a> for AstScope {
    fn parser() -> Boxed<'a, 'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        AstNode::parser()
            .then_ignore(select! { Token::Scope => () })
            .then(PotentialDollarIdentifier::parser())
            .map(|(base, field)| AstScope {
                base: Box::new(base),
                field,
            })
            .boxed()
    }
}

impl<'a> AstParser<'a> for AstIndex {
    fn parser() -> Boxed<'a, 'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        AstNode::parser()
            .then_ignore(select! { Token::LeftSquare => () })
            .then(AstNode::parser())
            .then_ignore(select! { Token::RightSquare => () })
            .map(|(base, index)| AstIndex {
                base: Box::new(base),
                index: Box::new(index),
            })
            .boxed()
    }
}
