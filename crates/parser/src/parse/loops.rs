use crate::ast::idents::PotentialDollarIdentifier;
use crate::ast::nodes::loops::LoopType;
use crate::{
    ast::nodes::AstNode,
    lexer::Token,
    parse::{AstParser, AstParserErr, TokenStream},
};
use chumsky::prelude::*;
use chumsky::{Boxed, Parser, select};

impl<'a> AstParser<'a> for LoopType {
    fn parser() -> Boxed<'a, 'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        choice((
            PotentialDollarIdentifier::parser()
                .then_ignore(select! { Token::In => () })
                .then(AstNode::parser())
                .map(|(ident, iter)| LoopType::For(ident, iter)),
            AstNode::parser().map(LoopType::While),
            // TODO Let
        ))
        .or_not()
        .map(|x| x.unwrap_or(LoopType::Loop))
        .boxed()
    }
}
