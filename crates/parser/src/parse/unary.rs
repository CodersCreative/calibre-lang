use crate::ast::nodes::unary::{AstNeg, AstNot};
use crate::{
    ast::nodes::AstNode,
    lexer::Token,
    parse::{AstParser, AstParserErr, TokenStream},
};
use chumsky::{Boxed, Parser, select};

impl<'a> AstParser<'a> for AstNot {
    fn parser() -> Boxed<'a, 'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        select! { Token::Not => () }
            .ignore_then(AstNode::parser())
            .map(|value| AstNot {
                value: Box::new(value),
            })
            .boxed()
    }
}

impl<'a> AstParser<'a> for AstNeg {
    fn parser() -> Boxed<'a, 'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        select! { Token::Sub => () }
            .ignore_then(AstNode::parser())
            .map(|value| AstNeg {
                value: Box::new(value),
            })
            .boxed()
    }
}
