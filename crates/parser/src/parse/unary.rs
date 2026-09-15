use crate::ast::nodes::unary::{AstNeg, AstNot};
use crate::parse::RecurseAstNode;
use crate::{
    lexer::Token,
    parse::{AstParser, AstParserErr, TokenStream},
};
use chumsky::{Boxed, Parser, select};

impl<'a> AstParser<'a> for AstNot {
    type Data = RecurseAstNode<'a>;

    fn parser(data: Self::Data) -> Boxed<'a, 'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        select! { Token::Not => () }
            .ignore_then(data.node)
            .map(|value| AstNot {
                value: Box::new(value),
            })
            .boxed()
    }
}

impl<'a> AstParser<'a> for AstNeg {
    type Data = RecurseAstNode<'a>;

    fn parser(data: Self::Data) -> Boxed<'a, 'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        select! { Token::Sub => () }
            .ignore_then(data.node)
            .map(|value| AstNeg {
                value: Box::new(value),
            })
            .boxed()
    }
}
