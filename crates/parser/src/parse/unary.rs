use crate::ast::nodes::unary::{AstNeg, AstNot};
use crate::parse::RecursiveData;
use crate::{
    lexer::Token,
    parse::{AstParser, AstParserErr, TokenStream},
};
use chumsky::{Parser, select};

impl<'a> AstParser<'a> for AstNot {
    type Data = RecursiveData<'a>;

    #[inline(always)]
    fn parser(data: Self::Data) -> impl Parser<'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        select! { Token::Not => () }
            .ignore_then(data.node.clone())
            .map(|value| AstNot {
                value: Box::new(value),
            })
            .boxed()
    }
}

impl<'a> AstParser<'a> for AstNeg {
    type Data = RecursiveData<'a>;

    #[inline(always)]
    fn parser(data: Self::Data) -> impl Parser<'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        select! { Token::Sub => () }
            .ignore_then(data.node.clone())
            .map(|value| AstNeg {
                value: Box::new(value),
            })
            .boxed()
    }
}
