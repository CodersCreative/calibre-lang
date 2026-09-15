use crate::ast::nodes::generator::AstGenerator;
use crate::ast::nodes::loops::LoopType;
use crate::parse::RecursiveData;
use crate::{
    lexer::Token,
    parse::{AstParser, AstParserErr, TokenStream},
};
use chumsky::prelude::*;
use chumsky::{Boxed, Parser, select};

impl<'a> AstParser<'a> for AstGenerator {
    type Data = RecursiveData<'a>;

    fn parser(data: Self::Data) -> Boxed<'a, 'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        select! { Token::Fn => () }
            .ignore_then(select! { Token::LeftParen => () })
            .then(data.node.clone())
            .then_ignore(select! { Token::For => () })
            .then(LoopType::parser(data.clone()))
            .then(
                select! { Token::If => () }
                    .ignore_then(data.node.clone())
                    .repeated()
                    .collect::<Vec<_>>(),
            )
            .then(
                select! { Token::Until => () }
                    .ignore_then(data.node)
                    .or_not(),
            )
            .then_ignore(select! { Token::RightParen => () })
            .then(
                select! { Token::RightArrow => () }
                    .ignore_then(data.data_type)
                    .or_not(),
            )
            .map(
                |(((((_, map_expr), loop_type), conditionals), until), data_type)| AstGenerator {
                    map: Box::new(map_expr),
                    data_type,
                    loop_type: Box::new(loop_type),
                    conditionals,
                    until: until.map(Box::new),
                },
            )
            .boxed()
    }
}
