use crate::ast::nodes::generator::AstGenerator;
use crate::ast::nodes::loops::LoopType;
use crate::{
    ast::{nodes::AstNode, types::ParserDataType},
    lexer::Token,
    parse::{AstParser, AstParserErr, TokenStream},
};
use chumsky::prelude::*;
use chumsky::{Boxed, Parser, select};

impl<'a> AstParser<'a> for AstGenerator {
    fn parser() -> Boxed<'a, 'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        select! { Token::Fn => () }
            .ignore_then(select! { Token::LeftParen => () })
            .then(AstNode::parser())
            .then_ignore(select! { Token::For => () })
            .then(LoopType::parser())
            .then(
                select! { Token::If => () }
                    .ignore_then(AstNode::parser())
                    .repeated()
                    .collect::<Vec<_>>(),
            )
            .then(
                select! { Token::Until => () }
                    .ignore_then(AstNode::parser())
                    .or_not(),
            )
            .then_ignore(select! { Token::RightParen => () })
            .then(
                select! { Token::RightArrow => () }
                    .ignore_then(ParserDataType::parser())
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
