use crate::ast::nodes::generator::AstGenerator;
use crate::ast::nodes::loops::LoopType;
use crate::parse::{StatementData, potential_new_line};
use crate::{
    lexer::Token,
    parse::{AstParser, AstParserErr},
};
use chumsky::Parser;
use chumsky::input::ValueInput;
use chumsky::prelude::*;

impl<'a, I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>> AstParser<'a, I>
    for AstGenerator
{
    type Data = StatementData<'a, I>;

    fn parser(data: Self::Data) -> impl Parser<'a, I, Self, AstParserErr<'a>> {
        just(Token::Fn)
            .ignore_then(
                data.node
                    .clone()
                    .then_ignore(just(Token::For))
                    .then(LoopType::parser(data.clone()))
                    .then(
                        just(Token::If)
                            .ignore_then(data.node.clone())
                            .repeated()
                            .collect::<Vec<_>>(),
                    )
                    .then(just(Token::Until).ignore_then(data.node.clone()).or_not())
                    .padded_by(potential_new_line())
                    .delimited_by(just(Token::LeftParen), just(Token::RightParen)),
            )
            .then(
                just(Token::RightArrow)
                    .ignore_then(data.data_type.clone())
                    .or_not(),
            )
            .map(
                |((((map_expr, loop_type), conditionals), until), data_type)| AstGenerator {
                    map: Box::new(map_expr),
                    data_type,
                    loop_type: Box::new(loop_type),
                    conditionals,
                    until: until.map(Box::new),
                },
            )
    }
}
