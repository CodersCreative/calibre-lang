use crate::ast::binary::BinaryOperator;
use crate::ast::nodes::DestructurePattern;
use crate::ast::nodes::assignment::{AstAssignDestructure, AstAssignment};
use crate::parse::{StatementData, potential_new_line};
use crate::{
    Span,
    ast::nodes::AstNode,
    ast::nodes::AstNodeType,
    lexer::Token,
    parse::{AstParser, AstParserErr, TokenStream},
};
use chumsky::prelude::*;
use chumsky::{Parser, select};

impl<'a> AstParser<'a> for AstAssignDestructure {
    type Data = StatementData<'a>;

    #[inline(always)]
    fn parser(data: Self::Data) -> impl Parser<'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        DestructurePattern::no_bracket_parser(data.clone())
            .then_ignore(select! { Token::Walrus => () }.padded_by(potential_new_line()))
            .then(data.node.clone())
            .map(|(pattern, value)| AstAssignDestructure {
                pattern,
                value: Box::new(value),
            })
    }
}
