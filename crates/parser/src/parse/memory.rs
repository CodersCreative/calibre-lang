use crate::ast::RefMutability;
use crate::ast::nodes::memory::{AstDrop, AstMove};
use crate::parse::StatementData;
use crate::{
    lexer::Token,
    parse::{AstParser, AstParserErr},
};
use chumsky::input::ValueInput;
use chumsky::prelude::*;
use chumsky::{Parser, select};

impl<'a, I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>> AstParser<'a, I>
    for RefMutability
{
    type Data = ();

    #[inline(always)]
    fn parser(_data: Self::Data) -> impl Parser<'a, I, Self, AstParserErr<'a>> {
        choice((
            just(Token::MutRef).map(|_| RefMutability::MutRef),
            just(Token::Mut).map(|_| RefMutability::MutValue),
            just(Token::BitAnd).map(|_| RefMutability::Ref),
        ))
        .or_not()
        .map(|x| x.unwrap_or(RefMutability::Value))
    }
}

impl<'a, I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>> AstParser<'a, I> for AstDrop {
    type Data = StatementData<'a, I>;

    #[inline(always)]
    fn parser(data: Self::Data) -> impl Parser<'a, I, Self, AstParserErr<'a>> {
        select! { Token::Identifier(x) if x == "drop" => () }
            .ignore_then(data.dollar_ident.clone())
            .map(|value| AstDrop { value })
    }
}

impl<'a, I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>> AstParser<'a, I> for AstMove {
    type Data = StatementData<'a, I>;

    #[inline(always)]
    fn parser(data: Self::Data) -> impl Parser<'a, I, Self, AstParserErr<'a>> {
        just(Token::Move)
            .ignore_then(data.node.clone())
            .map(|value| AstMove {
                value: Box::new(value),
            })
    }
}
