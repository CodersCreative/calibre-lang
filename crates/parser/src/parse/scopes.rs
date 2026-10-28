use crate::ast::nodes::scopes::{AstScopeDef, NamedScope};
use crate::parse::{ScopeData, potential_new_line};
use crate::{
    lexer::Token,
    parse::{AstParser, AstParserErr},
};
use chumsky::Parser;
use chumsky::input::ValueInput;
use chumsky::prelude::*;

impl<'a, I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>> AstParser<'a, I> for AstScopeDef {
    type Data = ScopeData<'a, I>;

    #[inline(always)]
    fn parser(data: Self::Data) -> impl Parser<'a, I, Self, AstParserErr<'a>> {
        let body = choice((
            data.node
                .clone()
                .padded_by(potential_new_line())
                .repeated()
                .collect::<Vec<_>>()
                .padded_by(potential_new_line())
                .delimited_by(
                    just(Token::LeftBracket).ignore_then(just(Token::LeftBracket)),
                    just(Token::RightBracket).ignore_then(just(Token::RightBracket)),
                )
                .map(|items| (items, false)),
            data.node
                .clone()
                .padded_by(potential_new_line())
                .repeated()
                .collect::<Vec<_>>()
                .padded_by(potential_new_line())
                .delimited_by(just(Token::LeftBracket), just(Token::RightBracket))
                .map(|items| (items, true)),
            // Im going to make node by itself produce a scope so that no scope is now an explicit action
            data.node
                .clone()
                .padded_by(potential_new_line())
                .map(|body| (vec![body], true)),
        ));

        let named = just(Token::At)
            .ignore_then(data.dollar_ident.clone())
            .then(
                just(Token::Dollar)
                    .ignore_then(data.dollar_ident.clone())
                    .then(just(Token::Walrus).ignore_then(data.node.clone()))
                    .map(|(ident, value)| (ident, value))
                    .separated_by(just(Token::Comma).padded_by(potential_new_line()))
                    .allow_trailing()
                    .collect::<Vec<_>>()
                    .padded_by(potential_new_line())
                    .delimited_by(just(Token::LeftSquare), just(Token::RightSquare))
                    .or_not()
                    .map(|x| x.unwrap_or_default()),
            )
            .map(|(name, args)| NamedScope { name, args });

        choice((
            just(Token::Let)
                .ignore_then(just(Token::FatArrow))
                .map(|_| true),
            just(Token::FatArrow).map(|_| false),
        ))
        .then(named.or_not())
        .then(body)
        .map(|((define, named), (body, create_new_scope))| AstScopeDef {
            body,
            named,
            is_temp: true,
            create_new_scope,
            define,
            is_function_body: false,
        })
        .boxed()
    }
}
