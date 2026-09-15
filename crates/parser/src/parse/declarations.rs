use crate::ast::nodes::DestructurePattern;
use crate::ast::nodes::VarType;
use crate::ast::nodes::declaration::{AstDeclaration, AstDeclareDestructure};
use crate::ast::types::ParserDataType;
use crate::parse::StatementData;
use crate::parse::potential_new_line;
use crate::{
    lexer::Token,
    parse::{AstParser, AstParserErr, TokenStream, typed_or_untyped_assignment},
};
use chumsky::error::Rich;
use chumsky::prelude::*;
use chumsky::{Parser, select};

impl<'a> AstParser<'a> for AstDeclaration {
    type Data = StatementData<'a>;

    fn parser(data: Self::Data) -> impl Parser<'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        choice((
            select! { Token::Let => () }.map(|_| VarType::Immutable),
            select! { Token::Const => () }.map(|_| VarType::Constant),
        ))
        .then(select! { Token::Mut => () }.or_not())
        .then(data.dollar_ident.clone())
        .then(typed_or_untyped_assignment(data))
        .try_map(|(((var_type, mut_tok), identifier), (data_type, value)), sp| {
            let var_type = if mut_tok.is_some() {
                match var_type {
                    VarType::Constant => {
                        return Err(Rich::custom(
                            sp,
                            "constant cannot be mutable; use `let mut` if mutability is required",
                        ));
                    }
                    _ => VarType::Mutable,
                }
            } else {
                var_type
            };

            let value = value.ok_or_else(|| {
                Rich::custom(sp, "expected a value assignment")
            })?;
            let value_span = value.span;

            Ok(AstDeclaration {
                var_type,
                identifier,
                value: Box::new(value),
                data_type: data_type.unwrap_or_else(|| ParserDataType::auto(value_span)),
            })
        })
    }
}

impl<'a> AstParser<'a> for AstDeclareDestructure {
    type Data = StatementData<'a>;

    #[inline(always)]
    fn parser(data: Self::Data) -> impl Parser<'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        select! { Token::Let => () }
            .ignore_then(DestructurePattern::no_bracket_parser(data.clone()))
            .then_ignore(select! { Token::Walrus => () }.padded_by(potential_new_line()))
            .then(data.node.clone())
            .map(|(pattern, value)| AstDeclareDestructure {
                var_type: VarType::Immutable,
                pattern,
                value: Box::new(value),
            })
    }
}
