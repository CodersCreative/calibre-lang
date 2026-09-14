use crate::ast::idents::PotentialDollarIdentifier;
use crate::ast::nodes::DestructurePattern;
use crate::ast::nodes::VarType;
use crate::ast::nodes::declaration::{AstDeclaration, AstDeclareDestructure};
use crate::ast::types::ParserDataType;
use crate::{
    ast::nodes::AstNode,
    lexer::Token,
    parse::{AstParser, AstParserErr, TokenStream},
};
use chumsky::error::Rich;
use chumsky::prelude::*;
use chumsky::{Boxed, Parser, select};

impl<'a> AstParser<'a> for AstDeclaration {
    fn parser() -> Boxed<'a, 'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        choice((
            select! { Token::Let => () }.map(|_| VarType::Immutable),
            select! { Token::Const => () }.map(|_| VarType::Constant),
        ))
        .then(select! { Token::Mut => () }.or_not())
        .then(PotentialDollarIdentifier::parser())
        .then(
            select! { Token::Colon => () }
                .ignore_then(ParserDataType::parser())
                .or_not(),
        )
        .then(
            choice((
                select! { Token::Walrus => () }.map(|_| false),
                select! { Token::Eq => () }.map(|_| true),
            ))
            .then(AstNode::parser()),
        )
        .try_map(|((((var_type, mut_tok), identifier), data_type), (is_typed, value)), sp| {
            match (data_type.is_some(), is_typed) {
                (true, false) => {
                    return Err(Rich::custom(
                        sp,
                        "expected `=` when a variable type is specified",
                    ));
                }
                (false, true) => {
                    return Err(Rich::custom(
                        sp,
                        "expected `:=` when a variable type is not specified",
                    ));
                }
                _ => {}
            }

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

            let value_span = value.span;
            Ok(AstDeclaration {
                var_type,
                identifier,
                value: Box::new(value),
                data_type: data_type.unwrap_or_else(|| ParserDataType::auto(value_span)),
            })
        })
        .boxed()
    }
}

impl<'a> AstParser<'a> for AstDeclareDestructure {
    fn parser() -> Boxed<'a, 'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        select! { Token::Let => () }
            .ignore_then(DestructurePattern::parser())
            .then_ignore(select! { Token::Walrus => () })
            .then(AstNode::parser())
            .map(|(pattern, value)| AstDeclareDestructure {
                var_type: VarType::Immutable,
                pattern,
                value: Box::new(value),
            })
            .boxed()
    }
}
