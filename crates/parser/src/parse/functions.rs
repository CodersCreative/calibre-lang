use crate::ast::idents::ParserText;
use crate::ast::idents::PotentialDollarIdentifier;
use crate::ast::nodes::DestructurePattern;
use crate::ast::nodes::functions::{
    AstCall, AstCurry, AstExtern, AstFunction, CallArg, FunctionHeader,
};
use crate::ast::nodes::scopes::AstScopeDef;
use crate::ast::types::GenericTypes;
use crate::ast::types::ParserDataType;
use crate::parse::MapWithSpanExt;
use crate::parse::RecurseAstNode;
use crate::parse::potential_new_line;
use crate::{
    Span,
    ast::nodes::AstNode,
    ast::nodes::AstNodeType,
    lexer::Token,
    parse::{AstParser, AstParserErr, TokenStream, typed_or_untyped_assignment},
};
use chumsky::prelude::*;
use chumsky::{Boxed, Parser, select};

impl<'a> AstParser<'a> for CallArg {
    type Data = RecurseAstNode<'a>;

    fn parser(data: Self::Data) -> Boxed<'a, 'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        choice((
            PotentialDollarIdentifier::parser(())
                .then_ignore(select! { Token::Colon => () })
                .then(data.node.clone())
                .map(|(name, value)| CallArg::Named(name, value)),
            data.node.map(CallArg::Value),
        ))
        .boxed()
    }
}

#[derive(Clone)]
enum FnParamGroup {
    Plain(
        Vec<(
            PotentialDollarIdentifier,
            Option<ParserDataType>,
            Option<Box<AstNode>>,
        )>,
    ),
    Destructure {
        pattern: DestructurePattern,
        data_type: Option<ParserDataType>,
        default: Option<Box<AstNode>>,
        span: Span,
    },
}

impl<'a> AstParser<'a> for FnParamGroup {
    type Data = RecurseAstNode<'a>;

    fn parser(data: Self::Data) -> Boxed<'a, 'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        let normal = select! { Token::Mut => () }
            .or_not()
            .ignore_then(PotentialDollarIdentifier::parser(()))
            .repeated()
            .at_least(1)
            .collect::<Vec<_>>()
            .then(typed_or_untyped_assignment(data.clone()))
            .map(|(names, (ty, default))| {
                FnParamGroup::Plain(
                    names
                        .into_iter()
                        .map(|n| (n, ty.clone(), default.clone().map(Box::new)))
                        .collect::<Vec<_>>(),
                )
            });

        let destructure = DestructurePattern::parser(())
            .then(typed_or_untyped_assignment(data))
            .map_with_span(
                move |(pattern, (ty, default)), sp| FnParamGroup::Destructure {
                    span: sp,
                    pattern,
                    data_type: ty,
                    default: default.map(Box::new),
                },
            );

        choice((destructure, normal)).boxed()
    }
}

impl<'a> AstParser<'a> for FunctionHeader {
    type Data = RecurseAstNode<'a>;

    fn parser(data: Self::Data) -> Boxed<'a, 'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        let fn_param_groups = FnParamGroup::parser(data.clone())
            .padded_by(potential_new_line())
            .separated_by(select! { Token::Comma => () })
            .allow_trailing()
            .collect::<Vec<_>>()
            .or_not()
            .map(|x| x.unwrap_or_default())
            .boxed();

        let fn_params = select! { Token::LeftParen => () }
            .ignore_then(fn_param_groups.clone())
            .then_ignore(select! { Token::RightParen => () })
            .or_not()
            .map(|x| x.unwrap_or_default())
            .boxed();

        GenericTypes::parser(())
            .then(fn_params)
            .then(
                select! { Token::RightArrow => () }
                    .ignore_then(ParserDataType::parser(()))
                    .or_not(),
            )
            .map_with_span(|((generics, params), ret), span| {
                let mut parameters = Vec::new();
                let mut param_destructures = Vec::new();
                let mut index: i16 = 0;

                for group in params {
                    match group {
                        FnParamGroup::Plain(items) => parameters.extend(items),
                        FnParamGroup::Destructure {
                            pattern,
                            data_type,
                            default,
                            span,
                        } => {
                            let param_index = parameters.len();
                            index += 1;

                            parameters.push((
                                PotentialDollarIdentifier::Identifier(ParserText::new(
                                    span,
                                    format!("destructure_param_{}", index),
                                )),
                                data_type,
                                default,
                            ));
                            param_destructures.push((param_index, pattern));
                        }
                    }
                }

                FunctionHeader {
                    generics,
                    parameters,
                    return_type: ret.unwrap_or_else(|| ParserDataType::null(span)),
                    param_destructures,
                }
            })
            .boxed()
    }
}

impl<'a> AstParser<'a> for AstFunction {
    type Data = RecurseAstNode<'a>;

    fn parser(data: Self::Data) -> Boxed<'a, 'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        select! { Token::Fn => () }
            .then_ignore(select! { Token::Match => () }.not())
            .ignore_then(FunctionHeader::parser(data.clone()))
            .then(AstScopeDef::parser(data))
            .map_with_span(|(header, body), span| AstFunction {
                header,
                body: Box::new(AstNode::new(span, AstNodeType::from(body))),
            })
            .boxed()
    }
}

impl<'a> AstParser<'a> for AstExtern {
    type Data = ();

    fn parser(_data: Self::Data) -> Boxed<'a, 'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        select! { Token::Extern => () }
            .ignore_then(select! { Token::StringLiteral(abi) => abi })
            .then_ignore(select! { Token::Const => () })
            .then(PotentialDollarIdentifier::parser(()))
            .then_ignore(select! { Token::Walrus => () }.padded_by(potential_new_line()))
            .then_ignore(select! { Token::Fn => () })
            .then_ignore(select! { Token::LeftParen => () })
            .then(
                ParserDataType::parser(())
                    .padded_by(potential_new_line())
                    .separated_by(select! { Token::Comma => () })
                    .allow_trailing()
                    .collect::<Vec<_>>()
                    .or_not()
                    .map(|x| x.unwrap_or_default()),
            )
            .then_ignore(select! { Token::RightParen => () })
            .then(
                select! { Token::RightArrow => () }
                    .padded_by(potential_new_line())
                    .ignore_then(ParserDataType::parser(()))
                    .or_not(),
            )
            .then_ignore(select! { Token::From => () }.padded_by(potential_new_line()))
            .then(select! { Token::StringLiteral(library) => library })
            .then(
                select! { Token::As => () }
                    .padded_by(potential_new_line())
                    .ignore_then(select! { Token::StringLiteral(symbol) => symbol })
                    .or_not(),
            )
            .map(
                |(((((abi, identifier), parameters), return_type), library), symbol)| AstExtern {
                    abi: abi.to_string(),
                    identifier,
                    parameters,
                    return_type: return_type
                        .unwrap_or_else(|| ParserDataType::null(Span::default())),
                    library: library.to_string(),
                    symbol: symbol.map(|s| s.to_string()),
                },
            )
            .boxed()
    }
}

impl<'a> AstParser<'a> for AstCurry {
    type Data = RecurseAstNode<'a>;

    fn parser(data: Self::Data) -> Boxed<'a, 'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        select! { Token::Curry => () }
            .ignore_then(data.node)
            .map(|value| AstCurry {
                value: Box::new(value),
            })
            .boxed()
    }
}

impl<'a> AstParser<'a> for AstCall {
    type Data = RecurseAstNode<'a>;

    fn parser(data: Self::Data) -> Boxed<'a, 'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        let call_args = select! { Token::LeftParen => () }
            .ignore_then(
                CallArg::parser(data.clone())
                    .padded_by(potential_new_line())
                    .separated_by(select! { Token::Comma => () })
                    .allow_trailing()
                    .collect::<Vec<_>>()
                    .or_not()
                    .map(|x| x.unwrap_or_default()),
            )
            .then_ignore(select! { Token::RightParen => () })
            .boxed();

        let reverse_args = select! { Token::Lesser => () }
            .ignore_then(select! { Token::LeftParen => () })
            .ignore_then(
                data.node
                    .clone()
                    .padded_by(potential_new_line())
                    .separated_by(select! { Token::Comma => () })
                    .allow_trailing()
                    .collect::<Vec<_>>()
                    .or_not()
                    .map(|x| x.unwrap_or_default()),
            )
            .then_ignore(select! { Token::RightParen => () })
            .boxed();

        data.node
            .then(
                select! { Token::Vampire => () }
                    .ignore_then(
                        ParserDataType::parser(())
                            .padded_by(potential_new_line())
                            .separated_by(select! { Token::Comma => () })
                            .collect::<Vec<_>>(),
                    )
                    .then_ignore(select! { Token::Greater => () })
                    .or_not(),
            )
            .then(call_args)
            .then(reverse_args)
            .map(|(((caller, generic_types), args), reverse_args)| AstCall {
                string_fn: None,
                caller: Box::new(caller),
                generic_types: generic_types.unwrap_or_default(),
                args,
                reverse_args,
            })
            .boxed()
    }
}
