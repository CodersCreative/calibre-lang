use crate::ast::ObjectType;
use crate::ast::idents::ParserText;
use crate::ast::idents::PotentialDollarIdentifier;
use crate::ast::idents::PotentialGenericTypeIdentifier;
use crate::ast::nodes::types::{
    AstImpl, AstImplTrait, AstTrait, AstType, Overload, TraitMember, TraitMemberKind, TypeDefType,
};
use crate::ast::types::GenericTypes;
use crate::ast::types::ParserDataType;
use crate::ast::types::ParserInnerType;
use crate::{
    ast::nodes::AstNode,
    ast::nodes::AstNodeType,
    lexer::Token,
    parse::{AstParser, AstParserErr, MapWithSpanExt, TokenStream},
};
use chumsky::prelude::*;
use chumsky::{Boxed, Parser, select};
use ustr::Ustr;

impl<'a> AstParser<'a> for TypeDefType {
    fn parser() -> Boxed<'a, 'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        let struct_named_fields = select! { Token::LeftBracket => () }
            .ignore_then(
                PotentialDollarIdentifier::parser()
                    .repeated()
                    .at_least(1)
                    .collect::<Vec<_>>()
                    .then_ignore(select! { Token::Colon => () })
                    .then(ParserDataType::parser())
                    .then(
                        select! { Token::Eq => () }
                            .ignore_then(AstNode::parser())
                            .or_not(),
                    )
                    .separated_by(select! { Token::Comma => () })
                    .allow_trailing()
                    .collect::<Vec<_>>(),
            )
            .then_ignore(select! { Token::RightBracket => () })
            .map(|groups| {
                let mut fields = Vec::new();
                for ((names, ty), default_value) in groups {
                    for name in names {
                        fields.push((
                            Ustr::from(&name.text().clone()),
                            (ty.clone(), default_value.clone()),
                        ));
                    }
                }
                TypeDefType::Struct {
                    fields: ObjectType::Map(fields),
                }
            });

        let struct_tuple_fields = select! { Token::LeftParen => () }
            .ignore_then(
                ParserDataType::parser()
                    .separated_by(select! { Token::Comma => () })
                    .allow_trailing()
                    .collect::<Vec<_>>()
                    .or_not()
                    .map(|x| x.unwrap_or_default()),
            )
            .then_ignore(select! { Token::RightParen => () })
            .map(|types| TypeDefType::Struct {
                fields: ObjectType::Tuple(types.into_iter().map(|t| (t, None)).collect()),
            });

        let enum_parser = select! { Token::Enum => () }
            .ignore_then(select! { Token::LeftBracket => () })
            .ignore_then(
                PotentialDollarIdentifier::parser()
                    .repeated()
                    .collect::<Vec<_>>()
                    .or_not()
                    .map(|x| x.unwrap_or_default())
                    .then(
                        select! { Token::Colon => () }
                            .ignore_then(ParserDataType::parser())
                            .or_not(),
                    )
                    .then(
                        select! { Token::Eq => () }
                            .ignore_then(AstNode::parser())
                            .or_not(),
                    )
                    .map(|((names, t), default_value)| {
                        names
                            .into_iter()
                            .map(|name| (name, t.clone(), default_value.clone()))
                            .collect::<Vec<_>>()
                    })
                    .separated_by(select! { Token::Comma => () })
                    .allow_trailing()
                    .collect::<Vec<_>>(),
            )
            .then_ignore(select! { Token::RightBracket => () })
            .map(|groups| {
                let mut variants = Vec::new();
                let mut default_variant = None;
                let mut default_value = None;

                for (idx, group) in groups.iter().enumerate() {
                    for (name, data_type, default_val) in group {
                        variants.push((name.clone(), data_type.clone()));
                        if default_val.is_some() {
                            default_variant = Some(idx);
                            default_value = default_val.clone().map(Box::new);
                        }
                    }
                }

                TypeDefType::Enum {
                    variants,
                    default_variant,
                    default_value,
                }
            });

        let newtype_parser = ParserDataType::parser().try_map(|typ, sp| {
            if typ.data_type == ParserInnerType::Dynamic
                || typ.data_type == ParserInnerType::Auto(None)
            {
                Err(Rich::custom(
                    sp,
                    "cannot overload `auto` or `dyn`; specify a concrete type",
                ))
            } else {
                Ok(TypeDefType::NewType(Box::new(typ)))
            }
        });

        choice((
            select! { Token::Struct => () }
                .ignore_then(choice((struct_named_fields, struct_tuple_fields))),
            enum_parser,
            newtype_parser,
        ))
        .boxed()
    }
}

impl<'a> AstParser<'a> for Overload {
    fn parser() -> Boxed<'a, 'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        select! { Token::Const => () }
            .ignore_then(select! { Token::StringLiteral(op) => op })
            .map_with_span(|op, sp| ParserText::new(sp, op))
            .then_ignore(select! { Token::Walrus => () })
            .then(AstNode::parser())
            .try_map(|(operator, value), sp| match value.node_type {
                AstNodeType::FunctionDeclaration(ref func) => Ok(Overload {
                    operator,
                    header: func.header.clone(),
                    body: Box::new(value),
                }),
                _ => Err(Rich::custom(sp, "expected function declaration")),
            })
            .boxed()
    }
}

impl<'a> AstParser<'a> for TraitMember {
    fn parser() -> Boxed<'a, 'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        let const_member = select! { Token::Const => () }
            .ignore_then(PotentialDollarIdentifier::parser())
            .then(
                select! { Token::Colon => () }
                    .ignore_then(ParserDataType::parser())
                    .or_not(),
            )
            .then(
                select! { Token::Walrus => () }
                    .ignore_then(AstNode::parser())
                    .or_not(),
            )
            .map_with_span(|((identifier, data_type), value), span| TraitMember {
                kind: TraitMemberKind::Const,
                identifier,
                data_type: data_type.unwrap_or_else(|| ParserDataType::auto(span)),
                value: value.map(Box::new),
            });

        let type_member = select! { Token::Type => () }
            .ignore_then(PotentialDollarIdentifier::parser())
            .map_with_span(|identifier, span| TraitMember {
                kind: TraitMemberKind::Type,
                identifier,
                data_type: ParserDataType::auto(span),
                value: None,
            });

        choice((const_member, type_member)).boxed()
    }
}

impl<'a> AstParser<'a> for AstImpl {
    fn parser() -> Boxed<'a, 'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        select! { Token::Impl => () }
            .ignore_then(GenericTypes::parser())
            .then(ParserDataType::parser())
            .then_ignore(select! { Token::LeftBracket => () })
            .then(
                AstNode::parser()
                    .separated_by(select! { Token::Comma => () })
                    .allow_trailing()
                    .collect::<Vec<_>>()
                    .or_not()
                    .map(|x| x.unwrap_or_default()),
            )
            .then_ignore(select! { Token::RightBracket => () })
            .map(|((generics, target), variables)| AstImpl {
                generics,
                target,
                variables,
            })
            .boxed()
    }
}

impl<'a> AstParser<'a> for AstImplTrait {
    fn parser() -> Boxed<'a, 'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        select! { Token::Impl => () }
            .ignore_then(GenericTypes::parser())
            .then(PotentialGenericTypeIdentifier::parser())
            .then_ignore(select! { Token::For => () })
            .then(ParserDataType::parser())
            .then_ignore(select! { Token::LeftBracket => () })
            .then(
                AstNode::parser()
                    .separated_by(select! { Token::Comma => () })
                    .allow_trailing()
                    .collect::<Vec<_>>()
                    .or_not()
                    .map(|x| x.unwrap_or_default()),
            )
            .then_ignore(select! { Token::RightBracket => () })
            .map(
                |(((generics, trait_ident), target), variables)| AstImplTrait {
                    generics,
                    trait_ident,
                    target,
                    variables,
                },
            )
            .boxed()
    }
}

impl<'a> AstParser<'a> for AstTrait {
    fn parser() -> Boxed<'a, 'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        select! { Token::Trait => () }
            .ignore_then(PotentialGenericTypeIdentifier::parser())
            .then_ignore(select! { Token::LeftBracket => () })
            .then(
                TraitMember::parser()
                    .separated_by(select! { Token::Comma => () })
                    .allow_trailing()
                    .collect::<Vec<_>>()
                    .or_not()
                    .map(|x| x.unwrap_or_default()),
            )
            .then_ignore(select! { Token::RightBracket => () })
            .map(|(identifier, members)| AstTrait {
                identifier,
                implied_traits: Vec::new(),
                members,
            })
            .boxed()
    }
}

impl<'a> AstParser<'a> for AstType {
    fn parser() -> Boxed<'a, 'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        select! { Token::Type => () }
            .ignore_then(PotentialGenericTypeIdentifier::parser())
            .then_ignore(select! { Token::Walrus => () })
            .then(TypeDefType::parser())
            .then(
                select! { Token::At => () }
                    .ignore_then(select! { Token::Identifier(ident) => ident })
                    .try_map(|ident, sp| {
                        if ident == "overload" {
                            Ok(ident)
                        } else {
                            Err(Rich::custom(sp, "expected 'overload'"))
                        }
                    })
                    .ignore_then(select! { Token::LeftBracket => () })
                    .ignore_then(
                        Overload::parser()
                            .separated_by(select! { Token::Comma => () })
                            .allow_trailing()
                            .collect::<Vec<_>>()
                            .or_not()
                            .map(|x| x.unwrap_or_default()),
                    )
                    .then_ignore(select! { Token::RightBracket => () })
                    .or_not()
                    .map(|x| x.unwrap_or_default()),
            )
            .map(|((identifier, object), overloads)| AstType {
                identifier,
                object,
                overloads,
            })
            .boxed()
    }
}
