use crate::ast::ObjectType;
use crate::ast::idents::ParserText;
use crate::ast::nodes::misc::AstTag;
use crate::ast::nodes::types::{
    AstImpl, AstImplTrait, AstTrait, AstType, Overload, TraitMember, TraitMemberKind, TypeDefType,
};
use crate::ast::types::GenericTypes;
use crate::ast::types::ParserDataType;
use crate::ast::types::ParserInnerType;
use crate::parse::StatementData;
use crate::{
    ast::nodes::AstNodeType,
    lexer::Token,
    parse::{AstParser, AstParserErr, MapWithSpanExt, TokenStream, typed_or_untyped_assignment},
};
use chumsky::prelude::*;
use chumsky::{Parser, select};
use ustr::Ustr;

impl<'a> AstParser<'a> for TypeDefType {
    type Data = StatementData<'a>;

    fn parser(data: Self::Data) -> impl Parser<'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        let struct_named_fields = select! { Token::LeftBracket => () }
            .ignore_then(
                data.dollar_ident
                    .clone()
                    .repeated()
                    .at_least(1)
                    .collect::<Vec<_>>()
                    .then_ignore(select! { Token::Colon => () })
                    .then(data.data_type.clone())
                    .then(
                        select! { Token::Eq => () }
                            .ignore_then(data.node.clone())
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
                data.data_type
                    .clone()
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
                AstTag::parser(data.clone())
                    .repeated()
                    .collect::<Vec<_>>()
                    .or_not()
                    .map(|x| x.unwrap_or_default())
                    .then(
                        data.dollar_ident
                            .clone()
                            .repeated()
                            .collect::<Vec<_>>()
                            .or_not()
                            .map(|x| x.unwrap_or_default()),
                    )
                    .then(
                        select! { Token::Colon => () }
                            .ignore_then(data.data_type.clone())
                            .or_not(),
                    )
                    .then(
                        select! { Token::Eq => () }
                            .ignore_then(data.node.clone())
                            .or_not(),
                    )
                    .map(|(((tags, names), t), default_value)| {
                        names
                            .into_iter()
                            .map(|name| (name, t.clone(), default_value.clone(), tags.clone()))
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
                    for (name, data_type, default_val, tags) in group {
                        variants.push((name.clone(), data_type.clone()));

                        for tag in tags {
                            if *tag.tag == "default" {
                                default_variant = Some(idx);
                                default_value = default_val.clone().map(Box::new);
                            }
                        }
                    }
                }

                TypeDefType::Enum {
                    variants,
                    default_variant,
                    default_value,
                }
            });

        let newtype_parser = data.data_type.clone().try_map(|typ, sp| {
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
    }
}

impl<'a> AstParser<'a> for Overload {
    type Data = StatementData<'a>;

    #[inline(always)]
    fn parser(data: Self::Data) -> impl Parser<'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        select! { Token::Const => () }
            .ignore_then(select! { Token::StringLiteral(op) => op })
            .map_with_span(|op, sp| ParserText::new(sp, op))
            .then_ignore(select! { Token::Walrus => () })
            .then(data.node.clone())
            .try_map(|(operator, value), sp| match value.node_type {
                AstNodeType::FunctionDeclaration(ref func) => Ok(Overload {
                    operator,
                    header: func.header.clone(),
                    body: Box::new(value),
                }),
                _ => Err(Rich::custom(sp, "expected function declaration")),
            })
    }
}

impl<'a> AstParser<'a> for TraitMember {
    type Data = StatementData<'a>;

    #[inline(always)]
    fn parser(data: Self::Data) -> impl Parser<'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        let const_member = select! { Token::Const => () }
            .ignore_then(data.dollar_ident.clone())
            .then(typed_or_untyped_assignment(data.clone()))
            .map_with_span(|(identifier, (data_type, value)), span| TraitMember {
                kind: TraitMemberKind::Const,
                identifier,
                data_type: data_type.unwrap_or_else(|| ParserDataType::auto(span)),
                value: value.map(Box::new),
            });

        let type_member = select! { Token::Type => () }
            .ignore_then(data.dollar_ident.clone())
            .map_with_span(|identifier, span| TraitMember {
                kind: TraitMemberKind::Type,
                identifier,
                data_type: ParserDataType::auto(span),
                value: None,
            });

        choice((const_member, type_member))
    }
}

impl<'a> AstParser<'a> for AstImpl {
    type Data = StatementData<'a>;

    #[inline(always)]
    fn parser(data: Self::Data) -> impl Parser<'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        select! { Token::Impl => () }
            .ignore_then(GenericTypes::parser(data.clone()))
            .then(data.data_type.clone())
            .then_ignore(select! { Token::LeftBracket => () })
            .then(
                data.node
                    .clone()
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
    }
}

impl<'a> AstParser<'a> for AstImplTrait {
    type Data = StatementData<'a>;

    #[inline(always)]
    fn parser(data: Self::Data) -> impl Parser<'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        select! { Token::Impl => () }
            .ignore_then(GenericTypes::parser(data.clone()))
            .then(data.generic_ident.clone())
            .then_ignore(select! { Token::For => () })
            .then(data.data_type.clone())
            .then_ignore(select! { Token::LeftBracket => () })
            .then(
                data.node
                    .clone()
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
    }
}

impl<'a> AstParser<'a> for AstTrait {
    type Data = StatementData<'a>;

    #[inline(always)]
    fn parser(data: Self::Data) -> impl Parser<'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        select! { Token::Trait => () }
            .ignore_then(data.generic_ident.clone())
            .then_ignore(select! { Token::LeftBracket => () })
            .then(
                TraitMember::parser(data.clone())
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
    }
}

impl<'a> AstParser<'a> for AstType {
    type Data = StatementData<'a>;

    fn parser(data: Self::Data) -> impl Parser<'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        select! { Token::Type => () }
            .ignore_then(data.generic_ident.clone())
            .then_ignore(select! { Token::Walrus => () })
            .then(TypeDefType::parser(data.clone()))
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
                        Overload::parser(data)
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
    }
}
