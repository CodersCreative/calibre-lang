use crate::ast::ObjectType;
use crate::ast::nodes::misc::StandaloneTag;
use crate::ast::nodes::types::{AstImpl, AstType, TypeDefType};
use crate::ast::types::GenericTypes;
use crate::ast::types::ParserInnerType;
use crate::parse::StatementData;
use crate::parse::potential_new_line;
use crate::{
    lexer::Token,
    parse::{AstParser, AstParserErr, TokenStream},
};
use chumsky::prelude::*;
use chumsky::{Parser, select};
use ustr::Ustr;

impl<'a> AstParser<'a> for TypeDefType {
    type Data = StatementData<'a>;

    fn parser(data: Self::Data) -> impl Parser<'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        let struct_named_fields = StandaloneTag::parser(data.clone())
            .padded_by(potential_new_line())
            .repeated()
            .collect::<Vec<_>>()
            .then(
                data.dollar_ident
                    .clone()
                    .repeated()
                    .at_least(1)
                    .collect::<Vec<_>>(),
            )
            .then_ignore(just(Token::Colon).padded_by(potential_new_line()))
            .then(data.data_type.clone())
            .then(just(Token::Eq).ignore_then(data.node.clone()).or_not())
            .separated_by(just(Token::Comma).padded_by(potential_new_line()))
            .allow_trailing()
            .collect::<Vec<_>>()
            .padded_by(potential_new_line())
            .delimited_by(just(Token::LeftBracket), just(Token::RightBracket))
            .map(|groups| {
                let mut fields = Vec::new();
                for (((tags, names), ty), default_value) in groups {
                    for name in names {
                        fields.push((
                            Ustr::from(&name.text().clone()),
                            (tags.clone(), ty.clone(), default_value.clone()),
                        ));
                    }
                }
                TypeDefType::Struct {
                    fields: ObjectType::Map(fields),
                }
            });

        let struct_tuple_fields = StandaloneTag::parser(data.clone())
            .padded_by(potential_new_line())
            .repeated()
            .collect::<Vec<_>>()
            .then(data.data_type.clone())
            .separated_by(just(Token::Comma).padded_by(potential_new_line()))
            .allow_trailing()
            .collect::<Vec<_>>()
            .padded_by(potential_new_line())
            .delimited_by(just(Token::LeftParen), just(Token::RightParen))
            .map(|types| TypeDefType::Struct {
                fields: ObjectType::Tuple(
                    types.into_iter().map(|(tags, t)| (tags, t, None)).collect(),
                ),
            });

        let enum_parser = just(Token::Enum)
            .ignore_then(
                StandaloneTag::parser(data.clone())
                    .padded_by(potential_new_line())
                    .repeated()
                    .collect::<Vec<_>>()
                    .then(data.dollar_ident.clone().repeated().collect::<Vec<_>>())
                    .then(
                        just(Token::Colon)
                            .padded_by(potential_new_line())
                            .ignore_then(data.data_type.clone())
                            .or_not(),
                    )
                    .then(
                        just(Token::Eq)
                            .padded_by(potential_new_line())
                            .ignore_then(data.node.clone())
                            .or_not(),
                    )
                    .map(|(((tags, names), t), default_value)| {
                        names
                            .into_iter()
                            .map(|name| (name, t.clone(), default_value.clone(), tags.clone()))
                            .collect::<Vec<_>>()
                    })
                    .separated_by(just(Token::Comma).padded_by(potential_new_line()))
                    .allow_trailing()
                    .collect::<Vec<_>>()
                    .padded_by(potential_new_line())
                    .delimited_by(just(Token::LeftBracket), just(Token::RightBracket)),
            )
            .map(|groups| {
                let mut variants = Vec::new();
                let mut default_variant = None;
                let mut default_value = None;

                for (idx, group) in groups.iter().enumerate() {
                    for (name, data_type, default_val, tags) in group {
                        for tag in tags {
                            if *tag.tag == "default" {
                                default_variant = Some(idx);
                                default_value = default_val.clone().map(Box::new);
                            }
                        }

                        variants.push((tags.clone(), name.clone(), data_type.clone()));
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

impl<'a> AstParser<'a> for AstImpl {
    type Data = StatementData<'a>;

    #[inline(always)]
    fn parser(data: Self::Data) -> impl Parser<'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        just(Token::Impl)
            .ignore_then(GenericTypes::parser(data.clone()))
            .then(data.data_type.clone())
            .then(
                data.node
                    .clone()
                    .padded_by(potential_new_line())
                    .repeated()
                    .collect::<Vec<_>>()
                    .padded_by(potential_new_line())
                    .delimited_by(just(Token::LeftBracket), just(Token::RightBracket)),
            )
            .map(|((generics, target), variables)| AstImpl {
                generics,
                target,
                variables,
            })
    }
}

impl<'a> AstParser<'a> for AstType {
    type Data = StatementData<'a>;

    fn parser(data: Self::Data) -> impl Parser<'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        just(Token::Type)
            .ignore_then(data.generic_ident.clone())
            .then_ignore(just(Token::Walrus).padded_by(potential_new_line()))
            .then(TypeDefType::parser(data.clone()))
            .map(|(identifier, object)| AstType { identifier, object })
    }
}
