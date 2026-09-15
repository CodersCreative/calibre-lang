use crate::{
    Span,
    ast::{
        RefMutability,
        ffi::{ParserFfiDataType, ParserFfiInnerType},
        types::{GenericType, GenericTypes, ParserDataType, ParserInnerType},
    },
    lexer::Token,
    parse::{
        AstParser, AstParserErr, MapWithSpanExt, RecursiveData, TokenStream, potential_new_line,
    },
};
use chumsky::prelude::*;
use chumsky::{Parser, select};
use std::str::FromStr;

impl<'a> AstParser<'a> for ParserFfiInnerType {
    type Data = ();

    fn parser(_data: &Self::Data) -> Boxed<'a, 'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        select! { Token::Identifier(x) => x }
            .try_map(|name, span| {
                ParserFfiInnerType::from_str(name)
                    .map_err(|()| chumsky::error::Rich::custom(span, "invalid FFI type"))
            })
            .boxed()
    }
}

impl<'a> AstParser<'a> for ParserFfiDataType {
    type Data = ();

    fn parser(_data: &Self::Data) -> Boxed<'a, 'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        select! { Token::At => () }
            .ignore_then(ParserFfiInnerType::parser(&()))
            .map_with_span(|data_type, span| ParserFfiDataType::new(span, data_type))
            .boxed()
    }
}

impl<'a> AstParser<'a> for ParserDataType {
    type Data = ();

    fn parser(_data: &Self::Data) -> Boxed<'a, 'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        recursive(
            |ty: chumsky::recursive::Recursive<
                dyn chumsky::Parser<'_, TokenStream<'a>, ParserDataType, AstParserErr<'a>>,
            >| {
                let tuple_parser = select! { Token::Lesser => () }
                    .ignore_then(
                        ty.clone()
                            .separated_by(select! { Token::Comma => () })
                            .allow_trailing()
                            .collect::<Vec<_>>()
                            .or_not()
                            .map(|x| x.unwrap_or_default()),
                    )
                    .then_ignore(select! { Token::Greater => () })
                    .map_with_span(|types, span| {
                        if types.len() == 1 {
                            types.into_iter().next().unwrap()
                        } else {
                            ParserDataType::new(span, ParserInnerType::Tuple(types))
                        }
                    })
                    .boxed();

                let ffi_parser = ParserFfiInnerType::parser(&())
                    .map_with_span(|ffi, span| {
                        ParserDataType::new(span, ParserInnerType::FfiType(ffi))
                    })
                    .boxed();

                let function_parser = select! { Token::Fn => () }.ignore_then(
                    select! { Token::LeftParen => () }
                    .ignore_then(
                        ty.clone().padded_by(potential_new_line())
                            .separated_by(select! { Token::Comma => () })
                            .allow_trailing()
                            .collect::<Vec<_>>()
                            .or_not()
                            .map(|x| x.unwrap_or_default()),
                    )
                    .then_ignore(select! { Token::RightParen => () })).or_not()
                    .then(
                        select! { Token::RightArrow => () }
                            .ignore_then(ty.clone())
                            .or_not(),
                    )
                    .map_with_span(|(parameters, ret), span| {
                        ParserDataType::new(
                            span,
                            ParserInnerType::Function {
                                return_type: Box::new(ret.unwrap_or_else(|| {
                                    ParserDataType::new(span, ParserInnerType::Null)
                                })),
                                parameters : parameters.unwrap_or_default(),
                            },
                        )
                    })
                    .boxed();

                let struct_parser = select! { Token::Identifier(x) => x }
                    .then(
                        select! { Token::Vampire => () }
                            .ignore_then(
                                ty.clone()
                                    .separated_by(select! { Token::Comma => () })
                                    .allow_trailing()
                                    .collect::<Vec<_>>()
                                    .or_not()
                                    .map(|x| x.unwrap_or_default()),
                            )
                            .then_ignore(select! { Token::Greater => () })
                            .or_not(),
                    )
                    .try_map_with_span(|(name, generic_types), span| {
                        Ok(if let Some(generic_types) = generic_types {
                            match name {
                                "dyn" => {
                                    let traits = generic_types
                                        .into_iter()
                                        .filter_map(|ty| {
                                            let text = ty.to_string().trim().to_string();
                                            (!text.is_empty()).then_some(text)
                                        })
                                        .collect::<Vec<_>>();
                                    ParserDataType::new(
                                        span,
                                        ParserInnerType::DynamicTraits(traits),
                                    )
                                }
                                "list" => {
                                    if generic_types.len() == 1 {
                                        ParserDataType::new(
                                            span,
                                            ParserInnerType::List(Box::new(
                                                generic_types.into_iter().next().unwrap(),
                                            )),
                                        )
                                    } else {
                                        return Err(Rich::custom(
                                            span.into(),
                                            "expected exactly one type parameter with a 'list' type",
                                        ))
                                    }
                                }
                                "ptr" => {
                                    if generic_types.len() == 1 {
                                        ParserDataType::new(
                                            span,
                                            ParserInnerType::Ptr(Box::new(
                                                generic_types.into_iter().next().unwrap(),
                                            )),
                                        )
                                    } else {
                                        return Err(Rich::custom(
                                            span.into(),
                                            "expected exactly one type parameter with a 'ptr' type",
                                        ))
                                    }
                                }
                                "gen" => {
                                    if generic_types.len() == 1 {
                                        ParserDataType::new(
                                            span,
                                            ParserInnerType::Gen(Box::new(
                                                generic_types.into_iter().next().unwrap(),
                                            )),
                                        )
                                    } else {
                                        return Err(Rich::custom(
                                            span.into(),
                                            "expected exactly one type parameter with a 'gen' type",
                                        ))
                                    }
                                }
                                "option" => {
                                    if generic_types.len() == 1 {
                                        ParserDataType::new(
                                            span,
                                            ParserInnerType::Option(Box::new(
                                                generic_types.into_iter().next().unwrap(),
                                            )),
                                        )
                                    } else {
                                        return Err(Rich::custom(
                                            span.into(),
                                            "expected exactly one type parameter with a 'option' type",
                                        ))
                                    }
                                }
                                "result" => {
                                    if generic_types.len() == 2 {
                                        let mut types = generic_types.into_iter();
                                        ParserDataType::new(
                                            span,
                                            ParserInnerType::Result {
                                                ok: Box::new(types.next().unwrap()),
                                                err: Box::new(types.next().unwrap()),
                                            },
                                        )
                                    } else {
                                        return Err(Rich::custom(
                                            span.into(),
                                            "expected exactly two type parameter with a 'result' type",
                                        ))
                                    }
                                }
                                _ => ParserDataType::new(
                                    span,
                                    ParserInnerType::StructWithGenerics {
                                        identifier: name.to_string(),
                                        generic_types,
                                    },
                                ),
                            }
                        } else {
                            ParserDataType::new(
                                span,
                                ParserInnerType::from_str(name)
                                    .unwrap_or(ParserInnerType::Struct(name.to_string())),
                            )
                        })
                    })
                    .boxed();

                let dollar_parser = select! { Token::Dollar => () }
                    .ignore_then(select! { Token::Identifier(x) => x })
                    .map_with_span(|name, span| {
                        ParserDataType::new(
                            span,
                            ParserInnerType::DollarIdentifier(name.to_string()),
                        )
                    })
                    .boxed();

                let null_parser = select! {
                    Token::Null => ()
                }.map_with_span(|_, sp| {
                    ParserDataType::new(sp, ParserInnerType::Null)
                }).boxed();

                let base = choice((
                    null_parser,
                    tuple_parser,
                    ffi_parser,
                    function_parser,
                    struct_parser,
                    dollar_parser,
                ));

                let mut_val_parser = select! { Token::Mut => () }
                    .ignore_then(ty.clone())
                    .map_with_span(|inner, span| {
                        ParserDataType::new(
                            span,
                            ParserInnerType::Ref(Box::new(inner), RefMutability::MutValue),
                        )
                    })
                    .boxed();

                let mut_ref_parser = select! { Token::MutRef => () }
                    .ignore_then(ty.clone())
                    .map_with_span(|inner, span| {
                        ParserDataType::new(
                            span,
                            ParserInnerType::Ref(Box::new(inner), RefMutability::MutRef),
                        )
                    })
                    .boxed();

                let ref_parser = select! { Token::BitAnd => () }
                    .ignore_then(ty.clone())
                    .map_with_span(|inner, span| {
                        ParserDataType::new(
                            span,
                            ParserInnerType::Ref(Box::new(inner), RefMutability::Ref),
                        )
                    })
                    .boxed();

                let result_type = choice((mut_val_parser, mut_ref_parser, ref_parser, base))
                    .then(
                        select! { Token::Not => () }
                            .ignore_then(ty.clone())
                            .or_not(),
                    )
                    .map(|(left, right)| {
                        if let Some(right) = right {
                            ParserDataType::new(
                                Span::new_from_spans(left.span, right.span),
                                ParserInnerType::Result {
                                    ok: Box::new(right),
                                    err: Box::new(left),
                                },
                            )
                        } else {
                            left
                        }
                    });

                result_type
                    .then(select! { Token::Question => () }.or_not())
                    .map(|(inner, option)| {
                        if option.is_some() {
                            ParserDataType::new(
                                inner.span,
                                ParserInnerType::Option(Box::new(inner)),
                            )
                        } else {
                            inner
                        }
                    })
            },
        )
        .boxed()
    }
}

impl<'a> AstParser<'a> for GenericTypes {
    type Data = RecursiveData<'a>;

    fn parser(data: &Self::Data) -> Boxed<'a, 'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        select! { Token::Lesser => () }
            .ignore_then(
                GenericType::parser(data)
                    .separated_by(select! { Token::Comma => () })
                    .allow_trailing()
                    .collect::<Vec<_>>()
                    .or_not()
                    .map(|x| x.unwrap_or_default()),
            )
            .then_ignore(select! { Token::Greater => () })
            .or_not()
            .map(|items| GenericTypes(items.unwrap_or_default()))
            .boxed()
    }
}

impl<'a> AstParser<'a> for GenericType {
    type Data = RecursiveData<'a>;

    fn parser(data: &Self::Data) -> Boxed<'a, 'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        data.dollar_ident
            .clone()
            .then(
                select! { Token::Colon => () }
                    .ignore_then(
                        data.dollar_ident.clone()
                            .separated_by(select! { Token::Add => () })
                            .collect::<Vec<_>>(),
                    )
                    .or_not(),
            )
            .map(|(identifier, trait_constraints)| GenericType {
                identifier,
                trait_constraints: trait_constraints.unwrap_or_default(),
            })
            .boxed()
    }
}
