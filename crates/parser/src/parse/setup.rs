use super::{LegacySpanMapExt, filter};
use crate::Span;
use crate::ast::RefMutability;
use crate::ast::ffi::ParserFfiInnerType;
use crate::ast::idents::{ParserText, PotentialDollarIdentifier};
use crate::ast::nodes::{AstNode, AstNodeType};
use crate::ast::types::{GenericType, GenericTypes, ParserDataType, ParserInnerType};
use crate::parse::util::{is_keyword, lex, span, unescape_char_literal, unescape_string};
use chumsky::error::Rich;
use chumsky::prelude::*;
use std::str::FromStr;
use std::sync::Arc;

pub type ParseExtra<'a> = extra::Err<Rich<'a, char>>;
pub type StrParser<'a, O> = Boxed<'a, 'a, &'a str, O, ParseExtra<'a>>;

pub struct ParserPrelude<'a> {
    pub pad: StrParser<'a, ()>,
    pub pad_with_newline: StrParser<'a, ()>,
    pub delim: StrParser<'a, ()>,
    pub comma: StrParser<'a, ()>,
    pub arrow: StrParser<'a, ()>,
    pub fat_arrow: StrParser<'a, ()>,
    pub left_arrow: StrParser<'a, ()>,
    pub raw_ident: StrParser<'a, (String, Span)>,
    pub ident: StrParser<'a, (String, Span)>,
    pub dollar_ident: StrParser<'a, PotentialDollarIdentifier>,
    pub named_ident: StrParser<'a, PotentialDollarIdentifier>,
    pub generic_params: StrParser<'a, GenericTypes>,
    pub string_text: StrParser<'a, String>,
    pub string_lit: StrParser<'a, AstNode>,
    pub char_lit: StrParser<'a, AstNode>,
    pub int_lit: StrParser<'a, AstNode>,
    pub float_lit: StrParser<'a, AstNode>,
    pub null_lit: StrParser<'a, AstNode>,
    pub type_name: StrParser<'a, ParserDataType>,
}

pub fn build_parser_prelude<'a>(line_starts: Arc<Vec<usize>>) -> ParserPrelude<'a> {
    let ws = filter(|c: &char| *c == ' ' || *c == '\t' || *c == '\r')
        .repeated()
        .at_least(1)
        .ignored();

    let comment = choice((
        just("/*")
            .then(super::take_until(just("*/")))
            .then_ignore(just("*/")),
        just("//").then(super::take_until(just('\n'))),
    ))
    .ignored();

    let pad = choice((ws.clone(), comment.clone()))
        .repeated()
        .ignored()
        .boxed();

    let pad_with_newline = choice((ws.clone(), comment, just('\n').ignored()))
        .repeated()
        .ignored()
        .boxed();

    let delim = choice((just('\n'), just(';')))
        .padded_by(pad.clone())
        .repeated()
        .at_least(1)
        .ignored()
        .boxed();

    let comma = lex(pad.clone(), just(','))
        .then_ignore(delim.clone().repeated().collect::<Vec<_>>())
        .ignored()
        .boxed();

    let arrow = lex(pad.clone(), just('-').then_ignore(just('>')))
        .ignored()
        .boxed();

    let fat_arrow = lex(pad.clone(), just('=').then_ignore(just('>')))
        .ignored()
        .boxed();

    let left_arrow = lex(pad.clone(), just('<').then_ignore(just('-')))
        .ignored()
        .boxed();

    let raw_ident = lex(pad.clone(), text::ident().map(|s: &str| s.to_string()))
        .map_with_span({
            let ls = line_starts.clone();
            move |s: String, r| (s, span(ls.as_ref(), r))
        })
        .boxed();

    let ident = lex(pad.clone(), text::ident().map(|s: &str| s.to_string()))
        .try_map(|s: String, parser_span| {
            if is_keyword(&s) {
                Err(Rich::custom(parser_span, "identifier cannot be a keyword"))
            } else {
                Ok(s)
            }
        })
        .map_with_span({
            let ls = line_starts.clone();
            move |s: String, r| (s, span(ls.as_ref(), r))
        })
        .boxed();

    let dollar_ident = lex(pad.clone(), just('$'))
        .ignore_then(raw_ident.clone())
        .map(|(name, sp)| PotentialDollarIdentifier::DollarIdentifier(ParserText::new(sp, name)))
        .boxed();

    let named_ident = choice((
        ident
            .clone()
            .map(|(name, sp)| PotentialDollarIdentifier::new(sp, name)),
        dollar_ident.clone(),
    ))
    .boxed();

    let generic_params = lex(pad.clone(), just('<'))
        .ignore_then(
            lex(pad_with_newline.clone(), ident.clone())
                .separated_by(comma.clone())
                .allow_trailing()
                .collect::<Vec<_>>()
                .or_not()
                .map(|items| items.unwrap_or_default()),
        )
        .then_ignore(lex(pad_with_newline.clone(), just('>')))
        .or_not()
        .map(|items| {
            GenericTypes(
                items
                    .unwrap_or_default()
                    .into_iter()
                    .map(|(name, sp)| GenericType {
                        identifier: PotentialDollarIdentifier::new(sp, name),
                        trait_constraints: Vec::new(),
                    })
                    .collect(),
            )
        })
        .boxed();

    let string_text = lex(
        pad.clone(),
        just('"')
            .ignore_then(
                choice((
                    just('\\').ignore_then(any()).map(|c| format!("\\{c}")),
                    filter(|c: &char| *c != '"' && *c != '\n').map(|c| c.to_string()),
                ))
                .repeated()
                .collect::<Vec<_>>(),
            )
            .then_ignore(just('"')),
    )
    .map(move |parts: Vec<String>| unescape_string(&parts.concat()))
    .boxed();

    let string_lit = string_text
        .clone()
        .map_with_span({
            let ls = line_starts.clone();
            move |text: String, r| {
                let sp = span(ls.as_ref(), r);
                AstNode::new(sp, AstNodeType::StringLiteral(ParserText::new(sp, text)))
            }
        })
        .boxed();

    let char_lit = lex(
        pad.clone(),
        just('\'')
            .ignore_then(
                choice((
                    just('\\').ignore_then(any()).map(|c| format!("\\{c}")),
                    filter(|c: &char| *c != '\'' && *c != '\n').map(|c| c.to_string()),
                ))
                .repeated()
                .at_least(1)
                .collect::<Vec<_>>(),
            )
            .then_ignore(just('\'')),
    )
    .try_map({
        let ls = line_starts.clone();
        move |parts: Vec<String>, parser_sp| {
            let sp = span(ls.as_ref(), parser_sp.into_range());
            match unescape_char_literal(&parts.concat()) {
                Some(ch) => Ok(AstNode::new(sp, AstNodeType::CharLiteral(ch))),
                None => Err(Rich::custom(
                    parser_sp,
                    "invalid char literal escape sequence",
                )),
            }
        }
    })
    .boxed();

    let dec_digits = any()
        .filter(|c: &char| c.is_ascii_digit() || *c == '_')
        .repeated()
        .at_least(1)
        .collect::<String>()
        .try_map(|s: String, parser_span| {
            if s.chars().any(|c| c.is_ascii_digit()) {
                Ok(s)
            } else {
                Err(Rich::custom(parser_span, "expected digit sequence"))
            }
        })
        .boxed();

    let exponent_part = just('e')
        .ignore_then(choice((just('+'), just('-'))).or_not())
        .then(dec_digits.clone())
        .map(|(sign, exp)| match sign {
            Some(sign) => format!("{sign}{exp}"),
            None => exp,
        })
        .boxed();

    let int_lit = lex(
        pad.clone(),
        dec_digits
            .clone()
            .then(exponent_part.clone().or_not())
            .then(
                choice((just('u'), just('i'), just('b')))
                    .or_not()
                    .map(|suffix| suffix.unwrap_or('\0')),
            )
            .map(|((number, exp), suffix)| {
                let mut out = number;
                if let Some(exp) = exp {
                    out.push('e');
                    out.push_str(&exp);
                }
                if suffix != '\0' {
                    out.push(suffix);
                }
                out
            }),
    )
    .map_with_span({
        let ls = line_starts.clone();
        move |number: String, r| {
            let sp = span(ls.as_ref(), r);
            AstNode::new(sp, AstNodeType::IntLiteral(ParserText::new(sp, number)))
        }
    })
    .boxed();

    let float_lit = lex(
        pad.clone(),
        choice((
            dec_digits
                .clone()
                .then(choice((just('f'), just('g'))))
                .map(|n| (n.0, Some(n.1))),
            dec_digits
                .clone()
                .then_ignore(just('.'))
                .then(dec_digits.clone())
                .then(exponent_part.clone().or_not())
                .then(choice((just('f'), just('g'))).or_not())
                .map(|(((a, b), exp), typ)| match exp {
                    Some(exp) => (format!("{a}.{b}e{exp}"), typ),
                    None => (format!("{a}.{b}"), typ),
                }),
            dec_digits
                .clone()
                .then(exponent_part.clone())
                .then_ignore(choice((just('u'), just('i'), just('b'))).not())
                .then(choice((just('f'), just('g'))).or_not())
                .map(|((number, exp), typ)| (format!("{number}e{exp}"), typ)),
        )),
    )
    .map_with_span({
        let ls = line_starts.clone();
        move |(number, typ), r| {
            let sp = span(ls.as_ref(), r);
            AstNode::new(
                sp,
                if typ == Some('g') {
                    AstNodeType::BigLiteral(ParserText::new(sp, number.replace('_', "")))
                } else {
                    AstNodeType::FloatLiteral(
                        number.replace('_', "").parse::<f64>().unwrap_or_default(),
                    )
                },
            )
        }
    })
    .boxed();

    let null_lit = lex(pad.clone(), just("null"))
        .map_with_span({
            let ls = line_starts.clone();
            move |_, r| AstNode::null(span(ls.as_ref(), r))
        })
        .boxed();

    let type_name: Boxed<'_, '_, &str, ParserDataType, extra::Full<Rich<'_, char>, (), ()>> =
        recursive(
            |ty: Recursive<
                dyn Parser<'_, &str, ParserDataType, extra::Full<Rich<'_, char>, (), ()>>,
            >| {
                let type_path = raw_ident
                    .clone()
                    .then(
                        lex(pad_with_newline.clone(), just("::"))
                            .ignore_then(raw_ident.clone())
                            .repeated()
                            .collect::<Vec<_>>(),
                    )
                    .map(|((first, sp), rest)| {
                        if rest.is_empty() {
                            (vec![first], sp)
                        } else {
                            let mut text = vec![first];
                            for (segment, _) in rest {
                                text.push(segment);
                            }
                            (text, sp)
                        }
                    })
                    .boxed();

                let struct_with_generics = type_path
                    .clone()
                    .then(
                        lex(pad.clone(), just(":<"))
                            .ignore_then(
                                lex(pad_with_newline.clone(),ty.clone())
                                    .separated_by(comma.clone())
                                    .allow_trailing()
                                    .collect::<Vec<_>>()
                                    .or_not()
                                    .map(|items| items.unwrap_or_default()),
                            )
                            .then_ignore(lex(pad_with_newline.clone(), just('>')))
                            .or_not(),
                    )
                    .try_map(|((mut path, sp), generic_types), parser_sp| {
                        if let Some(mut generic_types) = generic_types {
                            let name = path.pop().unwrap();
                            let mut path : Vec<ParserDataType> = path.into_iter().map(|x| ParserDataType::new(
                                sp,
                                ParserInnerType::from_str(x.as_str()).unwrap(),
                            )).collect();


                            let end = match name.as_str() {
                                "dyn" => {
                                    let traits = generic_types
                                        .into_iter()
                                        .filter_map(|ty| {
                                            let text = ty.to_string().trim().to_string();
                                            (!text.is_empty()).then_some(text)
                                        })
                                        .collect::<Vec<_>>();
                                    ParserDataType::new(sp, ParserInnerType::DynamicTraits(traits))
                                },
                                "list" => {
                                    if generic_types.len() == 1 {
                                        ParserDataType::new(sp, ParserInnerType::List(Box::new(generic_types.pop().unwrap())))}
                                    else {
                                        return Err(Rich::custom(
                                            parser_sp,
                                            "expected exactly one type parameter with a 'list' type",
                                        ));
                                    }
                                },
                                "ptr" => {
                                    if generic_types.len() == 1 {
                                        ParserDataType::new(sp, ParserInnerType::Ptr(Box::new(generic_types.pop().unwrap())))}
                                    else {
                                        return Err(Rich::custom(
                                            parser_sp,
                                            "expected exactly one type parameter with a 'ptr' type",
                                        ))
                                    }
                                },
                                _ => {
                                    ParserDataType::new(
                                        sp,
                                        ParserInnerType::StructWithGenerics {
                                            identifier: name,
                                            generic_types,
                                        },
                                    )
                                }
                            };

                            if path.is_empty() {
                                Ok(end)
                            } else{
                                path.push(end);
                                Ok(ParserDataType::new(
                                sp,
                                ParserInnerType::Scope(path),
                            ))
                            }
                        } else {
                            let mut path: Vec<ParserDataType> = path.into_iter().map(|x| ParserDataType::new(
                                sp,
                                ParserInnerType::from_str(x.as_str()).unwrap(),
                            )).collect();

                            if path.len() > 1 {
Ok(ParserDataType::new(
                                sp,
                                ParserInnerType::Scope(path),
                            ))
                            }else{
                                Ok(path.pop().unwrap())
                            }
                        }
                    })
                    .boxed();

                let base = choice((
                    lex(pad.clone(), just('<'))
                        .ignore_then(
                            ty.clone()
                                .separated_by(comma.clone())
                                .allow_trailing()
                                .collect::<Vec<_>>()
                                .or_not()
                                .map(|items: Option<Vec<ParserDataType>>| {
                                    items.unwrap_or_default()
                                }),
                        )
                        .then_ignore(lex(pad.clone(), just('>')))
                        .map_with_span({
                            let ls = line_starts.clone();
                            move |mut types, r| {
                                if types.len() == 1 {
                                    types.pop().unwrap()
                                }else {
                                ParserDataType::new(
                                    span(ls.as_ref(), r),
                                    ParserInnerType::Tuple(types),
                                )}
                            }
                        }),
                    lex(pad.clone(), just('@'))
                        .ignore_then(raw_ident.clone())
                        .map(|(name, sp)| {
                            let ffi = ParserFfiInnerType::from_str(&name)
                                .unwrap_or(ParserFfiInnerType::Int);
                            ParserDataType::new(sp, ParserInnerType::FfiType(ffi))
                        }),
                    lex(pad.clone(), just("fn"))
                        .ignore_then(lex(pad.clone(), just('(')))
                        .ignore_then(
                            lex(pad_with_newline.clone(),ty.clone())
                                .separated_by(comma.clone())
                                .allow_trailing()
                                .collect::<Vec<_>>()
                                .or_not()
                                .map(|items: Option<Vec<ParserDataType>>| {
                                    items.unwrap_or_default()
                                }),
                        )
                        .then_ignore(lex(pad_with_newline.clone(), just(')')))
                        .then(arrow.clone().ignore_then(ty.clone()).or_not())
                        .map_with_span({
                            let ls = line_starts.clone();
                            move |(parameters, ret), r| {
                                let sp = span(ls.as_ref(), r);
                                ParserDataType::new(
                                    sp,
                                    ParserInnerType::Function {
                                        return_type: Box::new(
                                            ret.unwrap_or(ParserDataType::null(sp)),
                                        ),
                                        parameters,
                                    },
                                )
                            }
                        }),
                    struct_with_generics,
                    lex(pad.clone(), just('$'))
                        .ignore_then(raw_ident.clone())
                        .map(|(name, sp)| {
                            ParserDataType::new(sp, ParserInnerType::DollarIdentifier(name))
                        }),
                ));

                choice((
                    lex(pad.clone(), just("mut"))
                        .ignore_then(ty.clone())
                        .map_with_span({
                            let ls = line_starts.clone();
                            move |inner, r| {
                                let sp = span(ls.as_ref(), r);
                                ParserDataType::new(
                                    sp,
                                    ParserInnerType::Ref(Box::new(inner), RefMutability::MutValue),
                                )
                            }
                        }),
                    lex(pad.clone(), just("&mut"))
                        .ignore_then(ty.clone())
                        .map_with_span({
                            let ls = line_starts.clone();
                            move |inner, r| {
                                let sp = span(ls.as_ref(), r);
                                ParserDataType::new(
                                    sp,
                                    ParserInnerType::Ref(Box::new(inner), RefMutability::MutRef),
                                )
                            }
                        }),
                    lex(pad.clone(), just('&'))
                        .ignore_then(ty.clone())
                        .map_with_span({
                            let ls = line_starts.clone();
                            move |inner, r| {
                                let sp = span(ls.as_ref(), r);
                                ParserDataType::new(
                                    sp,
                                    ParserInnerType::Ref(Box::new(inner), RefMutability::Ref),
                                )
                            }
                        }),
                    base,
                ))
                .then(lex(pad.clone(), just('!')).ignore_then(ty.clone()).or_not())
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
                })
                .then(lex(pad.clone(), just('?')).or_not())
                .map(|(inner, option)| {
                    if option.is_some() {
                        ParserDataType::new(inner.span, ParserInnerType::Option(Box::new(inner)))
                    } else {
                        inner
                    }
                })
                .boxed()
            },
        )
        .boxed();

    ParserPrelude {
        pad,
        pad_with_newline,
        delim,
        comma,
        arrow,
        fat_arrow,
        left_arrow,
        raw_ident,
        ident,
        dollar_ident,
        named_ident,
        generic_params,
        string_text,
        string_lit,
        char_lit,
        int_lit,
        float_lit,
        null_lit,
        type_name,
    }
}
