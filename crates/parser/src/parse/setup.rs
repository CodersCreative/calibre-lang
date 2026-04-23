use super::{LegacySpanMapExt, filter};
use crate::parse::util::{
    auto_type, is_keyword, lex, null_node, span, unescape_char_literal, unescape_string,
};
use crate::{
    Span,
    ast::{
        GenericType, GenericTypes, Node, NodeType, ParserDataType, ParserFfiInnerType,
        ParserInnerType, ParserText, PotentialDollarIdentifier, PotentialNewType, RefMutability,
    },
};
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
    pub string_lit: StrParser<'a, Node>,
    pub char_lit: StrParser<'a, Node>,
    pub int_lit: StrParser<'a, Node>,
    pub float_lit: StrParser<'a, Node>,
    pub null_lit: StrParser<'a, Node>,
    pub type_name: StrParser<'a, PotentialNewType>,
}

fn parser_data_type_or_auto(value: PotentialNewType) -> ParserDataType {
    match value {
        PotentialNewType::DataType(data_type) => data_type,
        _ => ParserDataType::new(Span::default(), ParserInnerType::Auto(None)),
    }
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

    let pad_with_newline = choice((ws, comment, just('\n').ignored()))
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
            .map(|(name, sp)| PotentialDollarIdentifier::Identifier(ParserText::new(sp, name))),
        dollar_ident.clone(),
    ))
    .boxed();

    let generic_params = lex(pad.clone(), just('<'))
        .ignore_then(
            ident
                .clone()
                .separated_by(comma.clone())
                .allow_trailing()
                .collect::<Vec<_>>()
                .or_not()
                .map(|items| items.unwrap_or_default()),
        )
        .then_ignore(lex(pad.clone(), just('>')))
        .or_not()
        .map(|items| {
            GenericTypes(
                items
                    .unwrap_or_default()
                    .into_iter()
                    .map(|(name, sp)| GenericType {
                        identifier: PotentialDollarIdentifier::Identifier(ParserText::new(
                            sp, name,
                        )),
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
                Node::new(sp, NodeType::StringLiteral(ParserText::new(sp, text)))
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
                Some(ch) => Ok(Node::new(sp, NodeType::CharLiteral(ch))),
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
        move |number: String, r| Node::new(span(ls.as_ref(), r), NodeType::IntLiteral(number))
    })
    .boxed();

    let float_lit = lex(
        pad.clone(),
        choice((
            dec_digits.clone().then_ignore(just('f')).map(|n| n),
            dec_digits
                .clone()
                .then_ignore(just('.'))
                .then(dec_digits.clone())
                .then(exponent_part.clone().or_not())
                .then_ignore(just('f').or_not())
                .map(|((a, b), exp)| match exp {
                    Some(exp) => format!("{a}.{b}e{exp}"),
                    None => format!("{a}.{b}"),
                }),
            dec_digits
                .clone()
                .then(exponent_part.clone())
                .then_ignore(choice((just('u'), just('i'), just('b'))).not())
                .then_ignore(just('f').or_not())
                .map(|(number, exp)| format!("{number}e{exp}")),
        )),
    )
    .map_with_span({
        let ls = line_starts.clone();
        move |number: String, r| {
            let value = number.replace('_', "").parse::<f64>().unwrap_or_default();
            Node::new(span(ls.as_ref(), r), NodeType::FloatLiteral(value))
        }
    })
    .boxed();

    let null_lit = lex(pad.clone(), just("null"))
        .map_with_span({
            let ls = line_starts.clone();
            move |_, r| null_node(span(ls.as_ref(), r))
        })
        .boxed();

    let type_name = recursive(|ty| {
        let type_path = raw_ident
            .clone()
            .then(
                lex(pad.clone(), just("::"))
                    .ignore_then(raw_ident.clone())
                    .repeated()
                    .collect::<Vec<_>>(),
            )
            .map(|((first, sp), rest)| {
                if rest.is_empty() {
                    (first, sp)
                } else {
                    let mut text = first;
                    for (segment, _) in rest {
                        text.push_str("::");
                        text.push_str(&segment);
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
                        ty.clone()
                            .separated_by(comma.clone())
                            .allow_trailing()
                            .collect::<Vec<_>>()
                            .or_not()
                            .map(|items| items.unwrap_or_default()),
                    )
                    .then_ignore(lex(pad.clone(), just('>')))
                    .or_not(),
            )
            .map(|((name, sp), generic_types)| {
                if let Some(generic_types) = generic_types {
                    if name == "dyn" {
                        let traits = generic_types
                            .into_iter()
                            .filter_map(|ty| {
                                let text = match ty {
                                    PotentialNewType::DataType(data_type) => data_type.to_string(),
                                    _ => String::new(),
                                };
                                let text = text.trim().to_string();
                                (!text.is_empty()).then_some(text)
                            })
                            .collect::<Vec<_>>();
                        PotentialNewType::DataType(ParserDataType::new(
                            sp,
                            ParserInnerType::DynamicTraits(traits),
                        ))
                    } else {
                        PotentialNewType::DataType(ParserDataType::new(
                            sp,
                            ParserInnerType::StructWithGenerics {
                                identifier: name,
                                generic_types: generic_types
                                    .into_iter()
                                    .map(parser_data_type_or_auto)
                                    .collect(),
                            },
                        ))
                    }
                } else {
                    PotentialNewType::DataType(ParserDataType::new(
                        sp,
                        match name.as_str() {
                            "int" => ParserInnerType::Int,
                            "uint" => ParserInnerType::UInt,
                            "byte" => ParserInnerType::Byte,
                            "float" => ParserInnerType::Float,
                            "bool" => ParserInnerType::Bool,
                            "str" => ParserInnerType::Str,
                            "char" => ParserInnerType::Char,
                            "dyn" => ParserInnerType::Dynamic,
                            "null" => ParserInnerType::Null,
                            "auto" => ParserInnerType::Auto(None),
                            _ => ParserInnerType::Struct(name),
                        },
                    ))
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
                        .map(|items| items.unwrap_or_default()),
                )
                .then_ignore(lex(pad.clone(), just('>')))
                .map_with_span({
                    let ls = line_starts.clone();
                    move |types, r| {
                        PotentialNewType::DataType(ParserDataType::new(
                            span(ls.as_ref(), r),
                            ParserInnerType::Tuple(
                                types.into_iter().map(parser_data_type_or_auto).collect(),
                            ),
                        ))
                    }
                }),
            lex(pad.clone(), just('@'))
                .ignore_then(raw_ident.clone())
                .map(|(name, sp)| {
                    let ffi =
                        ParserFfiInnerType::from_str(&name).unwrap_or(ParserFfiInnerType::Int);
                    PotentialNewType::DataType(ParserDataType::new(
                        sp,
                        ParserInnerType::FfiType(ffi),
                    ))
                }),
            lex(pad.clone(), just("list"))
                .ignore_then(lex(pad.clone(), just(":<")))
                .ignore_then(ty.clone())
                .then_ignore(lex(pad.clone(), just('>')))
                .map_with_span({
                    let ls = line_starts.clone();
                    move |inner, r| {
                        PotentialNewType::DataType(ParserDataType::new(
                            span(ls.as_ref(), r),
                            ParserInnerType::List(Box::new(parser_data_type_or_auto(inner))),
                        ))
                    }
                }),
            lex(pad.clone(), just("ptr"))
                .ignore_then(lex(pad.clone(), just(":<")))
                .ignore_then(ty.clone())
                .then_ignore(lex(pad.clone(), just('>')))
                .map_with_span({
                    let ls = line_starts.clone();
                    move |inner, r| {
                        PotentialNewType::DataType(ParserDataType::new(
                            span(ls.as_ref(), r),
                            ParserInnerType::Ptr(Box::new(parser_data_type_or_auto(inner))),
                        ))
                    }
                }),
            lex(pad.clone(), just("fn"))
                .ignore_then(lex(pad.clone(), just('(')))
                .ignore_then(
                    ty.clone()
                        .separated_by(comma.clone())
                        .allow_trailing()
                        .collect::<Vec<_>>()
                        .or_not()
                        .map(|items| items.unwrap_or_default()),
                )
                .then_ignore(lex(pad.clone(), just(')')))
                .then(arrow.clone().ignore_then(ty.clone()).or_not())
                .map_with_span({
                    let ls = line_starts.clone();
                    move |(params, ret), r| {
                        let sp = span(ls.as_ref(), r);
                        let return_type =
                            parser_data_type_or_auto(ret.unwrap_or_else(|| auto_type(sp)));
                        PotentialNewType::DataType(ParserDataType::new(
                            sp,
                            ParserInnerType::Function {
                                return_type: Box::new(return_type),
                                parameters: params
                                    .into_iter()
                                    .map(parser_data_type_or_auto)
                                    .collect(),
                            },
                        ))
                    }
                }),
            struct_with_generics,
            lex(pad.clone(), just('$'))
                .ignore_then(raw_ident.clone())
                .map(|(name, sp)| {
                    PotentialNewType::DataType(ParserDataType::new(
                        sp,
                        ParserInnerType::DollarIdentifier(name),
                    ))
                }),
        ));

        choice((
            lex(pad.clone(), just("mut"))
                .ignore_then(ty.clone())
                .map_with_span({
                    let ls = line_starts.clone();
                    move |inner, r| {
                        PotentialNewType::DataType(ParserDataType::new(
                            span(ls.as_ref(), r),
                            ParserInnerType::Ref(
                                Box::new(parser_data_type_or_auto(inner)),
                                RefMutability::MutValue,
                            ),
                        ))
                    }
                }),
            lex(pad.clone(), just("&mut"))
                .ignore_then(ty.clone())
                .map_with_span({
                    let ls = line_starts.clone();
                    move |inner, r| {
                        PotentialNewType::DataType(ParserDataType::new(
                            span(ls.as_ref(), r),
                            ParserInnerType::Ref(
                                Box::new(parser_data_type_or_auto(inner)),
                                RefMutability::MutRef,
                            ),
                        ))
                    }
                }),
            lex(pad.clone(), just('&'))
                .ignore_then(ty.clone())
                .map_with_span({
                    let ls = line_starts.clone();
                    move |inner, r| {
                        PotentialNewType::DataType(ParserDataType::new(
                            span(ls.as_ref(), r),
                            ParserInnerType::Ref(
                                Box::new(parser_data_type_or_auto(inner)),
                                RefMutability::Ref,
                            ),
                        ))
                    }
                }),
            base,
        ))
        .then(lex(pad.clone(), just('!')).ignore_then(ty.clone()).or_not())
        .map(|(left, right)| {
            if let Some(right) = right {
                let err = parser_data_type_or_auto(left);
                let ok = parser_data_type_or_auto(right);
                PotentialNewType::DataType(ParserDataType::new(
                    Span::new_from_spans(err.span, ok.span),
                    ParserInnerType::Result {
                        ok: Box::new(ok),
                        err: Box::new(err),
                    },
                ))
            } else {
                left
            }
        })
        .then(lex(pad.clone(), just('?')).or_not())
        .map(|(inner, option)| {
            if option.is_some() {
                match inner {
                    PotentialNewType::DataType(data_type) => {
                        PotentialNewType::DataType(ParserDataType::new(
                            data_type.span,
                            ParserInnerType::Option(Box::new(data_type)),
                        ))
                    }
                    _ => inner,
                }
            } else {
                inner
            }
        })
        .boxed()
    })
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
