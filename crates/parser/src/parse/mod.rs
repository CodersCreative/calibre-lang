use chumsky::error::Rich;
use chumsky::prelude::*;
use std::str::FromStr;
use std::sync::Arc;

mod diagnostics;
mod util;

use crate::{
    ParserError, Span,
    ast::{
        AsFailureMode, CallArg, DestructurePattern, FunctionHeader, GenericType, GenericTypes,
        IfComparisonType, LoopType, MatchArmType, MatchStructFieldPattern, MatchTupleItem,
        NamedScope, Node, NodeType, ObjectType, Overload, ParserDataType, ParserFfiInnerType,
        ParserInnerType, ParserText, PipeSegment, PotentialDollarIdentifier,
        PotentialGenericTypeIdentifier, PotentialNewType, RefMutability, SelectArm, SelectArmKind,
        TraitMember, TraitMemberKind, TryCatch, TypeDefType, VarType,
        binary::BinaryOperator,
        comparison::{BooleanOperator, ComparisonOperator},
    },
};
use diagnostics::to_parser_errors;
use util::{
    auto_type, ensure_scope_node, ident_node, is_keyword, lex, normalize_scope_member_chain,
    parse_embedded_expr, parse_splits, scope_body_or_single, scope_node, scope_node_parser, span,
    strip_block_comments_keep_layout, unescape_char, unescape_string, with_named_scope,
};

trait LegacySpanMapExt<'a, O>: Parser<'a, &'a str, O, extra::Err<Rich<'a, char>>> + Sized {
    fn map_with_span<U, F>(self, f: F) -> impl Parser<'a, &'a str, U, extra::Err<Rich<'a, char>>>
    where
        F: Fn(O, std::ops::Range<usize>) -> U + Clone + 'a,
    {
        self.map_with(move |out, e| f(out, e.span().into_range()))
    }
}

impl<'a, O, P> LegacySpanMapExt<'a, O> for P where
    P: Parser<'a, &'a str, O, extra::Err<Rich<'a, char>>> + Sized
{
}

fn filter<'a, F>(f: F) -> impl Parser<'a, &'a str, char, extra::Err<Rich<'a, char>>> + Clone
where
    F: Fn(&char) -> bool + Clone + 'a,
{
    any().filter(f)
}

fn take_until<'a, P, O>(
    end: P,
) -> impl Parser<'a, &'a str, String, extra::Err<Rich<'a, char>>> + Clone
where
    P: Parser<'a, &'a str, O, extra::Err<Rich<'a, char>>> + Clone + 'a,
{
    any().and_is(end.not()).repeated().collect::<String>()
}

pub fn parse_program(source: &str) -> Result<Node, Vec<ParserError>> {
    parse_program_with_source(source, None)
}

pub fn parse_program_with_source(
    source: &str,
    source_path: Option<&std::path::Path>,
) -> Result<Node, Vec<ParserError>> {
    let source = strip_block_comments_keep_layout(source);
    let source = source.as_str();
    let line_starts: Arc<Vec<usize>> = Arc::new(
        std::iter::once(0)
            .chain(source.match_indices('\n').map(|(i, _)| i + 1))
            .collect(),
    );

    let ws = filter(|c: &char| *c == ' ' || *c == '\t' || *c == '\r')
        .repeated()
        .at_least(1)
        .ignored();
    let line_comment = just("//").then(take_until(just('\n'))).ignored();
    let block_comment = just("/*")
        .then(take_until(just("*/")))
        .then_ignore(just("*/"))
        .ignored();
    let pad = choice((ws.clone(), line_comment.clone(), block_comment.clone()))
        .repeated()
        .ignored()
        .boxed();
    let pad_with_newline = choice((ws, line_comment, block_comment, just('\n').ignored()))
        .repeated()
        .ignored()
        .boxed();
    let delim = choice((just('\n'), just(';')))
        .padded_by(pad.clone())
        .repeated()
        .at_least(1)
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
        .try_map(|s: String, span| {
            if is_keyword(&s) {
                Err(Rich::custom(span, "keyword cannot be identifier"))
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
        .map(|(n, sp)| PotentialDollarIdentifier::DollarIdentifier(ParserText::new(sp, n)))
        .boxed();
    let named_ident = choice((
        ident
            .clone()
            .map(|(n, sp)| PotentialDollarIdentifier::Identifier(ParserText::new(sp, n))),
        dollar_ident.clone(),
    ))
    .boxed();
    let generic_params = lex(pad.clone(), just('<'))
        .ignore_then(
            ident
                .clone()
                .separated_by(lex(pad.clone(), just(',')))
                .allow_trailing()
                .collect::<Vec<_>>()
                .or_not()
                .map(|x| x.unwrap_or_default()),
        )
        .then_ignore(lex(pad.clone(), just('>')))
        .or_not()
        .map(|items| {
            GenericTypes(
                items
                    .unwrap_or_default()
                    .into_iter()
                    .map(|(n, sp)| GenericType {
                        identifier: PotentialDollarIdentifier::Identifier(ParserText::new(sp, n)),
                        trait_constraints: Vec::new(),
                    })
                    .collect(),
            )
        })
        .boxed();

    let string_lit = lex(
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
    .map_with_span({
        let ls = line_starts.clone();
        move |parts: Vec<String>, r| {
            let sp = span(ls.as_ref(), r);
            Node::new(
                sp,
                NodeType::StringLiteral(ParserText::new(sp, unescape_string(&parts.concat()))),
            )
        }
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
    .map(|parts: Vec<String>| unescape_string(&parts.concat()))
    .boxed();

    let char_lit = lex(
        pad.clone(),
        just('\'')
            .ignore_then(choice((
                just('\\').ignore_then(any()).map(|c| (true, c)),
                filter(|c: &char| *c != '\'' && *c != '\n').map(|c| (false, c)),
            )))
            .then_ignore(just('\'')),
    )
    .map_with_span({
        let ls = line_starts.clone();
        move |(escaped, c): (bool, char), r| {
            let ch = if escaped { unescape_char(c) } else { c };
            Node::new(span(ls.as_ref(), r), NodeType::CharLiteral(ch))
        }
    })
    .boxed();

    let dec_digits = any()
        .filter(|c: &char| c.is_ascii_digit() || *c == '_')
        .repeated()
        .at_least(1)
        .collect::<String>()
        .try_map(|s: String, sp| {
            if s.chars().any(|c| c.is_ascii_digit()) {
                Ok(s)
            } else {
                Err(Rich::custom(sp, "expected digits"))
            }
        })
        .boxed();

    let float_suffix_lit = lex(
        pad.clone(),
        dec_digits.clone().then_ignore(just('f')).map(|n| n),
    )
    .map_with_span({
        let ls = line_starts.clone();
        move |n: String, r| {
            let v = n.replace('_', "").parse::<f64>().unwrap_or_default();
            Node::new(span(ls.as_ref(), r), NodeType::FloatLiteral(v))
        }
    })
    .boxed();

    let int_lit = lex(
        pad.clone(),
        dec_digits
            .clone()
            .then(
                choice((just('u'), just('i')))
                    .or_not()
                    .map(|x| x.unwrap_or('\0')),
            )
            .map(|(n, suffix)| {
                if suffix == '\0' {
                    n
                } else {
                    let mut out = n;
                    out.push(suffix);
                    out
                }
            }),
    )
    .map_with_span({
        let ls = line_starts.clone();
        move |n: String, r| Node::new(span(ls.as_ref(), r), NodeType::IntLiteral(n))
    })
    .boxed();

    let float_lit = lex(
        pad.clone(),
        dec_digits
            .clone()
            .then_ignore(just('.'))
            .then(dec_digits.clone())
            .then_ignore(just('f').or_not())
            .map(|(a, b)| format!("{a}.{b}")),
    )
    .map_with_span({
        let ls = line_starts.clone();
        move |n: String, r| {
            let v = n.replace('_', "").parse::<f64>().unwrap_or_default();
            Node::new(span(ls.as_ref(), r), NodeType::FloatLiteral(v))
        }
    })
    .boxed();

    let null_lit = lex(pad.clone(), just("null"))
        .map_with_span({
            let ls = line_starts.clone();
            move |_, r| Node::new(span(ls.as_ref(), r), NodeType::Null)
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
                    let mut txt = first;
                    for (seg, _) in rest {
                        txt.push_str("::");
                        txt.push_str(&seg);
                    }
                    (txt, sp)
                }
            })
            .boxed();

        let struct_with_generics = type_path
            .clone()
            .then(
                lex(pad.clone(), just(":<"))
                    .ignore_then(
                        ty.clone()
                            .separated_by(lex(pad.clone(), just(',')))
                            .allow_trailing()
                            .collect::<Vec<_>>()
                            .or_not()
                            .map(|x| x.unwrap_or_default()),
                    )
                    .then_ignore(lex(pad.clone(), just('>')))
                    .or_not(),
            )
            .map(|((name, sp), generic_types)| {
                if let Some(generic_types) = generic_types {
                    let generic_types = generic_types
                        .into_iter()
                        .map(|x| match x {
                            PotentialNewType::DataType(y) => y,
                            _ => ParserDataType::new(Span::default(), ParserInnerType::Auto(None)),
                        })
                        .collect::<Vec<_>>();
                    PotentialNewType::DataType(ParserDataType::new(
                        sp,
                        ParserInnerType::StructWithGenerics {
                            identifier: name,
                            generic_types,
                        },
                    ))
                } else {
                    PotentialNewType::DataType(ParserDataType::new(
                        sp,
                        match name.as_str() {
                            "int" => ParserInnerType::Int,
                            "uint" => ParserInnerType::UInt,
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
                        .separated_by(lex(pad.clone(), just(',')))
                        .allow_trailing()
                        .collect::<Vec<_>>()
                        .or_not()
                        .map(|x| x.unwrap_or_default()),
                )
                .then_ignore(lex(pad.clone(), just('>')))
                .map(|types| {
                    let items = types
                        .into_iter()
                        .map(|x| match x {
                            PotentialNewType::DataType(y) => y,
                            _ => ParserDataType::new(Span::default(), ParserInnerType::Auto(None)),
                        })
                        .collect::<Vec<_>>();
                    PotentialNewType::DataType(ParserDataType::new(
                        Span::default(),
                        ParserInnerType::Tuple(items),
                    ))
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
                .map(|inner| {
                    let dt = match inner {
                        PotentialNewType::DataType(x) => x,
                        _ => ParserDataType::new(Span::default(), ParserInnerType::Auto(None)),
                    };
                    PotentialNewType::DataType(ParserDataType::new(
                        Span::default(),
                        ParserInnerType::List(Box::new(dt)),
                    ))
                }),
            lex(pad.clone(), just("ptr"))
                .ignore_then(lex(pad.clone(), just(":<")))
                .ignore_then(ty.clone())
                .then_ignore(lex(pad.clone(), just('>')))
                .map(|inner| {
                    let dt = match inner {
                        PotentialNewType::DataType(x) => x,
                        _ => ParserDataType::new(Span::default(), ParserInnerType::Auto(None)),
                    };
                    PotentialNewType::DataType(ParserDataType::new(
                        Span::default(),
                        ParserInnerType::Ptr(Box::new(dt)),
                    ))
                }),
            lex(pad.clone(), just("fn"))
                .ignore_then(lex(pad.clone(), just('(')))
                .ignore_then(
                    ty.clone()
                        .separated_by(lex(pad.clone(), just(',')))
                        .allow_trailing()
                        .collect::<Vec<_>>()
                        .or_not()
                        .map(|x| x.unwrap_or_default()),
                )
                .then_ignore(lex(pad.clone(), just(')')))
                .then(arrow.clone().ignore_then(ty.clone()).or_not())
                .map(|(params, ret)| {
                    let parameters: Vec<ParserDataType> = params
                        .into_iter()
                        .map(|x| match x {
                            PotentialNewType::DataType(y) => y,
                            _ => ParserDataType::new(Span::default(), ParserInnerType::Auto(None)),
                        })
                        .collect();
                    let ret = match ret.unwrap_or_else(|| auto_type(Span::default())) {
                        PotentialNewType::DataType(y) => y,
                        _ => ParserDataType::new(Span::default(), ParserInnerType::Auto(None)),
                    };
                    PotentialNewType::DataType(ParserDataType::new(
                        Span::default(),
                        ParserInnerType::Function {
                            return_type: Box::new(ret),
                            parameters,
                        },
                    ))
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
                .map(|inner| {
                    let dt = match inner {
                        PotentialNewType::DataType(x) => x,
                        _ => ParserDataType::new(Span::default(), ParserInnerType::Auto(None)),
                    };
                    PotentialNewType::DataType(ParserDataType::new(
                        Span::default(),
                        ParserInnerType::Ref(Box::new(dt), RefMutability::MutValue),
                    ))
                }),
            lex(pad.clone(), just("&mut"))
                .ignore_then(ty.clone())
                .map(|inner| {
                    let dt = match inner {
                        PotentialNewType::DataType(x) => x,
                        _ => ParserDataType::new(Span::default(), ParserInnerType::Auto(None)),
                    };
                    PotentialNewType::DataType(ParserDataType::new(
                        Span::default(),
                        ParserInnerType::Ref(Box::new(dt), RefMutability::MutRef),
                    ))
                }),
            lex(pad.clone(), just('&'))
                .ignore_then(ty.clone())
                .map(|inner| {
                    let dt = match inner {
                        PotentialNewType::DataType(x) => x,
                        _ => ParserDataType::new(Span::default(), ParserInnerType::Auto(None)),
                    };
                    PotentialNewType::DataType(ParserDataType::new(
                        Span::default(),
                        ParserInnerType::Ref(Box::new(dt), RefMutability::Ref),
                    ))
                }),
            base,
        ))
        .then(lex(pad.clone(), just('!')).ignore_then(ty.clone()).or_not())
        .map(|(left, right)| {
            if let Some(right) = right {
                let err = match left {
                    PotentialNewType::DataType(dt) => dt,
                    _ => ParserDataType::new(Span::default(), ParserInnerType::Auto(None)),
                };
                let ok = match right {
                    PotentialNewType::DataType(dt) => dt,
                    _ => ParserDataType::new(Span::default(), ParserInnerType::Auto(None)),
                };
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
        .map(|(inner, opt)| {
            if opt.is_some() {
                match inner {
                    PotentialNewType::DataType(dt) => PotentialNewType::DataType(
                        ParserDataType::new(dt.span, ParserInnerType::Option(Box::new(dt))),
                    ),
                    _ => inner,
                }
            } else {
                inner
            }
        })
        .boxed()
    })
    .boxed();

    let parser = recursive(|statement| {
        let expr = recursive(|expr| {
            let scope = scope_node_parser(statement.clone(), delim.clone(), pad.clone()).boxed();

            let labelled_scope = lex(pad.clone(), just('@'))
                .ignore_then(ident.clone())
                .then(lex(pad.clone(), just("[]")).or_not())
                .then(scope.clone())
                .map(|(((name, sp), _), body)| {
                    with_named_scope(
                        body,
                        NamedScope {
                            name: PotentialDollarIdentifier::Identifier(ParserText::new(sp, name)),
                            args: Vec::new(),
                        },
                    )
                })
                .boxed();

            #[derive(Clone)]
            enum FnParamGroup {
                Plain(Vec<(PotentialDollarIdentifier, PotentialNewType)>),
                Destructure {
                    pattern: DestructurePattern,
                    data_type: PotentialNewType,
                    span: Span,
                },
            }

            let tuple_destructure_param = lex(pad.clone(), just('('))
                .ignore_then(
                    choice((
                        lex(pad.clone(), just("..")).to(None),
                        lex(pad.clone(), just("mut"))
                            .or_not()
                            .then(ident.clone())
                            .map(|(mut_tok, (name, sp))| {
                                Some((
                                    if mut_tok.is_some() {
                                        VarType::Mutable
                                    } else {
                                        VarType::Immutable
                                    },
                                    PotentialDollarIdentifier::Identifier(ParserText::new(
                                        sp, name,
                                    )),
                                ))
                            }),
                    ))
                    .separated_by(lex(pad.clone(), just(',')))
                    .at_least(2)
                    .collect::<Vec<_>>(),
                )
                .then_ignore(lex(pad.clone(), just(')')))
                .then(
                    lex(pad.clone(), just(':'))
                        .ignore_then(type_name.clone())
                        .or_not(),
                )
                .map_with_span({
                    let ls = line_starts.clone();
                    move |(items, ty), r| FnParamGroup::Destructure {
                        pattern: DestructurePattern::Tuple(items),
                        data_type: ty.unwrap_or_else(|| auto_type(Span::default())),
                        span: span(ls.as_ref(), r),
                    }
                });

            let struct_destructure_param = lex(pad.clone(), just('{'))
                .ignore_then(
                    ident
                        .clone()
                        .then(
                            lex(pad.clone(), just(':'))
                                .ignore_then(lex(pad.clone(), just("mut")).or_not())
                                .then(ident.clone())
                                .or_not(),
                        )
                        .map(|((field, fsp), alias)| {
                            if let Some((mut_tok, (name, nsp))) = alias {
                                (
                                    field,
                                    if mut_tok.is_some() {
                                        VarType::Mutable
                                    } else {
                                        VarType::Immutable
                                    },
                                    PotentialDollarIdentifier::Identifier(ParserText::new(
                                        nsp, name,
                                    )),
                                )
                            } else {
                                (
                                    field.clone(),
                                    VarType::Immutable,
                                    PotentialDollarIdentifier::Identifier(ParserText::new(
                                        fsp, field,
                                    )),
                                )
                            }
                        })
                        .separated_by(lex(pad.clone(), just(',')))
                        .allow_trailing()
                        .collect::<Vec<_>>(),
                )
                .then_ignore(lex(pad.clone(), just('}')))
                .then(
                    lex(pad.clone(), just(':'))
                        .ignore_then(type_name.clone())
                        .or_not(),
                )
                .map_with_span({
                    let ls = line_starts.clone();
                    move |(items, ty), r| FnParamGroup::Destructure {
                        pattern: DestructurePattern::Struct(items),
                        data_type: ty.unwrap_or_else(|| auto_type(Span::default())),
                        span: span(ls.as_ref(), r),
                    }
                });

            let plain_param = ident
                .clone()
                .repeated()
                .at_least(1)
                .collect::<Vec<_>>()
                .then(
                    lex(pad.clone(), just(':'))
                        .ignore_then(type_name.clone())
                        .or_not(),
                )
                .map(|(names, ty)| {
                    let t = ty.unwrap_or_else(|| auto_type(Span::default()));
                    FnParamGroup::Plain(
                        names
                            .into_iter()
                            .map(|(n, sp)| {
                                (
                                    PotentialDollarIdentifier::Identifier(ParserText::new(sp, n)),
                                    t.clone(),
                                )
                            })
                            .collect::<Vec<_>>(),
                    )
                });

            let fn_param_groups = choice((
                tuple_destructure_param,
                struct_destructure_param,
                plain_param,
            ))
            .separated_by(lex(pad.clone(), just(',')))
            .allow_trailing()
            .collect::<Vec<_>>()
            .or_not()
            .map(|x| x.unwrap_or_default())
            .boxed();

            let fn_standard_expr = lex(pad.clone(), just("fn"))
                .ignore_then(generic_params.clone())
                .then(
                    lex(pad.clone(), just('('))
                        .ignore_then(fn_param_groups)
                        .then_ignore(lex(pad.clone(), just(')')))
                        .or_not()
                        .map(|x| x.unwrap_or_default()),
                )
                .then(arrow.clone().ignore_then(type_name.clone()).or_not())
                .then_ignore(fat_arrow.clone())
                .then(
                    lex(pad.clone(), just('{'))
                        .rewind()
                        .ignore_then(scope.clone())
                        .or(choice((statement.clone(), expr.clone()))
                            .map(|e| scope_node(vec![e], true, false))),
                )
                .map(|(((generics, params), ret), body)| {
                    let mut parameters = Vec::new();
                    let mut param_destructures = Vec::new();
                    let mut synthetic_idx = 0usize;
                    for group in params {
                        match group {
                            FnParamGroup::Plain(items) => parameters.extend(items),
                            FnParamGroup::Destructure {
                                pattern,
                                data_type,
                                span,
                            } => {
                                let param_index = parameters.len();
                                let synthetic_name =
                                    format!("__destructure_param_{}", synthetic_idx);
                                synthetic_idx += 1;
                                parameters.push((
                                    PotentialDollarIdentifier::Identifier(ParserText::new(
                                        span,
                                        synthetic_name,
                                    )),
                                    data_type,
                                ));
                                param_destructures.push((param_index, pattern));
                            }
                        }
                    }
                    let body = match body.node_type {
                        NodeType::ScopeDeclaration {
                            body,
                            named,
                            create_new_scope,
                            ..
                        } => Node::new(
                            body.as_ref()
                                .and_then(|b| b.first().zip(b.last()))
                                .map(|(a, b)| Span::new_from_spans(a.span, b.span))
                                .unwrap_or(Span::default()),
                            NodeType::ScopeDeclaration {
                                body,
                                named,
                                is_temp: true,
                                create_new_scope,
                                define: false,
                            },
                        ),
                        _ => ensure_scope_node(body, true, false),
                    };
                    Node::new(
                        body.span,
                        NodeType::FunctionDeclaration {
                            header: FunctionHeader {
                                generics,
                                parameters,
                                return_type: ret.unwrap_or_else(|| auto_type(body.span)),
                                param_destructures,
                            },
                            body: Box::new(body),
                        },
                    )
                })
                .boxed();

            let generic_ident = ident
                .clone()
                .then(
                    lex(pad.clone(), just(":<"))
                        .ignore_then(
                            type_name
                                .clone()
                                .separated_by(lex(pad.clone(), just(',')))
                                .allow_trailing()
                                .collect::<Vec<_>>()
                                .or_not()
                                .map(|x| x.unwrap_or_default()),
                        )
                        .then_ignore(lex(pad.clone(), just('>')))
                        .or_not(),
                )
                .map(|((n, sp), generics)| {
                    if let Some(generic_types) = generics {
                        PotentialGenericTypeIdentifier::Generic {
                            identifier: PotentialDollarIdentifier::Identifier(ParserText::new(
                                sp, n,
                            )),
                            generic_types,
                        }
                    } else {
                        PotentialGenericTypeIdentifier::Identifier(
                            PotentialDollarIdentifier::Identifier(ParserText::new(sp, n)),
                        )
                    }
                })
                .boxed();

            let struct_lit = generic_ident
                .clone()
                .then(
                    lex(pad.clone(), just('{'))
                        .ignore_then(
                            ident
                                .clone()
                                .then(
                                    lex(pad.clone(), just(':'))
                                        .ignore_then(expr.clone())
                                        .or_not(),
                                )
                                .map(|((k, sp), value)| {
                                    let value = value.unwrap_or_else(|| ident_node(sp, &k));
                                    (k, value)
                                })
                                .separated_by(lex(pad.clone(), just(',')))
                                .allow_trailing()
                                .collect::<Vec<_>>()
                                .or_not()
                                .map(|x| x.unwrap_or_default()),
                        )
                        .then_ignore(lex(pad.clone(), just('}'))),
                )
                .map(|(identifier, fields)| {
                    let sp = *identifier.span();
                    Node::new(
                        sp,
                        NodeType::StructLiteral {
                            identifier,
                            value: ObjectType::Map(fields),
                        },
                    )
                })
                .boxed();

            let match_var_type = choice((
                lex(pad.clone(), just("mut")).to(VarType::Mutable),
                lex(pad.clone(), just("const")).to(VarType::Constant),
                lex(pad.clone(), just("let")).to(VarType::Immutable),
            ));

            let optional_match_var_type = match_var_type
                .clone()
                .or_not()
                .map(|v| v.unwrap_or(VarType::Immutable))
                .boxed();

            let match_struct_destructure = lex(pad.clone(), just('{'))
                .ignore_then(
                    ident
                        .clone()
                        .then(
                            lex(pad.clone(), just(':'))
                                .ignore_then(lex(pad.clone(), just("mut")).or_not())
                                .then(ident.clone())
                                .or_not(),
                        )
                        .map(|((field, fsp), alias)| match alias {
                            Some((mut_tok, (name, nsp))) => (
                                field,
                                if mut_tok.is_some() {
                                    VarType::Mutable
                                } else {
                                    VarType::Immutable
                                },
                                PotentialDollarIdentifier::Identifier(ParserText::new(nsp, name)),
                            ),
                            None => (
                                field.clone(),
                                VarType::Immutable,
                                PotentialDollarIdentifier::Identifier(ParserText::new(fsp, field)),
                            ),
                        })
                        .separated_by(lex(pad.clone(), just(',')))
                        .allow_trailing()
                        .collect::<Vec<_>>()
                        .or_not()
                        .map(|x| x.unwrap_or_default()),
                )
                .then_ignore(lex(pad.clone(), just('}')))
                .map(DestructurePattern::Struct)
                .boxed();

            let match_tuple_destructure = lex(pad.clone(), just('('))
                .ignore_then(
                    choice((
                        lex(pad.clone(), just("..")).to(None),
                        choice((
                            lex(pad.clone(), just("mut")),
                            lex(pad.clone(), just("const")),
                        ))
                        .or_not()
                        .then(ident.clone())
                        .map(|(mut_tok, (name, sp))| {
                            Some((
                                match mut_tok {
                                    Some("mut") => VarType::Mutable,
                                    Some("const") => VarType::Constant,
                                    _ => VarType::Immutable,
                                },
                                PotentialDollarIdentifier::Identifier(ParserText::new(sp, name)),
                            ))
                        }),
                    ))
                    .separated_by(lex(pad.clone(), just(',')))
                    .allow_trailing()
                    .collect::<Vec<_>>()
                    .or_not()
                    .map(|x| x.unwrap_or_default()),
                )
                .then_ignore(lex(pad.clone(), just(')')))
                .map(DestructurePattern::Tuple)
                .boxed();

            let match_enum_tuple_pattern = lex(pad.clone(), just('('))
                .ignore_then(
                    choice((
                        lex(pad.clone(), just(".."))
                            .map_with_span({
                                let ls = line_starts.clone();
                                move |_, r| MatchTupleItem::Rest(span(ls.as_ref(), r))
                            })
                            .boxed(),
                        lex(pad.clone(), just('_'))
                            .map_with_span({
                                let ls = line_starts.clone();
                                move |_, r| MatchTupleItem::Wildcard(span(ls.as_ref(), r))
                            })
                            .boxed(),
                        match_var_type
                            .clone()
                            .then(ident.clone())
                            .map(|(var_type, (name, sp))| MatchTupleItem::Binding {
                                var_type,
                                name: PotentialDollarIdentifier::Identifier(ParserText::new(
                                    sp, name,
                                )),
                            })
                            .boxed(),
                        lex(pad.clone(), just('.'))
                            .ignore_then(ident.clone())
                            .then(
                                lex(pad.clone(), just(':'))
                                    .ignore_then(optional_match_var_type.clone())
                                    .then(choice((
                                        match_struct_destructure
                                            .clone()
                                            .map(|d| (None, Some(d), None)),
                                        match_tuple_destructure
                                            .clone()
                                            .map(|d| (None, Some(d), None)),
                                        ident.clone().map(|(name, sp)| {
                                            (
                                                Some(PotentialDollarIdentifier::Identifier(
                                                    ParserText::new(sp, name),
                                                )),
                                                None,
                                                None,
                                            )
                                        }),
                                    )))
                                    .or_not(),
                            )
                            .map(|((name, sp), bind)| {
                                let (var_type, name_bind, destructure, pattern) =
                                    if let Some((vt, (name_bind, destructure, pattern))) = bind {
                                        (vt, name_bind, destructure, pattern)
                                    } else {
                                        (VarType::Immutable, None, None, None)
                                    };
                                MatchTupleItem::Enum {
                                    value: PotentialDollarIdentifier::Identifier(ParserText::new(
                                        sp, name,
                                    )),
                                    var_type,
                                    name: name_bind,
                                    destructure,
                                    pattern,
                                }
                            })
                            .boxed(),
                        expr.clone().map(MatchTupleItem::Value),
                    ))
                    .separated_by(lex(pad.clone(), just(',')))
                    .allow_trailing()
                    .collect::<Vec<_>>()
                    .or_not()
                    .map(|x| x.unwrap_or_default()),
                )
                .then_ignore(lex(pad.clone(), just(')')))
                .map(MatchArmType::TuplePattern)
                .boxed();

            let match_enum_struct_pattern = lex(pad.clone(), just('{'))
                .ignore_then(
                    ident
                        .clone()
                        .then(
                            lex(pad.clone(), just(':'))
                                .ignore_then(
                                    match_var_type
                                        .clone()
                                        .then(ident.clone())
                                        .map(|(var_type, (name, sp))| {
                                            MatchStructFieldPattern::Binding {
                                                field: String::new(),
                                                var_type,
                                                name: PotentialDollarIdentifier::Identifier(
                                                    ParserText::new(sp, name),
                                                ),
                                            }
                                        })
                                        .or(expr.clone().map(|value| {
                                            MatchStructFieldPattern::Value {
                                                field: String::new(),
                                                value,
                                            }
                                        })),
                                )
                                .or_not(),
                        )
                        .map(|((field, sp), maybe)| match maybe {
                            Some(MatchStructFieldPattern::Binding { var_type, name, .. }) => {
                                MatchStructFieldPattern::Binding {
                                    field,
                                    var_type,
                                    name,
                                }
                            }
                            Some(MatchStructFieldPattern::Value { value, .. }) => {
                                MatchStructFieldPattern::Value { field, value }
                            }
                            None => MatchStructFieldPattern::Binding {
                                field: field.clone(),
                                var_type: VarType::Immutable,
                                name: PotentialDollarIdentifier::Identifier(ParserText::new(
                                    sp, field,
                                )),
                            },
                        })
                        .separated_by(lex(pad.clone(), just(',')))
                        .allow_trailing()
                        .collect::<Vec<_>>()
                        .or_not()
                        .map(|x| x.unwrap_or_default()),
                )
                .then_ignore(lex(pad.clone(), just('}')))
                .map(MatchArmType::StructPattern)
                .boxed();

            let match_enum_bind = lex(pad.clone(), just(':'))
                .ignore_then(optional_match_var_type.clone())
                .then(choice((
                    ident
                        .clone()
                        .then(
                            lex(pad.clone(), just(','))
                                .ignore_then(ident.clone())
                                .repeated()
                                .at_least(1)
                                .collect::<Vec<_>>(),
                        )
                        .map(|((first, fsp), rest)| {
                            let mut items = Vec::with_capacity(rest.len() + 1);
                            items.push(Some((
                                VarType::Immutable,
                                PotentialDollarIdentifier::Identifier(ParserText::new(fsp, first)),
                            )));
                            for (name, sp) in rest {
                                items.push(Some((
                                    VarType::Immutable,
                                    PotentialDollarIdentifier::Identifier(ParserText::new(
                                        sp, name,
                                    )),
                                )));
                            }
                            (None, Some(DestructurePattern::Tuple(items)), None)
                        }),
                    match_struct_destructure
                        .clone()
                        .map(|d| (None, Some(d), None)),
                    match_tuple_destructure
                        .clone()
                        .map(|d| (None, Some(d), None)),
                    match_enum_tuple_pattern
                        .clone()
                        .map(|p| (None, None, Some(Box::new(p)))),
                    match_enum_struct_pattern
                        .clone()
                        .map(|p| (None, None, Some(Box::new(p)))),
                    ident.clone().map(|(name, sp)| {
                        (
                            Some(PotentialDollarIdentifier::Identifier(ParserText::new(
                                sp, name,
                            ))),
                            None,
                            None,
                        )
                    }),
                )))
                .or_not()
                .boxed();

            let match_arm_start = choice((
                lex(pad.clone(), just('.'))
                    .ignore_then(ident.clone())
                    .then(match_enum_bind.clone())
                    .map(|((name, sp), bind)| {
                        let (var_type, name_bind, destructure, pattern) =
                            if let Some((vt, (name_bind, destructure, pattern))) = bind {
                                (vt, name_bind, destructure, pattern)
                            } else {
                                (VarType::Immutable, None, None, None)
                            };
                        MatchArmType::Enum {
                            value: PotentialDollarIdentifier::Identifier(ParserText::new(sp, name)),
                            var_type,
                            name: name_bind,
                            destructure,
                            pattern,
                        }
                    }),
                match_var_type
                    .clone()
                    .then(ident.clone())
                    .map(|(vt, (name, sp))| MatchArmType::Let {
                        var_type: vt,
                        name: PotentialDollarIdentifier::Identifier(ParserText::new(sp, name)),
                    }),
                lex(pad.clone(), just("..")).map_with_span({
                    let ls = line_starts.clone();
                    move |_, r| {
                        MatchArmType::TuplePattern(vec![MatchTupleItem::Rest(span(ls.as_ref(), r))])
                    }
                }),
                lex(pad.clone(), just('_')).map_with_span({
                    let ls = line_starts.clone();
                    move |_, r| MatchArmType::Wildcard(span(ls.as_ref(), r))
                }),
                expr.clone().map(MatchArmType::Value),
            ))
            .boxed();

            let match_arm_body = fat_arrow
                .clone()
                .ignore_then(pad_with_newline.clone().ignore_then(choice((
                    scope.clone(),
                    statement.clone(),
                    expr.clone(),
                ))))
                .map(|body| ensure_scope_node(body, true, false))
                .boxed();

            let match_struct_pattern = lex(pad.clone(), just('{'))
                .ignore_then(
                    choice((ident
                        .clone()
                        .then(
                            lex(pad.clone(), just(':'))
                                .ignore_then(
                                    lex(pad.clone(), just("mut"))
                                        .to(VarType::Mutable)
                                        .or(lex(pad.clone(), just("const")).to(VarType::Constant))
                                        .or_not()
                                        .then(ident.clone())
                                        .map(|(vt, (name, sp))| MatchStructFieldPattern::Binding {
                                            field: String::new(),
                                            var_type: vt.unwrap_or(VarType::Immutable),
                                            name: PotentialDollarIdentifier::Identifier(
                                                ParserText::new(sp, name),
                                            ),
                                        })
                                        .or(expr.clone().map(|node| {
                                            MatchStructFieldPattern::Value {
                                                field: String::new(),
                                                value: node,
                                            }
                                        })),
                                )
                                .or_not(),
                        )
                        .map(|((field, sp), maybe)| match maybe {
                            Some(MatchStructFieldPattern::Binding { var_type, name, .. }) => {
                                MatchStructFieldPattern::Binding {
                                    field,
                                    var_type,
                                    name,
                                }
                            }
                            Some(MatchStructFieldPattern::Value { value, .. }) => {
                                MatchStructFieldPattern::Value { field, value }
                            }
                            None => MatchStructFieldPattern::Binding {
                                field: field.clone(),
                                var_type: VarType::Immutable,
                                name: PotentialDollarIdentifier::Identifier(ParserText::new(
                                    sp, field,
                                )),
                            },
                        }),))
                    .separated_by(lex(pad.clone(), just(',')))
                    .allow_trailing()
                    .collect::<Vec<_>>()
                    .or_not()
                    .map(|x| x.unwrap_or_default()),
                )
                .then_ignore(lex(pad.clone(), just('}')))
                .map(MatchArmType::StructPattern)
                .boxed();

            let match_arm = choice((match_struct_pattern.clone(), match_arm_start.clone()))
                .then(
                    lex(pad.clone(), choice((just('|').to('|'), just(',').to(','))))
                        .then(choice((
                            match_struct_pattern.clone(),
                            match_arm_start.clone(),
                        )))
                        .repeated()
                        .collect::<Vec<(char, MatchArmType)>>(),
                )
                .then(
                    lex(pad.clone(), just("if"))
                        .ignore_then(expr.clone())
                        .repeated()
                        .collect::<Vec<_>>(),
                )
                .then(match_arm_body.clone())
                .map(|(((first, rest), conditions), body)| {
                    let mut values = vec![first.clone()];
                    values.extend(rest.iter().map(|(_, v)| v.clone()));
                    let has_comma = rest.iter().any(|(sep, _)| *sep == ',');

                    if has_comma {
                        let mut slots: Vec<Vec<MatchArmType>> = vec![vec![first.clone()]];
                        for (sep, arm) in rest {
                            if sep == ',' {
                                slots.push(vec![arm]);
                            } else if let Some(last) = slots.last_mut() {
                                last.push(arm);
                            }
                        }

                        let mut tuple_item_variants: Vec<Vec<Vec<MatchTupleItem>>> = Vec::new();
                        for slot in slots {
                            let mut variants = Vec::new();
                            for arm in slot {
                                let mut items = Vec::new();
                                match arm {
                                    MatchArmType::Wildcard(sp) => {
                                        items.push(MatchTupleItem::Wildcard(sp))
                                    }
                                    MatchArmType::Let { var_type, name } => {
                                        items.push(MatchTupleItem::Binding { var_type, name })
                                    }
                                    MatchArmType::Enum {
                                        value,
                                        var_type,
                                        name,
                                        destructure,
                                        pattern,
                                    } => items.push(MatchTupleItem::Enum {
                                        value,
                                        var_type,
                                        name,
                                        destructure,
                                        pattern,
                                    }),
                                    MatchArmType::Value(node) => {
                                        items.push(MatchTupleItem::Value(node))
                                    }
                                    MatchArmType::TuplePattern(inner) => items.extend(inner),
                                    MatchArmType::StructPattern(_) => {}
                                }
                                variants.push(items);
                            }
                            tuple_item_variants.push(variants);
                        }

                        let mut combos: Vec<Vec<MatchTupleItem>> = vec![Vec::new()];
                        for slot_variants in tuple_item_variants {
                            let mut next = Vec::new();
                            for prefix in &combos {
                                for variant in &slot_variants {
                                    let mut merged = prefix.clone();
                                    merged.extend(variant.clone());
                                    next.push(merged);
                                }
                            }
                            combos = next;
                        }

                        combos
                            .into_iter()
                            .map(|items| {
                                (
                                    MatchArmType::TuplePattern(items),
                                    conditions.clone(),
                                    Box::new(body.clone()),
                                )
                            })
                            .collect()
                    } else {
                        let shared_enum_payload = values.iter().rev().find_map(|v| match v {
                            MatchArmType::Enum {
                                var_type,
                                name,
                                destructure,
                                pattern,
                                ..
                            } if *var_type != VarType::Immutable
                                || name.is_some()
                                || destructure.is_some()
                                || pattern.is_some() =>
                            {
                                Some((
                                    *var_type,
                                    name.clone(),
                                    destructure.clone(),
                                    pattern.clone(),
                                ))
                            }
                            _ => None,
                        });

                        if let Some((shared_vt, shared_name, shared_destructure, shared_pattern)) =
                            shared_enum_payload
                        {
                            for value in values.iter_mut() {
                                if let MatchArmType::Enum {
                                    var_type,
                                    name,
                                    destructure,
                                    pattern,
                                    ..
                                } = value
                                    && *var_type == VarType::Immutable
                                    && name.is_none()
                                    && destructure.is_none()
                                    && pattern.is_none()
                                {
                                    *var_type = shared_vt;
                                    *name = shared_name.clone();
                                    *destructure = shared_destructure.clone();
                                    *pattern = shared_pattern.clone();
                                }
                            }
                        }

                        let mut out = Vec::with_capacity(values.len());
                        for value in values {
                            out.push((value, conditions.clone(), Box::new(body.clone())));
                        }
                        out
                    }
                })
                .boxed();

            let let_pattern_list = choice((match_struct_pattern.clone(), match_arm_start.clone()))
                .then(
                    lex(pad.clone(), just('|'))
                        .ignore_then(choice((
                            match_struct_pattern.clone(),
                            match_arm_start.clone(),
                        )))
                        .repeated()
                        .collect::<Vec<_>>(),
                )
                .map(|(first, rest)| {
                    let mut all = vec![first];
                    all.extend(rest);
                    all
                })
                .boxed();

            let fn_match_expr = lex(pad.clone(), just("fn"))
                .ignore_then(generic_params.clone())
                .then_ignore(lex(pad.clone(), just("match")))
                .then(type_name.clone().or_not())
                .then(arrow.clone().ignore_then(type_name.clone()).or_not())
                .then_ignore(lex(pad.clone(), just('{')))
                .then_ignore(delim.clone().repeated().collect::<Vec<_>>())
                .then(
                    match_arm
                        .clone()
                        .separated_by(
                            lex(pad.clone(), just(','))
                                .then_ignore(delim.clone().repeated().collect::<Vec<_>>()),
                        )
                        .allow_trailing()
                        .collect::<Vec<_>>()
                        .or_not()
                        .map(|x| x.unwrap_or_default()),
                )
                .then_ignore(delim.clone().or_not())
                .then_ignore(lex(pad.clone(), just('}')))
                .map_with_span({
                    let ls = line_starts.clone();
                    move |(((generics, param_ty), return_ty), body), r| {
                        let sp = span(ls.as_ref(), r);
                        let header = FunctionHeader {
                            generics,
                            parameters: vec![(
                                PotentialDollarIdentifier::Identifier(ParserText::new(
                                    sp,
                                    "__match_value".to_string(),
                                )),
                                param_ty.unwrap_or_else(|| auto_type(sp)),
                            )],
                            return_type: return_ty.unwrap_or_else(|| auto_type(sp)),
                            param_destructures: Vec::new(),
                        };

                        Node::new(
                            sp,
                            NodeType::FnMatchDeclaration {
                                header,
                                body: body.into_iter().flatten().collect(),
                            },
                        )
                    }
                })
                .boxed();

            let match_value = expr
                .clone()
                .then(
                    lex(pad.clone(), just(','))
                        .ignore_then(expr.clone())
                        .repeated()
                        .collect::<Vec<_>>(),
                )
                .map(|(first, rest)| {
                    if rest.is_empty() {
                        first
                    } else {
                        let span =
                            Span::new(first.span.from, rest.last().unwrap_or(&first).span.to);
                        let mut values = Vec::with_capacity(rest.len() + 1);
                        values.push(first);
                        values.extend(rest);
                        Node::new(span, NodeType::TupleLiteral { values })
                    }
                })
                .boxed();

            let match_expr = lex(pad.clone(), just("match"))
                .ignore_then(
                    match_value
                        .clone()
                        .then_ignore(lex(pad.clone(), just('{')))
                        .map(Some)
                        .or(lex(pad.clone(), just('{')).to(None)),
                )
                .then_ignore(delim.clone().repeated().collect::<Vec<_>>())
                .then(
                    match_arm
                        .clone()
                        .separated_by(
                            lex(pad.clone(), just(','))
                                .then_ignore(delim.clone().repeated().collect::<Vec<_>>()),
                        )
                        .allow_trailing()
                        .collect::<Vec<_>>()
                        .or_not()
                        .map(|x| x.unwrap_or_default()),
                )
                .then_ignore(delim.clone().or_not())
                .then_ignore(lex(pad.clone(), just('}')))
                .map_with_span({
                    let ls = line_starts.clone();
                    move |(value, arms), r| {
                        Node::new(
                            span(ls.as_ref(), r),
                            NodeType::MatchStatement {
                                value: value.map(Box::new),
                                body: arms.into_iter().flatten().collect(),
                            },
                        )
                    }
                })
                .boxed();

            let fat_scope_expr = fat_arrow
                .clone()
                .ignore_then(choice((
                    labelled_scope.clone(),
                    scope.clone(),
                    pad_with_newline
                        .clone()
                        .ignore_then(choice((statement.clone(), expr.clone())))
                        .map(|body| ensure_scope_node(body, true, false)),
                )))
                .boxed();

            let atom = choice((
                fat_scope_expr,
                match_expr,
                lex(pad.clone(), just("list"))
                    .ignore_then(lex(pad.clone(), just(":<")))
                    .ignore_then(type_name.clone())
                    .then_ignore(lex(pad.clone(), just('>')))
                    .or_not()
                    .map(|x| {
                        x.unwrap_or_else(|| {
                            PotentialNewType::DataType(ParserDataType::new(
                                Span::default(),
                                ParserInnerType::Auto(None),
                            ))
                        })
                    })
                    .then(lex(pad.clone(), just('[')))
                    .then(
                        expr.clone()
                            .separated_by(lex(pad.clone(), just(',')))
                            .allow_trailing()
                            .collect::<Vec<_>>()
                            .or_not()
                            .map(|x| x.unwrap_or_default()),
                    )
                    .then_ignore(lex(pad.clone(), just(']')))
                    .then(
                        choice((
                            lex(pad.clone(), just('.'))
                                .ignore_then(ident.clone())
                                .then(
                                    lex(pad.clone(), just('('))
                                        .ignore_then(
                                            expr.clone()
                                                .separated_by(lex(pad.clone(), just(',')))
                                                .allow_trailing()
                                                .collect::<Vec<_>>()
                                                .or_not()
                                                .map(|x| x.unwrap_or_default()),
                                        )
                                        .then_ignore(lex(pad.clone(), just(')')))
                                        .or_not(),
                                )
                                .map(|((name, sp), args)| {
                                    let member = ident_node(sp, &name);
                                    if let Some(args) = args {
                                        (
                                            Node::new(
                                                member.span,
                                                NodeType::CallExpression {
                                                    string_fn: None,
                                                    caller: Box::new(member),
                                                    generic_types: Vec::new(),
                                                    args: args
                                                        .into_iter()
                                                        .map(CallArg::Value)
                                                        .collect(),
                                                    reverse_args: Vec::new(),
                                                },
                                            ),
                                            false,
                                        )
                                    } else {
                                        (member, false)
                                    }
                                }),
                            lex(pad.clone(), just('['))
                                .ignore_then(expr.clone())
                                .then_ignore(lex(pad.clone(), just(']')))
                                .map(|idx| (idx, true)),
                        ))
                        .repeated()
                        .collect::<Vec<_>>(),
                    )
                    .map(|(((_open_ty, _open_br), values), tails)| {
                        let list =
                            Node::new(Span::default(), NodeType::ListLiteral(_open_ty, values));
                        if tails.is_empty() {
                            list
                        } else {
                            let mut path = vec![(list, false)];
                            for tail in tails {
                                path.push(tail);
                            }
                            let sp = match (path.first(), path.last()) {
                                (Some(first), Some(last)) => {
                                    Span::new_from_spans(first.0.span, last.0.span)
                                }
                                _ => Span::default(),
                            };
                            Node::new(sp, NodeType::MemberExpression { path })
                        }
                    }),
                float_lit.clone(),
                float_suffix_lit.clone(),
                int_lit.clone(),
                string_lit.clone(),
                char_lit.clone(),
                null_lit.clone(),
                dollar_ident.clone().map(|id| {
                    let sp = *id.span();
                    Node::new(
                        sp,
                        NodeType::Identifier(PotentialGenericTypeIdentifier::Identifier(id)),
                    )
                }),
                lex(pad.clone(), just("$("))
                    .ignore_then(
                        expr.clone()
                            .separated_by(lex(pad.clone(), just(',')))
                            .allow_trailing()
                            .collect::<Vec<_>>()
                            .or_not()
                            .map(|x| x.unwrap_or_default()),
                    )
                    .then_ignore(lex(pad.clone(), just(')')))
                    .map_with_span({
                        let ls = line_starts.clone();
                        move |args, r| {
                            let sp = span(ls.as_ref(), r);
                            Node::new(
                                sp,
                                NodeType::CallExpression {
                                    string_fn: None,
                                    caller: Box::new(Node::new(
                                        sp,
                                        NodeType::Identifier(
                                            PotentialGenericTypeIdentifier::Identifier(
                                                PotentialDollarIdentifier::Identifier(
                                                    ParserText::new(sp, "$".to_string()),
                                                ),
                                            ),
                                        ),
                                    )),
                                    generic_types: Vec::new(),
                                    args: args.into_iter().map(CallArg::Value).collect(),
                                    reverse_args: Vec::new(),
                                },
                            )
                        }
                    }),
                ident
                    .clone()
                    .map(|(n, sp)| ident_node(sp, &n))
                    .then(
                        lex(pad.clone(), just("::"))
                            .ignore_then(ident.clone().map(|(n, sp)| ident_node(sp, &n)))
                            .repeated()
                            .at_least(1)
                            .collect::<Vec<_>>(),
                    )
                    .map(|(first, rest)| {
                        let mut path = vec![first];
                        path.extend(rest);
                        let sp = match (path.first(), path.last()) {
                            (Some(first), Some(last)) => {
                                Span::new_from_spans(first.span, last.span)
                            }
                            _ => Span::default(),
                        };
                        Node::new(sp, NodeType::ScopeMemberExpression { path })
                    }),
                struct_lit,
                generic_ident.map(|identifier| {
                    Node::new(*identifier.span(), NodeType::Identifier(identifier))
                }),
                lex(pad.clone(), just('('))
                    .ignore_then(
                        expr.clone()
                            .separated_by(lex(pad.clone(), just(',')))
                            .allow_trailing()
                            .collect::<Vec<_>>()
                            .or_not()
                            .map(|x| x.unwrap_or_default()),
                    )
                    .then_ignore(lex(pad.clone(), just(')')))
                    .map_with_span({
                        let ls = line_starts.clone();
                        move |values, r| {
                            let sp = span(ls.as_ref(), r);
                            let inner = if values.len() == 1 {
                                values
                                    .first()
                                    .cloned()
                                    .unwrap_or_else(|| Node::new(sp, NodeType::EmptyLine))
                            } else {
                                Node::new(sp, NodeType::TupleLiteral { values })
                            };
                            Node::new(
                                sp,
                                NodeType::ParenExpression {
                                    value: Box::new(inner),
                                },
                            )
                        }
                    }),
            ))
            .boxed();

            let template_call_args = lex(
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
            .map_with_span({
                let ls = line_starts.clone();
                move |parts: Vec<String>, r| {
                    let sp = span(ls.as_ref(), r);
                    let raw = unescape_string(&parts.concat());
                    let (texts, args) = parse_splits(&raw);

                    let text_nodes = texts
                        .into_iter()
                        .map(|txt| Node::new(sp, NodeType::StringLiteral(ParserText::new(sp, txt))))
                        .collect::<Vec<_>>();

                    let mut call_args = vec![CallArg::Value(Node::new(
                        sp,
                        NodeType::ListLiteral(
                            PotentialNewType::DataType(ParserDataType::new(
                                sp,
                                ParserInnerType::Str,
                            )),
                            text_nodes,
                        ),
                    ))];

                    for arg in args {
                        call_args.push(CallArg::Value(parse_embedded_expr(&arg)));
                    }

                    (ParserText::new(sp, raw), call_args)
                }
            })
            .boxed();

            let call_args = lex(pad.clone(), just('('))
                .ignore_then(
                    expr.clone()
                        .then(
                            lex(pad.clone(), just('['))
                                .ignore_then(expr.clone())
                                .then_ignore(lex(pad.clone(), just(']')))
                                .repeated()
                                .collect::<Vec<_>>(),
                        )
                        .map(|(head, indexes)| {
                            if indexes.is_empty() {
                                head
                            } else {
                                let mut path = vec![(head, false)];
                                for idx in indexes {
                                    path.push((idx, true));
                                }
                                let sp = match (path.first(), path.last()) {
                                    (Some(first), Some(last)) => {
                                        Span::new_from_spans(first.0.span, last.0.span)
                                    }
                                    _ => Span::default(),
                                };
                                Node::new(sp, NodeType::MemberExpression { path })
                            }
                        })
                        .separated_by(lex(pad.clone(), just(',')))
                        .allow_trailing()
                        .collect::<Vec<_>>()
                        .or_not()
                        .map(|x| x.unwrap_or_default()),
                )
                .then_ignore(lex(pad.clone(), just(')')))
                .map(|args| args.into_iter().map(CallArg::Value).collect::<Vec<_>>())
                .boxed();

            let reverse_args = lex(pad.clone(), just("<("))
                .ignore_then(
                    expr.clone()
                        .then(
                            lex(pad.clone(), just('['))
                                .ignore_then(expr.clone())
                                .then_ignore(lex(pad.clone(), just(']')))
                                .repeated()
                                .collect::<Vec<_>>(),
                        )
                        .map(|(head, indexes)| {
                            if indexes.is_empty() {
                                head
                            } else {
                                let mut path = vec![(head, false)];
                                for idx in indexes {
                                    path.push((idx, true));
                                }
                                let sp = match (path.first(), path.last()) {
                                    (Some(first), Some(last)) => {
                                        Span::new_from_spans(first.0.span, last.0.span)
                                    }
                                    _ => Span::default(),
                                };
                                Node::new(sp, NodeType::MemberExpression { path })
                            }
                        })
                        .separated_by(lex(pad.clone(), just(',')))
                        .allow_trailing()
                        .collect::<Vec<_>>()
                        .or_not()
                        .map(|x| x.unwrap_or_default()),
                )
                .then_ignore(lex(pad.clone(), just(')')))
                .boxed();

            let member = choice((
                lex(pad.clone(), just('.'))
                    .ignore_then(choice((
                        ident.clone().map(|(n, sp)| ident_node(sp, &n)),
                        int_lit.clone(),
                    )))
                    .then(call_args.clone().repeated().collect::<Vec<_>>())
                    .map(|(m, calls)| {
                        let node = calls.into_iter().fold(m, |c, args| {
                            Node::new(
                                c.span,
                                NodeType::CallExpression {
                                    string_fn: None,
                                    caller: Box::new(c),
                                    generic_types: Vec::new(),
                                    args,
                                    reverse_args: Vec::new(),
                                },
                            )
                        });
                        (node, false)
                    }),
                lex(pad.clone(), just('['))
                    .ignore_then(expr.clone())
                    .then_ignore(lex(pad.clone(), just(']')))
                    .map(|e| (e, true)),
            ))
            .boxed();

            let call_suffix = choice((
                call_args
                    .clone()
                    .then(reverse_args.clone().or_not())
                    .map(|(args, rev)| (None::<ParserText>, args, rev.unwrap_or_default())),
                template_call_args
                    .clone()
                    .then(reverse_args.clone().or_not())
                    .map(|((string_fn, args), rev)| {
                        (Some(string_fn), args, rev.unwrap_or_default())
                    }),
            ))
            .boxed();

            let apply_calls =
                |head: Node, calls: Vec<(Option<ParserText>, Vec<CallArg>, Vec<Node>)>| {
                    calls
                        .into_iter()
                        .fold(head, |c, (string_fn, args, reverse_args)| {
                            Node::new(
                                c.span,
                                NodeType::CallExpression {
                                    string_fn,
                                    caller: Box::new(c),
                                    generic_types: Vec::new(),
                                    args,
                                    reverse_args,
                                },
                            )
                        })
                };

            let postfix = atom
                .then(call_suffix.clone().repeated().collect::<Vec<_>>())
                .map(move |(head, calls)| apply_calls(head, calls))
                .then(member.clone().repeated().collect::<Vec<_>>())
                .map(|(head, rest)| {
                    if rest.is_empty() {
                        head
                    } else {
                        let (head, rest) = normalize_scope_member_chain(head, rest);
                        if rest.is_empty() {
                            return head;
                        }
                        let mut path = vec![(head, false)];
                        path.extend(rest);
                        let sp = match (path.first(), path.last()) {
                            (Some(first), Some(last)) => {
                                Span::new_from_spans(first.0.span, last.0.span)
                            }
                            _ => Span::default(),
                        };
                        Node::new(sp, NodeType::MemberExpression { path })
                    }
                })
                .boxed();

            let fn_standard_postfix = fn_standard_expr
                .clone()
                .then(call_suffix.clone().repeated().collect::<Vec<_>>())
                .map(move |(head, calls)| apply_calls(head, calls))
                .boxed();

            let fn_match_postfix = fn_match_expr
                .clone()
                .then(call_suffix.clone().repeated().collect::<Vec<_>>())
                .map(move |(head, calls)| apply_calls(head, calls))
                .boxed();

            let enum_expr_value = expr.clone();
            let enum_with_payload = named_ident
                .clone()
                .then_ignore(lex(pad.clone(), just('.')))
                .then(named_ident.clone())
                .then_ignore(lex(pad.clone(), just(':')))
                .then(enum_expr_value.clone())
                .map(|((identifier, value), data)| {
                    let ident = identifier;
                    Node::new(
                        Span::new_from_spans(*ident.span(), data.span),
                        NodeType::EnumExpression {
                            identifier: ident.into(),
                            value,
                            data: Some(Box::new(data)),
                        },
                    )
                })
                .boxed();

            let enum_variant = choice((enum_with_payload, postfix.clone())).boxed();

            let list_prefix_type = lex(pad.clone(), just("list"))
                .ignore_then(lex(pad.clone(), just(":<")))
                .ignore_then(type_name.clone())
                .then_ignore(lex(pad.clone(), just('>')))
                .or_not()
                .map(|x| {
                    x.unwrap_or_else(|| {
                        PotentialNewType::DataType(ParserDataType::new(
                            Span::default(),
                            ParserInnerType::Auto(None),
                        ))
                    })
                })
                .boxed();

            let list_lit = list_prefix_type
                .clone()
                .then(lex(pad.clone(), just('[')))
                .then(
                    expr.clone()
                        .separated_by(lex(pad.clone(), just(',')))
                        .allow_trailing()
                        .collect::<Vec<_>>()
                        .or_not()
                        .map(|x| x.unwrap_or_default()),
                )
                .then_ignore(lex(pad.clone(), just(']')))
                .then(member.clone().repeated().collect::<Vec<_>>())
                .map(|(((_open_ty, _open_br), values), tails)| {
                    let data_type = _open_ty;
                    let list = Node::new(Span::default(), NodeType::ListLiteral(data_type, values));
                    if tails.is_empty() {
                        list
                    } else {
                        let mut path = vec![(list, false)];
                        path.extend(tails);
                        let sp = match (path.first(), path.last()) {
                            (Some(first), Some(last)) => {
                                Span::new_from_spans(first.0.span, last.0.span)
                            }
                            _ => Span::default(),
                        };
                        Node::new(sp, NodeType::MemberExpression { path })
                    }
                })
                .boxed();

            let iter_loop_type = ident
                .clone()
                .then_ignore(lex(pad.clone(), just("in")))
                .then(expr.clone())
                .map(|((n, sp), iter)| {
                    LoopType::For(
                        PotentialDollarIdentifier::Identifier(ParserText::new(sp, n)),
                        iter,
                    )
                })
                .or(expr.clone().map(LoopType::While))
                .or_not()
                .map(|x| x.unwrap_or(LoopType::Loop))
                .boxed();

            let iter_map_expr = choice((
                fat_arrow.clone().ignore_then(
                    lex(pad.clone(), just('{'))
                        .rewind()
                        .to(true)
                        .or_not()
                        .map(|x| x.unwrap_or(false))
                        .then(choice((scope.clone(), statement.clone(), expr.clone())))
                        .try_map(|(has_brace, body), sp| {
                            if has_brace
                                && !matches!(body.node_type, NodeType::ScopeDeclaration { .. })
                            {
                                Err(Rich::custom(sp, "expected scope body"))
                            } else {
                                Ok(body)
                            }
                        }),
                ),
                expr.clone(),
            ))
            .boxed();

            let iter_expr = list_prefix_type
                .clone()
                .then(lex(pad.clone(), just('[')))
                .then(iter_map_expr)
                .then(
                    lex(pad.clone(), just("spawn"))
                        .to(true)
                        .or_not()
                        .map(|x| x.unwrap_or(false)),
                )
                .then_ignore(lex(pad.clone(), text::keyword("for")))
                .then(iter_loop_type.clone())
                .then(
                    lex(pad.clone(), just("if"))
                        .ignore_then(expr.clone())
                        .repeated()
                        .collect::<Vec<_>>(),
                )
                .then(
                    lex(pad.clone(), just("until"))
                        .ignore_then(expr.clone())
                        .or_not(),
                )
                .then_ignore(lex(pad.clone(), just(']')))
                .map(
                    |(
                        (((((open_ty, _open_br), map), spawned), loop_type), conditionals),
                        until,
                    )| {
                        Node::new(
                            map.span,
                            NodeType::IterExpression {
                                data_type: open_ty,
                                map: Box::new(map),
                                spawned,
                                loop_type: Box::new(loop_type),
                                conditionals,
                                until: until.map(Box::new),
                            },
                        )
                    },
                )
                .boxed();

            #[derive(Clone)]
            enum PrefixOp {
                Ref(RefMutability),
                Deref,
            }

            let prefixed = choice((
                lex(pad.clone(), just("&mut")).to(PrefixOp::Ref(RefMutability::MutRef)),
                lex(pad.clone(), just('&')).to(PrefixOp::Ref(RefMutability::Ref)),
                lex(pad.clone(), just('*')).to(PrefixOp::Deref),
            ))
            .repeated()
            .collect::<Vec<_>>()
            .then(enum_variant)
            .map(|(prefixes, node)| {
                prefixes.into_iter().rev().fold(node, |value, op| match op {
                    PrefixOp::Ref(mutability) => Node::new(
                        value.span,
                        NodeType::RefStatement {
                            mutability,
                            value: Box::new(value),
                        },
                    ),
                    PrefixOp::Deref => Node::new(
                        value.span,
                        NodeType::DerefStatement {
                            value: Box::new(value),
                        },
                    ),
                })
            })
            .boxed();

            let unary = choice((
                lex(pad.clone(), just('-')).to(BinaryOperator::Sub),
                lex(pad.clone(), just('!')).to(BinaryOperator::BitXor),
            ))
            .repeated()
            .collect::<Vec<_>>()
            .then(
                prefixed
                    .clone()
                    .then(
                        lex(pad.clone(), just("**"))
                            .ignore_then(prefixed.clone())
                            .repeated()
                            .collect::<Vec<_>>(),
                    )
                    .map(|(head, rest)| {
                        let mut operands = Vec::with_capacity(rest.len() + 1);
                        operands.push(head);
                        operands.extend(rest);
                        let mut iter = operands.into_iter().rev();
                        let mut right = iter
                            .next()
                            .expect("power-expression operand list is never empty");
                        for left in iter {
                            right = Node::new(
                                Span::new_from_spans(left.span, right.span),
                                NodeType::BinaryExpression {
                                    left: Box::new(left),
                                    right: Box::new(right),
                                    operator: BinaryOperator::Pow,
                                },
                            );
                        }
                        right
                    }),
            )
            .map(|(ops, node)| {
                ops.into_iter().fold(node, |n, op| {
                    if op == BinaryOperator::Sub {
                        Node::new(n.span, NodeType::NegExpression { value: Box::new(n) })
                    } else {
                        Node::new(n.span, NodeType::NotExpression { value: Box::new(n) })
                    }
                })
            })
            .boxed();

            let mul_op = choice((
                lex(pad.clone(), just('*')).to(BinaryOperator::Mul),
                lex(pad.clone(), just('/')).to(BinaryOperator::Div),
                lex(pad.clone(), just('%')).to(BinaryOperator::Mod),
            ))
            .boxed();
            let add_op = choice((
                lex(pad.clone(), just('+')).to(BinaryOperator::Add),
                lex(pad.clone(), just('-')).to(BinaryOperator::Sub),
            ))
            .boxed();

            let product = unary
                .clone()
                .then(mul_op.then(unary).repeated().collect::<Vec<_>>())
                .map(|(h, t)| {
                    t.into_iter().fold(h, |l, (op, r)| {
                        Node::new(
                            Span::new_from_spans(l.span, r.span),
                            NodeType::BinaryExpression {
                                left: Box::new(l),
                                right: Box::new(r),
                                operator: op,
                            },
                        )
                    })
                })
                .boxed();

            let sum = product
                .clone()
                .then(add_op.then(product).repeated().collect::<Vec<_>>())
                .map(|(h, t)| {
                    t.into_iter().fold(h, |l, (op, r)| {
                        Node::new(
                            Span::new_from_spans(l.span, r.span),
                            NodeType::BinaryExpression {
                                left: Box::new(l),
                                right: Box::new(r),
                                operator: op,
                            },
                        )
                    })
                })
                .boxed();

            let bit_op = choice((
                lex(pad.clone(), just("<<")).to(BinaryOperator::Shl),
                lex(pad.clone(), just(">>")).to(BinaryOperator::Shr),
                lex(pad.clone(), just('&').then(just('&').not())).to(BinaryOperator::BitAnd),
                lex(pad.clone(), just('|').then(just('|').not())).to(BinaryOperator::BitOr),
                lex(pad.clone(), just('^')).to(BinaryOperator::BitXor),
            ))
            .boxed();

            let bitwise = sum
                .clone()
                .then(bit_op.then(sum.clone()).repeated().collect::<Vec<_>>())
                .map(|(h, t)| {
                    t.into_iter().fold(h, |l, (op, r)| {
                        Node::new(
                            Span::new_from_spans(l.span, r.span),
                            NodeType::BinaryExpression {
                                left: Box::new(l),
                                right: Box::new(r),
                                operator: op,
                            },
                        )
                    })
                })
                .boxed();

            let cmp_op = choice((
                lex(pad.clone(), just("==")).to(ComparisonOperator::Equal),
                lex(pad.clone(), just("!=")).to(ComparisonOperator::NotEqual),
                lex(pad.clone(), just(">=")).to(ComparisonOperator::GreaterEqual),
                lex(pad.clone(), just("<=")).to(ComparisonOperator::LesserEqual),
                lex(pad.clone(), just('>')).to(ComparisonOperator::Greater),
                lex(pad.clone(), just('<')).to(ComparisonOperator::Lesser),
            ))
            .boxed();

            let compared = bitwise
                .clone()
                .then(cmp_op.then(bitwise.clone()).repeated().collect::<Vec<_>>())
                .map(|(h, t)| {
                    t.into_iter().fold(h, |l, (op, r)| {
                        Node::new(
                            Span::new_from_spans(l.span, r.span),
                            NodeType::ComparisonExpression {
                                left: Box::new(l),
                                right: Box::new(r),
                                operator: op,
                            },
                        )
                    })
                })
                .boxed();

            let bool_op = choice((
                lex(pad.clone(), just("&&")).to(BooleanOperator::And),
                lex(pad.clone(), just("||")).to(BooleanOperator::Or),
            ))
            .boxed();

            let boolean = compared
                .clone()
                .then(
                    bool_op
                        .then(compared.clone())
                        .repeated()
                        .collect::<Vec<_>>(),
                )
                .map(|(h, t)| {
                    t.into_iter().fold(h, |l, (op, r)| {
                        Node::new(
                            Span::new_from_spans(l.span, r.span),
                            NodeType::BooleanExpression {
                                left: Box::new(l),
                                right: Box::new(r),
                                operator: op,
                            },
                        )
                    })
                })
                .boxed();

            let ranged = boolean
                .clone()
                .then(
                    lex(pad.clone(), just(".."))
                        .ignore_then(lex(pad.clone(), just('=').or_not()))
                        .then(boolean.clone())
                        .or_not(),
                )
                .map(|(from, tail)| {
                    if let Some((inc, to)) = tail {
                        Node::new(
                            Span::new_from_spans(from.span, to.span),
                            NodeType::RangeDeclaration {
                                from: Box::new(from),
                                to: Box::new(to),
                                inclusive: inc.is_some(),
                            },
                        )
                    } else {
                        from
                    }
                })
                .boxed();

            let as_expr = ranged
                .clone()
                .then(
                    lex(pad.clone(), just("as"))
                        .ignored()
                        .then(choice((just('!'), just('?'))).or_not())
                        .then(type_name.clone())
                        .repeated()
                        .collect::<Vec<_>>(),
                )
                .map(|(head, casts)| {
                    casts
                        .into_iter()
                        .fold(head, |value, ((_, suffix), data_type)| {
                            Node::new(
                                value.span,
                                NodeType::AsExpression {
                                    value: Box::new(value),
                                    data_type,
                                    failure_mode: match suffix {
                                        Some('!') => AsFailureMode::Panic,
                                        Some('?') => AsFailureMode::Option,
                                        _ => AsFailureMode::Result,
                                    },
                                },
                            )
                        })
                })
                .boxed();

            let in_expr = as_expr
                .clone()
                .then(
                    lex(pad.clone(), just("in"))
                        .ignore_then(as_expr.clone())
                        .repeated()
                        .collect::<Vec<_>>(),
                )
                .map(|(h, t)| {
                    t.into_iter().fold(h, |l, r| {
                        Node::new(
                            Span::new_from_spans(l.span, r.span),
                            NodeType::InDeclaration {
                                identifier: Box::new(l),
                                value: Box::new(r),
                            },
                        )
                    })
                })
                .boxed();

            let assign_expr = postfix
                .clone()
                .then_ignore(lex(pad.clone(), just('=')))
                .then(expr.clone())
                .map(|(identifier, value)| {
                    Node::new(
                        Span::new_from_spans(identifier.span, value.span),
                        NodeType::AssignmentExpression {
                            identifier: Box::new(identifier),
                            value: Box::new(value),
                        },
                    )
                })
                .boxed();

            let ternary_expr = recursive(|ternary_expr| {
                in_expr
                    .clone()
                    .then(
                        lex(pad.clone(), just('?'))
                            .ignore_then(choice((
                                assign_expr.clone(),
                                ternary_expr.clone(),
                                in_expr.clone(),
                            )))
                            .then_ignore(lex(pad.clone(), just(':')))
                            .then(choice((
                                assign_expr.clone(),
                                ternary_expr.clone(),
                                in_expr.clone(),
                            )))
                            .or_not(),
                    )
                    .map(|(comparison, then_otherwise)| {
                        if let Some((then, otherwise)) = then_otherwise {
                            Node::new(
                                Span::new_from_spans(comparison.span, otherwise.span),
                                NodeType::Ternary {
                                    comparison: Box::new(comparison),
                                    then: Box::new(then),
                                    otherwise: Box::new(otherwise),
                                },
                            )
                        } else {
                            comparison
                        }
                    })
                    .boxed()
            })
            .boxed();

            let pipe_seg = choice((
                lex(pad_with_newline.clone(), just("|>"))
                    .ignore_then(choice((
                        fn_standard_postfix.clone(),
                        fn_match_postfix.clone(),
                        ternary_expr.clone(),
                    )))
                    .map(PipeSegment::Unnamed),
                lex(pad_with_newline.clone(), just("|:"))
                    .ignore_then(named_ident.clone())
                    .then_ignore(lex(pad_with_newline.clone(), just('>')))
                    .then(choice((
                        fn_standard_postfix.clone(),
                        fn_match_postfix.clone(),
                        ternary_expr.clone(),
                    )))
                    .map(|(identifier, node)| PipeSegment::Named { identifier, node }),
            ))
            .boxed();

            let pipe_expr = ternary_expr
                .clone()
                .then(pipe_seg.repeated().collect::<Vec<_>>())
                .map(|(head, rest)| {
                    if rest.is_empty() {
                        return head;
                    }

                    let mut items = vec![PipeSegment::Unnamed(head)];
                    for seg in rest {
                        match seg {
                            PipeSegment::Unnamed(node) => {
                                if let NodeType::PipeExpression(mut nested) = node.node_type {
                                    items.append(&mut nested);
                                } else {
                                    items.push(PipeSegment::Unnamed(node));
                                }
                            }
                            PipeSegment::Named { identifier, node } => {
                                if let NodeType::PipeExpression(mut nested) = node.node_type {
                                    if let Some(first) = nested.first_mut() {
                                        *first = PipeSegment::Named {
                                            identifier,
                                            node: first.get_node().clone(),
                                        };
                                    }
                                    items.append(&mut nested);
                                } else {
                                    items.push(PipeSegment::Named { identifier, node });
                                }
                            }
                        }
                    }

                    Node::new(Span::default(), NodeType::PipeExpression(items))
                })
                .boxed();

            let fn_inline_gen_expr = lex(pad.clone(), just("fn"))
                .ignore_then(lex(pad.clone(), just('(')))
                .ignore_then(
                    expr.clone()
                        .then_ignore(lex(pad.clone(), text::keyword("for")))
                        .then(iter_loop_type.clone())
                        .then(
                            lex(pad.clone(), just("if"))
                                .ignore_then(expr.clone())
                                .repeated()
                                .collect::<Vec<_>>(),
                        )
                        .then(
                            lex(pad.clone(), just("until"))
                                .ignore_then(expr.clone())
                                .or_not(),
                        ),
                )
                .then_ignore(lex(pad.clone(), just(')')))
                .then(arrow.clone().ignore_then(type_name.clone()).or_not())
                .try_map(
                    |((((map_expr, loop_type), conditionals), until), explicit_return), sp| {
                        let data_type = match explicit_return {
                            Some(PotentialNewType::DataType(ParserDataType {
                                data_type:
                                    ParserInnerType::StructWithGenerics {
                                        identifier,
                                        generic_types,
                                    },
                                ..
                            })) if identifier == "gen" && generic_types.len() == 1 => {
                                Some(PotentialNewType::DataType(generic_types[0].clone()))
                            }
                            Some(_) => {
                                return Err(Rich::custom(
                                    sp,
                                    "inline generator return type must be gen:<T>",
                                ));
                            }
                            None => None,
                        };
                        Ok((map_expr, loop_type, conditionals, until, data_type))
                    },
                )
                .map_with_span({
                    let ls = line_starts.clone();
                    move |(map_expr, loop_type, conditionals, until, data_type), r| {
                        Node::new(
                            span(ls.as_ref(), r),
                            NodeType::InlineGenerator {
                                map: Box::new(map_expr),
                                data_type,
                                loop_type: Box::new(loop_type),
                                conditionals,
                                until: until.map(Box::new),
                            },
                        )
                    }
                })
                .boxed();

            let fn_inline_postfix = fn_inline_gen_expr
                .clone()
                .then(member.clone().repeated().collect::<Vec<_>>())
                .map(|(head, rest)| {
                    if rest.is_empty() {
                        head
                    } else {
                        let mut path = vec![(head, false)];
                        path.extend(rest);
                        let sp = match (path.first(), path.last()) {
                            (Some(first), Some(last)) => {
                                Span::new_from_spans(first.0.span, last.0.span)
                            }
                            _ => Span::default(),
                        };
                        Node::new(sp, NodeType::MemberExpression { path })
                    }
                })
                .boxed();

            let try_expr = lex(pad.clone(), just("try"))
                .ignore_then(expr.clone())
                .then(
                    choice((
                        lex(pad.clone(), just(':'))
                            .ignore_then(ident.clone())
                            .then_ignore(fat_arrow.clone())
                            .then(choice((scope.clone(), statement.clone(), expr.clone())))
                            .map(|((name, sp), body)| {
                                let body = ensure_scope_node(body, true, false);
                                TryCatch {
                                    name: Some(PotentialDollarIdentifier::Identifier(
                                        ParserText::new(sp, name),
                                    )),
                                    body: Box::new(body),
                                }
                            }),
                        fat_arrow
                            .clone()
                            .ignore_then(pad_with_newline.clone().ignore_then(choice((
                                scope.clone(),
                                statement.clone(),
                                expr.clone(),
                            ))))
                            .map(|body| {
                                let body = ensure_scope_node(body, true, false);
                                TryCatch {
                                    name: None,
                                    body: Box::new(body),
                                }
                            }),
                    ))
                    .or_not(),
                )
                .map(|(value, catch)| {
                    Node::new(
                        value.span,
                        NodeType::Try {
                            value: Box::new(value),
                            catch,
                        },
                    )
                })
                .boxed();

            let defer_expr = lex(pad.clone(), just("defer"))
                .ignore_then(
                    lex(pad.clone(), just("return"))
                        .to(true)
                        .or_not()
                        .map(|x| x.unwrap_or(false)),
                )
                .then(expr.clone())
                .map(|(function, value)| {
                    Node::new(
                        value.span,
                        NodeType::Defer {
                            value: Box::new(value),
                            function,
                        },
                    )
                })
                .boxed();
            let move_expr = lex(pad.clone(), just("move"))
                .ignore_then(expr.clone())
                .map(|value| {
                    Node::new(
                        value.span,
                        NodeType::MoveExpression {
                            value: Box::new(value),
                        },
                    )
                })
                .boxed();
            let break_expr = lex(pad.clone(), just("break"))
                .ignore_then(
                    lex(pad.clone(), just('@'))
                        .ignore_then(ident.clone())
                        .map(|(n, sp)| {
                            PotentialDollarIdentifier::Identifier(ParserText::new(sp, n))
                        })
                        .or_not(),
                )
                .then(expr.clone().or_not())
                .map(|(label, value)| {
                    Node::new(
                        Span::default(),
                        NodeType::Break {
                            label,
                            value: value.map(Box::new),
                        },
                    )
                })
                .boxed();
            let continue_expr = lex(pad.clone(), just("continue"))
                .ignore_then(
                    lex(pad.clone(), just('@'))
                        .ignore_then(ident.clone())
                        .map(|(n, sp)| {
                            PotentialDollarIdentifier::Identifier(ParserText::new(sp, n))
                        })
                        .or_not(),
                )
                .map(|label| Node::new(Span::default(), NodeType::Continue { label }))
                .boxed();

            let spawn_expr = lex(pad.clone(), just("spawn"))
                .ignore_then(choice((
                    lex(pad.clone(), just('{'))
                        .ignore_then(delim.clone().repeated().collect::<Vec<_>>())
                        .ignore_then(
                            fat_arrow
                                .clone()
                                .ignore_then(
                                    scope.clone().or(choice((statement.clone(), expr.clone()))
                                        .map(|n| ensure_scope_node(n, true, false))),
                                )
                                .or(expr.clone())
                                .separated_by(
                                    lex(pad.clone(), just(','))
                                        .then_ignore(delim.clone().repeated().collect::<Vec<_>>())
                                        .ignored(),
                                )
                                .allow_trailing()
                                .collect::<Vec<_>>()
                                .or_not()
                                .map(|x| x.unwrap_or_default()),
                        )
                        .then_ignore(delim.clone().or_not())
                        .then_ignore(lex(pad.clone(), just('}')))
                        .map(|items| Node::new(Span::default(), NodeType::Spawn { items })),
                    lex(pad.clone(), text::keyword("for"))
                        .ignore_then(
                            ident
                                .clone()
                                .then_ignore(lex(pad.clone(), just("in")))
                                .then(expr.clone())
                                .map(|((n, sp), iter)| {
                                    LoopType::For(
                                        PotentialDollarIdentifier::Identifier(ParserText::new(
                                            sp, n,
                                        )),
                                        iter,
                                    )
                                })
                                .or(expr.clone().map(LoopType::While)),
                        )
                        .then_ignore(fat_arrow.clone())
                        .then(delim.clone().repeated().ignore_then(
                            scope_node_parser(statement.clone(), delim.clone(), pad.clone()).or(
                                choice((labelled_scope.clone(), statement.clone(), expr.clone())),
                            ),
                        ))
                        .map(|(lt, body)| {
                            let body = ensure_scope_node(body, true, false);
                            Node::new(
                                body.span,
                                NodeType::LoopDeclaration {
                                    loop_type: Box::new(lt),
                                    body: Box::new(body),
                                    until: None,
                                    label: None,
                                    else_body: None,
                                },
                            )
                        }),
                    expr.clone(),
                )))
                .map(|item| {
                    if matches!(item.node_type, NodeType::Spawn { .. }) {
                        item
                    } else {
                        Node::new(item.span, NodeType::Spawn { items: vec![item] })
                    }
                })
                .boxed();

            let for_expr =
                lex(pad.clone(), text::keyword("for"))
                    .ignore_then(
                        fat_arrow.clone().rewind().to(LoopType::Loop).or(lex(
                            pad.clone(),
                            just("let"),
                        )
                        .ignore_then(let_pattern_list.clone())
                        .then_ignore(left_arrow.clone())
                        .then(expr.clone())
                        .map(|(values, value)| LoopType::Let {
                            value,
                            pattern: (values, Vec::new()),
                        })
                        .or(ident
                            .clone()
                            .then_ignore(lex(pad.clone(), just("in")))
                            .then(expr.clone())
                            .map(|((n, sp), iter)| {
                                LoopType::For(
                                    PotentialDollarIdentifier::Identifier(ParserText::new(sp, n)),
                                    iter,
                                )
                            })
                            .or(expr.clone().map(LoopType::While))
                            .or_not()
                            .map(|x| x.unwrap_or(LoopType::Loop)))),
                    )
                    .then_ignore(fat_arrow.clone())
                    .then(
                        pad_with_newline.clone().ignore_then(
                            lex(pad.clone(), just('@'))
                                .ignore_then(ident.clone())
                                .then(lex(pad.clone(), just("[]")).or_not())
                                .map(|((name, sp), _)| {
                                    PotentialDollarIdentifier::Identifier(ParserText::new(sp, name))
                                })
                                .or_not(),
                        ),
                    )
                    .then(pad_with_newline.clone().ignore_then(choice((
                        scope.clone(),
                        statement.clone(),
                        expr.clone(),
                    ))))
                    .then(
                        lex(pad.clone(), just("else"))
                            .ignore_then(fat_arrow.clone())
                            .ignore_then(pad_with_newline.clone().ignore_then(scope.clone().or(
                                choice((labelled_scope.clone(), statement.clone(), expr.clone())),
                            )))
                            .or_not(),
                    )
                    .then(
                        lex(pad.clone(), just("until"))
                            .ignore_then(expr.clone())
                            .or_not(),
                    )
                    .map(|((((loop_type, label), body), else_body), until)| {
                        let body = ensure_scope_node(body, true, false);
                        let else_body =
                            else_body.map(|body| Box::new(ensure_scope_node(body, true, false)));
                        Node::new(
                            body.span,
                            NodeType::LoopDeclaration {
                                loop_type: Box::new(loop_type),
                                body: Box::new(body),
                                until: until.map(Box::new),
                                label,
                                else_body,
                            },
                        )
                    })
                    .then_ignore(lex(pad.clone(), just(';')).or_not())
                    .boxed();

            let if_expr = recursive(|if_e| {
                let if_let_cond = lex(pad.clone(), just("let"))
                    .ignore_then(let_pattern_list.clone())
                    .then_ignore(left_arrow.clone())
                    .then(expr.clone())
                    .map(|(values, value)| IfComparisonType::IfLet {
                        value,
                        pattern: (values, Vec::new()),
                    })
                    .boxed();

                lex(pad.clone(), just("if"))
                    .ignore_then(choice((
                        if_let_cond,
                        expr.clone().map(IfComparisonType::If),
                    )))
                    .then_ignore(fat_arrow.clone())
                    .then(pad_with_newline.clone().ignore_then(choice((
                        scope_node_parser(statement.clone(), delim.clone(), pad.clone()),
                        statement.clone(),
                        expr.clone(),
                    ))))
                    .then(
                        lex(pad.clone(), just("else"))
                            .ignore_then(if_e.clone().or(choice((
                                scope_node_parser(statement.clone(), delim.clone(), pad.clone()),
                                statement.clone(),
                                expr.clone(),
                            ))))
                            .or_not(),
                    )
                    .map(|((cond, then_b), otherwise)| {
                        let then_node = match then_b.node_type {
                            NodeType::ScopeDeclaration { .. } => then_b,
                            _ => ensure_scope_node(then_b, true, false),
                        };
                        let cond_span = match &cond {
                            IfComparisonType::If(node) => node.span,
                            IfComparisonType::IfLet { value, .. } => value.span,
                        };
                        Node::new(
                            Span::new_from_spans(
                                cond_span,
                                otherwise.as_ref().map(|x| x.span).unwrap_or(then_node.span),
                            ),
                            NodeType::IfStatement {
                                comparison: Box::new(cond),
                                then: Box::new(then_node),
                                otherwise: otherwise.map(Box::new),
                            },
                        )
                    })
                    .boxed()
            })
            .boxed();

            choice((
                fn_inline_postfix,
                fn_standard_postfix,
                fn_match_postfix,
                iter_expr,
                list_lit,
                break_expr,
                continue_expr,
                for_expr,
                spawn_expr,
                defer_expr,
                move_expr,
                try_expr,
                if_expr,
                pipe_expr,
            ))
            .boxed()
        });

        let import_stmt = lex(pad.clone(), just("import"))
            .ignore_then(choice((
                choice((
                    lex(pad.clone(), just('('))
                        .ignore_then(
                            ident
                                .clone()
                                .map(|(n, sp)| {
                                    PotentialDollarIdentifier::Identifier(ParserText::new(sp, n))
                                })
                                .separated_by(lex(pad.clone(), just(',')))
                                .allow_trailing()
                                .collect::<Vec<_>>(),
                        )
                        .then_ignore(lex(pad.clone(), just(')'))),
                    ident.clone().map(|(n, sp)| {
                        vec![PotentialDollarIdentifier::Identifier(ParserText::new(
                            sp, n,
                        ))]
                    }),
                    lex(pad.clone(), just('*')).map(|_| {
                        vec![PotentialDollarIdentifier::Identifier(ParserText::from(
                            "*".to_string(),
                        ))]
                    }),
                ))
                .then_ignore(lex(pad.clone(), just("from")))
                .then(
                    ident
                        .clone()
                        .map(|(n, sp)| {
                            PotentialDollarIdentifier::Identifier(ParserText::new(sp, n))
                        })
                        .separated_by(lex(pad.clone(), just("::")))
                        .at_least(1)
                        .collect::<Vec<_>>(),
                )
                .map(|(values, module)| (values, module, None)),
                ident
                    .clone()
                    .map(|(n, sp)| PotentialDollarIdentifier::Identifier(ParserText::new(sp, n)))
                    .separated_by(lex(pad.clone(), just("::")))
                    .at_least(1)
                    .collect::<Vec<_>>()
                    .then(
                        lex(pad.clone(), just("as"))
                            .ignore_then(ident.clone())
                            .map(|(n, sp)| {
                                PotentialDollarIdentifier::Identifier(ParserText::new(sp, n))
                            })
                            .or_not(),
                    )
                    .map(|(module, alias)| (Vec::new(), module, alias)),
            )))
            .map(|(values, module, alias)| {
                Node::new(
                    Span::default(),
                    NodeType::ImportStatement {
                        module,
                        alias,
                        values,
                    },
                )
            });

        let type_stmt = lex(pad.clone(), just("type"))
            .ignore_then(ident.clone())
            .then(
                lex(pad.clone(), just(":<"))
                    .ignore_then(
                        ident
                            .clone()
                            .separated_by(lex(pad.clone(), just(',')))
                            .allow_trailing()
                            .collect::<Vec<_>>()
                            .or_not()
                            .map(|x| x.unwrap_or_default()),
                    )
                    .then_ignore(lex(pad.clone(), just('>')))
                    .or_not(),
            )
            .then_ignore(lex(pad.clone(), just('=')))
            .then(choice((
                lex(pad.clone(), just("struct")).ignore_then(
                    lex(pad.clone(), just('{'))
                        .then_ignore(delim.clone().repeated().collect::<Vec<_>>())
                        .ignore_then(
                            ident
                                .clone()
                                .repeated()
                                .at_least(1)
                                .collect::<Vec<_>>()
                                .then_ignore(lex(pad.clone(), just(':')))
                                .then(type_name.clone())
                                .separated_by(
                                    choice((lex(pad.clone(), just(',')).ignored(), delim.clone()))
                                        .repeated()
                                        .at_least(1)
                                        .ignored(),
                                )
                                .allow_trailing()
                                .collect::<Vec<_>>(),
                        )
                        .then_ignore(delim.clone().or_not())
                        .then_ignore(lex(pad.clone(), just('}')))
                        .map(|groups| {
                            let mut fields = Vec::new();
                            for (names, ty) in groups {
                                for (n, _sp) in names {
                                    fields.push((n, ty.clone()));
                                }
                            }
                            TypeDefType::Struct(ObjectType::Map(fields))
                        })
                        .or(lex(pad.clone(), just('('))
                            .ignore_then(
                                type_name
                                    .clone()
                                    .separated_by(
                                        choice((
                                            lex(pad.clone(), just(',')).ignored(),
                                            delim.clone(),
                                        ))
                                        .repeated()
                                        .at_least(1)
                                        .ignored(),
                                    )
                                    .allow_trailing()
                                    .collect::<Vec<_>>()
                                    .or_not()
                                    .map(|x| x.unwrap_or_default()),
                            )
                            .then_ignore(lex(pad.clone(), just(')')))
                            .map(|types| TypeDefType::Struct(ObjectType::Tuple(types)))),
                ),
                lex(pad.clone(), just("enum"))
                    .ignore_then(lex(pad.clone(), just('{')))
                    .then_ignore(delim.clone().repeated().collect::<Vec<_>>())
                    .ignore_then(
                        ident
                            .clone()
                            .repeated()
                            .at_least(1)
                            .collect::<Vec<_>>()
                            .then(
                                lex(pad.clone(), just(':'))
                                    .ignore_then(type_name.clone())
                                    .or_not(),
                            )
                            .map(|(names, t)| {
                                names
                                    .into_iter()
                                    .map(|(n, sp)| {
                                        (
                                            PotentialDollarIdentifier::Identifier(ParserText::new(
                                                sp, n,
                                            )),
                                            t.clone(),
                                        )
                                    })
                                    .collect::<Vec<_>>()
                            })
                            .separated_by(
                                choice((lex(pad.clone(), just(',')).ignored(), delim.clone()))
                                    .repeated()
                                    .at_least(1)
                                    .ignored(),
                            )
                            .allow_trailing()
                            .collect::<Vec<_>>(),
                    )
                    .then_ignore(delim.clone().or_not())
                    .then_ignore(lex(pad.clone(), just('}')))
                    .map(|groups| TypeDefType::Enum(groups.into_iter().flatten().collect())),
                type_name
                    .clone()
                    .map(|typ| TypeDefType::NewType(Box::new(typ))),
            )))
            .then(
                lex(pad.clone(), just("@overload"))
                    .ignore_then(lex(pad.clone(), just('{')))
                    .then_ignore(delim.clone().repeated().collect::<Vec<_>>())
                    .ignore_then(
                        lex(pad.clone(), just("const"))
                            .ignore_then(string_text.clone())
                            .map_with_span({
                                let ls = line_starts.clone();
                                move |op: String, r| ParserText::new(span(ls.as_ref(), r), op)
                            })
                            .then_ignore(lex(pad.clone(), just('=')))
                            .then(expr.clone())
                            .try_map(|(operator, value), sp| match value.node_type {
                                NodeType::FunctionDeclaration { header, body } => Ok(Overload {
                                    operator,
                                    body,
                                    header,
                                }),
                                _ => Err(Rich::custom(sp, "expected function declaration")),
                            })
                            .separated_by(delim.clone())
                            .allow_trailing()
                            .collect::<Vec<_>>()
                            .or_not()
                            .map(|x| x.unwrap_or_default()),
                    )
                    .then_ignore(delim.clone().or_not())
                    .then_ignore(lex(pad.clone(), just('}')))
                    .or_not()
                    .map(|x| x.unwrap_or_default()),
            )
            .map(|((((name, sp), generics), object), overloads)| {
                let identifier = if let Some(generics) = generics {
                    PotentialGenericTypeIdentifier::Generic {
                        identifier: PotentialDollarIdentifier::Identifier(ParserText::new(
                            sp, name,
                        )),
                        generic_types: generics
                            .into_iter()
                            .map(|(n, nsp)| {
                                PotentialNewType::DataType(ParserDataType::new(
                                    nsp,
                                    ParserInnerType::Struct(n),
                                ))
                            })
                            .collect(),
                    }
                } else {
                    PotentialGenericTypeIdentifier::Identifier(
                        PotentialDollarIdentifier::Identifier(ParserText::new(sp, name)),
                    )
                };
                Node::new(
                    sp,
                    NodeType::TypeDeclaration {
                        identifier,
                        object,
                        overloads,
                    },
                )
            });

        let trait_member = choice((
            lex(pad.clone(), just("const"))
                .ignore_then(ident.clone())
                .then(
                    lex(pad.clone(), just(':'))
                        .ignore_then(type_name.clone())
                        .or_not(),
                )
                .then(
                    lex(pad.clone(), just('='))
                        .ignore_then(statement.clone())
                        .or_not(),
                )
                .map(|(((n, sp), ty), value)| TraitMember {
                    kind: TraitMemberKind::Const,
                    identifier: PotentialDollarIdentifier::Identifier(ParserText::new(sp, n)),
                    data_type: ty.unwrap_or_else(|| auto_type(sp)),
                    value: value.map(Box::new),
                }),
            lex(pad.clone(), just("type"))
                .ignore_then(ident.clone())
                .map(|(n, sp)| TraitMember {
                    kind: TraitMemberKind::Type,
                    identifier: PotentialDollarIdentifier::Identifier(ParserText::new(sp, n)),
                    data_type: auto_type(sp),
                    value: None,
                }),
        ));

        let trait_stmt = lex(pad.clone(), just("trait"))
            .ignore_then(ident.clone())
            .then_ignore(lex(pad.clone(), just('{')))
            .then_ignore(delim.clone().repeated().collect::<Vec<_>>())
            .then(
                trait_member
                    .separated_by(delim.clone())
                    .allow_trailing()
                    .collect::<Vec<_>>()
                    .or_not()
                    .map(|x| x.unwrap_or_default()),
            )
            .then_ignore(lex(pad.clone(), just('}')))
            .map(|((name, sp), members)| {
                Node::new(
                    sp,
                    NodeType::TraitDeclaration {
                        identifier: PotentialGenericTypeIdentifier::Identifier(
                            PotentialDollarIdentifier::Identifier(ParserText::new(sp, name)),
                        ),
                        implied_traits: Vec::new(),
                        members,
                    },
                )
            });

        let impl_stmt = lex(pad.clone(), just("impl"))
            .ignore_then(generic_params.clone())
            .then(type_name.clone())
            .then(
                lex(pad.clone(), text::keyword("for"))
                    .ignore_then(type_name.clone())
                    .or_not(),
            )
            .then_ignore(lex(pad.clone(), just('{')))
            .then_ignore(delim.clone().repeated().collect::<Vec<_>>())
            .then(
                statement
                    .clone()
                    .separated_by(delim.clone())
                    .allow_trailing()
                    .collect::<Vec<_>>()
                    .or_not()
                    .map(|x| x.unwrap_or_default()),
            )
            .then_ignore(lex(pad.clone(), just('}')))
            .map(|(((generics, trait_ident), maybe_target), vars)| {
                if let Some(target) = maybe_target {
                    let trait_ident = match trait_ident {
                        PotentialNewType::DataType(dt) => match dt.data_type {
                            ParserInnerType::Struct(name) => {
                                PotentialGenericTypeIdentifier::Identifier(
                                    PotentialDollarIdentifier::Identifier(ParserText::new(
                                        dt.span, name,
                                    )),
                                )
                            }
                            ParserInnerType::StructWithGenerics {
                                identifier,
                                generic_types,
                            } => PotentialGenericTypeIdentifier::Generic {
                                identifier: PotentialDollarIdentifier::Identifier(ParserText::new(
                                    dt.span, identifier,
                                )),
                                generic_types: generic_types
                                    .into_iter()
                                    .map(PotentialNewType::DataType)
                                    .collect(),
                            },
                            _ => PotentialGenericTypeIdentifier::Identifier(
                                PotentialDollarIdentifier::Identifier(ParserText::new(
                                    dt.span,
                                    "unknown".to_string(),
                                )),
                            ),
                        },
                        _ => PotentialGenericTypeIdentifier::Identifier(
                            PotentialDollarIdentifier::Identifier(ParserText::from(
                                "unknown".to_string(),
                            )),
                        ),
                    };
                    Node::new(
                        *trait_ident.span(),
                        NodeType::ImplTraitDeclaration {
                            generics,
                            trait_ident,
                            target,
                            variables: vars,
                        },
                    )
                } else {
                    let target = trait_ident;
                    let sp = *target.span();
                    Node::new(
                        sp,
                        NodeType::ImplDeclaration {
                            generics,
                            target,
                            variables: vars,
                        },
                    )
                }
            });

        let let_struct_destruct_stmt = lex(pad.clone(), just("let"))
            .ignore_then(
                lex(pad.clone(), just('{'))
                    .ignore_then(
                        ident
                            .clone()
                            .then(
                                lex(pad.clone(), just(':'))
                                    .ignore_then(lex(pad.clone(), just("mut")).or_not())
                                    .then(ident.clone())
                                    .or_not(),
                            )
                            .map(|((field, fsp), alias)| match alias {
                                Some((mut_tok, (name, nsp))) => (
                                    field,
                                    if mut_tok.is_some() {
                                        VarType::Mutable
                                    } else {
                                        VarType::Immutable
                                    },
                                    PotentialDollarIdentifier::Identifier(ParserText::new(
                                        nsp, name,
                                    )),
                                ),
                                None => (
                                    field.clone(),
                                    VarType::Immutable,
                                    PotentialDollarIdentifier::Identifier(ParserText::new(
                                        fsp, field,
                                    )),
                                ),
                            })
                            .separated_by(lex(pad.clone(), just(',')))
                            .allow_trailing()
                            .collect::<Vec<_>>()
                            .or_not()
                            .map(|x| x.unwrap_or_default()),
                    )
                    .then_ignore(lex(pad.clone(), just('}'))),
            )
            .then_ignore(lex(pad.clone(), just('=')))
            .then(expr.clone())
            .map(|(fields, value)| {
                Node::new(
                    value.span,
                    NodeType::DestructureDeclaration {
                        var_type: VarType::Immutable,
                        pattern: DestructurePattern::Struct(fields),
                        value: Box::new(value),
                    },
                )
            })
            .boxed();

        let let_tuple_destruct_stmt = lex(pad.clone(), just("let"))
            .ignore_then(
                choice((
                    lex(pad.clone(), just("..")).to(None),
                    lex(pad.clone(), just("mut"))
                        .or_not()
                        .then(ident.clone())
                        .map(|(mut_tok, (name, sp))| {
                            Some((
                                if mut_tok.is_some() {
                                    VarType::Mutable
                                } else {
                                    VarType::Immutable
                                },
                                PotentialDollarIdentifier::Identifier(ParserText::new(sp, name)),
                            ))
                        }),
                ))
                .separated_by(lex(pad.clone(), just(',')))
                .at_least(2)
                .collect::<Vec<_>>(),
            )
            .then_ignore(lex(pad.clone(), just('=')))
            .then(
                expr.clone().then(
                    lex(pad.clone(), just(','))
                        .ignore_then(expr.clone())
                        .repeated()
                        .collect::<Vec<_>>(),
                ),
            )
            .map(|(items, (first_value, rest_values))| {
                let value = if rest_values.is_empty() {
                    first_value
                } else {
                    let mut values = vec![first_value];
                    values.extend(rest_values);
                    Node::new(Span::default(), NodeType::TupleLiteral { values })
                };
                Node::new(
                    value.span,
                    NodeType::DestructureDeclaration {
                        var_type: VarType::Immutable,
                        pattern: DestructurePattern::Tuple(items),
                        value: Box::new(value),
                    },
                )
            })
            .boxed();

        let let_stmt = choice((
            lex(pad.clone(), just("let")).to(VarType::Immutable),
            lex(pad.clone(), just("const")).to(VarType::Constant),
        ))
        .then(lex(pad.clone(), just("mut")).or_not())
        .then(named_ident.clone())
        .then(
            lex(pad.clone(), just(':'))
                .ignore_then(type_name.clone())
                .or_not(),
        )
        .then_ignore(lex(pad.clone(), just('=')))
        .then(
            expr.clone().then(
                lex(pad.clone(), just(','))
                    .ignore_then(expr.clone())
                    .repeated()
                    .collect::<Vec<_>>(),
            ),
        )
        .map(|((((vt, m), name), ty), (first_value, rest_values))| {
            let var_type = match (vt, m.is_some()) {
                (VarType::Immutable, true) => VarType::Mutable,
                (x, _) => x,
            };
            let value = if rest_values.is_empty() {
                first_value
            } else {
                match first_value.node_type {
                    NodeType::EnumExpression {
                        identifier,
                        value,
                        data,
                    } => {
                        let mut payload_values = if let Some(existing) = data {
                            match existing.node_type {
                                NodeType::TupleLiteral { values } => values,
                                other => vec![Node::new(Span::default(), other)],
                            }
                        } else {
                            Vec::new()
                        };
                        payload_values.extend(rest_values);
                        let payload = if payload_values.len() == 1 {
                            payload_values.into_iter().next()
                        } else {
                            Some(Node::new(
                                Span::default(),
                                NodeType::TupleLiteral {
                                    values: payload_values,
                                },
                            ))
                        };
                        Node::new(
                            Span::default(),
                            NodeType::EnumExpression {
                                identifier,
                                value,
                                data: payload.map(Box::new),
                            },
                        )
                    }
                    other => {
                        let mut values = vec![Node::new(Span::default(), other)];
                        values.extend(rest_values);
                        Node::new(Span::default(), NodeType::TupleLiteral { values })
                    }
                }
            };
            let value_span = value.span;
            Node::new(
                Span::new_from_spans(*name.span(), value.span),
                NodeType::VariableDeclaration {
                    var_type,
                    identifier: name,
                    value: Box::new(value),
                    data_type: ty.unwrap_or_else(|| auto_type(value_span)),
                },
            )
        });

        let scope_name = lex(pad.clone(), just('@'))
            .ignore_then(ident.clone())
            .map(|(n, sp)| PotentialDollarIdentifier::Identifier(ParserText::new(sp, n)))
            .boxed();

        let scope_args = lex(pad.clone(), just('['))
            .ignore_then(
                lex(pad.clone(), just('$'))
                    .ignore_then(raw_ident.clone())
                    .map(|(n, sp)| PotentialDollarIdentifier::Identifier(ParserText::new(sp, n)))
                    .then_ignore(lex(pad.clone(), just('=')))
                    .then(
                        lex(pad.clone(), just("type"))
                            .ignore_then(lex(pad.clone(), just(':')))
                            .ignore_then(type_name.clone())
                            .map(|data_type| {
                                Node::new(*data_type.span(), NodeType::DataType { data_type })
                            })
                            .or(expr.clone()),
                    )
                    .separated_by(lex(pad.clone(), just(',')))
                    .allow_trailing()
                    .collect::<Vec<_>>()
                    .or_not()
                    .map(|x| x.unwrap_or_default()),
            )
            .then_ignore(lex(pad.clone(), just(']')))
            .boxed();

        let named_scope = scope_name
            .clone()
            .then(scope_args.clone().or_not().map(|x| x.unwrap_or_default()))
            .map(|(name, args)| NamedScope { name, args })
            .boxed();

        let scope_body_with_mode = choice((
            lex(pad.clone(), just("{{"))
                .ignore_then(delim.clone().repeated().collect::<Vec<_>>())
                .ignore_then(
                    statement
                        .clone()
                        .separated_by(delim.clone())
                        .allow_trailing()
                        .collect::<Vec<_>>()
                        .or_not()
                        .map(|x| x.unwrap_or_default()),
                )
                .then_ignore(delim.clone().or_not())
                .then_ignore(lex(pad.clone(), just("}}")))
                .map(|items| (Some(items), Some(false))),
            scope_node_parser(statement.clone(), delim.clone(), pad.clone())
                .map(|body| (scope_body_or_single(body), Some(true))),
            pad_with_newline
                .clone()
                .ignore_then(choice((statement.clone(), expr.clone())))
                .map(|body| (Some(vec![body]), Some(false))),
        ))
        .or_not()
        .map(|x| x.unwrap_or((None, None)))
        .boxed();

        let scope_define_stmt = lex(pad.clone(), just("let"))
            .ignored()
            .ignore_then(fat_arrow.clone())
            .ignore_then(named_scope.clone().or_not())
            .then(scope_body_with_mode.clone())
            .map(|(named, (body, create_new_scope))| {
                Node::new(
                    Span::default(),
                    NodeType::ScopeDeclaration {
                        body,
                        named,
                        is_temp: true,
                        create_new_scope,
                        define: true,
                    },
                )
            })
            .boxed();

        let scope_call_mode = choice((
            lex(pad.clone(), just("{{"))
                .then_ignore(lex(pad.clone(), just("}}")))
                .to(Some(false)),
            lex(pad.clone(), just('{'))
                .then_ignore(lex(pad.clone(), just('}')))
                .to(Some(true)),
        ))
        .or_not()
        .map(|x| x.flatten())
        .boxed();

        let scope_alias_stmt = lex(pad.clone(), just("let"))
            .ignore_then(scope_name.clone())
            .then_ignore(fat_arrow.clone())
            .then(scope_name.clone())
            .then(scope_args.clone().or_not().map(|x| x.unwrap_or_default()))
            .then(scope_call_mode.clone())
            .map(|(((identifier, name), args), create_new_scope)| {
                Node::new(
                    Span::default(),
                    NodeType::ScopeAlias {
                        identifier,
                        value: NamedScope { name, args },
                        create_new_scope,
                    },
                )
            })
            .boxed();

        let scope_call_stmt = fat_arrow
            .clone()
            .ignored()
            .ignore_then(named_scope.or_not())
            .then(scope_body_with_mode)
            .map(|(named, (body, create_new_scope))| {
                Node::new(
                    Span::default(),
                    NodeType::ScopeDeclaration {
                        body,
                        named,
                        is_temp: true,
                        create_new_scope,
                        define: false,
                    },
                )
            })
            .boxed();

        let return_stmt = lex(pad.clone(), just("return"))
            .ignore_then(expr.clone().or_not())
            .map_with_span({
                let ls = line_starts.clone();
                move |value, r| {
                    Node::new(
                        span(ls.as_ref(), r),
                        NodeType::Return {
                            value: value.map(Box::new),
                        },
                    )
                }
            });

        let test_stmt =
            lex(pad.clone(), just("test"))
                .ignore_then(ident.clone())
                .then(
                    fat_arrow.clone().ignore_then(
                        scope_node_parser(statement.clone(), delim.clone(), pad.clone())
                            .or(choice((statement.clone(), expr.clone()))
                                .map(|n| ensure_scope_node(n, true, false))),
                    ),
                )
                .map(|((name, sp), body)| {
                    Node::new(
                        Span::new_from_spans(sp, body.span),
                        NodeType::TestDeclaration {
                            identifier: PotentialDollarIdentifier::Identifier(ParserText::new(
                                sp, name,
                            )),
                            body: Box::new(body),
                        },
                    )
                })
                .boxed();

        let bench_stmt =
            lex(pad.clone(), just("bench"))
                .ignore_then(ident.clone())
                .then(
                    fat_arrow.clone().ignore_then(
                        scope_node_parser(statement.clone(), delim.clone(), pad.clone())
                            .or(choice((statement.clone(), expr.clone()))
                                .map(|n| ensure_scope_node(n, true, false))),
                    ),
                )
                .map(|((name, sp), body)| {
                    Node::new(
                        Span::new_from_spans(sp, body.span),
                        NodeType::BenchDeclaration {
                            identifier: PotentialDollarIdentifier::Identifier(ParserText::new(
                                sp, name,
                            )),
                            body: Box::new(body),
                        },
                    )
                })
                .boxed();

        let ffi_type = recursive(|ffi_type| {
            let ffi_base = lex(pad.clone(), just('@'))
                .ignore_then(raw_ident.clone())
                .map(|(name, sp)| {
                    let parsed =
                        ParserFfiInnerType::from_str(&name).unwrap_or(ParserFfiInnerType::Int);
                    ParserDataType::new(sp, ParserInnerType::FfiType(parsed))
                })
                .or(type_name.clone().map(|x| match x {
                    PotentialNewType::DataType(dt) => dt,
                    _ => ParserDataType::new(Span::default(), ParserInnerType::Auto(None)),
                }))
                .boxed();

            lex(pad.clone(), just("ptr"))
                .ignore_then(lex(pad.clone(), just(":<")))
                .ignore_then(ffi_type.clone())
                .then_ignore(lex(pad.clone(), just('>')))
                .map(|inner: ParserDataType| {
                    ParserDataType::new(inner.span, ParserInnerType::Ptr(Box::new(inner)))
                })
                .or(ffi_base)
                .boxed()
        })
        .boxed();

        let extern_stmt = lex(pad.clone(), just("extern"))
            .ignore_then(string_text.clone())
            .then_ignore(lex(pad.clone(), just("const")))
            .then(ident.clone())
            .then_ignore(lex(pad.clone(), just('=')))
            .then_ignore(lex(pad.clone(), just("fn")))
            .then_ignore(lex(pad.clone(), just('(')))
            .then(
                ffi_type
                    .clone()
                    .separated_by(lex(pad.clone(), just(',')))
                    .allow_trailing()
                    .collect::<Vec<_>>()
                    .or_not()
                    .map(|x: Option<Vec<ParserDataType>>| x.unwrap_or_default()),
            )
            .then_ignore(lex(pad.clone(), just(')')))
            .then(arrow.clone().ignore_then(ffi_type.clone()).or_not().map(
                |x: Option<ParserDataType>| {
                    x.unwrap_or_else(|| ParserDataType::new(Span::default(), ParserInnerType::Null))
                },
            ))
            .then_ignore(lex(pad.clone(), just("from")))
            .then(string_text.clone())
            .then(
                lex(pad.clone(), just("as"))
                    .ignore_then(string_text.clone())
                    .or_not(),
            )
            .map(
                move |(((((abi, (name, sp)), parameters), return_type), mut library), symbol)| {
                    if !library.is_empty() {
                        if let Some(base) = source_path.and_then(|p| p.parent()) {
                            let candidates = [
                                base.join(&library),
                                base.join(library.trim_start().trim_start_matches("./")),
                            ];
                            for candidate in candidates {
                                if candidate.exists() {
                                    library = candidate.to_string_lossy().to_string();
                                    break;
                                }
                            }
                        }
                    }
                    Node::new(
                        sp,
                        NodeType::ExternFunctionDeclaration {
                            abi,
                            identifier: PotentialDollarIdentifier::Identifier(ParserText::new(
                                sp, name,
                            )),
                            parameters,
                            return_type,
                            library,
                            symbol,
                        },
                    )
                },
            )
            .boxed();

        let select_arm = choice((
            lex(pad.clone(), just('_'))
                .then_ignore(fat_arrow.clone())
                .then(
                    lex(pad.clone(), just('{'))
                        .rewind()
                        .to(true)
                        .or_not()
                        .map(|x| x.unwrap_or(false))
                        .then(
                            scope_node_parser(statement.clone(), delim.clone(), pad.clone())
                                .or(choice((statement.clone(), expr.clone()))),
                        )
                        .try_map(|(has_brace, body), sp| {
                            if has_brace
                                && !matches!(body.node_type, NodeType::ScopeDeclaration { .. })
                            {
                                Err(Rich::custom(sp, "expected scope body"))
                            } else {
                                Ok(body)
                            }
                        }),
                )
                .map(|(_wild, body)| SelectArm {
                    patterns: vec![(SelectArmKind::Default, None, None)],
                    conditionals: Vec::new(),
                    body,
                }),
            ident
                .clone()
                .map(|(n, sp)| ident_node(sp, &n))
                .then_ignore(left_arrow.clone())
                .then(expr.clone())
                .then_ignore(fat_arrow.clone())
                .then(
                    lex(pad.clone(), just('{'))
                        .rewind()
                        .to(true)
                        .or_not()
                        .map(|x| x.unwrap_or(false))
                        .then(
                            scope_node_parser(statement.clone(), delim.clone(), pad.clone())
                                .or(choice((statement.clone(), expr.clone()))),
                        )
                        .try_map(|(has_brace, body), sp| {
                            if has_brace
                                && !matches!(body.node_type, NodeType::ScopeDeclaration { .. })
                            {
                                Err(Rich::custom(sp, "expected scope body"))
                            } else {
                                Ok(body)
                            }
                        }),
                )
                .map(|((lhs, rhs), body)| SelectArm {
                    patterns: vec![(SelectArmKind::Recv, Some(lhs), Some(rhs))],
                    conditionals: Vec::new(),
                    body,
                }),
        ))
        .boxed();

        let select_stmt = lex(pad.clone(), just("select"))
            .ignore_then(lex(pad.clone(), just('{')))
            .ignore_then(delim.clone().repeated().collect::<Vec<_>>())
            .ignore_then(
                select_arm
                    .clone()
                    .separated_by(
                        choice((delim.clone(), lex(pad.clone(), just(',')).ignored()))
                            .repeated()
                            .at_least(1),
                    )
                    .allow_trailing()
                    .collect::<Vec<_>>()
                    .or_not()
                    .map(|x| x.unwrap_or_default()),
            )
            .then_ignore(delim.clone().or_not())
            .then_ignore(lex(pad.clone(), just('}')))
            .map(|arms| Node::new(Span::default(), NodeType::SelectStatement { arms }))
            .boxed();

        let use_stmt = lex(pad.clone(), just("use"))
            .ignore_then(
                ident
                    .clone()
                    .map(|(n, sp)| PotentialDollarIdentifier::Identifier(ParserText::new(sp, n)))
                    .separated_by(lex(pad.clone(), just(',')))
                    .at_least(1)
                    .collect::<Vec<_>>()
                    .then_ignore(left_arrow.clone())
                    .then(expr.clone())
                    .or(expr.clone().map(|value| (Vec::new(), value))),
            )
            .map(|(identifiers, value)| {
                Node::new(
                    value.span,
                    NodeType::Use {
                        identifiers,
                        value: Box::new(value),
                    },
                )
            })
            .boxed();

        let spawn_item = fat_arrow
            .clone()
            .ignore_then(
                scope_node_parser(statement.clone(), delim.clone(), pad.clone()).or(choice((
                    statement.clone(),
                    expr.clone(),
                ))
                .map(|n| ensure_scope_node(n, true, false))),
            )
            .boxed();

        let spawn_block = lex(pad.clone(), just('{'))
            .ignore_then(delim.clone().repeated().collect::<Vec<_>>())
            .ignore_then(
                spawn_item
                    .clone()
                    .separated_by(
                        lex(pad.clone(), just(','))
                            .then_ignore(delim.clone().repeated().collect::<Vec<_>>())
                            .ignored(),
                    )
                    .allow_trailing()
                    .collect::<Vec<_>>()
                    .or_not()
                    .map(|x| x.unwrap_or_default()),
            )
            .then_ignore(delim.clone().or_not())
            .then_ignore(lex(pad.clone(), just('}')))
            .boxed();

        let spawn_stmt = lex(pad.clone(), just("spawn"))
            .ignore_then(choice((spawn_block, expr.clone().map(|x| vec![x]))))
            .map(|items| {
                let sp = if let (Some(a), Some(b)) = (items.first(), items.last()) {
                    Span::new_from_spans(a.span, b.span)
                } else {
                    Span::default()
                };
                Node::new(sp, NodeType::Spawn { items })
            })
            .boxed();

        let assign_lhs_base = ident
            .clone()
            .map(|(n, sp)| ident_node(sp, &n))
            .then(
                choice((
                    lex(pad.clone(), just('.'))
                        .ignore_then(ident.clone().map(|(n, sp)| ident_node(sp, &n)))
                        .map(|n| (n, false)),
                    lex(pad.clone(), just('['))
                        .ignore_then(expr.clone())
                        .then_ignore(lex(pad.clone(), just(']')))
                        .map(|e| (e, true)),
                ))
                .repeated()
                .collect::<Vec<_>>(),
            )
            .map(|(head, rest)| {
                if rest.is_empty() {
                    head
                } else {
                    let mut path = vec![(head, false)];
                    path.extend(rest);
                    let sp = match (path.first(), path.last()) {
                        (Some(first), Some(last)) => {
                            Span::new_from_spans(first.0.span, last.0.span)
                        }
                        _ => Span::default(),
                    };
                    Node::new(sp, NodeType::MemberExpression { path })
                }
            })
            .boxed();

        let assign_lhs = lex(pad.clone(), just('*'))
            .repeated()
            .collect::<Vec<_>>()
            .then(assign_lhs_base)
            .map(|(stars, mut lhs)| {
                for _ in stars {
                    lhs = Node::new(
                        lhs.span,
                        NodeType::DerefStatement {
                            value: Box::new(lhs),
                        },
                    );
                }
                lhs
            })
            .boxed();

        let assign_stmt = assign_lhs
            .then(choice((
                lex(pad.clone(), just('=')).to(None),
                lex(pad.clone(), just("+=")).to(Some(BinaryOperator::Add)),
                lex(pad.clone(), just("-=")).to(Some(BinaryOperator::Sub)),
                lex(pad.clone(), just("*=")).to(Some(BinaryOperator::Mul)),
                lex(pad.clone(), just("/=")).to(Some(BinaryOperator::Div)),
                lex(pad.clone(), just("%=")).to(Some(BinaryOperator::Mod)),
                lex(pad.clone(), just("&=")).to(Some(BinaryOperator::BitAnd)),
                lex(pad.clone(), just("|=")).to(Some(BinaryOperator::BitOr)),
                lex(pad.clone(), just("^=")).to(Some(BinaryOperator::BitXor)),
                lex(pad.clone(), just("<<=")).to(Some(BinaryOperator::Shl)),
                lex(pad.clone(), just(">>=")).to(Some(BinaryOperator::Shr)),
            )))
            .then(expr.clone())
            .map(|((lhs, op), value)| {
                let rhs = if let Some(op) = op {
                    Node::new(
                        Span::new_from_spans(lhs.span, value.span),
                        NodeType::BinaryExpression {
                            left: Box::new(lhs.clone()),
                            right: Box::new(value),
                            operator: op,
                        },
                    )
                } else {
                    value
                };
                Node::new(
                    Span::new_from_spans(lhs.span, rhs.span),
                    NodeType::AssignmentExpression {
                        identifier: Box::new(lhs),
                        value: Box::new(rhs),
                    },
                )
            });

        let destruct_assign_stmt = ident
            .clone()
            .then(
                lex(pad.clone(), just(','))
                    .ignore_then(ident.clone())
                    .repeated()
                    .at_least(1)
                    .collect::<Vec<_>>(),
            )
            .then_ignore(lex(pad.clone(), just('=')))
            .then(expr.clone())
            .map(|(((first_n, first_sp), rest), value)| {
                let mut names = vec![(first_n, first_sp)];
                names.extend(rest);
                let pattern = DestructurePattern::Tuple(
                    names
                        .into_iter()
                        .map(|(n, sp)| {
                            Some((
                                VarType::Immutable,
                                PotentialDollarIdentifier::Identifier(ParserText::new(sp, n)),
                            ))
                        })
                        .collect(),
                );
                Node::new(
                    value.span,
                    NodeType::DestructureAssignment {
                        pattern,
                        value: Box::new(value),
                    },
                )
            })
            .boxed();

        choice((
            import_stmt,
            extern_stmt,
            type_stmt,
            trait_stmt,
            impl_stmt,
            use_stmt,
            select_stmt,
            spawn_stmt,
            test_stmt,
            bench_stmt,
            let_struct_destruct_stmt,
            let_tuple_destruct_stmt,
            scope_define_stmt,
            scope_alias_stmt,
            scope_call_stmt,
            let_stmt,
            return_stmt,
            destruct_assign_stmt,
            assign_stmt,
            expr,
        ))
        .boxed()
    })
    .boxed();

    let parsed = pad
        .clone()
        .then_ignore(delim.clone().repeated().collect::<Vec<_>>())
        .ignore_then(
            parser
                .separated_by(delim.clone())
                .allow_trailing()
                .collect::<Vec<_>>()
                .or_not()
                .map(|x| x.unwrap_or_default()),
        )
        .then_ignore(pad.clone())
        .then_ignore(delim.clone().repeated().collect::<Vec<_>>())
        .then_ignore(end())
        .parse(source);

    if let Some(items) = parsed.output().cloned() {
        let sp = if let (Some(a), Some(b)) = (items.first(), items.last()) {
            Span::new_from_spans(a.span, b.span)
        } else {
            Span::default()
        };
        return Ok(Node::new(
            sp,
            NodeType::ScopeDeclaration {
                body: Some(items),
                named: None,
                is_temp: false,
                create_new_scope: Some(false),
                define: false,
            },
        ));
    }
    Err(to_parser_errors(line_starts.as_ref(), parsed.into_errors()))
}
