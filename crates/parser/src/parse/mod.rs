use chumsky::prelude::*;
use std::str::FromStr;
use std::sync::Arc;

mod diagnostics;
mod util;

use crate::{
    ParserError,
    ast::{
        CallArg, DestructurePattern, FunctionHeader, GenericType, GenericTypes, IfComparisonType, LoopType,
        MatchArmType, Node, NodeType, ObjectType, ParserDataType, ParserInnerType, ParserText,
        ParserFfiDataType, ParserFfiInnerType, PotentialDollarIdentifier, PipeSegment,
        PotentialFfiDataType, PotentialGenericTypeIdentifier, PotentialNewType, RefMutability,
        SelectArm, SelectArmKind, TraitMember, TraitMemberKind, TryCatch, Overload, NamedScope,
        TypeDefType, VarType,
        binary::BinaryOperator,
        comparison::{BooleanOperator, ComparisonOperator},
    },
    lexer::Span,
};

use diagnostics::to_parser_errors;
use util::{
    auto_type, ident_node, is_keyword, lex, normalize_scope_member_chain, parse_block_scope,
    parse_embedded_expr, parse_splits, scope_node, scope_node_parser, span, unescape_char,
    unescape_string,
    strip_block_comments_keep_layout,
};

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
    let line_comment = just("//")
        .then(take_until(just('\n')))
        .ignored();
    let block_comment = just("/*")
        .then(take_until(just("*/")))
        .then_ignore(just("*/"))
        .ignored();
    let pad = choice((ws, line_comment, block_comment))
        .repeated()
        .ignored()
        .boxed();
    let delim = choice((just('\n'), just(';')))
        .padded_by(pad.clone())
        .repeated()
        .at_least(1)
        .ignored()
        .boxed();

    let arrow = lex(pad.clone(), just('-').then_ignore(just('>'))).ignored().boxed();
    let fat_arrow = lex(pad.clone(), just('=').then_ignore(just('>'))).ignored().boxed();
    let left_arrow = lex(pad.clone(), just('<').then_ignore(just('-'))).ignored().boxed();

    let raw_ident = lex(pad.clone(), text::ident()).map_with_span({
        let ls = line_starts.clone();
        move |s: String, r| (s, span(ls.as_ref(), r))
    }).boxed();

    let ident = lex(pad.clone(), text::ident())
    .try_map(|s: String, span| {
        if is_keyword(&s) {
            Err(Simple::custom(span, "keyword cannot be identifier"))
        } else {
            Ok(s)
        }
    })
    .map_with_span({
        let ls = line_starts.clone();
        move |s: String, r| (s, span(ls.as_ref(), r))
    }).boxed();
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

    let string_lit = lex(pad.clone(), 
        just('"')
            .ignore_then(
                choice((
                    just('\\').ignore_then(any()).map(|c| format!("\\{c}")),
                    filter(|c: &char| *c != '"' && *c != '\n').map(|c| c.to_string()),
                ))
                .repeated(),
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
    }).boxed();
    let string_text = lex(
        pad.clone(),
        just('"')
            .ignore_then(
                choice((
                    just('\\').ignore_then(any()).map(|c| format!("\\{c}")),
                    filter(|c: &char| *c != '"' && *c != '\n').map(|c| c.to_string()),
                ))
                .repeated(),
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

    let float_suffix_lit = lex(
        pad.clone(),
        text::int(10)
            .then_ignore(choice((just('f'), just('F'))))
            .map(|n: String| n),
    )
    .map_with_span({
        let ls = line_starts.clone();
        move |n: String, r| {
            let v = n.parse::<f64>().unwrap_or_default();
            Node::new(span(ls.as_ref(), r), NodeType::FloatLiteral(v))
        }
    })
    .boxed();

    let int_lit = lex(pad.clone(), text::int(10)).map_with_span({
        let ls = line_starts.clone();
        move |n: String, r| Node::new(span(ls.as_ref(), r), NodeType::IntLiteral(n))
    }).boxed();

    let float_lit = lex(pad.clone(), 
        text::int(10)
            .then_ignore(just('.'))
            .then(text::digits(10))
            .map(|(a, b): (String, String)| format!("{a}.{b}")),
    )
    .map_with_span({
        let ls = line_starts.clone();
        move |n: String, r| {
            let v = n.parse::<f64>().unwrap_or_default();
            Node::new(span(ls.as_ref(), r), NodeType::FloatLiteral(v))
        }
    }).boxed();

    let null_lit = lex(pad.clone(), just("null")).map_with_span({
        let ls = line_starts.clone();
        move |_, r| Node::new(span(ls.as_ref(), r), NodeType::Null)
    }).boxed();

    let type_name = recursive(|ty| {
        let type_path = raw_ident
            .clone()
            .then(
                lex(pad.clone(), just("::"))
                    .ignore_then(raw_ident.clone())
                    .repeated(),
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
                    let inner = match name.as_str() {
                        "f32" | "f64" => ParserInnerType::Float,
                        "u8" | "u16" | "u32" | "u64" | "usize" | "uint" => ParserInnerType::UInt,
                        "char" | "uchar" | "schar" => ParserInnerType::Char,
                        _ => ParserInnerType::Int,
                    };
                    PotentialNewType::DataType(ParserDataType::new(sp, inner))
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
            lex(pad.clone(), just("fn"))
                .ignore_then(lex(pad.clone(), just('(')))
                .ignore_then(
                    ty.clone()
                        .separated_by(lex(pad.clone(), just(',')))
                        .allow_trailing()
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
            lex(pad.clone(), just("mut")).ignore_then(ty.clone()).map(|inner| {
                let dt = match inner {
                    PotentialNewType::DataType(x) => x,
                    _ => ParserDataType::new(Span::default(), ParserInnerType::Auto(None)),
                };
                PotentialNewType::DataType(ParserDataType::new(
                    Span::default(),
                    ParserInnerType::Ref(Box::new(dt), RefMutability::MutValue),
                ))
            }),
            lex(pad.clone(), just("&mut")).ignore_then(ty.clone()).map(|inner| {
                let dt = match inner {
                    PotentialNewType::DataType(x) => x,
                    _ => ParserDataType::new(Span::default(), ParserInnerType::Auto(None)),
                };
                PotentialNewType::DataType(ParserDataType::new(
                    Span::default(),
                    ParserInnerType::Ref(Box::new(dt), RefMutability::MutRef),
                ))
            }),
            lex(pad.clone(), just('&')).ignore_then(ty.clone()).map(|inner| {
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
                let ok = match left {
                    PotentialNewType::DataType(dt) => dt,
                    _ => ParserDataType::new(Span::default(), ParserInnerType::Auto(None)),
                };
                let err = match right {
                    PotentialNewType::DataType(dt) => dt,
                    _ => ParserDataType::new(Span::default(), ParserInnerType::Auto(None)),
                };
                PotentialNewType::DataType(ParserDataType::new(
                    Span::new_from_spans(ok.span, err.span),
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
                    PotentialNewType::DataType(dt) => PotentialNewType::DataType(ParserDataType::new(
                        dt.span,
                        ParserInnerType::Option(Box::new(dt)),
                    )),
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
            let scope = lex(pad.clone(), just('{'))
                .then_ignore(delim.clone().repeated())
                .ignore_then(
                    statement
                        .clone()
                        .separated_by(delim.clone())
                        .allow_trailing()
                        .or_not()
                        .map(|x| x.unwrap_or_default()),
                )
                .then_ignore(delim.clone().or_not())
                .then_ignore(lex(pad.clone(), just('}')))
                .map(|items| scope_node(items, true, false))
                .boxed();

            let labelled_scope = lex(pad.clone(), just('@'))
                .ignore_then(ident.clone())
                .then(lex(pad.clone(), just("[]")).or_not())
                .ignore_then(scope.clone())
                .boxed();

            let tuple_param = lex(pad.clone(), just('('))
                .ignore_then(
                    ident
                        .clone()
                        .separated_by(lex(pad.clone(), just(',')))
                        .allow_trailing()
                        .or_not()
                        .map(|x| x.unwrap_or_default()),
                )
                .then_ignore(lex(pad.clone(), just(')')))
                .map(|items| {
                    items
                        .into_iter()
                        .map(|(n, sp)| {
                            (
                                PotentialDollarIdentifier::Identifier(ParserText::new(sp, n)),
                                auto_type(sp),
                            )
                        })
                        .collect::<Vec<_>>()
                });

            let plain_param = ident
                .clone()
                .repeated()
                .at_least(1)
                .then(lex(pad.clone(), just(':')).ignore_then(type_name.clone()).or_not())
                .map(|(names, ty)| {
                    let t = ty.unwrap_or_else(|| auto_type(Span::default()));
                    names
                        .into_iter()
                        .map(|(n, sp)| {
                            (
                                PotentialDollarIdentifier::Identifier(ParserText::new(sp, n)),
                                t.clone(),
                            )
                        })
                        .collect::<Vec<_>>()
                });

            let fn_param_groups = choice((tuple_param, plain_param))
                .separated_by(lex(pad.clone(), just(',')))
                .allow_trailing()
                .or_not()
                .map(|x| x.unwrap_or_default())
                .map(|groups| groups.into_iter().flatten().collect::<Vec<_>>())
                .boxed();

            let fn_match_expr = lex(pad.clone(), just("fn"))
                .ignore_then(lex(pad.clone(), just("match")))
                .then(take_until(lex(pad.clone(), just("};"))))
                .map(|_| {
                    let body = scope_node(Vec::new(), true, false);
                    Node::new(
                        body.span,
                        NodeType::FunctionDeclaration {
                            header: FunctionHeader {
                                generics: GenericTypes::default(),
                                parameters: Vec::new(),
                                return_type: auto_type(body.span),
                                param_destructures: Vec::new(),
                            },
                            body: Box::new(body),
                        },
                    )
                })
                .boxed();

            let fn_expr = lex(pad.clone(), just("fn"))
                .ignore_then(
                    lex(pad.clone(), just('<'))
                        .ignore_then(
                            ident
                                .clone()
                                .separated_by(lex(pad.clone(), just(',')))
                                .allow_trailing()
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
                                        identifier: PotentialDollarIdentifier::Identifier(
                                            ParserText::new(sp, n),
                                        ),
                                        trait_constraints: Vec::new(),
                                    })
                                    .collect(),
                            )
                        }),
                )
                .then_ignore(lex(pad.clone(), just('(')))
                .then(fn_param_groups)
                .then_ignore(lex(pad.clone(), just(')')))
                .then(arrow.clone().ignore_then(type_name.clone()).or_not())
                .then_ignore(fat_arrow.clone())
                .then(
                    lex(pad.clone(), just('{'))
                        .rewind()
                        .ignore_then(scope.clone())
                        .or(choice((statement.clone(), expr.clone())).map(|e| scope_node(vec![e], true, false))),
                )
                .map(|(((generics, params), ret), body)| {
                    let parameters = params;
                    let body = match body.node_type {
                        NodeType::ScopeDeclaration { body, named, create_new_scope: _, .. } => Node::new(
                            body.as_ref().and_then(|b| b.first().zip(b.last())).map(|(a,b)| Span::new_from_spans(a.span,b.span)).unwrap_or(Span::default()),
                            NodeType::ScopeDeclaration { body, named, is_temp: true, create_new_scope: Some(true), define: false },
                        ),
                        _ => scope_node(vec![body], true, false),
                    };
                    Node::new(
                        body.span,
                        NodeType::FunctionDeclaration {
                            header: FunctionHeader {
                                generics,
                                parameters,
                                return_type: ret.unwrap_or_else(|| auto_type(body.span)),
                                param_destructures: Vec::new(),
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
                                .or_not()
                                .map(|x| x.unwrap_or_default()),
                        )
                        .then_ignore(lex(pad.clone(), just('>')))
                        .or_not(),
                )
                .map(|((n, sp), generics)| {
                    if let Some(generic_types) = generics {
                        PotentialGenericTypeIdentifier::Generic {
                            identifier: PotentialDollarIdentifier::Identifier(ParserText::new(sp, n)),
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
            ))
            .or_not()
            .map(|v| v.unwrap_or(VarType::Immutable))
            .boxed();

            let match_arm_start = choice((
                lex(pad.clone(), just('.'))
                    .ignore_then(ident.clone())
                    .map(|(name, sp)| MatchArmType::Enum {
                        value: PotentialDollarIdentifier::Identifier(ParserText::new(sp, name)),
                        var_type: VarType::Immutable,
                        name: None,
                        destructure: None,
                    }),
                choice((lex(pad.clone(), just("let")).to(VarType::Immutable), lex(pad.clone(), just("const")).to(VarType::Constant)))
                    .then(lex(pad.clone(), just("mut")).or_not())
                    .then(ident.clone())
                    .map(|((vt, m), (name, sp))| MatchArmType::Let {
                        var_type: if vt == VarType::Immutable && m.is_some() {
                            VarType::Mutable
                        } else {
                            vt
                        },
                        name: PotentialDollarIdentifier::Identifier(ParserText::new(sp, name)),
                    }),
                lex(pad.clone(), just('_')).map_with_span({
                    let ls = line_starts.clone();
                    move |_, r| MatchArmType::Wildcard(span(ls.as_ref(), r))
                }),
                expr.clone().map(MatchArmType::Value),
            ))
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
                    .allow_trailing()
                    .or_not()
                    .map(|x| x.unwrap_or_default()),
                )
                .then_ignore(lex(pad.clone(), just(')')))
                .map(DestructurePattern::Tuple)
                .boxed();

            let match_arm_body = fat_arrow
                .clone()
                .ignore_then(choice((scope.clone(), statement.clone(), expr.clone())))
                .map(|body| match body.node_type {
                    NodeType::ScopeDeclaration { .. } => body,
                    _ => scope_node(vec![body], true, false),
                })
                .boxed();

            let match_arm = match_arm_start
                .clone()
                .then(
                    lex(pad.clone(), just('|'))
                        .ignore_then(match_arm_start.clone())
                        .repeated(),
                )
                .then(
                    lex(pad.clone(), just(':'))
                        .ignore_then(match_var_type.clone())
                        .then(
                            choice((
                                match_struct_destructure
                                    .clone()
                                    .map(|d| (None, Some(d))),
                                match_tuple_destructure
                                    .clone()
                                    .map(|d| (None, Some(d))),
                                lex(pad.clone(), just("mut"))
                                    .or_not()
                                    .then(ident.clone())
                                    .then(
                                        lex(pad.clone(), just(','))
                                            .ignore_then(
                                                lex(pad.clone(), just("mut"))
                                                    .or_not()
                                                    .then(ident.clone()),
                                            )
                                            .repeated(),
                                    )
                                    .map(|((first_mut, (first_name, first_sp)), rest)| {
                                        let first = (
                                            if first_mut.is_some() {
                                                VarType::Mutable
                                            } else {
                                                VarType::Immutable
                                            },
                                            PotentialDollarIdentifier::Identifier(ParserText::new(
                                                first_sp, first_name,
                                            )),
                                        );
                                        let mut bindings = vec![first];
                                        for (m, (name, sp)) in rest {
                                            bindings.push((
                                                if m.is_some() {
                                                    VarType::Mutable
                                                } else {
                                                    VarType::Immutable
                                                },
                                                PotentialDollarIdentifier::Identifier(
                                                    ParserText::new(sp, name),
                                                ),
                                            ));
                                        }
                                        if bindings.len() == 1 {
                                            (Some(bindings.remove(0).1), None)
                                        } else {
                                            (
                                                None,
                                                Some(DestructurePattern::Tuple(
                                                    bindings.into_iter().map(Some).collect(),
                                                )),
                                            )
                                        }
                                    }),
                            )),
                        )
                        .or_not(),
                )
                .then(
                    lex(pad.clone(), just("if"))
                        .ignore_then(expr.clone())
                        .repeated(),
                )
                .then(match_arm_body.clone())
                .map(|((((first, rest), binds), conditions), body)| {
                        let mut values = vec![first];
                        values.extend(rest);
                        if let Some((vt, (enum_name, enum_destructure))) = binds {
                            for val in values.iter_mut() {
                                match val {
                                    MatchArmType::Enum {
                                        var_type,
                                        name,
                                        destructure,
                                        ..
                                    } => {
                                        *var_type = vt.clone();
                                        *name = enum_name.clone();
                                        *destructure = enum_destructure.clone();
                                    }
                                    MatchArmType::Value(Node {
                                        node_type: NodeType::Identifier(id),
                                        ..
                                    }) => {
                                        *val = MatchArmType::Enum {
                                            value: id.clone().into(),
                                            var_type: vt.clone(),
                                            name: enum_name.clone(),
                                            destructure: enum_destructure.clone(),
                                        };
                                    }
                                    _ => {}
                                }
                            }
                        }
                        let mut out = Vec::with_capacity(values.len());
                        for value in values {
                            out.push((value, conditions.clone(), Box::new(body.clone())));
                        }
                        out
                    })
                .boxed();

            let match_expr = lex(pad.clone(), just("match"))
                .ignore_then(
                    expr.clone()
                        .then_ignore(lex(pad.clone(), just('{')))
                        .map(Some)
                        .or(lex(pad.clone(), just('{')).to(None)),
                )
                .then_ignore(delim.clone().repeated())
                .then(
                    match_arm
                        .clone()
                        .separated_by(
                            lex(pad.clone(), just(','))
                                .then_ignore(delim.clone().repeated()),
                        )
                        .allow_trailing()
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
                .ignore_then(
                    choice((
                        labelled_scope.clone(),
                        scope.clone(),
                        choice((statement.clone(), expr.clone()))
                            .map(|body| scope_node(vec![body], true, false)),
                    )),
                )
                .boxed();

            let atom = choice((
                fat_scope_expr,
                match_expr,
                fn_match_expr,
                fn_expr,
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
                                                    args: args.into_iter().map(CallArg::Value).collect(),
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
                        .repeated(),
                    )
                    .map(|(((_open_ty, _open_br), values), tails)| {
                        let list = Node::new(
                            Span::default(),
                            NodeType::ListLiteral(_open_ty, values),
                        );
                        if tails.is_empty() {
                            list
                        } else {
                            let mut path = vec![(list, false)];
                            for tail in tails {
                                path.push(tail);
                            }
                            let sp =
                                Span::new_from_spans(path.first().unwrap().0.span, path.last().unwrap().0.span);
                            Node::new(sp, NodeType::MemberExpression { path })
                        }
                    }),
                float_lit.clone(),
                float_suffix_lit.clone(),
                int_lit.clone(),
                string_lit.clone(),
                char_lit.clone(),
                null_lit.clone(),
                dollar_ident
                    .clone()
                    .map(|id| {
                        let sp = *id.span();
                        Node::new(
                            sp,
                            NodeType::Identifier(
                                PotentialGenericTypeIdentifier::Identifier(id),
                            ),
                        )
                    }),
                lex(pad.clone(), just("$("))
                    .ignore_then(
                        expr.clone()
                            .separated_by(lex(pad.clone(), just(',')))
                            .allow_trailing()
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
                            .at_least(1),
                    )
                    .map(|(first, rest)| {
                        let mut path = vec![first];
                        path.extend(rest);
                        let sp = Span::new_from_spans(path.first().unwrap().span, path.last().unwrap().span);
                        Node::new(sp, NodeType::ScopeMemberExpression { path })
                    }),
                struct_lit,
                generic_ident.map(|identifier| Node::new(*identifier.span(), NodeType::Identifier(identifier))),
                lex(pad.clone(), just('('))
                    .ignore_then(
                        expr.clone()
                            .separated_by(lex(pad.clone(), just(',')))
                            .allow_trailing()
                            .or_not()
                            .map(|x| x.unwrap_or_default()),
                    )
                    .then_ignore(lex(pad.clone(), just(')')))
                    .map(|values| {
                        if values.len() == 1 {
                            values.into_iter().next().unwrap()
                        } else {
                            Node::new(Span::default(), NodeType::TupleLiteral { values })
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
                        .repeated(),
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
                                .repeated(),
                        )
                        .map(|(head, indexes)| {
                            if indexes.is_empty() {
                                head
                            } else {
                                let mut path = vec![(head, false)];
                                for idx in indexes {
                                    path.push((idx, true));
                                }
                                let sp = Span::new_from_spans(
                                    path.first().unwrap().0.span,
                                    path.last().unwrap().0.span,
                                );
                                Node::new(sp, NodeType::MemberExpression { path })
                            }
                        })
                        .separated_by(lex(pad.clone(), just(',')))
                        .allow_trailing()
                        .or_not()
                        .map(|x| x.unwrap_or_default()),
                )
                .then_ignore(lex(pad.clone(), just(')')))
                .map(|args| args.into_iter().map(CallArg::Value).collect::<Vec<_>>())
                .boxed();

            let reverse_args = lex(pad.clone(), choice((just("<("), just("$("))))
                .ignore_then(
                    expr.clone()
                        .then(
                            lex(pad.clone(), just('['))
                                .ignore_then(expr.clone())
                                .then_ignore(lex(pad.clone(), just(']')))
                                .repeated(),
                        )
                        .map(|(head, indexes)| {
                            if indexes.is_empty() {
                                head
                            } else {
                                let mut path = vec![(head, false)];
                                for idx in indexes {
                                    path.push((idx, true));
                                }
                                let sp = Span::new_from_spans(
                                    path.first().unwrap().0.span,
                                    path.last().unwrap().0.span,
                                );
                                Node::new(sp, NodeType::MemberExpression { path })
                            }
                        })
                        .separated_by(lex(pad.clone(), just(',')))
                        .allow_trailing()
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
                    .then(call_args.clone().repeated())
                    .map(|(m, calls)| {
                        let node = calls.into_iter().fold(m, |c, args| Node::new(c.span, NodeType::CallExpression { string_fn: None, caller: Box::new(c), generic_types: Vec::new(), args, reverse_args: Vec::new() }));
                        (node, false)
                    }),
                lex(pad.clone(), just('[')).ignore_then(expr.clone()).then_ignore(lex(pad.clone(), just(']'))).map(|e| (e, true)),
            ))
            .boxed();

            let postfix = atom
                .then(choice((
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
                )).repeated())
                .map(|(head, calls)| {
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
                })
                .then(member.clone().repeated())
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
                        let sp = Span::new_from_spans(path.first().unwrap().0.span, path.last().unwrap().0.span);
                        Node::new(sp, NodeType::MemberExpression { path })
                    }
                })
                .boxed();

            let enum_expr_value = expr.clone();
            let enum_variant = postfix
                .clone()
                .then_with({
                    let pad = pad.clone();
                    let enum_expr_value = enum_expr_value.clone();
                    move |lhs| {
                        let is_enum_candidate = matches!(
                            &lhs.node_type,
                            NodeType::MemberExpression { path }
                                if path.len() == 2 && !path[0].1 && !path[1].1
                        );
                        if is_enum_candidate {
                            lex(pad.clone(), just(':'))
                                .ignore_then(choice((
                                    lex(pad.clone(), just('('))
                                        .ignore_then(
                                            enum_expr_value.clone()
                                                .separated_by(lex(pad.clone(), just(',')))
                                                .at_least(1),
                                        )
                                        .then_ignore(lex(pad.clone(), just(')')))
                                        .map(|values| {
                                            Node::new(Span::default(), NodeType::TupleLiteral { values })
                                        }),
                                    enum_expr_value.clone(),
                                )))
                                .or_not()
                                .map(move |data| (lhs.clone(), data))
                                .boxed()
                        } else {
                            empty().to((lhs, None)).boxed()
                        }
                    }
                })
                .map(|(lhs, data)| {
                    if let Some(data) = data {
                        if let NodeType::MemberExpression { path } = &lhs.node_type {
                            if path.len() == 2 && !path[0].1 && !path[1].1 {
                                if let (NodeType::Identifier(identifier), NodeType::Identifier(value)) =
                                    (&path[0].0.node_type, &path[1].0.node_type)
                                {
                                    return Node::new(
                                        lhs.span,
                                        NodeType::EnumExpression {
                                            identifier: identifier.clone(),
                                            value: value.clone().into(),
                                            data: Some(Box::new(data)),
                                        },
                                    );
                                }
                            }
                        }
                    }
                    lhs
                })
                .boxed();

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
                        .or_not()
                        .map(|x| x.unwrap_or_default()),
                )
                .then_ignore(lex(pad.clone(), just(']')))
                .then(member.clone().repeated())
                .map(|(((_open_ty, _open_br), values), tails)| {
                    let data_type = _open_ty;
                    let list = Node::new(
                        Span::default(),
                        NodeType::ListLiteral(data_type, values),
                    );
                    if tails.is_empty() {
                        list
                    } else {
                        let mut path = vec![(list, false)];
                        path.extend(tails);
                        let sp = Span::new_from_spans(path.first().unwrap().0.span, path.last().unwrap().0.span);
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
                fat_arrow
                    .clone()
                    .ignore_then(
                        lex(pad.clone(), just('{'))
                            .rewind()
                            .to(true)
                            .or_not()
                            .map(|x| x.unwrap_or(false))
                            .then(choice((scope.clone(), statement.clone(), expr.clone())))
                            .try_map(|(has_brace, body), sp| {
                                if has_brace && !matches!(body.node_type, NodeType::ScopeDeclaration { .. }) {
                                    Err(Simple::custom(sp, "expected scope body"))
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
                .then(lex(pad.clone(), just("spawn")).to(true).or_not().map(|x| x.unwrap_or(false)))
                .then_ignore(lex(pad.clone(), just("for")))
                .then(iter_loop_type)
                .then(
                    lex(pad.clone(), just("if"))
                        .ignore_then(expr.clone())
                        .repeated(),
                )
                .then(
                    lex(pad.clone(), just("until"))
                        .ignore_then(expr.clone())
                        .or_not(),
                )
                .then_ignore(lex(pad.clone(), just(']')))
                .map(|((((((open_ty, _open_br), map), spawned), loop_type), conditionals), until)| {
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
                })
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

            let unary = choice((lex(pad.clone(), just('-')).to(BinaryOperator::Sub), lex(pad.clone(), just('!')).to(BinaryOperator::BitXor)))
                .repeated()
                .then(prefixed)
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

            let product = unary.clone().then(mul_op.then(unary).repeated()).map(|(h, t)| {
                t.into_iter().fold(h, |l, (op, r)| Node::new(Span::new_from_spans(l.span, r.span), NodeType::BinaryExpression { left: Box::new(l), right: Box::new(r), operator: op }))
            }).boxed();

            let sum = product.clone().then(add_op.then(product).repeated()).map(|(h, t)| {
                t.into_iter().fold(h, |l, (op, r)| Node::new(Span::new_from_spans(l.span, r.span), NodeType::BinaryExpression { left: Box::new(l), right: Box::new(r), operator: op }))
            }).boxed();

            let bit_op = choice((
                lex(pad.clone(), just("<<")).to(BinaryOperator::Shl),
                lex(pad.clone(), just(">>")).to(BinaryOperator::Shr),
                lex(pad.clone(), just('&')).to(BinaryOperator::BitAnd),
                lex(pad.clone(), just('|')).to(BinaryOperator::BitOr),
                lex(pad.clone(), just('^')).to(BinaryOperator::BitXor),
            ))
            .boxed();

            let bitwise = sum.clone().then(bit_op.then(sum.clone()).repeated()).map(|(h, t)| {
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
            }).boxed();

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
                .then(cmp_op.then(bitwise.clone()).repeated())
                .map(|(h, t)| {
                t.into_iter().fold(h, |l, (op, r)| Node::new(Span::new_from_spans(l.span, r.span), NodeType::ComparisonExpression { left: Box::new(l), right: Box::new(r), operator: op }))
            }).boxed();

            let bool_op = choice((
                lex(pad.clone(), just("&&")).to(BooleanOperator::And),
                lex(pad.clone(), just("||")).to(BooleanOperator::Or),
            ))
            .boxed();

            let boolean = compared
                .clone()
                .then(bool_op.then(compared.clone()).repeated())
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
                .then(lex(pad.clone(), just("as")).ignore_then(type_name.clone()).repeated())
                .map(|(head, casts)| {
                    casts.into_iter().fold(head, |value, data_type| {
                        Node::new(
                            value.span,
                            NodeType::AsExpression {
                                value: Box::new(value),
                                data_type,
                            },
                        )
                    })
                })
                .boxed();

            let in_expr = as_expr
                .clone()
                .then(lex(pad.clone(), just("in")).ignore_then(as_expr.clone()).repeated())
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

            let ternary_expr = in_expr
                .clone()
                .then(
                    lex(pad.clone(), just('?'))
                        .ignore_then(choice((assign_expr.clone(), expr.clone())))
                        .then_ignore(lex(pad.clone(), just(':')))
                        .then(choice((assign_expr.clone(), expr.clone())))
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
                .boxed();

            let pipe_seg = choice((
                lex(pad.clone(), just("|>"))
                    .ignore_then(ternary_expr.clone())
                    .map(PipeSegment::Unnamed),
                lex(pad.clone(), just("|:"))
                    .ignore_then(named_ident.clone())
                    .then_ignore(lex(pad.clone(), just('>')))
                    .then(ternary_expr.clone())
                    .map(|(identifier, node)| PipeSegment::Named { identifier, node }),
            ))
            .boxed();

            let pipe_expr = ternary_expr
                .clone()
                .then(pipe_seg.repeated())
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

            let try_expr = lex(pad.clone(), just("try"))
                .ignore_then(expr.clone())
                .then(
                    choice((
                        lex(pad.clone(), just(':'))
                            .ignore_then(ident.clone())
                            .then_ignore(fat_arrow.clone())
                            .then(choice((scope.clone(), statement.clone(), expr.clone())))
                            .map(|((name, sp), body)| {
                                let body = match body.node_type {
                                    NodeType::ScopeDeclaration { .. } => body,
                                    _ => scope_node(vec![body], true, false),
                                };
                                TryCatch {
                                    name: Some(PotentialDollarIdentifier::Identifier(
                                        ParserText::new(sp, name),
                                    )),
                                    body: Box::new(body),
                                }
                            }),
                        fat_arrow
                            .clone()
                            .ignore_then(choice((scope.clone(), statement.clone(), expr.clone())))
                            .map(|body| {
                                let body = match body.node_type {
                                    NodeType::ScopeDeclaration { .. } => body,
                                    _ => scope_node(vec![body], true, false),
                                };
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
                .map(|label| {
                    Node::new(
                        Span::default(),
                        NodeType::Continue { label },
                    )
                })
                .boxed();

            let spawn_expr = lex(pad.clone(), just("spawn"))
                .ignore_then(
                    choice((
                        lex(pad.clone(), just('{'))
                            .ignore_then(delim.clone().repeated())
                            .ignore_then(
                                fat_arrow
                                    .clone()
                                    .ignore_then(
                                        scope.clone().or(
                                            choice((statement.clone(), expr.clone()))
                                                .map(|n| scope_node(vec![n], true, false)),
                                        ),
                                    )
                                    .or(expr.clone())
                                    .separated_by(
                                        lex(pad.clone(), just(','))
                                            .then_ignore(delim.clone().repeated())
                                            .ignored(),
                                    )
                                    .allow_trailing()
                                    .or_not()
                                    .map(|x| x.unwrap_or_default()),
                            )
                            .then_ignore(delim.clone().or_not())
                            .then_ignore(lex(pad.clone(), just('}')))
                            .map(|items| Node::new(Span::default(), NodeType::Spawn { items })),
                        lex(pad.clone(), just("for"))
                            .ignore_then(
                                ident
                                    .clone()
                                    .then_ignore(lex(pad.clone(), just("in")))
                                    .then(expr.clone())
                                    .map(|((n, sp), iter)| {
                                        LoopType::For(
                                            PotentialDollarIdentifier::Identifier(
                                                ParserText::new(sp, n),
                                            ),
                                            iter,
                                        )
                                    })
                                    .or(expr.clone().map(LoopType::While)),
                            )
                            .then_ignore(fat_arrow.clone())
                            .then_with({
                                let scope_body = scope_node_parser(statement.clone(), delim.clone(), pad.clone()).boxed();
                                let short_body = choice((labelled_scope.clone(), statement.clone(), expr.clone())).boxed();
                                let brace_peek = lex(pad.clone(), just('{'))
                                    .rewind()
                                    .to(true)
                                    .or_not()
                                    .map(|x| x.unwrap_or(false))
                                    .boxed();
                                move |lt| {
                                    brace_peek
                                        .clone()
                                        .then_with({
                                            let scope_body = scope_body.clone();
                                            let short_body = short_body.clone();
                                            move |has_brace| {
                                                if has_brace {
                                                    scope_body.clone()
                                                } else {
                                                    short_body.clone()
                                                }
                                            }
                                        })
                                        .map(move |body| (lt.clone(), body))
                                        .boxed()
                                }
                            })
                            .map(|(lt, body)| {
                                let body = match body.node_type {
                                    NodeType::ScopeDeclaration { .. } => body,
                                    _ => scope_node(vec![body], true, false),
                                };
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
                    )),
                )
                .map(|item| {
                    if matches!(item.node_type, NodeType::Spawn { .. }) {
                        item
                    } else {
                        Node::new(
                            item.span,
                            NodeType::Spawn {
                                items: vec![item],
                            },
                        )
                    }
                })
                .boxed();

            let for_expr = lex(pad.clone(), just("for"))
                .ignore_then(
                    fat_arrow
                        .clone()
                        .rewind()
                        .to(LoopType::Loop)
                        .or(
                    lex(pad.clone(), just("let"))
                        .ignore_then(
                            lex(pad.clone(), just('.'))
                                .ignore_then(ident.clone())
                                .then(lex(pad.clone(), just(':')).ignore_then(ident.clone()).or_not())
                                .map(|((variant, vsp), name)| {
                                    let value = PotentialDollarIdentifier::Identifier(
                                        ParserText::new(vsp, variant),
                                    );
                                    let name = name.map(|(n, nsp)| {
                                        PotentialDollarIdentifier::Identifier(ParserText::new(
                                            nsp, n,
                                        ))
                                    });
                                    MatchArmType::Enum {
                                        value,
                                        var_type: VarType::Immutable,
                                        name,
                                        destructure: None,
                                    }
                                })
                                .then(
                                    lex(pad.clone(), just('|'))
                                        .ignore_then(
                                            lex(pad.clone(), just('.'))
                                                .ignore_then(ident.clone())
                                                .then(
                                                    lex(pad.clone(), just(':'))
                                                        .ignore_then(ident.clone())
                                                        .or_not(),
                                                )
                                                .map(|((variant, vsp), name)| {
                                                    let value = PotentialDollarIdentifier::Identifier(
                                                        ParserText::new(vsp, variant),
                                                    );
                                                    let name = name.map(|(n, nsp)| {
                                                        PotentialDollarIdentifier::Identifier(
                                                            ParserText::new(nsp, n),
                                                        )
                                                    });
                                                    MatchArmType::Enum {
                                                        value,
                                                        var_type: VarType::Immutable,
                                                        name,
                                                        destructure: None,
                                                    }
                                                }),
                                        )
                                        .repeated(),
                                )
                                .map(|(first, rest)| {
                                    let mut all = vec![first];
                                    all.extend(rest);
                                    all
                                }),
                        )
                        .then_ignore(left_arrow.clone())
                        .then(expr.clone())
                        .map(|(values, value)| LoopType::Let {
                            value,
                            pattern: (values, Vec::new()),
                        })
                        .or(
                    ident
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
                        .map(|x| x.unwrap_or(LoopType::Loop)),
                        ),
                    ),
                )
                .then_ignore(fat_arrow.clone())
                .then(choice((scope.clone(), labelled_scope.clone(), statement.clone(), expr.clone())))
                .then(
                    lex(pad.clone(), just("else"))
                        .ignore_then(fat_arrow.clone())
                        .ignore_then(
                            lex(pad.clone(), just('{'))
                                .rewind()
                                .to(true)
                                .or_not()
                                .map(|x| x.unwrap_or(false))
                                .then_with({
                                    let scope_body = scope.clone().boxed();
                                    let short_body = choice((labelled_scope.clone(), statement.clone(), expr.clone())).boxed();
                                    move |has_brace| {
                                        if has_brace {
                                            scope_body.clone()
                                        } else {
                                            short_body.clone()
                                        }
                                    }
                                }),
                        )
                        .or_not(),
                )
                .then(
                    lex(pad.clone(), just("until"))
                        .ignore_then(expr.clone())
                        .or_not(),
                )
                .map(|(((loop_type, body), else_body), until)| {
                    let body = match body.node_type {
                        NodeType::ScopeDeclaration { .. } => body,
                        _ => scope_node(vec![body], true, false),
                    };
                    let else_body = else_body.map(|body| {
                        Box::new(match body.node_type {
                            NodeType::ScopeDeclaration { .. } => body,
                            _ => scope_node(vec![body], true, false),
                        })
                    });
                    Node::new(
                        body.span,
                        NodeType::LoopDeclaration {
                            loop_type: Box::new(loop_type),
                            body: Box::new(body),
                            until: until.map(Box::new),
                            label: None,
                            else_body,
                        },
                    )
                })
                .then_ignore(lex(pad.clone(), just(';')).or_not())
                .boxed();

            let if_expr = recursive(|if_e| {
                let if_let_pattern = lex(pad.clone(), just('.'))
                    .ignore_then(ident.clone())
                    .then(lex(pad.clone(), just(':')).ignore_then(ident.clone()).or_not())
                    .map(|((variant, vsp), name)| {
                        let value =
                            PotentialDollarIdentifier::Identifier(ParserText::new(vsp, variant));
                        let name = name
                            .map(|(n, nsp)| {
                                PotentialDollarIdentifier::Identifier(ParserText::new(nsp, n))
                            });

                        MatchArmType::Enum {
                            value,
                            var_type: VarType::Immutable,
                            name,
                            destructure: None,
                        }
                    })
                    .boxed();

                let if_let_cond = lex(pad.clone(), just("let"))
                    .ignore_then(
                        if_let_pattern
                            .clone()
                            .then(
                                lex(pad.clone(), just('|'))
                                    .ignore_then(if_let_pattern)
                                    .repeated(),
                            )
                            .map(|(first, rest)| {
                                let mut all = vec![first];
                                all.extend(rest);
                                all
                            }),
                    )
                    .then_ignore(left_arrow.clone())
                    .then(expr.clone())
                    .map(|(values, value)| IfComparisonType::IfLet {
                        value,
                        pattern: (values, Vec::new()),
                    })
                    .boxed();

                lex(pad.clone(), just("if"))
                    .ignore_then(
                        choice((
                            if_let_cond,
                            expr.clone().map(IfComparisonType::If),
                        )),
                    )
                    .then_ignore(fat_arrow.clone())
                    .then(choice((scope_node_parser(statement.clone(), delim.clone(), pad.clone()), statement.clone(), expr.clone())))
                    .then(
                        lex(pad.clone(), just("else"))
                            .ignore_then(if_e.clone().or(choice((scope_node_parser(statement.clone(), delim.clone(), pad.clone()), statement.clone(), expr.clone()))))
                            .or_not(),
                    )
                    .map(|((cond, then_b), otherwise)| {
                        let then_node = match then_b.node_type {
                            NodeType::ScopeDeclaration { .. } => then_b,
                            _ => scope_node(vec![then_b], true, false),
                        };
                        let cond_span = match &cond {
                            IfComparisonType::If(node) => node.span,
                            IfComparisonType::IfLet { value, .. } => value.span,
                        };
                        Node::new(
                            Span::new_from_spans(cond_span, otherwise.as_ref().map(|x| x.span).unwrap_or(then_node.span)),
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
            .ignore_then(
                choice((
                    choice((
                        lex(pad.clone(), just('('))
                            .ignore_then(ident.clone().map(|(n, sp)| PotentialDollarIdentifier::Identifier(ParserText::new(sp, n))).separated_by(lex(pad.clone(), just(','))).allow_trailing())
                            .then_ignore(lex(pad.clone(), just(')'))),
                        lex(pad.clone(), just('*')).map(|_| vec![PotentialDollarIdentifier::Identifier(ParserText::from("*".to_string()))]),
                    ))
                    .then_ignore(lex(pad.clone(), just("from")))
                    .then(
                        ident
                            .clone()
                            .map(|(n, sp)| PotentialDollarIdentifier::Identifier(ParserText::new(sp, n)))
                            .separated_by(lex(pad.clone(), just("::")))
                            .at_least(1),
                    ),
                    ident
                        .clone()
                        .map(|(n, sp)| PotentialDollarIdentifier::Identifier(ParserText::new(sp, n)))
                        .separated_by(lex(pad.clone(), just("::")))
                        .at_least(1)
                        .map(|module| {
                            (
                                vec![PotentialDollarIdentifier::Identifier(ParserText::from(
                                    "*".to_string(),
                                ))],
                                module,
                            )
                        }),
                )),
            )
            .map(|(values, module)| {
                Node::new(
                    Span::default(),
                    NodeType::ImportStatement {
                        module,
                        alias: None,
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
                            .or_not()
                            .map(|x| x.unwrap_or_default()),
                    )
                    .then_ignore(lex(pad.clone(), just('>')))
                    .or_not(),
            )
            .then_ignore(lex(pad.clone(), just('=')))
            .then(
                choice((
                    lex(pad.clone(), just("struct"))
                        .ignore_then(
                            lex(pad.clone(), just('{'))
                                .then_ignore(delim.clone().repeated())
                                .ignore_then(
                                    ident
                                        .clone()
                                        .repeated()
                                        .at_least(1)
                                        .then_ignore(lex(pad.clone(), just(':')))
                                        .then(type_name.clone())
                                        .separated_by(
                                            choice((
                                                lex(pad.clone(), just(',')).ignored(),
                                                delim.clone(),
                                            ))
                                            .repeated()
                                            .at_least(1)
                                            .ignored(),
                                        )
                                        .allow_trailing(),
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
                                .or(
                                    lex(pad.clone(), just('('))
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
                                                .or_not()
                                                .map(|x| x.unwrap_or_default()),
                                        )
                                        .then_ignore(lex(pad.clone(), just(')')))
                                        .map(|types| TypeDefType::Struct(ObjectType::Tuple(types))),
                                ),
                        ),
                    lex(pad.clone(), just("enum"))
                        .ignore_then(lex(pad.clone(), just('{')))
                        .then_ignore(delim.clone().repeated())
                        .ignore_then(
                            ident
                                .clone()
                                .repeated()
                                .at_least(1)
                                .then(lex(pad.clone(), just(':')).ignore_then(type_name.clone()).or_not())
                                .map(|(names, t)| {
                                    names
                                        .into_iter()
                                        .map(|(n, sp)| {
                                            (
                                                PotentialDollarIdentifier::Identifier(
                                                    ParserText::new(sp, n),
                                                ),
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
                                .allow_trailing(),
                        )
                        .then_ignore(delim.clone().or_not())
                        .then_ignore(lex(pad.clone(), just('}')))
                        .map(|groups| {
                            TypeDefType::Enum(groups.into_iter().flatten().collect())
                        }),
                    type_name
                        .clone()
                        .map(|typ| TypeDefType::NewType(Box::new(typ))),
                )),
            )
            .then(
                lex(pad.clone(), just("@overload"))
                    .ignore_then(lex(pad.clone(), just('{')))
                    .then_ignore(delim.clone().repeated())
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
                                _ => Err(Simple::custom(sp, "expected function declaration")),
                            })
                            .separated_by(delim.clone())
                            .allow_trailing()
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
                        identifier: PotentialDollarIdentifier::Identifier(ParserText::new(sp, name)),
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
                .then(lex(pad.clone(), just(':')).ignore_then(type_name.clone()).or_not())
                .then(lex(pad.clone(), just('=')).ignore_then(statement.clone()).or_not())
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
            .then_ignore(delim.clone().repeated())
            .then(
                trait_member
                    .separated_by(delim.clone())
                    .allow_trailing()
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
            .ignore_then(
                lex(pad.clone(), just('<'))
                    .ignore_then(
                        ident
                            .clone()
                            .separated_by(lex(pad.clone(), just(',')))
                            .allow_trailing()
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
                                    identifier: PotentialDollarIdentifier::Identifier(
                                        ParserText::new(sp, n),
                                    ),
                                    trait_constraints: Vec::new(),
                                })
                                .collect(),
                        )
                    }),
            )
            .then(type_name.clone())
            .then(lex(pad.clone(), just("for")).ignore_then(type_name.clone()).or_not())
            .then_ignore(lex(pad.clone(), just('{')))
            .then_ignore(delim.clone().repeated())
            .then(
                statement
                    .clone()
                    .separated_by(delim.clone())
                    .allow_trailing()
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
                            PotentialDollarIdentifier::Identifier(ParserText::from("unknown".to_string())),
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
                .at_least(2),
            )
            .then_ignore(lex(pad.clone(), just('=')))
            .then(expr.clone())
            .map(|(items, value)| {
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

        let let_stmt = choice((lex(pad.clone(), just("let")).to(VarType::Immutable), lex(pad.clone(), just("const")).to(VarType::Constant)))
            .then(lex(pad.clone(), just("mut")).or_not())
            .then(named_ident.clone())
            .then(lex(pad.clone(), just(':')).ignore_then(type_name.clone()).or_not())
            .then_ignore(lex(pad.clone(), just('=')))
            .then(
                expr.clone()
                    .then(
                        lex(pad.clone(), just(','))
                            .ignore_then(expr.clone())
                            .repeated(),
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
                    let mut values = vec![first_value];
                    values.extend(rest_values);
                    Node::new(Span::default(), NodeType::TupleLiteral { values })
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
                    .map(|(n, sp)| {
                        PotentialDollarIdentifier::Identifier(ParserText::new(sp, n))
                    })
                    .then_ignore(lex(pad.clone(), just('=')))
                    .then(
                        lex(pad.clone(), just("type"))
                            .ignore_then(lex(pad.clone(), just(':')))
                            .ignore_then(type_name.clone())
                            .map(|data_type| {
                                Node::new(
                                    *data_type.span(),
                                    NodeType::DataType { data_type },
                                )
                            })
                            .or(expr.clone()),
                    )
                    .separated_by(lex(pad.clone(), just(',')))
                    .allow_trailing()
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
                .ignore_then(delim.clone().repeated())
                .ignore_then(
                    statement
                        .clone()
                        .separated_by(delim.clone())
                        .allow_trailing()
                        .or_not()
                        .map(|x| x.unwrap_or_default()),
                )
                .then_ignore(delim.clone().or_not())
                .then_ignore(lex(pad.clone(), just("}}")))
                .map(|items| (Some(items), Some(false))),
            scope_node_parser(statement.clone(), delim.clone(), pad.clone()).map(|body| {
                let body = match body.node_type {
                    NodeType::ScopeDeclaration { body, .. } => body,
                    _ => Some(vec![body]),
                };
                (body, Some(true))
            }),
            choice((statement.clone(), expr.clone())).map(|body| (Some(vec![body]), Some(false))),
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
            .map(|value| Node::new(Span::default(), NodeType::Return { value: value.map(Box::new) }));

        let labelled_scope_stmt = lex(pad.clone(), just('@'))
            .ignore_then(ident.clone())
            .then(lex(pad.clone(), just("[]")).or_not())
            .ignore_then(scope_node_parser(statement.clone(), delim.clone(), pad.clone()))
            .boxed();

        let for_stmt = lex(pad.clone(), just("for"))
            .ignore_then(
                fat_arrow
                    .clone()
                    .rewind()
                    .to(LoopType::Loop)
                    .or(
                lex(pad.clone(), just("let"))
                    .ignore_then(
                        lex(pad.clone(), just('.'))
                            .ignore_then(ident.clone())
                            .then(lex(pad.clone(), just(':')).ignore_then(ident.clone()).or_not())
                            .map(|((variant, vsp), name)| {
                                let value = PotentialDollarIdentifier::Identifier(ParserText::new(
                                    vsp, variant,
                                ));
                                let name = name.map(|(n, nsp)| {
                                    PotentialDollarIdentifier::Identifier(ParserText::new(nsp, n))
                                });
                                MatchArmType::Enum {
                                    value,
                                    var_type: VarType::Immutable,
                                    name,
                                    destructure: None,
                                }
                            })
                            .then(
                                lex(pad.clone(), just('|'))
                                    .ignore_then(
                                        lex(pad.clone(), just('.'))
                                            .ignore_then(ident.clone())
                                            .then(
                                                lex(pad.clone(), just(':'))
                                                    .ignore_then(ident.clone())
                                                    .or_not(),
                                            )
                                            .map(|((variant, vsp), name)| {
                                                let value = PotentialDollarIdentifier::Identifier(
                                                    ParserText::new(vsp, variant),
                                                );
                                                let name = name.map(|(n, nsp)| {
                                                    PotentialDollarIdentifier::Identifier(
                                                        ParserText::new(nsp, n),
                                                    )
                                                });
                                                MatchArmType::Enum {
                                                    value,
                                                    var_type: VarType::Immutable,
                                                    name,
                                                    destructure: None,
                                                }
                                            }),
                                    )
                                    .repeated(),
                            )
                            .map(|(first, rest)| {
                                let mut all = vec![first];
                                all.extend(rest);
                                all
                            }),
                    )
                    .then_ignore(left_arrow.clone())
                    .then(expr.clone())
                    .map(|(values, value)| LoopType::Let {
                        value,
                        pattern: (values, Vec::new()),
                    })
                    .or(
                ident
                    .clone()
                    .then_ignore(lex(pad.clone(), just("in")))
                    .then(expr.clone())
                    .map(|((n, sp), iter)| LoopType::For(PotentialDollarIdentifier::Identifier(ParserText::new(sp, n)), iter))
                    .or(expr.clone().map(LoopType::While))
                    .or_not()
                    .map(|x| x.unwrap_or(LoopType::Loop)),
                    ),
                    ),
            )
            .then_ignore(fat_arrow.clone())
            .then(choice((
                    scope_node_parser(statement.clone(), delim.clone(), pad.clone()),
                    labelled_scope_stmt.clone(),
                    statement.clone(),
                    expr.clone(),
                )))
            .then(
                lex(pad.clone(), just("else"))
                    .ignore_then(fat_arrow.clone())
                    .ignore_then(
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
                                if has_brace && !matches!(body.node_type, NodeType::ScopeDeclaration { .. }) {
                                    Err(Simple::custom(sp, "expected scope body"))
                                } else {
                                    Ok(body)
                                }
                            }),
                    )
                    .or_not(),
            )
            .then(
                lex(pad.clone(), just("until"))
                    .ignore_then(expr.clone())
                    .or_not(),
            )
            .map(|(((lt, body), else_body), until)| {
                let body = match body.node_type {
                    NodeType::ScopeDeclaration { .. } => body,
                    _ => scope_node(vec![body], true, false),
                };
                let else_body = else_body.map(|body| {
                    Box::new(match body.node_type {
                        NodeType::ScopeDeclaration { .. } => body,
                        _ => scope_node(vec![body], true, false),
                    })
                });
                Node::new(
                    body.span,
                    NodeType::LoopDeclaration {
                        loop_type: Box::new(lt),
                        body: Box::new(body),
                        until: until.map(Box::new),
                        label: None,
                        else_body,
                    },
                )
            })
            .then_ignore(lex(pad.clone(), just(';')).or_not())
            .boxed();

        let ffi_type = recursive(|ffi_type| {
            let ffi_base = lex(pad.clone(), just('@'))
                .ignore_then(raw_ident.clone())
                .map(|(name, sp)| {
                    let parsed = ParserFfiInnerType::from_str(&name)
                        .unwrap_or(ParserFfiInnerType::Int);
                    PotentialFfiDataType::Ffi(ParserFfiDataType::new(sp, parsed))
                })
                .or(type_name.clone().map(|x| match x {
                    PotentialNewType::DataType(dt) => PotentialFfiDataType::Normal(dt),
                    _ => PotentialFfiDataType::Normal(ParserDataType::new(
                        Span::default(),
                        ParserInnerType::Auto(None),
                    )),
                }))
                .boxed();

            lex(pad.clone(), just("ptr"))
                .ignore_then(lex(pad.clone(), just(":<")))
                .ignore_then(ffi_type.clone())
                .then_ignore(lex(pad.clone(), just('>')))
                .map(|inner| {
                    let inner = match inner {
                        PotentialFfiDataType::Normal(x) => x,
                        PotentialFfiDataType::Ffi(x) => ParserDataType::new(x.span, x.data_type.into()),
                    };
                    PotentialFfiDataType::Normal(ParserDataType::new(
                        inner.span,
                        ParserInnerType::Ptr(Box::new(inner)),
                    ))
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
                    .or_not()
                    .map(|x| x.unwrap_or_default()),
            )
            .then_ignore(lex(pad.clone(), just(')')))
            .then(
                arrow
                    .clone()
                    .ignore_then(ffi_type.clone())
                    .or_not()
                    .map(|x| {
                        x.unwrap_or_else(|| {
                            PotentialFfiDataType::Normal(ParserDataType::new(
                                Span::default(),
                                ParserInnerType::Null,
                            ))
                        })
                    }),
            )
            .then_ignore(lex(pad.clone(), just("from")))
            .then(string_text.clone())
            .then(
                lex(pad.clone(), just("as"))
                    .ignore_then(string_text.clone())
                    .or_not(),
            )
            .map(move |(((((abi, (name, sp)), parameters), return_type), mut library), symbol)| {
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
            })
            .boxed();

        let _select_arm = choice((
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
                            if has_brace && !matches!(body.node_type, NodeType::ScopeDeclaration { .. }) {
                                Err(Simple::custom(sp, "expected scope body"))
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
                            if has_brace && !matches!(body.node_type, NodeType::ScopeDeclaration { .. }) {
                                Err(Simple::custom(sp, "expected scope body"))
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
            .then(take_until(lex(pad.clone(), just("};"))))
            .map(|_| Node::new(Span::default(), NodeType::SelectStatement { arms: Vec::new() }))
            .boxed();

        let use_stmt = lex(pad.clone(), just("use"))
            .ignore_then(
                ident
                    .clone()
                    .map(|(n, sp)| PotentialDollarIdentifier::Identifier(ParserText::new(sp, n)))
                    .separated_by(lex(pad.clone(), just(',')))
                    .at_least(1)
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

        let balanced_block_inner = recursive(|inner| {
            choice((
                filter(|c: &char| *c != '{' && *c != '}')
                    .repeated()
                    .at_least(1)
                    .collect::<String>(),
                just('{')
                    .ignore_then(inner.clone())
                    .then_ignore(just('}'))
                    .map(|s| format!("{{{s}}}")),
            ))
            .repeated()
            .collect::<Vec<_>>()
            .map(|parts| parts.concat())
        })
        .boxed();

        let spawn_item = fat_arrow
            .clone()
            .ignore_then(
                lex(pad.clone(), just('{'))
                    .ignore_then(balanced_block_inner.clone())
                    .then_ignore(lex(pad.clone(), just('}')))
                    .map(|txt| parse_block_scope(&txt))
                    .or(scope_node_parser(statement.clone(), delim.clone(), pad.clone()).or(
                        choice((statement.clone(), expr.clone()))
                            .map(|n| scope_node(vec![n], true, false)),
                    )),
            )
            .boxed();

        let spawn_block = lex(pad.clone(), just('{'))
            .ignore_then(delim.clone().repeated())
            .ignore_then(
                spawn_item
                    .clone()
                    .separated_by(
                        lex(pad.clone(), just(','))
                            .then_ignore(delim.clone().repeated())
                            .ignored(),
                    )
                    .allow_trailing()
                    .or_not()
                    .map(|x| x.unwrap_or_default()),
            )
            .then_ignore(delim.clone().or_not())
            .then_ignore(lex(pad.clone(), just('}')))
            .boxed();

        let spawn_stmt = lex(pad.clone(), just("spawn"))
            .ignore_then(choice((spawn_block, for_stmt.clone().map(|x| vec![x]), expr.clone().map(|x| vec![x]))))
            .map(|items| {
                let sp = if let (Some(a), Some(b)) = (items.first(), items.last()) {
                    Span::new_from_spans(a.span, b.span)
                } else {
                    Span::default()
                };
                Node::new(sp, NodeType::Spawn { items })
            })
            .boxed();

        let assign_lhs = ident
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
                .repeated(),
            )
            .map(|(head, rest)| {
                if rest.is_empty() {
                    head
                } else {
                    let mut path = vec![(head, false)];
                    path.extend(rest);
                    let sp = Span::new_from_spans(path.first().unwrap().0.span, path.last().unwrap().0.span);
                    Node::new(sp, NodeType::MemberExpression { path })
                }
            })
            .boxed();

        let assign_stmt = assign_lhs
            .then(
                choice((
                    lex(pad.clone(), just('='))
                        .to(None),
                    lex(pad.clone(), just("+="))
                        .to(Some(BinaryOperator::Add)),
                    lex(pad.clone(), just("-="))
                        .to(Some(BinaryOperator::Sub)),
                    lex(pad.clone(), just("*="))
                        .to(Some(BinaryOperator::Mul)),
                    lex(pad.clone(), just("/="))
                        .to(Some(BinaryOperator::Div)),
                    lex(pad.clone(), just("%="))
                        .to(Some(BinaryOperator::Mod)),
                    lex(pad.clone(), just("&="))
                        .to(Some(BinaryOperator::BitAnd)),
                    lex(pad.clone(), just("|="))
                        .to(Some(BinaryOperator::BitOr)),
                    lex(pad.clone(), just("^="))
                        .to(Some(BinaryOperator::BitXor)),
                    lex(pad.clone(), just("<<="))
                        .to(Some(BinaryOperator::Shl)),
                    lex(pad.clone(), just(">>="))
                        .to(Some(BinaryOperator::Shr)),
                )),
            )
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
                    .at_least(1),
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
            for_stmt,
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
        .then_ignore(delim.clone().repeated())
        .ignore_then(
            parser
                .separated_by(delim.clone())
                .allow_trailing()
                .or_not()
                .map(|x| x.unwrap_or_default()),
        )
        .then_ignore(pad.clone())
        .then_ignore(delim.clone().repeated())
        .then_ignore(end())
        .parse(source);

    match parsed {
        Ok(items) => {
            let sp = if let (Some(a), Some(b)) = (items.first(), items.last()) {
                Span::new_from_spans(a.span, b.span)
            } else {
                Span::default()
            };
            Ok(Node::new(
                sp,
                NodeType::ScopeDeclaration {
                    body: Some(items),
                    named: None,
                    is_temp: false,
                    create_new_scope: Some(false),
                    define: false,
                },
            ))
        }
        Err(errs) => Err(to_parser_errors(line_starts.as_ref(), errs)),
    }
}
