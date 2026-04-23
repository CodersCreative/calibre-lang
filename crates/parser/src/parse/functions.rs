use chumsky::prelude::*;
use std::sync::Arc;

use super::{LegacySpanMapExt, setup::StrParser};
use crate::parse::util::{
    auto_type, ensure_scope_node, labelled_scope_parser, lex, scope_node_parser, span,
    struct_destructure_fields_parser,
};
use crate::{
    Span,
    ast::{
        DestructurePattern, FunctionHeader, Node, NodeType, ParserDataType, ParserInnerType,
        ParserText, PotentialDollarIdentifier, PotentialNewType, VarType,
    },
};

#[derive(Clone)]
enum FnParamGroup {
    Plain(Vec<(PotentialDollarIdentifier, PotentialNewType)>),
    Destructure {
        pattern: DestructurePattern,
        data_type: PotentialNewType,
        span: Span,
    },
}

pub struct FunctionParsers<'a> {
    pub pad: StrParser<'a, ()>,
    pub pad_with_newline: StrParser<'a, ()>,
    pub delim: StrParser<'a, ()>,
    pub comma: StrParser<'a, ()>,
    pub arrow: StrParser<'a, ()>,
    pub fat_arrow: StrParser<'a, ()>,
    pub raw_ident: StrParser<'a, (String, Span)>,
    pub ident: StrParser<'a, (String, Span)>,
    pub generic_params: StrParser<'a, crate::ast::GenericTypes>,
    pub type_name: StrParser<'a, PotentialNewType>,
    pub expr: StrParser<'a, Node>,
    pub statement: StrParser<'a, Node>,
}

pub struct FunctionBuilt<'a> {
    pub scope_block: StrParser<'a, Node>,
    pub spawn_item_expr: StrParser<'a, Node>,
    pub fn_standard_expr: StrParser<'a, Node>,
}

pub fn build_function_parsers<'a>(
    parts: FunctionParsers<'a>,
    line_starts: Arc<Vec<usize>>,
) -> FunctionBuilt<'a> {
    let FunctionParsers {
        pad,
        pad_with_newline,
        delim,
        comma,
        arrow,
        fat_arrow,
        raw_ident,
        ident,
        generic_params,
        type_name,
        expr,
        statement,
    } = parts;

    let scope = scope_node_parser(statement.clone(), delim.clone(), pad.clone()).boxed();
    let labelled_scope = labelled_scope_parser(pad.clone(), ident.clone(), scope.clone()).boxed();

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
                            PotentialDollarIdentifier::Identifier(ParserText::new(sp, name)),
                        ))
                    }),
            ))
            .separated_by(comma.clone())
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
            move |(items, ty), r| {
                let sp = span(ls.as_ref(), r);
                FnParamGroup::Destructure {
                    span: sp,
                    pattern: DestructurePattern::Tuple(items),
                    data_type: ty.unwrap_or_else(|| auto_type(sp)),
                }
            }
        });

    let struct_destructure_param =
        struct_destructure_fields_parser(pad.clone(), comma.clone(), ident.clone())
            .then(
                lex(pad.clone(), just(':'))
                    .ignore_then(type_name.clone())
                    .or_not(),
            )
            .map_with_span({
                let ls = line_starts.clone();
                move |(items, ty), r| {
                    let sp = span(ls.as_ref(), r);
                    FnParamGroup::Destructure {
                        span: sp,
                        pattern: DestructurePattern::Struct(items),
                        data_type: ty.unwrap_or_else(|| auto_type(sp)),
                    }
                }
            });

    let impl_trait_param_type = lex(pad.clone(), just("impl"))
        .ignore_then(raw_ident.clone())
        .then(
            lex(pad.clone(), just(":<"))
                .ignore_then(any().and_is(just('>').not()).repeated().collect::<String>())
                .then_ignore(lex(pad.clone(), just('>')))
                .or_not(),
        )
        .map(|((name, sp), generic_text)| {
            let trait_text = if let Some(generic_text) = generic_text {
                let inner = generic_text.trim().to_string();
                format!("{name}:<{inner}>")
            } else {
                name
            };
            PotentialNewType::DataType(ParserDataType::new(
                sp,
                ParserInnerType::DynamicTraits(vec![trait_text]),
            ))
        })
        .boxed();

    let plain_param = lex(pad.clone(), just("mut"))
        .or_not()
        .ignore_then(ident.clone())
        .repeated()
        .at_least(1)
        .collect::<Vec<_>>()
        .then(
            lex(pad.clone(), just(':'))
                .ignore_then(choice((impl_trait_param_type.clone(), type_name.clone())))
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
    .separated_by(comma.clone())
    .allow_trailing()
    .collect::<Vec<_>>()
    .or_not()
    .map(|x| x.unwrap_or_default())
    .boxed();

    let fn_params = lex(pad.clone(), just('('))
        .ignore_then(fn_param_groups.clone())
        .then_ignore(lex(pad.clone(), just(')')))
        .or_not()
        .map(|x| x.unwrap_or_default())
        .boxed();

    let scope_block = fat_arrow
        .clone()
        .ignore_then(choice((
            labelled_scope.clone(),
            scope.clone(),
            pad_with_newline
                .clone()
                .ignore_then(statement.clone())
                .map(|body| ensure_scope_node(body, true, false)),
        )))
        .boxed();

    let spawn_item_expr = scope_block
        .clone()
        .or(expr.clone().map(|n| ensure_scope_node(n, true, false)))
        .boxed();

    let fn_standard_expr = lex(pad.clone(), just("fn"))
        .then_ignore(lex(pad.clone(), just("match")).not())
        .ignore_then(generic_params.clone())
        .then(fn_params)
        .then(arrow.clone().ignore_then(type_name.clone()).or_not())
        .then(scope_block.clone())
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
                        let synthetic_name = format!("__destructure_param_{}", synthetic_idx);
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

    FunctionBuilt {
        scope_block,
        spawn_item_expr,
        fn_standard_expr,
    }
}
