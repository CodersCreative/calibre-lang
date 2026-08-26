use crate::{
    ParserError, Span,
    ast::{
        ObjectType,
        idents::{ParserText, PotentialDollarIdentifier, PotentialGenericTypeIdentifier},
        nodes::{CallArg, EmitType, Node, NodeType},
        types::{ParserDataType, ParserInnerType},
    },
};
use chumsky::error::Rich;
use chumsky::prelude::*;
use diagnostics::to_parser_errors;
use expressions::{TailExpressionParsers, build_tail_expression_parser};
use functions::{FunctionParsers, build_function_parsers};
use matching::{MatchParsers, build_match_parsers};
use setup::build_parser_prelude;
use statements::{StatementParsers, build_statement_parser};
use std::sync::Arc;
use tracing::instrument;
use util::{lex, span, strip_block_comments_keep_layout};

mod diagnostics;
mod expressions;
mod functions;
mod matching;
mod setup;
mod statements;
pub mod util;

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

#[instrument(skip_all, fields(path = ?source_path, bytes = source.len()))]
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
    let setup::ParserPrelude {
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
    } = build_parser_prelude(line_starts.clone());

    let parser = recursive(|statement| {
        let expr = recursive(|expr| {
            let functions = build_function_parsers(
                FunctionParsers {
                    pad: pad.clone(),
                    pad_with_newline: pad_with_newline.clone(),
                    delim: delim.clone(),
                    comma: comma.clone(),
                    arrow: arrow.clone(),
                    fat_arrow: fat_arrow.clone(),
                    raw_ident: raw_ident.clone(),
                    ident: ident.clone(),
                    generic_params: generic_params.clone(),
                    type_name: type_name.clone(),
                    expr: expr.clone().boxed(),
                    statement: statement.clone().boxed(),
                },
                line_starts.clone(),
            );

            let scope_block = functions.scope_block.clone();
            let spawn_item_expr = functions.spawn_item_expr.clone();
            let fn_standard_expr = functions.fn_standard_expr.clone();

            let generic_ident = ident
                .clone()
                .then(
                    lex(pad.clone(), just(":<"))
                        .ignore_then(
                            lex(pad_with_newline.clone(), type_name.clone())
                                .separated_by(comma.clone())
                                .allow_trailing()
                                .collect::<Vec<_>>()
                                .or_not()
                                .map(|x| x.unwrap_or_default()),
                        )
                        .then_ignore(lex(pad_with_newline.clone(), just('>')))
                        .or_not(),
                )
                .map(|((n, sp), generics)| {
                    if let Some(generic_types) = generics {
                        PotentialGenericTypeIdentifier::Generic {
                            identifier: PotentialDollarIdentifier::new(sp, n),
                            generic_types,
                        }
                    } else {
                        PotentialGenericTypeIdentifier::new(sp, n)
                    }
                })
                .boxed();

            let struct_field_ident = lex(
                pad_with_newline.clone(),
                text::ident().map(|s: &str| s.to_string()),
            )
            .map_with_span({
                let ls = line_starts.clone();
                move |s: String, r| (s, span(ls.as_ref(), r))
            })
            .boxed();

            let struct_lit = generic_ident
                .clone()
                .then(
                    lex(pad_with_newline.clone(), just('{'))
                        .ignore_then(
                            struct_field_ident
                                .clone()
                                .then(
                                    lex(pad_with_newline.clone(), just(':'))
                                        .ignore_then(statement.clone())
                                        .or_not(),
                                )
                                .map(|((k, sp), value)| {
                                    let value = value.unwrap_or_else(|| Node::identifier(sp, &k));
                                    (k, value)
                                })
                                .separated_by(lex(pad_with_newline.clone(), just(',')))
                                .allow_trailing()
                                .collect::<Vec<_>>()
                                .or_not()
                                .map(|x| x.unwrap_or_default()),
                        )
                        .then_ignore(lex(pad_with_newline.clone(), just('}'))),
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

            let matching = build_match_parsers(
                MatchParsers {
                    pad: pad_with_newline.clone(),
                    delim: delim.clone(),
                    comma: comma.clone(),
                    arrow: arrow.clone(),
                    ident: ident.clone(),
                    generic_params: generic_params.clone(),
                    string_lit: string_lit.clone(),
                    type_name: type_name.clone(),
                    expr: expr.clone().boxed(),
                    scope_block: scope_block.clone(),
                },
                line_starts.clone(),
            );

            let let_pattern_list = matching.let_pattern_list.clone();
            let fn_match_expr = matching.fn_match_expr.clone();
            let match_expr = matching.match_expr.clone();

            let atom = choice((
                fn_match_expr.clone(),
                fn_standard_expr.clone(),
                scope_block.clone(),
                match_expr,
                lex(pad.clone(), just("emit"))
                    .ignore_then(expr.clone())
                    .then(expr.clone().or_not())
                    .map_with_span({
                        let ls = line_starts.clone();
                        move |args: (Node, Option<Node>), r| {
                            let sp = span(ls.as_ref(), r);
                            Node::new(
                                sp,
                                NodeType::Emit(if let Some(value) = args.1 {
                                    EmitType::Channel {
                                        channel: Box::new(args.0),
                                        value: Box::new(value),
                                    }
                                } else {
                                    EmitType::Scope(Box::new(args.0))
                                }),
                            )
                        }
                    }),
                lex(pad.clone(), just("list"))
                    .ignore_then(lex(pad.clone(), just(":<")))
                    .ignore_then(type_name.clone())
                    .then_ignore(lex(pad.clone(), just('>')))
                    .or_not()
                    .map(|x| {
                        x.unwrap_or_else(|| {
                            ParserDataType::new(Span::default(), ParserInnerType::Auto(None))
                        })
                    })
                    .then(lex(pad.clone(), just('[')))
                    .then(choice((
                        lex(pad_with_newline.clone(), expr.clone())
                            .then_ignore(lex(pad.clone(), just(';')))
                            .then(int_lit.clone())
                            .map(|(value, count)| (Some((value, count)), Vec::new())),
                        lex(pad_with_newline.clone(), expr.clone())
                            .separated_by(comma.clone())
                            .allow_trailing()
                            .collect::<Vec<_>>()
                            .or_not()
                            .map(|x| (None, x.unwrap_or_default())),
                    )))
                    .then_ignore(lex(pad_with_newline.clone(), just(']')))
                    .then(
                        choice((
                            lex(pad_with_newline.clone(), just('.'))
                                .ignore_then(ident.clone())
                                .then(
                                    lex(pad.clone(), just('('))
                                        .ignore_then(
                                            lex(pad_with_newline.clone(), expr.clone())
                                                .separated_by(comma.clone())
                                                .allow_trailing()
                                                .collect::<Vec<_>>()
                                                .or_not()
                                                .map(|x| x.unwrap_or_default()),
                                        )
                                        .then_ignore(lex(pad_with_newline.clone(), just(')')))
                                        .or_not(),
                                )
                                .map(|((name, sp), args)| {
                                    let member = Node::identifier(sp, &name);
                                    if let Some(args) = args {
                                        (
                                            Node::call_full(
                                                member.span,
                                                member,
                                                Vec::new(),
                                                args.into_iter().map(CallArg::Value).collect(),
                                                Vec::new(),
                                                None,
                                            ),
                                            false,
                                        )
                                    } else {
                                        (member, false)
                                    }
                                }),
                            lex(pad_with_newline.clone(), just('['))
                                .ignore_then(lex(pad_with_newline.clone(), expr.clone()))
                                .then_ignore(lex(pad_with_newline.clone(), just(']')))
                                .map(|idx| (idx, true)),
                        ))
                        .repeated()
                        .collect::<Vec<_>>(),
                    )
                    .map_with_span({
                        let ls = line_starts.clone();
                        move |(((_open_ty, _open_br), values), tails), r| {
                            let sp = span(ls.as_ref(), r);
                            let list = if let Some((value, count)) = values.0 {
                                Node::new(
                                    sp,
                                    NodeType::ListRepeatLiteral {
                                        data_type: _open_ty,
                                        value: Box::new(value),
                                        count: Box::new(count),
                                    },
                                )
                            } else {
                                Node::new(sp, NodeType::ListLiteral(_open_ty, values.1))
                            };

                            tails.into_iter().fold(list, |current, (node, is_index)| {
                                if is_index {
                                    Node::new(
                                        Span::new_from_spans(current.span, node.span),
                                        NodeType::IndexAccess {
                                            base: Box::new(current),
                                            index: Box::new(node),
                                        },
                                    )
                                } else if let NodeType::Identifier(ident) = node.node_type {
                                    Node::new(
                                        Span::new_from_spans(current.span, node.span),
                                        NodeType::FieldAccess {
                                            base: Box::new(current),
                                            field: ident.into(),
                                        },
                                    )
                                } else {
                                    current
                                }
                            })
                        }
                    }),
                float_lit.clone(),
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
                        lex(pad_with_newline.clone(), expr.clone())
                            .separated_by(comma.clone())
                            .allow_trailing()
                            .collect::<Vec<_>>()
                            .or_not()
                            .map(|x| x.unwrap_or_default()),
                    )
                    .then_ignore(lex(pad_with_newline.clone(), just(')')))
                    .map_with_span({
                        let ls = line_starts.clone();
                        move |args, r| {
                            let sp = span(ls.as_ref(), r);
                            Node::call_full(
                                sp,
                                Node::identifier(sp, "$"),
                                Vec::new(),
                                args.into_iter().map(CallArg::Value).collect(),
                                Vec::new(),
                                None,
                            )
                        }
                    }),
                ident
                    .clone()
                    .map(|(n, sp)| Node::identifier(sp, &n))
                    .then(
                        lex(pad_with_newline.clone(), just('.'))
                            .ignore_then(ident.clone().map(|(n, sp)| Node::identifier(sp, &n))),
                    )
                    .map(|(base, field)| {
                        Node::new(
                            Span::new_from_spans(base.span, field.span),
                            NodeType::FieldAccess {
                                base: Box::new(base),
                                field: match field.node_type {
                                    NodeType::Identifier(identifier) => identifier.into(),
                                    _ => unreachable!(),
                                },
                            },
                        )
                    }),
                ident
                    .clone()
                    .map(|(n, sp)| ParserText::new(sp, &n))
                    .then(
                        lex(pad_with_newline.clone(), just("::"))
                            .ignore_then(ident.clone().map(|(n, sp)| ParserText::new(sp, &n)))
                            .repeated()
                            .at_least(1)
                            .collect::<Vec<_>>(),
                    )
                    .map(|(first, mut segments)| {
                        let value_text = segments.pop().unwrap_or_else(|| first.clone());
                        let value_span = value_text.span;

                        let mut module: Vec<PotentialDollarIdentifier> = vec![first.clone().into()];
                        module.extend(segments.clone().into_iter().map(|segment| segment.into()));

                        let mut current = Node::identifier(first.span, &first);

                        for segment in segments {
                            current = Node::new(
                                Span::new_from_spans(current.span, segment.span),
                                NodeType::ScopeAccess {
                                    base: Box::new(current),
                                    field: segment.into(),
                                },
                            );
                        }

                        let sp = Span::new_from_spans(current.span, value_span);
                        Node::new(
                            sp,
                            NodeType::ScopeAccess {
                                base: Box::new(current),
                                field: value_text.into(),
                            },
                        )
                    }),
                struct_lit,
                generic_ident
                    .clone()
                    .then(
                        lex(pad_with_newline.clone(), just('.'))
                            .ignore_then(ident.clone().map(|(n, sp)| Node::identifier(sp, &n))),
                    )
                    .map(|(base, field)| {
                        Node::new(
                            Span::new_from_spans(*base.span(), field.span),
                            NodeType::FieldAccess {
                                base: Box::new(Node::new(*base.span(), NodeType::Identifier(base))),
                                field: match field.node_type {
                                    NodeType::Identifier(identifier) => identifier.into(),
                                    _ => unreachable!(),
                                },
                            },
                        )
                    }),
                generic_ident.map(|identifier| {
                    Node::new(*identifier.span(), NodeType::Identifier(identifier))
                }),
                lex(pad.clone(), just('('))
                    .ignore_then(
                        lex(pad_with_newline.clone(), expr.clone())
                            .separated_by(comma.clone())
                            .allow_trailing()
                            .collect::<Vec<_>>()
                            .or_not()
                            .map(|x| x.unwrap_or_default()),
                    )
                    .then_ignore(lex(pad_with_newline.clone(), just(')')))
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

            build_tail_expression_parser(
                TailExpressionParsers {
                    pad: pad.clone(),
                    pad_with_newline: pad_with_newline.clone(),
                    delim: delim.clone(),
                    comma: comma.clone(),
                    arrow: arrow.clone(),
                    fat_arrow: fat_arrow.clone(),
                    left_arrow: left_arrow.clone(),
                    ident: ident.clone(),
                    int_lit: int_lit.clone(),
                    named_ident: named_ident.clone(),
                    type_name: type_name.clone(),
                    expr: expr.boxed(),
                    statement: statement.clone().boxed(),
                    atom: atom.clone(),
                    fn_standard_expr: fn_standard_expr.clone(),
                    fn_match_expr: fn_match_expr.clone(),
                    scope_block: scope_block.clone(),
                    spawn_item_expr: spawn_item_expr.clone(),
                    let_pattern_list: let_pattern_list.clone(),
                },
                line_starts.clone(),
            )
        });

        build_statement_parser(
            StatementParsers {
                pad: pad.clone(),
                pad_with_newline: pad_with_newline.clone(),
                delim: delim.clone(),
                comma: comma.clone(),
                arrow: arrow.clone(),
                fat_arrow: fat_arrow.clone(),
                left_arrow: left_arrow.clone(),
                raw_ident: raw_ident.clone(),
                ident: ident.clone(),
                named_ident: named_ident.clone(),
                generic_params: generic_params.clone(),
                string_text: string_text.clone(),
                type_name: type_name.clone(),
                statement: statement.boxed(),
                expr: expr.boxed(),
            },
            line_starts.clone(),
            source_path,
        )
    })
    .boxed();

    let parsed = pad
        .clone()
        .then_ignore(delim.clone().repeated().collect::<Vec<_>>())
        .ignore_then(
            parser
                .then_ignore(delim.clone().repeated().collect::<Vec<_>>())
                .repeated()
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
