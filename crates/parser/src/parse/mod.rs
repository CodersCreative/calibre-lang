use crate::{
    ParserError, Span,
    ast::{
        ObjectType,
        idents::{ParserText, PotentialDollarIdentifier, PotentialGenericTypeIdentifier},
        nodes::{
            AstNode, AstNodeType,
            access::{AstField, AstIdentifier, AstIndex, AstScope},
            flow::AstEmit,
            functions::{AstCurry, CallArg},
            lists::AstList,
            literals::{AstStruct, AstTuple},
            misc::AstParen,
            scopes::AstScopeDef,
        },
        types::{ParserDataType, ParserInnerType},
    },
    lexer::Token,
};
use chumsky::prelude::*;
use chumsky::span::Span as ChumskySpan;
use chumsky::{error::Rich, extra::ParserExtra};
use diagnostics::to_parser_errors;
use expressions::{TailExpressionParsers, build_tail_expression_parser};
use functions_old::{FunctionParsers, build_function_parsers};
use matching_old::{MatchParsers, build_match_parsers};
use setup::build_parser_prelude;
use statements::{StatementParsers, build_statement_parser};
use tracing::instrument;
use ustr::Ustr;
use util::{lex, strip_block_comments_keep_layout};

pub mod access;
pub mod assignment;
pub mod binary;
pub mod conditionals;
pub mod data_types;
pub mod declarations;
mod diagnostics;
mod expressions;
pub mod flow;
pub mod functions;
mod functions_old;
pub mod generator;
pub mod idents;
pub mod lists;
pub mod literals;
pub mod loops;
pub mod matching;
mod matching_old;
pub mod memory;
pub mod misc;
pub mod scopes;
mod setup;
pub mod spawn;
mod statements;
pub mod types;
pub mod unary;
pub mod util;

pub type AstParserErr<'a> = extra::Err<Rich<'a, Token<'a>>>;
pub type TokenStream<'a> = &'a [Token<'a>];

pub trait AstParser<'a>: Sized {
    fn parser() -> Boxed<'a, 'a, TokenStream<'a>, Self, AstParserErr<'a>>;
}

pub trait MapWithSpanExt<'a, I, O, E>: Parser<'a, I, O, E>
where
    I: chumsky::input::Input<'a>,
    E: ParserExtra<'a, I>,
    I::Span: ChumskySpan<Offset = usize>,
{
    fn map_with_span<U, F>(self, f: F) -> impl Parser<'a, I, U, E>
    where
        F: Fn(O, Span) -> U + Clone + 'a,
        Self: Sized + 'a,
    {
        self.map_with(move |out, extra| {
            let span = extra.span();
            f(out, Span::from(span.start()..span.end()))
        })
    }

    fn try_map_with_span<U, F, Err>(self, f: F) -> impl Parser<'a, I, U, E>
    where
        F: Fn(O, Span) -> Result<U, Err> + Clone + 'a,
        Err: Into<E::Error>,
        Self: Sized + 'a,
    {
        self.try_map_with(move |out, extra| {
            let span = extra.span();
            f(out, Span::from(span.start()..span.end())).map_err(Into::into)
        })
    }
}

impl<'a, I, O, E, P> MapWithSpanExt<'a, I, O, E> for P
where
    I: chumsky::input::Input<'a>,
    E: ParserExtra<'a, I>,
    I::Span: ChumskySpan<Offset = usize>,
    P: Parser<'a, I, O, E>,
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

// TODO I will do these once the entire parser is complete
impl<'a> AstParser<'a> for AstNode {
    fn parser() -> Boxed<'a, 'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        unimplemented!()
    }
}

impl<'a> AstParser<'a> for AstNodeType {
    fn parser() -> Boxed<'a, 'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        unimplemented!()
    }
}

#[instrument(skip_all, fields(path = ?source_path, bytes = source.len()))]
pub fn parse_program_with_source(
    source: &str,
    source_path: Option<&std::path::Path>,
) -> Result<AstNode, Vec<ParserError>> {
    let source = strip_block_comments_keep_layout(source);

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
    } = build_parser_prelude();

    let parser = recursive(|statement| {
        let expr = recursive(|expr| {
            let functions = build_function_parsers(FunctionParsers {
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
            });

            let scope_block = functions.scope_block.clone();
            let spawn_item_expr = functions.spawn_item_expr.clone();
            let fn_standard_expr = functions.fn_standard_expr.clone();

            let generic_ident: Boxed<
                '_,
                '_,
                &str,
                PotentialGenericTypeIdentifier,
                extra::Full<Rich<'_, char>, (), ()>,
            > = ident
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
            .map_with_span(move |s: String, sp| (s, sp))
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
                                    let value =
                                        value.unwrap_or_else(|| AstNode::identifier(sp, &k));
                                    (Ustr::from(&k), value)
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
                    AstNode::new(
                        sp,
                        AstNodeType::StructLiteral(AstStruct {
                            identifier,
                            value: ObjectType::Map(fields),
                        }),
                    )
                })
                .boxed();

            let matching = build_match_parsers(MatchParsers {
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
            });

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
                    .map_with_span(move |args: (AstNode, Option<AstNode>), sp| {
                        AstNode::new(
                            sp,
                            AstNodeType::Emit(if let Some(value) = args.1 {
                                AstEmit::Channel {
                                    channel: Box::new(args.0),
                                    value: Box::new(value),
                                }
                            } else {
                                AstEmit::Scope(Box::new(args.0))
                            }),
                        )
                    }),
                lex(pad.clone(), just("curry"))
                    .ignore_then(expr.clone())
                    .map_with_span(move |value: AstNode, sp| {
                        AstNode::new(
                            sp,
                            AstNodeType::CurryExpression(AstCurry {
                                value: Box::new(value),
                            }),
                        )
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
                                    let member = AstNode::identifier(sp, &name);
                                    if let Some(args) = args {
                                        (
                                            AstNode::call_full(
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
                    .map_with_span(move |(((_open_ty, _open_br), values), tails), sp| {
                        let list = if let Some((_value, _count)) = values.0 {
                            unimplemented!()
                        } else {
                            AstNode::new(
                                sp,
                                AstNodeType::ListLiteral(AstList {
                                    data_type: _open_ty,
                                    values: values.1,
                                }),
                            )
                        };

                        tails.into_iter().fold(list, |current, (node, is_index)| {
                            if is_index {
                                AstNode::new(
                                    Span::new_from_spans(current.span, node.span),
                                    AstNodeType::IndexAccess(AstIndex {
                                        base: Box::new(current),
                                        index: Box::new(node),
                                    }),
                                )
                            } else if let AstNodeType::Identifier(ident) = node.node_type {
                                AstNode::new(
                                    Span::new_from_spans(current.span, node.span),
                                    AstNodeType::FieldAccess(AstField {
                                        base: Box::new(current),
                                        field: ident.value.into(),
                                    }),
                                )
                            } else {
                                current
                            }
                        })
                    }),
                float_lit.clone(),
                int_lit.clone(),
                string_lit.clone(),
                char_lit.clone(),
                null_lit.clone(),
                dollar_ident.clone().map(|id| {
                    let sp = *id.span();
                    AstNode::new(
                        sp,
                        AstNodeType::Identifier(AstIdentifier {
                            value: PotentialGenericTypeIdentifier::Identifier(id),
                        }),
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
                    .map_with_span(move |args, sp| {
                        AstNode::call_full(
                            sp,
                            AstNode::identifier(sp, "$"),
                            Vec::new(),
                            args.into_iter().map(CallArg::Value).collect(),
                            Vec::new(),
                            None,
                        )
                    }),
                ident
                    .clone()
                    .map(|(n, sp)| AstNode::identifier(sp, &n))
                    .then(
                        lex(pad_with_newline.clone(), just('.'))
                            .ignore_then(ident.clone().map(|(n, sp)| AstNode::identifier(sp, &n))),
                    )
                    .map(|(base, field)| {
                        AstNode::new(
                            Span::new_from_spans(base.span, field.span),
                            AstNodeType::FieldAccess(AstField {
                                base: Box::new(base),
                                field: match field.node_type {
                                    AstNodeType::Identifier(identifier) => identifier.value.into(),
                                    _ => unreachable!(),
                                },
                            }),
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

                        let mut current = AstNode::identifier(first.span, &first);

                        for segment in segments {
                            current = AstNode::new(
                                Span::new_from_spans(current.span, segment.span),
                                AstNodeType::ScopeAccess(AstScope {
                                    base: Box::new(current),
                                    field: segment.into(),
                                }),
                            );
                        }

                        let sp = Span::new_from_spans(current.span, value_span);
                        AstNode::new(
                            sp,
                            AstNodeType::ScopeAccess(AstScope {
                                base: Box::new(current),
                                field: value_text.into(),
                            }),
                        )
                    }),
                struct_lit,
                generic_ident
                    .clone()
                    .then(
                        lex(pad_with_newline.clone(), just('.'))
                            .ignore_then(ident.clone().map(|(n, sp)| AstNode::identifier(sp, &n))),
                    )
                    .map(|(base, field)| {
                        AstNode::new(
                            Span::new_from_spans(*base.span(), field.span),
                            AstNodeType::FieldAccess(AstField {
                                base: Box::new(AstNode::new(
                                    *base.span(),
                                    AstNodeType::Identifier(AstIdentifier { value: base }),
                                )),
                                field: match field.node_type {
                                    AstNodeType::Identifier(identifier) => identifier.value.into(),
                                    _ => unreachable!(),
                                },
                            }),
                        )
                    }),
                generic_ident.map(|identifier| {
                    AstNode::new(
                        *identifier.span(),
                        AstNodeType::Identifier(AstIdentifier { value: identifier }),
                    )
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
                    .map_with_span(move |values, sp| {
                        let inner = if values.len() == 1 {
                            values
                                .first()
                                .cloned()
                                .unwrap_or_else(|| AstNode::new(sp, AstNodeType::EmptyLine))
                        } else {
                            AstNode::new(sp, AstNodeType::TupleLiteral(AstTuple { values }))
                        };

                        AstNode::new(
                            sp,
                            AstNodeType::ParenExpression(AstParen {
                                value: Box::new(inner),
                            }),
                        )
                    }),
            ))
            .boxed();

            build_tail_expression_parser(TailExpressionParsers {
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
            })
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
        .parse(&source);

    if let Some(items) = parsed.output().cloned() {
        let sp = if let (Some(a), Some(b)) = (items.first(), items.last()) {
            Span::new_from_spans(a.span, b.span)
        } else {
            Span::default()
        };
        return Ok(AstNode::new(
            sp,
            AstNodeType::ScopeDeclaration(AstScopeDef {
                body: Some(items),
                named: None,
                is_temp: false,
                create_new_scope: Some(false),
                define: false,
            }),
        ));
    }

    Err(to_parser_errors(parsed.into_errors()))
}
