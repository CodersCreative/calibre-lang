use super::{LegacySpanMapExt, filter, setup::StrParser};
use crate::ast::RefMutability;
use crate::ast::idents::{ParserText, PotentialDollarIdentifier};
use crate::ast::matching::{MatchArmType, TryCatch};
use crate::ast::nodes::{
    AsFailureMode, AstNode, AstNodeType, CallArg, IfComparisonType, LoopType, PipeSegment,
};
use crate::ast::types::{ParserDataType, ParserInnerType};
use crate::parse::util::{
    ensure_scope_node, lex, parse_embedded_expr, parse_splits, span, span_from_nodes_or,
    unescape_string,
};
use crate::{
    Span,
    ast::{
        binary::BinaryOperator,
        comparison::{BooleanOperator, ComparisonOperator},
    },
};
use chumsky::error::Rich;
use chumsky::prelude::*;
use std::sync::Arc;

pub struct TailExpressionParsers<'a> {
    pub pad: StrParser<'a, ()>,
    pub pad_with_newline: StrParser<'a, ()>,
    pub delim: StrParser<'a, ()>,
    pub comma: StrParser<'a, ()>,
    pub arrow: StrParser<'a, ()>,
    pub fat_arrow: StrParser<'a, ()>,
    pub left_arrow: StrParser<'a, ()>,
    pub ident: StrParser<'a, (String, Span)>,
    pub int_lit: StrParser<'a, AstNode>,
    pub named_ident: StrParser<'a, PotentialDollarIdentifier>,
    pub type_name: StrParser<'a, ParserDataType>,
    pub expr: StrParser<'a, AstNode>,
    pub statement: StrParser<'a, AstNode>,
    pub atom: StrParser<'a, AstNode>,
    pub fn_standard_expr: StrParser<'a, AstNode>,
    pub fn_match_expr: StrParser<'a, AstNode>,
    pub scope_block: StrParser<'a, AstNode>,
    pub spawn_item_expr: StrParser<'a, AstNode>,
    pub let_pattern_list: StrParser<'a, Vec<MatchArmType>>,
}

pub fn build_tail_expression_parser<'a>(
    parts: TailExpressionParsers<'a>,
    line_starts: Arc<Vec<usize>>,
) -> StrParser<'a, AstNode> {
    let TailExpressionParsers {
        pad,
        pad_with_newline,
        delim,
        comma,
        arrow,
        fat_arrow,
        left_arrow,
        ident,
        int_lit,
        named_ident,
        type_name,
        expr,
        statement,
        atom,
        fn_standard_expr,
        fn_match_expr,
        scope_block,
        spawn_item_expr,
        let_pattern_list,
    } = parts;

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
    .try_map({
        let ls = line_starts.clone();
        move |parts: Vec<String>, parser_sp| {
            let sp = span(ls.as_ref(), parser_sp.into_range());
            let raw = unescape_string(&parts.concat());
            let (texts, args) = parse_splits(&raw);

            let text_nodes = texts
                .into_iter()
                .map(|txt| AstNode::new(sp, AstNodeType::StringLiteral(ParserText::new(sp, txt))))
                .collect::<Vec<_>>();

            let mut call_args = vec![CallArg::Value(AstNode::new(
                sp,
                AstNodeType::ListLiteral(ParserDataType::new(sp, ParserInnerType::Str), text_nodes),
            ))];

            for arg in args {
                let embedded =
                    parse_embedded_expr(&arg, sp).map_err(|msg| Rich::custom(parser_sp, msg))?;
                call_args.push(CallArg::Value(embedded));
            }

            Ok((ParserText::new(sp, raw), call_args))
        }
    })
    .boxed();

    let arg_value = lex(pad_with_newline.clone(), expr.clone())
        .then(
            lex(pad.clone(), just('['))
                .ignore_then(lex(pad_with_newline.clone(), expr.clone()))
                .then_ignore(lex(pad_with_newline.clone(), just(']')))
                .repeated()
                .collect::<Vec<_>>(),
        )
        .map(|(head, indexes)| {
            indexes.into_iter().fold(head, |current, index| {
                AstNode::new(
                    Span::new_from_spans(current.span, index.span),
                    AstNodeType::IndexAccess {
                        base: Box::new(current),
                        index: Box::new(index),
                    },
                )
            })
        })
        .boxed();

    let call_arg = choice((
        named_ident
            .clone()
            .then_ignore(lex(pad_with_newline.clone(), just(':')))
            .then(arg_value.clone())
            .map(|(name, value)| CallArg::Named(name, value)),
        arg_value.map(CallArg::Value),
    ))
    .boxed();

    let call_args = lex(pad.clone(), just('('))
        .ignore_then(
            lex(pad_with_newline.clone(), call_arg)
                .separated_by(comma.clone())
                .allow_trailing()
                .collect::<Vec<_>>()
                .or_not()
                .map(|x| x.unwrap_or_default()),
        )
        .then_ignore(lex(pad_with_newline.clone(), just(')')))
        .boxed();

    let reverse_args = lex(pad_with_newline.clone(), just("<("))
        .ignore_then(
            lex(pad_with_newline.clone(), expr.clone())
                .then(
                    lex(pad.clone(), just('['))
                        .ignore_then(expr.clone())
                        .then_ignore(lex(pad.clone(), just(']')))
                        .repeated()
                        .collect::<Vec<_>>(),
                )
                .map(|(head, indexes)| {
                    indexes.into_iter().fold(head, |current, index| {
                        AstNode::new(
                            Span::new_from_spans(current.span, index.span),
                            AstNodeType::IndexAccess {
                                base: Box::new(current),
                                index: Box::new(index),
                            },
                        )
                    })
                })
                .separated_by(comma.clone())
                .allow_trailing()
                .collect::<Vec<_>>()
                .or_not()
                .map(|x: Option<Vec<AstNode>>| x.unwrap_or_default()),
        )
        .then_ignore(lex(pad_with_newline.clone(), just(')')))
        .boxed();

    #[derive(Clone)]
    enum PostfixSuffix {
        Member(Box<AstNode>, bool),
        Ref(RefMutability),
        Deref,
    }

    let member = choice((
        lex(pad.clone(), just(".&mut")).to(PostfixSuffix::Ref(RefMutability::MutRef)),
        lex(pad.clone(), just(".&")).to(PostfixSuffix::Ref(RefMutability::Ref)),
        lex(pad.clone(), just(".*")).to(PostfixSuffix::Deref),
        lex(pad.clone(), just('.'))
            .ignore_then(choice((
                ident.clone().map(|(n, sp)| AstNode::identifier(sp, &n)),
                int_lit.clone(),
            )))
            .then(call_args.clone().repeated().collect::<Vec<_>>())
            .map(|(m, calls)| {
                let node = calls.into_iter().fold(m, |c, args| {
                    AstNode::call_full(c.span, c, Vec::new(), args, Vec::new(), None)
                });
                PostfixSuffix::Member(Box::new(node), false)
            }),
        lex(pad.clone(), just('['))
            .ignore_then(expr.clone())
            .then_ignore(lex(pad.clone(), just(']')))
            .map(|e| PostfixSuffix::Member(Box::new(e), true)),
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
            .map(|((string_fn, args), rev)| (Some(string_fn), args, rev.unwrap_or_default())),
    ))
    .boxed();

    let apply_calls =
        |head: AstNode, calls: Vec<(Option<ParserText>, Vec<CallArg>, Vec<AstNode>)>| {
            calls
                .into_iter()
                .fold(head, |c, (string_fn, args, reverse_args)| {
                    AstNode::call_full(c.span, c, Vec::new(), args, reverse_args, string_fn)
                })
        };

    let apply_postfix_suffixes = |head: AstNode, rest: Vec<PostfixSuffix>| {
        let mut current = head;

        for suffix in rest {
            match suffix {
                PostfixSuffix::Member(node, is_index) => {
                    if is_index {
                        current = AstNode::new(
                            Span::new_from_spans(current.span, node.span),
                            AstNodeType::IndexAccess {
                                base: Box::new(current),
                                index: node,
                            },
                        );
                    } else if let AstNodeType::Identifier(ident) = node.node_type {
                        current = AstNode::new(
                            Span::new_from_spans(current.span, node.span),
                            AstNodeType::FieldAccess {
                                base: Box::new(current),
                                field: ident.into(),
                            },
                        );
                    } else if let AstNodeType::IntLiteral(value) = node.node_type {
                        current = AstNode::new(
                            Span::new_from_spans(current.span, value.span),
                            AstNodeType::FieldAccess {
                                base: Box::new(current),
                                field: value.into(),
                            },
                        );
                    } else if let AstNodeType::CallExpression {
                        caller,
                        string_fn,
                        generic_types,
                        args,
                        reverse_args,
                    } = &node.node_type
                        && let AstNodeType::Identifier(ident) = &caller.node_type
                    {
                        current = AstNode::new(
                            Span::new_from_spans(current.span, node.span),
                            AstNodeType::FieldAccess {
                                base: Box::new(current),
                                field: ident.clone().into(),
                            },
                        );
                        current = AstNode::new(
                            Span::new_from_spans(current.span, node.span),
                            AstNodeType::CallExpression {
                                caller: Box::new(current),
                                string_fn: string_fn.clone(),
                                generic_types: generic_types.clone(),
                                args: args.clone(),
                                reverse_args: reverse_args.clone(),
                            },
                        );
                    } else {
                        current = *node;
                    }
                }
                PostfixSuffix::Ref(mutability) => {
                    current = AstNode::new(
                        current.span,
                        AstNodeType::RefStatement {
                            mutability,
                            value: Box::new(current),
                        },
                    );
                }
                PostfixSuffix::Deref => {
                    current = AstNode::new(
                        current.span,
                        AstNodeType::DerefStatement {
                            value: Box::new(current),
                        },
                    );
                }
            }
        }

        current
    };

    let postfix = atom
        .clone()
        .then(call_suffix.clone().repeated().collect::<Vec<_>>())
        .map(move |(head, calls)| apply_calls(head, calls))
        .then(member.clone().repeated().collect::<Vec<_>>())
        .map(move |(head, rest)| apply_postfix_suffixes(head, rest))
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

    let enum_with_payload = named_ident
        .clone()
        .then_ignore(lex(pad.clone(), just('.')))
        .then(named_ident.clone())
        .then_ignore(lex(pad.clone(), just(':')))
        .then(expr.clone())
        .map(|((identifier, value), data)| {
            let ident = identifier;
            AstNode::new(
                Span::new_from_spans(*ident.span(), data.span),
                AstNodeType::EnumExpression {
                    identifier: ident.into(),
                    value,
                    data: Some(Box::new(data)),
                },
            )
        })
        .boxed();

    let list_prefix_type = lex(pad.clone(), just("list"))
        .ignore_then(lex(pad.clone(), just(":<")))
        .ignore_then(type_name.clone())
        .then_ignore(lex(pad.clone(), just('>')))
        .or_not()
        .map(|x| {
            x.unwrap_or_else(|| ParserDataType::new(Span::default(), ParserInnerType::Auto(None)))
        })
        .boxed();

    let list_lit = list_prefix_type
        .clone()
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
        .then(member.clone().repeated().collect::<Vec<_>>())
        .map_with_span({
            let ls = line_starts.clone();
            move |(((_open_ty, _open_br), values), tails), r| {
                let data_type = _open_ty;
                let sp = span(ls.as_ref(), r);
                let list = if let Some((value, count)) = values.0 {
                    AstNode::new(
                        sp,
                        AstNodeType::ListRepeatLiteral {
                            data_type,
                            value: Box::new(value),
                            count: Box::new(count),
                        },
                    )
                } else {
                    AstNode::new(sp, AstNodeType::ListLiteral(data_type, values.1))
                };
                apply_postfix_suffixes(list, tails)
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

    let iter_expr = list_prefix_type
        .clone()
        .then(lex(pad.clone(), just('[')))
        .then(expr.clone())
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
        .map(|parts| {
            let (((((open_ty, _open_br), map), spawned), loop_type), conditionals) = parts.0;
            let until = parts.1;
            AstNode::new(
                map.span,
                AstNodeType::IterExpression {
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

    let prefixed = choice((enum_with_payload, atom.clone()))
        .then(call_suffix.clone().repeated().collect::<Vec<_>>())
        .map(move |(head, calls)| apply_calls(head, calls))
        .then(member.clone().repeated().collect::<Vec<_>>())
        .map(move |(head, rest)| apply_postfix_suffixes(head, rest))
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
                    right = AstNode::new(
                        Span::new_from_spans(left.span, right.span),
                        AstNodeType::BinaryExpression {
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
                AstNode::new(n.span, AstNodeType::NegExpression { value: Box::new(n) })
            } else {
                AstNode::new(n.span, AstNodeType::NotExpression { value: Box::new(n) })
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
        .then(mul_op.then(unary.clone()).repeated().collect::<Vec<_>>())
        .map(|(h, t)| {
            t.into_iter().fold(h, |l, (op, r)| {
                AstNode::new(
                    Span::new_from_spans(l.span, r.span),
                    AstNodeType::BinaryExpression {
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
        .then(add_op.then(product.clone()).repeated().collect::<Vec<_>>())
        .map(|(h, t)| {
            t.into_iter().fold(h, |l, (op, r)| {
                AstNode::new(
                    Span::new_from_spans(l.span, r.span),
                    AstNodeType::BinaryExpression {
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
                AstNode::new(
                    Span::new_from_spans(l.span, r.span),
                    AstNodeType::BinaryExpression {
                        left: Box::new(l),
                        right: Box::new(r),
                        operator: op,
                    },
                )
            })
        })
        .boxed();

    let cmp_op = choice((
        lex(pad.clone(), just('=')).to(ComparisonOperator::Equal),
        lex(pad.clone(), just("!=")).to(ComparisonOperator::NotEqual),
        lex(pad.clone(), just(">=")).to(ComparisonOperator::GreaterEqual),
        lex(pad.clone(), just("<=")).to(ComparisonOperator::LesserEqual),
        lex(pad.clone(), just('>')).to(ComparisonOperator::Greater),
        lex(pad.clone(), just('<').then(just('-').not())).to(ComparisonOperator::Lesser),
    ))
    .boxed();

    let compared = bitwise
        .clone()
        .then(cmp_op.then(bitwise.clone()).repeated().collect::<Vec<_>>())
        .map(|(h, t)| {
            t.into_iter().fold(h, |l, (op, r)| {
                AstNode::new(
                    Span::new_from_spans(l.span, r.span),
                    AstNodeType::ComparisonExpression {
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
                .then(compared.clone().padded_by(pad_with_newline.clone()))
                .repeated()
                .collect::<Vec<_>>(),
        )
        .map(|(h, t)| {
            t.into_iter().fold(h, |l, (op, r)| {
                AstNode::new(
                    Span::new_from_spans(l.span, r.span),
                    AstNodeType::BooleanExpression {
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
                AstNode::new(
                    Span::new_from_spans(from.span, to.span),
                    AstNodeType::RangeDeclaration {
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
                    AstNode::new(
                        value.span,
                        AstNodeType::AsExpression {
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

    let is_expr = as_expr
        .clone()
        .then(
            lex(pad.clone(), just("is"))
                .ignore_then(type_name.clone())
                .repeated()
                .collect::<Vec<_>>(),
        )
        .map(|(head, checks)| {
            checks.into_iter().fold(head, |value, data_type| {
                AstNode::new(
                    value.span,
                    AstNodeType::IsExpression {
                        value: Box::new(value),
                        data_type,
                    },
                )
            })
        })
        .boxed();

    let in_expr = is_expr
        .clone()
        .then(
            lex(pad.clone(), just("in"))
                .ignore_then(is_expr.clone())
                .repeated()
                .collect::<Vec<_>>(),
        )
        .map(|(h, t)| {
            t.into_iter().fold(h, |l, r| {
                AstNode::new(
                    Span::new_from_spans(l.span, r.span),
                    AstNodeType::InDeclaration {
                        identifier: Box::new(l),
                        value: Box::new(r),
                    },
                )
            })
        })
        .boxed();

    let ternary_expr = recursive(|_ternary_expr| {
        in_expr
            .clone()
            .then(
                lex(pad.clone(), just('?'))
                    .ignore_then(statement.clone())
                    .then_ignore(lex(pad.clone(), just(':')))
                    .then(statement.clone())
                    .or_not(),
            )
            .map(|(comparison, then_otherwise)| {
                if let Some((then, otherwise)) = then_otherwise {
                    AstNode::new(
                        Span::new_from_spans(comparison.span, otherwise.span),
                        AstNodeType::Ternary {
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
            .ignore_then(ternary_expr.clone())
            .map(PipeSegment::Unnamed),
        lex(pad_with_newline.clone(), just("|:"))
            .ignore_then(named_ident.clone())
            .then_ignore(lex(pad_with_newline.clone(), just('>')))
            .then(ternary_expr.clone())
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

            let head_span = head.span;
            let mut items = vec![PipeSegment::Unnamed(head)];
            for seg in rest {
                match seg {
                    PipeSegment::Unnamed(node) => {
                        if let AstNodeType::PipeExpression(mut nested) = node.node_type {
                            items.append(&mut nested);
                        } else {
                            items.push(PipeSegment::Unnamed(node));
                        }
                    }
                    PipeSegment::Named { identifier, node } => {
                        if let AstNodeType::PipeExpression(mut nested) = node.node_type {
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

            let sp = span_from_nodes_or(
                items[0].get_node(),
                items.last().map(|seg| seg.get_node()),
                head_span,
            );
            AstNode::new(sp, AstNodeType::PipeExpression(items))
        })
        .boxed();

    let fn_inline_gen_expr = lex(pad.clone(), just("fn"))
        .ignore_then(lex(pad.clone(), just('(')))
        .ignore_then(
            statement
                .clone()
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
                    Some(ParserDataType {
                        data_type:
                            ParserInnerType::StructWithGenerics {
                                identifier,
                                generic_types,
                            },
                        ..
                    }) if identifier == "gen" && generic_types.len() == 1 => {
                        Some(generic_types[0].clone())
                    }
                    Some(ParserDataType {
                        data_type: ParserInnerType::Gen(x),
                        ..
                    }) => Some(*x.clone()),
                    Some(_) => {
                        return Err(Rich::custom(
                            sp,
                            "expected inline generator return type `gen:<T>`",
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
                AstNode::new(
                    span(ls.as_ref(), r),
                    AstNodeType::InlineGenerator {
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
        .map(move |(head, rest)| apply_postfix_suffixes(head, rest))
        .boxed();

    let try_expr = lex(pad.clone(), just("try"))
        .ignore_then(lex(pad_with_newline.clone(), statement.clone()))
        .then(
            choice((
                lex(pad_with_newline.clone(), just(':'))
                    .ignore_then(ident.clone())
                    .then(scope_block.clone())
                    .map(|((name, sp), body)| TryCatch {
                        name: Some(PotentialDollarIdentifier::Identifier(ParserText::new(
                            sp, name,
                        ))),
                        body: Box::new(body),
                    }),
                scope_block.clone().map(|body| TryCatch {
                    name: None,
                    body: Box::new(body),
                }),
            ))
            .or_not(),
        )
        .map(|(value, catch)| {
            AstNode::new(
                value.span,
                AstNodeType::Try {
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
        .then(statement.clone())
        .map(|(function, value)| {
            AstNode::new(
                value.span,
                AstNodeType::Defer {
                    value: Box::new(value),
                    function,
                },
            )
        })
        .boxed();
    let move_expr = lex(pad.clone(), just("move"))
        .ignore_then(expr.clone())
        .map(|value| {
            AstNode::new(
                value.span,
                AstNodeType::MoveExpression {
                    value: Box::new(value),
                },
            )
        })
        .boxed();

    let break_expr = lex(pad.clone(), just("break"))
        .ignore_then(
            lex(pad.clone(), just('@'))
                .ignore_then(ident.clone())
                .map(|(n, sp)| PotentialDollarIdentifier::Identifier(ParserText::new(sp, n)))
                .or_not(),
        )
        .then(expr.clone().or_not())
        .map_with_span({
            let ls = line_starts.clone();
            move |(label, value), r| {
                AstNode::new(
                    span(ls.as_ref(), r),
                    AstNodeType::Break {
                        label,
                        value: value.map(Box::new),
                    },
                )
            }
        })
        .boxed();

    let continue_expr = lex(pad.clone(), just("continue"))
        .ignore_then(
            lex(pad.clone(), just('@'))
                .ignore_then(ident.clone())
                .map(|(n, sp)| PotentialDollarIdentifier::Identifier(ParserText::new(sp, n)))
                .or_not(),
        )
        .map_with_span({
            let ls = line_starts.clone();
            move |label, r| AstNode::new(span(ls.as_ref(), r), AstNodeType::Continue { label })
        })
        .boxed();

    let spawn_expr = lex(
        pad.clone(),
        just("spawn")
            .then(just('@').or_not())
            .map(|(_, at)| at.is_some()),
    )
    .then(choice((
        lex(pad.clone(), just('{'))
            .ignore_then(delim.clone().repeated().collect::<Vec<_>>())
            .ignore_then(
                spawn_item_expr
                    .clone()
                    .or(expr.clone().map(|n| ensure_scope_node(n, true, false)))
                    .separated_by(
                        comma
                            .clone()
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
            .map_with_span({
                let ls = line_starts.clone();
                move |items, r| {
                    AstNode::new(
                        span(ls.as_ref(), r),
                        AstNodeType::Spawn {
                            items,
                            auto_wait: false,
                        },
                    )
                }
            }),
        lex(pad.clone(), text::keyword("for"))
            .ignore_then(
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
                    .or(expr.clone().map(LoopType::While)),
            )
            .then(scope_block.clone())
            .map(|(lt, body)| {
                let body = ensure_scope_node(body, true, false);
                AstNode::new(
                    body.span,
                    AstNodeType::LoopDeclaration {
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
    .map(|(auto_wait, mut item)| {
        if let AstNodeType::Spawn {
            auto_wait: item_auto_wait,
            ..
        } = &mut item.node_type
        {
            *item_auto_wait = auto_wait;
            item
        } else {
            AstNode::new(
                item.span,
                AstNodeType::Spawn {
                    items: vec![item],
                    auto_wait,
                },
            )
        }
    })
    .boxed();

    let for_expr = lex(pad.clone(), text::keyword("for"))
        .ignore_then(
            fat_arrow
                .clone()
                .rewind()
                .to(LoopType::Loop)
                .or(lex(pad.clone(), just("let"))
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
        .then(scope_block.clone())
        .then(
            lex(pad_with_newline.clone(), just("else"))
                .ignore_then(scope_block.clone())
                .or_not(),
        )
        .then(
            lex(pad_with_newline.clone(), just("until"))
                .ignore_then(expr.clone())
                .or_not(),
        )
        .map(|((((loop_type, label), body), else_body), until)| {
            let body = ensure_scope_node(body, true, false);
            let else_body = else_body.map(|body| Box::new(ensure_scope_node(body, true, false)));
            AstNode::new(
                body.span,
                AstNodeType::LoopDeclaration {
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
        let if_let_cond = lex(pad_with_newline.clone(), just("let"))
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
            .then(scope_block.clone())
            .then(
                lex(pad_with_newline.clone(), just("else"))
                    .ignore_then(if_e.clone().or(scope_block.clone()))
                    .or_not(),
            )
            .map(|((cond, then_b), otherwise)| {
                let then_node = match then_b.node_type {
                    AstNodeType::ScopeDeclaration { .. } => then_b,
                    _ => ensure_scope_node(then_b, true, false),
                };
                let cond_span = match &cond {
                    IfComparisonType::If(node) => node.span,
                    IfComparisonType::IfLet { value, .. } => value.span,
                };
                AstNode::new(
                    Span::new_from_spans(
                        cond_span,
                        otherwise.as_ref().map(|x| x.span).unwrap_or(then_node.span),
                    ),
                    AstNodeType::IfStatement {
                        comparison: Box::new(cond),
                        then: Box::new(then_node),
                        otherwise: otherwise.map(Box::new),
                    },
                )
            })
            .boxed()
    })
    .boxed();

    let base_expr = choice((
        fn_inline_postfix,
        fn_match_postfix,
        fn_standard_postfix,
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
    .boxed();

    let assign_target = lex(pad.clone(), just('*'))
        .repeated()
        .collect::<Vec<_>>()
        .then(choice((ternary_expr, postfix.clone())))
        .map(|(stars, node)| {
            stars.into_iter().rev().fold(node, |value, _| {
                AstNode::new(
                    value.span,
                    AstNodeType::DerefStatement {
                        value: Box::new(value),
                    },
                )
            })
        })
        .boxed();

    assign_target
        .clone()
        .then(choice((
            lex(pad.clone(), just(":=")).to(None),
            lex(pad.clone(), just("+=")).to(Some(BinaryOperator::Add)),
            lex(pad.clone(), just("-=")).to(Some(BinaryOperator::Sub)),
            lex(pad.clone(), just("*=")).to(Some(BinaryOperator::Mul)),
            lex(pad.clone(), just("**=")).to(Some(BinaryOperator::Pow)),
            lex(pad.clone(), just("/=")).to(Some(BinaryOperator::Div)),
            lex(pad.clone(), just("%=")).to(Some(BinaryOperator::Mod)),
            lex(pad.clone(), just("&=")).to(Some(BinaryOperator::BitAnd)),
            lex(pad.clone(), just("|=")).to(Some(BinaryOperator::BitOr)),
            lex(pad.clone(), just("^=")).to(Some(BinaryOperator::BitXor)),
            lex(pad.clone(), just("<<=")).to(Some(BinaryOperator::Shl)),
            lex(pad.clone(), just(">>=")).to(Some(BinaryOperator::Shr)),
        )))
        .then(expr.clone())
        .map(|((identifier, op), value)| {
            let rhs = if let Some(op) = op {
                AstNode::new(
                    Span::new_from_spans(identifier.span, value.span),
                    AstNodeType::BinaryExpression {
                        left: Box::new(identifier.clone()),
                        right: Box::new(value),
                        operator: op,
                    },
                )
            } else {
                value
            };
            AstNode::new(
                Span::new_from_spans(identifier.span, rhs.span),
                AstNodeType::AssignmentExpression {
                    identifier: Box::new(identifier),
                    value: Box::new(rhs),
                },
            )
        })
        .or(base_expr)
        .boxed()
}
