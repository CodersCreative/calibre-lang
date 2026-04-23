use super::{LegacySpanMapExt, setup::StrParser};
use crate::parse::util::{
    auto_type, lex, match_arm_to_tuple_items, span, struct_destructure_fields_parser,
};
use crate::{
    Span,
    ast::{
        FunctionHeader, MatchArmType, MatchStringPatternPart, MatchStructFieldPattern,
        MatchTupleItem, Node, NodeType, ParserText, PotentialDollarIdentifier, PotentialNewType,
        VarType,
    },
};
use chumsky::prelude::*;
use std::sync::Arc;

pub struct MatchParsers<'a> {
    pub pad: StrParser<'a, ()>,
    pub delim: StrParser<'a, ()>,
    pub comma: StrParser<'a, ()>,
    pub arrow: StrParser<'a, ()>,
    pub ident: StrParser<'a, (String, Span)>,
    pub generic_params: StrParser<'a, crate::ast::GenericTypes>,
    pub string_lit: StrParser<'a, Node>,
    pub type_name: StrParser<'a, PotentialNewType>,
    pub expr: StrParser<'a, Node>,
    pub scope_block: StrParser<'a, Node>,
}

pub struct MatchBuilt<'a> {
    pub let_pattern_list: StrParser<'a, Vec<MatchArmType>>,
    pub fn_match_expr: StrParser<'a, Node>,
    pub match_expr: StrParser<'a, Node>,
}

pub fn build_match_parsers<'a>(
    parts: MatchParsers<'a>,
    line_starts: Arc<Vec<usize>>,
) -> MatchBuilt<'a> {
    let MatchParsers {
        pad,
        delim,
        comma,
        arrow,
        ident,
        generic_params,
        string_lit,
        type_name,
        expr,
        scope_block,
    } = parts;

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

    let match_is_type = lex(pad.clone(), just("is"))
        .ignore_then(type_name.clone())
        .try_map(|ty, sp| match ty {
            PotentialNewType::DataType(dt) => Ok(dt),
            _ => Err(Rich::custom(sp, "expected concrete type in match pattern")),
        })
        .boxed();

    let match_in_pattern = lex(pad.clone(), just("in"))
        .ignore_then(expr.clone())
        .boxed();

    let match_string_literal_part = string_lit
        .clone()
        .try_map(|node, sp| match node.node_type {
            NodeType::StringLiteral(text) => Ok(MatchStringPatternPart::Literal(text)),
            _ => Err(Rich::custom(
                sp,
                "internal parser error: expected string literal",
            )),
        })
        .boxed();

    let match_string_binding_part = optional_match_var_type
        .clone()
        .then(ident.clone())
        .map(|(var_type, (name, sp))| MatchStringPatternPart::Binding {
            var_type,
            name: PotentialDollarIdentifier::Identifier(ParserText::new(sp, name)),
        })
        .boxed();

    let match_string_wildcard_part = lex(pad.clone(), just('_'))
        .map_with_span({
            let ls = line_starts.clone();
            move |_, r| MatchStringPatternPart::Wildcard(span(ls.as_ref(), r))
        })
        .boxed();

    let match_string_part = choice((
        match_string_literal_part.clone(),
        match_string_wildcard_part.clone(),
        match_string_binding_part.clone(),
    ))
    .boxed();

    let match_string_pattern = match_string_literal_part
        .clone()
        .then(
            lex(pad.clone(), just('&'))
                .ignore_then(match_string_part.clone())
                .repeated()
                .at_least(1)
                .collect::<Vec<_>>(),
        )
        .map(|(head, mut tail)| {
            let mut parts = vec![head];
            parts.append(&mut tail);
            parts
        })
        .boxed();

    let match_struct_destructure =
        struct_destructure_fields_parser(pad.clone(), comma.clone(), ident.clone())
            .map(crate::ast::DestructurePattern::Struct)
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
            .separated_by(comma.clone())
            .allow_trailing()
            .collect::<Vec<_>>()
            .or_not()
            .map(|x| x.unwrap_or_default()),
        )
        .then_ignore(lex(pad.clone(), just(')')))
        .map(crate::ast::DestructurePattern::Tuple)
        .boxed();

    let match_tuple_item_atom = choice((
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
        match_is_type.clone().map(MatchTupleItem::IsType).boxed(),
        match_in_pattern.clone().map(MatchTupleItem::In).boxed(),
        match_string_pattern
            .clone()
            .map(MatchTupleItem::StringPattern)
            .boxed(),
        match_var_type
            .clone()
            .then(ident.clone())
            .map(|(var_type, (name, sp))| MatchTupleItem::Binding {
                var_type,
                name: PotentialDollarIdentifier::Identifier(ParserText::new(sp, name)),
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
                                Some(PotentialDollarIdentifier::Identifier(ParserText::new(
                                    sp, name,
                                ))),
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
                    value: PotentialDollarIdentifier::Identifier(ParserText::new(sp, name)),
                    var_type,
                    name: name_bind,
                    destructure,
                    pattern,
                }
            })
            .boxed(),
        expr.clone().map(MatchTupleItem::Value),
    ))
    .boxed();

    let match_tuple_at_pattern = optional_match_var_type
        .clone()
        .then(ident.clone())
        .then_ignore(lex(pad.clone(), just('@')))
        .then(match_tuple_item_atom.clone())
        .map(|((var_type, (name, sp)), pattern)| MatchTupleItem::At {
            var_type,
            name: PotentialDollarIdentifier::Identifier(ParserText::new(sp, name)),
            pattern: Box::new(pattern),
        })
        .boxed();

    let match_enum_tuple_pattern = lex(pad.clone(), just('('))
        .ignore_then(
            choice((
                match_tuple_at_pattern.clone(),
                match_tuple_item_atom.clone(),
            ))
            .separated_by(comma.clone())
            .allow_trailing()
            .collect::<Vec<_>>()
            .or_not()
            .map(|x| x.unwrap_or_default()),
        )
        .then_ignore(lex(pad.clone(), just(')')))
        .map(MatchArmType::TuplePattern)
        .boxed();

    let match_list_pattern = lex(pad.clone(), just('['))
        .ignore_then(
            choice((
                match_tuple_at_pattern.clone(),
                match_tuple_item_atom.clone(),
            ))
            .separated_by(comma.clone())
            .allow_trailing()
            .collect::<Vec<_>>()
            .or_not()
            .map(|x| x.unwrap_or_default()),
        )
        .then_ignore(lex(pad.clone(), just(']')))
        .map(MatchArmType::ListPattern)
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
                                .map(|(var_type, (name, sp))| MatchStructFieldPattern::Binding {
                                    field: String::new(),
                                    var_type,
                                    name: PotentialDollarIdentifier::Identifier(ParserText::new(
                                        sp, name,
                                    )),
                                })
                                .or(expr.clone().map(|value| MatchStructFieldPattern::Value {
                                    field: String::new(),
                                    value,
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
                        name: PotentialDollarIdentifier::Identifier(ParserText::new(sp, field)),
                    },
                })
                .separated_by(comma.clone())
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
                    comma
                        .clone()
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
                            PotentialDollarIdentifier::Identifier(ParserText::new(sp, name)),
                        )));
                    }
                    (
                        None,
                        Some(crate::ast::DestructurePattern::Tuple(items)),
                        None,
                    )
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

    let match_arm_atom = choice((
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
            move |_, r| MatchArmType::TuplePattern(vec![MatchTupleItem::Rest(span(ls.as_ref(), r))])
        }),
        lex(pad.clone(), just('_')).map_with_span({
            let ls = line_starts.clone();
            move |_, r| MatchArmType::Wildcard(span(ls.as_ref(), r))
        }),
        match_is_type.clone().map(MatchArmType::IsType),
        match_in_pattern.clone().map(MatchArmType::In),
        match_string_pattern
            .clone()
            .map(MatchArmType::StringPattern),
        match_list_pattern.clone(),
        expr.clone().map(MatchArmType::Value),
    ))
    .boxed();

    let match_at_pattern = optional_match_var_type
        .clone()
        .then(ident.clone())
        .then_ignore(lex(pad.clone(), just('@')))
        .then(match_arm_atom.clone())
        .map(|((var_type, (name, sp)), pattern)| MatchArmType::At {
            var_type,
            name: PotentialDollarIdentifier::Identifier(ParserText::new(sp, name)),
            pattern: Box::new(pattern),
        })
        .boxed();

    let match_arm_start = choice((match_at_pattern, match_arm_atom)).boxed();

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
                                    name: PotentialDollarIdentifier::Identifier(ParserText::new(
                                        sp, name,
                                    )),
                                })
                                .or(expr.clone().map(|node| MatchStructFieldPattern::Value {
                                    field: String::new(),
                                    value: node,
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
                        name: PotentialDollarIdentifier::Identifier(ParserText::new(sp, field)),
                    },
                }),))
            .separated_by(comma.clone())
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
        .then(scope_block.clone())
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
                        if let Some(mapped) = match_arm_to_tuple_items(arm) {
                            items.extend(mapped);
                        } else {
                            continue;
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
                .separated_by(choice((comma.clone().to(','), delim.clone().to(','))))
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
            comma
                .clone()
                .ignore_then(expr.clone())
                .repeated()
                .collect::<Vec<_>>(),
        )
        .map(|(first, rest)| {
            if rest.is_empty() {
                first
            } else {
                let span = Span::new(first.span.from, rest.last().unwrap_or(&first).span.to);
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
                .separated_by(choice((comma.clone().to(','), delim.clone().to(','))))
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

    MatchBuilt {
        let_pattern_list,
        fn_match_expr,
        match_expr,
    }
}
