use super::{LegacySpanMapExt, setup::StrParser};
use crate::parse::util::{
    auto_type, ensure_scope_node, ident_node, labelled_scope_parser, lex, scope_body_or_single,
    scope_node_parser, span, struct_destructure_fields_parser,
};
use crate::{
    Span,
    ast::{
        DestructurePattern, NamedScope, Node, NodeType, ObjectType, Overload, ParserDataType,
        ParserFfiInnerType, ParserInnerType, ParserText, PotentialDollarIdentifier,
        PotentialGenericTypeIdentifier, PotentialNewType, SelectArm, SelectArmKind, TraitMember,
        TraitMemberKind, TypeDefType, VarType,
    },
};
use chumsky::error::Rich;
use chumsky::prelude::*;
use std::path::Path;
use std::str::FromStr;
use std::sync::Arc;

#[derive(Clone, Copy)]
enum DeclAssignOp {
    Infer,
    Typed,
}

pub struct StatementParsers<'a> {
    pub pad: StrParser<'a, ()>,
    pub pad_with_newline: StrParser<'a, ()>,
    pub delim: StrParser<'a, ()>,
    pub comma: StrParser<'a, ()>,
    pub arrow: StrParser<'a, ()>,
    pub fat_arrow: StrParser<'a, ()>,
    pub left_arrow: StrParser<'a, ()>,
    pub raw_ident: StrParser<'a, (String, Span)>,
    pub ident: StrParser<'a, (String, Span)>,
    pub named_ident: StrParser<'a, PotentialDollarIdentifier>,
    pub generic_params: StrParser<'a, crate::ast::GenericTypes>,
    pub string_text: StrParser<'a, String>,
    pub type_name: StrParser<'a, PotentialNewType>,
    pub statement: StrParser<'a, Node>,
    pub expr: StrParser<'a, Node>,
}

pub fn build_statement_parser<'a>(
    parts: StatementParsers<'a>,
    line_starts: Arc<Vec<usize>>,
    source_path: Option<&'a Path>,
) -> StrParser<'a, Node> {
    let StatementParsers {
        pad,
        pad_with_newline,
        delim,
        comma,
        arrow,
        fat_arrow,
        left_arrow,
        raw_ident,
        ident,
        named_ident,
        generic_params,
        string_text,
        type_name,
        statement,
        expr,
    } = parts;

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
                            .separated_by(comma.clone())
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
                    .map(|(n, sp)| PotentialDollarIdentifier::Identifier(ParserText::new(sp, n)))
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
        .map_with_span({
            let ls = line_starts.clone();
            move |(values, module, alias), r| {
                Node::new(
                    span(ls.as_ref(), r),
                    NodeType::ImportStatement {
                        module,
                        alias,
                        values,
                    },
                )
            }
        });

    let type_stmt = lex(pad.clone(), just("type"))
        .ignore_then(ident.clone())
        .then(
            lex(pad.clone(), just(":<"))
                .ignore_then(
                    ident
                        .clone()
                        .separated_by(comma.clone())
                        .allow_trailing()
                        .collect::<Vec<_>>()
                        .or_not()
                        .map(|x| x.unwrap_or_default()),
                )
                .then_ignore(lex(pad.clone(), just('>')))
                .or_not(),
        )
        .then_ignore(lex(pad.clone(), just(":=")))
        .then(choice((
            lex(pad.clone(), just("struct")).ignore_then(
                lex(pad.clone(), just('{'))
                    .then_ignore(delim.clone().repeated().collect::<Vec<_>>())
                    .ignore_then(
                        raw_ident
                            .clone()
                            .repeated()
                            .at_least(1)
                            .collect::<Vec<_>>()
                            .then_ignore(lex(pad.clone(), just(':')))
                            .then(type_name.clone())
                            .separated_by(
                                choice((comma.clone().ignored(), delim.clone()))
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
                                    choice((comma.clone().ignored(), delim.clone()))
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
                            choice((comma.clone().ignored(), delim.clone()))
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
            type_name.clone().try_map(|typ, sp| match typ {
                PotentialNewType::DataType(x)
                    if x.data_type == ParserInnerType::Dynamic
                        || x.data_type == ParserInnerType::Auto(None) =>
                {
                    Err(Rich::custom(
                        sp,
                        "cannot overload `auto` or `dyn`; specify a concrete type",
                    ))
                }
                _ => Ok(TypeDefType::NewType(Box::new(typ))),
            }),
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
                        .then_ignore(lex(pad.clone(), just(":=")))
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
                PotentialGenericTypeIdentifier::Identifier(PotentialDollarIdentifier::Identifier(
                    ParserText::new(sp, name),
                ))
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
                lex(pad.clone(), just(":="))
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
        .try_map({
            let ls = line_starts.clone();
            move |(((generics, trait_ident), maybe_target), vars), parser_sp| {
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
                            _ => {
                                return Err(Rich::custom(
                                    parser_sp,
                                    "expected trait name after `impl`, found non-trait type",
                                ));
                            }
                        },
                        _ => {
                            return Err(Rich::custom(
                                parser_sp,
                                "expected trait name after `impl`",
                            ));
                        }
                    };
                    Ok(Node::new(
                        *trait_ident.span(),
                        NodeType::ImplTraitDeclaration {
                            generics,
                            trait_ident,
                            target,
                            variables: vars,
                        },
                    ))
                } else {
                    let target = trait_ident;

                    let sp = if target.span() == &Span::default() {
                        span(ls.as_ref(), parser_sp.into_range())
                    } else {
                        *target.span()
                    };

                    Ok(Node::new(
                        sp,
                        NodeType::ImplDeclaration {
                            generics,
                            target,
                            variables: vars,
                        },
                    ))
                }
            }
        });

    let let_struct_destruct_stmt = lex(pad.clone(), just("let"))
        .ignore_then(struct_destructure_fields_parser(
            pad.clone(),
            comma.clone(),
            ident.clone(),
        ))
        .then_ignore(lex(pad.clone(), just(":=")))
        .then_ignore(delim.clone().repeated())
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
            .separated_by(comma.clone())
            .at_least(2)
            .collect::<Vec<_>>(),
        )
        .then_ignore(lex(pad.clone(), just(":=")))
        .then_ignore(delim.clone().repeated())
        .then(
            expr.clone().then(
                comma
                    .clone()
                    .ignore_then(expr.clone())
                    .repeated()
                    .collect::<Vec<_>>(),
            ),
        )
        .map(|(items, (first_value, rest_values))| {
            let value = if rest_values.is_empty() {
                first_value
            } else {
                let sp = Span::new_from_spans(
                    first_value.span,
                    rest_values
                        .last()
                        .map(|n| n.span)
                        .unwrap_or(first_value.span),
                );
                let mut values = vec![first_value];
                values.extend(rest_values);
                Node::new(sp, NodeType::TupleLiteral { values })
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
    .then(choice((
        lex(pad.clone(), just(":=")).to(DeclAssignOp::Infer),
        lex(pad.clone(), just("=")).to(DeclAssignOp::Typed),
    )))
    .then_ignore(delim.clone().repeated())
    .then(
        expr.clone().then(
            comma
                .clone()
                .ignore_then(expr.clone())
                .repeated()
                .collect::<Vec<_>>(),
        ),
    )
    .try_map(
        |(((((vt, m), name), ty), op), (first_value, rest_values)), sp| {
            match (ty.is_some(), op) {
                (true, DeclAssignOp::Infer) => {
                    return Err(Rich::custom(
                        sp,
                        "expected `=` when a variable type is specified",
                    ));
                }
                (false, DeclAssignOp::Typed) => {
                    return Err(Rich::custom(
                        sp,
                        "expected `:=` when a variable type is not specified",
                    ));
                }
                _ => {}
            }
            let var_type = match (vt, m.is_some()) {
                (VarType::Immutable, true) => VarType::Mutable,
                (VarType::Constant, true) => {
                    return Err(Rich::custom(
                        sp,
                        "constant cannot be mutable; use `let mut` if mutability is required",
                    ));
                }
                (x, _) => x,
            };
            let value = if rest_values.is_empty() {
                first_value
            } else {
                let first_span = first_value.span;
                let tuple_span = Span::new_from_spans(
                    first_span,
                    rest_values.last().map(|n| n.span).unwrap_or(first_span),
                );

                match first_value.node_type {
                    NodeType::EnumExpression {
                        identifier,
                        value,
                        data,
                    } => {
                        let mut payload_values = if let Some(existing) = data {
                            match existing.node_type {
                                NodeType::TupleLiteral { values } => values,
                                other => vec![Node::new(existing.span, other)],
                            }
                        } else {
                            Vec::new()
                        };
                        payload_values.extend(rest_values);

                        let payload = if payload_values.len() == 1 {
                            payload_values.into_iter().next()
                        } else {
                            Some(Node::new(
                                Span::new_from_spans(
                                    payload_values.first().map(|n| n.span).unwrap_or(tuple_span),
                                    payload_values.last().map(|n| n.span).unwrap_or(tuple_span),
                                ),
                                NodeType::TupleLiteral {
                                    values: payload_values,
                                },
                            ))
                        };

                        Node::new(
                            tuple_span,
                            NodeType::EnumExpression {
                                identifier,
                                value,
                                data: payload.map(Box::new),
                            },
                        )
                    }
                    other => {
                        let mut values = vec![Node::new(first_span, other)];
                        values.extend(rest_values);
                        Node::new(tuple_span, NodeType::TupleLiteral { values })
                    }
                }
            };

            let value_span = value.span;
            Ok(Node::new(
                Span::new_from_spans(*name.span(), value_span),
                NodeType::VariableDeclaration {
                    var_type,
                    identifier: name,
                    value: Box::new(value),
                    data_type: ty.unwrap_or_else(|| auto_type(value_span)),
                },
            ))
        },
    );

    let scope_name = lex(pad.clone(), just('@'))
        .ignore_then(ident.clone())
        .map(|(n, sp)| PotentialDollarIdentifier::Identifier(ParserText::new(sp, n)))
        .boxed();

    let scope_args = lex(pad.clone(), just('['))
        .ignore_then(
            lex(pad.clone(), just('$'))
                .ignore_then(raw_ident.clone())
                .map(|(n, sp)| PotentialDollarIdentifier::Identifier(ParserText::new(sp, n)))
                .then_ignore(lex(pad.clone(), just(":=")).ignored())
                .then(
                    lex(pad.clone(), just("type"))
                        .ignore_then(lex(pad.clone(), just(':')))
                        .ignore_then(type_name.clone())
                        .map(|data_type| {
                            Node::new(*data_type.span(), NodeType::DataType { data_type })
                        })
                        .or(expr.clone()),
                )
                .separated_by(comma.clone())
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
            .ignore_then(statement.clone())
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
        .map_with_span({
            let ls = line_starts.clone();
            move |(named, (body, create_new_scope)), r| {
                Node::new(
                    span(ls.as_ref(), r),
                    NodeType::ScopeDeclaration {
                        body,
                        named,
                        is_temp: true,
                        create_new_scope,
                        define: true,
                    },
                )
            }
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
        .map_with_span({
            let ls = line_starts.clone();
            move |(((identifier, name), args), create_new_scope), r| {
                Node::new(
                    span(ls.as_ref(), r),
                    NodeType::ScopeAlias {
                        identifier,
                        value: NamedScope { name, args },
                        create_new_scope,
                    },
                )
            }
        })
        .boxed();

    let scope_call_stmt = fat_arrow
        .clone()
        .ignored()
        .ignore_then(named_scope.or_not())
        .then(scope_body_with_mode)
        .map_with_span({
            let ls = line_starts.clone();
            move |(named, (body, create_new_scope)), r| {
                Node::new(
                    span(ls.as_ref(), r),
                    NodeType::ScopeDeclaration {
                        body,
                        named,
                        is_temp: true,
                        create_new_scope,
                        define: false,
                    },
                )
            }
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

    let labelled_scope = labelled_scope_parser(
        pad.clone(),
        ident.clone(),
        scope_node_parser(statement.clone(), delim.clone(), pad.clone()),
    )
    .boxed();

    let arrow_body_expr = fat_arrow
        .clone()
        .ignore_then(
            labelled_scope
                .clone()
                .or(scope_node_parser(
                    statement.clone(),
                    delim.clone(),
                    pad.clone(),
                ))
                .or(pad_with_newline
                    .clone()
                    .ignore_then(statement.clone())
                    .map(|body| ensure_scope_node(body, true, false))),
        )
        .boxed();

    let test_stmt = lex(pad.clone(), just("test"))
        .ignore_then(ident.clone())
        .then(arrow_body_expr.clone())
        .map(|((name, sp), body)| {
            Node::new(
                Span::new_from_spans(sp, body.span),
                NodeType::TestDeclaration {
                    identifier: PotentialDollarIdentifier::Identifier(ParserText::new(sp, name)),
                    body: Box::new(body),
                },
            )
        })
        .boxed();

    let bench_stmt = lex(pad.clone(), just("bench"))
        .ignore_then(ident.clone())
        .then(arrow_body_expr.clone())
        .map(|((name, sp), body)| {
            Node::new(
                Span::new_from_spans(sp, body.span),
                NodeType::BenchDeclaration {
                    identifier: PotentialDollarIdentifier::Identifier(ParserText::new(sp, name)),
                    body: Box::new(body),
                },
            )
        })
        .boxed();

    let ffi_type = recursive(|ffi_type| {
        let ffi_base = lex(pad.clone(), just('@'))
            .ignore_then(raw_ident.clone())
            .map(|(name, sp)| {
                let parsed = ParserFfiInnerType::from_str(&name).unwrap_or(ParserFfiInnerType::Int);
                ParserDataType::new(sp, ParserInnerType::FfiType(parsed))
            })
            .or(type_name.clone().try_map(|x, sp| match x {
                PotentialNewType::DataType(dt) => Ok(dt),
                _ => Err(Rich::custom(sp, "expected FFI data type")),
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
        .then_ignore(lex(pad.clone(), just(":=")))
        .then_ignore(lex(pad.clone(), just("fn")))
        .then_ignore(lex(pad.clone(), just('(')))
        .then(
            ffi_type
                .clone()
                .separated_by(comma.clone())
                .allow_trailing()
                .collect::<Vec<_>>()
                .or_not()
                .map(|x: Option<Vec<ParserDataType>>| x.unwrap_or_default()),
        )
        .then_ignore(lex(pad.clone(), just(')')))
        .then(arrow.clone().ignore_then(ffi_type.clone()).or_not())
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
                        return_type: return_type
                            .unwrap_or_else(|| ParserDataType::new(sp, ParserInnerType::Null)),
                        library,
                        symbol,
                    },
                )
            },
        )
        .boxed();

    let select_arm = choice((
        lex(pad.clone(), just('_'))
            .then(arrow_body_expr.clone())
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
            .then(arrow_body_expr.clone())
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
                    choice((delim.clone(), comma.clone().ignored()))
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
        .map_with_span({
            let ls = line_starts.clone();
            move |arms, r| Node::new(span(ls.as_ref(), r), NodeType::SelectStatement { arms })
        })
        .boxed();

    let spawn_block = lex(pad.clone(), just('{'))
        .ignore_then(delim.clone().repeated().collect::<Vec<_>>())
        .ignore_then(
            arrow_body_expr
                .clone()
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
        .boxed();

    let spawn_stmt = lex(
        pad.clone(),
        just("spawn")
            .then(just('@').or_not())
            .map(|(_, at)| at.is_some()),
    )
    .then(choice((spawn_block, expr.clone().map(|x| vec![x]))))
    .map_with_span({
        let ls = line_starts.clone();
        move |(auto_wait, items), r| {
            let parser_span = span(ls.as_ref(), r);
            let sp = if let (Some(a), Some(b)) = (items.first(), items.last()) {
                Span::new_from_spans(a.span, b.span)
            } else {
                parser_span
            };
            Node::new(sp, NodeType::Spawn { items, auto_wait })
        }
    })
    .boxed();

    let destruct_assign_stmt = ident
        .clone()
        .then(
            comma
                .clone()
                .ignore_then(ident.clone())
                .repeated()
                .at_least(1)
                .collect::<Vec<_>>(),
        )
        .then_ignore(lex(pad.clone(), just(":=")))
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
        expr,
    ))
    .boxed()
}
