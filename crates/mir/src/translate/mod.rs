use crate::{
    ast::{
        MiddleNode, MiddleNodeType, MirAggregate, MirAs, MirAssignment, MirBig, MirBinary,
        MirBoolean, MirBreak, MirChar, MirComparison, MirConditional, MirContinue, MirDebug,
        MirDeref, MirDrop, MirEmit, MirEnum, MirFloat, MirInt, MirIs, MirListBuilder, MirMove,
        MirNeg, MirRange, MirRef, MirReturn, MirScopeDecl, MirSpawn, MirString, MirVarDecl,
    },
    environment::MiddleEnvironment,
    errors::MiddleErr,
    scoping::ScopeId,
    symbols::resolve::{ResolutionOptions, StrOrAstNode},
    tags::TagInfo,
    typing::{
        MiddleImplMember, MiddleObject, MiddleTrait, MiddleTraitMember, MiddleTypeDefType, Typing,
    },
};
use calibre_parser::{
    Span,
    ast::{
        ObjectMap, ObjectType, Operator, RefMutability,
        comparison::{BooleanOperator, ComparisonOperator},
        generics::TraitMemberKind,
        idents::{
            IntLiteralType, ParsedIntLiteral, ParserText, PotentialDollarIdentifier,
            PotentialGenericTypeIdentifier,
        },
        matching::{MatchArmType, SelectArmKind, TryCatch},
        nodes::{
            AsFailureMode, AstNode, AstNodeType, CallArg, EmitType, FunctionHeader,
            IfComparisonType, LoopType, PipeSegment, TypeDefType, VarType,
        },
        types::{GenericTypes, ParserDataType, ParserInnerType},
    },
};
use tracing::{debug, instrument, trace};
use ustr::{Ustr, UstrMap, UstrSet};

pub mod functions;
pub mod iter;
pub mod loops;
pub mod matches;
pub mod member;
pub mod scopes;
pub mod statements;

impl MiddleEnvironment {
    pub fn compare_types(
        &self,
        type1: Option<ParserDataType>,
        type2: Option<ParserDataType>,
        overload_tag: Option<&TagInfo>,
    ) -> Result<ParserDataType, MiddleErr> {
        if !self.context.type_check {
            return match (type1, type2) {
                (Some(x), None) => Ok(x),
                (None, Some(x)) => Ok(x),
                (Some(x), _) => Ok(x),
                (None, None) => Err(self.context.err_at_current(MiddleErr::InferImpossible)),
            };
        }

        match (type1, type2) {
            (None, None) => Err(self.context.err_at_current(MiddleErr::InferImpossible)),
            (Some(x), None) => Ok(x),
            (None, Some(x)) => Ok(x),
            (Some(x), Some(_))
                if overload_tag.is_some_and(|x| self.tagging.tag_info.contains(x)) =>
            {
                Ok(x)
            }
            (Some(x), Some(y)) => {
                // TODO Handle generics better
                if x.loose_eq(&y)
                    || self
                        .scoping
                        .all_time_generics
                        .contains(&Ustr::from(&y.impl_name()))
                    || self
                        .scoping
                        .all_time_generics
                        .contains(&Ustr::from(&x.impl_name()))
                {
                    Ok(x)
                } else {
                    Err(self.context.err_at_current(MiddleErr::InvalidType {
                        expected: Box::new(x.clone()),
                        found: Box::new(y.clone()),
                    }))
                }
            }
        }
    }

    pub fn compare_types_ref(
        &self,
        type1: Option<&ParserDataType>,
        type2: Option<&ParserDataType>,
        overload_tag: Option<&TagInfo>,
    ) -> Result<(), MiddleErr> {
        if !self.context.type_check {
            return Ok(());
        }

        match (type1, type2) {
            (None, None) => Err(self.context.err_at_current(MiddleErr::InferImpossible)),
            (Some(_), None) => Ok(()),
            (None, Some(_)) => Ok(()),
            (Some(_), Some(_))
                if overload_tag.is_some_and(|x| self.tagging.tag_info.contains(x)) =>
            {
                Ok(())
            }
            (Some(x), Some(y)) => {
                // TODO Handle generics better
                if x.loose_eq(y)
                    || self
                        .scoping
                        .all_time_generics
                        .contains(&Ustr::from(&y.impl_name()))
                    || self
                        .scoping
                        .all_time_generics
                        .contains(&Ustr::from(&x.impl_name()))
                {
                    Ok(())
                } else {
                    Err(self.context.err_at_current(MiddleErr::InvalidType {
                        expected: Box::new(x.clone()),
                        found: Box::new(y.clone()),
                    }))
                }
            }
        }
    }

    #[instrument(skip_all)]
    pub fn evaluate(&mut self, scope: ScopeId, node: AstNode) -> MiddleNode {
        let span = node.span;
        match self.evaluate_inner(scope, node) {
            Ok(node) => node,
            Err(err) => {
                debug!(error = %err, "evaluation failed, pushing error");
                self.context.push_error(err);
                MiddleNode::new(MiddleNodeType::EmptyLine, span)
            }
        }
    }

    #[instrument(skip_all)]
    pub fn evaluate_inner(
        &mut self,
        scope: ScopeId,
        node: AstNode,
    ) -> Result<MiddleNode, MiddleErr> {
        self.context.current_location = self.scoping.get_location(scope, node.span);
        trace!(location = ?self.context.current_location, "evaluating node");

        match node.node_type {
            AstNodeType::DataType { .. } => unreachable!(),
            AstNodeType::Null => Ok(MiddleNode {
                node_type: MiddleNodeType::Null,
                span: node.span,
            }),
            AstNodeType::Defer { value, function } => {
                if function {
                    self.symbols.func_defers.push(*value);
                } else {
                    let scope_data = self.scoping.scope_mut_or_err(scope)?;
                    scope_data.defers.push(*value);
                }
                Ok(MiddleNode {
                    node_type: MiddleNodeType::EmptyLine,
                    span: node.span,
                })
            }
            AstNodeType::Identifier(x) => Ok(MiddleNode::identifier(
                node.span,
                match self.resolve_potential_node(scope, &x, ResolutionOptions::idents())? {
                    StrOrAstNode::Str(x) => x,
                    StrOrAstNode::Node(x) => return self.evaluate_inner(scope, *x),
                },
            )),
            AstNodeType::IntLiteral(text) => Ok(MiddleNode {
                node_type: MiddleNodeType::IntLiteral(MirInt {
                    value: ParsedIntLiteral::parse(text.clone()).ok_or_else(|| {
                        MiddleErr::At(
                            node.span,
                            Box::new(MiddleErr::Internal(format!(
                                "invalid integer literal {text}"
                            ))),
                        )
                    })?,
                }),
                span: node.span,
            }),
            AstNodeType::BigLiteral(x) => Ok(MiddleNode {
                node_type: MiddleNodeType::BigLiteral(MirBig {
                    value: x
                        .text
                        .strip_suffix('g')
                        .map(Ustr::from)
                        .unwrap_or(Ustr::from(&x.text)),
                }),
                span: node.span,
            }),
            AstNodeType::FloatLiteral(x) => Ok(MiddleNode {
                node_type: MiddleNodeType::FloatLiteral(MirFloat { value: x }),
                span: node.span,
            }),
            AstNodeType::StringLiteral(x) => Ok(MiddleNode {
                node_type: MiddleNodeType::StringLiteral(MirString {
                    value: Ustr::from(&x.text),
                }),
                span: node.span,
            }),
            AstNodeType::CharLiteral(x) => Ok(MiddleNode {
                node_type: MiddleNodeType::CharLiteral(MirChar { value: x }),
                span: node.span,
            }),
            AstNodeType::RangeDeclaration {
                from,
                to,
                inclusive,
            } => {
                if !self.context.type_check {
                    let from_type = self.resolve_type_from_node(scope, &from);
                    let to_type = self.resolve_type_from_node(scope, &to);

                    let data_type = self.compare_types(
                        from_type,
                        to_type,
                        Some(&TagInfo::IgnoreInvalidTypeCheck),
                    )?;
                    if !data_type.clone().is_int() {
                        return Err(self.context.err_at_current(MiddleErr::InvalidType {
                            expected: Box::new(ParserDataType::new(
                                node.span,
                                ParserInnerType::Int,
                            )),
                            found: Box::new(data_type),
                        }));
                    }
                }

                Ok(MiddleNode {
                    node_type: MiddleNodeType::RangeDeclaration(MirRange {
                        from: Box::new(self.evaluate(scope, *from)),
                        to: Box::new(self.evaluate(scope, *to)),
                        inclusive,
                    }),
                    span: node.span,
                })
            }
            AstNodeType::Emit(EmitType::Channel { channel, value }) => {
                if !self.context.type_check {
                    let channel_ty = self.resolve_type_from_node(scope, &channel);
                    let expected = self.resolve_to_data_type(scope, &"Channel").ok();
                    self.compare_types_ref(
                        expected.as_ref(),
                        channel_ty.as_ref(),
                        Some(&TagInfo::IgnoreInvalidTypeCheck),
                    )?;
                }

                self.evaluate_inner(
                    scope,
                    AstNode::call(
                        node.span,
                        AstNode::member(node.span, *channel, "send"),
                        vec![CallArg::Value(*value)],
                    ),
                )
            }
            AstNodeType::Emit(EmitType::Scope(value)) => Ok(MiddleNode::new(
                MiddleNodeType::Emit(MirEmit {
                    value: Box::new(self.evaluate(scope, *value)),
                }),
                node.span,
            )),
            AstNodeType::FieldAccess { base, field } => {
                self.evaluate_field_access(scope, node.span, *base, field)
            }
            AstNodeType::ScopeAccess { base, field } => {
                self.evaluate_scope_access(scope, node.span, *base, field)
            }
            AstNodeType::IndexAccess { base, index } => {
                self.evaluate_index_access(scope, node.span, *base, *index)
            }
            AstNodeType::Spawn {
                items,
                auto_wait: true,
            } => {
                let ident: PotentialDollarIdentifier =
                    ParserText::temp_name_with_suffix("spawn_wait_wg", node.span).into();

                Ok(self.evaluate(
                    scope,
                    AstNode::new_temp_scope_with_create(
                        vec![
                            AstNode::new(
                                node.span,
                                AstNodeType::VariableDeclaration {
                                    var_type: VarType::Immutable,
                                    identifier: ident.clone(),
                                    data_type: ParserDataType::object(node.span, "WaitGroup"),
                                    value: Box::new(AstNode::new(
                                        node.span,
                                        AstNodeType::Spawn {
                                            items,
                                            auto_wait: false,
                                        },
                                    )),
                                },
                            ),
                            AstNode::call(
                                node.span,
                                AstNode::member(
                                    node.span,
                                    AstNode::identifier(node.span, ident),
                                    "wait",
                                ),
                                Vec::new(),
                            ),
                        ],
                        Some(false),
                    ),
                ))
            }
            AstNodeType::Spawn {
                mut items,
                auto_wait: _,
            } if items.len() == 1 => {
                let value: AstNode = items.remove(0);

                let inner = match value.node_type {
                    AstNodeType::ScopeDeclaration { .. } => AstNode::new(
                        node.span,
                        AstNodeType::FunctionDeclaration {
                            header: FunctionHeader {
                                generics: GenericTypes::default(),
                                parameters: Vec::new(),
                                return_type: ParserDataType::auto(node.span),
                                param_destructures: Vec::new(),
                            },
                            body: Box::new(value),
                        },
                    ),
                    AstNodeType::CallExpression { .. } => AstNode::new(
                        node.span,
                        AstNodeType::FunctionDeclaration {
                            header: FunctionHeader {
                                generics: GenericTypes::default(),
                                parameters: Vec::new(),
                                return_type: ParserDataType::auto(node.span),
                                param_destructures: Vec::new(),
                            },
                            body: Box::new(AstNode::new_temp_scope(vec![value])),
                        },
                    ),
                    AstNodeType::LoopDeclaration {
                        loop_type,
                        body,
                        until,
                        label,
                        else_body,
                    } => {
                        let ident: PotentialDollarIdentifier =
                            ParserText::temp_name_with_suffix("spawn_wg", node.span)
                                .clone()
                                .into();

                        let decl = AstNode::new(
                            node.span,
                            AstNodeType::VariableDeclaration {
                                var_type: VarType::Mutable,
                                identifier: ident.clone(),
                                data_type: ParserDataType::auto(node.span),
                                value: Box::new(AstNode::call(
                                    node.span,
                                    AstNode::member(
                                        node.span,
                                        AstNode::identifier(node.span, "WaitGroup"),
                                        "new",
                                    ),
                                    Vec::new(),
                                )),
                            },
                        );

                        let join_call = AstNode::call(
                            node.span,
                            AstNode::member(
                                node.span,
                                AstNode::identifier(node.span, ident.clone()),
                                "join",
                            ),
                            vec![CallArg::Value(AstNode::new(
                                node.span,
                                AstNodeType::Spawn {
                                    items: vec![*body],
                                    auto_wait: false,
                                },
                            ))],
                        );

                        let loop_node = AstNode::new(
                            node.span,
                            AstNodeType::LoopDeclaration {
                                loop_type,
                                body: Box::new(AstNode::new_temp_scope_with_create(
                                    vec![join_call],
                                    Some(false),
                                )),
                                until,
                                label,
                                else_body,
                            },
                        );
                        return self.evaluate_inner(
                            scope,
                            AstNode::new_temp_scope(vec![
                                decl,
                                loop_node,
                                AstNode::new(
                                    node.span,
                                    AstNodeType::Emit(EmitType::Scope(Box::new(
                                        AstNode::identifier(node.span, ident),
                                    ))),
                                ),
                            ]),
                        );
                    }
                    AstNodeType::FunctionDeclaration { .. } => value,
                    _ => unimplemented!(),
                };

                Ok(MiddleNode::new(
                    MiddleNodeType::Spawn(MirSpawn {
                        value: Box::new(self.evaluate_inner(scope, inner)?),
                    }),
                    node.span,
                ))
            }
            AstNodeType::Spawn { items, .. } => {
                let span = node.span;
                let ident: PotentialDollarIdentifier =
                    ParserText::temp_name_with_suffix("spawn_wg", span).into();

                let mut body = vec![AstNode::new(
                    span,
                    AstNodeType::VariableDeclaration {
                        var_type: VarType::Mutable,
                        identifier: ident.clone(),
                        data_type: ParserDataType::object(span, "WaitGroup"),
                        value: Box::new(AstNode::call(
                            span,
                            AstNode::member(span, AstNode::identifier(span, "WaitGroup"), "new"),
                            Vec::new(),
                        )),
                    },
                )];

                for item in items {
                    let item = match item.node_type {
                        AstNodeType::Spawn { .. } => item,
                        other => AstNode::new(
                            item.span,
                            AstNodeType::Spawn {
                                items: vec![AstNode::new(item.span, other)],
                                auto_wait: false,
                            },
                        ),
                    };

                    let join_call = AstNode::call(
                        span,
                        AstNode::member(span, AstNode::identifier(span, "WaitGroup"), "join"),
                        vec![
                            CallArg::Value(AstNode::new(
                                span,
                                AstNodeType::RefStatement {
                                    mutability: RefMutability::MutRef,
                                    value: Box::new(AstNode::identifier(span, &ident)),
                                },
                            )),
                            CallArg::Value(item),
                        ],
                    );
                    body.push(join_call);
                }

                body.push(AstNode::new(
                    span,
                    AstNodeType::Emit(EmitType::Scope(Box::new(AstNode::identifier(span, ident)))),
                ));

                self.evaluate_inner(
                    scope,
                    AstNode::new(
                        span,
                        AstNodeType::ScopeDeclaration {
                            body: Some(body),
                            named: None,
                            is_temp: true,
                            create_new_scope: Some(false),
                            define: false,
                        },
                    ),
                )
            }
            AstNodeType::Ternary {
                comparison,
                then,
                otherwise,
            } => {
                if !self.context.type_check {
                    let then_type = self.resolve_type_from_node(scope, &then);
                    let otherwise_type = self.resolve_type_from_node(scope, &otherwise);

                    if !then_type.as_ref().is_some_and(|x| x.is_null()) {
                        self.compare_types_ref(
                            then_type.as_ref(),
                            otherwise_type.as_ref(),
                            Some(&TagInfo::IgnoreInvalidTypeCheck),
                        )?;
                    }
                }

                self.evaluate_inner(
                    scope,
                    AstNode {
                        node_type: AstNodeType::IfStatement {
                            comparison: Box::new(IfComparisonType::If(*comparison)),
                            then,
                            otherwise: Some(otherwise),
                        },
                        span: node.span,
                    },
                )
            }
            AstNodeType::MoveExpression { value } => match value.node_type {
                AstNodeType::Identifier(x) => Ok(MiddleNode {
                    node_type: MiddleNodeType::Move(MirMove {
                        identifier: self.resolve(scope, &x, ResolutionOptions::idents())?,
                    }),
                    span: node.span,
                }),
                AstNodeType::FieldAccess { base, field } => {
                    let tmp_ident: PotentialDollarIdentifier =
                        ParserText::temp_name_with_suffix("move", node.span).into();

                    let tmp_decl = AstNode::new(
                        node.span,
                        AstNodeType::VariableDeclaration {
                            var_type: VarType::Immutable,
                            identifier: tmp_ident.clone(),
                            data_type: ParserDataType::auto(node.span),
                            value: Box::new(AstNode::new(
                                node.span,
                                AstNodeType::MoveExpression {
                                    value: Box::new(*base),
                                },
                            )),
                        },
                    );

                    let moved_base = AstNode::new(
                        node.span,
                        AstNodeType::Identifier(PotentialGenericTypeIdentifier::Identifier(
                            tmp_ident,
                        )),
                    );
                    let member = AstNode::new(
                        node.span,
                        AstNodeType::FieldAccess {
                            base: Box::new(moved_base),
                            field,
                        },
                    );

                    self.evaluate_inner(scope, AstNode::new_temp_scope(vec![tmp_decl, member]))
                }
                AstNodeType::ScopeAccess { base, field } => {
                    let tmp_ident: PotentialDollarIdentifier =
                        ParserText::temp_name_with_suffix("move", node.span).into();

                    let tmp_decl = AstNode::new(
                        node.span,
                        AstNodeType::VariableDeclaration {
                            var_type: VarType::Immutable,
                            identifier: tmp_ident.clone(),
                            data_type: ParserDataType::auto(node.span),
                            value: Box::new(AstNode::new(
                                node.span,
                                AstNodeType::MoveExpression {
                                    value: Box::new(*base),
                                },
                            )),
                        },
                    );

                    let moved_base = AstNode::new(
                        node.span,
                        AstNodeType::Identifier(PotentialGenericTypeIdentifier::Identifier(
                            tmp_ident,
                        )),
                    );
                    let member = AstNode::new(
                        node.span,
                        AstNodeType::ScopeAccess {
                            base: Box::new(moved_base),
                            field,
                        },
                    );

                    self.evaluate_inner(scope, AstNode::new_temp_scope(vec![tmp_decl, member]))
                }
                AstNodeType::IndexAccess { base, index } => {
                    let tmp_ident: PotentialDollarIdentifier =
                        ParserText::temp_name_with_suffix("move", node.span).into();

                    let tmp_decl = AstNode::new(
                        node.span,
                        AstNodeType::VariableDeclaration {
                            var_type: VarType::Immutable,
                            identifier: tmp_ident.clone(),
                            data_type: ParserDataType::auto(node.span),
                            value: Box::new(AstNode::new(
                                node.span,
                                AstNodeType::MoveExpression {
                                    value: Box::new(*base),
                                },
                            )),
                        },
                    );

                    let moved_base = AstNode::new(
                        node.span,
                        AstNodeType::Identifier(PotentialGenericTypeIdentifier::Identifier(
                            tmp_ident,
                        )),
                    );
                    let member = AstNode::new(
                        node.span,
                        AstNodeType::IndexAccess {
                            base: Box::new(moved_base),
                            index,
                        },
                    );

                    self.evaluate_inner(scope, AstNode::new_temp_scope(vec![tmp_decl, member]))
                }
                _ => self.evaluate_inner(scope, *value),
            },
            AstNodeType::TupleLiteral { values } => {
                let span = node.span;

                self.evaluate_inner(
                    scope,
                    AstNode::call(
                        span,
                        AstNode::identifier(span, "tuple"),
                        values.into_iter().map(CallArg::Value).collect(),
                    ),
                )
            }
            AstNodeType::Drop(x) => Ok(MiddleNode {
                node_type: MiddleNodeType::Drop(MirDrop {
                    identifier: self.resolve(scope, &x, ResolutionOptions::idents())?,
                }),
                span: node.span,
            }),
            AstNodeType::IfStatement {
                comparison,
                then,
                otherwise,
            } => {
                if !self.context.type_check {
                    let then_type = self.resolve_type_from_node(scope, &then);
                    let otherwise_type = otherwise
                        .as_ref()
                        .and_then(|x| self.resolve_type_from_node(scope, x))
                        .unwrap_or_else(|| ParserDataType::null(node.span));

                    self.compare_types_ref(
                        then_type.as_ref(),
                        Some(&otherwise_type),
                        Some(&TagInfo::IgnoreInvalidTypeCheck),
                    )?;
                }

                match *comparison {
                    IfComparisonType::If(x) => Ok(MiddleNode {
                        node_type: MiddleNodeType::Conditional(MirConditional {
                            comparison: Box::new(self.evaluate(scope, x)),
                            then: Box::new(self.evaluate(scope, *then)),
                            otherwise: otherwise.map(|x| Box::new(self.evaluate(scope, *x))),
                        }),
                        span: node.span,
                    }),
                    IfComparisonType::IfLet { value, pattern } => self.evaluate_inner(
                        scope,
                        AstNode {
                            node_type: AstNodeType::MatchStatement {
                                value: Some(Box::new(value)),
                                body: {
                                    let mut lst: Vec<(MatchArmType, Vec<AstNode>, Box<AstNode>)> =
                                        pattern
                                            .0
                                            .clone()
                                            .into_iter()
                                            .map(|x| (x, pattern.1.clone(), then.clone()))
                                            .collect();

                                    lst.push((
                                        MatchArmType::Wildcard(Span::default()),
                                        Vec::new(),
                                        otherwise.unwrap_or(Box::new(AstNode {
                                            node_type: AstNodeType::EmptyLine,
                                            span: Span::default(),
                                        })),
                                    ));

                                    lst
                                },
                            },
                            span: node.span,
                        },
                    ),
                }
            }
            AstNodeType::Until { condition } => self.evaluate_inner(
                scope,
                AstNode {
                    node_type: AstNodeType::IfStatement {
                        comparison: Box::new(IfComparisonType::If(*condition)),
                        then: Box::new(AstNode {
                            node_type: AstNodeType::Break {
                                label: None,
                                value: None,
                            },
                            span: node.span,
                        }),
                        otherwise: None,
                    },
                    span: node.span,
                },
            ),
            AstNodeType::Break { label, value } => Ok(MiddleNode {
                node_type: {
                    let mut lst = Vec::new();

                    let raw_label_text = label.as_ref().map(|l| l.to_string());
                    let label_text = label.as_ref().and_then(|l| {
                        self.resolve(scope, l, ResolutionOptions::default().with_dollar())
                            .ok()
                    });

                    let (result_target, broke_target, target_scope) = {
                        let target_ctx = if label.is_some() {
                            self.scoping.loop_stack.iter().rev().find(|ctx| {
                                label_text
                                    .as_ref()
                                    .is_some_and(|l| ctx.label.as_deref() == Some(l.as_str()))
                                    || raw_label_text
                                        .as_ref()
                                        .is_some_and(|l| ctx.label.as_deref() == Some(l.as_str()))
                            })
                        } else {
                            self.scoping.loop_stack.last()
                        };
                        (
                            target_ctx.and_then(|ctx| ctx.result_target),
                            target_ctx.and_then(|ctx| ctx.broke_target),
                            target_ctx.map(|ctx| ctx.scope_id),
                        )
                    };
                    let has_break_value = value.is_some();
                    let value_node = value.map(|v| self.evaluate(scope, *v));

                    if has_break_value && let Some(result_target) = result_target {
                        let assign = MiddleNode::new(
                            MiddleNodeType::AssignmentExpression(MirAssignment {
                                identifier: Box::new(MiddleNode::identifier(
                                    self.context.current_span(),
                                    result_target,
                                )),
                                value: Box::new(value_node.unwrap_or(MiddleNode::new(
                                    MiddleNodeType::Null,
                                    self.context.current_span(),
                                ))),
                            }),
                            self.context.current_span(),
                        );
                        lst.push(assign);
                    } else if let Some(val) = value_node {
                        lst.push(val);
                    }

                    if has_break_value && let Some(broke_target) = broke_target {
                        let assign = MiddleNode::new(
                            MiddleNodeType::AssignmentExpression(MirAssignment {
                                identifier: Box::new(MiddleNode::identifier(
                                    self.context.current_span(),
                                    broke_target,
                                )),
                                value: Box::new(MiddleNode::new(
                                    MiddleNodeType::IntLiteral(MirInt {
                                        value: ParsedIntLiteral {
                                            value: 1,
                                            int_type: IntLiteralType::Int,
                                        },
                                    }),
                                    self.context.current_span(),
                                )),
                            }),
                            self.context.current_span(),
                        );
                        lst.push(assign);
                    }

                    if let Some(target_scope) = target_scope {
                        let chain_defers =
                            self.scoping.collect_defers_until(scope, Some(target_scope));

                        for x in chain_defers {
                            lst.push(self.evaluate(scope, x));
                        }
                    } else if let Ok(s) = self.scoping.scope_or_err(scope) {
                        for x in s.defers.clone() {
                            lst.push(self.evaluate(scope, x));
                        }
                    }

                    let break_node = MiddleNode::new(
                        MiddleNodeType::Break(MirBreak {
                            label: label_text.or(raw_label_text.map(|x| Ustr::from(&x))),
                            value: None,
                        }),
                        self.context.current_span(),
                    );

                    if lst.is_empty() {
                        return Ok(MiddleNode::new(break_node.node_type, node.span));
                    }

                    lst.push(break_node);

                    MiddleNodeType::ScopeDeclaration(MirScopeDecl {
                        body: lst,
                        create_new_scope: false,
                        is_temp: true,
                        scope_id: scope,
                    })
                },
                span: node.span,
            }),
            AstNodeType::Continue { label } => Ok(MiddleNode {
                node_type: {
                    let mut lst = Vec::new();

                    let raw_label_text = label.as_ref().map(|l| l.to_string());
                    let label_text = label.as_ref().and_then(|l| {
                        self.resolve(scope, l, ResolutionOptions::default().with_dollar())
                            .ok()
                    });

                    let continue_ctx = if label.is_some() {
                        self.scoping
                            .loop_stack
                            .iter()
                            .rev()
                            .find(|ctx| {
                                label_text
                                    .as_ref()
                                    .is_some_and(|l| ctx.label.as_deref() == Some(l.as_str()))
                                    || raw_label_text
                                        .as_ref()
                                        .is_some_and(|l| ctx.label.as_deref() == Some(l.as_str()))
                            })
                            .cloned()
                    } else {
                        self.scoping.loop_stack.last().cloned()
                    };

                    if let Some(ctx) = continue_ctx.as_ref() {
                        let chain_defers =
                            self.scoping.collect_defers_until(scope, Some(ctx.scope_id));

                        for x in chain_defers {
                            lst.push(self.evaluate(scope, x));
                        }
                    } else if let Ok(s) = self.scoping.scope_or_err(scope) {
                        for x in s.defers.clone() {
                            lst.push(self.evaluate(scope, x));
                        }
                    }

                    if let Some(ctx) = continue_ctx.clone()
                        && let Some(inject) = ctx.continue_inject.clone()
                    {
                        lst.push(self.evaluate(scope, inject));
                    }

                    let cont_node = MiddleNode::new(
                        MiddleNodeType::Continue(MirContinue {
                            label: label_text.or(raw_label_text.map(|x| Ustr::from(&x))),
                        }),
                        self.context.current_span(),
                    );

                    if lst.is_empty() {
                        return Ok(MiddleNode::new(cont_node.node_type, node.span));
                    }

                    lst.push(cont_node);

                    MiddleNodeType::ScopeDeclaration(MirScopeDecl {
                        body: lst,
                        create_new_scope: false,
                        is_temp: true,
                        scope_id: scope,
                    })
                },
                span: node.span,
            }),
            AstNodeType::EmptyLine => Ok(MiddleNode {
                node_type: MiddleNodeType::EmptyLine,
                span: node.span,
            }),
            AstNodeType::Return { value } => Ok(MiddleNode {
                node_type: MiddleNodeType::Return(MirReturn {
                    value: {
                        let mut lst = Vec::new();

                        if !self
                            .tagging
                            .tag_info
                            .contains(&TagInfo::IgnoreInvalidReturn)
                        {
                            if let Some(ret_ty) = self.scoping.return_type_stack.last().cloned() {
                                let node_ty = if let Some(value) = &value {
                                    if let Some(x) = self.resolve_type_from_node(scope, value) {
                                        x.key()
                                    } else {
                                        ParserInnerType::Dynamic
                                    }
                                } else {
                                    ParserInnerType::Null
                                };

                                // TODO Properly check for the generators inner type
                                if !node_ty.loose_eq(&ret_ty) && !ret_ty.is_gen() {
                                    return Err(self.context.err_at_current(
                                        MiddleErr::InvalidReturnType {
                                            expected: Box::new(ParserDataType::new(
                                                node.span, ret_ty,
                                            )),
                                            found: Box::new(ParserDataType::new(
                                                node.span, node_ty,
                                            )),
                                        },
                                    ));
                                }
                            } else {
                                return Err(self
                                    .context
                                    .err_at_current(MiddleErr::ReturnOutOfFunction));
                            }
                        }

                        let value = value.map(|x| self.evaluate(scope, *x));

                        let chain_defers = self.scoping.collect_defers_until(scope, None);
                        for x in chain_defers {
                            lst.push(self.evaluate(scope, x));
                        }

                        for x in self.symbols.func_defers.clone() {
                            lst.push(self.evaluate(scope, x));
                        }

                        if lst.is_empty() {
                            value.map(Box::new)
                        } else {
                            if let Some(x) = value {
                                lst.push(x);
                            }

                            Some(Box::new(MiddleNode::new(
                                MiddleNodeType::ScopeDeclaration(MirScopeDecl {
                                    body: lst,
                                    create_new_scope: false,
                                    is_temp: true,
                                    scope_id: scope,
                                }),
                                node.span,
                            )))
                        }
                    },
                }),
                span: node.span,
            }),
            AstNodeType::RefStatement { mutability, value } => Ok(MiddleNode {
                node_type: MiddleNodeType::RefStatement(MirRef {
                    mutability,
                    value: Box::new(self.evaluate_inner(scope, *value)?),
                }),
                span: node.span,
            }),
            AstNodeType::DerefStatement { value } => Ok(MiddleNode {
                node_type: MiddleNodeType::DerefStatement(MirDeref {
                    value: Box::new(self.evaluate_inner(scope, *value)?),
                }),
                span: node.span,
            }),
            AstNodeType::ParenExpression { value } => self.evaluate_inner(scope, *value),
            AstNodeType::DestructureDeclaration {
                var_type: _,
                pattern,
                value,
            } => {
                let tmp_ident: PotentialDollarIdentifier =
                    ParserText::temp_name_with_suffix("destructure_tmp", node.span).into();

                let tmp_decl = AstNode::new(
                    node.span,
                    AstNodeType::VariableDeclaration {
                        var_type: VarType::Immutable,
                        identifier: tmp_ident.clone(),
                        data_type: ParserDataType::auto(node.span),
                        value,
                    },
                );

                let mut body = Vec::new();
                body.push(tmp_decl);
                body.extend(
                    self.emit_destructure_statements(&tmp_ident, &pattern, node.span, true),
                );

                self.evaluate_inner(
                    scope,
                    AstNode::new(
                        node.span,
                        AstNodeType::ScopeDeclaration {
                            body: Some(body),
                            named: None,
                            is_temp: true,
                            create_new_scope: Some(false),
                            define: false,
                        },
                    ),
                )
            }
            AstNodeType::DestructureAssignment { pattern, value } => {
                let tmp_ident: PotentialDollarIdentifier =
                    ParserText::temp_name_with_suffix("destructure_tmp", node.span).into();

                let tmp_decl = AstNode::new(
                    node.span,
                    AstNodeType::VariableDeclaration {
                        var_type: VarType::Immutable,
                        identifier: tmp_ident.clone(),
                        data_type: ParserDataType::auto(node.span),
                        value,
                    },
                );

                let mut body = vec![tmp_decl];
                body.extend(
                    self.emit_destructure_statements(&tmp_ident, &pattern, node.span, false),
                );

                self.evaluate_inner(
                    scope,
                    AstNode::new_temp_scope_with_create(body, Some(false)),
                )
            }
            AstNodeType::VariableDeclaration {
                var_type,
                identifier,
                value,
                data_type,
            } => self.evaluate_var_declaration(
                scope, node.span, var_type, identifier, *value, data_type,
            ),
            AstNodeType::TypeDeclaration {
                identifier,
                object,
                overloads,
            } => self.evaluate_type_declaration(scope, node.span, identifier, object, overloads),
            AstNodeType::BooleanExpression {
                left,
                right,
                operator,
            } => {
                if let Some(x) = self.handle_operator_overloads(
                    scope,
                    node.span,
                    *left.clone(),
                    *right.clone(),
                    Operator::Boolean(operator),
                )? {
                    return Ok(x);
                }

                Ok(MiddleNode {
                    node_type: MiddleNodeType::BooleanExpression(MirBoolean {
                        left: Box::new(self.evaluate(scope, *left)),
                        right: Box::new(self.evaluate(scope, *right)),
                        operator,
                    }),
                    span: node.span,
                })
            }
            AstNodeType::ComparisonExpression {
                left,
                right,
                operator,
            } => {
                if let Some(x) = self.handle_operator_overloads(
                    scope,
                    node.span,
                    *left.clone(),
                    *right.clone(),
                    Operator::Comparison(operator),
                )? {
                    return Ok(x);
                }

                Ok(MiddleNode {
                    node_type: MiddleNodeType::ComparisonExpression(MirComparison {
                        left: Box::new(self.evaluate(scope, *left)),
                        right: Box::new(self.evaluate(scope, *right)),
                        operator,
                    }),
                    span: node.span,
                })
            }
            AstNodeType::BinaryExpression {
                left,
                right,
                operator,
            } => {
                if let Some(x) = self.handle_operator_overloads(
                    scope,
                    node.span,
                    *left.clone(),
                    *right.clone(),
                    Operator::Binary(operator),
                )? {
                    return Ok(x);
                }

                Ok(MiddleNode {
                    node_type: MiddleNodeType::BinaryExpression(MirBinary {
                        left: Box::new(self.evaluate(scope, *left)),
                        right: Box::new(self.evaluate(scope, *right)),
                        operator,
                    }),
                    span: node.span,
                })
            }
            AstNodeType::NotExpression { value } => self.evaluate_inner(
                scope,
                AstNode {
                    node_type: AstNodeType::ComparisonExpression {
                        left: value,
                        right: Box::new(AstNode::bool(self.context.current_span(), false)),
                        operator: ComparisonOperator::Equal,
                    },
                    span: node.span,
                },
            ),
            AstNodeType::NegExpression { value } => Ok(MiddleNode {
                node_type: MiddleNodeType::NegExpression(MirNeg {
                    value: Box::new(self.evaluate_inner(scope, *value)?),
                }),
                span: node.span,
            }),
            AstNodeType::AsExpression {
                value,
                data_type,
                failure_mode,
            } => {
                let target =
                    self.resolve_data_type(scope, &data_type, ResolutionOptions::typing())?;
                if self
                    .handle_as_overload_exists(scope, *value.clone(), target.clone())
                    .unwrap_or_default()
                {
                    match failure_mode {
                        AsFailureMode::Result | AsFailureMode::Option => {}
                        AsFailureMode::Panic => {
                            let temp_ident = ParserText::temp_name_with_suffix("as_res", node.span);
                            return self.evaluate_inner(
                                scope,
                                AstNode {
                                    node_type: AstNodeType::Try {
                                        value: Box::new(AstNode {
                                            node_type: AstNodeType::AsExpression {
                                                value,
                                                data_type,
                                                failure_mode: AsFailureMode::Result,
                                            },
                                            span: node.span,
                                        }),
                                        catch: Some(TryCatch {
                                            name: Some(PotentialDollarIdentifier::new(
                                                node.span,
                                                temp_ident.clone(),
                                            )),
                                            body: Box::new(AstNode::call(
                                                node.span,
                                                AstNode::identifier(node.span, "panic"),
                                                vec![CallArg::Value(AstNode::identifier(
                                                    node.span,
                                                    &temp_ident,
                                                ))],
                                            )),
                                        }),
                                    },
                                    span: node.span,
                                },
                            );
                        }
                    }
                }

                if let Some(x) =
                    self.handle_as_overload(scope, node.span, *value.clone(), target.clone())?
                {
                    return Ok(x);
                }

                Ok(MiddleNode {
                    node_type: MiddleNodeType::AsExpression(MirAs {
                        value: Box::new(self.evaluate_inner(scope, *value)?),
                        data_type: target,
                        failure_mode,
                    }),
                    span: node.span,
                })
            }
            AstNodeType::IsExpression { value, data_type } => Ok(MiddleNode {
                node_type: MiddleNodeType::IsExpression(MirIs {
                    value: Box::new(self.evaluate_inner(scope, *value)?),
                    data_type: self.resolve_data_type(
                        scope,
                        &data_type,
                        ResolutionOptions::typing(),
                    )?,
                }),
                span: node.span,
            }),
            AstNodeType::InDeclaration { identifier, value } => {
                if let Some(x) = self.handle_operator_overloads(
                    scope,
                    node.span,
                    *identifier.clone(),
                    *value.clone(),
                    Operator::In,
                )? {
                    return Ok(x);
                }

                if let AstNodeType::RangeDeclaration {
                    from,
                    to,
                    inclusive,
                } = value.node_type.clone()
                {
                    let lower = AstNode::new(
                        self.context.current_span(),
                        AstNodeType::ComparisonExpression {
                            left: Box::new(*identifier.clone()),
                            right: from,
                            operator: ComparisonOperator::GreaterEqual,
                        },
                    );

                    let upper = AstNode::new(
                        self.context.current_span(),
                        AstNodeType::ComparisonExpression {
                            left: Box::new(*identifier.clone()),
                            right: to,
                            operator: if inclusive {
                                ComparisonOperator::LesserEqual
                            } else {
                                ComparisonOperator::Lesser
                            },
                        },
                    );

                    return self.evaluate_inner(
                        scope,
                        AstNode::new(
                            self.context.current_span(),
                            AstNodeType::BooleanExpression {
                                left: Box::new(lower),
                                right: Box::new(upper),
                                operator: BooleanOperator::And,
                            },
                        ),
                    );
                }

                if let AstNodeType::ListLiteral(_, values) = value.node_type.clone() {
                    let mut comparisons = values.into_iter().map(|item| {
                        AstNode::new(
                            self.context.current_span(),
                            AstNodeType::ComparisonExpression {
                                left: Box::new(*identifier.clone()),
                                right: Box::new(item),
                                operator: ComparisonOperator::Equal,
                            },
                        )
                    });

                    if let Some(first) = comparisons.next() {
                        let cond = comparisons.fold(first, |acc, cmp| {
                            AstNode::new(
                                self.context.current_span(),
                                AstNodeType::BooleanExpression {
                                    left: Box::new(acc),
                                    right: Box::new(cmp),
                                    operator: BooleanOperator::Or,
                                },
                            )
                        });
                        return self.evaluate_inner(scope, cond);
                    }
                }

                if let Some(data_type) = self.resolve_type_from_node(scope, &value)
                    && matches!(
                        data_type.data_type.unwrap_all_refs(),
                        ParserInnerType::List(_) | ParserInnerType::Str
                    )
                {
                    let member = AstNode::new(
                        self.context.current_span(),
                        AstNodeType::FieldAccess {
                            base: Box::new(*value.clone()),
                            field: PotentialDollarIdentifier::new(
                                self.context.current_span(),
                                "contains",
                            ),
                        },
                    );

                    return self.evaluate_inner(
                        scope,
                        AstNode::call(
                            self.context.current_span(),
                            member,
                            vec![CallArg::Value(*identifier)],
                        ),
                    );
                }

                self.evaluate_inner(
                    scope,
                    AstNode::call(
                        self.context.current_span(),
                        AstNode::identifier(self.context.current_span(), "contains"),
                        vec![CallArg::Value(*value), CallArg::Value(*identifier)],
                    ),
                )
            }
            AstNodeType::DebugExpression { value } => Ok(MiddleNode {
                node_type: MiddleNodeType::DebugExpression(MirDebug {
                    pretty_printed_str: Ustr::from(&value.to_string()),
                    value: Box::new(self.evaluate_inner(scope, *value)?),
                }),
                span: node.span,
            }),
            AstNodeType::ListLiteral(data_type, x) => {
                let mut value = MirListBuilder::default();

                let mut data_type = if data_type.is_auto() {
                    None
                } else {
                    Some(self.resolve_data_type(scope, &data_type, ResolutionOptions::typing())?)
                };

                value.values(
                    x.into_iter()
                        .map(|item| {
                            let node_ty = self.resolve_type_from_node(scope, &item);
                            data_type = Some(self.compare_types(
                                data_type.clone(),
                                node_ty,
                                Some(&TagInfo::IgnoreInvalidTypeCheck),
                            )?);
                            self.evaluate_inner(scope, item)
                        })
                        .collect::<Result<Vec<_>, MiddleErr>>()?,
                );

                if let Some(x) = data_type {
                    value.data_type(x);
                } else {
                    return Err(self.context.err_at_current(MiddleErr::InferImpossible));
                }

                Ok(MiddleNode {
                    node_type: MiddleNodeType::ListLiteral(value.build().unwrap()),
                    span: node.span,
                })
            }
            // TODO Give a dedicated instruction to this for optimisation
            AstNodeType::ListRepeatLiteral {
                data_type,
                value,
                count,
            } => {
                let count = self.evaluate(scope, *count);
                let count = match count.node_type {
                    MiddleNodeType::IntLiteral(value) => value.value.value as usize,
                    _ => {
                        return Err(MiddleErr::At(
                            count.span,
                            Box::new(MiddleErr::Internal(
                                "list repeat count must be an int literal".to_string(),
                            )),
                        ));
                    }
                };

                let mut lst = MirListBuilder::default();

                let node_ty = self.resolve_type_from_node(scope, &value);

                let data_type = if data_type.is_auto() {
                    None
                } else {
                    Some(self.resolve_data_type(scope, &data_type, ResolutionOptions::typing())?)
                };

                lst.data_type(self.compare_types(
                    data_type,
                    node_ty,
                    Some(&TagInfo::IgnoreInvalidTypeCheck),
                )?);

                let item = self.evaluate(scope, *value);
                lst.values((0..count).map(|_| item.clone()).collect());

                Ok(MiddleNode {
                    node_type: MiddleNodeType::ListLiteral(lst.build().unwrap()),
                    span: node.span,
                })
            }
            AstNodeType::Try { value, catch } => {
                let resolved_type = self.resolve_type_from_node(scope, &value);
                let is_option_try = matches!(
                    resolved_type.as_ref().map(|t| t.key()),
                    Some(ParserInnerType::Option(_))
                );

                let enum_arm = |variant: &str, name: Option<PotentialDollarIdentifier>, body| {
                    (
                        MatchArmType::Enum {
                            var_type: VarType::Immutable,
                            value: ParserText::from(variant.to_string()).into(),
                            name,
                            destructure: None,
                            pattern: None,
                        },
                        Vec::new(),
                        Box::new(body),
                    )
                };

                let return_call = |name: &str, args: Vec<CallArg>| {
                    AstNode::new(
                        Span::default(),
                        AstNodeType::Return {
                            value: Some(Box::new(AstNode::call(
                                self.context.current_span(),
                                AstNode::identifier(self.context.current_span(), name),
                                args,
                            ))),
                        },
                    )
                };

                self.evaluate_inner(
                    scope,
                    AstNode {
                        node_type: AstNodeType::MatchStatement {
                            value: Some(value),
                            body: if is_option_try {
                                let ok_name = "anon_ok_value";
                                let ok_arm = enum_arm(
                                    "Some",
                                    Some(ParserText::from(ok_name.to_string()).into()),
                                    AstNode::identifier(self.context.current_span(), ok_name),
                                );
                                let err_arm = if let Some(catch) = catch {
                                    enum_arm("None", catch.name, *catch.body)
                                } else {
                                    enum_arm("None", None, return_call("none", Vec::new()))
                                };
                                vec![ok_arm, err_arm]
                            } else {
                                let ok_name = "anon_ok_value";
                                let ok_arm = enum_arm(
                                    "Ok",
                                    Some(ParserText::from(ok_name.to_string()).into()),
                                    AstNode::identifier(self.context.current_span(), ok_name),
                                );
                                let err_arm = if let Some(catch) = catch {
                                    enum_arm("Err", catch.name, *catch.body)
                                } else {
                                    let err_name = "anon_err_value";
                                    enum_arm(
                                        "Err",
                                        Some(ParserText::from(err_name.to_string()).into()),
                                        return_call(
                                            "err",
                                            vec![CallArg::Value(AstNode::identifier(
                                                self.context.current_span(),
                                                err_name,
                                            ))],
                                        ),
                                    )
                                };
                                vec![ok_arm, err_arm]
                            },
                        },
                        span: node.span,
                    },
                )
            }
            AstNodeType::LoopDeclaration {
                loop_type,
                body,
                until,
                label,
                else_body,
            } => self.evaluate_loop_statement(scope, *loop_type, *body, until, label, else_body),
            AstNodeType::TestDeclaration { identifier, body } => {
                let func_identifier = format!(
                    "test::{}",
                    ParserText::temp_name_with_suffix(identifier.text.trim(), node.span).text
                );
                let file_path = self
                    .scoping
                    .scope_or_err(scope)
                    .map(|s| s.path.clone())
                    .ok();

                self.register_test(
                    Ustr::from(&identifier.text),
                    Ustr::from(&func_identifier),
                    scope,
                    file_path,
                );

                self.evaluate_inner(
                    scope,
                    AstNode::new(
                        node.span,
                        AstNodeType::VariableDeclaration {
                            var_type: VarType::Constant,
                            identifier: PotentialDollarIdentifier::Identifier(ParserText::new(
                                node.span,
                                func_identifier,
                            )),
                            data_type: ParserDataType::auto(node.span),
                            value: Box::new(AstNode::new(
                                node.span,
                                AstNodeType::FunctionDeclaration {
                                    header: FunctionHeader {
                                        generics: GenericTypes::default(),
                                        parameters: Vec::new(),
                                        return_type: ParserDataType::null(node.span),
                                        param_destructures: Vec::new(),
                                    },
                                    body,
                                },
                            )),
                        },
                    ),
                )
            }
            AstNodeType::IterExpression {
                data_type,
                map,
                spawned,
                loop_type,
                conditionals,
                until,
            } => self.evaluate_iter_expression(
                scope,
                data_type,
                map,
                spawned,
                loop_type,
                conditionals,
                until,
            ),
            AstNodeType::InlineGenerator {
                map,
                data_type,
                loop_type,
                conditionals,
                until,
            } => self.evaluate_inner(
                scope,
                Self::wrap_inline_generator(
                    node.span,
                    *map,
                    *loop_type,
                    conditionals,
                    until,
                    data_type.unwrap_or(ParserDataType::auto(node.span)),
                ),
            ),
            AstNodeType::AssignmentExpression { identifier, value } => {
                if !self.context.type_check {
                    let identifier_type = self.resolve_type_from_node(scope, &identifier);
                    let value_type = self.resolve_type_from_node(scope, &value);
                    self.compare_types_ref(
                        identifier_type.as_ref(),
                        value_type.as_ref(),
                        Some(&TagInfo::IgnoreInvalidTypeCheck),
                    )?;
                }

                match identifier.node_type.clone() {
                    AstNodeType::Ternary {
                        comparison,
                        then,
                        otherwise,
                    } => self.evaluate_inner(
                        scope,
                        AstNode {
                            node_type: AstNodeType::IfStatement {
                                comparison: Box::new(IfComparisonType::If(*comparison)),
                                then: Box::new(AstNode::new(
                                    self.context.current_span(),
                                    AstNodeType::AssignmentExpression {
                                        identifier: then,
                                        value: value.clone(),
                                    },
                                )),
                                otherwise: Some(Box::new(AstNode::new(
                                    self.context.current_span(),
                                    AstNodeType::AssignmentExpression {
                                        identifier: otherwise,
                                        value,
                                    },
                                ))),
                            },
                            span: node.span,
                        },
                    ),
                    AstNodeType::DerefStatement {
                        value: deref_target,
                    } => Ok(MiddleNode {
                        node_type: MiddleNodeType::AssignmentExpression(MirAssignment {
                            identifier: Box::new(self.evaluate(
                                scope,
                                AstNode::new(
                                    node.span,
                                    AstNodeType::DerefStatement {
                                        value: deref_target,
                                    },
                                ),
                            )),
                            value: Box::new(self.evaluate(scope, *value)),
                        }),
                        span: node.span,
                    }),
                    AstNodeType::FieldAccess { base, field } => Ok(MiddleNode {
                        node_type: MiddleNodeType::AssignmentExpression(MirAssignment {
                            identifier: Box::new(self.evaluate(
                                scope,
                                AstNode::new(node.span, AstNodeType::FieldAccess { base, field }),
                            )),
                            value: Box::new(self.evaluate(scope, *value)),
                        }),
                        span: node.span,
                    }),
                    AstNodeType::ScopeAccess { base, field } => Ok(MiddleNode {
                        node_type: MiddleNodeType::AssignmentExpression(MirAssignment {
                            identifier: Box::new(self.evaluate(
                                scope,
                                AstNode::new(node.span, AstNodeType::ScopeAccess { base, field }),
                            )),
                            value: Box::new(self.evaluate(scope, *value)),
                        }),
                        span: node.span,
                    }),
                    AstNodeType::IndexAccess { base, index } => {
                        if let Some(overloaded) = self.handle_index_assign_overload(
                            scope,
                            node.span,
                            *base.clone(),
                            *index.clone(),
                            *value.clone(),
                        )? {
                            return Ok(overloaded);
                        }

                        Ok(MiddleNode {
                            node_type: MiddleNodeType::AssignmentExpression(MirAssignment {
                                identifier: Box::new(self.evaluate(
                                    scope,
                                    AstNode::new(
                                        node.span,
                                        AstNodeType::IndexAccess { base, index },
                                    ),
                                )),
                                value: Box::new(self.evaluate(scope, *value)),
                            }),
                            span: node.span,
                        })
                    }
                    _ => Ok(MiddleNode {
                        node_type: MiddleNodeType::AssignmentExpression(MirAssignment {
                            identifier: Box::new(self.evaluate(scope, *identifier)),
                            value: Box::new(self.evaluate(scope, *value)),
                        }),
                        span: node.span,
                    }),
                }
            }
            AstNodeType::ImplDeclaration {
                generics,
                target,
                variables,
            } => {
                let mut prev_generics = Vec::new();
                if let Ok(scope_ref) = self.scoping.scope_mut_or_err(scope) {
                    for generic in generics.0.iter() {
                        let name = Ustr::from(&generic.identifier.to_string());
                        prev_generics.push((name, scope_ref.mappings.get(&name).cloned()));
                        scope_ref.mappings.insert(name, name);
                    }
                }

                let generic_params: Vec<Ustr> = generics
                    .0
                    .iter()
                    .map(|g| {
                        self.resolve(
                            scope,
                            &g.identifier,
                            ResolutionOptions::default().with_dollar(),
                        )
                        .unwrap_or(Ustr::from(&g.identifier.to_string()))
                    })
                    .collect();

                if !generic_params.is_empty() {
                    self.scoping.push_generic_params(generic_params.clone());
                }

                let resolved = self
                    .resolve_data_type(scope, &target, ResolutionOptions::typing())
                    .unwrap()
                    .unwrap_all_refs();

                let impl_key = Ustr::from(&resolved.impl_name());

                self.typing
                    .get_or_create_impl(impl_key, self.context.current_location.clone());

                {
                    let placeholders = variables
                        .iter()
                        .filter_map(|var| {
                            if let AstNodeType::VariableDeclaration { identifier, .. } =
                                &var.node_type
                            {
                                let identifier = self
                                    .resolve(
                                        scope,
                                        identifier,
                                        ResolutionOptions::default().with_dollar(),
                                    )
                                    .ok()?;
                                let resolved_iden =
                                    Ustr::from(&format!("{}.{}", impl_key, identifier));
                                // TODO Unpack the dollar ident only without resolving
                                Some((identifier, resolved_iden, generic_params.clone()))
                            } else {
                                None
                            }
                        })
                        .collect::<Vec<_>>();

                    let type_defs = variables
                        .iter()
                        .filter_map(|var| {
                            if let AstNodeType::TypeDeclaration {
                                identifier, object, ..
                            } = &var.node_type
                            {
                                let ident = self
                                    .resolve(
                                        scope,
                                        identifier.get_ident(),
                                        ResolutionOptions::default().with_dollar(),
                                    )
                                    .ok()?;
                                if let TypeDefType::NewType(inner) = object {
                                    let resolved_ty = self
                                        .resolve_data_type(
                                            scope,
                                            inner.as_ref(),
                                            ResolutionOptions::typing(),
                                        )
                                        .ok()?
                                        .unwrap_all_refs();
                                    Some((ident, resolved_ty))
                                } else {
                                    None
                                }
                            } else {
                                None
                            }
                        })
                        .collect::<Vec<_>>();

                    let impl_ref = self.typing.impls.get_mut(&impl_key).ok_or_else(|| {
                        MiddleErr::At(
                            node.span,
                            Box::new(MiddleErr::Internal(format!("missing impl {impl_key:?}"))),
                        )
                    })?;

                    for var in placeholders {
                        impl_ref.insert_member_placeholder(&var.0, var.1, var.2);
                    }

                    for (ident, ty) in type_defs {
                        impl_ref.assoc_types.insert(ident, ty);
                    }
                }

                let previous_self_type = {
                    let scope = self.scoping.scope_mut_or_err(scope)?;

                    scope
                        .type_mappings
                        .insert(Ustr::from("Self"), resolved.data_type.clone())
                };

                let mut statements = Vec::new();

                fn process_var(
                    env: &mut MiddleEnvironment,
                    scope: ScopeId,
                    resolved: &ParserDataType,
                    generic_params: &[Ustr],
                    var: AstNode,
                ) -> Result<Option<(AstNode, Ustr, bool)>, MiddleErr> {
                    match var.node_type {
                        AstNodeType::VariableDeclaration {
                            var_type,
                            identifier,
                            value,
                            data_type,
                        } => {
                            let identifier = env.resolve(
                                scope,
                                &identifier,
                                ResolutionOptions::default().with_dollar(),
                            )?;
                            let resolved_iden = format!("{}.{}", resolved.impl_name(), identifier);

                            let dependant = match &value.node_type {
                                AstNodeType::FunctionDeclaration { header, .. } => {
                                    let param_type = if let Some(Some(param)) =
                                        header.parameters.first().map(|x| &x.1)
                                    {
                                        env.resolve_data_type(
                                            scope,
                                            param,
                                            ResolutionOptions::typing(),
                                        )
                                        .ok()
                                        .map(|x| x.unwrap_all_refs())
                                    } else if let Some(Some(node)) =
                                        header.parameters.first().map(|x| x.2.clone())
                                    {
                                        env.resolve_type_from_node(scope, &node)
                                            .map(|x| x.unwrap_all_refs())
                                    } else {
                                        None
                                    };

                                    if let Some(param_type) = param_type {
                                        resolved.data_type.matches(
                                            &param_type.data_type,
                                            &generic_params
                                                .iter()
                                                .map(|x| x.as_ref())
                                                .collect::<Vec<_>>(),
                                        )
                                    } else {
                                        false
                                    }
                                }
                                _ => false,
                            };

                            Ok(Some((
                                AstNode {
                                    span: var.span,
                                    node_type: AstNodeType::VariableDeclaration {
                                        var_type,
                                        identifier: PotentialDollarIdentifier::Identifier(
                                            ParserText::from(resolved_iden),
                                        ),
                                        value,
                                        data_type,
                                    },
                                },
                                identifier,
                                dependant,
                            )))
                        }
                        AstNodeType::Tag {
                            node,
                            tag,
                            arguments,
                        } => match process_var(env, scope, resolved, generic_params, *node) {
                            Ok(Some(x)) => Ok(Some((
                                AstNode::new(
                                    Span::default(),
                                    AstNodeType::Tag {
                                        node: Box::new(x.0),
                                        tag,
                                        arguments,
                                    },
                                ),
                                x.1,
                                x.2,
                            ))),
                            x => x,
                        },
                        AstNodeType::TypeDeclaration { .. } => Ok(None),
                        _ => Err(MiddleErr::At(
                            var.span,
                            Box::new(MiddleErr::Internal(
                                "expected variable declaration in impl".to_string(),
                            )),
                        )),
                    }
                }

                for var in variables {
                    let (dec, iden, dependant) =
                        match process_var(self, scope, &resolved, &generic_params, var)? {
                            Some(x) => x,
                            None => continue,
                        };

                    let dec = self.evaluate(scope, dec);

                    let new_name = match &dec.node_type {
                        MiddleNodeType::VariableDeclaration(MirVarDecl { identifier, .. }) => {
                            identifier
                        }
                        _ => {
                            return Err(MiddleErr::At(
                                dec.span,
                                Box::new(MiddleErr::Internal(
                                    "impl body did not lower to variable declaration".to_string(),
                                )),
                            ));
                        }
                    };

                    self.typing
                        .impls
                        .get_mut(&impl_key)
                        .ok_or_else(|| {
                            MiddleErr::At(
                                dec.span,
                                Box::new(MiddleErr::Internal(format!("missing impl {impl_key:?}"))),
                            )
                        })?
                        .insert_member(
                            &iden,
                            MiddleImplMember::new(*new_name, generic_params.clone(), dependant),
                        );

                    statements.push(dec);
                }

                {
                    let scope = self.scoping.scope_mut_or_err(scope)?;

                    if let Some(prev) = previous_self_type {
                        scope.type_mappings.insert(Ustr::from("Self"), prev);
                    }

                    for (name, prev) in prev_generics {
                        if let Some(prev) = prev {
                            scope.mappings.insert(name, prev);
                        } else {
                            scope.mappings.remove(&name);
                        }
                    }

                    if !generic_params.is_empty() {
                        self.scoping.pop_generic_params();
                    }
                }

                Ok(MiddleNode {
                    node_type: MiddleNodeType::ScopeDeclaration(MirScopeDecl {
                        body: statements,
                        create_new_scope: false,
                        is_temp: false,
                        scope_id: scope,
                    }),
                    span: node.span,
                })
            }
            AstNodeType::ImplTraitDeclaration {
                generics,
                trait_ident,
                target,
                variables,
            } => {
                let mut prev_generics = Vec::new();
                if let Ok(scope_ref) = self.scoping.scope_mut_or_err(scope) {
                    for generic in generics.0.iter() {
                        let name = Ustr::from(&generic.identifier.to_string());
                        prev_generics.push((name, scope_ref.mappings.get(&name).cloned()));
                        scope_ref.mappings.insert(name, name);
                    }
                }

                let generic_params: Vec<Ustr> = generics
                    .0
                    .iter()
                    .map(|g| {
                        self.resolve(
                            scope,
                            &g.identifier,
                            ResolutionOptions::default().with_dollar(),
                        )
                        .unwrap_or(Ustr::from(&g.identifier.to_string()))
                    })
                    .collect();

                if !generic_params.is_empty() {
                    self.scoping.push_generic_params(generic_params.clone());
                }

                let resolved_trait =
                    self.resolve(scope, &trait_ident, ResolutionOptions::typing())?;

                let resolved_target = self
                    .resolve_data_type(scope, &target, ResolutionOptions::typing())?
                    .unwrap_all_refs();
                let impl_key = Ustr::from(&resolved_target.impl_name());

                let mut provided = UstrSet::default();
                let mut assoc_types = Vec::new();
                for var in &variables {
                    match &var.node_type {
                        AstNodeType::VariableDeclaration { identifier, .. } => {
                            provided.insert(Ustr::from(&identifier.to_string()));
                        }
                        AstNodeType::TypeDeclaration {
                            identifier, object, ..
                        } => {
                            assoc_types.push((identifier.clone(), object.clone()));
                        }
                        _ => {}
                    }
                }

                let mut all_vars = variables;
                for (name, member) in Typing::collect_trait_default_members(
                    &self.typing.trait_defs,
                    &resolved_trait,
                    &provided,
                ) {
                    if member.default.is_none() {
                        continue;
                    }
                    let default = member.default.unwrap();
                    all_vars.push(AstNode::new(
                        default.span,
                        AstNodeType::VariableDeclaration {
                            var_type: VarType::Constant,
                            identifier: PotentialDollarIdentifier::Identifier(ParserText::from(
                                name,
                            )),
                            data_type: member.data_type.clone(),
                            value: Box::new(default),
                        },
                    ));
                }

                let (previous_self, previous_self_type) = {
                    let scope = self.scoping.scope_mut_or_err(scope)?;

                    (
                        scope.mappings.insert(Ustr::from("Self"), impl_key),
                        scope
                            .type_mappings
                            .insert(Ustr::from("Self"), resolved_target.data_type.clone()),
                    )
                };

                self.typing
                    .get_or_create_impl(impl_key, self.context.current_location.clone());

                for (identifier, object) in assoc_types {
                    if let TypeDefType::NewType(inner) = object {
                        let resolved_ty = self
                            .resolve_data_type(scope, inner.as_ref(), ResolutionOptions::typing())?
                            .unwrap_all_refs();

                        let ident = self.resolve(
                            scope,
                            identifier.get_ident(),
                            ResolutionOptions::default().with_dollar(),
                        )?;

                        let impl_ref = self.typing.impls.get_mut(&impl_key).ok_or_else(|| {
                            MiddleErr::At(
                                node.span,
                                Box::new(MiddleErr::Internal(format!("missing impl {impl_key:?}"))),
                            )
                        })?;

                        impl_ref.assoc_types.insert(ident, resolved_ty);
                    }
                }

                {
                    let impl_ref = self.typing.impls.get_mut(&impl_key).ok_or_else(|| {
                        MiddleErr::At(
                            node.span,
                            Box::new(MiddleErr::Internal(format!("missing impl {impl_key:?}"))),
                        )
                    })?;
                    for var in &all_vars {
                        if let AstNodeType::VariableDeclaration { identifier, .. } = &var.node_type
                        {
                            let resolved_iden = Ustr::from(&format!("{}.{}", impl_key, identifier));
                            impl_ref.insert_member_placeholder(
                                &identifier.to_string(),
                                resolved_iden,
                                generic_params.clone(),
                            );
                        }
                    }
                }

                let mut statements = Vec::new();

                for var in all_vars {
                    let (dec, iden, dependant) = match var.node_type {
                        AstNodeType::VariableDeclaration {
                            var_type,
                            identifier,
                            value,
                            data_type,
                        } => {
                            // TODO Deal with dollar ident
                            let iden = identifier.to_string();
                            let resolved_iden = format!("{}.{}", impl_key, identifier);

                            let dependant = match &value.node_type {
                                AstNodeType::FunctionDeclaration { header, .. } => {
                                    let param_type = if let Some(Some(param)) =
                                        header.parameters.first().map(|x| &x.1)
                                    {
                                        Some(
                                            self.resolve_data_type(
                                                scope,
                                                param,
                                                ResolutionOptions::typing(),
                                            )?
                                            .unwrap_all_refs(),
                                        )
                                    } else if let Some(Some(node)) =
                                        header.parameters.first().map(|x| x.2.clone())
                                    {
                                        self.resolve_type_from_node(scope, &node)
                                            .map(|x| x.unwrap_all_refs())
                                    } else {
                                        None
                                    };

                                    if let Some(param_type) = param_type {
                                        resolved_target.data_type.matches(
                                            &param_type.data_type,
                                            &generic_params
                                                .iter()
                                                .map(|x| x.as_ref())
                                                .collect::<Vec<_>>(),
                                        )
                                    } else {
                                        false
                                    }
                                }
                                _ => false,
                            };

                            (
                                AstNode {
                                    span: var.span,
                                    node_type: AstNodeType::VariableDeclaration {
                                        var_type,
                                        identifier: PotentialDollarIdentifier::Identifier(
                                            ParserText::from(resolved_iden),
                                        ),
                                        value,
                                        data_type,
                                    },
                                },
                                iden,
                                dependant,
                            )
                        }
                        AstNodeType::TypeDeclaration { .. } => {
                            continue;
                        }
                        _ => {
                            return Err(MiddleErr::At(
                                var.span,
                                Box::new(MiddleErr::Internal(
                                    "expected variable declaration in impl trait".to_string(),
                                )),
                            ));
                        }
                    };

                    let dec = self.evaluate(scope, dec);

                    let new_name = match &dec.node_type {
                        MiddleNodeType::VariableDeclaration(MirVarDecl { identifier, .. }) => {
                            identifier
                        }
                        _ => {
                            return Err(MiddleErr::At(
                                var.span,
                                Box::new(MiddleErr::Internal(
                                    "impl trait body did not lower to variable declaration"
                                        .to_string(),
                                )),
                            ));
                        }
                    };

                    let impl_ref = self.typing.impls.get_mut(&impl_key).ok_or_else(|| {
                        MiddleErr::At(
                            var.span,
                            Box::new(MiddleErr::Internal(format!("missing impl {impl_key:?}"))),
                        )
                    })?;

                    impl_ref.insert_member(
                        &iden,
                        MiddleImplMember::new(*new_name, generic_params.clone(), dependant),
                    );
                    if !impl_ref.traits.contains(&resolved_trait) {
                        impl_ref.traits.push(resolved_trait);
                    }

                    if let Some(trait_def) = self.typing.trait_defs.get(&resolved_trait) {
                        for implied in &trait_def.implied_traits {
                            if !impl_ref.traits.contains(implied) {
                                impl_ref.traits.push(*implied);
                            }
                        }
                    }

                    statements.push(dec);
                }

                {
                    let scope = self.scoping.scope_mut_or_err(scope)?;

                    if let Some(prev) = previous_self {
                        scope.mappings.insert(Ustr::from("Self"), prev);
                    }

                    if let Some(prev) = previous_self_type {
                        scope.type_mappings.insert(Ustr::from("Self"), prev);
                    }

                    for (name, prev) in prev_generics {
                        if let Some(prev) = prev {
                            scope.mappings.insert(name, prev);
                        } else {
                            scope.mappings.remove(&name);
                        }
                    }

                    if !generic_params.is_empty() {
                        self.scoping.pop_generic_params();
                    }
                }

                Ok(MiddleNode {
                    node_type: MiddleNodeType::ScopeDeclaration(MirScopeDecl {
                        body: statements,
                        create_new_scope: false,
                        is_temp: false,
                        scope_id: scope,
                    }),
                    span: node.span,
                })
            }
            AstNodeType::TraitDeclaration {
                identifier,
                implied_traits,
                members,
            } => {
                let mut generic_names = Vec::new();
                let base_name = match &identifier {
                    PotentialGenericTypeIdentifier::Identifier(x) => Ustr::from(&x.to_string()),
                    PotentialGenericTypeIdentifier::Generic {
                        identifier,
                        generic_types,
                    } => {
                        for t in generic_types {
                            if let ParserDataType {
                                data_type: ParserInnerType::Struct(s),
                                ..
                            } = t
                            {
                                generic_names.push(Ustr::from(s));
                            }
                        }
                        Ustr::from(&identifier.to_string())
                    }
                };

                let new_name =
                    Ustr::from(&ParserText::temp_name_with_suffix(base_name, node.span).text);

                self.typing.objects.insert(
                    new_name,
                    MiddleObject {
                        object_type: MiddleTypeDefType::Trait,
                        variables: UstrMap::default(),
                        traits: Vec::new(),
                        location: self.context.current_location.clone(),
                    },
                );

                let mut prev_generics = Vec::new();
                if let Ok(scope_ref) = self.scoping.scope_mut_or_err(scope) {
                    scope_ref.mappings.insert(base_name, new_name);

                    for name in &generic_names {
                        prev_generics.push((name, scope_ref.mappings.get(name).cloned()));
                        scope_ref.mappings.insert(*name, *name);
                    }
                }

                let mut trait_members = UstrMap::default();
                let mut assoc_types = UstrMap::default();
                for member in members {
                    match member.kind {
                        TraitMemberKind::Type => {
                            let data_type = self.resolve_data_type(
                                scope,
                                &member.data_type,
                                ResolutionOptions::typing(),
                            )?;
                            assoc_types
                                .insert(Ustr::from(&member.identifier.to_string()), data_type);
                        }
                        TraitMemberKind::Const => {
                            let data_type = self.resolve_data_type(
                                scope,
                                &member.data_type,
                                ResolutionOptions::typing(),
                            )?;
                            trait_members.insert(
                                Ustr::from(&member.identifier.to_string()),
                                MiddleTraitMember {
                                    data_type,
                                    default: member.value.map(|x| *x),
                                },
                            );
                        }
                    }
                }

                let mut implied = Vec::new();
                for imp in implied_traits {
                    let resolved = self
                        .resolve(scope, &imp, ResolutionOptions::default().with_dollar())
                        .unwrap_or_else(|_| Ustr::from(&imp.to_string()));
                    implied.push(resolved);
                }

                self.typing.trait_defs.insert(
                    new_name,
                    MiddleTrait {
                        implied_traits: implied,
                        members: trait_members,
                        assoc_types,
                    },
                );

                if let Ok(scope_ref) = self.scoping.scope_mut_or_err(scope) {
                    for (name, prev) in prev_generics {
                        if let Some(prev) = prev {
                            scope_ref.mappings.insert(*name, prev);
                        } else {
                            scope_ref.mappings.remove(name);
                        }
                    }
                }

                Ok(MiddleNode {
                    node_type: MiddleNodeType::EmptyLine,
                    span: node.span,
                })
            }
            AstNodeType::ScopeAlias {
                identifier,
                value,
                create_new_scope,
            } => self.evaluate_scope_alias(scope, node.span, identifier, value, create_new_scope),
            AstNodeType::ScopeDeclaration {
                body,
                named,
                is_temp,
                create_new_scope,
                define,
            } => self.evaluate_scope_declaration(
                scope,
                body,
                named,
                create_new_scope,
                define,
                is_temp,
            ),
            // TODO Handle generics
            AstNodeType::StructLiteral { identifier, value } => {
                let identifier = self.resolve(scope, &identifier, ResolutionOptions::typing())?;
                let obj = self.typing.objects.get(&identifier).cloned();

                if obj.is_none()
                    && !self
                        .tagging
                        .tag_info
                        .contains(&TagInfo::IgnoreInvalidTypeCheck)
                {
                    return Err(MiddleErr::At(
                        node.span,
                        Box::new(MiddleErr::Object(identifier.to_string())),
                    ));
                };

                let value = match value {
                    ObjectType::Map(x) => {
                        let mut map = Vec::new();

                        for itm in x {
                            if !self.context.type_check {
                                let node_ty = self.resolve_type_from_node(scope, &itm.1);
                                if let Some(obj) = &obj
                                    && let MiddleTypeDefType::Struct(fields) = &obj.object_type
                                    && let Some((_, (expected_ty, _))) =
                                        fields.0.iter().find(|(name, _)| name == &itm.0)
                                {
                                    self.compare_types_ref(
                                        Some(expected_ty),
                                        node_ty.as_ref(),
                                        Some(&TagInfo::IgnoreInvalidTypeCheck),
                                    )?;
                                }
                            }
                            map.push((itm.0, self.evaluate(scope, itm.1)));
                        }

                        map
                    }
                    ObjectType::Tuple(x) => {
                        let mut map = Vec::new();

                        for (idx, itm) in x.into_iter().enumerate() {
                            if !self.context.type_check {
                                let node_ty = self.resolve_type_from_node(scope, &itm);
                                if let Some(obj) = &obj
                                    && let MiddleTypeDefType::Struct(fields) = &obj.object_type
                                {
                                    let field_name = idx.to_string();
                                    if let Some((_, (expected_ty, _))) =
                                        fields.0.iter().find(|(name, _)| name == &field_name)
                                    {
                                        self.compare_types_ref(
                                            Some(expected_ty),
                                            node_ty.as_ref(),
                                            Some(&TagInfo::IgnoreInvalidTypeCheck),
                                        )?;
                                    }
                                }
                            }
                            map.push((idx.to_string(), self.evaluate(scope, itm)));
                        }

                        map
                    }
                };

                Ok(MiddleNode {
                    node_type: MiddleNodeType::AggregateExpression(MirAggregate {
                        identifier: Some(identifier),
                        value: ObjectMap(value),
                    }),
                    span: node.span,
                })
            }
            AstNodeType::EnumExpression {
                identifier,
                value,
                data,
            } => {
                let identifier = self.resolve(scope, &identifier, ResolutionOptions::typing())?;

                let raw_variant = value.to_string();
                let obj = self.typing.objects.get(&identifier);

                let (value, data_type) = if let Some(obj) = obj
                    && let MiddleTypeDefType::Enum { variants, .. } = &obj.object_type
                {
                    variants
                        .iter()
                        .find(|(name, _)| name.eq_ignore_ascii_case(&raw_variant))
                        .map(|(name, x)| (name, x.clone()))
                        .ok_or(MiddleErr::At(
                            node.span,
                            Box::new(MiddleErr::EnumVariant(raw_variant.clone())),
                        ))?
                } else {
                    return Err(MiddleErr::At(
                        node.span,
                        Box::new(MiddleErr::Object(identifier.to_string())),
                    ));
                };

                Ok(MiddleNode {
                    node_type: MiddleNodeType::EnumExpression(MirEnum {
                        identifier,
                        value: *value,
                        data: if let Some(data) = data {
                            if !self.context.type_check {
                                let node_ty = self.resolve_type_from_node(scope, &data);
                                self.compare_types_ref(
                                    node_ty.as_ref(),
                                    data_type.as_ref(),
                                    Some(&TagInfo::IgnoreInvalidTypeCheck),
                                )?;
                            }
                            Some(Box::new(self.evaluate_inner(scope, *data)?))
                        } else {
                            None
                        },
                    }),
                    span: node.span,
                })
            }
            AstNodeType::MatchStatement { value, body } => {
                self.evaluate_match_statement(scope, node.span, value, body)
            }
            AstNodeType::FnMatchDeclaration { header, body } => self.evaluate_inner(
                scope,
                AstNode::new(
                    self.context.current_span(),
                    AstNodeType::FunctionDeclaration {
                        body: Box::new(AstNode::new(
                            self.context.current_span(),
                            AstNodeType::ScopeDeclaration {
                                body: Some(vec![AstNode::new(
                                    self.context.current_span(),
                                    AstNodeType::MatchStatement {
                                        value: Some(Box::new(AstNode::identifier(
                                            self.context.current_span(),
                                            header.parameters[0].0.clone(),
                                        ))),
                                        body,
                                    },
                                )]),
                                named: None,
                                is_temp: true,
                                create_new_scope: Some(false),
                                define: false,
                            },
                        )),
                        header: FunctionHeader {
                            param_destructures: Vec::new(),
                            ..header
                        },
                    },
                ),
            ),
            AstNodeType::FunctionDeclaration { header, body } => {
                self.evaluate_function_declaration(scope, node.span, header, *body)
            }
            AstNodeType::Tag {
                node,
                tag,
                arguments,
            } => {
                if let Some(handler) = self
                    .tagging
                    .tag_handlers
                    .get(&Ustr::from(&tag.text))
                    .cloned()
                {
                    let handler_fn = handler.handler.lock().unwrap();
                    handler_fn(self, scope, *node, tag, arguments)
                } else {
                    self.context.push_error(MiddleErr::InvalidTag(tag.text));
                    self.evaluate_inner(scope, *node)
                }
            }
            AstNodeType::ExternFunctionDeclaration {
                abi,
                identifier,
                parameters,
                return_type,
                library,
                symbol,
            } => self.evaluate_extern_function(
                scope,
                abi,
                identifier,
                parameters,
                return_type,
                library,
                symbol,
            ),
            AstNodeType::CallExpression {
                string_fn: _,
                caller,
                generic_types,
                args,
                reverse_args,
            } => self.evaluate_call_expression(
                scope,
                node.span,
                *caller,
                generic_types,
                args,
                reverse_args,
            ),
            AstNodeType::ImportStatement {
                module,
                alias,
                values,
            } => {
                let values: Vec<Ustr> = values
                    .into_iter()
                    .map(|val| Ustr::from(&val.to_string()))
                    .collect();
                let module_path: Vec<Ustr> =
                    module.iter().map(|x| Ustr::from(&x.to_string())).collect();

                let alias = if let Some(alias) = alias {
                    self.resolve(scope, &alias, ResolutionOptions::default().with_dollar())
                        .ok()
                } else {
                    None
                };

                let (new_scope, build_node) = if let Some(alias) = alias {
                    if ["super", "root"].contains(&alias.as_str()) {
                        // TODO return err
                        return Ok(MiddleNode {
                            node_type: MiddleNodeType::EmptyLine,
                            span: node.span,
                        });
                    }

                    let (new_scope_id, build_node) = self.import_scope_list(scope, &module_path)?;

                    self.scoping
                        .scope_mut_or_err(scope)?
                        .children
                        .insert(alias, new_scope_id);

                    return Ok(build_node.unwrap_or(MiddleNode {
                        node_type: MiddleNodeType::EmptyLine,
                        span: node.span,
                    }));
                } else if !values.is_empty() {
                    let (new_scope_id, build_node) = self.import_scope_list(scope, &module_path)?;
                    (new_scope_id, build_node)
                } else {
                    let (_, n) = self.import_scope_list(scope, &module_path)?;
                    return Ok(if let Some(x) = n {
                        x
                    } else {
                        MiddleNode {
                            node_type: MiddleNodeType::EmptyLine,
                            span: node.span,
                        }
                    });
                };

                let (ident_map, type_map) = {
                    let scope = self.scoping.scope_or_err(new_scope)?;

                    (scope.mappings.clone(), scope.type_mappings.clone())
                };

                if &values[0] == "*" {
                    let scope = self.scoping.scope_mut_or_err(scope)?;

                    for (key, value) in ident_map {
                        scope.mappings.entry(key).or_insert(value);
                    }

                    for (key, value) in type_map {
                        scope.type_mappings.entry(key).or_insert(value);
                    }
                } else {
                    let scope = self.scoping.scope_mut_or_err(scope)?;

                    for key in values {
                        if let Some(value) = ident_map.get(&key).cloned() {
                            scope.mappings.insert(key, value);
                            continue;
                        }

                        if let Some(value) = type_map.get(&key).cloned() {
                            scope.type_mappings.insert(key, value);
                        } else {
                            return Err(MiddleErr::At(
                                node.span,
                                Box::new(MiddleErr::CantImport(format!("{} at {:?}", key, module))),
                            ));
                        }
                    }
                }

                Ok(build_node.unwrap_or(MiddleNode {
                    node_type: MiddleNodeType::EmptyLine,
                    span: node.span,
                }))
            }
            AstNodeType::SelectStatement { arms } => {
                let done_ident: PotentialDollarIdentifier =
                    ParserText::temp_name_with_suffix("select_done", node.span).into();

                let done_decl = AstNode::new(
                    node.span,
                    AstNodeType::VariableDeclaration {
                        var_type: VarType::Mutable,
                        identifier: done_ident.clone(),
                        data_type: ParserDataType::new(node.span, ParserInnerType::Bool),
                        value: Box::new(AstNode::bool(node.span, false)),
                    },
                );

                let mut loop_body = Vec::new();
                let mut has_default = false;

                let done_ident_node = || AstNode::identifier(node.span, done_ident.clone());

                let break_node = || {
                    AstNode::new(
                        node.span,
                        AstNodeType::Break {
                            label: None,
                            value: None,
                        },
                    )
                };

                let set_done_node = || {
                    AstNode::new(
                        node.span,
                        AstNodeType::AssignmentExpression {
                            identifier: Box::new(done_ident_node()),
                            value: Box::new(AstNode::bool(node.span, true)),
                        },
                    )
                };

                let fold_guards = |initial: AstNode, guards: &[AstNode]| -> AstNode {
                    let mut cond = initial;
                    for guard in guards {
                        cond = AstNode::new(
                            node.span,
                            AstNodeType::BooleanExpression {
                                left: Box::new(cond),
                                right: Box::new(guard.clone()),
                                operator: BooleanOperator::And,
                            },
                        );
                    }
                    cond
                };

                for arm in arms {
                    for (kind, left, right) in arm.patterns.iter() {
                        match kind {
                            SelectArmKind::Recv => {
                                let Some(left) = left.clone() else { continue };
                                let Some(right) = right.clone() else { continue };
                                let tmp_ident = PotentialDollarIdentifier::Identifier(
                                    ParserText::temp_name_with_suffix("select", node.span),
                                );

                                let try_get_call = AstNode::call(
                                    node.span,
                                    AstNode::member(node.span, right, "try_get"),
                                    vec![],
                                );

                                loop_body.push(AstNode::new(
                                    node.span,
                                    AstNodeType::VariableDeclaration {
                                        var_type: VarType::Immutable,
                                        identifier: tmp_ident.clone(),
                                        data_type: ParserDataType::auto(node.span),
                                        value: Box::new(try_get_call),
                                    },
                                ));

                                let cond = AstNode::new(
                                    node.span,
                                    AstNodeType::ComparisonExpression {
                                        left: Box::new(AstNode::new(
                                            node.span,
                                            AstNodeType::Identifier(tmp_ident.clone().into()),
                                        )),
                                        right: Box::new(AstNode::none(node.span)),
                                        operator: ComparisonOperator::NotEqual,
                                    },
                                );

                                let extracted = AstNode::new(
                                    node.span,
                                    AstNodeType::FieldAccess {
                                        base: Box::new(AstNode::new(
                                            node.span,
                                            AstNodeType::Identifier(tmp_ident.clone().into()),
                                        )),
                                        field: PotentialDollarIdentifier::new(node.span, "next"),
                                    },
                                );

                                let bind_node = match left.node_type {
                                    AstNodeType::Identifier(ident) => AstNode::new(
                                        node.span,
                                        AstNodeType::VariableDeclaration {
                                            var_type: VarType::Immutable,
                                            identifier: ident.into(),
                                            data_type: ParserDataType::auto(node.span),
                                            value: Box::new(extracted),
                                        },
                                    ),
                                    _ => AstNode::new(
                                        node.span,
                                        AstNodeType::AssignmentExpression {
                                            identifier: Box::new(left),
                                            value: Box::new(extracted),
                                        },
                                    ),
                                };

                                let mut body_items = vec![bind_node];
                                let done_and_arm = AstNode::new(
                                    node.span,
                                    AstNodeType::ScopeDeclaration {
                                        body: Some(vec![
                                            set_done_node(),
                                            arm.body.clone(),
                                            break_node(),
                                        ]),
                                        named: None,
                                        is_temp: true,
                                        create_new_scope: Some(true),
                                        define: false,
                                    },
                                );
                                if arm.conditionals.is_empty() {
                                    body_items.push(done_and_arm);
                                } else {
                                    let mut guard_cond = arm.conditionals[0].clone();
                                    for guard in arm.conditionals.iter().skip(1) {
                                        guard_cond = AstNode::new(
                                            node.span,
                                            AstNodeType::BooleanExpression {
                                                left: Box::new(guard_cond),
                                                right: Box::new(guard.clone()),
                                                operator: BooleanOperator::And,
                                            },
                                        );
                                    }
                                    body_items.push(AstNode::new(
                                        node.span,
                                        AstNodeType::IfStatement {
                                            comparison: Box::new(IfComparisonType::If(guard_cond)),
                                            then: Box::new(done_and_arm),
                                            otherwise: None,
                                        },
                                    ));
                                }

                                let body = AstNode::new(
                                    node.span,
                                    AstNodeType::ScopeDeclaration {
                                        body: Some(body_items),
                                        named: None,
                                        is_temp: true,
                                        create_new_scope: Some(true),
                                        define: false,
                                    },
                                );

                                loop_body.push(AstNode::new(
                                    node.span,
                                    AstNodeType::IfStatement {
                                        comparison: Box::new(IfComparisonType::If(cond)),
                                        then: Box::new(body),
                                        otherwise: None,
                                    },
                                ));
                            }
                            SelectArmKind::Send => {
                                let Some(left) = left.clone() else { continue };
                                let Some(right) = right.clone() else { continue };

                                let cond = fold_guards(
                                    AstNode::call(
                                        node.span,
                                        AstNode::member(node.span, left, "try_send"),
                                        vec![CallArg::Value(right)],
                                    ),
                                    &arm.conditionals,
                                );

                                let body = AstNode::new(
                                    node.span,
                                    AstNodeType::ScopeDeclaration {
                                        body: Some(vec![
                                            set_done_node(),
                                            arm.body.clone(),
                                            break_node(),
                                        ]),
                                        named: None,
                                        is_temp: true,
                                        create_new_scope: Some(true),
                                        define: false,
                                    },
                                );

                                loop_body.push(AstNode::new(
                                    node.span,
                                    AstNodeType::IfStatement {
                                        comparison: Box::new(IfComparisonType::If(cond)),
                                        then: Box::new(body),
                                        otherwise: None,
                                    },
                                ));
                            }
                            SelectArmKind::Default => {
                                has_default = true;
                                let mut body_items = vec![AstNode::new(
                                    node.span,
                                    AstNodeType::AssignmentExpression {
                                        identifier: Box::new(done_ident_node()),
                                        value: Box::new(AstNode::bool(node.span, true)),
                                    },
                                )];
                                body_items.push(arm.body.clone());
                                body_items.push(break_node());
                                let default_body = AstNode::new(
                                    node.span,
                                    AstNodeType::ScopeDeclaration {
                                        body: Some(body_items),
                                        named: None,
                                        is_temp: true,
                                        create_new_scope: Some(true),
                                        define: false,
                                    },
                                );
                                let cond = fold_guards(
                                    AstNode::new(
                                        node.span,
                                        AstNodeType::NotExpression {
                                            value: Box::new(done_ident_node()),
                                        },
                                    ),
                                    &arm.conditionals,
                                );
                                loop_body.push(AstNode::new(
                                    node.span,
                                    AstNodeType::IfStatement {
                                        comparison: Box::new(IfComparisonType::If(cond)),
                                        then: Box::new(default_body),
                                        otherwise: None,
                                    },
                                ));
                            }
                        }
                    }
                }

                loop_body.push(AstNode::new(
                    node.span,
                    AstNodeType::IfStatement {
                        comparison: Box::new(IfComparisonType::If(done_ident_node())),
                        then: Box::new(break_node()),
                        otherwise: None,
                    },
                ));

                if !has_default {
                    loop_body.push(AstNode::new(
                        node.span,
                        AstNodeType::IfStatement {
                            comparison: Box::new(IfComparisonType::If(AstNode::new(
                                node.span,
                                AstNodeType::NotExpression {
                                    value: Box::new(done_ident_node()),
                                },
                            ))),
                            then: Box::new(AstNode::call(
                                node.span,
                                AstNode::identifier(node.span, "wait"),
                                vec![CallArg::Value(AstNode::int(node.span, 1))],
                            )),
                            otherwise: None,
                        },
                    ));
                }

                let loop_body = AstNode::new(
                    node.span,
                    AstNodeType::ScopeDeclaration {
                        body: Some(loop_body),
                        named: None,
                        is_temp: true,
                        create_new_scope: Some(true),
                        define: false,
                    },
                );

                let select_loop = AstNode::new(
                    node.span,
                    AstNodeType::LoopDeclaration {
                        loop_type: Box::new(LoopType::Loop),
                        body: Box::new(loop_body),
                        until: None,
                        label: None,
                        else_body: None,
                    },
                );

                self.evaluate_inner(
                    scope,
                    AstNode::new(
                        node.span,
                        AstNodeType::ScopeDeclaration {
                            body: Some(vec![done_decl, select_loop]),
                            named: None,
                            is_temp: true,
                            create_new_scope: Some(false),
                            define: false,
                        },
                    ),
                )
            }
            AstNodeType::PipeExpression(mut path) if !path.is_empty() => {
                let mut value = path.remove(0).into();
                let mut prior_mappings = UstrMap::default();

                let is_callable_point = |env: &mut Self, point: &PipeSegment| {
                    if let AstNodeType::Identifier(id) = &point.get_node().node_type
                        && let Ok(resolved) = env.resolve(scope, id, ResolutionOptions::idents())
                        && env
                            .symbols
                            .variables
                            .get(&resolved)
                            .is_some_and(|var| var.data_type.is_callable())
                    {
                        return true;
                    }
                    let from_type = env
                        .resolve_type_from_node(scope, point.get_node())
                        .map(|x| x.unwrap_all_refs().data_type);

                    from_type.map(|x| x.is_callable()).unwrap_or_default()
                };

                let get_mapping = |env: &Self, key: &Ustr| -> Result<Option<Ustr>, MiddleErr> {
                    Ok(env.scoping.scope_or_err(scope)?.mappings.get(key).cloned())
                };

                let restore_mapping =
                    |env: &mut Self, key: Ustr, value: Option<Ustr>| -> Result<(), MiddleErr> {
                        let scope_ref = env.scoping.scope_mut_or_err(scope)?;
                        if let Some(v) = value {
                            scope_ref.mappings.insert(key, v);
                        } else {
                            scope_ref.mappings.remove(&key);
                        }
                        Ok(())
                    };

                prior_mappings.insert(Ustr::from("$"), get_mapping(self, &Ustr::from("$"))?);

                let mut idx = 0usize;
                while idx < path.len() {
                    let point = path[idx].clone();
                    let next_point = path.get(idx + 1).cloned();
                    let point_callable = is_callable_point(self, &point);
                    let point_is_identifier =
                        matches!(point.get_node().node_type, AstNodeType::Identifier(_));

                    if !point.is_named()
                        && !point.get_node().node_type.is_call()
                        && !point_callable
                        && let Some(next) = next_point
                        && !next.is_named()
                        && !next.get_node().node_type.is_call()
                        && is_callable_point(self, &next)
                    {
                        value = AstNode::call(
                            self.context.current_span(),
                            next.into(),
                            vec![CallArg::Value(value), CallArg::Value(point.into())],
                        );
                        idx += 2;
                        continue;
                    }

                    match point_callable || point_is_identifier {
                        true if !point.is_named() && !point.get_node().node_type.is_call() => {
                            value = AstNode::call(
                                self.context.current_span(),
                                point.into(),
                                vec![CallArg::Value(value)],
                            )
                        }
                        _ => {
                            let keep_scope = point.is_named();
                            let var_dec = match &point {
                                PipeSegment::Named { identifier, .. } => {
                                    let ident = self.resolve(
                                        scope,
                                        identifier,
                                        ResolutionOptions::default().with_dollar(),
                                    )?;

                                    prior_mappings.insert(ident, get_mapping(self, &ident)?);

                                    AstNode::new(
                                        self.context.current_span(),
                                        AstNodeType::VariableDeclaration {
                                            var_type: VarType::Mutable,
                                            identifier: PotentialDollarIdentifier::new(
                                                node.span, ident,
                                            ),
                                            value: Box::new(value),
                                            data_type: ParserDataType::auto(
                                                self.context.current_span(),
                                            ),
                                        },
                                    )
                                }
                                _ => AstNode::new(
                                    self.context.current_span(),
                                    AstNodeType::VariableDeclaration {
                                        var_type: VarType::Mutable,
                                        identifier: ParserText::from("$".to_string()).into(),
                                        value: Box::new(value),
                                        data_type: ParserDataType::auto(
                                            self.context.current_span(),
                                        ),
                                    },
                                ),
                            };

                            let point: AstNode = point.into();
                            value = match point.node_type {
                                AstNodeType::ScopeDeclaration {
                                    body: Some(mut body),
                                    named: None,
                                    is_temp,
                                    create_new_scope: _,
                                    define,
                                } => {
                                    body.insert(0, var_dec);

                                    AstNode {
                                        node_type: AstNodeType::ScopeDeclaration {
                                            body: Some(body),
                                            named: None,
                                            is_temp,
                                            create_new_scope: Some(!keep_scope),
                                            define,
                                        },
                                        ..point
                                    }
                                }
                                _ => AstNode::new(
                                    self.context.current_span(),
                                    AstNodeType::ScopeDeclaration {
                                        body: Some(vec![var_dec, point]),
                                        named: None,
                                        is_temp: true,
                                        create_new_scope: Some(!keep_scope),
                                        define: false,
                                    },
                                ),
                            }
                        }
                    }
                    idx += 1;
                }

                for (k, v) in prior_mappings {
                    restore_mapping(self, k, v)?;
                }

                self.evaluate_inner(scope, value)
            }
            AstNodeType::PipeExpression(_) => Ok(MiddleNode::new(
                MiddleNodeType::EmptyLine,
                self.context.current_span(),
            )),
        }
    }
}
