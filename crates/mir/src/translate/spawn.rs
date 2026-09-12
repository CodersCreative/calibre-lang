use crate::{
    ast::{MiddleNode, MiddleNodeType, MirSpawn},
    environment::MiddleEnvironment,
    errors::MiddleErr,
    scoping::ScopeId,
    translate::MirLowering,
};
use calibre_parser::{
    Span,
    ast::{
        RefMutability,
        comparison::{BooleanOperator, ComparisonOperator},
        idents::{ParserText, PotentialDollarIdentifier},
        nodes::{
            AstNode, AstNodeType, LoopType, VarType,
            access::AstField,
            binary::{AstBoolean, AstComparison},
            conditionals::{AstIf, IfComparisonType},
            flow::{AstBreak, AstEmit},
            functions::{AstFunction, CallArg, FunctionHeader},
            memory::AstRef,
            spawn::{AstSelect, AstSpawn, SelectArmKind},
            unary::AstNot,
        },
        types::{GenericTypes, ParserDataType, ParserInnerType},
    },
};

impl MirLowering for AstSelect {
    fn lower(
        self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        span: Span,
    ) -> Result<MiddleNode, MiddleErr> {
        let done_ident: PotentialDollarIdentifier =
            ParserText::temp_name_with_suffix("select_done", span).into();

        let done_decl = AstNode::new(
            span,
            AstNodeType::VariableDeclaration {
                var_type: VarType::Mutable,
                identifier: done_ident.clone(),
                data_type: ParserDataType::new(span, ParserInnerType::Bool),
                value: Box::new(AstNode::bool(span, false)),
            },
        );

        let mut loop_body = Vec::new();
        let mut has_default = false;

        let done_ident_node = || AstNode::identifier(span, done_ident.clone());

        let break_node = || {
            AstNode::new(
                span,
                AstNodeType::Break(AstBreak {
                    label: None,
                    value: None,
                }),
            )
        };

        let set_done_node = || {
            AstNode::new(
                span,
                AstNodeType::AssignmentExpression {
                    identifier: Box::new(done_ident_node()),
                    value: Box::new(AstNode::bool(span, true)),
                },
            )
        };

        let fold_guards = |initial: AstNode, guards: &[AstNode]| -> AstNode {
            let mut cond = initial;
            for guard in guards {
                cond = AstNode::new(
                    span,
                    AstNodeType::BooleanExpression(AstBoolean {
                        left: Box::new(cond),
                        right: Box::new(guard.clone()),
                        operator: BooleanOperator::And,
                    }),
                );
            }
            cond
        };

        for arm in self.arms {
            for (kind, left, right) in arm.patterns.iter() {
                match kind {
                    SelectArmKind::Recv => {
                        let Some(left) = left.clone() else { continue };
                        let Some(right) = right.clone() else { continue };
                        let tmp_ident = PotentialDollarIdentifier::Identifier(
                            ParserText::temp_name_with_suffix("select", span),
                        );

                        let try_get_call =
                            AstNode::call(span, AstNode::member(span, right, "try_get"), vec![]);

                        loop_body.push(AstNode::new(
                            span,
                            AstNodeType::VariableDeclaration {
                                var_type: VarType::Immutable,
                                identifier: tmp_ident.clone(),
                                data_type: ParserDataType::auto(span),
                                value: Box::new(try_get_call),
                            },
                        ));

                        let cond = AstNode::new(
                            span,
                            AstNodeType::ComparisonExpression(AstComparison {
                                left: Box::new(AstNode::new(
                                    span,
                                    AstNodeType::Identifier(tmp_ident.clone().into()),
                                )),
                                right: Box::new(AstNode::none(span)),
                                operator: ComparisonOperator::NotEqual,
                            }),
                        );

                        let extracted = AstNode::new(
                            span,
                            AstNodeType::FieldAccess(AstField {
                                base: Box::new(AstNode::new(
                                    span,
                                    AstNodeType::Identifier(tmp_ident.clone().into()),
                                )),
                                field: PotentialDollarIdentifier::new(span, "next"),
                            }),
                        );

                        let bind_node = match left.node_type {
                            AstNodeType::Identifier(ident) => AstNode::new(
                                span,
                                AstNodeType::VariableDeclaration {
                                    var_type: VarType::Immutable,
                                    identifier: ident.value.into(),
                                    data_type: ParserDataType::auto(span),
                                    value: Box::new(extracted),
                                },
                            ),
                            _ => AstNode::new(
                                span,
                                AstNodeType::AssignmentExpression {
                                    identifier: Box::new(left),
                                    value: Box::new(extracted),
                                },
                            ),
                        };

                        let mut body_items = vec![bind_node];
                        let done_and_arm = AstNode::new(
                            span,
                            AstNodeType::ScopeDeclaration {
                                body: Some(vec![set_done_node(), arm.body.clone(), break_node()]),
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
                                    span,
                                    AstNodeType::BooleanExpression(AstBoolean {
                                        left: Box::new(guard_cond),
                                        right: Box::new(guard.clone()),
                                        operator: BooleanOperator::And,
                                    }),
                                );
                            }
                            body_items.push(AstNode::new(
                                span,
                                AstNodeType::IfStatement(AstIf {
                                    comparison: Box::new(IfComparisonType::If(guard_cond)),
                                    then: Box::new(done_and_arm),
                                    otherwise: None,
                                }),
                            ));
                        }

                        let body = AstNode::new(
                            span,
                            AstNodeType::ScopeDeclaration {
                                body: Some(body_items),
                                named: None,
                                is_temp: true,
                                create_new_scope: Some(true),
                                define: false,
                            },
                        );

                        loop_body.push(AstNode::new(
                            span,
                            AstNodeType::IfStatement(AstIf {
                                comparison: Box::new(IfComparisonType::If(cond)),
                                then: Box::new(body),
                                otherwise: None,
                            }),
                        ));
                    }
                    SelectArmKind::Send => {
                        let Some(left) = left.clone() else { continue };
                        let Some(right) = right.clone() else { continue };

                        let cond = fold_guards(
                            AstNode::call(
                                span,
                                AstNode::member(span, left, "try_send"),
                                vec![CallArg::Value(right)],
                            ),
                            &arm.conditionals,
                        );

                        let body = AstNode::new(
                            span,
                            AstNodeType::ScopeDeclaration {
                                body: Some(vec![set_done_node(), arm.body.clone(), break_node()]),
                                named: None,
                                is_temp: true,
                                create_new_scope: Some(true),
                                define: false,
                            },
                        );

                        loop_body.push(AstNode::new(
                            span,
                            AstNodeType::IfStatement(AstIf {
                                comparison: Box::new(IfComparisonType::If(cond)),
                                then: Box::new(body),
                                otherwise: None,
                            }),
                        ));
                    }
                    SelectArmKind::Default => {
                        has_default = true;
                        let mut body_items = vec![AstNode::new(
                            span,
                            AstNodeType::AssignmentExpression {
                                identifier: Box::new(done_ident_node()),
                                value: Box::new(AstNode::bool(span, true)),
                            },
                        )];
                        body_items.push(arm.body.clone());
                        body_items.push(break_node());
                        let default_body = AstNode::new(
                            span,
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
                                span,
                                AstNodeType::NotExpression(AstNot {
                                    value: Box::new(done_ident_node()),
                                }),
                            ),
                            &arm.conditionals,
                        );
                        loop_body.push(AstNode::new(
                            span,
                            AstNodeType::IfStatement(AstIf {
                                comparison: Box::new(IfComparisonType::If(cond)),
                                then: Box::new(default_body),
                                otherwise: None,
                            }),
                        ));
                    }
                }
            }
        }

        loop_body.push(AstNode::new(
            span,
            AstNodeType::IfStatement(AstIf {
                comparison: Box::new(IfComparisonType::If(done_ident_node())),
                then: Box::new(break_node()),
                otherwise: None,
            }),
        ));

        if !has_default {
            loop_body.push(AstNode::new(
                span,
                AstNodeType::IfStatement(AstIf {
                    comparison: Box::new(IfComparisonType::If(AstNode::new(
                        span,
                        AstNodeType::NotExpression(AstNot {
                            value: Box::new(done_ident_node()),
                        }),
                    ))),
                    then: Box::new(AstNode::call(
                        span,
                        AstNode::identifier(span, "wait"),
                        vec![CallArg::Value(AstNode::int(span, 1))],
                    )),
                    otherwise: None,
                }),
            ));
        }

        let loop_body = AstNode::new(
            span,
            AstNodeType::ScopeDeclaration {
                body: Some(loop_body),
                named: None,
                is_temp: true,
                create_new_scope: Some(true),
                define: false,
            },
        );

        let select_loop = AstNode::new(
            span,
            AstNodeType::LoopDeclaration {
                loop_type: Box::new(LoopType::Loop),
                body: Box::new(loop_body),
                until: None,
                label: None,
                else_body: None,
            },
        );

        AstNode::new(
            span,
            AstNodeType::ScopeDeclaration {
                body: Some(vec![done_decl, select_loop]),
                named: None,
                is_temp: true,
                create_new_scope: Some(false),
                define: false,
            },
        )
        .lower(env, scope, span)
    }
}

impl MirLowering for AstSpawn {
    fn lower(
        mut self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        span: Span,
    ) -> Result<MiddleNode, MiddleErr> {
        if self.auto_wait {
            let ident: PotentialDollarIdentifier =
                ParserText::temp_name_with_suffix("spawn_wait_wg", span).into();

            AstNode::new_temp_scope_with_create(
                vec![
                    AstNode::new(
                        span,
                        AstNodeType::VariableDeclaration {
                            var_type: VarType::Immutable,
                            identifier: ident.clone(),
                            data_type: ParserDataType::object(span, "WaitGroup"),
                            value: Box::new(AstNode::new(
                                span,
                                AstNodeType::Spawn(AstSpawn {
                                    items: self.items,
                                    auto_wait: false,
                                }),
                            )),
                        },
                    ),
                    AstNode::call(
                        span,
                        AstNode::member(span, AstNode::identifier(span, ident), "wait"),
                        Vec::new(),
                    ),
                ],
                Some(false),
            )
            .lower(env, scope, span)
        } else if self.items.len() == 1 {
            let value: AstNode = self.items.remove(0);

            let inner = match value.node_type {
                AstNodeType::ScopeDeclaration { .. } => AstNode::new(
                    span,
                    AstNodeType::FunctionDeclaration(AstFunction {
                        header: FunctionHeader {
                            generics: GenericTypes::default(),
                            parameters: Vec::new(),
                            return_type: ParserDataType::auto(span),
                            param_destructures: Vec::new(),
                        },
                        body: Box::new(value),
                    }),
                ),
                AstNodeType::CallExpression { .. } => AstNode::new(
                    span,
                    AstNodeType::FunctionDeclaration(AstFunction {
                        header: FunctionHeader {
                            generics: GenericTypes::default(),
                            parameters: Vec::new(),
                            return_type: ParserDataType::auto(span),
                            param_destructures: Vec::new(),
                        },
                        body: Box::new(AstNode::new_temp_scope(vec![value])),
                    }),
                ),
                AstNodeType::LoopDeclaration {
                    loop_type,
                    body,
                    until,
                    label,
                    else_body,
                } => {
                    let ident: PotentialDollarIdentifier =
                        ParserText::temp_name_with_suffix("spawn_wg", span)
                            .clone()
                            .into();

                    let decl = AstNode::new(
                        span,
                        AstNodeType::VariableDeclaration {
                            var_type: VarType::Mutable,
                            identifier: ident.clone(),
                            data_type: ParserDataType::auto(span),
                            value: Box::new(AstNode::call(
                                span,
                                AstNode::member(
                                    span,
                                    AstNode::identifier(span, "WaitGroup"),
                                    "new",
                                ),
                                Vec::new(),
                            )),
                        },
                    );

                    let join_call = AstNode::call(
                        span,
                        AstNode::member(span, AstNode::identifier(span, ident.clone()), "join"),
                        vec![CallArg::Value(AstNode::new(
                            span,
                            AstNodeType::Spawn(AstSpawn {
                                items: vec![*body],
                                auto_wait: false,
                            }),
                        ))],
                    );

                    let loop_node = AstNode::new(
                        span,
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
                    return AstNode::new_temp_scope(vec![
                        decl,
                        loop_node,
                        AstNode::new(
                            span,
                            AstNodeType::Emit(AstEmit::Scope(Box::new(AstNode::identifier(
                                span, ident,
                            )))),
                        ),
                    ])
                    .lower(env, scope, span);
                }
                AstNodeType::FunctionDeclaration { .. } => value,
                _ => unimplemented!(),
            };

            Ok(MiddleNode::new(
                MiddleNodeType::Spawn(MirSpawn {
                    value: Box::new(inner.lower(env, scope, span)?),
                }),
                span,
            ))
        } else {
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

            for item in self.items {
                let item = match item.node_type {
                    AstNodeType::Spawn { .. } => item,
                    other => AstNode::new(
                        item.span,
                        AstNodeType::Spawn(AstSpawn {
                            items: vec![AstNode::new(item.span, other)],
                            auto_wait: false,
                        }),
                    ),
                };

                let join_call = AstNode::call(
                    span,
                    AstNode::member(span, AstNode::identifier(span, "WaitGroup"), "join"),
                    vec![
                        CallArg::Value(AstNode::new(
                            span,
                            AstNodeType::RefStatement(AstRef {
                                mutability: RefMutability::MutRef,
                                value: Box::new(AstNode::identifier(span, &ident)),
                            }),
                        )),
                        CallArg::Value(item),
                    ],
                );
                body.push(join_call);
            }

            body.push(AstNode::new(
                span,
                AstNodeType::Emit(AstEmit::Scope(Box::new(AstNode::identifier(span, ident)))),
            ));

            AstNode::new(
                span,
                AstNodeType::ScopeDeclaration {
                    body: Some(body),
                    named: None,
                    is_temp: true,
                    create_new_scope: Some(false),
                    define: false,
                },
            )
            .lower(env, scope, span)
        }
    }

    fn type_of(
        &self,
        _env: &mut MiddleEnvironment,
        _scope: ScopeId,
        span: Span,
    ) -> Option<ParserDataType> {
        Some(ParserDataType::new(
            span,
            if self.auto_wait {
                ParserInnerType::Null
            } else {
                ParserInnerType::Struct(String::from("WaitGroup"))
            },
        ))
    }
}
