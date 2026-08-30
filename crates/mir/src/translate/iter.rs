use crate::{
    ast::MiddleNode, environment::MiddleEnvironment, errors::MiddleErr, scoping::ScopeId,
    symbols::resolve::ResolutionOptions,
};
use calibre_parser::{
    Span,
    ast::{
        binary::BinaryOperator,
        comparison::BooleanOperator,
        idents::{ParserText, PotentialDollarIdentifier},
        matching::MatchArmType,
        nodes::{AstNode, AstNodeType, CallArg, IfComparisonType, LoopType, VarType},
        types::{ParserDataType, ParserInnerType},
    },
};
use tracing::instrument;

pub fn transform_spawn_iter(
    span: Span,
    data_type: ParserDataType,
    map: AstNode,
    loop_type: Box<LoopType>,
    conditionals: Vec<AstNode>,
    until: Option<Box<AstNode>>,
) -> AstNode {
    let chan_ident: PotentialDollarIdentifier =
        ParserText::from(String::from("anon_iter_chan")).into();

    let wg_ident: PotentialDollarIdentifier = ParserText::from(String::from("anon_iter_wg")).into();

    let list_ident: PotentialDollarIdentifier =
        ParserText::from(String::from("anon_iter_list")).into();
    let item_ident: PotentialDollarIdentifier =
        ParserText::from(String::from("anon_iter_item")).into();
    let value_ident: PotentialDollarIdentifier =
        ParserText::from(String::from("anon_iter_value")).into();

    let list_ident_node = AstNode::identifier(span, &list_ident);

    let mut spawned_loop_items: Vec<AstNode> = conditionals
        .into_iter()
        .map(|condition| {
            AstNode::new(
                span,
                AstNodeType::IfStatement {
                    comparison: Box::new(IfComparisonType::If(condition)),
                    then: Box::new(AstNode::new(span, AstNodeType::EmptyLine)),
                    otherwise: Some(Box::new(AstNode::new(
                        span,
                        AstNodeType::Continue { label: None },
                    ))),
                },
            )
        })
        .collect();

    spawned_loop_items.push(AstNode::new(
        span,
        AstNodeType::VariableDeclaration {
            var_type: VarType::Immutable,
            identifier: value_ident.clone(),
            data_type: ParserDataType::auto(span),
            value: Box::new(map),
        },
    ));

    spawned_loop_items.push(AstNode::call(
        span,
        AstNode::member(span, AstNode::identifier(span, &chan_ident), "send"),
        vec![CallArg::Value(AstNode::new(
            span,
            AstNodeType::Identifier(value_ident.into()),
        ))],
    ));

    let dispatch_loop = AstNode::new(
        span,
        AstNodeType::LoopDeclaration {
            loop_type,
            until,
            label: None,
            else_body: None,
            body: Box::new(AstNode::new_temp_scope(vec![AstNode::call(
                span,
                AstNode::member(span, AstNode::identifier(span, &wg_ident), "join"),
                vec![CallArg::Value(AstNode::new(
                    span,
                    AstNodeType::Spawn {
                        items: vec![AstNode::new_temp_scope(spawned_loop_items)],
                        auto_wait: false,
                    },
                ))],
            )])),
        },
    );

    let channel_get = AstNode::call(
        span,
        AstNode::member(span, AstNode::identifier(span, &chan_ident), "get"),
        Vec::new(),
    );

    let collect_loop = AstNode::new(
        span,
        AstNodeType::LoopDeclaration {
            loop_type: Box::new(LoopType::Loop),
            until: None,
            label: None,
            else_body: None,
            body: Box::new(AstNode::new_temp_scope(vec![AstNode::new(
                span,
                AstNodeType::MatchStatement {
                    value: Some(Box::new(channel_get)),
                    body: vec![
                        (
                            calibre_parser::ast::matching::MatchArmType::Enum {
                                value: ParserText::from(String::from("Some")).into(),
                                var_type: VarType::Immutable,
                                name: Some(item_ident.clone()),
                                destructure: None,
                                pattern: None,
                            },
                            Vec::new(),
                            Box::new(AstNode::new(
                                span,
                                AstNodeType::AssignmentExpression {
                                    identifier: Box::new(list_ident_node.clone()),
                                    value: Box::new(AstNode::new(
                                        span,
                                        AstNodeType::BinaryExpression {
                                            left: Box::new(list_ident_node.clone()),
                                            right: Box::new(AstNode::new(
                                                span,
                                                AstNodeType::Identifier(item_ident.clone().into()),
                                            )),
                                            operator: BinaryOperator::Shl,
                                        },
                                    )),
                                },
                            )),
                        ),
                        (
                            MatchArmType::Wildcard(span),
                            Vec::new(),
                            Box::new(AstNode::new(
                                span,
                                AstNodeType::Break {
                                    label: None,
                                    value: None,
                                },
                            )),
                        ),
                    ],
                },
            )])),
        },
    );

    let list_type = ParserDataType::new(span, ParserInnerType::List(Box::new(data_type.clone())));

    AstNode::new_temp_scope(vec![
        AstNode::new(
            span,
            AstNodeType::VariableDeclaration {
                var_type: VarType::Mutable,
                identifier: chan_ident.clone(),
                data_type: ParserDataType::auto(span),
                value: Box::new(AstNode::call_with_generics(
                    span,
                    AstNode::member(span, AstNode::identifier(span, "Channel"), "new"),
                    vec![data_type.clone()],
                    Vec::new(),
                )),
            },
        ),
        AstNode::new(
            span,
            AstNodeType::VariableDeclaration {
                var_type: VarType::Mutable,
                identifier: wg_ident.clone(),
                data_type: ParserDataType::object(span, "WaitGroup"),
                value: Box::new(AstNode::call(
                    span,
                    AstNode::member(span, AstNode::identifier(span, "WaitGroup"), "new"),
                    Vec::new(),
                )),
            },
        ),
        dispatch_loop,
        AstNode::call(
            span,
            AstNode::member(span, AstNode::identifier(span, &wg_ident), "wait"),
            Vec::new(),
        ),
        AstNode::call(
            span,
            AstNode::member(span, AstNode::identifier(span, &chan_ident), "close"),
            Vec::new(),
        ),
        AstNode::new(
            span,
            AstNodeType::VariableDeclaration {
                var_type: VarType::Mutable,
                identifier: list_ident.clone(),
                value: Box::new(AstNode::new(
                    span,
                    AstNodeType::ListLiteral(data_type.clone(), Vec::new()),
                )),
                data_type: list_type.clone(),
            },
        ),
        collect_loop,
        list_ident_node,
    ])
}

impl MiddleEnvironment {
    #[instrument(skip_all)]
    pub fn evaluate_iter_expression(
        &mut self,
        scope: ScopeId,
        data_type: ParserDataType,
        map: Box<AstNode>,
        spawned: bool,
        loop_type: Box<LoopType>,
        conditionals: Vec<AstNode>,
        until: Option<Box<AstNode>>,
    ) -> Result<MiddleNode, MiddleErr> {
        let span = self.context.current_span();
        let resolved_data_type = if data_type.is_auto() {
            self.resolve_type_from_node(scope, &map)
                .ok_or_else(|| self.context.err_at_current(MiddleErr::InferImpossible))?
        } else {
            self.resolve_data_type(scope, &data_type, ResolutionOptions::typing())?
        };

        if spawned {
            return self.evaluate_inner(
                scope,
                transform_spawn_iter(
                    span,
                    resolved_data_type.clone(),
                    *map,
                    loop_type,
                    conditionals,
                    until,
                ),
            );
        }

        let list_ident = PotentialDollarIdentifier::from(ParserText::temp_name_with_suffix(
            "anon_iter_list",
            span,
        ));
        let list_ident_node = AstNode::identifier(span, &list_ident);

        let list_type = ParserDataType::new(
            span,
            ParserInnerType::List(Box::new(resolved_data_type.clone())),
        );

        let guard = conditionals.into_iter().reduce(|left, right| {
            AstNode::new(
                span,
                AstNodeType::BooleanExpression {
                    left: Box::new(left),
                    right: Box::new(right),
                    operator: BooleanOperator::And,
                },
            )
        });

        let block = AstNode::new_temp_scope(vec![AstNode::new(
            span,
            AstNodeType::AssignmentExpression {
                identifier: Box::new(list_ident_node.clone()),
                value: Box::new(AstNode::new(
                    span,
                    AstNodeType::BinaryExpression {
                        left: Box::new(list_ident_node.clone()),
                        right: map,
                        operator: BinaryOperator::Shl,
                    },
                )),
            },
        )]);

        let block = if let Some(cond) = guard {
            AstNode::new_temp_scope(vec![AstNode::new(
                span,
                AstNodeType::IfStatement {
                    comparison: Box::new(IfComparisonType::If(cond)),
                    then: Box::new(block),
                    otherwise: None,
                },
            )])
        } else {
            block
        };

        let loop_node = AstNode::new(
            span,
            AstNodeType::LoopDeclaration {
                loop_type,
                until,
                label: None,
                else_body: None,
                body: Box::new(block),
            },
        );

        self.evaluate_inner(
            scope,
            AstNode::new_temp_scope(vec![
                AstNode::new(
                    span,
                    AstNodeType::VariableDeclaration {
                        var_type: VarType::Mutable,
                        identifier: list_ident.clone(),
                        value: Box::new(AstNode::new(
                            span,
                            AstNodeType::ListLiteral(data_type.clone(), Vec::new()),
                        )),
                        data_type: list_type,
                    },
                ),
                loop_node,
                AstNode::emit(AstNode::identifier(span, list_ident)),
            ]),
        )
    }
}
