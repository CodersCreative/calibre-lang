use calibre_parser::{
    Span,
    ast::{
        CallArg, IfComparisonType, MatchArmType, Node, NodeType, ParserDataType, ParserInnerType,
        ParserText, PotentialGenericTypeIdentifier, PotentialNewType, RefMutability, VarType,
        comparison::{BooleanOperator, ComparisonOperator},
    },
};

use crate::{
    ast::MiddleNode,
    environment::{MiddleEnvironment, MiddleTypeDefType},
    errors::MiddleErr,
};

impl MiddleEnvironment {
    pub fn evaluate_match_statement(
        &mut self,
        scope: &u64,
        span: Span,
        value: Option<Box<Node>>,
        body: Vec<(MatchArmType, Vec<Node>, Box<Node>)>,
    ) -> Result<MiddleNode, MiddleErr> {
        let (resolved_data_type, decl, value) = if let Some(value) = value {
            let tmp_name = format!("__match_tmp_{}_{}", span.from.line, span.from.col);
            (
                self.resolve_type_from_node(scope, &value),
                Some(Node::new(
                    self.current_span(),
                    NodeType::VariableDeclaration {
                        var_type: VarType::Mutable,
                        identifier: ParserText::from(tmp_name.clone()).into(),
                        data_type: PotentialNewType::DataType(ParserDataType::new(
                            self.current_span(),
                            ParserInnerType::Auto(None),
                        )),
                        value,
                    },
                )),
                Some(Node::new(
                    self.current_span(),
                    NodeType::Identifier(PotentialGenericTypeIdentifier::Identifier(
                        ParserText::from(tmp_name).into(),
                    )),
                )),
            )
        } else {
            (None, None, None)
        };

        let mut ifs: Vec<Node> = Vec::new();
        let mut reference = None;
        let resolved_unwrapped = resolved_data_type
            .as_ref()
            .map(|t| t.clone().unwrap_all_refs());
        let enum_object = if let Some(resolved_data_type) = &resolved_data_type {
            reference = Some(match &resolved_data_type.data_type {
                ParserInnerType::Ref(_, mutability) => mutability.clone(),
                _ => RefMutability::Value,
            });
            let enum_key = match &resolved_unwrapped
                .as_ref()
                .unwrap_or(resolved_data_type)
                .data_type
            {
                ParserInnerType::Struct(name) => name.clone(),
                ParserInnerType::StructWithGenerics { identifier, .. } => identifier.clone(),
                other => other.to_string(),
            };
            if let Some(x) = self.objects.get(&enum_key) {
                match &x.object_type {
                    MiddleTypeDefType::Enum(x) => Some(x),
                    _ => None,
                }
            } else {
                None
            }
        } else {
            None
        };
        for mut pattern in body {
            let mut conditionals = if pattern.1.is_empty() {
                Node::new(
                    self.current_span(),
                    NodeType::Identifier(ParserText::from("true".to_string()).into()),
                )
            } else {
                pattern.1.remove(0)
            };

            for condition in pattern.1 {
                conditionals = Node::new(
                    self.current_span(),
                    NodeType::BooleanExpression {
                        left: Box::new(conditionals),
                        right: Box::new(condition),
                        operator: BooleanOperator::And,
                    },
                );
            }

            let (Some(value), Some(resolved_data_type)) =
                (value.clone(), resolved_data_type.as_ref())
            else {
                if let Some(value) = value.clone() {
                    match pattern.0 {
                        MatchArmType::Wildcard(_) => ifs.push(Node::new(
                            self.current_span(),
                            NodeType::IfStatement {
                                comparison: Box::new(IfComparisonType::If(conditionals)),
                                then: pattern.2,
                                otherwise: None,
                            },
                        )),
                        MatchArmType::Value(x) => ifs.push(Node::new(
                            self.current_span(),
                            NodeType::IfStatement {
                                comparison: Box::new(IfComparisonType::If(Node::new(
                                    self.current_span(),
                                    NodeType::BooleanExpression {
                                        left: Box::new(Node::new(
                                            self.current_span(),
                                            NodeType::ComparisonExpression {
                                                left: Box::new(value.clone()),
                                                right: Box::new(x),
                                                operator: ComparisonOperator::Equal,
                                            },
                                        )),
                                        right: Box::new(conditionals),
                                        operator: BooleanOperator::And,
                                    },
                                ))),
                                then: pattern.2,
                                otherwise: None,
                            },
                        )),
                        MatchArmType::Enum {
                            value: val,
                            var_type,
                            name,
                            destructure,
                        } => {
                            let val =
                                self.resolve_dollar_ident_only(scope, &val).ok_or_else(|| {
                                    self.err_at_current(MiddleErr::Scope(val.to_string()))
                                })?;
                            let index: i64 = match val.text.trim() {
                                "Ok" | "Some" => 0,
                                "Err" | "None" => 1,
                                _ => {
                                    return Err(MiddleErr::At(
                                        val.span,
                                        Box::new(MiddleErr::CantMatch(ParserDataType::new(
                                            val.span,
                                            ParserInnerType::Auto(None),
                                        ))),
                                    ));
                                }
                            };

                            ifs.push(Node::new(
                                self.current_span(),
                                NodeType::IfStatement {
                                    comparison: Box::new(IfComparisonType::If(Node::new(
                                        self.current_span(),
                                        NodeType::BooleanExpression {
                                            left: Box::new(Node::new(
                                                self.current_span(),
                                                NodeType::ComparisonExpression {
                                                    left: Box::new(Node::new(
                                                        self.current_span(),
                                                        NodeType::CallExpression {
                                                            string_fn: None,
                                                            generic_types: Vec::new(),
                                                            caller: Box::new(Node::new(
                                                                self.current_span(),
                                                                NodeType::Identifier(
                                                                    ParserText::from(String::from(
                                                                        "discriminant",
                                                                    ))
                                                                    .into(),
                                                                ),
                                                            )),
                                                            args: vec![CallArg::Value(
                                                                value.clone(),
                                                            )],
                                                            reverse_args: Vec::new(),
                                                        },
                                                    )),
                                                    right: Box::new(Node::new(
                                                        self.current_span(),
                                                        NodeType::IntLiteral(index.to_string()),
                                                    )),
                                                    operator: ComparisonOperator::Equal,
                                                },
                                            )),
                                            right: Box::new(conditionals),
                                            operator: BooleanOperator::And,
                                        },
                                    ))),
                                    then: {
                                        Box::new(if name.is_some() || destructure.is_some() {
                                            let bind_name = if let Some(name) = name {
                                                name
                                            } else {
                                                ParserText::from(format!(
                                                    "__match_destructure_{}_{}",
                                                    self.current_span().from.line,
                                                    self.current_span().from.col
                                                ))
                                                .into()
                                            };
                                            let mut body_nodes = Vec::new();
                                            body_nodes.push(Node::new(
                                                self.current_span(),
                                                NodeType::VariableDeclaration {
                                                    var_type: var_type.clone(),
                                                    identifier: bind_name.clone(),
                                                    value: Box::new(Node::new(
                                                        self.current_span(),
                                                        NodeType::MemberExpression {
                                                            path: vec![
                                                                (value, false),
                                                                (
                                                                    Node::new(
                                                                        self.current_span(),
                                                                        NodeType::Identifier(
                                                                            ParserText::from(
                                                                                String::from(
                                                                                    "next",
                                                                                ),
                                                                            )
                                                                            .into(),
                                                                        ),
                                                                    ),
                                                                    false,
                                                                ),
                                                            ],
                                                        },
                                                    )),
                                                    data_type: PotentialNewType::DataType(
                                                        ParserDataType::new(
                                                            self.current_span(),
                                                            ParserInnerType::Auto(None),
                                                        ),
                                                    ),
                                                },
                                            ));

                                            if let Some(pattern) = destructure {
                                                body_nodes.extend(
                                                    self.emit_destructure_statements(
                                                        &bind_name,
                                                        &pattern,
                                                        self.current_span(),
                                                        true,
                                                    ),
                                                );
                                            }

                                            body_nodes.push(*pattern.2);

                                            Node::new(
                                                self.current_span(),
                                                NodeType::ScopeDeclaration {
                                                    body: Some(body_nodes),
                                                    is_temp: true,
                                                    create_new_scope: Some(true),
                                                    named: None,
                                                    define: false,
                                                },
                                            )
                                        } else {
                                            *pattern.2
                                        })
                                    },
                                    otherwise: None,
                                },
                            ));
                        }
                        _ => unreachable!(),
                    }
                }

                continue;
            };

            match pattern.0 {
                MatchArmType::Wildcard(_) => ifs.push(Node::new(
                    self.current_span(),
                    NodeType::IfStatement {
                        comparison: Box::new(IfComparisonType::If(conditionals)),
                        then: pattern.2,
                        otherwise: None,
                    },
                )),
                MatchArmType::Value(x) => ifs.push(Node::new(
                    self.current_span(),
                    NodeType::IfStatement {
                        comparison: Box::new(IfComparisonType::If(Node::new(
                            self.current_span(),
                            NodeType::BooleanExpression {
                                left: Box::new(Node::new(
                                    self.current_span(),
                                    NodeType::ComparisonExpression {
                                        left: Box::new(value),
                                        right: Box::new(x),
                                        operator: ComparisonOperator::Equal,
                                    },
                                )),
                                right: Box::new(conditionals),
                                operator: BooleanOperator::And,
                            },
                        ))),
                        then: pattern.2,
                        otherwise: None,
                    },
                )),
                MatchArmType::Let { var_type, name } => ifs.push(Node::new(
                    self.current_span(),
                    NodeType::IfStatement {
                        comparison: Box::new(IfComparisonType::If(conditionals)),
                        then: Box::new(Node::new(
                            self.current_span(),
                            NodeType::ScopeDeclaration {
                                body: Some(vec![
                                    Node::new(
                                        self.current_span(),
                                        NodeType::VariableDeclaration {
                                            var_type,
                                            identifier: name,
                                            value: Box::new(value.clone()),
                                            data_type: resolved_data_type.clone().into(),
                                        },
                                    ),
                                    *pattern.2,
                                ]),
                                create_new_scope: Some(true),
                                define: false,
                                named: None,
                                is_temp: true,
                            },
                        )),
                        otherwise: None,
                    },
                )),
                MatchArmType::Enum {
                    value: val,
                    var_type,
                    name,
                    destructure,
                } => {
                    let val = self.resolve_dollar_ident_only(scope, &val).ok_or_else(|| {
                        MiddleErr::At(*val.span(), Box::new(MiddleErr::Scope(val.to_string())))
                    })?;
                    let index: i64 = match val.text.trim() {
                        _ if enum_object.is_some() => {
                            let Some(object) = enum_object else {
                                return Err(MiddleErr::At(
                                    val.span,
                                    Box::new(MiddleErr::CantMatch(resolved_data_type.clone())),
                                ));
                            };
                            let Some(index) = object.iter().position(|x| x.0.text == val.text)
                            else {
                                return Err(MiddleErr::At(
                                    val.span,
                                    Box::new(MiddleErr::EnumVariant(val.text)),
                                ));
                            };
                            index as i64
                        }
                        "Ok" => 0,
                        "Err" => 1,
                        "Some" => 0,
                        "None" => 1,
                        _ => {
                            return Err(MiddleErr::At(
                                val.span,
                                Box::new(MiddleErr::CantMatch(resolved_data_type.clone())),
                            ));
                        }
                    };

                    ifs.push(Node::new(
                        self.current_span(),
                        NodeType::IfStatement {
                            comparison: Box::new(IfComparisonType::If(Node::new(
                                self.current_span(),
                                NodeType::BooleanExpression {
                                    left: Box::new(Node::new(
                                        self.current_span(),
                                        NodeType::ComparisonExpression {
                                            left: Box::new(Node::new(
                                                self.current_span(),
                                                NodeType::CallExpression {
                                                    string_fn: None,
                                                    generic_types: Vec::new(),
                                                    caller: Box::new(Node::new(
                                                        self.current_span(),
                                                        NodeType::Identifier(
                                                            ParserText::from(String::from(
                                                                "discriminant",
                                                            ))
                                                            .into(),
                                                        ),
                                                    )),
                                                    args: vec![CallArg::Value(value.clone())],
                                                    reverse_args: Vec::new(),
                                                },
                                            )),
                                            right: Box::new(Node::new(
                                                self.current_span(),
                                                NodeType::IntLiteral(index.to_string()),
                                            )),
                                            operator: ComparisonOperator::Equal,
                                        },
                                    )),
                                    right: Box::new(conditionals),
                                    operator: BooleanOperator::And,
                                },
                            ))),
                            then: {
                                Box::new(if name.is_some() || destructure.is_some() {
                                    let bind_name = if let Some(name) = name {
                                        name
                                    } else {
                                        ParserText::from(format!(
                                            "__match_destructure_{}_{}",
                                            self.current_span().from.line,
                                            self.current_span().from.col
                                        ))
                                        .into()
                                    };
                                    let mut body_nodes = Vec::new();
                                    body_nodes.push(Node::new(
                                        self.current_span(),
                                        NodeType::VariableDeclaration {
                                            var_type: var_type.clone(),
                                            identifier: bind_name.clone(),
                                            value: if reference.is_some()
                                                && reference != Some(RefMutability::Value)
                                            {
                                                let mutability =
                                                    reference.clone().ok_or_else(|| {
                                                        MiddleErr::At(
                                                            value.span,
                                                            Box::new(MiddleErr::Internal(
                                                                "missing reference mutability"
                                                                    .to_string(),
                                                            )),
                                                        )
                                                    })?;
                                                Box::new(Node::new(
                                                    self.current_span(),
                                                    NodeType::RefStatement {
                                                        mutability,
                                                        value: Box::new(Node::new(
                                                            self.current_span(),
                                                            NodeType::MemberExpression {
                                                                path: vec![
                                                                    (value, false),
                                                                    (
                                                                        Node::new(
                                                                            self.current_span(),
                                                                            NodeType::Identifier(
                                                                                ParserText::from(
                                                                                    String::from(
                                                                                        "next",
                                                                                    ),
                                                                                )
                                                                                .into(),
                                                                            ),
                                                                        ),
                                                                        false,
                                                                    ),
                                                                ],
                                                            },
                                                        )),
                                                    },
                                                ))
                                            } else {
                                                Box::new(Node::new(
                                                    self.current_span(),
                                                    NodeType::MemberExpression {
                                                        path: vec![
                                                            (value, false),
                                                            (
                                                                Node::new(
                                                                    self.current_span(),
                                                                    NodeType::Identifier(
                                                                        ParserText::from(
                                                                            String::from("next"),
                                                                        )
                                                                        .into(),
                                                                    ),
                                                                ),
                                                                false,
                                                            ),
                                                        ],
                                                    },
                                                ))
                                            },
                                            data_type: PotentialNewType::DataType(
                                                ParserDataType::new(
                                                    self.current_span(),
                                                    ParserInnerType::Auto(None),
                                                ),
                                            ),
                                        },
                                    ));

                                    if let Some(pattern) = destructure {
                                        body_nodes.extend(self.emit_destructure_statements(
                                            &bind_name,
                                            &pattern,
                                            self.current_span(),
                                            true,
                                        ));
                                    }

                                    body_nodes.push(*pattern.2);

                                    Node::new(
                                        self.current_span(),
                                        NodeType::ScopeDeclaration {
                                            body: Some(body_nodes),
                                            is_temp: true,
                                            create_new_scope: Some(true),
                                            named: None,
                                            define: false,
                                        },
                                    )
                                } else {
                                    *pattern.2
                                })
                            },
                            otherwise: None,
                        },
                    ));
                }
            }
        }
        let ifs = if ifs.is_empty() {
            Node::new(self.current_span(), NodeType::EmptyLine)
        } else {
            let Some(mut cur_if) = ifs.pop() else {
                return self.evaluate_inner(scope, Node::new(self.current_span(), NodeType::EmptyLine));
            };
            while let Some(mut prev) = ifs.pop() {
                if let NodeType::IfStatement { otherwise, .. } = &mut prev.node_type {
                    *otherwise = Some(Box::new(cur_if));
                }
                cur_if = prev;
            }
            cur_if
        };

        self.evaluate_inner(
            scope,
            if let Some(decl) = decl {
                Node::new(
                    self.current_span(),
                    NodeType::ScopeDeclaration {
                        body: Some(vec![decl, ifs]),
                        named: None,
                        is_temp: true,
                        create_new_scope: Some(true),
                        define: false,
                    },
                )
            } else {
                ifs
            },
        )
    }
}
