use calibre_parser::{
    ast::{
        CallArg, FunctionHeader, LoopType, NamedScope, Node, NodeType, ParserDataType,
        ParserInnerType, ParserText, PotentialDollarIdentifier, PotentialGenericTypeIdentifier,
        PotentialNewType,
    },
    lexer::Span,
};

use crate::{
    ast::{MiddleNode, MiddleNodeType},
    environment::{MiddleEnvironment, ScopeMacro, get_disamubiguous_name},
    errors::MiddleErr,
};

impl MiddleEnvironment {
    fn desugar_use_chain(&mut self, mut stmts: Vec<Node>) -> Vec<Node> {
        let Some(pos) = stmts
            .iter()
            .position(|n| matches!(n.node_type, NodeType::Use { .. }))
        else {
            return stmts;
        };

        let mut prefix = stmts.drain(..pos).collect::<Vec<Node>>();
        let use_node = stmts.remove(0);
        let rest = stmts;

        let (identifiers, value) = match use_node.node_type {
            NodeType::Use { identifiers, value } => (identifiers, *value),
            other => {
                prefix.push(Node::new(use_node.span, other));
                return prefix;
            }
        };

        if matches!(value.node_type, NodeType::Spawn { .. }) {
            let span = use_node.span;
            let wg_ident = if let Some(first) = identifiers.first() {
                first.clone()
            } else {
                ParserText::from(format!("__use_spawn_wg_{}_{}", span.from.line, span.from.col))
                    .into()
            };

            let wg_decl = Node::new(
                span,
                NodeType::VariableDeclaration {
                    var_type: calibre_parser::ast::VarType::Immutable,
                    identifier: wg_ident.clone(),
                    data_type: PotentialNewType::DataType(ParserDataType::from(
                        ParserInnerType::Auto(None),
                    )),
                    value: Box::new(value),
                },
            );

            let wg_ident_node = Node::new(
                span,
                NodeType::Identifier(PotentialGenericTypeIdentifier::Identifier(
                    wg_ident.clone().into(),
                )),
            );

            let wait_call = Node::new(
                span,
                NodeType::CallExpression {
                    caller: Box::new(Node::new(
                        span,
                        NodeType::MemberExpression {
                            path: vec![
                                (wg_ident_node.clone(), false),
                                (
                                    Node::new(
                                        span,
                                        NodeType::Identifier(
                                            PotentialGenericTypeIdentifier::Identifier(
                                                ParserText::from(String::from("wait")).into(),
                                            ),
                                        ),
                                    ),
                                    false,
                                ),
                            ],
                        },
                    )),
                    generic_types: Vec::new(),
                    args: Vec::new(),
                    reverse_args: Vec::new(),
                    string_fn: None,
                },
            );

            prefix.extend(self.desugar_use_chain(rest));
            prefix.push(wg_decl);
            prefix.push(wait_call);
            return prefix;
        }

        let rest = if rest.is_empty() {
            vec![Node::new(use_node.span, NodeType::Null)]
        } else {
            self.desugar_use_chain(rest)
        };

        let body = Node::new(
            use_node.span,
            NodeType::ScopeDeclaration {
                body: Some(rest),
                named: None,
                is_temp: true,
                create_new_scope: Some(true),
                define: false,
            },
        );

        let params = identifiers
            .iter()
            .map(|id| {
                (
                    id.clone(),
                    PotentialNewType::DataType(ParserDataType::from(ParserInnerType::Auto(None))),
                )
            })
            .collect();

        let header = FunctionHeader {
            generics: calibre_parser::ast::GenericTypes(Vec::new()),
            parameters: params,
            return_type: PotentialNewType::DataType(ParserDataType::from(ParserInnerType::Auto(
                None,
            ))),
            param_destructures: Vec::new(),
        };

        let callback = Node::new(
            use_node.span,
            NodeType::FunctionDeclaration {
                header,
                body: Box::new(body),
            },
        );

        let call_node = match value.node_type {
            NodeType::CallExpression {
                caller,
                mut args,
                reverse_args,
                string_fn,
                generic_types,
            } => {
                args.push(CallArg::Value(callback));
                Node::new(
                    use_node.span,
                    NodeType::CallExpression {
                        caller,
                        args,
                        reverse_args,
                        string_fn,
                        generic_types,
                    },
                )
            }
            other => Node::new(
                use_node.span,
                NodeType::CallExpression {
                    caller: Box::new(Node::new(use_node.span, other)),
                    args: vec![CallArg::Value(callback)],
                    reverse_args: Vec::new(),
                    string_fn: None,
                    generic_types: Vec::new(),
                },
            ),
        };

        prefix.push(call_node);
        prefix
    }

    pub fn evaluate_scope_alias(
        &mut self,
        scope: &u64,
        span: Span,
        identifier: PotentialDollarIdentifier,
        value: NamedScope,
        create_new_scope: Option<bool>,
    ) -> Result<MiddleNode, MiddleErr> {
        let identifer = self
            .resolve_dollar_ident_only(scope, &identifier)
            .unwrap()
            .text;

        let name = self
            .resolve_dollar_ident_only(scope, &value.name)
            .unwrap()
            .text;

        let scope_macro = self.resolve_macro(scope, &name).unwrap().clone();
        let mut args = Vec::new();

        let mut added = Vec::new();

        for arg in value.args {
            let arg_text = self.resolve_dollar_ident_only(scope, &arg.0).unwrap();
            added.push(arg_text.text.clone());
            args.push(arg);
        }

        for arg in scope_macro.args {
            let arg_text = self.resolve_dollar_ident_only(scope, &arg.0).unwrap();
            if !added.contains(&arg_text) {
                added.push(arg_text.text.clone());
                args.push(arg);
            }
        }

        let scope_macro = ScopeMacro {
            name: name.clone(),
            args,
            create_new_scope: create_new_scope.unwrap_or(scope_macro.create_new_scope),
            ..scope_macro
        };

        self.scopes
            .get_mut(scope)
            .unwrap()
            .macros
            .insert(identifer, scope_macro);

        Ok(MiddleNode {
            node_type: MiddleNodeType::EmptyLine,
            span,
        })
    }

    pub fn evaluate_scope_declaration(
        &mut self,
        scope: &u64,
        span: Span,
        mut body: Option<Vec<Node>>,
        named: Option<NamedScope>,
        create_new_scope: Option<bool>,
        define: bool,
        is_temp: bool,
    ) -> Result<MiddleNode, MiddleErr> {
        let mut stmts = Vec::new();
        let mut og_create_new_scope = create_new_scope;
        let mut create_new_scope = create_new_scope.unwrap_or(true);
        let mut macro_args_to_insert: Vec<(String, Node)> = Vec::new();

        if let Some(named) = named {
            if define {
                let name = self
                    .resolve_dollar_ident_only(scope, &named.name)
                    .unwrap()
                    .text;

                let scope_macro = ScopeMacro {
                    name: name.clone(),
                    args: named.args.clone(),
                    body: body.clone().unwrap_or_default(),
                    create_new_scope: og_create_new_scope.unwrap_or(create_new_scope),
                };

                self.scopes
                    .get_mut(scope)
                    .unwrap()
                    .macros
                    .insert(name, scope_macro);

                return Ok(MiddleNode {
                    node_type: MiddleNodeType::EmptyLine,
                    span,
                });
            }

            let name = self
                .resolve_dollar_ident_only(scope, &named.name)
                .unwrap()
                .text;
            if self.resolve_macro(scope, &name).is_none() {
                if !named.args.is_empty() {
                    let scope_macro = ScopeMacro {
                        name: name.clone(),
                        args: named.args.clone(),
                        body: body.clone().unwrap_or_default(),
                        create_new_scope,
                    };
                    self.scopes
                        .get_mut(scope)
                        .unwrap()
                        .macros
                        .insert(name.clone(), scope_macro);
                }

                let mut body_nodes = body.unwrap_or_default();
                let last = body_nodes.pop();
                let break_value = last.map(Box::new);
                body_nodes.push(Node::new(
                    self.current_span(),
                    NodeType::Break {
                        label: Some(named.name.clone()),
                        value: break_value,
                    },
                ));

                let loop_body = Node::new(
                    self.current_span(),
                    NodeType::ScopeDeclaration {
                        body: Some(body_nodes),
                        is_temp: true,
                        create_new_scope: Some(create_new_scope),
                        define: false,
                        named: None,
                    },
                );

                return self.evaluate_loop_statement(
                    scope,
                    span,
                    LoopType::Loop,
                    loop_body,
                    None,
                    Some(named.name),
                    Some(Box::new(Node::new(self.current_span(), NodeType::Null))),
                );
            }
            let mut added = Vec::new();

            let scope_macro_args: Vec<(PotentialDollarIdentifier, Node)> = {
                let scope_macro = self.resolve_macro(scope, &name).unwrap();
                if og_create_new_scope.is_none() {
                    og_create_new_scope = Some(scope_macro.create_new_scope);
                }
                body = Some(scope_macro.body.clone());
                scope_macro.args.clone()
            };

            for arg in named.args {
                let arg_text = self.resolve_dollar_ident_only(scope, &arg.0).unwrap();
                added.push(arg_text.text.clone());
                macro_args_to_insert.push((arg_text.text, arg.1));
            }

            for arg in scope_macro_args {
                let arg_text = self.resolve_dollar_ident_only(scope, &arg.0).unwrap();
                if !added.contains(&arg_text) {
                    added.push(arg_text.text.clone());
                    macro_args_to_insert.push((arg_text.text, arg.1));
                }
            }
        }

        if let Some(og) = og_create_new_scope {
            create_new_scope = og;
        }

        let new_scope = if create_new_scope && !define {
            self.new_scope_from_parent_shallow(*scope)
        } else {
            *scope
        };

        if !macro_args_to_insert.is_empty() {
            let scope_data = self.scopes.get_mut(&new_scope).unwrap();
            for (key, value) in macro_args_to_insert {
                scope_data.macro_args.insert(key, value);
            }
        }

        if let Some(mut body) = body {
            body = self.desugar_use_chain(body);
            for stmt in body.iter() {
                if let NodeType::VariableDeclaration {
                    var_type,
                    identifier,
                    value,
                    ..
                } = &stmt.node_type
                    && matches!(value.node_type, NodeType::FunctionDeclaration { .. })
                {
                    let ident = self
                        .resolve_dollar_ident_only(&new_scope, identifier)
                        .unwrap();
                    let new_name = if ident.text.contains("->") {
                        ident.text.clone()
                    } else {
                        get_disamubiguous_name(&new_scope, Some(ident.text.trim()), Some(var_type))
                    };
                    self.scopes
                        .get_mut(&new_scope)
                        .unwrap()
                        .mappings
                        .entry(ident.text.clone())
                        .or_insert(new_name);
                }
            }

            if is_temp {
                let last = body.pop();
                for statement in body.into_iter() {
                    stmts.push(self.evaluate(&new_scope, statement));
                }

                let mut in_return = Vec::new();

                let last = if let Some(x) = last {
                    let x = self.evaluate(&new_scope, x);
                    in_return = x
                        .identifiers_used()
                        .into_iter()
                        .map(|x| x.to_string())
                        .collect();
                    Some(x)
                } else {
                    None
                };

                for x in self.scopes.get(&new_scope).unwrap().defers.clone() {
                    stmts.push(self.evaluate(&new_scope, x));
                }

                for value in self
                    .scopes
                    .get(&new_scope)
                    .unwrap()
                    .defined
                    .clone()
                    .into_iter()
                    .filter(|x| !in_return.contains(x))
                {
                    stmts.push(MiddleNode::new(
                        MiddleNodeType::Drop(value.into()),
                        self.current_span(),
                    ))
                }

                if let Some(last) = last {
                    stmts.push(last);
                }
            } else {
                let mut errored = Vec::new();

                for statement in body.into_iter() {
                    if let Ok(x) = self.evaluate_inner(&new_scope, statement.clone()) {
                        stmts.push(x);
                    } else {
                        errored.push(statement);
                    }
                }

                let mut last_len = 0;
                while !errored.is_empty() && last_len != errored.len() {
                    last_len = errored.len();

                    for elem in errored.clone() {
                        if let Ok(x) = self.evaluate_inner(&new_scope, elem.clone()) {
                            stmts.push(x);
                            errored.retain(|x| x != &elem);
                        }
                    }
                }

                for val in errored {
                    let _ = self.evaluate(&new_scope, val);
                }
            }
        }

        if &new_scope != scope && !og_create_new_scope.unwrap() {
            let (mappings, macros) = {
                let scope = self.scopes.get(&new_scope).unwrap();
                (scope.mappings.clone(), scope.macros.clone())
            };

            for mapping in mappings {
                self.scopes
                    .get_mut(scope)
                    .unwrap()
                    .mappings
                    .insert(mapping.0, mapping.1);
            }

            for scope_macro in macros {
                self.scopes
                    .get_mut(scope)
                    .unwrap()
                    .macros
                    .insert(scope_macro.0, scope_macro.1);
            }
        }

        Ok(MiddleNode {
            node_type: MiddleNodeType::ScopeDeclaration {
                body: {
                    let mut body: Vec<MiddleNode> = stmts
                        .into_iter()
                        .filter(|x| x.node_type != MiddleNodeType::EmptyLine)
                        .collect();
                    let defined = self
                        .scopes
                        .get(&new_scope)
                        .map(|s| s.defined.clone())
                        .unwrap_or_default();
                    let defers_empty = self
                        .scopes
                        .get(&new_scope)
                        .map(|s| s.defers.is_empty())
                        .unwrap_or(true);
                    if defers_empty {
                        let protected_tail = body
                            .last()
                            .map(|n| {
                                n.identifiers_used()
                                    .into_iter()
                                    .map(|x| x.to_string())
                                    .collect::<Vec<String>>()
                            })
                            .unwrap_or_default();
                        self.insert_auto_drops(&mut body, &defined, &protected_tail);
                    }
                    if is_temp && body.len() > 1 {
                        let last_used = body
                            .last()
                            .map(|n| {
                                n.identifiers_used()
                                    .into_iter()
                                    .map(|x| x.to_string())
                                    .collect::<rustc_hash::FxHashSet<String>>()
                            })
                            .unwrap_or_default();
                        let mut trailing = Vec::new();
                        while let Some(MiddleNodeType::Drop(name)) =
                            body.last().map(|n| &n.node_type)
                        {
                            if last_used.contains(&name.text) {
                                break;
                            }
                            trailing.push(body.pop().unwrap());
                        }
                        if !trailing.is_empty() {
                            let insert_at = body.len().saturating_sub(1);
                            for drop_node in trailing.into_iter().rev() {
                                body.insert(insert_at, drop_node);
                            }
                        }
                    }
                    body
                },
                is_temp,
                create_new_scope: og_create_new_scope.unwrap(),
                scope_id: new_scope,
            },
            span,
        })
    }
}
