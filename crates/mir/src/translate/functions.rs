use calibre_parser::{
    ast::{
        CallArg, FunctionHeader, GenericTypes, Node, NodeType, ParserDataType, ParserInnerType,
        ParserText, PotentialDollarIdentifier, PotentialFfiDataType,
        PotentialGenericTypeIdentifier, PotentialNewType, VarType,
    },
    lexer::Span,
};

use crate::{
    ast::{MiddleNode, MiddleNodeType, hm},
    environment::{MiddleEnvironment, MiddleVariable, get_disamubiguous_name},
    errors::MiddleErr,
    infer::infer_node_hm,
};

impl MiddleEnvironment {
    pub fn evaluate_extern_function(
        &mut self,
        scope: &u64,
        span: Span,
        abi: String,
        identifier: PotentialDollarIdentifier,
        parameters: Vec<PotentialFfiDataType>,
        return_type: PotentialFfiDataType,
        library: String,
        symbol: Option<String>,
    ) -> Result<MiddleNode, MiddleErr> {
        let ident = self.resolve_dollar_ident_only(scope, &identifier).unwrap();
        let new_name = get_disamubiguous_name(scope, Some(ident.trim()), Some(&VarType::Constant));

        let mut params = Vec::new();
        for ty in parameters {
            let resolved = self.resolve_potential_ffi_type(scope, ty);
            params.push(resolved);
        }

        let return_type = self.resolve_potential_ffi_type(scope, return_type);

        let fn_type = ParserDataType::from(ParserInnerType::Function {
            return_type: Box::new(match return_type.clone() {
                PotentialFfiDataType::Normal(x) => x,
                PotentialFfiDataType::Ffi(x) => ParserDataType::from(x),
            }),
            parameters: params
                .clone()
                .into_iter()
                .map(|x| match x {
                    PotentialFfiDataType::Normal(x) => x,
                    PotentialFfiDataType::Ffi(x) => ParserDataType::from(x),
                })
                .collect(),
            is_async: false,
        });

        self.variables.insert(
            new_name.clone(),
            MiddleVariable {
                data_type: fn_type.clone(),
                var_type: VarType::Constant,
                location: self.current_location.clone(),
            },
        );

        self.scopes
            .get_mut(scope)
            .unwrap()
            .mappings
            .insert(ident.text.clone(), new_name.clone());

        Ok(MiddleNode {
            node_type: MiddleNodeType::VariableDeclaration {
                var_type: VarType::Constant,
                identifier: ParserText::from(new_name),
                value: Box::new(MiddleNode::new(
                    MiddleNodeType::ExternFunction {
                        abi,
                        library,
                        symbol: symbol.unwrap_or_else(|| ident.text.clone()),
                        parameters: params,
                        return_type,
                    },
                    self.current_span(),
                )),
                data_type: fn_type,
            },
            span,
        })
    }

    pub fn evaluate_function_declaration(
        &mut self,
        scope: &u64,
        span: Span,
        header: FunctionHeader,
        body: Node,
    ) -> Result<MiddleNode, MiddleErr> {
        let mut params = Vec::with_capacity(header.parameters.len());
        let mut old_func_defers = Vec::new();
        old_func_defers.append(&mut self.func_defers);
        let new_scope = self.new_scope_from_parent_shallow(*scope);
        for param in header.parameters {
            let og_name = self.resolve_dollar_ident_only(scope, &param.0).unwrap();
            let new_name =
                get_disamubiguous_name(scope, Some(og_name.trim()), Some(&VarType::Mutable));
            let data_type = self.resolve_potential_new_type(scope, param.1);
            self.variables.insert(
                new_name.clone(),
                MiddleVariable {
                    data_type: data_type.clone(),
                    var_type: VarType::Mutable,
                    location: self.current_location.clone(),
                },
            );

            self.scopes
                .get_mut(&new_scope)
                .unwrap()
                .mappings
                .insert(og_name.text.clone(), new_name.clone());

            self.scopes
                .get_mut(&new_scope)
                .unwrap()
                .defined
                .push(new_name.clone());
            params.push((ParserText::from(new_name), data_type));
        }

        let return_type = self.resolve_potential_new_type(&new_scope, header.return_type);

        let mut body_node = body;

        if !header.param_destructures.is_empty() {
            let mut destructures = Vec::new();
            for (tmp_name, pattern) in header.param_destructures {
                destructures
                    .extend(self.emit_destructure_statements(&tmp_name, &pattern, span, true));
            }
            body_node = match body_node.node_type {
                NodeType::ScopeDeclaration {
                    body: Some(mut inner),
                    named,
                    is_temp,
                    create_new_scope,
                    define,
                } => {
                    let mut new_body = destructures;
                    new_body.append(&mut inner);
                    Node::new(
                        body_node.span,
                        NodeType::ScopeDeclaration {
                            body: Some(new_body),
                            named,
                            is_temp,
                            create_new_scope,
                            define,
                        },
                    )
                }
                _ => Node::new(
                    body_node.span,
                    NodeType::ScopeDeclaration {
                        body: Some({
                            let mut new_body = destructures;
                            new_body.push(body_node);
                            new_body
                        }),
                        named: None,
                        is_temp: true,
                        create_new_scope: Some(false),
                        define: false,
                    },
                ),
            };
        }

        let body = self.evaluate(&new_scope, body_node);
        let mut func_defers = Vec::new();
        func_defers.append(&mut self.func_defers);

        let body = if let MiddleNodeType::ScopeDeclaration {
            body: mut scope_body,
            create_new_scope,
            is_temp: _,
            scope_id,
        } = body.node_type
        {
            let mut last = scope_body.pop();
            for defer in func_defers {
                scope_body.push(self.evaluate(scope, defer));
            }

            if return_type.data_type != ParserInnerType::Null {
                if let Some(last_node) = last.take() {
                    if matches!(last_node.node_type, MiddleNodeType::Return { .. }) {
                        last = Some(last_node);
                    } else {
                        let simple_return = matches!(
                            last_node.node_type,
                            MiddleNodeType::Identifier(_)
                                | MiddleNodeType::IntLiteral(_)
                                | MiddleNodeType::FloatLiteral(_)
                                | MiddleNodeType::StringLiteral(_)
                                | MiddleNodeType::CharLiteral(_)
                                | MiddleNodeType::Null
                                | MiddleNodeType::MemberExpression { .. }
                        );
                        if simple_return {
                            last = Some(MiddleNode::new(
                                MiddleNodeType::Return {
                                    value: Some(Box::new(last_node)),
                                },
                                self.current_span(),
                            ));
                        } else {
                            let last_node = match last_node.node_type {
                                MiddleNodeType::IfStatement {
                                    comparison,
                                    then,
                                    otherwise,
                                } => {
                                    let wrap = |node: Box<MiddleNode>| {
                                        if matches!(node.node_type, MiddleNodeType::Return { .. }) {
                                            node
                                        } else {
                                            Box::new(MiddleNode::new(
                                                MiddleNodeType::Return { value: Some(node) },
                                                self.current_span(),
                                            ))
                                        }
                                    };
                                    let then = wrap(then);
                                    let otherwise = match otherwise {
                                        Some(other) => Some(wrap(other)),
                                        None => Some(Box::new(MiddleNode::new(
                                            MiddleNodeType::Return { value: None },
                                            self.current_span(),
                                        ))),
                                    };
                                    MiddleNode {
                                        span: last_node.span,
                                        node_type: MiddleNodeType::IfStatement {
                                            comparison,
                                            then,
                                            otherwise,
                                        },
                                    }
                                }
                                _ => last_node,
                            };
                            last = Some(last_node);
                        }
                    }
                }
            }

            if let Some(last) = last {
                scope_body.push(last);
            }

            MiddleNode {
                span: body.span,
                node_type: MiddleNodeType::ScopeDeclaration {
                    body: scope_body,
                    create_new_scope,
                    is_temp: false,
                    scope_id,
                },
            }
        } else {
            body
        };
        self.func_defers.append(&mut old_func_defers);

        let mut fn_node = MiddleNode {
            node_type: MiddleNodeType::FunctionDeclaration {
                parameters: params.clone(),
                body: Box::new(body.clone()),
                return_type: return_type.clone(),
                is_async: header.is_async,
                scope_id: new_scope,
            },
            span,
        };

        let ast_node: Node = fn_node.clone().into();
        for (p_name, _p_ty) in params.iter() {
            let full = p_name.text.clone();
            if let Some(idx) = full.rfind(':') {
                let short = full[idx + 1..].to_string();
                self.scopes
                    .get_mut(&new_scope)
                    .unwrap()
                    .mappings
                    .insert(short, full.clone());
            }
        }

        if let Some((hm_t, subst)) = infer_node_hm(self, &new_scope, &ast_node) {
            let t_applied = hm::apply_subst(&subst, &hm_t);
            let parser_ty = hm::to_parser_data_type(&t_applied);

            if let MiddleNodeType::FunctionDeclaration {
                parameters: ref mut params2,
                return_type: ref mut ret2,
                ..
            } = fn_node.node_type
            {
                if let calibre_parser::ast::ParserInnerType::Function {
                    return_type: inferred_ret,
                    parameters: inferred_params,
                    is_async: _,
                } = parser_ty.data_type
                {
                    for (i, (name, p_ty)) in params2.iter_mut().enumerate() {
                        if i < inferred_params.len() {
                            *p_ty = inferred_params[i].clone();
                            if let Some(var) = self.variables.get_mut(&name.text) {
                                var.data_type = inferred_params[i].clone();
                            }
                        }
                    }

                    *ret2 = *inferred_ret.clone();
                }
            }
        }

        Ok(fn_node)
    }

    pub fn evaluate_call_expression(
        &mut self,
        scope: &u64,
        span: Span,
        caller: Node,
        generic_types: Vec<PotentialNewType>,
        mut args: Vec<CallArg>,
        mut reverse_args: Vec<Node>,
    ) -> Result<MiddleNode, MiddleErr> {
        if let NodeType::Identifier(caller_ident) = &caller.node_type {
            if let Some(resolved_caller) = self.resolve_potential_generic_ident(scope, caller_ident)
            {
                let base_name = resolved_caller.text.clone();

                if let Some((tpl_params, header, _body)) =
                    self.generic_fn_templates.get(&base_name).cloned()
                {
                    let explicit_args: Vec<ParserDataType> = generic_types
                        .iter()
                        .map(|g| self.resolve_potential_new_type(scope, g.clone()))
                        .collect();

                    let concrete_args: Option<Vec<ParserDataType>> = if !explicit_args.is_empty() {
                        Some(explicit_args)
                    } else {
                        let mut all_args: Vec<Node> =
                            args.iter().cloned().map(|a| a.into()).collect();
                        all_args.append(&mut reverse_args.clone());
                        let arg_types: Vec<ParserDataType> = all_args
                            .iter()
                            .filter_map(|a| self.resolve_type_from_node(scope, a))
                            .collect();

                        let param_types: Vec<ParserDataType> = header
                            .parameters
                            .iter()
                            .filter_map(|(_n, p)| match p {
                                PotentialNewType::DataType(dt) => Some(dt.clone()),
                                _ => None,
                            })
                            .collect();

                        if param_types.len() == arg_types.len() {
                            self.infer_generic_args_from_call(&tpl_params, &param_types, &arg_types)
                        } else {
                            None
                        }
                    };

                    if let Some(concrete_args) = concrete_args {
                        if let Some(spec) = self.ensure_specialized_function(
                            scope,
                            &base_name,
                            &tpl_params,
                            &concrete_args,
                        ) {
                            return self.evaluate_inner(
                                scope,
                                Node::new(
                                    self.current_span(),
                                    NodeType::CallExpression {
                                        string_fn: None,
                                        caller: Box::new(Node::new(
                                            self.current_span(),
                                            NodeType::Identifier(ParserText::from(spec).into()),
                                        )),
                                        generic_types: Vec::new(),
                                        args,
                                        reverse_args,
                                    },
                                ),
                            );
                        }
                    }
                }
            }
        }

        if let NodeType::Identifier(caller) = &caller.node_type {
            if "tuple" == &caller.to_string() {
                let mut args: Vec<Node> = args.into_iter().map(|x| x.into()).collect();
                args.append(&mut reverse_args);

                let mut map = Vec::new();

                for (i, arg) in args.into_iter().enumerate() {
                    map.push((i.to_string(), self.evaluate(scope, arg)));
                }

                return Ok(MiddleNode {
                    node_type: MiddleNodeType::AggregateExpression {
                        identifier: None,
                        value: map.into(),
                    },
                    span,
                });
            }

            if let Some(caller) = self
                .resolve_potential_generic_ident(scope, &caller)
                .map(|x| x.clone())
            {
                if self.objects.contains_key(&caller.text) {
                    let mut map = Vec::new();
                    let mut args: Vec<Node> = args.into_iter().map(|x| x.into()).collect();
                    args.append(&mut reverse_args);

                    for (i, arg) in args.into_iter().enumerate() {
                        map.push((i.to_string(), self.evaluate(scope, arg)));
                    }

                    return Ok(MiddleNode {
                        node_type: MiddleNodeType::AggregateExpression {
                            identifier: Some(ParserText::from(caller)),
                            value: map.into(),
                        },
                        span,
                    });
                }
            }
        }

        let data_type = self
            .resolve_type_from_node(scope, &caller)
            .map(|x| x.unwrap_all_refs().data_type);

        Ok(MiddleNode {
            node_type: MiddleNodeType::CallExpression {
                args: match data_type {
                    Some(ParserInnerType::Function {
                        return_type,
                        parameters,
                        is_async,
                    }) if (parameters.len() < args.len() + reverse_args.len()
                        || parameters.len() == args.len() + reverse_args.len() + 1)
                        && parameters
                            .get(parameters.len() - reverse_args.len() - 1)
                            .unwrap()
                            .is_list() =>
                    {
                        let mut lst = Vec::new();
                        for _ in 0..(parameters.len() - 1 - reverse_args.len()) {
                            let arg = args.remove(0);
                            lst.push(self.evaluate(scope, arg.into()));
                        }

                        lst.push(
                            self.evaluate(
                                scope,
                                Node::new(
                                    self.current_span(),
                                    NodeType::ListLiteral(
                                        match parameters
                                            .last()
                                            .unwrap()
                                            .clone()
                                            .unwrap_all_refs()
                                            .data_type
                                        {
                                            ParserInnerType::List(x) => (*x).into(),
                                            _ => PotentialNewType::DataType(ParserDataType::from(
                                                ParserInnerType::Auto(None),
                                            )),
                                        },
                                        args.into_iter().map(|x| x.into()).collect(),
                                    ),
                                ),
                            ),
                        );

                        for _ in 0..reverse_args.len() {
                            lst.push(self.evaluate(scope, reverse_args.remove(0)));
                        }

                        lst
                    }
                    Some(ParserInnerType::Function {
                        return_type,
                        parameters,
                        is_async,
                    }) if parameters.len() > args.len() + reverse_args.len() => {
                        let provided_len = args.len();
                        let mut capture_decls = Vec::new();
                        let mut captured_args: Vec<CallArg> = Vec::new();

                        for (i, arg) in args.into_iter().enumerate() {
                            match arg {
                                CallArg::Value(node) => {
                                    let name = format!("__curry_capture_{i}");
                                    let ident: PotentialDollarIdentifier =
                                        ParserText::from(name.clone()).into();
                                    capture_decls.push(Node::new(
                                        self.current_span(),
                                        NodeType::VariableDeclaration {
                                            var_type: VarType::Immutable,
                                            identifier: ident.clone(),
                                            data_type: PotentialNewType::DataType(
                                                ParserDataType::from(ParserInnerType::Auto(None)),
                                            ),
                                            value: Box::new(node),
                                        },
                                    ));
                                    captured_args.push(CallArg::Value(Node::new(
                                        self.current_span(),
                                        NodeType::Identifier(
                                            PotentialGenericTypeIdentifier::Identifier(
                                                ident.into(),
                                            ),
                                        ),
                                    )));
                                }
                                other => captured_args.push(other),
                            }
                        }

                        let mut converted_missing_param_types: Vec<PotentialNewType> = parameters
                            .iter()
                            .enumerate()
                            .filter(|(i, _)| i >= &(provided_len + reverse_args.len()))
                            .map(|x| x.1.clone().into())
                            .collect();

                        let mut missing_param_names: Vec<String> = (0
                            ..converted_missing_param_types.len())
                            .map(|i| format!("$-curry-{i}"))
                            .collect();

                        for name in missing_param_names.clone() {
                            captured_args.push(CallArg::Value(Node::new(
                                self.current_span(),
                                NodeType::Identifier(ParserText::from(name).into()),
                            )));
                        }

                        for _ in 0..reverse_args.len() {
                            captured_args.push(CallArg::Value(reverse_args.remove(0)));
                        }

                        let func_decl = Node {
                            node_type: NodeType::FunctionDeclaration {
                                header: FunctionHeader {
                                    generics: GenericTypes::default(),
                                    parameters: (0..converted_missing_param_types.len())
                                        .map(|_| {
                                            (
                                                ParserText::from(missing_param_names.remove(0))
                                                    .into(),
                                                converted_missing_param_types.remove(0),
                                            )
                                        })
                                        .collect(),
                                    return_type: (*return_type).into(),
                                    is_async,
                                    param_destructures: Vec::new(),
                                },
                                body: Box::new(Node::new(
                                    self.current_span(),
                                    NodeType::CallExpression {
                                        string_fn: None,
                                        caller: Box::new(caller),
                                        generic_types,
                                        args: captured_args,
                                        reverse_args: Vec::new(),
                                    },
                                )),
                            },
                            span,
                        };

                        let tmp_name = format!(
                            "__curry_fn_{}_{}",
                            self.current_span().from.line,
                            self.current_span().from.col
                        );
                        let tmp_ident: PotentialDollarIdentifier =
                            ParserText::from(tmp_name.clone()).into();
                        let tmp_ident_node = Node::new(
                            self.current_span(),
                            NodeType::Identifier(PotentialGenericTypeIdentifier::Identifier(
                                tmp_ident.clone().into(),
                            )),
                        );

                        let mut body = capture_decls;
                        body.push(Node::new(
                            self.current_span(),
                            NodeType::VariableDeclaration {
                                var_type: VarType::Immutable,
                                identifier: tmp_ident,
                                data_type: PotentialNewType::DataType(ParserDataType::from(
                                    ParserInnerType::Auto(None),
                                )),
                                value: Box::new(func_decl),
                            },
                        ));
                        body.push(tmp_ident_node);

                        return self.evaluate_inner(
                            scope,
                            Node::new(
                                self.current_span(),
                                NodeType::ScopeDeclaration {
                                    body: Some(body),
                                    named: None,
                                    is_temp: true,
                                    create_new_scope: Some(true),
                                    define: false,
                                },
                            ),
                        );
                    }
                    _ => {
                        let mut lst = Vec::new();

                        for arg in args {
                            lst.push(self.evaluate(scope, arg.into()));
                        }

                        for _ in 0..reverse_args.len() {
                            lst.push(self.evaluate(scope, reverse_args.remove(0)));
                        }

                        lst
                    }
                },
                caller: Box::new(self.evaluate(scope, caller)),
            },
            span,
        })
    }
}
