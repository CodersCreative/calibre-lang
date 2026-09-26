use crate::{
    ast::{
        MiddleNode, MiddleNodeType, MirAggregate, MirCall, MirConditional, MirExtern, MirFunction,
        MirReturn, MirScopeDecl, MirVarDecl,
    },
    environment::MiddleEnvironment,
    errors::MiddleErr,
    scoping::ScopeId,
    symbols::{FunctionParamDefault, resolve::ResolutionOptions},
    tags::TagInfo,
    translate::MirLowering,
};
use calibre_parser::{
    Span,
    ast::{
        ObjectType,
        comparison::ComparisonOperator,
        idents::{ParserText, PotentialDollarIdentifier, PotentialGenericTypeIdentifier},
        nodes::{
            AstNode, AstNodeType, VarType,
            access::AstField,
            binary::AstComparison,
            conditionals::{AstTernary, TernaryType},
            declaration::AstDeclaration,
            flow::AstReturn,
            functions::{AstCall, AstExtern, AstFunction, CallArg, FunctionHeader},
            lists::AstList,
            literals::{AstString, AstStruct},
            scopes::AstScopeDef,
        },
        traversal::NodeVisitor,
        types::{GenericTypes, ParserDataType, ParserInnerType},
    },
};
use tracing::instrument;
use ustr::Ustr;

struct GeneratorReturnsRewriter;

impl NodeVisitor for GeneratorReturnsRewriter {
    fn visit(&mut self, node: AstNode) -> AstNode {
        let span = node.span;
        match node.node_type {
            AstNodeType::Return(AstReturn { value: Some(value) }) => AstNode::call(
                span,
                AstNode::identifier(span, "gen_suspend"),
                vec![CallArg::Value(*value)],
            ),
            AstNodeType::Return(AstReturn { value: None }) => AstNode::new(
                span,
                AstNodeType::Return(AstReturn {
                    value: Some(Box::new(AstNode::identifier(span, "none"))),
                }),
            ),
            _ => {
                let node_type = self.visit_children(node.node_type);
                AstNode::new(span, node_type)
            }
        }
    }
}

impl MiddleEnvironment {
    #[inline]
    fn unwrap_option_or_default_expr(span: Span, value: AstNode, default: AstNode) -> AstNode {
        AstNode::new(
            span,
            AstNodeType::Ternary(AstTernary {
                comparison: Box::new(AstNode::new(
                    span,
                    AstNodeType::ComparisonExpression(AstComparison {
                        left: Box::new(value.clone()),
                        right: Box::new(AstNode::none(span)),
                        operator: ComparisonOperator::Equal,
                    }),
                )),
                then: Box::new(default),
                otherwise: Some(Box::new(AstNode::new(
                    span,
                    AstNodeType::FieldAccess(AstField {
                        base: Box::new(value),
                        field: PotentialDollarIdentifier::new(span, "next"),
                    }),
                ))),
                ternary_type: TernaryType::Normal,
            }),
        )
    }

    #[inline]
    fn should_combine_excess_args_into_list_param(
        parameters: &[ParserDataType],
        positional_count: usize,
        reverse_arg_count: usize,
    ) -> bool {
        let total_args = positional_count + reverse_arg_count;
        let list_idx = parameters.len().saturating_sub(reverse_arg_count + 1);
        let has_list_param = parameters
            .get(list_idx)
            .map(|p| p.is_list())
            .unwrap_or(false);
        has_list_param
            && (parameters.len() < total_args
                || parameters.len() == total_args + 1
                || parameters.len() == total_args)
    }

    pub(crate) fn lower_args(
        &mut self,
        scope: ScopeId,
        span: Span,
        caller: &MiddleNode,
        data_type: &Option<ParserInnerType>,
        mut args: Vec<CallArg>,
        reverse_args: Vec<AstNode>,
    ) -> Option<Box<[MiddleNode]>> {
        let defaults_id = match &caller.node_type {
            MiddleNodeType::Identifier(x) => self.symbols.name_to_param_defaults.get(&x.identifier),
            MiddleNodeType::FunctionDeclaration(x) => x.default_args_id.as_ref(),
            _ => None,
        };

        let defaults = defaults_id
            .and_then(|id| self.symbols.function_param_defaults.get(id).cloned())
            .unwrap_or_default();

        let parameters = match data_type {
            Some(ParserInnerType::Function { parameters, .. }) => Some(parameters.as_slice()),
            _ => None,
        };

        if let Some(params) = parameters
            && Self::should_combine_excess_args_into_list_param(
                params,
                args.iter()
                    .filter(|arg| matches!(arg, CallArg::Value(_)))
                    .count(),
                reverse_args.len(),
            )
        {
            let normal_count = params
                .len()
                .saturating_sub(1 + reverse_args.len())
                .min(args.len());
            let mut lst = Vec::with_capacity(params.len());

            for arg in args.drain(..normal_count) {
                let node: AstNode = arg.into();
                lst.push(node.lower_or_empty(self, scope, span));
            }

            let list_inner_type = params
                .last()
                .map(|p| p.clone().unwrap_all_refs().data_type)
                .and_then(|dt| match dt {
                    ParserInnerType::List(x) => Some(*x),
                    _ => None,
                })
                .unwrap_or_else(|| ParserDataType::auto(span));

            let list_arg = if args.len() == 1 {
                let arg: AstNode = args.pop().unwrap().into();
                let is_already_list = matches!(arg.node_type, AstNodeType::ListLiteral(_))
                    || arg
                        .type_of(self, scope, span)
                        .is_some_and(|dt| dt.is_list());

                if is_already_list {
                    arg
                } else {
                    AstNode::new(
                        span,
                        AstNodeType::ListLiteral(AstList {
                            data_type: list_inner_type,
                            values: vec![arg],
                        }),
                    )
                }
            } else {
                AstNode::new(
                    span,
                    AstNodeType::ListLiteral(AstList {
                        data_type: list_inner_type,
                        values: args.into_iter().map(Into::into).collect(),
                    }),
                )
            };

            lst.push(list_arg.lower_or_empty(self, scope, span));

            for arg in reverse_args {
                lst.push(arg.lower_or_empty(self, scope, span));
            }

            return Some(lst.into_boxed_slice());
        }

        let param_len = parameters.map_or(defaults.len(), |p| p.len());
        if defaults.is_empty() || param_len == 0 {
            return None;
        }

        let mut slots: Vec<Option<AstNode>> = vec![None; param_len];
        let reverse_len = reverse_args.len().min(param_len);

        for (i, node) in reverse_args.into_iter().enumerate().take(reverse_len) {
            slots[param_len - reverse_len + i] = Some(node);
        }

        let mut next_pos = 0usize;
        for arg in args {
            match arg {
                CallArg::Named(name, value) => {
                    if let Some(idx) = defaults.iter().position(|d| d.name == name.to_string()) {
                        slots[idx] = Some(value);
                    }
                }
                CallArg::Value(value) => {
                    while next_pos < param_len && slots[next_pos].is_some() {
                        next_pos += 1;
                    }
                    if next_pos < param_len {
                        slots[next_pos] = Some(value);
                        next_pos += 1;
                    }
                }
            }
        }

        let is_option_type = |env: &mut MiddleEnvironment, node: &AstNode| {
            matches!(
                env.resolve_type_from_node(scope, node)
                    .as_ref()
                    .map(|x| x.data_type.unwrap_all_refs()),
                Some(ParserInnerType::Option(_))
            )
        };

        slots
            .into_iter()
            .enumerate()
            .map(|(i, slot)| {
                let meta = defaults.get(i)?;
                let mut wrap_some = false;

                let node = match slot {
                    None => {
                        if let Some(default) = &meta.explicit_default {
                            default.clone().into()
                        } else if meta.implicit_none {
                            AstNode::none(span)
                        } else {
                            return None;
                        }
                    }
                    Some(current) => {
                        if current.is_none() {
                            meta.explicit_default.as_ref()?.clone().into()
                        } else if meta.implicit_none {
                            if !current.is_raw_option_value() && !is_option_type(self, &current) {
                                wrap_some = true;
                            }
                            current
                        } else if let Some(default) = &meta.explicit_default {
                            if is_option_type(self, &current) {
                                Self::unwrap_option_or_default_expr(
                                    span,
                                    current,
                                    default.clone().into(),
                                )
                            } else {
                                current
                            }
                        } else {
                            current
                        }
                    }
                };

                if wrap_some {
                    Some(
                        AstNode::call(
                            span,
                            AstNode::identifier(span, "some"),
                            vec![CallArg::Value(node)],
                        )
                        .lower_or_empty(self, scope, span),
                    )
                } else {
                    Some(node.lower_or_empty(self, scope, span))
                }
            })
            .collect::<Option<Box<[_]>>>()
    }

    #[inline]
    fn collect_call_nodes(args: Vec<CallArg>, mut reverse_args: Vec<AstNode>) -> Vec<AstNode> {
        let mut nodes: Vec<AstNode> = args.into_iter().map(Into::into).collect();
        nodes.append(&mut reverse_args);
        nodes
    }

    #[inline]
    fn aggregate_from_call_nodes(
        &mut self,
        scope: ScopeId,
        span: Span,
        identifier: Option<Ustr>,
        args: Vec<CallArg>,
        reverse_args: Vec<AstNode>,
    ) -> MiddleNode {
        let value = Self::collect_call_nodes(args, reverse_args)
            .into_iter()
            .enumerate()
            .map(|(i, arg)| (i.to_string(), arg.lower_or_empty(self, scope, span)))
            .collect::<Vec<_>>()
            .into();

        MiddleNode {
            node_type: MiddleNodeType::AggregateExpression(MirAggregate { identifier, value }),
            span,
        }
    }

    fn rewrite_generator_returns(node: AstNode) -> AstNode {
        let mut rewriter = GeneratorReturnsRewriter;
        rewriter.visit(node)
    }

    pub(crate) fn wrap_generator_body(
        body: AstNode,
        elem_type: ParserDataType,
        span: Span,
    ) -> AstNode {
        let next_name = ParserText::temp_name_with_suffix("gen_next", span);
        let rewritten = Self::rewrite_generator_returns(body);

        let next_body = match rewritten.node_type {
            AstNodeType::ScopeDeclaration(AstScopeDef {
                body: Some(mut items),
                ..
            }) => {
                items.push(AstNode::identifier(span, "none"));
                AstNode::new_temp_scope(items)
            }
            other => AstNode::new_temp_scope(vec![
                AstNode::new(span, other),
                AstNode::identifier(span, "none"),
            ]),
        };

        let next_decl = AstNode::new(
            span,
            AstNodeType::VariableDeclaration(AstDeclaration {
                var_type: VarType::Immutable,
                identifier: PotentialDollarIdentifier::Identifier(ParserText::new(
                    span,
                    next_name.clone(),
                )),
                data_type: ParserDataType::function(
                    span,
                    vec![],
                    ParserDataType::new(span, ParserInnerType::Option(Box::new(elem_type.clone()))),
                ),
                value: Box::new(AstNode::new(
                    span,
                    AstNodeType::FunctionDeclaration(AstFunction {
                        header: FunctionHeader {
                            generics: GenericTypes::default(),
                            parameters: vec![],
                            return_type: ParserDataType::new(
                                span,
                                ParserInnerType::Option(Box::new(elem_type.clone())),
                            ),
                            param_destructures: Vec::new(),
                        },
                        body: Box::new(next_body),
                    }),
                )),
            }),
        );

        let gen_value = AstNode::new(
            span,
            AstNodeType::StructLiteral(AstStruct {
                identifier: PotentialGenericTypeIdentifier::Generic {
                    identifier: PotentialDollarIdentifier::Identifier(ParserText::new(
                        span,
                        String::from("gen"),
                    )),
                    generic_types: vec![elem_type],
                },
                value: ObjectType::Map(vec![
                    (Ustr::from("data"), AstNode::identifier(span, &next_name)),
                    (Ustr::from("index"), AstNode::int(span, 0)),
                    (Ustr::from("done"), AstNode::identifier(span, "false")),
                ]),
            }),
        );

        AstNode::new_temp_scope_with_create(vec![next_decl, gen_value], Some(false))
    }

    pub fn get_caller_context(&self, scope: ScopeId, span: Span) -> Option<AstNode> {
        let scope_ref = self.scoping.scope_or_err(scope).ok()?;

        let value = |v: Ustr| {
            AstNode::new(
                span,
                AstNodeType::StringLiteral(AstString {
                    value: ParserText::new(span, v),
                }),
            )
        };

        let current_function_name = if scope_ref.namespace.parse::<u64>().is_ok() {
            Ustr::from("main")
        } else {
            scope_ref.namespace
        };

        let module_name = if !scope_ref.path.as_os_str().is_empty() {
            scope_ref.namespace
        } else {
            Ustr::from("unknown")
        };

        Some(AstNode::new(
            span,
            AstNodeType::StructLiteral(AstStruct {
                identifier: PotentialGenericTypeIdentifier::new(span, "ExecContext"),
                value: ObjectType::Map(vec![
                    (Ustr::from("function_name"), value(current_function_name)),
                    (Ustr::from("module_name"), value(module_name)),
                    (
                        Ustr::from("path"),
                        value(Ustr::from(
                            &scope_ref
                                .path
                                .canonicalize()
                                .unwrap_or_default()
                                .to_string_lossy(),
                        )),
                    ),
                    (Ustr::from("span"), AstNode::range(span, span.to_range())),
                ]),
            }),
        ))
    }
}

impl MirLowering for FunctionHeader {
    fn lower(
        self,
        _env: &mut MiddleEnvironment,
        _scope: ScopeId,
        _span: Span,
    ) -> Result<MiddleNode, MiddleErr> {
        unreachable!()
    }

    fn type_of(
        &self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        span: Span,
    ) -> Option<ParserDataType> {
        let generic_params: Vec<Ustr> = self
            .generics
            .0
            .iter()
            .map(|g| {
                env.resolve(
                    scope,
                    &g.identifier,
                    ResolutionOptions::default().with_dollar(),
                )
            })
            .collect::<Result<Vec<Ustr>, MiddleErr>>()
            .ok()?;

        if !generic_params.is_empty() {
            env.scoping.push_generic_params(generic_params.clone());
        }

        let return_type = env
            .resolve_data_type(scope, &self.return_type, ResolutionOptions::typing())
            .ok()?;

        Some(ParserDataType {
            data_type: ParserInnerType::Function {
                return_type: Box::new(return_type),
                parameters: {
                    let mut params = Vec::with_capacity(self.parameters.len());

                    for param in &self.parameters {
                        let data_type = if let Some(x) = &param.1 {
                            env.resolve_data_type(scope, x, ResolutionOptions::typing())
                                .ok()?
                        } else if let Some(node) = &param.2 {
                            node.type_of(env, scope, span)?
                        } else {
                            return None;
                        };
                        params.push(data_type);
                    }

                    env.scoping.pop_generic_params();

                    params
                },
            },
            span,
        })
    }
}

impl MirLowering for AstExtern {
    #[instrument(skip_all)]
    fn lower(
        self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        span: Span,
    ) -> Result<MiddleNode, MiddleErr> {
        let ident = env.resolve(
            scope,
            &self.identifier,
            ResolutionOptions::default().with_dollar(),
        )?;

        let new_name = Ustr::from(&ParserText::temp_name_with_suffix(ident.trim(), span).text);

        let params = self
            .parameters
            .iter()
            .map(|ty| {
                let ty = ty.clone().resolve_ffi();

                env.resolve_data_type(scope, &ty, ResolutionOptions::typing())
                    .unwrap_or(ty)
            })
            .collect::<Vec<_>>();

        let return_type = env.resolve_data_type(
            scope,
            &self.return_type.resolve_ffi(),
            ResolutionOptions::typing(),
        )?;

        let fn_type = ParserDataType::function(span, params.clone(), return_type.clone());

        let mut pure = false;
        let mut memo = false;
        let mut memo_params = Vec::new();

        for tag in env.tagging.tag_info.iter() {
            if let TagInfo::Pure(x) = tag {
                pure = true;
                memo = x.memo;
                memo_params.append(&mut x.params.clone());
            }
        }

        env.register_variable(scope, ident, new_name, fn_type.clone(), VarType::Constant)?;

        Ok(MiddleNode {
            node_type: MiddleNodeType::VariableDeclaration(MirVarDecl {
                var_type: VarType::Constant,
                identifier: new_name,
                value: Box::new(MiddleNode::new(
                    MiddleNodeType::ExternFunction(MirExtern {
                        abi: Ustr::from(&self.abi),
                        library: Ustr::from(&self.library),
                        symbol: self.symbol.map(|x| Ustr::from(&x)).unwrap_or_else(|| ident),
                        parameters: params.into_boxed_slice(),
                        return_type,
                        pure,
                        memo,
                        memo_params: memo_params.into_boxed_slice(),
                    }),
                    span,
                )),
                data_type: fn_type,
            }),
            span,
        })
    }

    fn type_of(
        &self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        span: Span,
    ) -> Option<ParserDataType> {
        Some(ParserDataType {
            span,
            data_type: ParserInnerType::NativeFunction {
                return_type: Box::new(
                    env.resolve_data_type(
                        scope,
                        &self.return_type.clone().resolve_ffi(),
                        ResolutionOptions::typing(),
                    )
                    .ok()?,
                ),
                parameters: self
                    .parameters
                    .clone()
                    .into_iter()
                    .map(|x| {
                        env.resolve_data_type(scope, &x.resolve_ffi(), ResolutionOptions::typing())
                    })
                    .collect::<Result<Vec<_>, MiddleErr>>()
                    .ok()?,
            },
        })
    }
}

impl MirLowering for AstFunction {
    #[instrument(skip_all)]
    fn lower(
        self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        span: Span,
    ) -> Result<MiddleNode, MiddleErr> {
        let default_params = FunctionParamDefault::get(env, scope, &self.header);
        let mut params = Vec::with_capacity(self.header.parameters.len());
        let mut param_idents = Vec::with_capacity(self.header.parameters.len());
        let mut old_func_defers = std::mem::take(&mut env.symbols.function_defers);
        let new_scope = env.scoping.new_scope_from_parent_shallow(scope);

        let generic_params: Vec<Ustr> = self
            .header
            .generics
            .0
            .iter()
            .map(|g| Ustr::from(&g.identifier.to_string()))
            .collect();

        if !generic_params.is_empty() {
            env.scoping.push_generic_params(generic_params.clone());
        }

        let needs_caller_context = env.tagging.tag_info.contains(&TagInfo::CallerContext);

        for param in self.header.parameters {
            param_idents.push(param.0.clone());
            let og_name = env.resolve(
                new_scope,
                &param.0,
                ResolutionOptions::default().with_dollar(),
            )?;

            let new_name =
                Ustr::from(&ParserText::temp_name_with_suffix(og_name.trim(), span).text);

            let data_type = if let Some(x) = param.1 {
                env.resolve_data_type(new_scope, &x, ResolutionOptions::typing())?
            } else if let Some(node) = &param.2 {
                node.type_of(env, new_scope, span).ok_or_else(|| {
                    env.context
                        .err_at_current(MiddleErr::CannotInferParameterType(
                            og_name.to_string(),
                            "function".to_string(),
                        ))
                })?
            } else {
                return Err(env
                    .context
                    .err_at_current(MiddleErr::CannotInferParameterType(
                        og_name.to_string(),
                        "function".to_string(),
                    )));
            };

            env.register_variable(
                new_scope,
                og_name,
                new_name,
                data_type.clone(),
                VarType::Mutable,
            )?;

            params.push((
                new_name,
                data_type,
                param
                    .2
                    .map(|x| Box::new(x.lower_or_empty(env, new_scope, span))),
            ));
        }

        if needs_caller_context {
            let caller_context_name =
                Ustr::from(&ParserText::temp_name_with_suffix("caller_context", span).text);
            let caller_context_type =
                ParserDataType::new(span, ParserInnerType::Struct(String::from("ExecContext")));

            env.register_variable(
                new_scope,
                Ustr::from("caller_context"),
                caller_context_name,
                caller_context_type.clone(),
                VarType::Mutable,
            )?;

            params.push((caller_context_name, caller_context_type, None));
        }

        let return_type = env.resolve_data_type(
            new_scope,
            &self.header.return_type,
            ResolutionOptions::typing(),
        )?;

        env.scoping.return_type_stack.push(return_type.key());

        let mut body = self.body.rewrite_main_emits_to_returns();

        if !self.header.param_destructures.is_empty() {
            let mut destructures = Vec::new();
            for (param_index, pattern) in self.header.param_destructures {
                if let Some(tmp_name) = param_idents.get(param_index) {
                    destructures
                        .extend(env.emit_destructure_statements(tmp_name, &pattern, span, true));
                }
            }
            body = match body.node_type {
                AstNodeType::ScopeDeclaration(AstScopeDef {
                    body: Some(mut inner),
                    named,
                    is_temp,
                    create_new_scope,
                    define,
                }) => {
                    let mut new_body = destructures;
                    new_body.append(&mut inner);
                    AstNode::new(
                        body.span,
                        AstNodeType::ScopeDeclaration(AstScopeDef {
                            body: Some(new_body),
                            named,
                            is_temp,
                            create_new_scope,
                            define,
                        }),
                    )
                }
                _ => {
                    let mut new_body = destructures;
                    new_body.push(body);
                    AstNode::new_temp_scope_with_create(new_body, Some(false))
                }
            };
        }

        if let Some(elem_type) = return_type.clone().get_gen() {
            body = MiddleEnvironment::wrap_generator_body(body, elem_type, span);
        }

        let body = body.lower(env, new_scope, span)?;
        let mut func_defers = Vec::new();
        func_defers.append(&mut env.symbols.function_defers);

        let body = if let MiddleNodeType::ScopeDeclaration(MirScopeDecl {
            body: scope_body,
            create_new_scope,
            is_temp: _,
            scope_id,
        }) = body.node_type
        {
            let mut scope_body = scope_body.into_vec();
            let mut last = scope_body.pop();
            for defer in func_defers {
                scope_body.push(defer.lower(env, scope_id, span)?);
            }

            if return_type.data_type != ParserInnerType::Null
                && let Some(last_node) = last.take()
            {
                if matches!(last_node.node_type, MiddleNodeType::Return { .. }) {
                    last = Some(last_node);
                } else {
                    let simple_return = matches!(
                        last_node.node_type,
                        MiddleNodeType::Identifier(_)
                            | MiddleNodeType::IntLiteral { .. }
                            | MiddleNodeType::FloatLiteral(_)
                            | MiddleNodeType::StringLiteral(_)
                            | MiddleNodeType::CharLiteral(_)
                            | MiddleNodeType::Null
                            | MiddleNodeType::FieldAccess { .. }
                            | MiddleNodeType::IndexAccess { .. }
                    );
                    if simple_return {
                        last = Some(MiddleNode::new(
                            MiddleNodeType::Return(MirReturn {
                                value: Some(Box::new(last_node)),
                            }),
                            span,
                        ));
                    } else {
                        let last_node = match last_node.node_type {
                            MiddleNodeType::Conditional(MirConditional {
                                comparison,
                                then,
                                otherwise,
                            }) => {
                                let wrap = |node: Box<MiddleNode>| {
                                    if matches!(node.node_type, MiddleNodeType::Return { .. }) {
                                        node
                                    } else {
                                        Box::new(MiddleNode::new(
                                            MiddleNodeType::Return(MirReturn { value: Some(node) }),
                                            span,
                                        ))
                                    }
                                };
                                let then = wrap(then);
                                let otherwise = match otherwise {
                                    Some(other) => Some(wrap(other)),
                                    None => Some(Box::new(MiddleNode::new(
                                        MiddleNodeType::Return(MirReturn { value: None }),
                                        span,
                                    ))),
                                };
                                MiddleNode {
                                    span: last_node.span,
                                    node_type: MiddleNodeType::Conditional(MirConditional {
                                        comparison,
                                        then,
                                        otherwise,
                                    }),
                                }
                            }
                            _ => last_node,
                        };
                        last = Some(last_node);
                    }
                }
            }

            if let Some(last) = last {
                scope_body.push(last);
            }

            MiddleNode {
                span: body.span,
                node_type: MiddleNodeType::ScopeDeclaration(MirScopeDecl {
                    body: scope_body.into_boxed_slice(),
                    create_new_scope,
                    is_temp: true,
                    scope_id,
                }),
            }
        } else {
            MiddleNode {
                span: body.span,
                node_type: MiddleNodeType::ScopeDeclaration(MirScopeDecl {
                    body: Box::new([body]),
                    create_new_scope: true,
                    is_temp: true,
                    scope_id: scope,
                }),
            }
        };

        env.symbols.function_defers.append(&mut old_func_defers);

        let mut memo = false;
        let mut memo_params = Vec::new();
        let mut pure = false;

        for tag in env.tagging.tag_info.iter() {
            #[allow(clippy::single_match)]
            match tag {
                TagInfo::Pure(x) => {
                    pure = true;
                    if x.memo {
                        memo = true
                    };
                    memo_params.append(&mut x.params.clone());
                }
                _ => {}
            }
        }

        if pure && return_type.is_null() {
            env.context.push_error(
                env.context
                    .err_at_current(MiddleErr::PureFunctionNoReturnType),
            );
        }

        let default_args_id = if default_params.is_empty() {
            None
        } else {
            let index = env.context.increment_counter();
            env.symbols
                .function_param_defaults
                .insert(index, default_params);
            Some(index)
        };

        let fn_node = MiddleNode {
            node_type: MiddleNodeType::FunctionDeclaration(MirFunction {
                parameters: params.clone().into_boxed_slice(),
                body: Box::new(body.clone()),
                return_type: return_type.clone(),
                scope_id: new_scope,
                default_args_id,
                memo,
                memo_params: memo_params.into_boxed_slice(),
                pure,
            }),
            span,
        };

        let _ = env.scoping.return_type_stack.pop();

        if !self.header.generics.0.is_empty() {
            env.scoping.pop_generic_params();
        }

        // TODO revisit this
        for (name, _, _) in params.iter() {
            if let Some(short) = ParserText::get_temp_name_suffix(name) {
                env.scoping
                    .scope_mut_or_err(new_scope)?
                    .mappings
                    .insert(Ustr::from(&short), *name);
            }
        }

        Ok(fn_node)
    }

    fn type_of(
        &self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        span: Span,
    ) -> Option<ParserDataType> {
        self.header.type_of(env, scope, span)
    }
}

impl MirLowering for AstCall {
    // TODO Deal with generics
    #[instrument(skip_all)]
    fn lower(
        mut self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        span: Span,
    ) -> Result<MiddleNode, MiddleErr> {
        match self.caller.node_type.clone() {
            AstNodeType::FieldAccess(AstField { base, field }) => {
                let field_name = env
                    .resolve(scope, &field, ResolutionOptions::default().with_dollar())
                    .unwrap_or(Ustr::from(field.text()));

                if let Some(ty) = base.type_of(env, scope, span)
                    && let Some(x) = env
                        .typing
                        .find_impl_member(&ty, &field_name)
                        .map(|x| x.symbol_name)
                {
                    self.args.insert(0, CallArg::Value(*base));
                    return AstCall {
                        string_fn: None,
                        caller: Box::new(AstNode::identifier(self.caller.span, x)),
                        ..self
                    }
                    .lower(env, scope, span);
                }

                if let Ok(resolved) = (AstField {
                    base: base.clone(),
                    field: field.clone(),
                }
                .lower(env, scope, span))
                    && let MiddleNodeType::Identifier(symbol) = resolved.node_type
                {
                    return AstCall {
                        string_fn: None,
                        caller: Box::new(AstNode::identifier(self.caller.span, symbol.identifier)),
                        ..self
                    }
                    .lower(env, scope, span);
                }
            }
            AstNodeType::Identifier(caller_ident) => {
                match caller_ident.value.get_ident().text().as_str() {
                    "tuple" => {
                        return Ok(env.aggregate_from_call_nodes(
                            scope,
                            span,
                            None,
                            self.args,
                            self.reverse_args,
                        ));
                    }
                    "curry" if self.args.len() == 1 && self.reverse_args.is_empty() => {
                        let rewritten =
                            env.rewrite_curry_call(scope, span, self.args.pop().unwrap().into())?;
                        return rewritten.lower(env, scope, span);
                    }
                    _ => {}
                }

                if let Ok(caller) =
                    env.resolve(scope, &caller_ident.value, ResolutionOptions::typing())
                    && env.typing.objects.contains_key(&caller)
                {
                    return Ok(env.aggregate_from_call_nodes(
                        scope,
                        span,
                        Some(caller),
                        self.args,
                        self.reverse_args,
                    ));
                }
            }
            _ => {}
        }

        let data_type = self
            .caller
            .type_of(env, scope, span)
            .map(|x| x.unwrap_all_refs().data_type);

        if !env.context.type_check
            && let Some(ParserInnerType::Function { parameters, .. }) = &data_type
        {
            let all_args: Vec<&AstNode> = self
                .args
                .iter()
                .map(|a| match a {
                    CallArg::Value(v) => v,
                    CallArg::Named(_, v) => v,
                })
                .chain(self.reverse_args.iter())
                .collect();

            for (i, arg) in all_args.iter().enumerate() {
                if let Some(param) = parameters.get(i) {
                    let arg_ty = arg.type_of(env, scope, span);
                    env.compare_types_ref(
                        Some(param),
                        arg_ty.as_ref(),
                        Some(&TagInfo::IgnoreInvalidTypeCheck),
                    )?;
                }
            }
        }

        let caller_name = if let AstNodeType::Identifier(ident) = &self.caller.node_type {
            env.resolve(
                scope,
                &ident.value,
                ResolutionOptions::default().with_dollar(),
            )?
        } else {
            Ustr::default()
        };

        let needs_caller_context = if let Some(var) = env.symbols.variables.get(&caller_name) {
            match var.data_type.clone().unwrap_all_refs().data_type {
                ParserInnerType::Function {
                    return_type: _,
                    parameters,
                } if !parameters.is_empty() => {
                    parameters.last().unwrap().data_type
                        == ParserInnerType::Struct(String::from("ExecContext"))
                }
                _ => false,
            }
        } else {
            false
        };

        if needs_caller_context && let Some(x) = env.get_caller_context(scope, span) {
            self.reverse_args.push(x);
        }

        let caller = self.caller.lower(env, scope, span)?;

        Ok(MiddleNode {
            node_type: MiddleNodeType::CallExpression(MirCall {
                args: if let Some(x) = env.lower_args(
                    scope,
                    span,
                    &caller,
                    &data_type,
                    self.args.clone(),
                    self.reverse_args.clone(),
                ) {
                    x
                } else {
                    env.lower_call_args(scope, self.args, self.reverse_args)
                },
                caller: Box::new(caller),
            }),
            span,
        })
    }

    fn type_of(
        &self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        span: Span,
    ) -> Option<ParserDataType> {
        if let AstNodeType::FieldAccess(AstField { base, field }) = &self.caller.node_type {
            let member_name = env
                .resolve(scope, field, ResolutionOptions::default().with_dollar())
                .unwrap_or(Ustr::from(field.text()));

            if !member_name.is_empty() {
                if let Some(ty) = &base.type_of(env, scope, span).or_else(|| {
                    if let AstNodeType::Identifier(id) = &base.node_type {
                        env.resolve_to_data_type(scope, &id.value).ok()
                    } else {
                        None
                    }
                }) && let Some(method_ty) = env.resolve_member_fn_type(ty, &member_name)
                {
                    return method_ty.apply_callable();
                }

                return Some(ParserDataType::new(base.span, ParserInnerType::Dynamic));
            }
        }

        let mut caller_type = None;
        if let AstNodeType::Identifier(caller) = &self.caller.node_type {
            match caller.value.get_ident().text().as_str() {
                "tuple" => {
                    let mut lst = Vec::new();

                    for arg in &self.args {
                        let ty = arg.get_node().type_of(env, scope, span)?;
                        lst.push(ty);
                    }
                    return Some(ParserDataType {
                        data_type: ParserInnerType::Tuple(lst),
                        span,
                    });
                }
                "curry" if self.args.len() == 1 && self.reverse_args.is_empty() => {
                    return env.resolve_curried_type(scope, self.args[0].get_node());
                }
                _ => {}
            }

            if let Ok(caller_ty) = env.resolve_to_data_type(scope, &caller.value) {
                match &caller_ty.data_type {
                    ParserInnerType::Struct(name)
                        if env.typing.objects.contains_key(&Ustr::from(name)) =>
                    {
                        return Some(ParserDataType {
                            data_type: ParserInnerType::Struct(name.clone()),
                            span,
                        });
                    }
                    ParserInnerType::StructWithGenerics {
                        identifier,
                        generic_types,
                    } if env.typing.objects.contains_key(&Ustr::from(identifier)) => {
                        return Some(ParserDataType {
                            data_type: ParserInnerType::StructWithGenerics {
                                identifier: identifier.clone(),
                                generic_types: generic_types.clone(),
                            },
                            span,
                        });
                    }
                    _ => {}
                }
            }
        }

        caller_type = caller_type.or_else(|| self.caller.type_of(env, scope, span));

        caller_type?.data_type.apply_callable()
    }
}
