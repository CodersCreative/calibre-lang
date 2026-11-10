use crate::{
    ast::{
        MiddleNode, MiddleNodeType, MirAggregate, MirCall, MirDiscriminant, MirExtern, MirFunction,
        MirFunctionParam, MirScopeDecl, MirVarDecl, types::MirDataType, typing::MirTypable,
    },
    environment::MiddleEnvironment,
    errors::MiddleErr,
    scoping::{FullyQualifiedPath, ScopeId},
    symbols::{
        FunctionParamDefault, TypeKey, VariableKey,
        resolve::{Key, ResolutionOptions},
    },
    tags::TagInfo,
    translate::MirLowering,
};
use calibre_parser::{
    Span,
    ast::{
        ObjectType,
        idents::{ParserText, PotentialDollarIdentifier, PotentialGenericTypeIdentifier},
        nodes::{
            AstNode, AstNodeType, VarType,
            access::AstField,
            binary::{AsFailureMode, AstAs},
            declaration::AstDeclaration,
            functions::{AstCall, AstExtern, AstFunction, CallArg, FunctionHeader},
            lists::AstList,
            literals::{AstString, AstStruct},
            memory::AstRef,
            scopes::AstScopeDef,
        },
        types::{GenericTypes, ParserDataType, ParserInnerType},
    },
};
use std::sync::Arc;
use tracing::instrument;
use ustr::Ustr;

impl MiddleEnvironment {
    pub(crate) fn wrap_generator_body(
        &mut self,
        _scope: ScopeId,
        body: AstNode,
        elem_type: MirDataType,
        span: Span,
    ) -> AstNode {
        let next_name = self.context.get_temp("gen_next");

        let elem_type: ParserDataType = elem_type.into();
        let next_body = AstNode::new_temp_scope(
            body.nodes()
                .into_iter()
                .chain(std::iter::once(AstNode::identifier(span, "none")))
                .collect(),
        );

        let next_decl = AstNode::new(
            span,
            AstNodeType::VariableDeclaration(AstDeclaration {
                var_type: VarType::Immutable,
                identifier: PotentialDollarIdentifier::new(span, next_name.clone()),
                data_type: ParserDataType::function(
                    span,
                    Vec::new(),
                    ParserDataType::new(span, ParserInnerType::Option(Box::new(elem_type.clone()))),
                ),
                value: Box::new(AstNode::new(
                    span,
                    AstNodeType::FunctionDeclaration(AstFunction {
                        header: FunctionHeader {
                            generics: GenericTypes::default(),
                            parameters: Vec::new(),
                            return_type: ParserDataType::new(
                                span,
                                ParserInnerType::Option(Box::new(elem_type.clone())),
                            ),
                            param_destructures: Vec::new(),
                        },
                        body: Box::new(next_body),
                    }),
                )),
                declared: false,
            }),
        );

        let gen_value = AstNode::new(
            span,
            AstNodeType::StructLiteral(AstStruct {
                identifier: Some(PotentialGenericTypeIdentifier::Generic {
                    identifier: PotentialDollarIdentifier::Identifier(ParserText::new(
                        span,
                        String::from("gen"),
                    )),
                    generic_types: vec![elem_type],
                }),
                value: ObjectType::Map(vec![
                    (Ustr::from("data"), AstNode::identifier(span, &next_name)),
                    (Ustr::from("index"), AstNode::int(span, 0)),
                    (Ustr::from("done"), AstNode::bool(span, false)),
                ]),
            }),
        );

        AstNode::new_temp_scope_with_create(vec![next_decl, AstNode::ret(gen_value)], true)
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
                identifier: Some(PotentialGenericTypeIdentifier::new(span, "ExecContext")),
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
        _data_type: Option<MirDataType>,
    ) -> Result<MiddleNode, MiddleErr> {
        unreachable!()
    }

    fn type_of(
        &self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        span: Span,
    ) -> Option<MirDataType> {
        let generic_params: Vec<Ustr> = self
            .generics
            .0
            .iter()
            .map(|g| {
                env.resolve(scope, g, ResolutionOptions::default().with_dollar())
                    .map(|x| x.unwrap_dollar())
            })
            .collect::<Result<Vec<Ustr>, MiddleErr>>()
            .ok()?;

        if !generic_params.is_empty() {
            env.scoping.push_generic_params(generic_params.clone());
        }

        let return_type = env
            .resolve_data_type(scope, &self.return_type, ResolutionOptions::typing())
            .ok()?;

        Some(MirDataType::Function {
            return_type: Box::new(return_type),
            parameters: {
                let mut params = Vec::with_capacity(self.parameters.len());

                for param in &self.parameters {
                    let data_type = if let Some(x) = &param.data_type {
                        env.resolve_data_type(scope, x, ResolutionOptions::typing())
                            .ok()?
                    } else if let Some(node) = &param.default {
                        node.type_of(env, scope, span)?
                    } else {
                        return None;
                    };
                    params.push(data_type);
                }

                env.scoping.pop_generic_params();

                params
            },
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
        _data_type: Option<MirDataType>,
    ) -> Result<MiddleNode, MiddleErr> {
        let ident = env
            .resolve(
                scope,
                &self.identifier,
                ResolutionOptions::default().with_dollar(),
            )?
            .unwrap_dollar();

        let params = self
            .parameters
            .iter()
            .map(|ty| env.resolve_data_type(scope, ty, ResolutionOptions::typing()))
            .collect::<Result<Vec<_>, MiddleErr>>()?;

        let return_type =
            env.resolve_data_type(scope, &self.return_type, ResolutionOptions::typing())?;

        let fn_type = MirDataType::function(params.clone(), return_type.clone());

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

        let var_key = if self.declared {
            env.resolve(scope, ident, ResolutionOptions::idents())?
                .unwrap_variable()
        } else {
            env.register_variable(scope, ident, fn_type.clone(), VarType::Constant)?
        };

        Ok(MiddleNode {
            node_type: MiddleNodeType::VariableDeclaration(MirVarDecl {
                var_type: VarType::Constant,
                identifier: var_key,
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
        _span: Span,
    ) -> Option<MirDataType> {
        Some(MirDataType::NativeFunction {
            return_type: Box::new(
                env.resolve_data_type(scope, &self.return_type, ResolutionOptions::typing())
                    .ok()?,
            ),
            parameters: self
                .parameters
                .iter()
                .map(|x| env.resolve_data_type(scope, x, ResolutionOptions::typing()))
                .collect::<Result<Vec<_>, MiddleErr>>()
                .ok()?,
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
        data_type: Option<MirDataType>,
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
            .map(|g| Ustr::from(g))
            .collect();

        if !generic_params.is_empty() {
            env.scoping.push_generic_params(generic_params.clone());
        }

        let needs_caller_context = env.tagging.tag_info.contains(&TagInfo::CallerContext);

        for (i, param) in self.header.parameters.into_iter().enumerate() {
            param_idents.push(param.identifier.clone());

            let og_name = env
                .resolve(
                    new_scope,
                    &param.identifier,
                    ResolutionOptions::default().with_dollar(),
                )?
                .unwrap_dollar();

            let data_type = if let Some(x) = param.data_type
                && !x.is_auto()
            {
                env.resolve_data_type(new_scope, &x, ResolutionOptions::typing())?
            } else if let Some(MirDataType::Function { parameters, .. }) = &data_type
                && let Some(x) = parameters.get(i).cloned()
            {
                x
            } else if let Some(node) = &param.default {
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

            let param_key = env.register_variable_with_temp_scope(
                new_scope,
                og_name,
                data_type.clone(),
                param.var_type,
                true,
            )?;

            params.push(MirFunctionParam {
                var_type: param.var_type,
                identifier: param_key,
                data_type: data_type.clone(),
                default: param
                    .default
                    .map(|x| Box::new(x.lower_or_empty(env, new_scope, span, Some(data_type)))),
            });
        }

        if needs_caller_context {
            let caller_context_type = env.resolve_to_data_type(new_scope, &"ExecContext")?;

            let caller_key = env.register_variable_with_temp_scope(
                new_scope,
                Ustr::from("caller_context"),
                caller_context_type.clone(),
                VarType::Mutable,
                true,
            )?;

            params.push(MirFunctionParam {
                var_type: VarType::Mutable,
                identifier: caller_key,
                data_type: caller_context_type,
                default: None,
            });
        }

        let return_type = if self.header.return_type.is_null()
            && let Some(MirDataType::Function { return_type, .. }) = &data_type
        {
            *return_type.clone()
        } else {
            env.resolve_data_type(
                new_scope,
                &self.header.return_type,
                ResolutionOptions::typing(),
            )?
        };

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
                    body: mut inner,
                    named,
                    is_temp,
                    create_new_scope,
                    define,
                    is_function_body,
                }) => {
                    let mut new_body = destructures;
                    new_body.append(&mut inner);
                    AstNode::new(
                        body.span,
                        AstNodeType::ScopeDeclaration(AstScopeDef {
                            body: new_body,
                            named,
                            is_temp,
                            create_new_scope,
                            is_function_body,
                            define,
                        }),
                    )
                }
                _ => {
                    let mut new_body = destructures;
                    new_body.push(body);
                    AstNode::new_temp_scope_with_create(new_body, false)
                }
            };
        }

        let old_in_generator = env.context.in_generator;

        if let Some(elem_type) = return_type.clone().get_gen()
            && !env.tagging.tag_info.contains(&TagInfo::ReturnsGen)
        {
            body = MiddleEnvironment::wrap_generator_body(env, scope, body, elem_type, span);
            env.context.in_generator = true;
        }

        let body = body.lower(env, new_scope, span, None)?;

        let mut func_defers = Vec::new();
        func_defers.append(&mut env.symbols.function_defers);

        let body = if let MiddleNodeType::ScopeDeclaration(MirScopeDecl {
            body: scope_body,
            create_new_scope,
            is_temp: _,
            scope_id,
            function_body: _,
        }) = body.node_type
        {
            let mut scope_body = scope_body.into_vec();

            for defer in func_defers {
                scope_body.push(defer.lower(env, scope_id, span, None)?);
            }

            MiddleNode {
                span: body.span,
                node_type: MiddleNodeType::ScopeDeclaration(MirScopeDecl {
                    body: scope_body.into_boxed_slice(),
                    create_new_scope,
                    is_temp: true,
                    function_body: true,
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
                    function_body: true,
                    scope_id: scope,
                }),
            }
        };

        env.symbols.function_defers.append(&mut old_func_defers);
        env.context.in_generator = old_in_generator;

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
                    .err_at_span(span, MiddleErr::PureFunctionNoReturnType),
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

        Ok(fn_node)
    }

    fn type_of(
        &self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        span: Span,
    ) -> Option<MirDataType> {
        self.header.type_of(env, scope, span)
    }
}

impl MiddleEnvironment {
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

    #[allow(clippy::type_complexity)]
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

            for (i, arg) in args.drain(..normal_count).enumerate() {
                let node: AstNode = arg.into();
                let param_ty = params.get(i).and_then(|p| {
                    self.resolve_data_type(scope, p, ResolutionOptions::typing())
                        .ok()
                });
                lst.push(node.lower_or_empty(self, scope, span, param_ty));
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
                let arg: AstNode = args.pop()?.into();

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
                            data_type: list_inner_type.clone(),
                            values: vec![AstNode::new(
                                span,
                                AstNodeType::AsExpression(AstAs {
                                    value: Box::new(arg),
                                    data_type: list_inner_type.clone(),
                                    failure_mode: AsFailureMode::Panic,
                                }),
                            )],
                        }),
                    )
                }
            } else {
                AstNode::new(
                    span,
                    AstNodeType::ListLiteral(AstList {
                        data_type: list_inner_type.clone(),
                        values: args
                            .into_iter()
                            .map(|x| {
                                AstNode::new(
                                    span,
                                    AstNodeType::AsExpression(AstAs {
                                        value: Box::new(x.into()),
                                        data_type: list_inner_type.clone(),
                                        failure_mode: AsFailureMode::Panic,
                                    }),
                                )
                            })
                            .collect(),
                    }),
                )
            };

            let list_param_ty = self
                .resolve_data_type(scope, &list_inner_type, ResolutionOptions::typing())
                .ok();
            lst.push(list_arg.lower_or_empty(self, scope, span, list_param_ty));

            let reverse_len = reverse_args.len();
            for (r_idx, arg) in reverse_args.into_iter().enumerate() {
                let param_idx = params.len().saturating_sub(reverse_len) + r_idx;
                let param_ty = params.get(param_idx).and_then(|p| {
                    self.resolve_data_type(scope, p, ResolutionOptions::typing())
                        .ok()
                });
                lst.push(arg.lower_or_empty(self, scope, span, param_ty));
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
                    .map(|x| x.unwrap_all_refs()),
                Some(MirDataType::Option(_))
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
                                AstNode::unwrap_or(span, current, default.clone().into())
                            } else {
                                current
                            }
                        } else {
                            current
                        }
                    }
                };

                let param_ty = parameters.and_then(|p| p.get(i)).and_then(|p| {
                    self.resolve_data_type(scope, p, ResolutionOptions::typing())
                        .ok()
                });

                if wrap_some {
                    Some(
                        AstNode::call(
                            span,
                            AstNode::identifier(span, "some"),
                            vec![CallArg::Value(node)],
                        )
                        .lower_or_empty(self, scope, span, param_ty),
                    )
                } else {
                    Some(node.lower_or_empty(self, scope, span, param_ty))
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
        identifier: Option<TypeKey>,
        args: Vec<CallArg>,
        reverse_args: Vec<AstNode>,
        parameters: Option<&[ParserDataType]>,
    ) -> MiddleNode {
        let value = Self::collect_call_nodes(args, reverse_args)
            .into_iter()
            .enumerate()
            .map(|(i, arg)| {
                let param_ty = parameters.and_then(|p| p.get(i)).and_then(|p| {
                    self.resolve_data_type(scope, p, ResolutionOptions::typing())
                        .ok()
                });
                (
                    i.to_string(),
                    arg.lower_or_empty(self, scope, span, param_ty),
                )
            })
            .collect::<Vec<_>>()
            .into();

        MiddleNode {
            node_type: MiddleNodeType::AggregateExpression(MirAggregate { identifier, value }),
            span,
        }
    }

    #[inline]
    pub(crate) fn lower_call_args(
        &mut self,
        scope: ScopeId,
        args: Vec<CallArg>,
        reverse_args: Vec<AstNode>,
        parameters: Option<&[ParserDataType]>,
    ) -> Box<[MiddleNode]> {
        args.into_iter()
            .map(AstNode::from)
            .chain(reverse_args)
            .enumerate()
            .map(|(i, arg)| {
                let span = arg.span;
                let param_ty = parameters.and_then(|p| p.get(i)).and_then(|p| {
                    self.resolve_data_type(scope, p, ResolutionOptions::typing())
                        .ok()
                });
                arg.lower_or_empty(self, scope, span, param_ty)
            })
            .collect()
    }
}

fn method_receiver(
    env: &MiddleEnvironment,
    base: AstNode,
    base_type: &MirDataType,
    symbol_name: &VariableKey,
) -> AstNode {
    let mutability = env
        .symbols
        .variables
        .get(symbol_name)
        .and_then(|var| match &var.data_type {
            MirDataType::Function { parameters, .. } => parameters.first(),
            _ => None,
        })
        .and_then(|parameter| match parameter {
            MirDataType::Ref(_, mutability) => Some(*mutability),
            _ => None,
        });

    if let Some(mutability) = mutability
        && !base_type.is_ref()
    {
        AstNode::new(
            base.span,
            AstNodeType::RefStatement(AstRef {
                value: Box::new(base),
                mutability,
            }),
        )
    } else {
        base
    }
}

trait AstCallLowering {
    fn lower_field_call(
        &self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        span: Span,
        data_type: Option<MirDataType>,
    ) -> Result<Option<AstCall>, MiddleErr>;
}

impl AstCallLowering for AstCall {
    fn lower_field_call(
        &self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        span: Span,
        data_type: Option<MirDataType>,
    ) -> Result<Option<Self>, MiddleErr> {
        let AstNodeType::FieldAccess(AstField { base, field }) = self.caller.node_type.clone()
        else {
            return Ok(None);
        };

        let field_name = env
            .resolve(scope, &field, ResolutionOptions::default().with_dollar())?
            .unwrap_dollar();

        let base_type = base.type_of(env, scope, span);
        let is_instance_call = base_type.is_some();

        let base_type = base_type.or_else(|| {
            if let AstNodeType::Identifier(ident) = &base.node_type {
                env.resolve_to_data_type(scope, &ident.value).ok()
            } else {
                None
            }
        });

        if let Some(ty) = base_type {
            if let Some((member, bindings)) =
                env.typing.find_impl_member_with_subst(&ty, field_name)
            {
                let symbol_name = member.symbol_name.clone();
                let generic_params = env
                    .monomorphizer
                    .function_template(&symbol_name)
                    .map(|(params, _, _)| params.clone())
                    .unwrap_or_else(|| member.generic_params.clone());

                if member.has_method_generics
                    || env
                        .monomorphizer
                        .function_template_key(&symbol_name)
                        .is_some()
                {
                    let mut concrete_args = generic_params
                        .iter()
                        .map(|param| {
                            bindings
                                .get(param.as_str())
                                .cloned()
                                .unwrap_or(MirDataType::Dynamic)
                        })
                        .collect::<Vec<_>>();

                    let mut arg_types = Vec::new();

                    if is_instance_call {
                        arg_types.push(ty.clone());
                    }

                    arg_types.extend(self.args.iter().filter_map(|arg| match arg {
                        CallArg::Value(node) => node.type_of(env, scope, span),
                        CallArg::Named(_, _) => None,
                    }));

                    if let Some(MirDataType::Function {
                        return_type,
                        parameters,
                    }) = env
                        .symbols
                        .variables
                        .get(&symbol_name)
                        .map(|var| var.data_type.clone())
                        && let Ok(inferred_types) = env.infer_concrete_type_args(
                            scope,
                            &generic_params,
                            &parameters,
                            &return_type,
                            &arg_types,
                            data_type.as_ref(),
                        )
                    {
                        concrete_args = inferred_types
                            .into_iter()
                            .enumerate()
                            .map(|(index, inferred)| {
                                let param = generic_params[index];
                                bindings.get(param.as_str()).cloned().unwrap_or(inferred)
                            })
                            .collect();
                    }

                    let key =
                        env.monomorphize_function(scope, symbol_name.clone(), concrete_args)?;

                    let mut node = self.clone();

                    node.caller = Box::new(AstNode::identifier(
                        self.caller.span,
                        env.context
                            .convert_key_to_ustr(Key::VariableKey(key.clone())),
                    ));

                    if is_instance_call {
                        node.args
                            .insert(0, CallArg::Value(method_receiver(env, *base, &ty, &key)));
                    }

                    return Ok(Some(node));
                }

                if is_instance_call {
                    let mut node = self.clone();

                    node.caller = Box::new(AstNode::identifier(
                        self.caller.span,
                        env.context
                            .convert_key_to_ustr(Key::VariableKey(symbol_name.clone())),
                    ));

                    node.args.insert(
                        0,
                        CallArg::Value(method_receiver(env, *base, &ty, &symbol_name)),
                    );

                    return Ok(Some(node));
                }
            }

            if is_instance_call
                && let Some(symbol_name) = env
                    .typing
                    .find_impl_member(&ty, field_name)
                    .map(|member| member.symbol_name.clone())
            {
                let mut node = self.clone();

                node.caller = Box::new(AstNode::identifier(
                    self.caller.span,
                    env.context
                        .convert_key_to_ustr(Key::VariableKey(symbol_name.clone())),
                ));

                node.args.insert(
                    0,
                    CallArg::Value(method_receiver(env, *base, &ty, &symbol_name)),
                );

                return Ok(Some(node));
            }
        }

        if let Ok(resolved) = (AstField { base, field }).lower(env, scope, span, None)
            && let MiddleNodeType::Identifier(symbol) = resolved.node_type
        {
            let mut node = self.clone();

            node.caller = Box::new(AstNode::identifier(
                self.caller.span,
                env.context
                    .convert_key_to_ustr(Key::VariableKey(symbol.identifier)),
            ));

            return Ok(Some(node));
        }

        Ok(None)
    }
}

fn lower_basic_call(
    call: &mut AstCall,
    env: &mut MiddleEnvironment,
    scope: ScopeId,
    span: Span,
    data_type: Option<&MirDataType>,
) -> Result<Option<MiddleNode>, MiddleErr> {
    let AstNodeType::Identifier(caller) = &call.caller.node_type else {
        return Ok(None);
    };

    let parameters = data_type
        .map(|ty| ParserInnerType::from(ty.clone()))
        .and_then(|ty| match ty {
            ParserInnerType::Function { parameters, .. } => Some(parameters),
            _ => None,
        });

    match caller.value.get_ident().text().as_str() {
        "discriminant"
            if call.args.len() == 1
                && let Some(value) = call.args.pop() =>
        {
            let value = AstNode::from(value).lower(env, scope, span, None)?;
            Ok(Some(MiddleNode::new(
                MiddleNodeType::Discriminant(MirDiscriminant {
                    value: Box::new(value),
                }),
                span,
            )))
        }
        "tuple" => Ok(Some(env.aggregate_from_call_nodes(
            scope,
            span,
            None,
            std::mem::take(&mut call.args),
            std::mem::take(&mut call.reverse_args),
            parameters.as_deref(),
        ))),
        "curry"
            if call.args.len() == 1
                && call.reverse_args.is_empty()
                && let Some(value) = call.args.pop() =>
        {
            let value = env.rewrite_curry_call(scope, span, value.into())?;
            Ok(Some(value.lower(env, scope, span, None)?))
        }
        _ => {
            let Ok(Key::TypeKey(caller)) =
                env.resolve(scope, &caller.value, ResolutionOptions::typing())
            else {
                return Ok(None);
            };

            if !env.typing.objects.contains_key(&caller) {
                return Ok(None);
            }

            Ok(Some(env.aggregate_from_call_nodes(
                scope,
                span,
                Some(caller),
                std::mem::take(&mut call.args),
                std::mem::take(&mut call.reverse_args),
                parameters.as_deref(),
            )))
        }
    }
}

fn prepare_call(
    call: &mut AstCall,
    env: &mut MiddleEnvironment,
    scope: ScopeId,
    span: Span,
    data_type: Option<&MirDataType>,
) -> Result<(), MiddleErr> {
    let AstNodeType::Identifier(ident) = &call.caller.node_type else {
        return Ok(());
    };

    let caller_name = env
        .resolve(scope, &ident.value, ResolutionOptions::idents())?
        .unwrap_variable();

    let explicit_args = match &ident.value {
        PotentialGenericTypeIdentifier::Generic { generic_types, .. } => Some(
            generic_types
                .iter()
                .map(|ty| env.resolve_data_type(scope, ty, ResolutionOptions::typing()))
                .collect::<Result<Vec<_>, _>>(),
        ),
        _ => None,
    };

    if let Some((generic_params, header, _)) =
        env.monomorphizer.function_template(&caller_name).cloned()
    {
        let concrete_args = if let Some(explicit) = explicit_args {
            Some(explicit?)
        } else {
            let arg_types = call
                .args
                .iter()
                .filter_map(|arg| arg.get_node().type_of(env, scope, span))
                .collect::<Vec<_>>();

            let param_types = header
                .parameters
                .iter()
                .map(|parameter| {
                    if let Some(data_type) = &parameter.data_type {
                        if let ParserInnerType::Struct(name) = &data_type.data_type
                            && generic_params
                                .iter()
                                .any(|param| param.as_str() == name.as_str())
                        {
                            return Ok(MirDataType::Struct {
                                identifier: TypeKey {
                                    fully_qualified_path: Arc::new(FullyQualifiedPath {
                                        name: Some(Ustr::from(name.as_str())),
                                        parent: None,
                                    }),
                                },
                                generic_types: Vec::new(),
                            });
                        }

                        env.resolve_data_type(scope, data_type, ResolutionOptions::typing())
                    } else if let Some(node) = &parameter.default {
                        node.type_of(env, scope, span)
                            .ok_or(MiddleErr::InferImpossible)
                    } else {
                        Err(MiddleErr::InferImpossible)
                    }
                })
                .collect::<Result<Vec<_>, MiddleErr>>()?;

            let return_type = if let ParserInnerType::Struct(name) = &header.return_type.data_type
                && generic_params
                    .iter()
                    .any(|param| param.as_str() == name.as_str())
            {
                MirDataType::Struct {
                    identifier: TypeKey {
                        fully_qualified_path: Arc::new(FullyQualifiedPath {
                            name: Some(Ustr::from(name.as_str())),
                            parent: None,
                        }),
                    },
                    generic_types: Vec::new(),
                }
            } else {
                data_type.cloned().unwrap_or(MirDataType::Dynamic)
            };

            Some(env.infer_concrete_type_args(
                scope,
                &generic_params,
                &param_types,
                &return_type,
                &arg_types,
                None,
            )?)
        };

        if let Some(concrete_args) = concrete_args
            && !concrete_args
                .iter()
                .all(|ty| matches!(ty, MirDataType::Dynamic))
        {
            let key = env.monomorphize_function(scope, caller_name.clone(), concrete_args)?;
            *call.caller = AstNode::identifier(call.caller.span, *key.name());
        }
    }

    let needs_caller_context = env
        .symbols
        .variables
        .get(&caller_name)
        .is_some_and(|var| match var.data_type.unwrap_all_refs() {
            MirDataType::Function { parameters, .. } if !parameters.is_empty() => matches!(
                parameters.last(),
                Some(MirDataType::Struct { identifier, .. }) if identifier.name() == "ExecContext"
            ),
            _ => false,
        });

    if needs_caller_context && let Some(context) = env.get_caller_context(scope, span) {
        call.reverse_args.push(context);
    }

    Ok(())
}

impl MirLowering for AstCall {
    #[instrument(skip_all)]
    #[allow(clippy::only_used_in_recursion)]
    fn lower(
        mut self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        span: Span,
        data_type: Option<MirDataType>,
    ) -> Result<MiddleNode, MiddleErr> {
        if let Some(node) = self.lower_field_call(env, scope, span, data_type.clone())? {
            return node.lower(env, scope, span, data_type);
        }

        if let Some(node) = lower_basic_call(&mut self, env, scope, span, data_type.as_ref())? {
            return Ok(node);
        }

        let caller_type = self
            .caller
            .type_of(env, scope, span)
            .map(|x| x.unwrap_all_refs().clone());

        prepare_call(&mut self, env, scope, span, caller_type.as_ref())?;
        let caller = self.caller.lower(env, scope, span, None)?;

        let parser_inner_type = caller_type
            .as_ref()
            .map(|dt| ParserInnerType::from(dt.clone()));

        let parameters = match &parser_inner_type {
            Some(ParserInnerType::Function { parameters, .. }) => Some(parameters.as_slice()),
            _ => None,
        };

        let args = if let Some(x) = env.lower_args(
            scope,
            span,
            &caller,
            &parser_inner_type,
            self.args.clone(),
            self.reverse_args.clone(),
        ) {
            x
        } else {
            env.lower_call_args(scope, self.args, self.reverse_args, parameters)
        };

        if env.context.type_check
            && let Some(MirDataType::Function { parameters, .. })
            | Some(MirDataType::NativeFunction { parameters, .. }) = &caller_type
        {
            for (i, arg) in args.iter().enumerate() {
                let span = arg.span;
                let arg_ty = arg.mir_type_of(env, scope, span);

                if let Some(param) = parameters.get(i) {
                    env.compare_types_ref(
                        Some(param),
                        arg_ty.as_ref(),
                        Some(&TagInfo::IgnoreInvalidTypeCheck),
                        span,
                    )?;
                }
            }
        }

        Ok(MiddleNode {
            node_type: MiddleNodeType::CallExpression(MirCall {
                args,
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
    ) -> Option<MirDataType> {
        if let AstNodeType::FieldAccess(AstField { base, field }) = &self.caller.node_type {
            let member_name = env
                .resolve(scope, field, ResolutionOptions::default().with_dollar())
                .ok()?
                .unwrap_dollar();

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

                return Some(MirDataType::Dynamic);
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

                    return Some(MirDataType::Tuple(lst));
                }
                "curry" if self.args.len() == 1 && self.reverse_args.is_empty() => {
                    return env.resolve_curried_type(scope, self.args[0].get_node());
                }
                "discriminant" => {
                    return Some(MirDataType::UInt);
                }
                _ => {}
            }

            if let Ok(caller_ty) = env.resolve_to_data_type(scope, &caller.value) {
                match &caller_ty {
                    MirDataType::Struct {
                        identifier,
                        generic_types,
                    } if env.typing.objects.contains_key(identifier) => {
                        return Some(MirDataType::Struct {
                            identifier: identifier.clone(),
                            generic_types: generic_types.clone(),
                        });
                    }
                    _ => {}
                }
            }
        }

        caller_type = caller_type.or_else(|| self.caller.type_of(env, scope, span));

        caller_type?.apply_callable()
    }
}
