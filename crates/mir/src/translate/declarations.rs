use crate::{
    ast::{MiddleNode, MiddleNodeType, MirAs, MirVarDecl},
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
        binary::BinaryOperator,
        idents::{ParserText, PotentialDollarIdentifier, PotentialGenericTypeIdentifier},
        nodes::{
            AstNode, AstNodeType, DestructurePattern, VarType,
            access::{AstField, AstIdentifier, AstIndex},
            assignment::AstAssignment,
            binary::{AsFailureMode, AstBinary},
            declaration::{AstDeclaration, AstDeclareDestructure},
            functions::{AstCall, AstFunction, FunctionHeader},
            memory::AstRef,
            scopes::AstScopeDef,
        },
        types::{ParserDataType, ParserInnerType},
    },
};
use tracing::instrument;
use ustr::Ustr;

impl MiddleEnvironment {
    fn handle_function_template(
        &mut self,
        scope: ScopeId,
        header: &AstFunction,
        new_name: Ustr,
    ) -> bool {
        if header.header.generics.0.is_empty() {
            return false;
        }

        let template_params: Vec<Ustr> = header
            .header
            .generics
            .0
            .iter()
            .map(|g| {
                self.resolve(
                    scope,
                    &g.identifier,
                    ResolutionOptions::default().with_dollar(),
                )
            })
            .collect::<Result<Vec<_>, MiddleErr>>()
            .unwrap_or_default();

        self.symbols
            .generic_fn_templates
            .entry(new_name)
            .or_insert((
                template_params,
                header.header.clone(),
                (*header.body).clone(),
            ));

        true
    }

    fn process_parameter_defaults(
        &mut self,
        scope: ScopeId,
        header: &FunctionHeader,
        new_name: Ustr,
        identifier: Ustr,
    ) {
        let defaults = FunctionParamDefault::get(self, scope, header);

        if !defaults.is_empty() {
            let index = self.context.increment_counter();

            self.symbols.function_param_defaults.insert(index, defaults);
            self.symbols.name_to_param_defaults.insert(new_name, index);
            self.symbols
                .name_to_param_defaults
                .insert(identifier, index);
        }
    }

    pub fn emit_destructure_statements(
        &self,
        tmp_ident: &PotentialDollarIdentifier,
        pattern: &DestructurePattern,
        span: Span,
        is_declaration: bool,
    ) -> Vec<AstNode> {
        let estimated = match pattern {
            DestructurePattern::Tuple(bindings) => bindings.iter().flatten().count(),
            DestructurePattern::Struct(fields) => fields.len(),
        };
        let mut out = Vec::with_capacity(estimated);

        let tmp_member_base = || {
            AstNode::new(
                span,
                AstNodeType::Identifier(AstIdentifier {
                    value: PotentialGenericTypeIdentifier::Identifier(tmp_ident.clone()),
                }),
            )
        };

        let push_binding = |out: &mut Vec<AstNode>,
                            var_type: &VarType,
                            name: &PotentialDollarIdentifier,
                            member: AstNode| {
            if is_declaration {
                out.push(AstNode::new(
                    span,
                    AstNodeType::VariableDeclaration(AstDeclaration {
                        var_type: *var_type,
                        identifier: name.clone(),
                        data_type: ParserDataType::auto(span),
                        value: Box::new(member),
                    }),
                ));
            } else {
                out.push(AstNode::new(
                    span,
                    AstNodeType::AssignmentExpression(AstAssignment {
                        identifier: Box::new(AstNode::new(
                            span,
                            AstNodeType::Identifier(AstIdentifier {
                                value: PotentialGenericTypeIdentifier::Identifier(name.clone()),
                            }),
                        )),
                        value: Box::new(member),
                    }),
                ));
            }
        };
        match pattern {
            DestructurePattern::Tuple(bindings) => {
                let mut head = Vec::new();
                let mut tail = Vec::new();
                let mut in_tail = false;

                for binding in bindings {
                    if binding.is_none() {
                        in_tail = true;
                        continue;
                    }

                    if in_tail {
                        tail.push(binding);
                    } else {
                        head.push(binding);
                    }
                }

                let total_tail = tail.len() as i64;
                for (idx, entry) in head.into_iter().enumerate() {
                    if let Some((var_type, name)) = entry {
                        push_binding(&mut out, var_type, name, {
                            let index_node = AstNode::int(span, idx);
                            AstNode::new(
                                span,
                                AstNodeType::IndexAccess(AstIndex {
                                    base: Box::new(tmp_member_base()),
                                    index: Box::new(index_node),
                                }),
                            )
                        });
                    }
                }

                for (i, entry) in tail.into_iter().enumerate() {
                    if let Some((var_type, name)) = entry {
                        let index_expr = AstNode::new(
                            span,
                            AstNodeType::BinaryExpression(AstBinary {
                                left: Box::new(AstNode::len(
                                    span,
                                    AstNode::new(
                                        span,
                                        AstNodeType::Identifier(AstIdentifier {
                                            value: PotentialGenericTypeIdentifier::Identifier(
                                                tmp_ident.clone(),
                                            ),
                                        }),
                                    ),
                                )),
                                right: Box::new(AstNode::int(span, total_tail - i as i64)),
                                operator: BinaryOperator::Sub,
                            }),
                        );

                        push_binding(
                            &mut out,
                            var_type,
                            name,
                            AstNode::new(
                                span,
                                AstNodeType::IndexAccess(AstIndex {
                                    base: Box::new(tmp_member_base()),
                                    index: Box::new(index_expr),
                                }),
                            ),
                        );
                    }
                }
            }
            DestructurePattern::Struct(fields) => {
                for (field, var_type, name) in fields {
                    push_binding(
                        &mut out,
                        var_type,
                        name,
                        AstNode::new(
                            span,
                            AstNodeType::FieldAccess(AstField {
                                base: Box::new(tmp_member_base()),
                                field: PotentialDollarIdentifier::new(span, field),
                            }),
                        ),
                    );
                }
            }
        }

        out
    }
}

impl MirLowering for AstDeclaration {
    #[instrument(skip_all)]
    fn lower(
        mut self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        span: Span,
    ) -> Result<MiddleNode, MiddleErr> {
        let identifier = env.resolve(
            scope,
            &self.identifier,
            ResolutionOptions::default().with_dollar(),
        )?;

        let new_name = Ustr::from(&ParserText::temp_name_with_suffix(identifier.trim(), span).text);

        if let AstNodeType::CallExpression(AstCall {
            caller,
            generic_types,
            args,
            reverse_args,
            ..
        }) = &self.value.node_type
            && let AstNodeType::Identifier(callee_ident) = &caller.node_type
            && callee_ident.value.get_ident().text() == identifier
            && let Some(first_arg) = args.first()
        {
            let first_arg_node: AstNode = first_arg.clone().into();
            let first_ty = env
                .resolve_type_from_node(scope, &first_arg_node)
                .or_else(|| match &first_arg_node.node_type {
                    AstNodeType::RefStatement(AstRef { value, .. }) => {
                        env.resolve_type_from_node(scope, value.as_ref())
                    }
                    _ => None,
                });

            if let Some(first_ty) = first_ty
                && let Some(mapped_name) = env.resolve_member_fn_name(
                    &first_ty.unwrap_all_refs(),
                    &callee_ident.value.get_ident().text(),
                )
                && mapped_name != callee_ident.value.get_ident().text()
            {
                *self.value = AstNode::new(
                    self.value.span,
                    AstNodeType::CallExpression(AstCall {
                        string_fn: None,
                        caller: Box::new(AstNode::identifier(self.value.span, mapped_name)),
                        generic_types: generic_types.clone(),
                        args: args.clone(),
                        reverse_args: reverse_args.clone(),
                    }),
                );
            }
        }

        let mut is_function = false;

        if let AstNodeType::FunctionDeclaration(func) = &self.value.node_type {
            is_function = true;
            env.handle_function_template(scope, func, new_name);

            for tag in &env.tagging.tag_info {
                match tag {
                    TagInfo::Init(priority) => {
                        env.tagging.init_functions.push((*priority, new_name))
                    }
                    TagInfo::Fin(priority) => env.tagging.fin_functions.push((*priority, new_name)),
                    _ => {}
                }
            }

            env.process_parameter_defaults(scope, &func.header, new_name, identifier);
        }

        let node_ty = self.value.type_of(env, scope, span);

        let data_type = if self.data_type.is_auto() {
            None
        } else {
            Some(env.resolve_data_type(scope, &self.data_type, ResolutionOptions::typing())?)
        };

        let data_type = env.compare_types(data_type, node_ty, Some(&TagInfo::IgnoreInvalidLet))?;

        if is_function {
            env.register_variable(
                scope,
                identifier,
                new_name,
                data_type.clone(),
                self.var_type,
            )?;
        }

        let mut value = self.value.lower_or_empty(env, scope, span);

        if !is_function {
            env.register_variable(
                scope,
                identifier,
                new_name,
                data_type.clone(),
                self.var_type,
            )?;
        }

        if matches!(data_type.data_type, ParserInnerType::DynamicTraits(_)) {
            value = MiddleNode::new(
                MiddleNodeType::AsExpression(MirAs {
                    value: Box::new(value),
                    data_type: data_type.clone(),
                    failure_mode: AsFailureMode::Panic,
                }),
                span,
            );
        }

        Ok(MiddleNode::new(
            MiddleNodeType::VariableDeclaration(MirVarDecl {
                var_type: self.var_type,
                identifier: new_name,
                value: Box::new(value),
                data_type,
            }),
            span,
        ))
    }
}

impl MirLowering for AstDeclareDestructure {
    #[instrument(skip_all)]
    fn lower(
        self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        span: Span,
    ) -> Result<MiddleNode, MiddleErr> {
        let tmp_ident: PotentialDollarIdentifier =
            ParserText::temp_name_with_suffix("destructure_tmp", span).into();

        let tmp_decl = AstNode::new(
            span,
            AstNodeType::VariableDeclaration(AstDeclaration {
                var_type: VarType::Immutable,
                identifier: tmp_ident.clone(),
                data_type: ParserDataType::auto(span),
                value: self.value,
            }),
        );

        let mut body = Vec::new();
        body.push(tmp_decl);
        body.extend(env.emit_destructure_statements(&tmp_ident, &self.pattern, span, true));

        AstScopeDef {
            body: Some(body),
            named: None,
            is_temp: true,
            create_new_scope: Some(false),
            define: false,
        }
        .lower(env, scope, span)
    }
}
