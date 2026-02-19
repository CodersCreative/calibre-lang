use crate::{
    ast::{
        MiddleNode, MiddleNodeType,
        hm::{self, Type, TypeGenerator, TypeScheme},
    },
    environment::{MiddleEnvironment, MiddleVariable, get_disamubiguous_name},
    errors::MiddleErr,
    infer::infer_node_hm,
};
use calibre_parser::{
    Span,
    ast::{
        Node, NodeType, ParserDataType, ParserInnerType, ParserText, PotentialDollarIdentifier,
        PotentialNewType, VarType,
    },
};
use rustc_hash::FxHashMap;

impl MiddleEnvironment {
    pub fn evaluate_var_declaration(
        &mut self,
        scope: &u64,
        span: Span,
        var_type: VarType,
        identifier: PotentialDollarIdentifier,
        value: Node,
        data_type: PotentialNewType,
    ) -> Result<MiddleNode, MiddleErr> {
        let identifier = self
            .resolve_dollar_ident_only(scope, &identifier)
            .ok_or_else(|| self.err_at_current(MiddleErr::Scope(identifier.to_string())))?;

        let is_reserved_constructor =
            matches!(identifier.text.as_str(), "ok" | "err" | "some" | "none");

        let new_name = if let Some(existing) = self
            .scopes
            .get(scope)
            .and_then(|s| s.mappings.get(&identifier.text))
        {
            let existing_is_native = self.variables.get(existing).is_some_and(|var| {
                matches!(var.data_type.data_type, ParserInnerType::NativeFunction(_))
            });
            if is_reserved_constructor && existing_is_native {
                get_disamubiguous_name(scope, Some(identifier.text.trim()), Some(&var_type))
            } else {
                existing.clone()
            }
        } else if identifier.text.contains("->") {
            identifier.text.clone()
        } else {
            get_disamubiguous_name(scope, Some(identifier.text.trim()), Some(&var_type))
        };

        let mut value = value;

        if let NodeType::CallExpression {
            caller,
            generic_types,
            args,
            reverse_args,
            ..
        } = value.clone().node_type
            && let NodeType::Identifier(callee_ident) = &caller.node_type
            && callee_ident.to_string() == identifier.text
            && let Some(first_arg) = args.first().cloned().map(|a| -> Node { a.into() })
        {
            let first_ty = self.resolve_type_from_node(scope, &first_arg).or_else(|| {
                match &first_arg.node_type {
                    NodeType::RefStatement { value, .. } => {
                        self.resolve_type_from_node(scope, value.as_ref())
                    }
                    _ => None,
                }
            });
            if let Some(first_ty) = first_ty
                && let Some(mapped_name) = self
                    .resolve_member_fn_name(&first_ty.unwrap_all_refs(), &callee_ident.to_string())
                && mapped_name != callee_ident.to_string()
            {
                value = Node::new(
                    value.span,
                    NodeType::CallExpression {
                        string_fn: None,
                        caller: Box::new(Node::new(
                            value.span,
                            NodeType::Identifier(ParserText::from(mapped_name).into()),
                        )),
                        generic_types,
                        args,
                        reverse_args,
                    },
                );
            }
        }

        let original_value_node = value.clone();

        if let NodeType::FunctionDeclaration {
            ref header,
            ref body,
            ..
        } = original_value_node.node_type
            && !header.generics.0.is_empty()
        {
            let base_name = new_name.clone();
            let template_params: Vec<String> = header
                .generics
                .0
                .iter()
                .map(|g| g.identifier.to_string())
                .collect();
            self.generic_fn_templates.entry(base_name).or_insert((
                template_params,
                header.clone(),
                (**body).clone(),
            ));
        }

        let is_function_decl = matches!(
            original_value_node.node_type,
            NodeType::FunctionDeclaration { .. }
        );

        let mut data_type = if data_type.is_auto() {
            let inferred = self.resolve_type_from_node(scope, &value).or_else(|| {
                if let NodeType::CallExpression { caller, .. } = &value.node_type
                    && let NodeType::MemberExpression { path } = &caller.node_type
                    && path.len() >= 2
                    && let Some((last, _)) = path.last()
                    && let NodeType::Identifier(ident) = &last.node_type
                    && ident.to_string() == "new"
                {
                    let receiver_path = path[..path.len() - 1].to_vec();
                    let receiver = Node::new(
                        span,
                        NodeType::MemberExpression {
                            path: receiver_path,
                        },
                    );
                    self.resolve_type_from_node(scope, &receiver)
                } else {
                    None
                }
            });

            inferred.unwrap_or(self.resolve_potential_new_type(scope, data_type))
        } else {
            self.resolve_potential_new_type(scope, data_type)
        };

        if let NodeType::FunctionDeclaration { ref header, .. } = original_value_node.node_type {
            data_type = ParserDataType::new(
                span,
                ParserInnerType::Function {
                    return_type: Box::new(
                        self.resolve_potential_new_type(scope, header.return_type.clone()),
                    ),
                    parameters: header
                        .parameters
                        .iter()
                        .map(|(_, t)| self.resolve_potential_new_type(scope, t.clone()))
                        .collect(),
                },
            );
        }

        if let NodeType::FunctionDeclaration { ref header, .. } = original_value_node.node_type {
            let mut tg = TypeGenerator::default();
            let ret_pd = self.resolve_potential_new_type(scope, header.return_type.clone());

            let mut ret_hm = if matches!(ret_pd.data_type, ParserInnerType::Auto(_)) {
                tg.fresh()
            } else {
                hm::from_parser_data_type(&ret_pd, &mut tg)
            };

            for param in header.parameters.iter().rev() {
                let p_pd = self.resolve_potential_new_type(scope, param.1.clone());
                let p_hm = if matches!(p_pd.data_type, ParserInnerType::Auto(_)) {
                    tg.fresh()
                } else {
                    hm::from_parser_data_type(&p_pd, &mut tg)
                };

                ret_hm = Type::TArrow(std::sync::Arc::new(p_hm), std::sync::Arc::new(ret_hm));
            }

            if !self.hm_env.contains_key(&new_name) {
                self.hm_env
                    .insert(new_name.clone(), TypeScheme::new(Vec::new(), ret_hm));
            }
        }

        let mut value = if let NodeType::FunctionDeclaration { ref header, .. } =
            original_value_node.node_type
        {
            self.variables.insert(
                new_name.clone(),
                MiddleVariable {
                    data_type: data_type.clone(),
                    var_type: var_type.clone(),
                    location: self.current_location.clone(),
                },
            );

            let err = self.err_at_current(MiddleErr::Scope(scope.to_string()));
            let scope_ref = self.scopes.get_mut(scope).ok_or(err)?;
            let existing_is_native = scope_ref
                .mappings
                .get(&identifier.text)
                .and_then(|name| self.variables.get(name))
                .is_some_and(|var| matches!(var.data_type.data_type, ParserInnerType::NativeFunction(_)));
            if !(is_reserved_constructor && existing_is_native) {
                scope_ref
                    .mappings
                    .insert(identifier.text.clone(), new_name.clone());
            }

            let new_scope = self.new_scope_from_parent_shallow(*scope);

            for param in header.parameters.iter() {
                let og_name = self
                    .resolve_dollar_ident_only(scope, &param.0)
                    .ok_or_else(|| self.err_at_current(MiddleErr::Scope(param.0.to_string())))?;

                let new_name =
                    get_disamubiguous_name(scope, Some(og_name.trim()), Some(&VarType::Mutable));

                let data_type = self.resolve_potential_new_type(scope, param.1.clone());

                self.variables.insert(
                    new_name.clone(),
                    MiddleVariable {
                        data_type: data_type.clone(),
                        var_type: VarType::Mutable,
                        location: self.current_location.clone(),
                    },
                );

                let err = self.err_at_current(MiddleErr::Scope(new_scope.to_string()));
                let scope_ref = self.scopes.get_mut(&new_scope).ok_or(err)?;
                scope_ref
                    .mappings
                    .insert(og_name.text.clone(), new_name.clone());
                scope_ref.defined.push(new_name.clone());
            }

            self.evaluate(&new_scope, value)
        } else {
            self.evaluate(scope, value)
        };

        if !matches!(
            original_value_node.node_type,
            NodeType::FunctionDeclaration { .. }
        ) {
            self.variables.insert(
                new_name.clone(),
                MiddleVariable {
                    data_type: data_type.clone(),
                    var_type: var_type.clone(),
                    location: self.current_location.clone(),
                },
            );

            let err = self.err_at_current(MiddleErr::Scope(scope.to_string()));
            let scope_ref = self.scopes.get_mut(scope).ok_or(err)?;
            let existing_is_native = scope_ref
                .mappings
                .get(&identifier.text)
                .and_then(|name| self.variables.get(name))
                .is_some_and(|var| matches!(var.data_type.data_type, ParserInnerType::NativeFunction(_)));
            if !(is_reserved_constructor && existing_is_native) {
                scope_ref.mappings.insert(identifier.text, new_name.clone());
            }
        }

        if data_type.contains_auto()
            && !matches!(original_value_node.node_type, NodeType::InlineGenerator { .. })
            && let Some((hm_t, subst)) = infer_node_hm(self, scope, &original_value_node)
        {
            let t_applied = hm::apply_subst(&subst, &hm_t);

            let mut tenv: FxHashMap<String, hm::TypeScheme> = FxHashMap::default();
            for (k, s) in self.hm_env.iter() {
                tenv.insert(k.clone(), s.clone());
            }

            let parser_ty = hm::to_parser_data_type(&t_applied, &mut self.type_cache);
            let scheme = match &parser_ty.data_type {
                calibre_parser::ast::ParserInnerType::Function { .. }
                    if parser_ty.contains_auto() =>
                {
                    hm::TypeScheme::new(Vec::new(), t_applied.clone())
                }
                _ => hm::generalize(&tenv, &t_applied),
            };
            self.hm_env.insert(new_name.clone(), scheme);

            if let Some(v) = self.variables.get_mut(&new_name) {
                if !is_function_decl {
                    v.data_type = parser_ty.clone();
                    data_type = parser_ty.clone();

                    if let MiddleNodeType::FunctionDeclaration {
                        parameters: ref mut params,
                        return_type: ref mut ret_type,
                        ..
                    } = value.node_type
                        && let ParserInnerType::Function {
                            return_type: inferred_ret,
                            parameters: inferred_params,
                        } = parser_ty.data_type
                    {
                        for (i, (_name, p_ty)) in params.iter_mut().enumerate() {
                            if i < inferred_params.len() && p_ty.contains_auto() {
                                *p_ty = inferred_params[i].clone();
                            }
                        }

                        if ret_type.contains_auto() {
                            *ret_type = *inferred_ret.clone();
                        }
                    }
                }
            }
        }

        Ok(MiddleNode {
            node_type: MiddleNodeType::VariableDeclaration {
                var_type,
                identifier: ParserText {
                    text: new_name,
                    span: identifier.span,
                },
                value: Box::new(value),
                data_type,
            },
            span,
        })
    }
}
