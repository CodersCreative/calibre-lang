use crate::{
    ast::{MiddleNode, MiddleNodeType},
    environment::{
        FunctionParamDefault, MiddleEnvironment, MiddleVariable, get_disamubiguous_name,
    },
    errors::MiddleErr,
};
use calibre_parser::{
    Span,
    ast::{
        Node, NodeType, ParserDataType, ParserInnerType, ParserText, PotentialDollarIdentifier,
        PotentialNewType, VarType,
    },
};

impl MiddleEnvironment {
    pub fn evaluate_var_declaration(
        &mut self,
        scope: &u64,
        span: Span,
        var_type: VarType,
        identifier: PotentialDollarIdentifier,
        mut value: Node,
        data_type: PotentialNewType,
    ) -> Result<MiddleNode, MiddleErr> {
        let identifier = self
            .resolve_dollar_ident_only(scope, &identifier)
            .ok_or_else(|| self.err_at_current(MiddleErr::Scope(identifier.to_string())))?;

        let new_name = if identifier.text.contains("->") || identifier.text.contains("::") {
            identifier.text.clone()
        } else {
            get_disamubiguous_name(scope, Some(identifier.text.trim()), Some(&var_type))
        };

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
                        caller: Box::new(Node::identifier(value.span, mapped_name)),
                        generic_types,
                        args,
                        reverse_args,
                    },
                );
            }
        }

        let original_value_node = value.clone();

        let function_decl = match &original_value_node.node_type {
            NodeType::FunctionDeclaration { header, body, .. } => Some((header, body)),
            _ => None,
        };
        if let Some((header, body)) = function_decl
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
                (*header).clone(),
                (**body).clone(),
            ));
        }

        if let Some((header, _)) = function_decl {
            for (tag_name, priority) in &self.current_tag_info {
                if tag_name == "init" {
                    self.init_functions.push((*priority, new_name.clone()));
                } else if tag_name == "fin" {
                    self.fin_functions.push((*priority, new_name.clone()));
                }
            }

            let defaults: Vec<FunctionParamDefault> = header
                .parameters
                .iter()
                .map(|(name, declared_ty, default)| FunctionParamDefault {
                    name: name.to_string(),
                    explicit_default: default
                        .clone()
                        .map(|node| Box::new(self.evaluate(scope, *node)))
                        .map(|x| *x),
                    implicit_none: default.is_none()
                        && matches!(
                            declared_ty,
                            Some(PotentialNewType::DataType(ParserDataType {
                                data_type: ParserInnerType::Option(_),
                                ..
                            }))
                        ),
                })
                .collect();

            self.function_param_defaults
                .insert(new_name.clone(), defaults.clone());
            self.function_param_defaults
                .insert(identifier.text.clone(), defaults);
        }

        let current_location = self.current_location.clone();

        let data_type = if data_type.is_auto() {
            let err = self.err_at_current(MiddleErr::InferImpossible);
            self.resolve_type_from_node(scope, &value).ok_or(err)?
        } else {
            self.resolve_potential_new_type(scope, data_type)
        };

        let mut value = if let Some((header, _)) = function_decl {
            self.variables.insert(
                new_name.clone(),
                MiddleVariable {
                    data_type: data_type.clone(),
                    var_type,
                    location: current_location.clone(),
                },
            );

            let err = self.err_at_current(MiddleErr::Scope(scope.to_string()));
            self.scopes
                .get_mut(scope)
                .ok_or(err)?
                .mappings
                .insert(identifier.text.clone(), new_name.clone());

            let new_scope = self.new_scope_from_parent_shallow(*scope);

            for param in header.parameters.iter() {
                let og_name = self
                    .resolve_dollar_ident_only(scope, &param.0)
                    .ok_or_else(|| self.err_at_current(MiddleErr::Scope(param.0.to_string())))?;

                let new_name =
                    get_disamubiguous_name(scope, Some(og_name.trim()), Some(&VarType::Mutable));

                let data_type = if let Some(x) = param.1.clone() {
                    self.resolve_potential_new_type(scope, x)
                } else if let Some(node) = &param.2 {
                    self.resolve_type_from_node(scope, node)
                        .ok_or(MiddleErr::InferImpossible)?
                } else {
                    return Err(MiddleErr::InferImpossible);
                };

                self.variables.insert(
                    new_name.clone(),
                    MiddleVariable {
                        data_type: data_type.clone(),
                        var_type: VarType::Mutable,
                        location: current_location.clone(),
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

        if matches!(data_type.data_type, ParserInnerType::DynamicTraits(_)) {
            value = MiddleNode::new(
                MiddleNodeType::AsExpression {
                    value: Box::new(value),
                    data_type: data_type.clone(),
                    failure_mode: calibre_parser::ast::AsFailureMode::Panic,
                },
                span,
            );
        }

        if !matches!(
            original_value_node.node_type,
            NodeType::FunctionDeclaration { .. }
        ) {
            self.variables.insert(
                new_name.clone(),
                MiddleVariable {
                    data_type: data_type.clone(),
                    var_type: var_type.clone(),
                    location: current_location.clone(),
                },
            );

            let err = self.err_at_current(MiddleErr::Scope(scope.to_string()));
            self.scopes
                .get_mut(scope)
                .ok_or(err)?
                .mappings
                .insert(identifier.text.clone(), new_name.clone());
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
