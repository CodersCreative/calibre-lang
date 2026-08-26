use crate::{
    ast::{MiddleNode, MiddleNodeType},
    environment::MiddleEnvironment,
    errors::MiddleErr,
    scoping::ScopeId,
    symbols::{FunctionParamDefault, resolve::ResolutionOptions},
    tags::TagInfo,
    typing::{MiddleObject, MiddleTypeDefType},
};
use calibre_parser::{
    Span,
    ast::{
        idents::{ParserText, PotentialDollarIdentifier, PotentialGenericTypeIdentifier},
        nodes::{AsFailureMode, AstNode, AstNodeType, Overload, TypeDefType, VarType},
        types::{ParserDataType, ParserInnerType},
    },
};
use rustc_hash::FxHashMap;
use tracing::instrument;

impl MiddleEnvironment {
    #[instrument(skip_all, fields(scope, identifier))]
    pub fn evaluate_var_declaration(
        &mut self,
        scope: ScopeId,
        span: Span,
        var_type: VarType,
        identifier: PotentialDollarIdentifier,
        mut value: AstNode,
        data_type: ParserDataType,
    ) -> Result<MiddleNode, MiddleErr> {
        let identifier = self.resolve(
            scope,
            &identifier,
            ResolutionOptions::default().with_dollar(),
        )?;

        let new_name = ParserText::temp_name_with_suffix(identifier.trim(), span).text;

        if let AstNodeType::CallExpression {
            caller,
            generic_types,
            args,
            reverse_args,
            ..
        } = value.clone().node_type
            && let AstNodeType::Identifier(callee_ident) = &caller.node_type
            && callee_ident.to_string() == identifier
            && let Some(first_arg) = args.first().cloned().map(|a| -> AstNode { a.into() })
        {
            let first_ty = self.resolve_type_from_node(scope, &first_arg).or_else(|| {
                match &first_arg.node_type {
                    AstNodeType::RefStatement { value, .. } => {
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
                value = AstNode::new(
                    value.span,
                    AstNodeType::CallExpression {
                        string_fn: None,
                        caller: Box::new(AstNode::identifier(value.span, mapped_name)),
                        generic_types,
                        args,
                        reverse_args,
                    },
                );
            }
        }

        let original_value_node = value.clone();

        let function_decl = match &original_value_node.node_type {
            AstNodeType::FunctionDeclaration { header, body, .. } => Some((header, body)),
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
            self.symbols
                .generic_fn_templates
                .entry(base_name)
                .or_insert((template_params, (*header).clone(), (**body).clone()));
        }

        if let Some((header, _)) = function_decl {
            for tag in &self.tagging.tag_info {
                match tag {
                    TagInfo::Init(priority) => self
                        .tagging
                        .init_functions
                        .push((*priority, new_name.clone())),
                    TagInfo::Fin(priority) => self
                        .tagging
                        .fin_functions
                        .push((*priority, new_name.clone())),
                    _ => {}
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
                            Some(ParserDataType {
                                data_type: ParserInnerType::Option(_),
                                ..
                            })
                        ),
                })
                .collect();

            self.symbols
                .function_param_defaults
                .insert(new_name.clone(), defaults.clone());
            self.symbols
                .function_param_defaults
                .insert(identifier.clone(), defaults);
        }

        let node_ty = self.resolve_type_from_node(scope, &value);

        let data_type = if data_type.is_auto() {
            None
        } else {
            Some(self.resolve_data_type(scope, &data_type, ResolutionOptions::typing())?)
        };

        let data_type = match (data_type, node_ty) {
            (None, None) => return Err(self.context.err_at_current(MiddleErr::InferImpossible)),
            (Some(x), None) => x,
            (None, Some(x)) => x,
            (Some(x), Some(_)) if self.tagging.tag_info.contains(&TagInfo::IgnoreInvalidLet) => x,
            (Some(x), Some(y)) => {
                if x.loose_eq(&y) {
                    x
                } else {
                    return Err(self.context.err_at_current(
                        MiddleErr::InvalidVarDeclarationType {
                            expected: Box::new(x),
                            found: Box::new(y),
                        },
                    ));
                }
            }
        };

        let mut value = if let Some((header, _)) = function_decl {
            self.register_variable(
                scope,
                &identifier,
                new_name.clone(),
                data_type.clone(),
                var_type,
            )?;

            let new_scope = self.scoping.new_scope_from_parent_shallow(scope);

            for param in header.parameters.iter() {
                let og_name =
                    self.resolve(scope, &param.0, ResolutionOptions::default().with_dollar())?;

                let new_name = ParserText::temp_name_with_suffix(og_name.trim(), span);

                let data_type = if let Some(x) = param.1.clone() {
                    self.resolve_data_type(scope, &x, ResolutionOptions::typing())?
                } else if let Some(node) = &param.2 {
                    self.resolve_type_from_node(scope, node)
                        .ok_or_else(|| self.context.err_at_current(MiddleErr::InferImpossible))?
                } else {
                    return Err(self.context.err_at_current(MiddleErr::InferImpossible));
                };

                self.register_variable(
                    new_scope,
                    &og_name,
                    new_name.text.clone(),
                    data_type.clone(),
                    VarType::Mutable,
                )?;
            }

            self.evaluate(new_scope, value)
        } else {
            self.evaluate(scope, value)
        };

        if matches!(data_type.data_type, ParserInnerType::DynamicTraits(_)) {
            value = MiddleNode::new(
                MiddleNodeType::AsExpression {
                    value: Box::new(value),
                    data_type: data_type.clone(),
                    failure_mode: AsFailureMode::Panic,
                },
                span,
            );
        }

        if !matches!(
            original_value_node.node_type,
            AstNodeType::FunctionDeclaration { .. }
        ) {
            self.register_variable(
                scope,
                &identifier,
                new_name.clone(),
                data_type.clone(),
                var_type,
            )?;
        }

        Ok(MiddleNode {
            node_type: MiddleNodeType::VariableDeclaration {
                var_type,
                identifier: ParserText {
                    text: new_name,
                    span,
                },
                value: Box::new(value),
                data_type,
            },
            span,
        })
    }

    #[instrument(skip_all, fields(scope, identifier))]
    pub fn evaluate_type_declaration(
        &mut self,
        scope: ScopeId,
        span: Span,
        identifier: PotentialGenericTypeIdentifier,
        object: TypeDefType,
        overloads: Vec<Overload>,
    ) -> Result<MiddleNode, MiddleErr> {
        let mut has_default = false;
        let mut has_builder = false;

        for tag in &self.tagging.tag_info {
            if has_builder && has_default {
                break;
            }

            match tag {
                TagInfo::Builder => {
                    has_builder = true;
                }
                TagInfo::Default => {
                    has_default = true;
                }
                _ => {}
            }
        }

        // TODO Work on NewTypes
        if let TypeDefType::NewType(inner) = &object {
            let generic_params: Vec<String> = match &identifier {
                PotentialGenericTypeIdentifier::Generic { generic_types, .. } => generic_types
                    .iter()
                    .filter_map(|t| match t {
                        ParserDataType {
                            data_type: ParserInnerType::Struct(s),
                            ..
                        } => Some(s.clone()),
                        _ => None,
                    })
                    .collect(),
                _ => Vec::new(),
            };

            let identifier = self.resolve(
                scope,
                &identifier,
                ResolutionOptions::default().with_dollar(),
            )?;

            let inner =
                self.resolve_data_type(scope, inner.as_ref(), ResolutionOptions::typing())?;

            let target_name = if identifier == inner.impl_name() {
                Some(identifier.clone())
            } else {
                None
            };

            {
                let scope_ref = self.scoping.scope_mut_or_err(scope)?;

                scope_ref
                    .type_mappings
                    .insert(identifier.to_string(), inner.data_type);
            }

            if !overloads.is_empty() {
                for overload in overloads {
                    if let Some(processed) = self.process_overload(
                        scope,
                        overload,
                        generic_params.clone(),
                        target_name.clone(),
                    )? {
                        self.symbols.overloads.push(processed);
                    }
                }
            }

            return Ok(MiddleNode {
                node_type: MiddleNodeType::EmptyLine,
                span,
            });
        }

        let ident = self.resolve(
            scope,
            &identifier,
            ResolutionOptions::default().with_dollar(),
        )?;

        let generic_params = if let PotentialGenericTypeIdentifier::Generic {
            identifier: _,
            generic_types,
        } = identifier.clone()
        {
            let template_params: Vec<String> = generic_types
                .iter()
                .filter_map(|t| match t {
                    ParserDataType {
                        data_type: ParserInnerType::Struct(s),
                        ..
                    } => Some(s.clone()),
                    _ => None,
                })
                .collect();

            self.typing
                .generic_type_templates
                .entry(ident.clone())
                .or_insert((template_params, object.clone(), overloads.clone()));

            self.typing
                .generic_type_templates
                .get(&ident)
                .map(|(params, _, _)| params.clone())
                .unwrap_or_default()
        } else {
            Vec::new()
        };

        let new_name = ParserText::temp_name_with_suffix(ident.trim(), span).text;

        let object = MiddleTypeDefType::from_type_def_type(self, scope, object.clone());

        has_default = has_default
            || match &object {
                MiddleTypeDefType::Enum {
                    default_variant, ..
                } => default_variant.is_some(),
                MiddleTypeDefType::Struct(_) => false,
                _ => false,
            };

        let default_ident = self.resolve(scope, &"Default", ResolutionOptions::all());
        self.typing.objects.insert(
            new_name.clone(),
            MiddleObject {
                object_type: object.clone(),
                variables: FxHashMap::default(),
                traits: if let Ok(x) = default_ident
                    && has_default
                {
                    vec![x]
                } else {
                    Vec::new()
                },
                location: self.context.current_location.clone(),
            },
        );

        let previous_self_type = {
            let scope = self.scoping.scope_mut_or_err(scope)?;

            scope
                .type_mappings
                .insert(ident.clone(), ParserInnerType::Struct(new_name.clone()));

            scope.type_mappings.insert(
                String::from("Self"),
                ParserInnerType::Struct(new_name.clone()),
            )
        };

        let identifier = ParserText::new(span, ident);

        let default_node = if has_default {
            Some(self.generate_default_impl(scope, span, identifier.clone(), object.clone())?)
        } else {
            None
        };

        let builder_nodes = if has_builder {
            Some(self.generate_builder(
                scope,
                span,
                identifier.clone(),
                object.clone(),
                has_default,
            )?)
        } else {
            None
        };

        for overload in overloads {
            if let Some(processed) = self.process_overload(
                scope,
                overload,
                generic_params.clone(),
                Some(new_name.clone()),
            )? {
                self.symbols.overloads.push(processed);
            }
        }

        {
            let scope = self.scoping.scope_mut_or_err(scope)?;

            if let Some(prev) = previous_self_type {
                scope.type_mappings.insert(String::from("Self"), prev);
            }
        }

        match (default_node, builder_nodes) {
            (Some(node), None) => Ok(node),
            (None, None) => Ok(MiddleNode {
                node_type: MiddleNodeType::EmptyLine,
                span,
            }),
            (None, Some(nodes)) => Ok(MiddleNode {
                node_type: MiddleNodeType::ScopeDeclaration {
                    body: vec![nodes.0, nodes.1],
                    create_new_scope: false,
                    is_temp: false,
                    scope_id: scope,
                },
                span,
            }),
            (Some(node), Some(nodes)) => Ok(MiddleNode {
                node_type: MiddleNodeType::ScopeDeclaration {
                    body: vec![node, nodes.0, nodes.1],
                    create_new_scope: false,
                    is_temp: false,
                    scope_id: scope,
                },
                span,
            }),
        }
    }
}
