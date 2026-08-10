use crate::{
    ast::{MiddleNode, MiddleNodeType},
    environment::MiddleEnvironment,
    errors::MiddleErr,
    symbols::FunctionParamDefault,
    tags::TagInfo,
    typing::{MiddleObject, MiddleTypeDefType},
};
use calibre_parser::{
    Span,
    ast::{
        idents::{ParserText, PotentialDollarIdentifier, PotentialGenericTypeIdentifier},
        nodes::{AsFailureMode, Node, NodeType, Overload, TypeDefType, VarType},
        types::{ParserDataType, ParserInnerType, PotentialNewType},
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
        mut value: Node,
        data_type: PotentialNewType,
    ) -> Result<MiddleNode, MiddleErr> {
        let identifier = self
            .resolve_dollar_ident_only(scope, &identifier)
            .ok_or_else(|| {
                self.context
                    .err_at_current(MiddleErr::Scope(identifier.to_string()))
            })?;

        let new_name = ParserText::temp_name_with_prefix(identifier.text.trim(), span).text;

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
                            Some(PotentialNewType::DataType(ParserDataType {
                                data_type: ParserInnerType::Option(_),
                                ..
                            }))
                        ),
                })
                .collect();

            self.symbols
                .function_param_defaults
                .insert(new_name.clone(), defaults.clone());
            self.symbols
                .function_param_defaults
                .insert(identifier.text.clone(), defaults);
        }

        let data_type = if data_type.is_auto() {
            let err = self.context.err_at_current(MiddleErr::InferImpossible);
            self.resolve_type_from_node(scope, &value).ok_or(err)?
        } else {
            self.resolve_potential_new_type(scope, data_type)
        };

        let mut value = if let Some((header, _)) = function_decl {
            self.register_variable(
                scope,
                &identifier.text,
                new_name.clone(),
                data_type.clone(),
                var_type,
            )?;

            let new_scope = self.scoping.new_scope_from_parent_shallow(*scope);

            for param in header.parameters.iter() {
                let og_name = self
                    .resolve_dollar_ident_only(scope, &param.0)
                    .ok_or_else(|| {
                        self.context
                            .err_at_current(MiddleErr::Scope(param.0.to_string()))
                    })?;

                let new_name = ParserText::temp_name_with_prefix(og_name.trim(), span);

                let data_type = if let Some(x) = param.1.clone() {
                    self.resolve_potential_new_type(scope, x)
                } else if let Some(node) = &param.2 {
                    self.resolve_type_from_node(scope, node)
                        .ok_or(MiddleErr::InferImpossible)?
                } else {
                    return Err(MiddleErr::InferImpossible);
                };

                self.register_variable(
                    &new_scope,
                    &og_name.text,
                    new_name.text.clone(),
                    data_type.clone(),
                    VarType::Mutable,
                )?;

                self.scoping
                    .scope_mut_or_err(&new_scope)?
                    .defined
                    .push(new_name.text.clone());
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
                    failure_mode: AsFailureMode::Panic,
                },
                span,
            );
        }

        if !matches!(
            original_value_node.node_type,
            NodeType::FunctionDeclaration { .. }
        ) {
            self.register_variable(
                scope,
                &identifier.text,
                new_name.clone(),
                data_type.clone(),
                var_type.clone(),
            )?;
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

    pub fn evaluate_type_declaration(
        &mut self,
        scope: &u64,
        span: Span,
        identifier: PotentialGenericTypeIdentifier,
        object: TypeDefType,
        overloads: Vec<Overload>,
    ) -> Result<MiddleNode, MiddleErr> {
        let mut has_default = false;

        for tag in &self.tagging.tag_info {
            match tag {
                TagInfo::Default => {
                    has_default = true;
                    break;
                }
                _ => {}
            }
        }

        if let TypeDefType::NewType(inner) = &object {
            let identifier_text = identifier.to_string();
            let is_overload_auto = !overloads.is_empty()
                && matches!(
                    inner.as_ref(),
                    PotentialNewType::DataType(dt) if dt.is_auto()
                );
            let is_overload_self = !is_overload_auto
                && !overloads.is_empty()
                && matches!(
                    inner.as_ref(),
                    PotentialNewType::DataType(dt) if dt.to_string() == identifier_text
                );
            let mut generic_params: Vec<String> = match &identifier {
                PotentialGenericTypeIdentifier::Generic { generic_types, .. } => generic_types
                    .iter()
                    .filter_map(|t| match t {
                        PotentialNewType::DataType(ParserDataType {
                            data_type: ParserInnerType::Struct(s),
                            ..
                        }) => Some(s.clone()),
                        _ => None,
                    })
                    .collect(),
                _ => Vec::new(),
            };
            if generic_params.is_empty()
                && let Some(start) = identifier_text.find('<')
                && let Some(end) = identifier_text.rfind('>')
                && end > start + 1
            {
                let inner = &identifier_text[start + 1..end];
                for raw in inner.split(',') {
                    let mut name = raw.trim().to_string();
                    if let Some(idx) = name.find('<') {
                        name = name[..idx].trim().to_string();
                    }
                    if !name.is_empty() {
                        generic_params.push(name);
                    }
                }
            }

            let identifier = self
                .resolve_dollar_ident_potential_generic_only(scope, &identifier)
                .unwrap_or_else(|| ParserText::from(identifier_text.clone()));
            if !is_overload_self && !is_overload_auto {
                let resolved = self.resolve_potential_new_type(scope, *inner.clone());
                let resolved_name = resolved.data_type.to_string();
                let is_self_alias = identifier.text == resolved_name;

                let is_builtin_alias = matches!(
                    resolved.data_type,
                    ParserInnerType::Int
                        | ParserInnerType::UInt
                        | ParserInnerType::Float
                        | ParserInnerType::Bool
                        | ParserInnerType::Str
                        | ParserInnerType::Char
                        | ParserInnerType::Range
                        | ParserInnerType::Dynamic
                        | ParserInnerType::DynamicTraits(_)
                        | ParserInnerType::Null
                        | ParserInnerType::Auto(_)
                );

                if !is_builtin_alias && !is_self_alias {
                    self.typing
                        .type_aliases
                        .insert(identifier.text.clone(), resolved.clone());
                }
            }

            self.scoping
                .scopes
                .get_mut(scope)
                .ok_or_else(|| {
                    MiddleErr::At(
                        span,
                        Box::new(MiddleErr::Internal(format!("missing scope {scope}"))),
                    )
                })?
                .mappings
                .insert(identifier.text.clone(), identifier.text.clone());

            if !overloads.is_empty() {
                for overload in overloads {
                    if let Some(processed) =
                        self.process_overload(scope, overload, generic_params.clone(), None)?
                    {
                        self.symbols.overloads.push(processed);
                    }
                }
            }

            return Ok(MiddleNode {
                node_type: MiddleNodeType::EmptyLine,
                span,
            });
        }

        if let PotentialGenericTypeIdentifier::Generic {
            identifier: base_ident,
            generic_types,
        } = identifier.clone()
        {
            let base_ident = self
                .resolve_dollar_ident_only(scope, &base_ident)
                .ok_or_else(|| {
                    MiddleErr::At(span, Box::new(MiddleErr::Scope(base_ident.to_string())))
                })?;
            let template_params: Vec<String> = generic_types
                .iter()
                .filter_map(|t| match t {
                    PotentialNewType::DataType(ParserDataType {
                        data_type: ParserInnerType::Struct(s),
                        ..
                    }) => Some(s.clone()),
                    _ => None,
                })
                .collect();

            self.typing
                .generic_type_templates
                .entry(base_ident.text.clone())
                .or_insert((template_params, object.clone(), overloads.clone()));

            let generic_params = self
                .typing
                .generic_type_templates
                .get(&base_ident.text)
                .map(|(params, _, _)| params.clone())
                .unwrap_or_default();

            for overload in overloads {
                if let Some(processed) =
                    self.process_overload(scope, overload, generic_params.clone(), None)?
                {
                    self.symbols.overloads.push(processed);
                }
            }

            self.scoping
                .scopes
                .get_mut(scope)
                .ok_or_else(|| {
                    MiddleErr::At(
                        span,
                        Box::new(MiddleErr::Internal(format!("missing scope {scope}"))),
                    )
                })?
                .mappings
                .insert(base_ident.text.clone(), base_ident.text.clone());

            return Ok(MiddleNode {
                node_type: MiddleNodeType::EmptyLine,
                span,
            });
        }

        let identifier = self
            .resolve_dollar_ident_potential_generic_only(scope, &identifier)
            .ok_or_else(|| {
                MiddleErr::At(span, Box::new(MiddleErr::Scope(identifier.to_string())))
            })?;

        let new_name = ParserText::temp_name_with_prefix(identifier.text.trim(), span).text;

        let object = MiddleTypeDefType::from_type_def_type(self, scope, object.clone());

        has_default = has_default
            || match &object {
                MiddleTypeDefType::Enum {
                    default_variant, ..
                } => default_variant.is_some(),
                MiddleTypeDefType::Struct(_) => false,
                _ => false,
            };

        self.typing.objects.insert(
            new_name.clone(),
            MiddleObject {
                object_type: object.clone(),
                variables: FxHashMap::default(),
                traits: if has_default {
                    vec!["Default".to_string()]
                } else {
                    Vec::new()
                },
                location: self.context.current_location.clone(),
            },
        );

        self.scoping
            .scopes
            .get_mut(scope)
            .ok_or_else(|| {
                MiddleErr::At(
                    span,
                    Box::new(MiddleErr::Internal(format!("missing scope {scope}"))),
                )
            })?
            .mappings
            .insert(identifier.text.clone(), new_name.clone());

        let previous_self = self
            .scoping
            .scopes
            .get_mut(scope)
            .ok_or_else(|| {
                MiddleErr::At(
                    span,
                    Box::new(MiddleErr::Internal(format!("missing scope {scope}"))),
                )
            })?
            .mappings
            .insert(String::from("Self"), new_name.clone());

        let default_node = if has_default {
            Some(self.generate_default_impl(scope, span, identifier.clone(), object.clone())?)
        } else {
            None
        };

        for overload in overloads {
            if let Some(processed) =
                self.process_overload(scope, overload, Vec::new(), Some(new_name.clone()))?
            {
                self.symbols.overloads.push(processed);
            }
        }

        if let Some(prev) = previous_self {
            self.scoping
                .scopes
                .get_mut(scope)
                .ok_or_else(|| {
                    MiddleErr::At(
                        span,
                        Box::new(MiddleErr::Internal(format!("missing scope {scope}"))),
                    )
                })?
                .mappings
                .insert(String::from("Self"), prev);
        }

        if let Some(node) = default_node {
            Ok(node)
        } else {
            Ok(MiddleNode {
                node_type: MiddleNodeType::EmptyLine,
                span,
            })
        }
    }
}
