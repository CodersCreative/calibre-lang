use crate::{
    ast::{MiddleNode, MiddleNodeType, MirScopeDecl, MirVarDecl},
    environment::MiddleEnvironment,
    errors::MiddleErr,
    scoping::ScopeId,
    symbols::resolve::ResolutionOptions,
    tags::TagInfo,
    translate::MirLowering,
    typing::{
        MiddleImplMember, MiddleObject, MiddleTrait, MiddleTraitMember, MiddleTypeDefType, Typing,
    },
};
use calibre_parser::{
    Span,
    ast::{
        idents::{ParserText, PotentialDollarIdentifier, PotentialGenericTypeIdentifier},
        nodes::{
            AstNode, AstNodeType, VarType,
            declaration::AstDeclaration,
            functions::AstFunction,
            misc::AstTag,
            types::{AstImpl, AstImplTrait, AstTrait, AstType, TraitMemberKind, TypeDefType},
        },
        types::{GenericTypes, ParserDataType, ParserInnerType},
    },
};
use tracing::instrument;
use ustr::{Ustr, UstrMap, UstrSet};

impl MirLowering for AstType {
    #[instrument(skip_all, fields(scope, identifier))]
    fn lower(
        self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        span: Span,
    ) -> Result<MiddleNode, MiddleErr> {
        let mut has_default = false;
        let mut has_builder = false;

        for tag in &env.tagging.tag_info {
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
        if let TypeDefType::NewType(inner) = &self.object {
            let generic_params: Vec<Ustr> = match &self.identifier {
                PotentialGenericTypeIdentifier::Generic { generic_types, .. } => generic_types
                    .iter()
                    .filter_map(|t| match t {
                        ParserDataType {
                            data_type: ParserInnerType::Struct(s),
                            ..
                        } => Some(Ustr::from(s)),
                        _ => None,
                    })
                    .collect(),
                _ => Vec::new(),
            };

            let identifier = env.resolve(
                scope,
                &self.identifier,
                ResolutionOptions::default().with_dollar(),
            )?;

            let inner =
                env.resolve_data_type(scope, inner.as_ref(), ResolutionOptions::typing())?;

            let target_name = if identifier == inner.impl_name() {
                Some(identifier)
            } else {
                None
            };

            {
                let scope_ref = env.scoping.scope_mut_or_err(scope)?;

                scope_ref.type_mappings.insert(identifier, inner.data_type);
            }

            if let Some(x) = env.context.in_stdlib
                && let Some(y) = target_name
            {
                env.symbols
                    .native_mappings
                    .insert(Ustr::from(&format!("{}.{}", x, identifier)), y);
            }

            if !self.overloads.is_empty() {
                for overload in self.overloads {
                    if let Some(processed) =
                        env.process_overload(scope, overload, generic_params.clone(), target_name)?
                    {
                        env.symbols.overloads.push(processed);
                    }
                }
            }

            return Ok(MiddleNode {
                node_type: MiddleNodeType::EmptyLine,
                span,
            });
        }

        let ident = env.resolve(
            scope,
            &self.identifier,
            ResolutionOptions::default().with_dollar(),
        )?;

        let generic_params = if let PotentialGenericTypeIdentifier::Generic {
            identifier: _,
            generic_types,
        } = self.identifier.clone()
        {
            let template_params: Vec<Ustr> = generic_types
                .iter()
                .filter_map(|t| match t {
                    ParserDataType {
                        data_type: ParserInnerType::Struct(s),
                        ..
                    } => Some(Ustr::from(s)),
                    _ => None,
                })
                .collect();

            env.typing.generic_type_templates.entry(ident).or_insert((
                template_params,
                self.object.clone(),
                self.overloads.clone(),
            ));

            env.typing
                .generic_type_templates
                .get(&ident)
                .map(|(params, _, _)| params.clone())
                .unwrap_or_default()
        } else {
            Vec::new()
        };

        let new_name = Ustr::from(&ParserText::temp_name_with_suffix(ident.trim(), span).text);

        let object = MiddleTypeDefType::from_type_def_type(env, scope, self.object.clone());

        has_default = has_default
            || match &object {
                MiddleTypeDefType::Enum {
                    default_variant, ..
                } => default_variant.is_some(),
                MiddleTypeDefType::Struct(_) => false,
                _ => false,
            };

        let default_ident = env.resolve(scope, &"Default", ResolutionOptions::typing());

        env.typing.objects.insert(
            new_name,
            MiddleObject {
                object_type: object.clone(),
                variables: UstrMap::default(),
                traits: if let Ok(x) = default_ident
                    && has_default
                {
                    vec![x]
                } else {
                    Vec::new()
                },
                location: env.context.current_location.clone(),
            },
        );

        if let Some(x) = env.context.in_stdlib {
            env.symbols
                .native_mappings
                .insert(Ustr::from(&format!("{}.{}", x, ident)), new_name);
        }

        let previous_self_type = {
            let scope = env.scoping.scope_mut_or_err(scope)?;

            scope
                .type_mappings
                .insert(ident, ParserInnerType::Struct(new_name.to_string()));

            scope.type_mappings.insert(
                Ustr::from("Self"),
                ParserInnerType::Struct(new_name.to_string()),
            )
        };

        let identifier = ParserText::new(span, ident);

        let default_node = if has_default {
            Some(env.generate_default_impl(scope, span, identifier.clone(), object.clone())?)
        } else {
            None
        };

        let builder_nodes = if has_builder {
            Some(env.generate_builder(
                scope,
                span,
                identifier.clone(),
                object.clone(),
                has_default,
            )?)
        } else {
            None
        };

        for overload in self.overloads {
            if let Some(processed) =
                env.process_overload(scope, overload, generic_params.clone(), Some(new_name))?
            {
                env.symbols.overloads.push(processed);
            }
        }

        {
            let scope = env.scoping.scope_mut_or_err(scope)?;

            if let Some(prev) = previous_self_type {
                scope.type_mappings.insert(Ustr::from("Self"), prev);
            }
        }

        match (default_node, builder_nodes) {
            (Some(node), None) => Ok(node),
            (None, None) => Ok(MiddleNode {
                node_type: MiddleNodeType::EmptyLine,
                span,
            }),
            (None, Some(nodes)) => Ok(MiddleNode {
                node_type: MiddleNodeType::ScopeDeclaration(MirScopeDecl {
                    body: vec![nodes.0, nodes.1],
                    create_new_scope: false,
                    is_temp: false,
                    scope_id: scope,
                }),
                span,
            }),
            (Some(node), Some(nodes)) => Ok(MiddleNode {
                node_type: MiddleNodeType::ScopeDeclaration(MirScopeDecl {
                    body: vec![node, nodes.0, nodes.1],
                    create_new_scope: false,
                    is_temp: false,
                    scope_id: scope,
                }),
                span,
            }),
        }
    }
}

impl MirLowering for AstTrait {
    #[instrument(skip_all)]
    fn lower(
        self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        span: Span,
    ) -> Result<MiddleNode, MiddleErr> {
        let mut generic_names = Vec::new();
        let base_name = match &self.identifier {
            PotentialGenericTypeIdentifier::Identifier(x) => Ustr::from(&x.to_string()),
            PotentialGenericTypeIdentifier::Generic {
                identifier,
                generic_types,
            } => {
                for t in generic_types {
                    if let ParserDataType {
                        data_type: ParserInnerType::Struct(s),
                        ..
                    } = t
                    {
                        generic_names.push(Ustr::from(s));
                    }
                }
                Ustr::from(&identifier.to_string())
            }
        };

        let new_name = Ustr::from(&ParserText::temp_name_with_suffix(base_name, span).text);

        env.typing.objects.insert(
            new_name,
            MiddleObject {
                object_type: MiddleTypeDefType::Trait,
                variables: UstrMap::default(),
                traits: Vec::new(),
                location: env.context.current_location.clone(),
            },
        );

        let mut prev_generics = Vec::new();
        if let Ok(scope_ref) = env.scoping.scope_mut_or_err(scope) {
            scope_ref.mappings.insert(base_name, new_name);

            for name in &generic_names {
                prev_generics.push((name, scope_ref.mappings.get(name).cloned()));
                scope_ref.mappings.insert(*name, *name);
            }
        }

        let mut trait_members = UstrMap::default();
        let mut assoc_types = UstrMap::default();
        for member in self.members {
            match member.kind {
                TraitMemberKind::Type => {
                    let data_type = env.resolve_data_type(
                        scope,
                        &member.data_type,
                        ResolutionOptions::typing(),
                    )?;

                    assoc_types.insert(Ustr::from(&member.identifier.to_string()), data_type);
                }
                TraitMemberKind::Const => {
                    let data_type = env.resolve_data_type(
                        scope,
                        &member.data_type,
                        ResolutionOptions::typing(),
                    )?;

                    trait_members.insert(
                        Ustr::from(&member.identifier.to_string()),
                        MiddleTraitMember {
                            data_type,
                            default: member.value.map(|x| *x),
                        },
                    );
                }
            }
        }

        let mut implied = Vec::new();
        for imp in self.implied_traits {
            let resolved = env
                .resolve(scope, &imp, ResolutionOptions::default().with_dollar())
                .unwrap_or_else(|_| Ustr::from(&imp.to_string()));
            implied.push(resolved);
        }

        env.typing.trait_defs.insert(
            new_name,
            MiddleTrait {
                implied_traits: implied,
                members: trait_members,
                assoc_types,
            },
        );

        if let Ok(scope_ref) = env.scoping.scope_mut_or_err(scope) {
            for (name, prev) in prev_generics {
                if let Some(prev) = prev {
                    scope_ref.mappings.insert(*name, prev);
                } else {
                    scope_ref.mappings.remove(name);
                }
            }
        }

        Ok(MiddleNode {
            node_type: MiddleNodeType::EmptyLine,
            span,
        })
    }
}

struct GenericParamState {
    params: Vec<Ustr>,
    prev_mappings: Vec<(Ustr, Option<Ustr>)>,
}

impl GenericParamState {
    fn setup(
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        generics: &GenericTypes,
    ) -> Result<Self, MiddleErr> {
        let mut prev_mappings = Vec::new();

        if let Ok(scope_ref) = env.scoping.scope_mut_or_err(scope) {
            for generic in generics.0.iter() {
                let name = Ustr::from(&generic.identifier.to_string());
                prev_mappings.push((name, scope_ref.mappings.get(&name).cloned()));
                scope_ref.mappings.insert(name, name);
            }
        }

        let params: Vec<Ustr> = generics
            .0
            .iter()
            .map(|g| {
                env.resolve(
                    scope,
                    &g.identifier,
                    ResolutionOptions::default().with_dollar(),
                )
                .unwrap_or(Ustr::from(&g.identifier.to_string()))
            })
            .collect();

        if !params.is_empty() {
            env.scoping.push_generic_params(params.clone());
        }

        Ok(GenericParamState {
            params,
            prev_mappings,
        })
    }

    fn restore(
        self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        previous_self_type: Option<ParserInnerType>,
        previous_self_mapping: Option<Ustr>,
    ) {
        let scope = env.scoping.scope_mut_or_err(scope);

        if let Ok(scope) = scope {
            if let Some(prev) = previous_self_type {
                scope.type_mappings.insert(Ustr::from("Self"), prev);
            }

            if let Some(prev) = previous_self_mapping {
                scope.mappings.insert(Ustr::from("Self"), prev);
            }

            for (name, prev) in self.prev_mappings {
                if let Some(prev) = prev {
                    scope.mappings.insert(name, prev);
                } else {
                    scope.mappings.remove(&name);
                }
            }

            if !self.params.is_empty() {
                env.scoping.pop_generic_params();
            }
        }
    }
}

struct ProcessedVariable {
    node: AstNode,
    identifier: Ustr,
    is_dependant: bool,
}

impl ProcessedVariable {
    fn process(
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        resolved_target: &ParserDataType,
        generic_params: &[Ustr],
        impl_key: Ustr,
        var: AstNode,
    ) -> Result<Option<Self>, MiddleErr> {
        let span = var.span;

        match var.node_type {
            AstNodeType::VariableDeclaration(AstDeclaration {
                var_type,
                identifier,
                value,
                data_type,
            }) => {
                let identifier = env.resolve(
                    scope,
                    &identifier,
                    ResolutionOptions::default().with_dollar(),
                )?;
                let resolved_iden = format!("{}.{}", impl_key, identifier);

                let is_dependant = match &value.node_type {
                    AstNodeType::FunctionDeclaration(AstFunction { header, .. }) => {
                        let param_type =
                            if let Some(Some(param)) = header.parameters.first().map(|x| &x.1) {
                                env.resolve_data_type(scope, param, ResolutionOptions::typing())
                                    .ok()
                                    .map(|x| x.unwrap_all_refs())
                            } else if let Some(Some(node)) =
                                header.parameters.first().map(|x| x.2.clone())
                            {
                                env.resolve_type_from_node(scope, &node)
                                    .map(|x| x.unwrap_all_refs())
                            } else {
                                None
                            };

                        if let Some(param_type) = param_type {
                            resolved_target.data_type.matches(
                                &param_type.data_type,
                                &generic_params
                                    .iter()
                                    .map(|x| x.as_ref())
                                    .collect::<Vec<_>>(),
                            )
                        } else {
                            false
                        }
                    }
                    _ => false,
                };

                Ok(Some(ProcessedVariable {
                    node: AstNode {
                        span,
                        node_type: AstNodeType::VariableDeclaration(AstDeclaration {
                            var_type,
                            identifier: PotentialDollarIdentifier::Identifier(ParserText::from(
                                resolved_iden,
                            )),
                            value,
                            data_type,
                        }),
                    },
                    identifier,
                    is_dependant,
                }))
            }
            AstNodeType::Tag(AstTag {
                node,
                tag,
                arguments,
            }) => {
                if let Some(inner) =
                    Self::process(env, scope, resolved_target, generic_params, impl_key, *node)?
                {
                    Ok(Some(ProcessedVariable {
                        node: AstNode::new(
                            Span::default(),
                            AstNodeType::Tag(AstTag {
                                node: Box::new(inner.node),
                                tag,
                                arguments,
                            }),
                        ),
                        identifier: inner.identifier,
                        is_dependant: inner.is_dependant,
                    }))
                } else {
                    Ok(None)
                }
            }
            AstNodeType::TypeDeclaration { .. } => Ok(None),
            _ => Err(MiddleErr::At(
                span,
                Box::new(MiddleErr::InternalExpectedVariableInImpl),
            )),
        }
    }
}

impl MirLowering for AstImpl {
    #[instrument(skip_all)]
    fn lower(
        self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        span: Span,
    ) -> Result<MiddleNode, MiddleErr> {
        let generic_state = GenericParamState::setup(env, scope, &self.generics)?;
        let generic_params = &generic_state.params;

        let resolved = env
            .resolve_data_type(scope, &self.target, ResolutionOptions::typing())
            .unwrap()
            .unwrap_all_refs();

        let impl_key = Ustr::from(&resolved.impl_name());

        env.typing
            .get_or_create_impl(impl_key, env.context.current_location.clone());

        let placeholders: Vec<_> = self
            .variables
            .iter()
            .filter_map(|var| {
                if let AstNodeType::VariableDeclaration(AstDeclaration { identifier, .. }) =
                    &var.node_type
                {
                    let identifier = env
                        .resolve(
                            scope,
                            identifier,
                            ResolutionOptions::default().with_dollar(),
                        )
                        .ok()?;

                    let resolved_iden = Ustr::from(&format!("{}.{}", impl_key, identifier));

                    Some((identifier, resolved_iden, generic_params.clone()))
                } else {
                    None
                }
            })
            .collect();

        let type_defs: Vec<_> = self
            .variables
            .iter()
            .filter_map(|var| {
                if let AstNodeType::TypeDeclaration(AstType {
                    identifier, object, ..
                }) = &var.node_type
                {
                    let ident = env
                        .resolve(
                            scope,
                            identifier.get_ident(),
                            ResolutionOptions::default().with_dollar(),
                        )
                        .ok()?;
                    if let TypeDefType::NewType(inner) = object {
                        let resolved_ty = env
                            .resolve_data_type(scope, inner.as_ref(), ResolutionOptions::typing())
                            .ok()?
                            .unwrap_all_refs();
                        Some((ident, resolved_ty))
                    } else {
                        None
                    }
                } else {
                    None
                }
            })
            .collect();

        if let Some(impl_ref) = env.typing.impls.get_mut(&impl_key) {
            for var in placeholders {
                impl_ref.insert_member_placeholder(&var.0, var.1, var.2);
            }

            for (ident, ty) in type_defs {
                impl_ref.assoc_types.insert(ident, ty);
            }
        }

        let previous_self_type = env.scoping.scope_mut_or_err(scope).ok().and_then(|scope| {
            scope
                .type_mappings
                .insert(Ustr::from("Self"), resolved.data_type.clone())
        });

        let mut statements = Vec::new();

        for var in self.variables {
            let processed = match ProcessedVariable::process(
                env,
                scope,
                &resolved,
                generic_params,
                impl_key,
                var,
            )? {
                Some(x) => x,
                None => continue,
            };

            let dec = processed.node.lower_or_empty(env, scope, span);

            let new_name = match &dec.node_type {
                MiddleNodeType::VariableDeclaration(MirVarDecl { identifier, .. }) => identifier,
                _ => {
                    return Err(MiddleErr::At(
                        dec.span,
                        Box::new(MiddleErr::InternalImplBodyNotVariableDeclaration),
                    ));
                }
            };

            if let Some(impl_ref) = env.typing.impls.get_mut(&impl_key) {
                impl_ref.insert_member(
                    &processed.identifier,
                    MiddleImplMember::new(
                        *new_name,
                        generic_params.clone(),
                        processed.is_dependant,
                    ),
                );
            }

            statements.push(dec);
        }

        generic_state.restore(env, scope, previous_self_type, None);

        Ok(MiddleNode {
            node_type: MiddleNodeType::ScopeDeclaration(MirScopeDecl {
                body: statements,
                create_new_scope: false,
                is_temp: false,
                scope_id: scope,
            }),
            span,
        })
    }
}

impl MirLowering for AstImplTrait {
    #[instrument(skip_all)]
    fn lower(
        mut self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        span: Span,
    ) -> Result<MiddleNode, MiddleErr> {
        let generic_state = GenericParamState::setup(env, scope, &self.generics)?;
        let generic_params = &generic_state.params;

        let resolved_trait = env.resolve(scope, &self.trait_ident, ResolutionOptions::typing())?;

        let resolved_target = env
            .resolve_data_type(scope, &self.target, ResolutionOptions::typing())?
            .unwrap_all_refs();

        let impl_key = Ustr::from(&resolved_target.impl_name());

        let mut provided = UstrSet::default();
        let mut assoc_types = Vec::new();
        for var in &self.variables {
            match &var.node_type {
                AstNodeType::VariableDeclaration(AstDeclaration { identifier, .. }) => {
                    provided.insert(Ustr::from(&identifier.to_string()));
                }
                AstNodeType::TypeDeclaration(AstType {
                    identifier, object, ..
                }) => {
                    assoc_types.push((identifier.clone(), object.clone()));
                }
                _ => {}
            }
        }

        for (name, member) in Typing::collect_trait_default_members(
            &env.typing.trait_defs,
            &resolved_trait,
            &provided,
        ) {
            if member.default.is_none() {
                continue;
            }
            let default = member.default.unwrap();
            self.variables.push(AstNode::new(
                default.span,
                AstNodeType::VariableDeclaration(AstDeclaration {
                    var_type: VarType::Constant,
                    identifier: PotentialDollarIdentifier::Identifier(ParserText::from(name)),
                    data_type: member.data_type.clone(),
                    value: Box::new(default),
                }),
            ));
        }

        let (previous_self_mapping, previous_self_type) = env
            .scoping
            .scope_mut_or_err(scope)
            .ok()
            .map(|scope| {
                (
                    scope.mappings.insert(Ustr::from("Self"), impl_key),
                    scope
                        .type_mappings
                        .insert(Ustr::from("Self"), resolved_target.data_type.clone()),
                )
            })
            .unwrap_or((None, None));

        env.typing
            .get_or_create_impl(impl_key, env.context.current_location.clone());

        for (identifier, object) in assoc_types {
            if let TypeDefType::NewType(inner) = object {
                let resolved_ty = env
                    .resolve_data_type(scope, inner.as_ref(), ResolutionOptions::typing())?
                    .unwrap_all_refs();

                let ident = env.resolve(
                    scope,
                    identifier.get_ident(),
                    ResolutionOptions::default().with_dollar(),
                )?;

                if let Some(impl_ref) = env.typing.impls.get_mut(&impl_key) {
                    impl_ref.assoc_types.insert(ident, resolved_ty);
                }
            }
        }

        if let Some(impl_ref) = env.typing.impls.get_mut(&impl_key) {
            for var in &self.variables {
                if let AstNodeType::VariableDeclaration(AstDeclaration { identifier, .. }) =
                    &var.node_type
                {
                    let resolved_iden = Ustr::from(&format!("{}.{}", impl_key, identifier));
                    impl_ref.insert_member_placeholder(
                        &identifier.to_string(),
                        resolved_iden,
                        generic_params.clone(),
                    );
                }
            }
        }

        let mut statements = Vec::new();

        for var in self.variables {
            let processed = match ProcessedVariable::process(
                env,
                scope,
                &resolved_target,
                generic_params,
                impl_key,
                var,
            )? {
                Some(x) => x,
                None => continue,
            };

            let dec = processed.node.lower_or_empty(env, scope, span);

            let new_name = match &dec.node_type {
                MiddleNodeType::VariableDeclaration(MirVarDecl { identifier, .. }) => identifier,
                _ => {
                    return Err(MiddleErr::At(
                        dec.span,
                        Box::new(MiddleErr::InternalImplBodyNotVariableDeclaration),
                    ));
                }
            };

            if let Some(impl_ref) = env.typing.impls.get_mut(&impl_key) {
                impl_ref.insert_member(
                    &processed.identifier,
                    MiddleImplMember::new(
                        *new_name,
                        generic_params.clone(),
                        processed.is_dependant,
                    ),
                );

                if !impl_ref.traits.contains(&resolved_trait) {
                    impl_ref.traits.push(resolved_trait);
                }

                if let Some(trait_def) = env.typing.trait_defs.get(&resolved_trait) {
                    for implied in &trait_def.implied_traits {
                        if !impl_ref.traits.contains(implied) {
                            impl_ref.traits.push(*implied);
                        }
                    }
                }
            }

            statements.push(dec);
        }

        generic_state.restore(env, scope, previous_self_type, previous_self_mapping);

        Ok(MiddleNode {
            node_type: MiddleNodeType::ScopeDeclaration(MirScopeDecl {
                body: statements,
                create_new_scope: false,
                is_temp: false,
                scope_id: scope,
            }),
            span,
        })
    }
}
