use crate::{
    ast::{
        MiddleNode, MiddleNodeType, MirScopeDecl, MirVarDecl,
        types::{MirDataType, unify::TypeImplKey},
    },
    environment::MiddleEnvironment,
    errors::MiddleErr,
    scoping::{FullyQualifiedPath, ScopeId},
    symbols::{
        TypeKey, VariableKey,
        resolve::{Key, ResolutionOptions},
    },
    tags::TagInfo,
    translate::MirLowering,
    typing::{MiddleImpl, MiddleImplMember, MiddleObject, MiddleTypeDefType},
};
use calibre_parser::{
    Span,
    ast::{
        idents::{ParserText, PotentialDollarIdentifier},
        nodes::{
            AstNode, AstNodeType,
            declaration::AstDeclaration,
            functions::AstFunction,
            misc::AstTag,
            types::{AstImpl, AstType, TypeDefType},
        },
        types::GenericTypes,
    },
};
use rustc_hash::FxHashMap;
use std::sync::Arc;
use tracing::instrument;
use ustr::{Ustr, UstrMap};

impl MirLowering for AstType {
    #[instrument(skip_all, fields(scope, identifier))]
    fn lower(
        self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        span: Span,
        _data_type: Option<MirDataType>,
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
            let identifier = env
                .resolve(
                    scope,
                    &self.identifier,
                    ResolutionOptions::default().with_dollar(),
                )?
                .unwrap_dollar();

            let inner =
                env.resolve_data_type(scope, inner.as_ref(), ResolutionOptions::typing())?;

            let target_name = match &inner {
                MirDataType::Struct { identifier, .. } => Some(identifier.clone()),
                _ => None,
            };

            {
                let scope_ref = env.scoping.scope_mut_or_err(scope)?;

                scope_ref.type_mappings.insert(identifier, inner);
            }

            if let Some(x) = env.context.in_stdlib
                && let Some(ref y) = target_name
            {
                env.symbols.native_mappings.insert(
                    Ustr::from(&format!("{}.{}", x, identifier)),
                    Key::TypeKey(y.clone()),
                );
            }

            return Ok(MiddleNode {
                node_type: MiddleNodeType::EmptyLine,
                span,
            });
        }

        let ident_key = env.resolve(
            scope,
            &self.identifier,
            ResolutionOptions::default().with_dollar(),
        )?;
        let type_key = match ident_key {
            Key::TypeKey(tk) => tk,
            Key::VariableKey(vk) => TypeKey {
                fully_qualified_path: vk.fully_qualified_path,
            },
        };
        let ident_ustr = *type_key.name();

        let object = MiddleTypeDefType::from_type_def_type(env, scope, self.object.clone());

        has_default = has_default
            || match &object {
                MiddleTypeDefType::Enum {
                    default_variant, ..
                } => default_variant.is_some(),
                MiddleTypeDefType::Struct(_) => false,
                _ => false,
            };

        let default_ident = env
            .resolve(scope, &"Default", ResolutionOptions::typing())
            .ok()
            .map(|k| k.unwrap_typing());

        env.typing.objects.insert(
            type_key.clone(),
            MiddleObject {
                object_type: object.clone(),
                variables: UstrMap::default(),
                traits: if let Some(x) = default_ident
                    && has_default
                {
                    vec![x]
                } else {
                    Vec::new()
                },
                location: env.context.current_location.clone(),
                scope,
            },
        );

        if let Some(x) = env.context.in_stdlib {
            env.symbols.native_mappings.insert(
                Ustr::from(&format!("{}.{}", x, ident_ustr)),
                Key::TypeKey(type_key.clone()),
            );
        }

        let previous_self_type = {
            let scope = env.scoping.scope_mut_or_err(scope)?;

            scope.type_mappings.insert(
                ident_ustr,
                MirDataType::Struct {
                    identifier: type_key.clone(),
                    generic_types: Vec::new(),
                },
            );

            scope.type_mappings.insert(
                Ustr::from("Self"),
                MirDataType::Struct {
                    identifier: type_key.clone(),
                    generic_types: Vec::new(),
                },
            )
        };

        let identifier = ParserText::new(span, ident_ustr);

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
                    body: Box::new([nodes.0, nodes.1]),
                    create_new_scope: false,
                    is_temp: false,
                    function_body: false,
                    scope_id: scope,
                }),
                span,
            }),
            (Some(node), Some(nodes)) => Ok(MiddleNode {
                node_type: MiddleNodeType::ScopeDeclaration(MirScopeDecl {
                    body: Box::new([node, nodes.0, nodes.1]),
                    create_new_scope: false,
                    is_temp: false,
                    function_body: false,
                    scope_id: scope,
                }),
                span,
            }),
        }
    }
}

struct GenericParamState {
    params: Vec<Ustr>,
    prev_mappings: Vec<(Ustr, Option<MirDataType>)>,
}

impl GenericParamState {
    fn setup(
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        generics: &GenericTypes,
    ) -> Result<Self, MiddleErr> {
        let mut prev_mappings = Vec::new();
        let generics = generics
            .0
            .iter()
            .map(|generic| {
                let name = env
                    .resolve(scope, generic, ResolutionOptions::default().with_dollar())
                    .map(|k| k.unwrap_dollar())?;
                Ok((name, env.get_new_type_key(scope, name)?))
            })
            .collect::<Result<Vec<_>, MiddleErr>>()?;

        if let Ok(scope_ref) = env.scoping.scope_mut_or_err(scope) {
            for (name, key) in generics.clone().into_iter() {
                prev_mappings.push((name, scope_ref.type_mappings.get(&name).cloned()));
                scope_ref.type_mappings.insert(
                    name,
                    MirDataType::Struct {
                        identifier: key,
                        generic_types: Vec::new(),
                    },
                );
            }
        }

        let params: Vec<Ustr> = generics.into_iter().map(|x| x.0).collect();

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
        previous_self_type: Option<MirDataType>,
    ) {
        let scope = env.scoping.scope_mut_or_err(scope);

        if let Ok(scope) = scope {
            if let Some(prev) = previous_self_type {
                scope.type_mappings.insert(Ustr::from("Self"), prev);
            }

            for (name, prev) in self.prev_mappings {
                if let Some(prev) = prev {
                    scope.type_mappings.insert(name, prev);
                } else {
                    scope.type_mappings.remove(&name);
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
        resolved_target: &MirDataType,
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
                declared: _,
            }) => {
                let identifier = env
                    .resolve(
                        scope,
                        &identifier,
                        ResolutionOptions::default().with_dollar(),
                    )?
                    .unwrap_dollar();
                let resolved_iden = format!("{}.{}", impl_key, identifier);

                let is_dependant = match &value.node_type {
                    AstNodeType::FunctionDeclaration(AstFunction { header, .. }) => {
                        let param_type =
                            if let Some(Some(param)) = header.parameters.first().map(|x| &x.1) {
                                env.resolve_data_type(scope, param, ResolutionOptions::typing())
                                    .ok()
                                    .map(|x| x.unwrap_all_refs().clone())
                            } else if let Some(Some(node)) =
                                header.parameters.first().map(|x| x.2.clone())
                            {
                                env.resolve_type_from_node(scope, &node)
                                    .map(|x| x.unwrap_all_refs().clone())
                            } else {
                                None
                            };

                        if let Some(ref param_type) = param_type {
                            resolved_target.can_unify(
                                param_type,
                                generic_params,
                                &mut FxHashMap::default(),
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
                            declared: false,
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
        _data_type: Option<MirDataType>,
    ) -> Result<MiddleNode, MiddleErr> {
        let generic_state = GenericParamState::setup(env, scope, &self.generics)?;
        let generic_params = &generic_state.params;

        let resolved = env
            .resolve_data_type(scope, &self.target, ResolutionOptions::typing())?
            .unwrap_all_refs()
            .clone();

        let impl_key = Ustr::from(&resolved.to_string());
        let target_key = TypeImplKey::from(&resolved);

        let mut imp = MiddleImpl::new_inherent(
            resolved.clone(),
            generic_params.clone(),
            env.context.current_location.clone(),
        );

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
                        .ok()?
                        .unwrap_dollar();

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
                        .ok()?
                        .unwrap_dollar();
                    if let TypeDefType::NewType(inner) = object {
                        let resolved_ty = env
                            .resolve_data_type(scope, inner.as_ref(), ResolutionOptions::typing())
                            .ok()?
                            .unwrap_all_refs()
                            .clone();
                        Some((ident, resolved_ty))
                    } else {
                        None
                    }
                } else {
                    None
                }
            })
            .collect();

        for var in placeholders {
            imp.insert_member_placeholder(
                var.0,
                VariableKey {
                    fully_qualified_path: Arc::new(FullyQualifiedPath {
                        name: Some(var.1),
                        parent: None,
                    }),
                    shadow_counter: None,
                },
                var.2,
            );
        }

        for (ident, ty) in type_defs {
            imp.assoc_types.insert(ident, ty);
        }

        let previous_self_type = env.scoping.scope_mut_or_err(scope).ok().and_then(|scope| {
            scope
                .type_mappings
                .insert(Ustr::from("Self"), resolved.clone())
        });

        env.typing.add_inherent_impl(imp);

        let statements = self
            .variables
            .into_iter()
            .filter_map(|var| {
                let processed = match ProcessedVariable::process(
                    env,
                    scope,
                    &resolved,
                    generic_params,
                    impl_key,
                    var,
                ) {
                    Ok(Some(x)) => x,
                    Ok(None) => return None,
                    Err(e) => return Some(Err(e)),
                };

                let dec = processed.node.lower_or_empty(env, scope, span, None);

                let new_name = match &dec.node_type {
                    MiddleNodeType::VariableDeclaration(MirVarDecl { identifier, .. }) => {
                        identifier
                    }
                    _ => {
                        return Some(Err(MiddleErr::At(
                            dec.span,
                            Box::new(MiddleErr::InternalImplBodyNotVariableDeclaration),
                        )));
                    }
                };

                if let Some(impl_ref) = env
                    .typing
                    .inherent_impls
                    .get_mut(&target_key)
                    .and_then(|v| v.last_mut())
                {
                    impl_ref.insert_member(
                        processed.identifier,
                        MiddleImplMember::new(
                            new_name.clone(),
                            generic_params.clone(),
                            processed.is_dependant,
                        ),
                    );
                }

                Some(Ok(dec))
            })
            .collect::<Result<Vec<_>, MiddleErr>>()?;

        generic_state.restore(env, scope, previous_self_type);

        Ok(MiddleNode {
            node_type: MiddleNodeType::ScopeDeclaration(MirScopeDecl {
                body: statements.into_boxed_slice(),
                create_new_scope: false,
                is_temp: false,
                function_body: false,
                scope_id: scope,
            }),
            span,
        })
    }
}
