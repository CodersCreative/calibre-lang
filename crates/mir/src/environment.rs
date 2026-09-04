use crate::ast::{MiddleNode, MiddleNodeType, MirScopeDecl};
use crate::context::MiddleContext;
use crate::errors::MiddleErr;
use crate::manifest::Manifest;
use crate::scoping::{ScopeId, Scoping};
use crate::symbols::resolve::ResolutionOptions;
use crate::symbols::{MiddleOverload, MiddleVariable, Symbols};
use crate::tags::Tagging;
use crate::tags::context::PackageMetadata;
use crate::testing::Testing;
use crate::typing::{
    MiddleImplMember, MiddleObject, MiddleTrait, MiddleTraitMember, MiddleTypeDefType, Typing,
};
use calibre_parser::ast::ObjectMap;
use calibre_parser::{AlphaRenamable, AlphaRenameState};
use calibre_parser::{
    Span,
    ast::{
        Operator,
        nodes::{AstNode, AstNodeType, Overload, VarType},
        types::{ParserDataType, ParserInnerType},
    },
};
use indextree::{Arena, NodeId};
use rustc_hash::FxHashMap;
use std::fmt::Debug;
use std::path::PathBuf;
use std::str::FromStr;
use tracing::{debug, instrument};
use ustr::Ustr;

#[derive(Debug, Clone, Default)]
pub struct MiddleEnvironment {
    pub nodes: MirNodes,
    pub context: MiddleContext,
    pub symbols: Symbols,
    pub typing: Typing,
    pub scoping: Scoping,
    pub tagging: Tagging,
    pub testing: Testing,
}

pub type MirId = NodeId;

#[derive(Debug, Clone, Default)]
pub struct MirNodes {
    pub nodes: Arena<MiddleNodeType>,
    pub spans: FxHashMap<MirId, Span>,
}

impl MiddleEnvironment {
    #[instrument(skip_all, fields(operator = %overload.operator.text))]
    pub fn process_overload(
        &mut self,
        scope: ScopeId,
        overload: Overload,
        generic_params: Vec<Ustr>,
        target_name: Option<Ustr>,
    ) -> Result<Option<MiddleOverload>, MiddleErr> {
        debug!("processing overload");
        overload.verify().map_err(MiddleErr::Overload)?;

        let operator = Operator::from_str(&overload.operator.text).map_err(MiddleErr::Overload)?;

        let return_type = self.resolve_data_type(
            scope,
            &overload.header.return_type,
            ResolutionOptions::typing(),
        )?;

        let mut params = Vec::new();
        let mut contains_target = false;

        for param in overload.header.parameters.iter() {
            let ty = match param.1.clone() {
                Some(x) if param.2.is_none() => {
                    self.resolve_data_type(scope, &x, ResolutionOptions::typing())?
                }
                _ => {
                    return Err(MiddleErr::Overload(String::from(
                        "Type needs to be explicit when doing overloads and default types arent allowed",
                    )));
                }
            };

            if let Some(ref target) = target_name
                && let ParserInnerType::Struct(x) = ty.data_type.clone().unwrap_all_refs()
                && x == target
            {
                contains_target = true;
            }

            params.push(ty);
        }

        if target_name.is_some() && !contains_target {
            debug!("overload does not contain target, skipping");
            return Ok(None);
        }

        debug!(operator = %operator, "overload processed successfully");
        Ok(Some(MiddleOverload {
            operator,
            return_type,
            parameters: params,
            func: overload.into(),
            generic_params,
        }))
    }

    #[instrument(skip_all, fields(original_name = %original_name.to_string(), new_name = %new_name))]
    pub fn register_variable(
        &mut self,
        scope: ScopeId,
        original_name: Ustr,
        new_name: Ustr,
        data_type: ParserDataType,
        var_type: VarType,
    ) -> Result<(), MiddleErr> {
        debug!(var_type = ?var_type, data_type = %data_type, "registering variable");
        self.symbols.variables.insert(
            new_name,
            MiddleVariable {
                data_type,
                var_type,
                location: self.context.current_location.clone(),
            },
        );

        if original_name != new_name {
            debug!("adding name mapping");
            self.scoping
                .scope_mut_or_err(scope)?
                .mappings
                .insert(original_name, new_name);
        } else {
            debug!(name = ?original_name, "name already present");
        }

        Ok(())
    }

    #[instrument(skip_all, fields(path = ?path, no_std = no_std))]
    pub fn new_and_evaluate_with_package(
        mut node: AstNode,
        path: PathBuf,
        package_metadata: Option<PackageMetadata>,
        included: Vec<Manifest>,
        no_std: bool,
        type_check: bool,
    ) -> (Self, ScopeId, MiddleNode) {
        debug!("creating MIR environment with package metadata");
        let mut env = Self {
            context: MiddleContext {
                package_metadata,
                type_check,
                ..Default::default()
            },
            ..Default::default()
        };

        let scope = if no_std {
            debug!("creating root scope without stdlib");
            env.scoping.new_root_scope_no_std(None, path, None)
        } else {
            debug!("creating root scope with stdlib");
            env.new_root_scope_with_std(None, path, None)
        };
        debug!(index = %scope, "root scope created");

        let wrap = |env: &MiddleEnvironment, scope: ScopeId, span: Span, inner: MiddleNode| {
            if env.context.stdlib_nodes.is_empty() {
                inner
            } else {
                let mut body = env.context.stdlib_nodes.clone();
                body.push(inner);
                MiddleNode {
                    node_type: MiddleNodeType::ScopeDeclaration(MirScopeDecl {
                        body,
                        create_new_scope: false,
                        is_temp: false,
                        scope_id: scope,
                    }),
                    span,
                }
            }
        };

        for manifest in included {
            if let Err(err) = env.import_manifest(manifest) {
                env.context.push_error(err);
            }
        }

        if let AstNodeType::ScopeDeclaration {
            body: Some(body), ..
        } = &mut node.node_type
        {
            debug!("predeclaring nodes");
            env.predeclare_nodes(scope, body);
        }

        debug!("translating AST to MIR");
        let inner = env.evaluate(scope, node.clone());
        let mut middle = wrap(&env, scope, node.span, inner);

        if let Some(mut decls) = env.symbols.specialization_decls_by_scope.remove(&scope)
            && !decls.is_empty()
        {
            debug!(
                decl_count = decls.len(),
                "adding specialization declarations"
            );
            match &mut middle.node_type {
                MiddleNodeType::ScopeDeclaration(MirScopeDecl { body, .. }) => {
                    let mut new_body = Vec::new();
                    new_body.append(&mut decls);
                    new_body.append(body);
                    *body = new_body;
                }
                _ => {
                    let mut body = Vec::new();
                    body.append(&mut decls);
                    let middle_span = middle.span;
                    body.push(middle);
                    middle = MiddleNode::new(
                        MiddleNodeType::ScopeDeclaration(MirScopeDecl {
                            body,
                            create_new_scope: false,
                            is_temp: false,
                            scope_id: scope,
                        }),
                        middle_span,
                    );
                }
            }
        }

        debug!("MIR construction completed");
        (env, scope, middle)
    }

    #[instrument(skip_all, fields(path = ?path, no_std = no_std))]
    pub fn new_and_evaluate(
        node: AstNode,
        path: PathBuf,
        included: Vec<Manifest>,
        no_std: bool,
        type_check: bool,
    ) -> (Self, ScopeId, MiddleNode) {
        debug!("starting MIR construction");
        Self::new_and_evaluate_with_package(node, path, None, included, no_std, type_check)
    }

    // TODO Reduce cloning
    pub fn import_manifest(&mut self, mut manifest: Manifest) -> Result<(), MiddleErr> {
        let mut rename_state = AlphaRenameState::default();
        rename_state.from_native_mappings(
            &self.symbols.native_mappings,
            &manifest.symbols.native_mappings,
        );

        manifest
            .symbols
            .variables
            .retain(|name, _| !rename_state.data.contains_key(name));

        manifest
            .typing
            .objects
            .retain(|name, _| !rename_state.data.contains_key(name));

        for (name, impl_data) in std::mem::take(&mut manifest.typing.impls) {
            let renamed_name = if let Some(new_name) = rename_state.data.get(&name) {
                *new_name
            } else {
                name
            };

            if let Some(existing) = self.typing.impls.get_mut(&renamed_name) {
                for (member_name, member) in impl_data.get_all_members() {
                    existing.insert_member(
                        member_name,
                        MiddleImplMember {
                            symbol_name: rename_state.mapped_name_or_original(member.symbol_name),
                            generic_params: member
                                .generic_params
                                .iter()
                                .map(|p| rename_state.mapped_name_or_original(*p))
                                .collect(),
                            dependant: member.dependant,
                        },
                    );
                }

                existing.traits.extend(impl_data.traits);
                for (assoc_name, assoc_type) in &impl_data.assoc_types {
                    existing.assoc_types.insert(
                        *assoc_name,
                        assoc_type.clone().rename_owned(&mut rename_state),
                    );
                }
            } else {
                self.typing
                    .get_or_create_impl(renamed_name, impl_data.location.clone());

                let existing = self.typing.impls.get_mut(&renamed_name).unwrap();
                existing.traits = impl_data
                    .traits
                    .iter()
                    .map(|t| rename_state.mapped_name_or_original(*t))
                    .collect();

                for (assoc_name, assoc_type) in &impl_data.assoc_types {
                    existing.assoc_types.insert(
                        *assoc_name,
                        assoc_type.clone().rename_owned(&mut rename_state),
                    );
                }

                for (member_name, member) in impl_data.get_all_members() {
                    let renamed_member = MiddleImplMember {
                        symbol_name: rename_state.mapped_name_or_original(member.symbol_name),
                        generic_params: member
                            .generic_params
                            .iter()
                            .map(|p| rename_state.mapped_name_or_original(*p))
                            .collect(),
                        dependant: member.dependant,
                    };
                    existing.insert_member(member_name, renamed_member);
                }
            }
        }

        for (name, trait_def) in std::mem::take(&mut manifest.typing.trait_defs) {
            let renamed_name = rename_state.data.get(&name).cloned().unwrap_or(name);

            let renamed_implied_traits = trait_def
                .implied_traits
                .iter()
                .map(|t| rename_state.mapped_name_or_original(*t))
                .collect();

            let renamed_members = trait_def
                .members
                .iter()
                .map(|(k, v)| {
                    (
                        rename_state.mapped_name_or_original(*k),
                        MiddleTraitMember {
                            data_type: v.data_type.clone().rename_owned(&mut rename_state),
                            default: v.default.clone(),
                        },
                    )
                })
                .collect();

            let renamed_assoc_types = trait_def
                .assoc_types
                .iter()
                .map(|(k, v)| (*k, v.clone().rename_owned(&mut rename_state)))
                .collect();

            self.typing.trait_defs.insert(
                renamed_name,
                MiddleTrait {
                    implied_traits: renamed_implied_traits,
                    members: renamed_members,
                    assoc_types: renamed_assoc_types,
                },
            );
        }

        for (name, template) in std::mem::take(&mut manifest.typing.generic_type_templates) {
            let renamed_name = if let Some(new_name) = rename_state.data.get(&name) {
                *new_name
            } else {
                name
            };

            // TODO Ensure everything is being renamed here
            self.typing.generic_type_templates.insert(
                renamed_name,
                (
                    template
                        .0
                        .iter()
                        .map(|p| rename_state.mapped_name_or_original(*p))
                        .collect(),
                    template.1,
                    template.2,
                ),
            );
        }

        for overload in std::mem::take(&mut manifest.symbols.overloads) {
            let overload = MiddleOverload {
                operator: overload.operator.clone(),
                return_type: overload.return_type.clone().rename_owned(&mut rename_state),
                parameters: overload
                    .parameters
                    .iter()
                    .map(|p| p.clone().rename_owned(&mut rename_state))
                    .collect(),
                func: overload.func.clone(),
                generic_params: overload
                    .generic_params
                    .iter()
                    .map(|p| rename_state.mapped_name_or_original(*p))
                    .collect(),
            };

            if !self.symbols.overloads.contains(&overload) {
                self.symbols.overloads.push(overload);
            }
        }

        self.tagging
            .init_functions
            .append(&mut manifest.tagging.init_functions);

        self.tagging
            .fin_functions
            .append(&mut manifest.tagging.fin_functions);

        for (name, var) in manifest.symbols.variables {
            self.symbols.variables.insert(
                name,
                MiddleVariable {
                    data_type: var.data_type.rename_owned(&mut rename_state),
                    ..var
                },
            );
        }

        for (name, obj) in manifest.typing.objects {
            let name = if let Some(new_name) = rename_state.data.get(&name) {
                *new_name
            } else {
                name
            };

            let new_obj = match &obj.object_type {
                MiddleTypeDefType::Struct(fields) => MiddleTypeDefType::Struct(ObjectMap(
                    fields
                        .0
                        .iter()
                        .map(|(field_name, (data_type, default_val))| {
                            (
                                field_name.clone(),
                                (
                                    data_type.clone().rename_owned(&mut rename_state),
                                    default_val.clone(),
                                ),
                            )
                        })
                        .collect(),
                )),
                MiddleTypeDefType::Enum {
                    variants,
                    default_variant,
                    default_value,
                } => MiddleTypeDefType::Enum {
                    variants: variants
                        .iter()
                        .map(|(variant_name, data_type)| {
                            (
                                *variant_name,
                                data_type
                                    .as_ref()
                                    .map(|t| t.clone().rename_owned(&mut rename_state)),
                            )
                        })
                        .collect(),
                    default_variant: *default_variant,
                    default_value: default_value.clone(),
                },
                MiddleTypeDefType::NewType(t) => {
                    MiddleTypeDefType::NewType(t.clone().rename_owned(&mut rename_state))
                }
                MiddleTypeDefType::Trait => MiddleTypeDefType::Trait,
            };

            self.typing.objects.insert(
                name,
                MiddleObject {
                    object_type: new_obj,
                    variables: obj
                        .variables
                        .iter()
                        .map(|(k, (v, b))| (*k, (rename_state.mapped_name_or_original(*v), *b)))
                        .collect(),
                    traits: obj
                        .traits
                        .iter()
                        .map(|t| rename_state.mapped_name_or_original(*t))
                        .collect(),
                    location: obj.location,
                },
            );
        }

        self.scoping
            .append_manifest(manifest.metadata.name, manifest.scoping);

        for (name, (params, header, node)) in manifest.symbols.generic_fn_templates {
            let name = if let Some(new_name) = rename_state.data.get(&name) {
                *new_name
            } else {
                name
            };

            self.symbols.generic_fn_templates.insert(
                name,
                (
                    params
                        .iter()
                        .map(|p| rename_state.mapped_name_or_original(*p))
                        .collect(),
                    header,
                    node,
                ),
            );
        }

        for (original, specialized) in manifest.symbols.fn_specializations {
            self.symbols.fn_specializations.insert(
                rename_state.mapped_name_or_original(original),
                rename_state.mapped_name_or_original(specialized),
            );
        }

        Ok(())
    }
}
