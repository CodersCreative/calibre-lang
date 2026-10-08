use crate::MirRenamable;
use crate::MirRenameState;
use crate::ast::types::MirDataType;
use crate::ast::{MiddleNode, MiddleNodeType, MirScopeDecl};
use crate::context::MiddleContext;
use crate::errors::MiddleErr;
use crate::manifest::Manifest;
use crate::monomorphization::Monomorphizer;
use crate::scoping::{FullyQualifiedPath, ScopeId, Scoping};
use crate::symbols::resolve::ResolutionOptions;
use crate::symbols::{MiddleOverload, MiddleVariable, Symbols, TypeKey, VariableKey};
use crate::tags::Tagging;
use crate::tags::context::PackageMetadata;
use crate::testing::Testing;
use crate::translate::MirLowering;
use crate::typing::Typing;
use calibre_parser::ast::idents::PotentialDollarIdentifier;
use calibre_parser::ast::nodes::declaration::AstDeclaration;
use calibre_parser::ast::nodes::scopes::AstScopeDef;
use calibre_parser::ast::nodes::types::Overload;
use calibre_parser::ast::types::ParserDataType;
use calibre_parser::{
    Span,
    ast::nodes::{AstNode, AstNodeType, VarType},
};
use indextree::{Arena, NodeId};
use rustc_hash::FxHashMap;
use std::fmt::Debug;
use std::path::PathBuf;
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
    pub monomorphizer: Monomorphizer,
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
    ) -> Result<Option<MiddleOverload>, MiddleErr> {
        debug!("processing overload");
        let operator = overload.verify().map_err(MiddleErr::Overload)?;

        let generic_params: Vec<Ustr> = overload
            .header
            .generics
            .0
            .iter()
            .map(|g| Ustr::from(g))
            .chain(generic_params)
            .collect();

        if !generic_params.is_empty() {
            self.scoping.push_generic_params(generic_params.clone());
        }

        let return_type = self.resolve_data_type(
            scope,
            &overload.header.return_type,
            ResolutionOptions::typing(),
        )?;

        let params = overload.header.parameters.iter().map(|param| {
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

            Ok(ty)
        }).collect::<Result<Vec<_>, MiddleErr>>()?;

        let span = *overload.span();

        let name = Ustr::from(&format!(
            "{}_{}_{}",
            operator,
            params
                .iter()
                .map(|x| x.impl_name())
                .collect::<Vec<String>>()
                .join("_"),
            return_type.impl_name()
        ));

        let data_type = MirDataType::Function {
            return_type: Box::new(return_type.clone()),
            parameters: params.clone(),
        };

        let key = self.register_variable(scope, name, data_type.clone(), VarType::Constant)?;

        let node = AstNode::new(
            span,
            AstNodeType::VariableDeclaration(AstDeclaration {
                var_type: VarType::Constant,
                identifier: PotentialDollarIdentifier::new(span, name),
                value: Box::new(overload.into()),
                data_type: ParserDataType::from(data_type),
                declared: true,
            }),
        );

        self.monomorphizer.awaiting_lowering.push((node, scope));

        if !generic_params.is_empty() {
            self.scoping.pop_generic_params();
        }

        debug!(operator = %operator, "overload processed successfully");
        Ok(Some(MiddleOverload {
            operator,
            return_type,
            parameters: params,
            func: key,
            generic_params,
        }))
    }
    #[instrument(skip_all, fields(name = %name.to_string()))]
    pub fn register_variable(
        &mut self,
        scope: ScopeId,
        name: Ustr,
        data_type: MirDataType,
        var_type: VarType,
    ) -> Result<VariableKey, MiddleErr> {
        self.register_variable_with_temp_scope(
            scope,
            name,
            data_type,
            var_type,
            self.context.in_temp_scope,
        )
    }

    pub fn get_new_variable_key(
        &mut self,
        scope: ScopeId,
        name: Ustr,
    ) -> Result<VariableKey, MiddleErr> {
        self.get_new_variable_key_with_temp_scope(scope, name, self.context.in_temp_scope)
    }

    pub fn get_new_type_key(&mut self, scope: ScopeId, name: Ustr) -> Result<TypeKey, MiddleErr> {
        let scope_ref = self.scoping.scope_or_err(scope)?;

        Ok(TypeKey {
            fully_qualified_path: FullyQualifiedPath::combine(
                Some(scope_ref.fully_qualified_path.clone()),
                name,
            ),
        })
    }

    pub fn get_new_variable_key_with_temp_scope(
        &mut self,
        scope: ScopeId,
        name: Ustr,
        in_temp_scope: bool,
    ) -> Result<VariableKey, MiddleErr> {
        let scope_ref = self.scoping.scope_or_err(scope)?;
        let fully_qualified_path =
            FullyQualifiedPath::combine(Some(scope_ref.fully_qualified_path.clone()), name);

        if !in_temp_scope {
            let key = VariableKey {
                fully_qualified_path: fully_qualified_path.clone(),
                shadow_counter: None,
            };

            if self.symbols.variables.contains_key(&key) {
                return Err(self
                    .context
                    .err_at_current(MiddleErr::VariableShadowing(name.to_string())));
            }
        }

        let shadow_counter = if in_temp_scope {
            Some(
                self.symbols
                    .variables
                    .keys()
                    .filter(|x| x.fully_qualified_path == fully_qualified_path)
                    .count() as u32,
            )
        } else {
            None
        };

        Ok(VariableKey {
            fully_qualified_path: fully_qualified_path.clone(),
            shadow_counter,
        })
    }

    #[instrument(skip_all, fields(name = %name.to_string()))]
    pub fn register_variable_with_temp_scope(
        &mut self,
        scope: ScopeId,
        name: Ustr,
        data_type: MirDataType,
        var_type: VarType,
        in_temp_scope: bool,
    ) -> Result<VariableKey, MiddleErr> {
        debug!(var_type = ?var_type, data_type = %data_type, "registering variable");

        let key = self.get_new_variable_key_with_temp_scope(scope, name, in_temp_scope)?;
        self.symbols.variables.insert(
            key.clone(),
            MiddleVariable {
                data_type,
                var_type,
                location: self.context.current_location.clone(),
                key: key.clone(),
                scope,
            },
        );

        self.scoping
            .scope_mut_or_err(scope)?
            .mappings
            .insert(name, key.clone());

        Ok(key)
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
            env.new_root_scope_no_std(None, path, None)
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
                        body: body.into_boxed_slice(),
                        create_new_scope: false,
                        is_temp: false,
                        function_body: false,
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

        if let AstNodeType::ScopeDeclaration(AstScopeDef {
            body: Some(body), ..
        }) = &mut node.node_type
        {
            debug!("predeclaring nodes");
            env.predeclare_nodes(scope, body);
        }

        debug!("translating AST to MIR");
        let span = node.span;
        let inner = node.clone().lower_or_empty(&mut env, scope, span, None);
        let middle = wrap(&env, scope, span, inner);

        debug!("MIR construction completed");
        (env, scope, middle)
    }

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

    #[instrument(skip_all)]
    pub fn import_manifest(&mut self, mut manifest: Manifest) -> Result<(), MiddleErr> {
        let mut rename_state = MirRenameState::default();
        rename_state.from_native_mappings(
            &self.symbols.native_mappings,
            &manifest.symbols.native_mappings,
        );

        manifest
            .symbols
            .variables
            .retain(|name, _| !rename_state.variables.contains_key(name));

        manifest
            .typing
            .objects
            .retain(|name, _| !rename_state.types.contains_key(name));

        for (_key, impl_list) in std::mem::take(&mut manifest.typing.inherent_impls) {
            for mut imp in impl_list {
                imp.rename(&mut rename_state);
                self.typing.add_inherent_impl(imp);
            }
        }

        self.monomorphizer
            .append(std::mem::take(&mut manifest.monomorphizer));

        for mut overload in std::mem::take(&mut manifest.symbols.overloads) {
            overload.rename(&mut rename_state);

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

        for (mut name, mut var) in manifest.symbols.variables {
            name.rename(&mut rename_state);
            var.rename(&mut rename_state);
            self.symbols.variables.insert(name, var);
        }

        for (mut name, mut obj) in manifest.typing.objects {
            name.rename(&mut rename_state);
            obj.rename(&mut rename_state);
            self.typing.objects.insert(name, obj);
        }

        self.scoping
            .append_manifest(manifest.metadata.name, manifest.scoping);

        self.monomorphizer
            .append(std::mem::take(&mut manifest.monomorphizer));

        for (original, specialized) in manifest.symbols.fn_specializations {
            self.symbols
                .function_specializations
                .insert(original, specialized);
        }

        Ok(())
    }
}
