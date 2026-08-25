use crate::ast::{MiddleNode, MiddleNodeType};
use crate::context::MiddleContext;
use crate::errors::MiddleErr;
use crate::scoping::Scoping;
use crate::symbols::resolve::ResolutionOptions;
use crate::symbols::{MiddleOverload, MiddleVariable, Symbols};
use crate::tags::Tagging;
use crate::tags::context::PackageMetadata;
use crate::testing::Testing;
use crate::typing::Typing;
use calibre_parser::{
    Span,
    ast::{
        Operator,
        nodes::{Node, NodeType, Overload, VarType},
        types::{ParserDataType, ParserInnerType},
    },
};
use std::fmt::Debug;
use std::path::PathBuf;
use std::str::FromStr;
use tracing::{debug, instrument};

#[derive(Debug, Clone, Default)]
pub struct MiddleEnvironment {
    pub context: MiddleContext,
    pub symbols: Symbols,
    pub typing: Typing,
    pub scoping: Scoping,
    pub tagging: Tagging,
    pub testing: Testing,
}

impl MiddleEnvironment {
    #[instrument(skip_all, fields(operator = %overload.operator.text, scope = scope))]
    pub fn process_overload(
        &mut self,
        scope: &u64,
        overload: Overload,
        generic_params: Vec<String>,
        target_name: Option<String>,
    ) -> Result<Option<MiddleOverload>, MiddleErr> {
        debug!("processing overload");
        overload.verify().map_err(MiddleErr::Overload)?;

        let operator = Operator::from_str(&overload.operator.text).map_err(MiddleErr::Overload)?;

        let return_type = self.resolve_data_type(scope, overload.header.return_type.clone());

        let mut params = Vec::new();
        let mut contains_target = false;

        for param in overload.header.parameters.iter() {
            let ty = match param.1.clone() {
                Some(x) if param.2.is_none() => self.resolve_data_type(scope, x),
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

    #[instrument(skip_all, fields(scope = scope, original_name = %original_name.to_string(), new_name = %new_name))]
    pub fn register_variable(
        &mut self,
        scope: &u64,
        original_name: impl ToString,
        new_name: String,
        data_type: ParserDataType,
        var_type: VarType,
    ) -> Result<(), MiddleErr> {
        debug!(var_type = ?var_type, data_type = %data_type, "registering variable");
        self.symbols.variables.insert(
            new_name.clone(),
            MiddleVariable {
                data_type,
                var_type,
                location: self.context.current_location.clone(),
            },
        );

        let original_name = original_name.to_string();
        if original_name != new_name {
            debug!("adding name mapping");
            self.scoping
                .scope_mut_or_err(scope)?
                .mappings
                .insert(original_name.to_string(), new_name);
        } else {
            debug!(name = ?original_name, "name already present");
        }

        Ok(())
    }

    #[instrument(skip_all, fields(path = ?path, no_std = no_std))]
    pub fn new_and_evaluate_with_package(
        mut node: Node,
        path: PathBuf,
        package_metadata: Option<PackageMetadata>,
        no_std: bool,
    ) -> (Self, u64, MiddleNode) {
        debug!("creating MIR environment with package metadata");
        let mut env = Self {
            context: MiddleContext {
                package_metadata,
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

        let wrap = |env: &MiddleEnvironment, scope: u64, span: Span, inner: MiddleNode| {
            if env.context.stdlib_nodes.is_empty() {
                inner
            } else {
                let mut body = env.context.stdlib_nodes.clone();
                body.push(inner);
                MiddleNode {
                    node_type: MiddleNodeType::ScopeDeclaration {
                        body,
                        create_new_scope: false,
                        is_temp: false,
                        scope_id: scope,
                    },
                    span,
                }
            }
        };

        if let NodeType::ScopeDeclaration {
            body: Some(body), ..
        } = &mut node.node_type
        {
            debug!("predeclaring nodes");
            let _ = env.predeclare_nodes(&scope, body).map_err(|err| {
                env.context.push_error(err);
            });
        }

        debug!("translating AST to MIR");
        let inner = env.evaluate(&scope, node.clone());
        let mut middle = wrap(&env, scope, node.span, inner);

        if let Some(mut decls) = env.symbols.specialization_decls_by_scope.remove(&scope)
            && !decls.is_empty()
        {
            debug!(
                decl_count = decls.len(),
                "adding specialization declarations"
            );
            match &mut middle.node_type {
                MiddleNodeType::ScopeDeclaration { body, .. } => {
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
                        MiddleNodeType::ScopeDeclaration {
                            body,
                            create_new_scope: false,
                            is_temp: false,
                            scope_id: scope,
                        },
                        middle_span,
                    );
                }
            }
        }

        debug!("MIR construction completed");
        (env, scope, middle)
    }

    #[instrument(skip_all, fields(path = ?path, no_std = no_std))]
    pub fn new_and_evaluate(node: Node, path: PathBuf, no_std: bool) -> (Self, u64, MiddleNode) {
        debug!("starting MIR construction");
        Self::new_and_evaluate_with_package(node, path, None, no_std)
    }

    // TODO Remove
    pub fn quick_resolve_potential_scope_member(
        &mut self,
        scope: &u64,
        node: Node,
    ) -> Result<Node, MiddleErr> {
        Ok(Node {
            node_type: match &node.node_type {
                NodeType::ScopeAccess { base, field } => {
                    if let NodeType::Identifier(module_name) = &base.node_type {
                        let field_text = self
                            .resolve(scope, field, ResolutionOptions::default().with_dollar())
                            .unwrap_or(field.text().clone());
                        let module_path =
                            vec![module_name.get_ident().text().clone(), field_text.clone()];

                        for prefix_len in (0..=module_path.len()).rev() {
                            let prefix = module_path[..prefix_len].to_vec();
                            let new_scope = self
                                .get_scope_list(*scope, prefix.clone())
                                .or_else(|_| self.import_scope_list(*scope, prefix).map(|x| x.0));

                            if let Ok(new_scope) = new_scope
                                && prefix_len == module_path.len()
                            {
                                let resolved_value: MiddleNode = self
                                    .evaluate(&new_scope, Node::identifier(node.span, field_text));
                                return Ok(resolved_value.into());
                            }
                        }
                    }
                    node.node_type.clone()
                }
                _ => node.node_type.clone(),
            },
            span: node.span,
        })
    }
}
