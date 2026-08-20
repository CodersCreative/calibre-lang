use crate::ast::{MiddleNode, MiddleNodeType};
use crate::context::MiddleContext;
use crate::errors::MiddleErr;
use crate::multipass::prepare_ast;
use crate::scoping::Scoping;
use crate::symbols::{MiddleOverload, MiddleVariable, Symbols};
use crate::tags::Tagging;
use crate::tags::context::PackageMetadata;
use crate::testing::Testing;
use crate::typing::Typing;
use calibre_parser::{
    Span,
    ast::{
        Operator,
        idents::ParserText,
        nodes::{FunctionHeader, Node, NodeType, Overload, VarType},
        types::{GenericTypes, ParserDataType, ParserInnerType},
    },
};
use rustc_hash::FxHashMap;
use std::fmt::Debug;
use std::path::PathBuf;
use std::str::FromStr;

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
    pub fn process_overload(
        &mut self,
        scope: &u64,
        overload: Overload,
        generic_params: Vec<String>,
        target_name: Option<String>,
    ) -> Result<Option<MiddleOverload>, MiddleErr> {
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
            return Ok(None);
        }

        Ok(Some(MiddleOverload {
            operator,
            return_type,
            parameters: params,
            func: overload.into(),
            generic_params,
        }))
    }

    pub fn register_variable(
        &mut self,
        scope: &u64,
        original_name: impl ToString,
        new_name: String,
        data_type: ParserDataType,
        var_type: VarType,
    ) -> Result<(), MiddleErr> {
        self.symbols.variables.insert(
            new_name.clone(),
            MiddleVariable {
                data_type,
                var_type,
                location: self.context.current_location.clone(),
            },
        );

        self.scoping
            .scope_mut_or_err(scope)?
            .mappings
            .insert(original_name.to_string(), new_name);

        Ok(())
    }

    pub fn ensure_specialized_type(
        &mut self,
        _scope: &u64,
        base: &str,
        template_params: &[String],
        concrete_args: &[ParserDataType],
    ) -> Option<String> {
        let decl_scope = self.scoping.get_root_scope().id;
        if template_params.len() != concrete_args.len() {
            return None;
        }

        let key = format!(
            "type::{}::{}",
            base,
            ParserDataType::canonical_args_key(concrete_args)
        );
        if let Some(existing) = self.typing.type_specializations.get(&key) {
            return Some(existing.clone());
        }

        let (tpl_params, obj, overloads) = self.typing.generic_type_templates.get(base)?.clone();
        if tpl_params.len() != template_params.len() {
            return None;
        }

        let mut subst: FxHashMap<String, ParserDataType> = FxHashMap::default();
        for (p, arg) in template_params.iter().zip(concrete_args.iter()) {
            subst.insert(p.clone(), arg.clone());
        }

        let specialized_name = format!(
            "{}:<{}>",
            base,
            ParserDataType::canonical_args_key(concrete_args)
        );
        self.typing
            .type_specializations
            .insert(key, specialized_name.clone());

        let new_obj = obj.substitute(&subst);

        let decl_node = Node::new(
            self.context.current_span(),
            NodeType::TypeDeclaration {
                identifier: ParserText::from(specialized_name.clone()).into(),
                object: new_obj,
                overloads,
            },
        );
        let _ = self.evaluate(&decl_scope, decl_node);

        Some(specialized_name)
    }

    pub fn ensure_specialized_function(
        &mut self,
        _scope: &u64,
        base: &str,
        template_params: &[String],
        concrete_args: &[ParserDataType],
    ) -> Option<String> {
        let decl_scope = self.scoping.get_root_scope().id;
        if template_params.len() != concrete_args.len() {
            return None;
        }

        let key = format!(
            "fn::{}::{}",
            base,
            ParserDataType::canonical_args_key(concrete_args)
        );
        if let Some(existing) = self.symbols.fn_specializations.get(&key) {
            return Some(existing.clone());
        }

        let (tpl_params, header, body) = self.symbols.generic_fn_templates.get(base)?.clone();
        if tpl_params.len() != template_params.len() {
            return None;
        }

        let mut subst: FxHashMap<String, ParserDataType> = FxHashMap::default();
        for (p, arg) in template_params.iter().zip(concrete_args.iter()) {
            subst.insert(p.clone(), arg.clone());
        }

        let specialized_name = format!(
            "{}:<{}>",
            base,
            ParserDataType::canonical_args_key(concrete_args)
        );
        self.symbols
            .fn_specializations
            .insert(key, specialized_name.clone());

        let mut new_header = header.clone();
        new_header.generics = GenericTypes::default();
        new_header.parameters = new_header
            .parameters
            .into_iter()
            .map(|(n, t, v)| (n, t.map(|t| t.substitute(&subst)), v))
            .collect();
        new_header.return_type = new_header.return_type.substitute(&subst);

        let decl_node = Node::new(
            self.context.current_span(),
            NodeType::VariableDeclaration {
                var_type: VarType::Constant,
                identifier: ParserText::from(specialized_name.clone()).into(),
                data_type: ParserDataType::auto(self.context.current_span()),
                value: Box::new(Node::new(
                    self.context.current_span(),
                    NodeType::FunctionDeclaration {
                        header: FunctionHeader {
                            param_destructures: Vec::new(),
                            ..new_header
                        },
                        body: Box::new(body.clone()),
                    },
                )),
            },
        );

        let errors_before = self.context.errors.len();
        let mn = self.evaluate(&decl_scope, decl_node);
        if self.context.errors.len() == errors_before {
            self.symbols
                .specialization_decls_by_scope
                .entry(decl_scope)
                .or_default()
                .push(mn);
        }

        Some(specialized_name)
    }

    pub fn infer_generic_args_from_call(
        &mut self,
        template_params: &[String],
        param_types: &[ParserDataType],
        arg_types: &[ParserDataType],
    ) -> Option<Vec<ParserDataType>> {
        fn unify_pat(
            env: &mut MiddleEnvironment,
            template_params: &[String],
            pat: &ParserDataType,
            arg: &ParserDataType,
            out: &mut FxHashMap<String, ParserDataType>,
        ) -> bool {
            match (&pat.data_type, &arg.data_type) {
                (ParserInnerType::Struct(s), _) if template_params.contains(s) => {
                    if let Some(existing) = out.get(s) {
                        existing.data_type == arg.data_type
                    } else {
                        out.insert(s.clone(), env.resolve_data_type(&0, arg.clone()));
                        true
                    }
                }
                (ParserInnerType::List(p), ParserInnerType::List(a)) => {
                    unify_pat(env, template_params, p, a, out)
                }
                (ParserInnerType::Ptr(p), ParserInnerType::Ptr(a)) => {
                    unify_pat(env, template_params, p, a, out)
                }
                (ParserInnerType::Option(p), ParserInnerType::Option(a)) => {
                    unify_pat(env, template_params, p, a, out)
                }
                (
                    ParserInnerType::Result { ok: pk, err: pe },
                    ParserInnerType::Result { ok: ak, err: ae },
                ) => {
                    unify_pat(env, template_params, pe, ae, out)
                        && unify_pat(env, template_params, pk, ak, out)
                }
                (ParserInnerType::Tuple(ps), ParserInnerType::Tuple(as_))
                    if ps.len() == as_.len() =>
                {
                    ps.iter()
                        .zip(as_.iter())
                        .all(|(p, a)| unify_pat(env, template_params, p, a, out))
                }
                (ParserInnerType::Ref(p, _), ParserInnerType::Ref(a, _)) => {
                    unify_pat(env, template_params, p, a, out)
                }
                _ => pat.data_type == arg.data_type,
            }
        }

        if param_types.len() != arg_types.len() {
            return None;
        }

        let mut mapping: FxHashMap<String, ParserDataType> = FxHashMap::default();
        for (p, a) in param_types.iter().zip(arg_types.iter()) {
            if !unify_pat(self, template_params, p, a, &mut mapping) {
                return None;
            }
        }

        let mut result = Vec::new();
        for tp in template_params.iter() {
            result.push(mapping.get(tp)?.clone());
        }
        Some(result)
    }

    pub fn new_and_evaluate_with_package(
        node: Node,
        path: PathBuf,
        package_metadata: Option<PackageMetadata>,
        no_std: bool,
    ) -> (Self, u64, MiddleNode) {
        let mut env = Self {
            context: MiddleContext {
                package_metadata,
                ..Default::default()
            },
            ..Default::default()
        };

        let scope = if no_std {
            env.scoping.new_root_scope_no_std(None, path, None)
        } else {
            env.new_root_scope_with_std(None, path, None)
        };

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

        let node = prepare_ast(node);

        if let NodeType::ScopeDeclaration {
            body: Some(ref body),
            ..
        } = node.node_type
        {
            env.predeclare_forward_refs(&scope, body);
        }

        let inner = env.evaluate(&scope, node.clone());
        let mut middle = wrap(&env, scope, node.span, inner);

        if let Some(mut decls) = env.symbols.specialization_decls_by_scope.remove(&scope)
            && !decls.is_empty()
        {
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

        (env, scope, middle)
    }

    pub fn new_and_evaluate(node: Node, path: PathBuf, no_std: bool) -> (Self, u64, MiddleNode) {
        Self::new_and_evaluate_with_package(node, path, None, no_std)
    }

    // TODO Remove
    pub fn quick_resolve_potential_scope_member(
        &mut self,
        scope: &u64,
        node: Node,
    ) -> Result<Node, MiddleErr> {
        fn member_path_from_node(node: Node) -> Vec<(Node, bool)> {
            match node.node_type {
                NodeType::Identifier(_) => vec![(node, false)],
                NodeType::MemberExpression { path } => path,
                other => vec![(Node::new(node.span, other), false)],
            }
        }

        Ok(Node {
            node_type: match node.node_type {
                NodeType::ScopeMemberExpression { module, value } => {
                    let module_path: Vec<String> = module.iter().map(|x| x.to_string()).collect();

                    for prefix_len in (0..=module_path.len()).rev() {
                        let prefix = module_path[..prefix_len].to_vec();
                        let new_scope = self
                            .get_scope_list(*scope, prefix.clone())
                            .or_else(|_| self.import_scope_list(*scope, prefix).map(|x| x.0));
                        if let Ok(new_scope) = new_scope {
                            if prefix_len == module_path.len() {
                                let resolved_value: MiddleNode = self.evaluate(&new_scope, *value);
                                return Ok(resolved_value.into());
                            }

                            let mut path = module_path[prefix_len..]
                                .into_iter()
                                .map(|segment| {
                                    (
                                        Node::new(
                                            node.span,
                                            NodeType::Identifier(
                                                ParserText::from(segment.clone()).into(),
                                            ),
                                        ),
                                        false,
                                    )
                                })
                                .collect::<Vec<_>>();
                            path.extend(member_path_from_node(*value));

                            return Ok(self
                                .evaluate(
                                    &new_scope,
                                    Node::new(node.span, NodeType::MemberExpression { path }),
                                )
                                .into());
                        }
                    }

                    return Err(self.context.err_at_current(MiddleErr::Scope(
                        module_path.last().cloned().unwrap_or_default(),
                    )));
                }
                _ => node.node_type,
            },
            span: node.span,
        })
    }
}
