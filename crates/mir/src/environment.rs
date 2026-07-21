use crate::ast::{MiddleNode, MiddleNodeType};
use crate::context::MiddleContext;
use crate::errors::MiddleErr;
use crate::multipass::prepare_ast;
use crate::scoping::{MiddleScope, Scoping};
use crate::symbols::Symbols;
use crate::tags::Tagging;
use crate::testing::Testing;
use crate::typing::{MiddleTypeDefType, Typing};
use calibre_parser::COUNTER;
use calibre_parser::ast::EmitType;
use calibre_parser::{
    Location, Parser, Span,
    ast::{
        FunctionHeader, Node, NodeType, Overload, ParserDataType, ParserInnerType, ParserText,
        PotentialDollarIdentifier, PotentialGenericTypeIdentifier, PotentialNewType, TypeDefType,
        VarType,
        binary::BinaryOperator,
        comparison::{BooleanOperator, ComparisonOperator},
    },
};
use rustc_hash::{FxHashMap, FxHashSet};
use std::fmt::Debug;
use std::{fs, path::PathBuf, str::FromStr};

#[derive(Debug, Clone, Default)]
pub struct MiddleEnvironment {
    pub context: MiddleContext,
    pub symbols: Symbols,
    pub typing: Typing,
    pub scoping: Scoping,
    pub tagging: Tagging,
    pub testing: Testing,
}

pub fn get_disamubiguous_name(
    scope: &u64,
    name: Option<impl ToString>,
    var_type: Option<&VarType>,
) -> String {
    let name = name.map(|x| x.to_string()).unwrap_or(String::from("anon"));

    format!(
        "{0}-{1}{2}:{3}",
        match var_type {
            Some(VarType::Mutable) => "mut",
            Some(VarType::Immutable) => "let",
            _ => "const",
        },
        scope,
        if name.contains("-") {
            let mut counter = COUNTER.write().unwrap();
            *counter += 1;
            format!("-{}", counter)
        } else {
            String::new()
        },
        name
    )
}

impl MiddleEnvironment {
    #[inline]
    fn resolve_operator_or_bool(
        &mut self,
        scope: &u64,
        left: &Node,
        right: &Node,
        operator: Operator,
        span: Span,
    ) -> Option<ParserDataType> {
        self.get_operator_overload(scope, left, right, &operator)
            .map(|x| x.return_type.clone())
            .or_else(|| Some(ParserDataType::new(span, ParserInnerType::Bool)))
    }

    pub fn ensure_specialized_type(
        &mut self,
        _scope: &u64,
        base: &str,
        template_params: &[String],
        concrete_args: &[ParserDataType],
    ) -> Option<String> {
        let decl_scope = self.get_root_scope().id;
        if template_params.len() != concrete_args.len() {
            return None;
        }

        let key = format!(
            "type::{}::{}",
            base,
            self.canonical_type_args_key(concrete_args)
        );
        if let Some(existing) = self.type_specializations.get(&key) {
            return Some(existing.clone());
        }

        let (tpl_params, obj, overloads) = self.generic_type_templates.get(base)?.clone();
        if tpl_params.len() != template_params.len() {
            return None;
        }

        let mut subst: FxHashMap<String, ParserDataType> = FxHashMap::default();
        for (p, arg) in template_params.iter().zip(concrete_args.iter()) {
            subst.insert(p.clone(), arg.clone());
        }

        let specialized_name = format!("{}->{}", base, self.canonical_type_args_key(concrete_args));
        self.type_specializations
            .insert(key, specialized_name.clone());

        let new_obj = obj.substitute(&subst);

        let decl_node = Node::new(
            self.current_span(),
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
        let decl_scope = self.get_root_scope().id;
        if template_params.len() != concrete_args.len() {
            return None;
        }

        let key = format!(
            "fn::{}::{}",
            base,
            self.canonical_type_args_key(concrete_args)
        );
        if let Some(existing) = self.fn_specializations.get(&key) {
            return Some(existing.clone());
        }

        let (tpl_params, header, body) = self.generic_fn_templates.get(base)?.clone();
        if tpl_params.len() != template_params.len() {
            return None;
        }

        let mut subst: FxHashMap<String, ParserDataType> = FxHashMap::default();
        for (p, arg) in template_params.iter().zip(concrete_args.iter()) {
            subst.insert(p.clone(), arg.clone());
        }

        let specialized_name = format!("{}->{}", base, self.canonical_type_args_key(concrete_args));
        self.fn_specializations
            .insert(key, specialized_name.clone());

        let mut new_header = header.clone();
        new_header.generics = calibre_parser::ast::GenericTypes::default();
        new_header.parameters = new_header
            .parameters
            .into_iter()
            .map(|(n, t, v)| (n, t.map(|t| t.substitute(&subst)), v))
            .collect();
        new_header.return_type = new_header.return_type.substitute(&subst);

        let decl_node = Node::new(
            self.current_span(),
            NodeType::VariableDeclaration {
                var_type: VarType::Constant,
                identifier: ParserText::from(specialized_name.clone()).into(),
                data_type: PotentialNewType::DataType(ParserDataType::new(
                    self.current_span(),
                    ParserInnerType::Auto(None),
                )),
                value: Box::new(Node::new(
                    self.current_span(),
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

        let errors_before = self.errors.len();
        let mn = self.evaluate(&decl_scope, decl_node);
        if self.errors.len() == errors_before {
            self.specialization_decls_by_scope
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
            package_metadata,
            ..Default::default()
        };

        let scope = if no_std {
            env.new_root_scope_no_std(None, path, None)
        } else {
            env.new_root_scope_with_std(None, path, None)
        };

        let wrap = |env: &MiddleEnvironment, scope: u64, span: Span, inner: MiddleNode| {
            if env.stdlib_nodes.is_empty() {
                inner
            } else {
                let mut body = env.stdlib_nodes.clone();
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

        if let Some(mut decls) = env.specialization_decls_by_scope.remove(&scope)
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

    pub fn add_scope(&mut self, mut scope: MiddleScope) {
        scope.id = self.scope_counter;
        self.scopes.insert(scope.id, scope);
        self.scope_counter += 1;
    }

    #[inline]
    pub fn scope_file_or_fallback(scope: &MiddleScope) -> String {
        let file = scope.path.to_string_lossy().to_string();
        if file.is_empty() {
            String::from("unknown")
        } else {
            file
        }
    }

    pub fn get_scope_from_path(
        &self,
        path: &[String],
        mut parent: Option<u64>,
    ) -> Result<u64, MiddleErr> {
        let mut skip = 0;

        if parent.is_none() {
            parent = self
                .scopes
                .iter()
                .find(|(_, v)| v.namespace == path[0])
                .map(|x| x.0)
                .cloned();

            if parent.is_none() {
                return Err(self.err_at_current(MiddleErr::Scope(path[0].clone())));
            }

            skip = 1;
        }

        for name in path.iter().skip(skip) {
            if let Some(p) = parent {
                parent = Some(self.get_scope_from_parent(p, name)?);
            }
        }

        parent.ok_or_else(|| self.err_at_current(MiddleErr::Scope(path.join("::"))))
    }

    pub fn get_scope_from_parent(&self, parent: u64, namespace: &str) -> Result<u64, MiddleErr> {
        let parent_scope = self.scopes.get(&parent).ok_or_else(|| {
            self.err_at_current(MiddleErr::Internal(format!("missing scope {parent}")))
        })?;

        for (_, child) in parent_scope.children.iter() {
            if let Some(x) = self.scopes.get(child)
                && x.namespace == namespace
            {
                return Ok(x.id);
            }
        }

        Err(self.err_at_current(MiddleErr::Scope(namespace.to_string())))
    }

    pub fn new_scope_from_parent_shallow(&mut self, parent: u64) -> u64 {
        let Some(path) = self.scopes.get(&parent).map(|s| s.path.clone()) else {
            return parent;
        };
        self.new_scope(Some(parent), path, None)
    }

    pub fn new_build_scope_from_parent(&mut self, parent: u64, namespace: &str) -> Option<u64> {
        let path = self.scopes.get(&parent)?.path.clone();
        let parent_name = path.file_name()?;
        let folder = path.parent()?.to_path_buf();

        let extra = if parent_name == "main.cal" || parent_name == "mod.cal" {
            String::new()
        } else {
            let parent_str = parent_name.to_str()?;
            let base = parent_str.split('.').next()?;
            format!("{base}/")
        };

        let mut path1 = folder.clone();
        path1 = path1.join(format!("{extra}{namespace}/build.cal"));

        if path1.exists() {
            Some(self.new_scope(Some(parent), path1, Some(namespace)))
        } else {
            None
        }
    }

    pub fn new_scope_from_parent(
        &mut self,
        parent: u64,
        namespace: &str,
    ) -> Result<u64, MiddleErr> {
        if let Ok(scope) = self.get_scope_from_parent(parent, namespace) {
            return Ok(scope);
        }

        let path = self
            .scopes
            .get(&parent)
            .ok_or_else(|| {
                self.err_at_current(MiddleErr::Internal(format!(
                    "missing parent scope {parent}"
                )))
            })?
            .path
            .clone();
        let parent_name = path.file_name().ok_or_else(|| {
            self.err_at_current(MiddleErr::Internal(format!(
                "missing parent filename for scope {parent}"
            )))
        })?;
        let folder = path.parent().ok_or_else(|| {
            self.err_at_current(MiddleErr::Internal(format!(
                "missing parent directory for scope {parent}"
            )))
        })?;

        let extra = if parent_name == "main.cal" || parent_name == "mod.cal" {
            String::new()
        } else {
            let parent_str = parent_name.to_str().ok_or_else(|| {
                self.err_at_current(MiddleErr::Internal(format!(
                    "invalid parent filename for scope {parent}"
                )))
            })?;
            let base = parent_str.split('.').next().ok_or_else(|| {
                self.err_at_current(MiddleErr::Internal(format!(
                    "invalid parent filename for scope {parent}"
                )))
            })?;
            format!("{base}/")
        };

        let path_ends = [".cal", "/main.cal", "/mod.cal"];
        let path_starts = [format!("{extra}{namespace}"), format!("{namespace}")];
        let paths: Vec<PathBuf> = path_starts
            .into_iter()
            .map(|x| {
                let folder = folder.to_path_buf();
                path_ends
                    .iter()
                    .map(|y| folder.join(format!("{}{}", x, y)))
                    .collect::<Vec<_>>()
            })
            .flatten()
            .collect();

        for path in paths.clone() {
            if path.exists() {
                return Ok(self.new_scope(Some(parent), path, Some(namespace)));
            }
        }

        Err(self.err_at_current(MiddleErr::Scope(format!(
            "could not resolve module {namespace}; tried {paths:?}"
        ))))
    }

    pub fn new_scope(
        &mut self,
        parent: Option<u64>,
        path: PathBuf,
        namespace: Option<&str>,
    ) -> u64 {
        if let (Some(parent_id), Some(ns)) = (parent, namespace) {
            let existing = self.scopes.values().find_map(|scope| {
                if scope.namespace != ns {
                    return None;
                }
                if scope.path == path {
                    return Some(scope.id);
                }
                let left = std::fs::canonicalize(&scope.path).ok();
                let right = std::fs::canonicalize(&path).ok();
                if left.is_some() && left == right {
                    Some(scope.id)
                } else {
                    None
                }
            });
            if let Some(existing_id) = existing {
                if let Some(parent_scope) = self.scopes.get_mut(&parent_id) {
                    parent_scope.children.insert(ns.to_string(), existing_id);
                }
                return existing_id;
            }
        }

        if let Some(parent) = parent {
            let scope = MiddleScope {
                macros: FxHashMap::default(),
                macro_args: FxHashMap::default(),
                id: self.scope_counter,
                namespace: namespace
                    .unwrap_or(&self.scope_counter.to_string())
                    .to_string(),
                parent: Some(parent),
                children: FxHashMap::default(),
                mappings: FxHashMap::default(),
                defined: Vec::new(),
                defers: Vec::new(),
                path,
            };

            let _ = self.add_scope(scope);

            if let Some(scope_ref) = self.scopes.get_mut(&parent) {
                scope_ref.children.insert(
                    namespace
                        .map(String::from)
                        .unwrap_or((self.scope_counter - 1).to_string()),
                    self.scope_counter - 1,
                );
            }

            self.scope_counter - 1
        } else {
            let scope = MiddleScope {
                macros: FxHashMap::default(),
                macro_args: FxHashMap::default(),
                id: self.scope_counter,
                namespace: namespace
                    .unwrap_or(&self.scope_counter.to_string())
                    .to_string(),
                parent: None,
                children: FxHashMap::default(),
                mappings: FxHashMap::default(),
                defined: Vec::new(),
                defers: Vec::new(),
                path,
            };
            let _ = self.add_scope(scope);
            self.scope_counter - 1
        }
    }

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

                    return Err(self.err_at_current(MiddleErr::Scope(
                        module_path.last().cloned().unwrap_or_default(),
                    )));
                }
                _ => node.node_type,
            },
            span: node.span,
        })
    }
}
