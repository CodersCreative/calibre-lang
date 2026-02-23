use calibre_parser::{
    Span,
    ast::{
        CallArg, Node, NodeType, ParserDataType, ParserInnerType, ParserText,
        PotentialDollarIdentifier, PotentialGenericTypeIdentifier,
    },
};
use std::str::FromStr;

use crate::{
    ast::{MiddleNode, MiddleNodeType},
    environment::{MiddleEnvironment, MiddleTypeDefType},
    errors::MiddleErr,
};

impl MiddleEnvironment {
    fn evaluate_explicit_member_call(
        &mut self,
        scope: &u64,
        list: &[(MiddleNode, bool)],
        caller: Box<Node>,
        _generic_types: Vec<calibre_parser::ast::PotentialNewType>,
        args: Vec<CallArg>,
        reverse_args: Vec<Node>,
        receiver_is_value: bool,
        target_type: Option<ParserDataType>,
    ) -> Result<MiddleNode, MiddleErr> {
        let receiver_middle = list
            .last()
            .map(|(node, _)| node.clone())
            .unwrap_or_else(|| MiddleNode::new(MiddleNodeType::EmptyLine, self.current_span()));
        let receiver_node: Node = list
            .last()
            .map(|(node, _)| node.clone().into())
            .unwrap_or_else(|| {
                MiddleNode::new(MiddleNodeType::EmptyLine, self.current_span()).into()
            });

        let resolved_caller = if let NodeType::Identifier(member_ident) = &caller.node_type {
            let name = member_ident.to_string();
            target_type
                .as_ref()
                .and_then(|ty| self.resolve_impl_member(scope, ty, &name))
                .or_else(|| {
                    target_type
                        .as_ref()
                        .and_then(|ty| self.resolve_member_fn_name(ty, &name))
                })
                .or_else(|| {
                    list.last()
                        .and_then(|(base, _)| self.resolve_member_from_chain_family(base, &name))
                })
                .or_else(|| {
                    list.last()
                        .and_then(|(base, _)| self.resolve_chain_member_name(base, &name))
                        .and_then(|candidate| {
                            if self.variables.contains_key(&candidate) {
                                Some(candidate)
                            } else {
                                self.resolve_str(scope, &candidate)
                            }
                        })
                })
        } else {
            None
        };

        let mut receiver_is_bound_value = receiver_is_value;
        if !receiver_is_bound_value && let NodeType::Identifier(ident) = &receiver_node.node_type {
            let receiver_name = ident.to_string();
            let resolved_name = self.resolve_str(scope, &receiver_name);
            let resolved_ident = self.resolve_potential_generic_ident(scope, ident);
            receiver_is_bound_value = self.variables.contains_key(&receiver_name)
                || resolved_name
                    .as_ref()
                    .is_some_and(|name| self.variables.contains_key(name))
                || resolved_ident
                    .as_ref()
                    .is_some_and(|id| self.variables.contains_key(&id.text));
        }

        if let Some(function_name) = resolved_caller {
            let mut lowered_args = Vec::new();
            let mut should_prepend_receiver = receiver_is_bound_value;
            if !should_prepend_receiver
                && let Some(var) = self.variables.get(&function_name)
                && let ParserInnerType::Function { parameters, .. } = &var.data_type.data_type
                && let Some(first) = parameters.first()
                && let Some(target) = target_type.as_ref()
            {
                let target_inner = target.clone().unwrap_all_refs().data_type;
                let first_inner = match &first.data_type {
                    ParserInnerType::Ref(inner, _) => &inner.data_type,
                    other => other,
                };
                if self.impl_type_matches(first_inner, &target_inner, &Vec::new()) {
                    should_prepend_receiver = true;
                }
            }

            if should_prepend_receiver {
                let mut self_arg = receiver_middle;
                if let Some(var) = self.variables.get(&function_name)
                    && let ParserInnerType::Function { parameters, .. } = &var.data_type.data_type
                    && let Some(first) = parameters.first()
                    && let ParserInnerType::Ref(_, mutability) = &first.data_type
                {
                    self_arg = MiddleNode::new(
                        MiddleNodeType::RefStatement {
                            mutability: mutability.clone(),
                            value: Box::new(self_arg),
                        },
                        self.current_span(),
                    );
                }
                lowered_args.push(self_arg);
            }

            for arg in args {
                lowered_args.push(self.evaluate(scope, arg.into()));
            }
            for arg in reverse_args {
                lowered_args.push(self.evaluate(scope, arg));
            }
            if lowered_args.len() >= 2 && lowered_args[0].to_string() == lowered_args[1].to_string()
            {
                lowered_args.remove(1);
            }

            Ok(MiddleNode::new(
                MiddleNodeType::CallExpression {
                    caller: Box::new(MiddleNode::new(
                        MiddleNodeType::Identifier(ParserText::from(function_name)),
                        self.current_span(),
                    )),
                    args: lowered_args,
                },
                self.current_span(),
            ))
        } else {
            // Keep unresolved member calls as true member calls (`receiver.member(...)`)
            // so runtime/member dispatch can resolve methods on inferred/runtime types.
            let member_call_caller = Node::new(
                self.current_span(),
                NodeType::MemberExpression {
                    path: vec![(receiver_node, false), (*caller, false)],
                },
            );

            let mut lowered_args = Vec::new();
            for arg in args {
                lowered_args.push(self.evaluate(scope, arg.into()));
            }
            for arg in reverse_args {
                lowered_args.push(self.evaluate(scope, arg));
            }

            Ok(MiddleNode::new(
                MiddleNodeType::CallExpression {
                    caller: Box::new(self.evaluate(scope, member_call_caller)),
                    args: lowered_args,
                },
                self.current_span(),
            ))
        }
    }

    fn resolve_member_from_chain_family(&self, base: &MiddleNode, member: &str) -> Option<String> {
        let MiddleNodeType::CallExpression { caller, .. } = &base.node_type else {
            return None;
        };
        let MiddleNodeType::Identifier(caller_name) = &caller.node_type else {
            return None;
        };

        let text = caller_name.text.as_str();
        let family = text.rsplit_once("::").map(|(lhs, _)| lhs).unwrap_or(text);

        for imp in self.impls.values() {
            if let Some((mapped, _)) = imp.variables.get(member) {
                let mapped_family = mapped
                    .rsplit_once("::")
                    .map(|(lhs, _)| lhs)
                    .unwrap_or(mapped.as_str());
                if mapped_family == family {
                    return Some(mapped.clone());
                }
            }
        }
        None
    }

    fn resolve_chain_member_name(&self, base: &MiddleNode, member: &str) -> Option<String> {
        let MiddleNodeType::CallExpression { caller, .. } = &base.node_type else {
            return None;
        };
        let MiddleNodeType::Identifier(caller_name) = &caller.node_type else {
            return None;
        };

        let text = caller_name.text.as_str();
        let family = text.rsplit_once("::").map(|(lhs, _)| lhs).unwrap_or(text);

        let needle = format!("{family}::{member}");
        let mut found = None;
        for key in self.variables.keys() {
            if key.ends_with(&needle) {
                found = Some(key.clone());
                break;
            }
        }

        found.or(Some(needle))
    }

    fn member_base_type(&mut self, scope: &u64, base: &MiddleNode) -> Option<ParserDataType> {
        match &base.node_type {
            MiddleNodeType::Identifier(ident) => {
                let resolved_ident = self.resolve_dollar_ident_only(
                    scope,
                    &PotentialDollarIdentifier::Identifier(ident.clone()),
                );
                if let Some(resolved) = resolved_ident
                    && let Some(var) = self.variables.get(&resolved.text)
                {
                    return Some(var.data_type.clone().unwrap_all_refs());
                }
                if let Some(resolved_name) = self.resolve_str(scope, &ident.text)
                    && let Some(var) = self.variables.get(&resolved_name)
                {
                    return Some(var.data_type.clone().unwrap_all_refs());
                }
                if let Some(var) = self.variables.get(&ident.text) {
                    return Some(var.data_type.clone().unwrap_all_refs());
                }
                let generic_ident = PotentialGenericTypeIdentifier::Identifier(
                    PotentialDollarIdentifier::Identifier(ident.clone()),
                );
                if let Some(ty) = self.resolve_type_from_ident(scope, &generic_ident) {
                    return Some(ty.unwrap_all_refs());
                }
            }
            MiddleNodeType::CallExpression { caller, .. } => {
                if let MiddleNodeType::Identifier(ident) = &caller.node_type
                    && let Some(var) = self.variables.get(&ident.text)
                    && let ParserInnerType::Function { return_type, .. } = &var.data_type.data_type
                {
                    return Some((**return_type).clone().unwrap_all_refs());
                }
            }
            _ => {}
        }
        self.resolve_type_from_node(scope, &base.clone().into())
            .map(|x| x.unwrap_all_refs())
    }

    fn member_base_is_value(&mut self, scope: &u64, base: &MiddleNode) -> bool {
        if let MiddleNodeType::Identifier(ident) = &base.node_type {
            if let Some(resolved) = self.resolve_dollar_ident_only(
                scope,
                &PotentialDollarIdentifier::Identifier(ident.clone()),
            ) && self.variables.contains_key(&resolved.text)
            {
                return true;
            }
            if let Some(resolved_name) = self.resolve_str(scope, &ident.text)
                && self.variables.contains_key(&resolved_name)
            {
                return true;
            }
            if self.variables.contains_key(&ident.text) {
                return true;
            }
            let generic_ident = PotentialGenericTypeIdentifier::Identifier(
                PotentialDollarIdentifier::Identifier(ident.clone()),
            );
            return self
                .resolve_type_from_ident(scope, &generic_ident)
                .is_none();
        }
        true
    }

    fn resolve_impl_member(
        &mut self,
        scope: &u64,
        data_type: &ParserDataType,
        member: &str,
    ) -> Option<String> {
        let resolve_from = |env: &MiddleEnvironment, ty: &ParserDataType, m: &str| {
            env.resolve_member_fn_name(ty, m)
        };

        let resolved = self
            .resolve_data_type(scope, data_type.clone())
            .unwrap_all_refs();
        let resolved_impl_key = self.impl_key(&resolved);
        if let Some(imp) = self.impls.get(&resolved_impl_key)
            && let Some((mapped, _)) = imp.variables.get(member)
        {
            return Some(mapped.clone());
        }
        let input_impl_key = self.impl_key(data_type);
        if let Some(imp) = self.impls.get(&input_impl_key)
            && let Some((mapped, _)) = imp.variables.get(member)
        {
            return Some(mapped.clone());
        }
        if let Some(found) =
            resolve_from(self, &resolved, member).or_else(|| resolve_from(self, data_type, member))
        {
            return Some(found);
        }

        let target_inner = resolved.clone().unwrap_all_refs().data_type;
        if let Some(found) = self.variables.iter().find_map(|(name, var)| {
            if !name.ends_with(&format!("::{member}")) {
                return None;
            }
            let ParserInnerType::Function { parameters, .. } = &var.data_type.data_type else {
                return None;
            };
            let Some(first) = parameters.first() else {
                return None;
            };
            let param_inner = match &first.data_type {
                ParserInnerType::Ref(inner, _) => &inner.data_type,
                other => other,
            };
            if self.impl_type_matches(param_inner, &target_inner, &Vec::new()) {
                Some(name.clone())
            } else {
                None
            }
        }) {
            return Some(found);
        }
        if let Some(found) = self.variables.iter().find_map(|(name, var)| {
            let tail = name.rsplit(':').next().unwrap_or(name.as_str());
            if tail != member {
                return None;
            }
            let ParserInnerType::Function { parameters, .. } = &var.data_type.data_type else {
                return None;
            };
            let Some(first) = parameters.first() else {
                return None;
            };
            let param_inner = match &first.data_type {
                ParserInnerType::Ref(inner, _) => &inner.data_type,
                other => other,
            };
            if self.impl_type_matches(param_inner, &target_inner, &Vec::new()) {
                Some(name.clone())
            } else {
                None
            }
        }) {
            return Some(found);
        }

        fn normalize_owner(owner: &str) -> String {
            let mut cur = owner.to_string();
            loop {
                let is_mangled =
                    cur.starts_with("const-") || cur.starts_with("let-") || cur.starts_with("mut-");
                if !is_mangled {
                    break;
                }
                let Some((_, rest)) = cur.split_once(':') else {
                    break;
                };
                cur = rest.to_string();
            }
            cur
        }
        let target_family: Option<String> = match &target_inner {
            ParserInnerType::Int => Some("int".to_string()),
            ParserInnerType::UInt => Some("uint".to_string()),
            ParserInnerType::Float => Some("float".to_string()),
            ParserInnerType::Bool => Some("bool".to_string()),
            ParserInnerType::Char => Some("char".to_string()),
            ParserInnerType::Dynamic => Some("dyn".to_string()),
            ParserInnerType::Null => Some("null".to_string()),
            ParserInnerType::List(_) => Some("list".to_string()),
            ParserInnerType::Range => Some("range".to_string()),
            ParserInnerType::Str => Some("str".to_string()),
            ParserInnerType::Struct(name) => Some(
                normalize_owner(name)
                    .split("->")
                    .next()
                    .unwrap_or(name.as_str())
                    .to_string(),
            ),
            ParserInnerType::StructWithGenerics { identifier, .. } => Some(
                normalize_owner(identifier)
                    .split("->")
                    .next()
                    .unwrap_or(identifier.as_str())
                    .to_string(),
            ),
            _ => None,
        };
        if let Some(target_family) = target_family
            && let Some(found) = self.variables.keys().find(|name| {
                let Some((owner, meth)) = name.rsplit_once("::") else {
                    return false;
                };
                if meth != member {
                    return false;
                }
                let owner = normalize_owner(owner);
                let owner_family = owner
                    .split("->")
                    .next()
                    .unwrap_or(owner.as_str())
                    .rsplit_once("::")
                    .map(|(_, rhs)| rhs)
                    .unwrap_or_else(|| owner.split("->").next().unwrap_or(owner.as_str()));
                if target_family == "list" {
                    return owner_family.starts_with("list:<") || owner.starts_with("list:<");
                }
                owner_family == target_family
            })
        {
            return Some(found.clone());
        }
        if let Some(target_family) = match &target_inner {
            ParserInnerType::Int => Some("int"),
            ParserInnerType::UInt => Some("uint"),
            ParserInnerType::Float => Some("float"),
            ParserInnerType::Bool => Some("bool"),
            ParserInnerType::Char => Some("char"),
            ParserInnerType::Str => Some("str"),
            ParserInnerType::Range => Some("range"),
            ParserInnerType::List(_) => Some("list"),
            _ => None,
        } {
            return Some(format!("{target_family}::{member}"));
        }
        let target = resolved.clone();
        let target_name = match &target.data_type {
            ParserInnerType::Struct(name) => Some(
                name.rsplit_once(':')
                    .map(|(_, rhs)| rhs.to_string())
                    .unwrap_or_else(|| name.clone()),
            ),
            _ => None,
        }?;
        let target_family = target_name
            .split("->")
            .next()
            .unwrap_or(target_name.as_str())
            .to_string();

        let template = self.impls.values().find_map(|imp| {
            let imp_name = match &imp.data_type.data_type {
                ParserInnerType::Struct(name) => {
                    name.rsplit_once(':').map(|(_, rhs)| rhs).unwrap_or(name)
                }
                ParserInnerType::StructWithGenerics { identifier, .. } => identifier,
                _ => return None,
            };
            let imp_family = imp_name.split("->").next().unwrap_or(imp_name);
            if imp_family == target_family && imp.variables.contains_key(member) {
                Some(imp.clone())
            } else {
                None
            }
        })?;

        let impl_key = self.get_or_create_impl(target.clone(), template.generic_params.clone());
        if let Some(new_impl) = self.impls.get_mut(&impl_key) {
            if new_impl.variables.is_empty() {
                new_impl.variables = template.variables.clone();
                new_impl.assoc_types = template.assoc_types.clone();
                new_impl.traits = template.traits.clone();
            }
        }

        resolve_from(self, &target, member).or_else(|| resolve_from(self, data_type, member))
    }

    fn resolve_type_from_ident(
        &mut self,
        scope: &u64,
        ident: &PotentialGenericTypeIdentifier,
    ) -> Option<ParserDataType> {
        if let PotentialGenericTypeIdentifier::Identifier(x) = ident {
            let builtin = match ParserInnerType::from_str(&x.to_string()) {
                Ok(builtin) => builtin,
                Err(_) => ParserInnerType::Struct(x.to_string()),
            };
            if !matches!(builtin, ParserInnerType::Struct(_)) {
                return Some(ParserDataType::new(*x.span(), builtin));
            }
        }
        self.resolve_potential_generic_ident_to_data_type(scope, ident)
    }

    pub fn evaluate_member_expression(
        &mut self,
        scope: &u64,
        span: Span,
        mut path: Vec<(Node, bool)>,
    ) -> Result<MiddleNode, MiddleErr> {
        if path.is_empty() {
            return Ok(MiddleNode {
                node_type: MiddleNodeType::EmptyLine,
                span,
            });
        }

        if path.len() > 1
            && let NodeType::Identifier(x) = &path[0].0.node_type
        {
            let resolved_ident = self.resolve_potential_generic_ident(scope, x);
            let base_has_value_binding = resolved_ident
                .as_ref()
                .map(|id| self.variables.contains_key(&id.text))
                .unwrap_or(false)
                || self
                    .resolve_str(scope, &x.to_string())
                    .as_ref()
                    .is_some_and(|name| self.variables.contains_key(name))
                || self.variables.contains_key(&x.to_string());
            let base_type = self.resolve_type_from_ident(scope, x);
            let base_is_value = base_has_value_binding;
            if let Some(Some(object)) = resolved_ident.as_ref().map(|x| self.objects.get(&x.text)) {
                match (&object.object_type, &path[1].0.node_type) {
                    (MiddleTypeDefType::Enum(variants), NodeType::Identifier(y))
                        if path.len() == 2 =>
                    {
                        let variant_name = y.to_string();
                        if let Some((canonical, _)) = variants
                            .iter()
                            .find(|(name, _)| name.text.eq_ignore_ascii_case(&variant_name))
                        {
                            return self.evaluate_inner(
                                scope,
                                Node::new(
                                    self.current_span(),
                                    NodeType::EnumExpression {
                                        identifier: x.clone(),
                                        value: canonical.clone().into(),
                                        data: None,
                                    },
                                ),
                            );
                        }
                    }
                    _ => {}
                }
            }

            if let Some(ty) = base_type {
                match &path[1].0.node_type {
                    NodeType::CallExpression {
                        string_fn,
                        caller,
                        generic_types,
                        args,
                        reverse_args,
                    } if !base_is_value => {
                        if let NodeType::Identifier(second) = &caller.node_type
                            && let Some(static_fn) =
                                self.resolve_impl_member(scope, &ty, &second.to_string())
                        {
                            let new_args = args.clone();
                            if path.len() == 2 {
                                return self.evaluate_inner(
                                    scope,
                                    Node::new(
                                        self.current_span(),
                                        NodeType::CallExpression {
                                            string_fn: string_fn.clone(),
                                            caller: Box::new(Node::new(
                                                self.current_span(),
                                                NodeType::Identifier(
                                                    PotentialGenericTypeIdentifier::Identifier(
                                                        ParserText::from(static_fn).into(),
                                                    ),
                                                ),
                                            )),
                                            generic_types: generic_types.clone(),
                                            args: new_args,
                                            reverse_args: reverse_args.clone(),
                                        },
                                    ),
                                );
                            }
                            let call_node = Node::new(
                                path[1].0.span,
                                NodeType::CallExpression {
                                    string_fn: string_fn.clone(),
                                    caller: Box::new(Node::new(
                                        self.current_span(),
                                        NodeType::Identifier(
                                            PotentialGenericTypeIdentifier::Identifier(
                                                ParserText::from(static_fn).into(),
                                            ),
                                        ),
                                    )),
                                    generic_types: generic_types.clone(),
                                    args: new_args,
                                    reverse_args: reverse_args.clone(),
                                },
                            );
                            path[0].0 = call_node;
                            path.remove(1);
                        }
                    }
                    NodeType::Identifier(ident) if !base_is_value => {
                        let ident = self.resolve_dollar_ident_potential_generic_only(scope, ident);
                        if let Some(ident) = ident
                            && let Some(var) = self.resolve_impl_member(scope, &ty, &ident.text)
                        {
                            if path.len() == 2 {
                                return self.evaluate_inner(
                                    scope,
                                    Node::new(
                                        self.current_span(),
                                        NodeType::Identifier(
                                            PotentialGenericTypeIdentifier::Identifier(
                                                ParserText::from(var).into(),
                                            ),
                                        ),
                                    ),
                                );
                            }
                            let ident_node = Node::new(
                                path[1].0.span,
                                NodeType::Identifier(PotentialGenericTypeIdentifier::Identifier(
                                    ParserText::from(var).into(),
                                )),
                            );
                            path[0].0 = ident_node;
                            path.remove(1);
                        }
                    }
                    _ => {}
                }
            }
        }

        let first = path.remove(0);
        let mut list = vec![(self.evaluate(scope, first.0), first.1)];

        let path_len = path.len();
        for (i, item) in path.into_iter().enumerate() {
            if item.1 {
                let base_node = MiddleNode::new(
                    MiddleNodeType::MemberExpression { path: list.clone() },
                    self.current_span(),
                );
                if let Some(overloaded) = self.handle_operator_overloads(
                    scope,
                    item.0.span,
                    base_node.clone().into(),
                    item.0.clone(),
                    crate::environment::Operator::Index,
                )? {
                    list = vec![(overloaded, false)];
                    continue;
                }
            }
            list.push((
                match item.0.node_type {
                    NodeType::Identifier(_) if item.1 => self.evaluate(scope, item.0),
                    NodeType::Identifier(x) if i == 0 => {
                        let first = list.first().cloned().ok_or_else(|| {
                            MiddleErr::At(
                                item.0.span,
                                Box::new(MiddleErr::Internal(
                                    "missing base for member expression".to_string(),
                                )),
                            )
                        })?;
                        let unresolved = match &x {
                            PotentialGenericTypeIdentifier::Identifier(id) => {
                                ParserText::from(id.to_string())
                            }
                            PotentialGenericTypeIdentifier::Generic { identifier, .. } => {
                                ParserText::from(identifier.to_string())
                            }
                        };

                        if let Some(ty) = self.member_base_type(scope, &first.0)
                            && let Some(static_var) =
                                self.resolve_impl_member(scope, &ty, unresolved.text.as_str())
                        {
                            self.evaluate_inner(
                                scope,
                                Node::new(
                                    self.current_span(),
                                    NodeType::Identifier(
                                        PotentialGenericTypeIdentifier::Identifier(
                                            ParserText::from(static_var).into(),
                                        ),
                                    ),
                                ),
                            )?
                        } else {
                            MiddleNode {
                                node_type: MiddleNodeType::Identifier(unresolved),
                                span,
                            }
                        }
                    }
                    NodeType::Identifier(x) => {
                        let unresolved = match &x {
                            PotentialGenericTypeIdentifier::Identifier(id) => {
                                ParserText::from(id.to_string())
                            }
                            PotentialGenericTypeIdentifier::Generic { identifier, .. } => {
                                ParserText::from(identifier.to_string())
                            }
                        };
                        MiddleNode {
                            node_type: MiddleNodeType::Identifier(unresolved),
                            span,
                        }
                    }
                    NodeType::CallExpression {
                        string_fn: _,
                        caller,
                        generic_types,
                        args,
                        reverse_args,
                    } if !item.1 => {
                        let target_type = list
                            .last()
                            .and_then(|(last_node, _)| self.member_base_type(scope, last_node))
                            .or_else(|| {
                                self.resolve_type_from_node(
                                    scope,
                                    &MiddleNode::new(
                                        MiddleNodeType::MemberExpression { path: list.clone() },
                                        self.current_span(),
                                    )
                                    .into(),
                                )
                                .map(|x| x.unwrap_all_refs())
                            });

                        let receiver_is_value = list
                            .last()
                            .map(|(n, _)| self.member_base_is_value(scope, n))
                            .unwrap_or(true);

                        let call_node = self.evaluate_explicit_member_call(
                            scope,
                            &list,
                            caller,
                            generic_types,
                            args,
                            reverse_args,
                            receiver_is_value,
                            target_type,
                        )?;

                        if i + 1 == path_len {
                            return Ok(call_node);
                        }

                        call_node
                    }
                    _ => self.evaluate(scope, item.0),
                },
                item.1,
            ));
        }

        Ok(MiddleNode {
            node_type: MiddleNodeType::MemberExpression { path: list },
            span,
        })
    }
}
