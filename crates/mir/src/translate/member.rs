use calibre_parser::{
    Span,
    ast::{
        CallArg, Node, NodeType, ParserDataType, ParserInnerType, ParserText,
        PotentialDollarIdentifier, PotentialGenericTypeIdentifier, RefMutability,
    },
};
use std::str::FromStr;

use crate::{
    ast::{MiddleNode, MiddleNodeType},
    environment::{MiddleEnvironment, MiddleTypeDefType},
    errors::MiddleErr,
};

impl MiddleEnvironment {
    fn first_param_ref_mutability(&self, function_name: &str) -> Option<RefMutability> {
        let from_var = |var: &crate::environment::MiddleVariable| -> Option<RefMutability> {
            let ParserInnerType::Function { parameters, .. } = &var.data_type.data_type else {
                return None;
            };
            let first = parameters.first()?;
            match &first.data_type {
                ParserInnerType::Ref(_, mutability) => Some(*mutability),
                _ => None,
            }
        };

        if let Some(var) = self.variables.get(function_name)
            && let Some(m) = from_var(var)
        {
            return Some(m);
        }

        let short = function_name.rsplit("::").next().unwrap_or(function_name);
        let mut found: Option<RefMutability> = None;
        for (name, var) in &self.variables {
            if !name.ends_with(&format!("::{short}")) {
                continue;
            }
            if let Some(m) = from_var(var) {
                if let Some(prev) = found {
                    if prev != m {
                        return None;
                    }
                } else {
                    found = Some(m);
                }
            }
        }
        found
    }

    #[inline]
    fn same_node_text(a: &MiddleNode, b: &MiddleNode) -> bool {
        Self::text_matches(&a.to_string(), &b.to_string())
    }

    #[inline]
    fn dedupe_receiver_args(args: &mut Vec<MiddleNode>) {
        if args.len() < 2 {
            return;
        }
        let receiver = args[0].clone();
        let receiver_txt = receiver.to_string();
        let mut i = 1usize;
        while i < args.len() {
            let same = Self::same_node_text(&receiver, &args[i]);
            let same_as_receiver_ref = matches!(
                &args[i].node_type,
                MiddleNodeType::RefStatement { value, .. } if value.to_string() == receiver_txt
            );
            let receiver_is_ref_of_current = matches!(
                &receiver.node_type,
                MiddleNodeType::RefStatement { value, .. } if value.to_string() == args[i].to_string()
            );
            if same || same_as_receiver_ref || receiver_is_ref_of_current {
                args.remove(i);
            } else {
                i += 1;
            }
        }
    }

    #[inline]
    fn normalize_member_path_name(name: &str) -> String {
        let mut parts: Vec<&str> = name.split("::").collect();
        let mut i = 1;

        while i < parts.len() {
            if parts[i] == parts[i - 1] {
                parts.remove(i);
            } else {
                i += 1;
            }
        }

        let mut out = parts.join("::");

        if let Some((lhs, rhs)) = out.split_once("::")
            && rhs.starts_with(&format!("{lhs}:<"))
        {
            out = rhs.to_string();
        }

        if let Some((_, rhs)) = out.rsplit_once('.')
            && rhs.contains("::")
        {
            out = rhs.to_string();
        }

        out
    }

    #[inline]
    pub(crate) fn lower_call_args(
        &mut self,
        scope: &u64,
        args: Vec<CallArg>,
        reverse_args: Vec<Node>,
    ) -> Vec<MiddleNode> {
        let mut lowered = Vec::with_capacity(args.len() + reverse_args.len());

        for arg in args {
            lowered.push(self.evaluate(scope, arg.into()));
        }

        for arg in reverse_args {
            lowered.push(self.evaluate(scope, arg));
        }

        lowered
    }

    #[inline]
    fn lower_call_args_with_receiver(
        &mut self,
        scope: &u64,
        receiver: MiddleNode,
        args: Vec<CallArg>,
        reverse_args: Vec<Node>,
    ) -> Vec<MiddleNode> {
        let mut lowered = Vec::with_capacity(args.len() + reverse_args.len() + 1);
        lowered.push(receiver);
        lowered.extend(self.lower_call_args(scope, args, reverse_args));
        lowered
    }

    #[inline]
    fn unresolved_ident_text(ident: &PotentialGenericTypeIdentifier) -> ParserText {
        match ident {
            PotentialGenericTypeIdentifier::Identifier(id) => ParserText::from(id.to_string()),
            PotentialGenericTypeIdentifier::Generic { identifier, .. } => {
                ParserText::from(identifier.to_string())
            }
        }
    }

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
        let receiver_middle = if list.len() <= 1 {
            list.first()
                .map(|(node, _)| node.clone())
                .unwrap_or_else(|| MiddleNode::new(MiddleNodeType::EmptyLine, self.current_span()))
        } else {
            MiddleNode::new(
                MiddleNodeType::MemberExpression {
                    path: list.to_vec(),
                },
                self.current_span(),
            )
        };
        let receiver_node: Node = receiver_middle.clone().into();

        let type_style_member = if !receiver_is_value {
            if let (NodeType::Identifier(receiver_ident), NodeType::Identifier(member_ident)) =
                (&receiver_node.node_type, &caller.node_type)
            {
                let receiver_name = receiver_ident.to_string();
                let member_name = member_ident.to_string();
                let candidate = format!("{receiver_name}::{member_name}");
                if self.variables.contains_key(&candidate) {
                    Some(candidate)
                } else {
                    self.resolve_str(scope, &candidate)
                }
            } else {
                None
            }
        } else {
            None
        };

        let mut resolved_caller = type_style_member.or_else(|| {
            if let NodeType::Identifier(member_ident) = &caller.node_type {
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
                        list.last().and_then(|(base, _)| {
                            self.resolve_member_from_chain_family(base, &name)
                        })
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
                    .or_else(|| self.resolve_str(scope, &name))
            } else {
                None
            }
        });

        if resolved_caller.is_none()
            && let NodeType::Identifier(member_ident) = &caller.node_type
        {
            let name = member_ident.to_string();
            if name.contains("::") {
                resolved_caller = Some(name);
            }
        }

        let resolved_caller = resolved_caller.map(|name| Self::normalize_member_path_name(&name));

        if let Some(function_name) = resolved_caller {
            let mut lowered_args = if receiver_is_value {
                let mut self_arg = receiver_middle;

                let inferred_mutability =
                    self.first_param_ref_mutability(&function_name).or_else(|| {
                        let is_self_ident = matches!(
                            &self_arg.node_type,
                            MiddleNodeType::Identifier(name)
                                if name.text == "self" || name.text.ends_with(":self")
                        );
                        is_self_ident.then_some(RefMutability::MutRef)
                    });

                if let Some(mutability) = inferred_mutability {
                    self_arg = MiddleNode::new(
                        MiddleNodeType::RefStatement {
                            mutability,
                            value: Box::new(self_arg),
                        },
                        self.current_span(),
                    );
                }

                self.lower_call_args_with_receiver(scope, self_arg, args, Vec::new())
            } else {
                self.lower_call_args(scope, args, reverse_args)
            };
            Self::dedupe_receiver_args(&mut lowered_args);

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
            if let NodeType::Identifier(member_ident) = &caller.node_type {
                let qualified = member_ident.to_string();
                if qualified.contains("::") {
                    let lowered_args = self.lower_call_args_with_receiver(
                        scope,
                        receiver_middle,
                        args,
                        reverse_args,
                    );
                    return Ok(MiddleNode::new(
                        MiddleNodeType::CallExpression {
                            caller: Box::new(MiddleNode::new(
                                MiddleNodeType::Identifier(ParserText::from(qualified)),
                                self.current_span(),
                            )),
                            args: lowered_args,
                        },
                        self.current_span(),
                    ));
                }
            }

            let member_call_caller = Node::new(
                self.current_span(),
                NodeType::MemberExpression {
                    path: vec![(receiver_node, false), (*caller, false)],
                },
            );

            let mut lowered_args = self.lower_call_args(scope, args, reverse_args);
            let lowered_caller = self.evaluate(scope, member_call_caller);

            if let MiddleNodeType::MemberExpression { path } = &lowered_caller.node_type
                && path.len() == 2
                && let MiddleNodeType::Identifier(qualified) = &path[1].0.node_type
                && qualified.text.contains("::")
            {
                let receiver = path[0].0.clone();

                if let Some(first_arg) = lowered_args.first()
                    && Self::same_node_text(&receiver, first_arg)
                {
                    lowered_args.remove(0);
                }

                let mut call_args = vec![receiver];
                call_args.extend(lowered_args);

                return Ok(MiddleNode::new(
                    MiddleNodeType::CallExpression {
                        caller: Box::new(MiddleNode::new(
                            MiddleNodeType::Identifier(qualified.clone()),
                            self.current_span(),
                        )),
                        args: call_args,
                    },
                    self.current_span(),
                ));
            }

            Ok(MiddleNode::new(
                MiddleNodeType::CallExpression {
                    caller: Box::new(lowered_caller),
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
                if let Some(var) = resolved_ident
                    .as_ref()
                    .and_then(|resolved| self.variables.get(&resolved.text))
                    .or_else(|| {
                        self.resolve_str(scope, &ident.text)
                            .and_then(|resolved| self.variables.get(&resolved))
                    })
                    .or_else(|| self.variables.get(&ident.text))
                {
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
            if self
                .resolve_dollar_ident_only(
                    scope,
                    &PotentialDollarIdentifier::Identifier(ident.clone()),
                )
                .as_ref()
                .is_some_and(|resolved| self.variables.contains_key(&resolved.text))
                || self
                    .resolve_str(scope, &ident.text)
                    .is_some_and(|resolved| self.variables.contains_key(&resolved))
                || self.variables.contains_key(&ident.text)
            {
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
        let impl_var = |env: &MiddleEnvironment, key: &ParserDataType, m: &str| {
            let key = env.impl_key(key);
            env.impls
                .get(&key)
                .and_then(|imp| imp.variables.get(m))
                .map(|(mapped, _)| mapped.clone())
        };
        let first_param_inner = |ty: &ParserDataType| {
            let ParserInnerType::Function { parameters, .. } = &ty.data_type else {
                return None;
            };
            let first = parameters.first()?;
            Some(match &first.data_type {
                ParserInnerType::Ref(inner, _) => inner.data_type.clone(),
                other => other.clone(),
            })
        };
        let find_impl_var_by_param = |env: &MiddleEnvironment,
                                      target_inner: &ParserInnerType,
                                      member: &str,
                                      match_tail: bool| {
            env.variables.iter().find_map(|(name, var)| {
                if match_tail {
                    if calibre_parser::qualified_name_tail(name) != member {
                        return None;
                    }
                } else if !name.ends_with(&format!("::{member}")) {
                    return None;
                }
                let param_inner = first_param_inner(&var.data_type)?;
                if env.impl_type_matches(&param_inner, target_inner, &Vec::new()) {
                    Some(name.clone())
                } else {
                    None
                }
            })
        };

        let resolved = self
            .resolve_data_type(scope, data_type.clone())
            .unwrap_all_refs();
        if let Some(mapped) = impl_var(self, &resolved, member) {
            return Some(mapped);
        }
        if let Some(mapped) = impl_var(self, data_type, member) {
            return Some(mapped);
        }
        if let Some(found) =
            resolve_from(self, &resolved, member).or_else(|| resolve_from(self, data_type, member))
        {
            return Some(found);
        }

        let target_inner = resolved.clone().unwrap_all_refs().data_type;
        if let Some(found) = find_impl_var_by_param(self, &target_inner, member, false) {
            return Some(found);
        }
        if let Some(found) = find_impl_var_by_param(self, &target_inner, member, true) {
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
            ParserInnerType::Int
            | ParserInnerType::UInt
            | ParserInnerType::Byte
            | ParserInnerType::Float
            | ParserInnerType::Bool
            | ParserInnerType::Char
            | ParserInnerType::Dynamic
            | ParserInnerType::Null
            | ParserInnerType::Range
            | ParserInnerType::Str => Some(target_inner.to_string()),
            ParserInnerType::List(_) => Some("list".to_string()),
            ParserInnerType::DynamicTraits(_) => Some("dyn".to_string()),
            ParserInnerType::Struct(name) => {
                Some(calibre_parser::qualified_name_base(&normalize_owner(name)).to_string())
            }
            ParserInnerType::StructWithGenerics { identifier, .. } => {
                Some(calibre_parser::qualified_name_base(&normalize_owner(identifier)).to_string())
            }
            _ => None,
        };
        if let Some(target_family) = &target_family
            && let Some(found) = self.variables.keys().find(|name| {
                let Some((owner, meth)) = name.rsplit_once("::") else {
                    return false;
                };
                if meth != member {
                    return false;
                }
                let owner = normalize_owner(owner);
                let owner_base = calibre_parser::qualified_name_base(&owner);
                let owner_family = owner_base
                    .rsplit_once("::")
                    .map(|(_, rhs)| rhs)
                    .unwrap_or(owner_base);
                if target_family == "list" {
                    return owner_family.starts_with("list:<") || owner.starts_with("list:<");
                }
                owner_family == target_family
            })
        {
            return Some(found.clone());
        }

        if let Some(target_family) = target_family.as_ref()
            && matches!(
                &target_inner,
                ParserInnerType::Int
                    | ParserInnerType::UInt
                    | ParserInnerType::Byte
                    | ParserInnerType::Float
                    | ParserInnerType::Bool
                    | ParserInnerType::Char
                    | ParserInnerType::Str
                    | ParserInnerType::Range
                    | ParserInnerType::List(_)
            )
        {
            return Some(format!("{target_family}::{member}"));
        }

        let target = resolved.clone();
        let target_name = match &target.data_type {
            ParserInnerType::Struct(name) => {
                Some(calibre_parser::qualified_name_tail(name).to_string())
            }
            _ => None,
        }?;
        let target_family = calibre_parser::qualified_name_base(&target_name).to_string();

        let template = self.impls.values().find_map(|imp| {
            let imp_name = match &imp.data_type.data_type {
                ParserInnerType::Struct(name) => calibre_parser::qualified_name_tail(name),
                ParserInnerType::StructWithGenerics { identifier, .. } => identifier,
                _ => return None,
            };
            let imp_family = calibre_parser::qualified_name_base(imp_name);
            if imp_family == target_family && imp.variables.contains_key(member) {
                Some(imp.clone())
            } else {
                None
            }
        })?;

        let impl_key = self.get_or_create_impl(target.clone(), template.generic_params.clone());
        if let Some(new_impl) = self.impls.get_mut(&impl_key)
            && new_impl.variables.is_empty()
        {
            new_impl.variables = template.variables.clone();
            new_impl.assoc_types = template.assoc_types.clone();
            new_impl.traits = template.traits.clone();
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
            if let NodeType::CallExpression {
                string_fn: _,
                caller,
                generic_types,
                args,
                reverse_args,
            } = item.0.node_type.clone()
                && !item.1
            {
                let receiver_expr = if list.len() <= 1 {
                    list.first().map(|(n, _)| n.clone()).unwrap_or_else(|| {
                        MiddleNode::new(MiddleNodeType::EmptyLine, self.current_span())
                    })
                } else {
                    MiddleNode::new(
                        MiddleNodeType::MemberExpression { path: list.clone() },
                        self.current_span(),
                    )
                };

                let target_type = self
                    .resolve_type_from_node(scope, &receiver_expr.clone().into())
                    .map(|x| x.unwrap_all_refs())
                    .or_else(|| self.member_base_type(scope, &receiver_expr));

                let receiver_is_value = self.member_base_is_value(scope, &receiver_expr);

                let receiver_txt = receiver_expr.to_string();
                let mut args = args;
                let mut reverse_args = reverse_args;
                if let Some(first_arg) = args.first() {
                    let first_txt: Node = first_arg.clone().into();
                    let first_txt = first_txt.to_string();
                    if first_txt == receiver_txt
                        || first_txt.ends_with(&format!(".{receiver_txt}"))
                        || receiver_txt.ends_with(&format!(".{first_txt}"))
                    {
                        args.remove(0);
                    }
                }
                if let Some(first_reverse) = reverse_args.first() {
                    let rev_txt = first_reverse.to_string();
                    if rev_txt == receiver_txt
                        || rev_txt.ends_with(&format!(".{receiver_txt}"))
                        || receiver_txt.ends_with(&format!(".{rev_txt}"))
                    {
                        reverse_args.remove(0);
                    }
                }

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

                list = vec![(call_node, false)];
                continue;
            }

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
                        let x = self
                            .resolve_dollar_ident_potential_generic_only(scope, &x)
                            .unwrap_or_else(|| Self::unresolved_ident_text(&x));

                        if let Some(ty) = self.member_base_type(scope, &first.0)
                            && let Some(static_var) = self.resolve_impl_member(scope, &ty, &x.text)
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
                                node_type: MiddleNodeType::Identifier(x),
                                span,
                            }
                        }
                    }
                    NodeType::Identifier(x) => {
                        let resolved = self
                            .resolve_dollar_ident_potential_generic_only(scope, &x)
                            .unwrap_or_else(|| Self::unresolved_ident_text(&x));
                        MiddleNode {
                            node_type: MiddleNodeType::Identifier(resolved),
                            span,
                        }
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
