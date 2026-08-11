use crate::{
    ast::{MiddleNode, MiddleNodeType},
    environment::MiddleEnvironment,
    errors::MiddleErr,
    symbols::MiddleVariable,
    typing::MiddleTypeDefType,
};
use calibre_parser::{
    Span,
    ast::{
        Operator, RefMutability,
        idents::{ParserText, PotentialDollarIdentifier, PotentialGenericTypeIdentifier},
        nodes::{CallArg, Node, NodeType},
        types::{ParserDataType, ParserInnerType, PotentialNewType},
    },
};
use std::str::FromStr;

impl MiddleEnvironment {
    fn first_param_ref_mutability(&self, function_name: &str) -> Option<RefMutability> {
        let from_var = |var: &MiddleVariable| -> Option<RefMutability> {
            let ParserInnerType::Function { parameters, .. } = &var.data_type.data_type else {
                return None;
            };
            let first = parameters.first()?;
            match &first.data_type {
                ParserInnerType::Ref(_, mutability) => Some(*mutability),
                _ => None,
            }
        };

        if let Some(var) = self.symbols.variables.get(function_name)
            && let Some(m) = from_var(var)
        {
            return Some(m);
        }

        let short = function_name.rsplit("::").next().unwrap_or(function_name);
        let mut found: Option<RefMutability> = None;
        for (name, var) in &self.symbols.variables {
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
    fn dedupe_receiver_args(args: &mut Vec<MiddleNode>) {
        if args.len() < 2 {
            return;
        }
        let receiver = args[0].clone();
        let receiver_txt = receiver.to_string();
        let mut i = 1usize;
        while i < args.len() {
            let same = Self::text_matches(&receiver, &args[i]);
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
        _generic_types: Vec<PotentialNewType>,
        args: Vec<CallArg>,
        reverse_args: Vec<Node>,
        receiver_is_value: bool,
        target_type: Option<ParserDataType>,
    ) -> Result<MiddleNode, MiddleErr> {
        let receiver_middle = if list.len() <= 1 {
            list.first()
                .map(|(node, _)| node.clone())
                .unwrap_or_else(|| {
                    MiddleNode::new(MiddleNodeType::EmptyLine, self.context.current_span())
                })
        } else {
            MiddleNode::new(
                MiddleNodeType::MemberExpression {
                    path: list.to_vec(),
                },
                self.context.current_span(),
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
                if self.symbols.variables.contains_key(&candidate) {
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
                                if self.symbols.variables.contains_key(&candidate) {
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
            if ParserText::is_temp_name(&name) {
                resolved_caller = Some(name);
            }
        }

        let resolved_caller = resolved_caller.map(|name| Self::normalize_member_path_name(&name));

        if let Some(function_name) = resolved_caller {
            let span = self.context.current_span();
            let caller_node = Node::identifier(span, function_name.clone());
            let data_type = self
                .symbols
                .variables
                .get(&function_name)
                .map(|var| var.data_type.clone().unwrap_all_refs().data_type);

            let mut defaulted_args = args.clone();
            if receiver_is_value {
                defaulted_args.insert(0, CallArg::Value(receiver_node.clone()));
            }

            if let Some(mut lowered_args) = self.lower_defaulted_call_args(
                scope,
                span,
                &caller_node,
                &data_type,
                defaulted_args,
                reverse_args.clone(),
            ) {
                if receiver_is_value
                    && let Some(mutability) =
                        self.first_param_ref_mutability(&function_name).or_else(|| {
                            let is_self_ident = matches!(
                                &receiver_middle.node_type,
                                MiddleNodeType::Identifier(name)
                                    if name.text == "self" || name.text.ends_with(":self")
                            );
                            is_self_ident.then_some(RefMutability::MutRef)
                        })
                {
                    lowered_args[0] = MiddleNode::new(
                        MiddleNodeType::RefStatement {
                            mutability,
                            value: Box::new(lowered_args[0].clone()),
                        },
                        span,
                    );
                }

                Self::dedupe_receiver_args(&mut lowered_args);

                return Ok(MiddleNode::new(
                    MiddleNodeType::CallExpression {
                        caller: Box::new(MiddleNode::identifier(span, function_name)),
                        args: lowered_args,
                    },
                    span,
                ));
            }

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
                        self.context.current_span(),
                    );
                }

                self.lower_call_args_with_receiver(scope, self_arg, args, Vec::new())
            } else {
                self.lower_call_args(scope, args, reverse_args)
            };
            Self::dedupe_receiver_args(&mut lowered_args);

            Ok(MiddleNode::new(
                MiddleNodeType::CallExpression {
                    caller: Box::new(MiddleNode::identifier(
                        self.context.current_span(),
                        function_name,
                    )),
                    args: lowered_args,
                },
                self.context.current_span(),
            ))
        } else {
            if let NodeType::Identifier(member_ident) = &caller.node_type {
                let qualified = member_ident.to_string();
                if ParserText::is_temp_name(&qualified) {
                    let lowered_args = self.lower_call_args_with_receiver(
                        scope,
                        receiver_middle,
                        args,
                        reverse_args,
                    );

                    return Ok(MiddleNode::new(
                        MiddleNodeType::CallExpression {
                            caller: Box::new(MiddleNode::identifier(
                                self.context.current_span(),
                                qualified,
                            )),
                            args: lowered_args,
                        },
                        self.context.current_span(),
                    ));
                }
            }

            let member_call_caller = Node::new(
                self.context.current_span(),
                NodeType::MemberExpression {
                    path: vec![(receiver_node, false), (*caller, false)],
                },
            );

            let mut lowered_args = self.lower_call_args(scope, args, reverse_args);
            let lowered_caller = self.evaluate(scope, member_call_caller);

            if let MiddleNodeType::MemberExpression { path } = &lowered_caller.node_type
                && path.len() == 2
                && let MiddleNodeType::Identifier(qualified) = &path[1].0.node_type
                && ParserText::is_temp_name(&qualified)
            {
                let receiver = path[0].0.clone();

                if let Some(first_arg) = lowered_args.first()
                    && Self::text_matches(&receiver, first_arg)
                {
                    lowered_args.remove(0);
                }

                let mut call_args = vec![receiver];
                call_args.extend(lowered_args);

                return Ok(MiddleNode::new(
                    MiddleNodeType::CallExpression {
                        caller: Box::new(MiddleNode::identifier(
                            self.context.current_span(),
                            qualified.clone(),
                        )),
                        args: call_args,
                    },
                    self.context.current_span(),
                ));
            }

            Ok(MiddleNode::new(
                MiddleNodeType::CallExpression {
                    caller: Box::new(lowered_caller),
                    args: lowered_args,
                },
                self.context.current_span(),
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

        for imp in self.typing.impls.values() {
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
        for key in self.symbols.variables.keys() {
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
                    .and_then(|resolved| self.symbols.variables.get(&resolved.text))
                    .or_else(|| {
                        self.resolve_str(scope, &ident.text)
                            .and_then(|resolved| self.symbols.variables.get(&resolved))
                    })
                    .or_else(|| self.symbols.variables.get(&ident.text))
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
                    && let Some(var) = self.symbols.variables.get(&ident.text)
                    && let ParserInnerType::Function { return_type, .. } = &var.data_type.data_type
                {
                    return Some((*return_type.clone()).unwrap_all_refs());
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
                .is_some_and(|resolved| self.symbols.variables.contains_key(&resolved.text))
                || self
                    .resolve_str(scope, &ident.text)
                    .is_some_and(|resolved| self.symbols.variables.contains_key(&resolved))
                || self.symbols.variables.contains_key(&ident.text)
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
            let key = key.key();
            env.typing
                .impls
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
            env.symbols.variables.iter().find_map(|(name, var)| {
                let name = ParserText::get_temp_name_prefix(name)?;
                if !(name.ends_with(&format!("::{member}")) || name.ends_with(&format!(".{member}"))) {
                    return None;
                }

                let param_inner = first_param_inner(&var.data_type)?;
                if param_inner.matches(target_inner, &Vec::new()) {
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

        fn normalize_owner(owner: impl ToString) -> String {
            let mut cur = owner.to_string();
            loop {
                if !ParserText::is_temp_name(&cur) {
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
            _ => Some(target_inner.to_string()),
            ParserInnerType::StructWithGenerics { identifier, .. } => {
                Some(identifier.to_string())
            }
            _ => None,
        };

        if let Some(target_family) = &target_family
            && let Some(found) = self.symbols.variables.keys().find(|name| {
                let Some((owner, meth)) = name.rsplit_once(".") else {
                    return false;
                };
                if meth != member {
                    return false;
                }

                if target_family == "list" {
                    return owner.starts_with("list:<") || owner.starts_with("list:<");
                }
                owner == target_family
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
            return Some(format!("{target_family}.{member}"));
        }

        let target = resolved.clone();

        let template = self.typing.impls.values().find_map(|imp| {
            let imp_name = match &imp.data_type.data_type {
                ParserInnerType::Struct(name) => name,
                ParserInnerType::StructWithGenerics { identifier, .. } => identifier,
                _ => return None,
            };
            let imp_family = ParserText::get_temp_name_prefix(imp_name);
            if imp_family == target_family && imp.variables.contains_key(member) {
                Some(imp.clone())
            } else {
                None
            }
        })?;

        let impl_key = self.typing.get_or_create_impl(
            target.clone(),
            template.generic_params.clone(),
            self.context.current_location.clone(),
        );
        if let Some(new_impl) = self.typing.impls.get_mut(&impl_key)
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
                .map(|id| self.symbols.variables.contains_key(&id.text))
                .unwrap_or(false)
                || self
                    .resolve_str(scope, &x.to_string())
                    .as_ref()
                    .is_some_and(|name| self.symbols.variables.contains_key(name))
                || self.symbols.variables.contains_key(&x.to_string());
            let base_type = self.resolve_type_from_ident(scope, x);
            let base_is_value = base_has_value_binding;
            if let Some(Some(object)) = resolved_ident
                .as_ref()
                .map(|x| self.typing.objects.get(&x.text))
            {
                match (&object.object_type, &path[1].0.node_type) {
                    (MiddleTypeDefType::Enum { variants, .. }, NodeType::Identifier(y))
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
                                    self.context.current_span(),
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
                                        self.context.current_span(),
                                        NodeType::CallExpression {
                                            string_fn: string_fn.clone(),
                                            caller: Box::new(Node::identifier(
                                                self.context.current_span(),
                                                static_fn,
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
                                    caller: Box::new(Node::identifier(
                                        self.context.current_span(),
                                        static_fn,
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
                                    Node::identifier(self.context.current_span(), var),
                                );
                            }
                            let ident_node = Node::identifier(path[1].0.span, var);
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
                        MiddleNode::new(MiddleNodeType::EmptyLine, self.context.current_span())
                    })
                } else {
                    MiddleNode::new(
                        MiddleNodeType::MemberExpression { path: list.clone() },
                        self.context.current_span(),
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
                    self.context.current_span(),
                );
                if let Some(overloaded) = self.handle_operator_overloads(
                    scope,
                    item.0.span,
                    base_node.clone().into(),
                    item.0.clone(),
                    Operator::Index,
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
                                Node::identifier(self.context.current_span(), static_var),
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
