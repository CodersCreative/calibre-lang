use crate::{
    environment::MiddleEnvironment,
    errors::MiddleErr,
    symbols::MiddleOverload,
    typing::{MiddleObject, MiddleTrait, MiddleTypeDefType},
};
use calibre_parser::{
    Span,
    ast::{
        Operator,
        idents::{ParserText, PotentialDollarIdentifier, PotentialGenericTypeIdentifier},
        nodes::NodeType,
        types::{ParserDataType, ParserInnerType, PotentialNewType},
    },
};
use rustc_hash::{FxHashMap, FxHashSet};
use std::str::FromStr;

impl MiddleEnvironment {
    pub fn resolve_member_fn_type(
        &self,
        ty: &ParserDataType,
        member: &str,
    ) -> Option<ParserDataType> {
        self.resolve_member_fn_name(ty, member)
            .and_then(|name| self.symbols.variables.get(&name))
            .map(|var| var.data_type.clone())
    }

    pub fn resolve_member_field_type(
        &mut self,
        scope: &u64,
        base: &ParserDataType,
        member: &str,
        span: Span,
    ) -> Option<ParserDataType> {
        fn trait_member_type(
            defs: &FxHashMap<String, MiddleTrait>,
            trait_name: &str,
            member: &str,
        ) -> Option<ParserDataType> {
            let root = defs
                .iter()
                .find(|(name, _)| calibre_parser::qualified_name_matches(name, trait_name))
                .map(|(name, _)| name.clone())?;
            let mut stack = vec![root];
            let mut visited = FxHashSet::default();

            while let Some(current) = stack.pop() {
                if !visited.insert(current.clone()) {
                    continue;
                }
                let Some(def) = defs.get(&current) else {
                    continue;
                };
                if let Some(m) = def.members.get(member) {
                    return Some(m.data_type.clone());
                }
                for implied in &def.implied_traits {
                    if let Some((resolved, _)) = defs
                        .iter()
                        .find(|(name, _)| calibre_parser::qualified_name_matches(name, implied))
                    {
                        stack.push(resolved.clone());
                    } else {
                        stack.push(implied.clone());
                    }
                }
            }

            None
        }

        let resolved = self
            .resolve_data_type(scope, base.clone())
            .unwrap_all_refs();
        let out = match &resolved.data_type {
            ParserInnerType::Struct(struct_name) => self
                .typing
                .find_object_for_struct_name(struct_name)
                .and_then(|obj| match &obj.object_type {
                    MiddleTypeDefType::Struct(fields) => {
                        fields.get(member).map(|(ty, _)| ty.clone())
                    }
                    _ => None,
                }),
            ParserInnerType::StructWithGenerics { identifier, .. } => self
                .typing
                .find_object_for_struct_name(identifier)
                .and_then(|obj| match &obj.object_type {
                    MiddleTypeDefType::Struct(fields) => {
                        fields.get(member).map(|(ty, _)| ty.clone())
                    }
                    _ => None,
                }),
            ParserInnerType::Tuple(values) => member
                .parse::<usize>()
                .ok()
                .and_then(|idx| values.get(idx).cloned()),
            ParserInnerType::Option(inner) | ParserInnerType::Ptr(inner)
                if member == "next" || member == "0" =>
            {
                Some((**inner).clone())
            }
            ParserInnerType::Result { ok, err } => {
                if member == "ok" || member == "0" {
                    Some((**ok).clone())
                } else if member == "err" || member == "1" {
                    Some((**err).clone())
                } else if member == "next" {
                    if ok.data_type == err.data_type {
                        Some((**ok).clone())
                    } else {
                        Some(ParserDataType::new(span, ParserInnerType::Dynamic))
                    }
                } else {
                    None
                }
            }
            ParserInnerType::DynamicTraits(traits) => {
                for tr in traits {
                    if let Some(found) = trait_member_type(&self.typing.trait_defs, tr, member) {
                        return Some(found);
                    }
                }
                None
            }
            _ => None,
        };

        if out.is_some() {
            return out;
        }

        if let Some(imp) = self.typing.find_impl_for_type(&resolved)
            && let Some((mapped_name, _)) = imp.variables.get(member)
        {
            return self
                .symbols
                .variables
                .get(mapped_name)
                .map(|var| var.data_type.clone());
        }

        None
    }

    pub fn resolve_member_fn_name(&self, ty: &ParserDataType, member: &str) -> Option<String> {
        self.typing
            .member_fn_candidates(ty, member)
            .into_iter()
            .find(|name| {
                self.symbols.variables.get(name).is_some_and(|var| {
                    matches!(
                        var.data_type.data_type,
                        ParserInnerType::Function { .. } | ParserInnerType::NativeFunction(_)
                    )
                })
            })
    }

    pub fn resolve_str(&self, scope: &u64, iden: &str) -> Option<String> {
        if self.symbols.variables.contains_key(iden) || self.typing.objects.contains_key(iden) {
            return Some(iden.to_string());
        }

        let scope_ref = self.scoping.scopes.get(scope)?;

        let current_mapping = scope_ref.mappings.get(iden).cloned();
        if current_mapping.is_none() {
            let mut parent_id = scope_ref.parent;
            while let Some(parent) = parent_id {
                let Some(parent_scope) = self.scoping.scopes.get(&parent) else {
                    break;
                };
                if let Some(mapped) = parent_scope.mappings.get(iden) {
                    return Some(mapped.clone());
                }
                parent_id = parent_scope.parent;
            }
        }
        if let Some(current) = current_mapping {
            let mut parent_id = scope_ref.parent;
            let mut root_mapping: Option<String> = None;
            while let Some(parent) = parent_id {
                let Some(parent_scope) = self.scoping.scopes.get(&parent) else {
                    break;
                };
                if parent_scope.parent.is_none() {
                    root_mapping = parent_scope.mappings.get(iden).cloned();
                    break;
                }
                parent_id = parent_scope.parent;
            }

            if let Some(root) = root_mapping {
                let in_impl_scope = scope_ref.mappings.contains_key("Self");
                let collides_with_impl_member = current != root
                    && current.contains("::")
                    && calibre_parser::qualified_name_tail(&current) == iden;
                if in_impl_scope && collides_with_impl_member {
                    return Some(root);
                }
            }

            return Some(current);
        }

        if let Some(parent) = scope_ref.parent.as_ref() {
            self.resolve_str(parent, iden).or_else(|| {
                self.scoping
                    .scopes
                    .values()
                    .find(|s| s.mappings.contains_key(iden))
                    .and_then(|s| s.mappings.get(iden).cloned())
            })
        } else {
            self.scoping
                .scopes
                .values()
                .find(|s| s.mappings.contains_key(iden))
                .and_then(|s| s.mappings.get(iden).cloned())
        }
    }

    pub fn resolve_parser_text(&self, scope: &u64, iden: &ParserText) -> Option<ParserText> {
        let resolved = self.resolve_str(scope, &iden.text);

        Some(ParserText {
            text: resolved?.to_string(),
            span: iden.span,
        })
    }

    pub fn resolve_potential_generic_ident(
        &self,
        scope: &u64,
        iden: &PotentialGenericTypeIdentifier,
    ) -> Option<ParserText> {
        match iden {
            PotentialGenericTypeIdentifier::Identifier(x) => {
                self.resolve_potential_dollar_ident(scope, x)
            }
            PotentialGenericTypeIdentifier::Generic {
                identifier,
                generic_types: _,
            } => self.resolve_potential_dollar_ident(scope, identifier),
        }
    }

    pub fn resolve_potential_generic_ident_to_data_type(
        &mut self,
        scope: &u64,
        iden: &PotentialGenericTypeIdentifier,
    ) -> Option<ParserDataType> {
        match iden {
            PotentialGenericTypeIdentifier::Identifier(x) => {
                let resolved = self
                    .resolve_potential_dollar_ident(scope, x)
                    .unwrap_or_else(|| ParserText::from(x.to_string()));
                if let Some(alias) = self.typing.type_aliases.get(&resolved.text) {
                    return Some(alias.clone());
                }
                Some(ParserDataType {
                    data_type: ParserInnerType::Struct(resolved.text.to_string()),
                    span: resolved.span,
                })
            }
            PotentialGenericTypeIdentifier::Generic {
                identifier,
                generic_types,
            } => {
                let base = self
                    .resolve_potential_dollar_ident(scope, identifier)
                    .unwrap_or_else(|| ParserText::from(identifier.to_string()));

                let mut gens: Vec<ParserDataType> = Vec::new();
                for g in generic_types.iter() {
                    gens.push(self.resolve_potential_new_type(scope, g.clone()));
                }

                Some(ParserDataType {
                    data_type: ParserInnerType::StructWithGenerics {
                        identifier: base.text.to_string(),
                        generic_types: gens,
                    },
                    span: *identifier.span(),
                })
            }
        }
    }

    pub fn resolve_dollar_ident_potential_generic_only(
        &self,
        scope: &u64,
        iden: &PotentialGenericTypeIdentifier,
    ) -> Option<ParserText> {
        match iden {
            PotentialGenericTypeIdentifier::Identifier(x) => {
                self.resolve_dollar_ident_only(scope, x)
            }
            PotentialGenericTypeIdentifier::Generic {
                identifier,
                generic_types: _,
            } => self.resolve_dollar_ident_only(scope, identifier),
        }
    }

    fn resolve_identifier_with_mode(
        &self,
        scope: &u64,
        iden: &PotentialDollarIdentifier,
        with_macro_expansion: bool,
        with_scope_resolution: bool,
    ) -> Option<ParserText> {
        let base_text = match iden {
            PotentialDollarIdentifier::Identifier(x) => Some(x.clone()),
            PotentialDollarIdentifier::DollarIdentifier(x) => if with_macro_expansion {
                self.scoping
                    .resolve_macro_arg(scope, x)
                    .map(|x| match &x.node_type {
                        NodeType::Identifier(x) => match x.get_ident() {
                            PotentialDollarIdentifier::DollarIdentifier(x) => Some(x.clone()),
                            PotentialDollarIdentifier::Identifier(x) => Some(x.clone()),
                        },
                        _ => None,
                    })
                    .flatten()
            } else {
                None
            }
            .or_else(|| Some(x.clone())),
        };

        if let Some(text) = base_text {
            if with_scope_resolution {
                self.resolve_parser_text(scope, &text)
            } else {
                Some(text)
            }
        } else {
            None
        }
    }

    pub fn resolve_potential_dollar_ident(
        &self,
        scope: &u64,
        iden: &PotentialDollarIdentifier,
    ) -> Option<ParserText> {
        self.resolve_identifier_with_mode(scope, iden, true, true)
    }

    pub fn resolve_dollar_ident_only(
        &self,
        scope: &u64,
        iden: &PotentialDollarIdentifier,
    ) -> Option<ParserText> {
        self.resolve_identifier_with_mode(scope, iden, true, false)
    }

    pub fn resolve_ffi_data_type(
        &mut self,
        scope: &u64,
        data_type: ParserDataType,
    ) -> ParserDataType {
        self.resolve_data_type(scope, data_type).resolve_ffi()
    }

    pub fn resolve_potential_new_type(
        &mut self,
        scope: &u64,
        data_type: PotentialNewType,
    ) -> ParserDataType {
        let data_type_span = *data_type.span();
        match data_type {
            PotentialNewType::DataType(x) => self.resolve_data_type(scope, x),
            PotentialNewType::NewType {
                identifier,
                type_def,
                overloads,
            } => {
                let identifier = self
                    .resolve_dollar_ident_only(scope, &identifier)
                    .unwrap_or_else(|| ParserText::from(identifier.to_string()).into());
                let new_name =
                    ParserText::temp_name_with_prefix(identifier.text.trim(), identifier.span).text;
                let type_def = MiddleTypeDefType::from_type_def_type(self, scope, type_def);
                self.typing.objects.insert(
                    new_name.clone(),
                    MiddleObject {
                        object_type: type_def.clone(),
                        variables: FxHashMap::default(),
                        traits: Vec::new(),
                        location: self.context.current_location.clone(),
                    },
                );

                if let Some(scope_ref) = self.scoping.scopes.get_mut(scope) {
                    scope_ref.mappings.insert(identifier.text, new_name.clone());
                }

                let previous_self = self
                    .scoping
                    .scopes
                    .get_mut(scope)
                    .map(|scope_ref| {
                        scope_ref
                            .mappings
                            .insert(String::from("Self"), new_name.clone())
                    })
                    .flatten();

                for overload in overloads {
                    let overload = MiddleOverload {
                        operator: match Operator::from_str(&overload.operator.text) {
                            Ok(op) => op,
                            Err(err) => {
                                self.context.errors.push(MiddleErr::Overload(err));
                                continue;
                            }
                        },
                        return_type: self
                            .resolve_potential_new_type(scope, overload.header.return_type.clone()),
                        parameters: {
                            let mut params = Vec::new();
                            let mut contains = false;

                            for param in overload.header.parameters.iter() {
                                let Some(ty) = (if let Some(x) = param.1.clone() {
                                    Some(self.resolve_potential_new_type(scope, x))
                                } else if let Some(node) = &param.2 {
                                    self.resolve_type_from_node(scope, node)
                                } else {
                                    None
                                }) else {
                                    continue;
                                };

                                if let ParserInnerType::Struct(x) =
                                    ty.data_type.clone().unwrap_all_refs()
                                {
                                    if x == &new_name {
                                        contains = true;
                                    }
                                }

                                params.push(ty);
                            }

                            if !contains {
                                continue;
                            }

                            params
                        },
                        func: overload.into(),
                        generic_params: Vec::new(),
                    };

                    self.symbols.overloads.push(overload);
                }

                if let Some(prev) = previous_self {
                    if let Some(scope_ref) = self.scoping.scopes.get_mut(scope) {
                        scope_ref.mappings.insert(String::from("Self"), prev);
                    }
                }

                ParserDataType::new(data_type_span, ParserInnerType::Struct(new_name))
            }
        }
    }

    pub fn resolve_data_type(&mut self, scope: &u64, data_type: ParserDataType) -> ParserDataType {
        match data_type.data_type {
            ParserInnerType::Struct(identifier) => {
                let id = self.resolve_str(scope, &identifier).unwrap_or(identifier);
                if let Some(alias) = self.typing.type_aliases.get(&id) {
                    return alias.clone();
                }
                ParserDataType {
                    data_type: ParserInnerType::Struct(id),
                    span: data_type.span,
                }
            }
            ParserInnerType::StructWithGenerics {
                identifier,
                generic_types,
            } => {
                let id = self.resolve_str(scope, &identifier).unwrap_or(identifier);
                let mut resolved_gens: Vec<ParserDataType> = Vec::new();
                for g in generic_types {
                    resolved_gens.push(self.resolve_data_type(scope, g));
                }

                if id == "ptr" && resolved_gens.len() == 1 {
                    return ParserDataType {
                        data_type: ParserInnerType::Ptr(Box::new(resolved_gens.remove(0))),
                        span: data_type.span,
                    };
                }

                if let Some((tpl_params, _, _)) =
                    self.typing.generic_type_templates.get(&id).cloned()
                    && tpl_params.len() == resolved_gens.len()
                    && !resolved_gens.iter().any(|g| g.is_auto())
                    && let Some(spec) =
                        self.ensure_specialized_type(scope, &id, &tpl_params, &resolved_gens)
                {
                    return ParserDataType {
                        data_type: ParserInnerType::Struct(spec),
                        span: data_type.span,
                    };
                }

                ParserDataType {
                    data_type: ParserInnerType::StructWithGenerics {
                        identifier: id,
                        generic_types: resolved_gens,
                    },
                    span: data_type.span,
                }
            }
            ParserInnerType::Tuple(x) => {
                let mut lst = Vec::new();

                for x in x {
                    lst.push(self.resolve_data_type(scope, x));
                }

                ParserDataType {
                    data_type: ParserInnerType::Tuple(lst),
                    span: data_type.span,
                }
            }
            ParserInnerType::Function {
                return_type,
                parameters,
            } => ParserDataType {
                data_type: ParserInnerType::Function {
                    return_type: Box::new(self.resolve_data_type(scope, *return_type)),
                    parameters: {
                        let mut params = Vec::new();

                        for param in parameters {
                            params.push(self.resolve_data_type(scope, param));
                        }

                        params
                    },
                },
                span: data_type.span,
            },
            ParserInnerType::Ref(d_type, mutability) => ParserDataType {
                data_type: ParserInnerType::Ref(
                    Box::new(self.resolve_data_type(scope, *d_type)),
                    mutability,
                ),
                span: data_type.span,
            },
            ParserInnerType::List(x) => ParserDataType {
                data_type: ParserInnerType::List(Box::new(self.resolve_data_type(scope, *x))),
                span: data_type.span,
            },
            ParserInnerType::Ptr(x) => ParserDataType {
                data_type: ParserInnerType::Ptr(Box::new(self.resolve_data_type(scope, *x))),
                span: data_type.span,
            },
            ParserInnerType::Option(x) => ParserDataType {
                data_type: ParserInnerType::Option(Box::new(self.resolve_data_type(scope, *x))),
                span: data_type.span,
            },
            ParserInnerType::Result { ok, err } => ParserDataType {
                data_type: ParserInnerType::Result {
                    err: Box::new(self.resolve_data_type(scope, *err)),
                    ok: Box::new(self.resolve_data_type(scope, *ok)),
                },
                span: data_type.span,
            },
            ParserInnerType::Scope(x) => {
                let mut lst = Vec::new();

                for x in x {
                    lst.push(self.resolve_data_type(scope, x));
                }

                if lst.len() == 2
                    && let ParserInnerType::Struct(name) = &lst[1].data_type
                    && let Some(resolved) =
                        self.typing.resolve_associated_type(&lst[0], name.as_str())
                {
                    return resolved;
                }

                ParserDataType {
                    data_type: ParserInnerType::Scope(lst),
                    span: data_type.span,
                }
            }
            ParserInnerType::DollarIdentifier(ref x) => {
                if let Some(node) = self.scoping.resolve_macro_arg(scope, x) {
                    let NodeType::DataType { data_type } = node.node_type.clone() else {
                        unimplemented!()
                    };

                    self.resolve_potential_new_type(scope, data_type.clone())
                } else {
                    data_type
                }
            }
            ParserInnerType::DynamicTraits(traits) => ParserDataType {
                data_type: ParserInnerType::DynamicTraits(
                    traits
                        .into_iter()
                        .map(|t| self.resolve_str(scope, &t).unwrap_or(t))
                        .collect(),
                ),
                span: data_type.span,
            },
            _ => data_type,
        }
        .verify()
    }
}
