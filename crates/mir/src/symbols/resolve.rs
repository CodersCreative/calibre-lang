use crate::{
    environment::MiddleEnvironment,
    errors::MiddleErr,
    typing::{MiddleTrait, MiddleTypeDefType},
};
use calibre_parser::{
    Span,
    ast::{
        idents::{ParserText, PotentialDollarIdentifier, PotentialGenericTypeIdentifier},
        nodes::NodeType,
        types::{ParserDataType, ParserInnerType},
    },
};
use rustc_hash::{FxHashMap, FxHashSet};
use std::str::FromStr;

impl MiddleEnvironment {
    pub fn resolve_member_fn_type(
        &self,
        ty: &ParserDataType,
        member: &impl ToString,
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
                .find(|(name, _)| ParserText::temp_name_suffix_matches(name, &trait_name))
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
                        .find(|(name, _)| ParserText::temp_name_suffix_matches(name, implied))
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
            && let Some(mapped_member) = imp.get_member(&member, &[])
        {
            return self
                .symbols
                .variables
                .get(&mapped_member.symbol_name)
                .map(|var| var.data_type.clone());
        }

        None
    }

    pub fn resolve_member_fn_name(
        &self,
        ty: &ParserDataType,
        member: &impl ToString,
    ) -> Option<String> {
        let symbol_name = self
            .typing
            .find_impl_member(ty, member)?
            .symbol_name
            .clone();

        self.symbols.variables.get(&symbol_name).and_then(|var| {
            if var.data_type.clone().unwrap_all_refs().is_callable() {
                Some(symbol_name)
            } else {
                None
            }
        })
    }

    pub fn resolve_str(&self, scope: &u64, iden: &str) -> Option<String> {
        if self.symbols.variables.contains_key(iden)
            || self.typing.objects.contains_key(iden)
            || self.typing.trait_defs.contains_key(iden)
        {
            return Some(iden.to_string());
        }

        let scope_ref = self.scoping.scopes.get(scope)?;

        if let Some(x) = scope_ref.mappings.get(iden).cloned() {
            return Some(x);
        }

        if let Some(x) = scope_ref
            .type_mappings
            .get(&ParserInnerType::from_str(iden).ok()?)
            .cloned()
        {
            return Some(ParserDataType::from(x).impl_name());
        }

        if let Some(x) = scope_ref.parent {
            return self.resolve_str(&x, iden);
        }

        // TODO Remove this, its a bit of a worst case scenario and costly

        for key in self.typing.trait_defs.keys() {
            if key == iden || ParserText::temp_name_suffix_matches(key, &iden) {
                return Some(key.clone());
            }
        }

        for key in self.typing.objects.keys() {
            if key == iden || ParserText::temp_name_suffix_matches(key, &iden) {
                return Some(key.clone());
            }
        }

        for key in self.symbols.variables.keys() {
            if key == iden || ParserText::temp_name_suffix_matches(key, &iden) {
                return Some(key.clone());
            }
        }

        None
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
        let (ty, mut generic_types) = match iden {
            PotentialGenericTypeIdentifier::Identifier(x) => {
                let resolved = self
                    .resolve_dollar_ident_only(scope, x)
                    .map(|x| x.text)
                    .unwrap_or(x.text().clone());

                (
                    match ParserInnerType::from_str(&resolved).unwrap() {
                        ParserInnerType::Struct(x) => {
                            ParserInnerType::Struct(self.resolve_str(scope, &x).unwrap_or(x))
                        }
                        x => x,
                    },
                    Vec::new(),
                )
            }
            PotentialGenericTypeIdentifier::Generic {
                identifier,
                generic_types,
            } => {
                let generic_types: Vec<ParserDataType> = generic_types
                    .iter()
                    .map(|x| self.resolve_data_type(scope, x.clone()))
                    .collect();

                let resolved = self
                    .resolve_dollar_ident_only(scope, identifier)
                    .map(|x| x.text)
                    .unwrap_or(identifier.text().clone());

                if self.symbols.variables.contains_key(&resolved) {
                    return None;
                }

                (
                    match ParserInnerType::from_str(&resolved).unwrap() {
                        ParserInnerType::Struct(x) => {
                            ParserInnerType::Struct(self.resolve_str(scope, &x).unwrap_or(x))
                        }
                        x => x,
                    },
                    generic_types,
                )
            }
        };

        Some(ParserDataType {
            span: *iden.span(),
            data_type: match ty {
                ParserInnerType::Struct(x) if !generic_types.is_empty() => {
                    ParserInnerType::StructWithGenerics {
                        identifier: x,
                        generic_types,
                    }
                }
                ParserInnerType::Ptr(_) if !generic_types.is_empty() => {
                    ParserInnerType::Ptr(Box::new(generic_types.pop().unwrap()))
                }
                ParserInnerType::List(_) if !generic_types.is_empty() => {
                    ParserInnerType::List(Box::new(generic_types.pop().unwrap()))
                }
                x => x,
            },
        })
    }

    pub fn resolve_dollar_ident_potential_generic_only(
        &self,
        scope: &u64,
        iden: &PotentialGenericTypeIdentifier,
    ) -> Result<ParserText, MiddleErr> {
        match iden {
            PotentialGenericTypeIdentifier::Identifier(identifier)
            | PotentialGenericTypeIdentifier::Generic {
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
    ) -> Result<ParserText, MiddleErr> {
        if let Some(x) = self.resolve_identifier_with_mode(scope, iden, true, false) {
            Ok(x)
        } else {
            Err(MiddleErr::MacroArg(iden.text().clone()))
        }
    }

    pub fn resolve_ffi_data_type(
        &mut self,
        scope: &u64,
        data_type: ParserDataType,
    ) -> ParserDataType {
        self.resolve_data_type(scope, data_type).resolve_ffi()
    }

    pub fn resolve_type_from_type_mappings(
        &self,
        scope: &u64,
        data_type: &ParserInnerType,
    ) -> Option<&ParserInnerType> {
        let scope_ref = self.scoping.scopes.get(scope)?;
        match scope_ref.type_mappings.get(data_type) {
            Some(x) => return Some(x),
            _ => scope_ref
                .parent
                .and_then(|x| self.resolve_type_from_type_mappings(&x, data_type)),
        }
    }

    pub fn resolve_data_type(&mut self, scope: &u64, data_type: ParserDataType) -> ParserDataType {
        let mut data_type = self
            .resolve_type_from_type_mappings(scope, &data_type.data_type)
            .map(|x| ParserDataType::new(data_type.span, x.clone()))
            .unwrap_or(data_type);
        data_type = self.resolve_type_from_mappings(scope, data_type);
        self.resolve_type_from_type_mappings(scope, &data_type.data_type)
            .map(|x| ParserDataType::new(data_type.span, x.clone()))
            .unwrap_or(data_type)
    }

    pub fn resolve_type_from_mappings(
        &mut self,
        scope: &u64,
        data_type: ParserDataType,
    ) -> ParserDataType {
        match data_type.data_type {
            ParserInnerType::Struct(identifier) => ParserDataType {
                data_type: ParserInnerType::Struct(
                    self.resolve_str(scope, &identifier).unwrap_or(identifier),
                ),
                span: data_type.span,
            },
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

                    self.resolve_data_type(scope, data_type.clone())
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
