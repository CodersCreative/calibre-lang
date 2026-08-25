use crate::{
    environment::MiddleEnvironment,
    errors::MiddleErr::{self},
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
use std::{fmt::Display, str::FromStr, write};
use tracing::{instrument, trace, warn};

pub enum IdentifierType<'a> {
    Generic(&'a PotentialGenericTypeIdentifier),
    Dollar(&'a PotentialDollarIdentifier),
    Ident(&'a dyn ToString),
}

impl<'a> Display for IdentifierType<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Generic(x) => x.get_ident().to_string(),
                Self::Dollar(x) => x.to_string(),
                Self::Ident(x) => x.to_string(),
            }
        )
    }
}

impl<'a> IdentifierType<'a> {
    pub fn supports_dollar(&self) -> bool {
        matches!(self, Self::Generic(_) | Self::Dollar(_))
    }
}

impl<'a> Into<IdentifierType<'a>> for &'a PotentialGenericTypeIdentifier {
    fn into(self) -> IdentifierType<'a> {
        IdentifierType::Generic(self)
    }
}

impl<'a> Into<IdentifierType<'a>> for &'a PotentialDollarIdentifier {
    fn into(self) -> IdentifierType<'a> {
        IdentifierType::Dollar(self)
    }
}

impl<'a> Into<IdentifierType<'a>> for &'a ParserText {
    fn into(self) -> IdentifierType<'a> {
        IdentifierType::Ident(&self.text)
    }
}

impl<'a> Into<IdentifierType<'a>> for &'a dyn ToString {
    fn into(self) -> IdentifierType<'a> {
        IdentifierType::Ident(self)
    }
}

impl<'a> Into<IdentifierType<'a>> for &'a String {
    fn into(self) -> IdentifierType<'a> {
        IdentifierType::Ident(self)
    }
}

impl<'a> Into<IdentifierType<'a>> for &'a &'a str {
    fn into(self) -> IdentifierType<'a> {
        IdentifierType::Ident(self)
    }
}

#[derive(Default, Clone, Copy)]
pub struct ResolutionOptions {
    pub dollar_resolution: bool,
    pub name_resolution: bool,
    pub type_resolution: bool,
}

impl ResolutionOptions {
    pub fn all() -> Self {
        Self {
            dollar_resolution: true,
            name_resolution: true,
            type_resolution: true,
        }
    }

    pub fn typing() -> Self {
        Self {
            dollar_resolution: true,
            name_resolution: false,
            type_resolution: true,
        }
    }

    pub fn idents() -> Self {
        Self {
            dollar_resolution: true,
            name_resolution: true,
            type_resolution: false,
        }
    }

    pub fn with_dollar(mut self) -> Self {
        self.dollar_resolution = true;
        self
    }

    pub fn with_name(mut self) -> Self {
        self.name_resolution = true;
        self
    }

    pub fn with_type(mut self) -> Self {
        self.type_resolution = true;
        self
    }
}

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
            .resolve_data_type(scope, base, ResolutionOptions::typing())
            .ok()?
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

    #[instrument(skip_all)]
    pub fn resolve<'a>(
        &'a self,
        scope: &'a u64,
        ident: impl Into<IdentifierType<'a>>,
        options: ResolutionOptions,
    ) -> Result<String, MiddleErr> {
        let ident = ident.into();
        trace!(ident = %ident, "Resolving identifier");

        let (ident, dollar_ident) = match ident {
            IdentifierType::Generic(x) => {
                return self.resolve(scope, x.get_ident(), options);
            }
            IdentifierType::Dollar(PotentialDollarIdentifier::DollarIdentifier(x)) => {
                (None, Some(x))
            }
            IdentifierType::Dollar(PotentialDollarIdentifier::Identifier(x)) => {
                (Some(x.text.clone()), None)
            }
            IdentifierType::Ident(x) => (Some(x.to_string()), None),
        };

        if dollar_ident.is_some() && !options.dollar_resolution {
            warn!("Resolution failed : No dollar resolution allowed but dollar ident provided");
            return Err(self
                .context
                .err_at_current(MiddleErr::Internal(String::from(
                    "No dollar resolution allowed but dollar ident provided",
                ))));
        }

        let ident = if let Some(x) = dollar_ident {
            let resolved = self.scoping.resolve_macro_arg(scope, x).ok_or_else(|| {
                self.context
                    .err_at_current(MiddleErr::MacroArg(x.to_string()))
            })?;

            match &resolved.node_type {
                NodeType::Identifier(x) => match x.get_ident() {
                    PotentialDollarIdentifier::Identifier(x) => x.text.clone(),
                    x => return self.resolve(scope, x, options),
                },
                _ => {
                    return Err(self
                        .context
                        .err_at_current(MiddleErr::UnexpectedMacroArgType(x.to_string())));
                }
            }
        } else {
            ident.unwrap()
        };

        trace!(ident = %ident, "Dollar resolution succedded");

        if options.type_resolution {
            if self.scoping.is_generic_param(&ident)
                || self.typing.objects.contains_key(&ident)
                || self.typing.trait_defs.contains_key(&ident)
            {
                return Ok(ident);
            }

            let scope_ref = self.scoping.scopes.get(scope).ok_or_else(|| {
                self.context
                    .err_at_current(MiddleErr::Scope(scope.to_string()))
            })?;

            let ty = ParserDataType::from(ParserInnerType::from_str(&ident).unwrap());

            if let Some(x) = scope_ref.type_mappings.get(&ty.impl_name()).cloned() {
                return Ok(ParserDataType::from(x).impl_name());
            }

            if options.name_resolution {
                if self.symbols.variables.contains_key(&ident) {
                    return Ok(ident);
                }

                if let Some(x) = scope_ref.mappings.get(&ident).cloned() {
                    return Ok(x);
                }
            }

            if let Some(x) = scope_ref.parent {
                return self.resolve(&x, &ident, options);
            }

            for key in self.typing.trait_defs.keys() {
                if ParserText::temp_name_suffix_matches(key, &ident) {
                    return Ok(key.clone());
                }
            }

            for key in self.typing.objects.keys() {
                if ParserText::temp_name_suffix_matches(key, &ident) {
                    return Ok(key.clone());
                }
            }

            if self.scoping.all_time_generics.contains(&ident) {
                return Ok(ident);
            }

            if options.name_resolution {
                for key in self.symbols.variables.keys() {
                    if ParserText::temp_name_suffix_matches(key, &ident) {
                        return Ok(key.clone());
                    }
                }
            }

            return Err(self.context.err_at_current(MiddleErr::Object(ident)));
        }

        if !options.name_resolution {
            return Ok(ident);
        }

        if self.symbols.variables.contains_key(&ident) {
            return Ok(ident);
        }

        let scope_ref = self.scoping.scopes.get(scope).ok_or_else(|| {
            self.context
                .err_at_current(MiddleErr::Scope(scope.to_string()))
        })?;

        if let Some(x) = scope_ref.mappings.get(&ident).cloned() {
            return Ok(x);
        }

        if let Some(x) = scope_ref.parent {
            return self.resolve(&x, &ident, options);
        }

        for key in self.symbols.variables.keys() {
            if ParserText::temp_name_suffix_matches(key, &ident) {
                return Ok(key.clone());
            }
        }

        Err(self.context.err_at_current(MiddleErr::Variable(ident)))
    }

    #[instrument(skip_all)]
    pub fn resolve_to_data_type<'a>(
        &'a mut self,
        scope: &'a u64,
        ident: impl Into<IdentifierType<'a>>,
    ) -> Result<ParserDataType, MiddleErr> {
        let ident = ident.into();
        trace!(ident = %ident, "Resolving identifier");

        let (ty, mut generic_types) = match ident {
            IdentifierType::Ident(x) => {
                let x = x.to_string();

                let resolved = self
                    .resolve(scope, &x, ResolutionOptions::default().with_dollar())
                    .unwrap_or(x.clone());

                if self.symbols.variables.contains_key(&resolved) {
                    return Err(self.context.err_at_current(MiddleErr::Object(resolved)));
                }

                (
                    match ParserInnerType::from_str(&resolved).unwrap() {
                        ParserInnerType::Struct(x) => ParserInnerType::Struct(
                            self.resolve(scope, &x, ResolutionOptions::typing())
                                .unwrap_or(x),
                        ),
                        x => x,
                    },
                    Vec::new(),
                )
            }
            IdentifierType::Generic(PotentialGenericTypeIdentifier::Identifier(x))
            | IdentifierType::Dollar(x) => {
                let resolved = self
                    .resolve(scope, x, ResolutionOptions::default().with_dollar())
                    .unwrap_or(x.text().clone());

                if self.symbols.variables.contains_key(&resolved) {
                    return Err(self.context.err_at_current(MiddleErr::Object(resolved)));
                }

                (
                    match ParserInnerType::from_str(&resolved).unwrap() {
                        ParserInnerType::Struct(x) => ParserInnerType::Struct(
                            self.resolve(scope, &x, ResolutionOptions::typing())
                                .unwrap_or(x),
                        ),
                        x => x,
                    },
                    Vec::new(),
                )
            }
            IdentifierType::Generic(PotentialGenericTypeIdentifier::Generic {
                identifier,
                generic_types,
            }) => {
                let generic_types: Vec<ParserDataType> = generic_types
                    .iter()
                    .map(|x| {
                        self.resolve_data_type(scope, x, ResolutionOptions::typing())
                            .unwrap()
                    })
                    .collect();

                let resolved = self
                    .resolve(
                        scope,
                        identifier,
                        ResolutionOptions::default().with_dollar(),
                    )
                    .unwrap_or(identifier.text().clone());

                if self.symbols.variables.contains_key(&resolved) {
                    return Err(self.context.err_at_current(MiddleErr::Object(resolved)));
                }

                (
                    match ParserInnerType::from_str(&resolved).unwrap() {
                        ParserInnerType::Struct(x) => ParserInnerType::Struct(
                            self.resolve(scope, &x, ResolutionOptions::typing())
                                .unwrap_or(x),
                        ),
                        x => x,
                    },
                    generic_types,
                )
            }
        };

        Ok(ParserDataType {
            span: self.context.current_span(),
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

    #[instrument(skip_all)]
    pub fn resolve_data_type<'a>(
        &'a self,
        scope: &'a u64,
        data_type: impl Into<&'a ParserInnerType>,
        options: ResolutionOptions,
    ) -> Result<ParserDataType, MiddleErr> {
        let data_type = data_type.into();
        trace!(data_type = %data_type, "Resolving type");

        Ok(match data_type {
            ParserInnerType::Struct(identifier) => ParserDataType {
                data_type: ParserInnerType::Struct(self.resolve(scope, identifier, options)?),
                span: self.context.current_span(),
            },
            ParserInnerType::StructWithGenerics {
                identifier,
                generic_types,
            } => {
                let id = self.resolve(scope, identifier, options)?;

                let mut resolved_gens: Vec<ParserDataType> = Vec::new();
                for g in generic_types {
                    resolved_gens.push(self.resolve_data_type(scope, g, options)?);
                }

                if id == "ptr" && resolved_gens.len() == 1 {
                    return Ok(ParserDataType {
                        data_type: ParserInnerType::Ptr(Box::new(resolved_gens.remove(0))),
                        span: self.context.current_span(),
                    });
                }

                if id == "list" && resolved_gens.len() == 1 {
                    return Ok(ParserDataType {
                        data_type: ParserInnerType::List(Box::new(resolved_gens.remove(0))),
                        span: self.context.current_span(),
                    });
                }

                ParserDataType {
                    data_type: ParserInnerType::StructWithGenerics {
                        identifier: id,
                        generic_types: resolved_gens,
                    },
                    span: self.context.current_span(),
                }
            }
            ParserInnerType::Tuple(x) => {
                let mut lst = Vec::new();

                for x in x {
                    lst.push(self.resolve_data_type(scope, x, options)?);
                }

                ParserDataType {
                    data_type: ParserInnerType::Tuple(lst),
                    span: self.context.current_span(),
                }
            }
            ParserInnerType::Function {
                return_type,
                parameters,
            } => ParserDataType {
                data_type: ParserInnerType::Function {
                    return_type: Box::new(self.resolve_data_type(
                        scope,
                        return_type.as_ref(),
                        options,
                    )?),
                    parameters: {
                        let mut params = Vec::new();

                        for param in parameters {
                            params.push(self.resolve_data_type(scope, param, options)?);
                        }

                        params
                    },
                },
                span: self.context.current_span(),
            },
            ParserInnerType::Ref(d_type, mutability) => ParserDataType {
                data_type: ParserInnerType::Ref(
                    Box::new(self.resolve_data_type(scope, d_type.as_ref(), options)?),
                    mutability.clone(),
                ),
                span: self.context.current_span(),
            },
            ParserInnerType::List(x) => ParserDataType {
                data_type: ParserInnerType::List(Box::new(self.resolve_data_type(
                    scope,
                    x.as_ref(),
                    options,
                )?)),
                span: self.context.current_span(),
            },
            ParserInnerType::Ptr(x) => ParserDataType {
                data_type: ParserInnerType::Ptr(Box::new(self.resolve_data_type(
                    scope,
                    x.as_ref(),
                    options,
                )?)),
                span: self.context.current_span(),
            },
            ParserInnerType::Option(x) => ParserDataType {
                data_type: ParserInnerType::Option(Box::new(self.resolve_data_type(
                    scope,
                    x.as_ref(),
                    options,
                )?)),
                span: self.context.current_span(),
            },
            ParserInnerType::Result { ok, err } => ParserDataType {
                data_type: ParserInnerType::Result {
                    err: Box::new(self.resolve_data_type(scope, err.as_ref(), options.clone())?),
                    ok: Box::new(self.resolve_data_type(scope, ok.as_ref(), options)?),
                },
                span: self.context.current_span(),
            },
            ParserInnerType::Scope(x) => {
                let mut lst = Vec::new();

                for x in x {
                    lst.push(self.resolve_data_type(scope, x, options)?);
                }

                if lst.len() == 2
                    && let ParserInnerType::Struct(name) = &lst[1].data_type
                    && let Some(resolved) =
                        self.typing.resolve_associated_type(&lst[0], name.as_str())
                {
                    return Ok(resolved);
                }

                ParserDataType {
                    data_type: ParserInnerType::Scope(lst),
                    span: self.context.current_span(),
                }
            }
            ParserInnerType::DollarIdentifier(x) => {
                if let Some(node) = self.scoping.resolve_macro_arg(scope, x) {
                    let NodeType::DataType { data_type } = node.node_type.clone() else {
                        unimplemented!()
                    };

                    self.resolve_data_type(scope, &data_type, options)?
                } else {
                    return Err(self.context.err_at_current(MiddleErr::MacroArg(x.clone())));
                }
            }
            ParserInnerType::DynamicTraits(traits) => ParserDataType {
                data_type: ParserInnerType::DynamicTraits(
                    traits
                        .into_iter()
                        .map(|t| {
                            self.resolve(scope, t, ResolutionOptions::typing())
                                .unwrap_or(t.to_string())
                        })
                        .collect(),
                ),
                span: self.context.current_span(),
            },
            x => ParserDataType {
                data_type: x.clone(),
                span: self.context.current_span(),
            },
        }
        .verify())
    }
}
