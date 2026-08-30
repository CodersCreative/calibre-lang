use crate::{
    environment::MiddleEnvironment,
    errors::MiddleErr::{self},
    scoping::ScopeId,
    typing::{MiddleTrait, MiddleTypeDefType},
};
use calibre_parser::{
    Span,
    ast::{
        idents::{ParserText, PotentialDollarIdentifier, PotentialGenericTypeIdentifier},
        nodes::{AstNode, AstNodeType},
        types::{ParserDataType, ParserInnerType},
    },
};
use rustc_hash::{FxHashMap, FxHashSet};
use std::{fmt::Display, str::FromStr, write};
use tracing::{instrument, trace, warn};

#[derive(PartialEq)]
pub enum StrOrAstNode {
    Str(String),
    Node(AstNode),
}

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

impl<'a> From<&'a PotentialGenericTypeIdentifier> for IdentifierType<'a> {
    fn from(val: &'a PotentialGenericTypeIdentifier) -> IdentifierType<'a> {
        IdentifierType::Generic(val)
    }
}

impl<'a> From<&'a PotentialDollarIdentifier> for IdentifierType<'a> {
    fn from(val: &'a PotentialDollarIdentifier) -> IdentifierType<'a> {
        IdentifierType::Dollar(val)
    }
}

impl<'a> From<&'a ParserText> for IdentifierType<'a> {
    fn from(val: &'a ParserText) -> IdentifierType<'a> {
        IdentifierType::Ident(&val.text)
    }
}

impl<'a> From<&'a dyn ToString> for IdentifierType<'a> {
    fn from(val: &'a dyn ToString) -> IdentifierType<'a> {
        IdentifierType::Ident(val)
    }
}

impl<'a> From<&'a String> for IdentifierType<'a> {
    fn from(val: &'a String) -> IdentifierType<'a> {
        IdentifierType::Ident(val)
    }
}

impl<'a> From<&'a &'a str> for IdentifierType<'a> {
    fn from(val: &'a &'a str) -> IdentifierType<'a> {
        IdentifierType::Ident(val)
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
        scope: ScopeId,
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

        if let Some(out) = out {
            return Some(
                self.resolve_data_type(scope, &out, ResolutionOptions::typing())
                    .unwrap_or(out),
            );
        }

        if let Some(imp) = self.typing.find_impl_for_type(&resolved.impl_name())
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

    pub fn resolve<'a>(
        &'a self,
        scope: ScopeId,
        ident: impl Into<IdentifierType<'a>>,
        options: ResolutionOptions,
    ) -> Result<String, MiddleErr> {
        let mut current = match self.resolve_inner(scope, ident, options)? {
            StrOrAstNode::Node(x) => {
                return Err(self
                    .context
                    .err_at_current(MiddleErr::UnexpectedMacroArgType(x.to_string())));
            }
            StrOrAstNode::Str(x) => x,
        };

        for _ in 0..64 {
            match self.resolve_inner(scope, &current, options) {
                Ok(StrOrAstNode::Str(x)) if current != x => current = x,
                _ => break,
            }
        }

        Ok(current)
    }

    pub fn resolve_potential_node<'a>(
        &'a self,
        scope: ScopeId,
        ident: impl Into<IdentifierType<'a>>,
        options: ResolutionOptions,
    ) -> Result<StrOrAstNode, MiddleErr> {
        let mut current = match self.resolve_inner(scope, ident, options)? {
            StrOrAstNode::Node(x) => return Ok(StrOrAstNode::Node(x)),
            StrOrAstNode::Str(x) => x,
        };

        for _ in 0..64 {
            match self.resolve_inner(scope, &current, options) {
                Ok(StrOrAstNode::Str(x)) if current != x => current = x,
                Ok(StrOrAstNode::Node(x)) => return Ok(StrOrAstNode::Node(x)),
                _ => break,
            }
        }

        Ok(StrOrAstNode::Str(current))
    }

    #[instrument(skip_all)]
    fn resolve_inner<'a>(
        &'a self,
        scope: ScopeId,
        ident: impl Into<IdentifierType<'a>>,
        options: ResolutionOptions,
    ) -> Result<StrOrAstNode, MiddleErr> {
        let ident = ident.into();
        trace!(ident = %ident, "Resolving identifier");

        let ident = match ident {
            IdentifierType::Generic(PotentialGenericTypeIdentifier::Generic {
                identifier: PotentialDollarIdentifier::DollarIdentifier(x),
                ..
            })
            | IdentifierType::Generic(PotentialGenericTypeIdentifier::Identifier(
                PotentialDollarIdentifier::DollarIdentifier(x),
            ))
            | IdentifierType::Dollar(PotentialDollarIdentifier::DollarIdentifier(x)) => {
                if !options.dollar_resolution {
                    warn!(
                        "Resolution failed : No dollar resolution allowed but dollar ident provided"
                    );
                    return Err(self
                        .context
                        .err_at_current(MiddleErr::Internal(String::from(
                            "No dollar resolution allowed but dollar ident provided",
                        ))));
                }
                let resolved = self.scoping.resolve_macro_arg(scope, x).ok_or_else(|| {
                    self.context
                        .err_at_current(MiddleErr::MacroArg(x.to_string()))
                })?;

                match &resolved.node_type {
                    AstNodeType::Identifier(x) => match x.get_ident() {
                        PotentialDollarIdentifier::Identifier(x) => x.text.clone(),
                        PotentialDollarIdentifier::DollarIdentifier(x) => x.to_string(),
                    },
                    _ => {
                        return Ok(StrOrAstNode::Node(resolved.clone()));
                    }
                }
            }
            IdentifierType::Generic(x) => {
                let inner_ident = x.get_ident();
                inner_ident.to_string()
            }
            IdentifierType::Dollar(PotentialDollarIdentifier::Identifier(x)) => x.text.clone(),
            IdentifierType::Ident(x) => x.to_string(),
        };

        if options.type_resolution {
            match ParserInnerType::from_str(&ident) {
                Ok(ParserInnerType::Struct(_) | ParserInnerType::StructWithGenerics { .. })
                | Err(_) => {}
                _ => return Ok(StrOrAstNode::Str(ident)),
            }
        }

        trace!(ident = %ident, "Identifier resolution complete");

        for current_scope in scope.ancestors(&self.scoping.scopes) {
            if options.type_resolution {
                if self.typing.objects.contains_key(&ident)
                    || self.typing.trait_defs.contains_key(&ident)
                    || self.typing.impls.contains_key(&ident)
                {
                    return Ok(StrOrAstNode::Str(ident));
                }

                let ty = ParserDataType::from(ParserInnerType::from_str(&ident).unwrap());

                let scope_ref = self.scoping.scope_or_err(current_scope)?;

                if let Some(x) = scope_ref.type_mappings.get(&ty.impl_name()).cloned() {
                    return Ok(StrOrAstNode::Str(ParserDataType::from(x).impl_name()));
                }

                if options.name_resolution {
                    if self.symbols.variables.contains_key(&ident) {
                        return Ok(StrOrAstNode::Str(ident));
                    }

                    if let Some(x) = scope_ref.mappings.get(&ident).cloned() {
                        return Ok(StrOrAstNode::Str(x));
                    }
                }
            } else if options.name_resolution {
                if self.symbols.variables.contains_key(&ident) {
                    return Ok(StrOrAstNode::Str(ident));
                }

                let scope_ref = self.scoping.scope_or_err(current_scope)?;

                if let Some(x) = scope_ref.mappings.get(&ident).cloned() {
                    return Ok(StrOrAstNode::Str(x));
                }
            }
        }

        if options.type_resolution {
            for key in self.typing.trait_defs.keys() {
                if ParserText::temp_name_suffix_matches(key, &ident) {
                    return Ok(StrOrAstNode::Str(key.clone()));
                }
            }

            for key in self.typing.objects.keys() {
                if ParserText::temp_name_suffix_matches(key, &ident) {
                    return Ok(StrOrAstNode::Str(key.clone()));
                }
            }

            for key in self.typing.impls.keys() {
                if ParserText::temp_name_suffix_matches(key, &ident) {
                    return Ok(StrOrAstNode::Str(key.clone()));
                }
            }

            if self.scoping.all_time_generics.contains(&ident) {
                return Ok(StrOrAstNode::Str(ident));
            }

            if options.name_resolution {
                for key in self.symbols.variables.keys() {
                    if ParserText::temp_name_suffix_matches(key, &ident) {
                        return Ok(StrOrAstNode::Str(key.clone()));
                    }
                }
            }

            return Err(self.context.err_at_current(MiddleErr::Object(ident)));
        }

        if !options.name_resolution {
            return Ok(StrOrAstNode::Str(ident));
        }

        for key in self.symbols.variables.keys() {
            if ParserText::temp_name_suffix_matches(key, &ident) {
                return Ok(StrOrAstNode::Str(key.clone()));
            }
        }

        Err(self.context.err_at_current(MiddleErr::Variable(ident)))
    }

    #[instrument(skip_all)]
    pub fn resolve_to_data_type<'a>(
        &'a mut self,
        scope: ScopeId,
        ident: impl Into<IdentifierType<'a>>,
    ) -> Result<ParserDataType, MiddleErr> {
        let ident = ident.into();
        trace!(ident = %ident, "Resolving identifier");

        let (ty, mut generic_types) = match ident {
            IdentifierType::Ident(x) => {
                let x = x.to_string();
                let resolved =
                    self.resolve(scope, &x, ResolutionOptions::default().with_dollar())?;

                (
                    match ParserInnerType::from_str(&resolved).unwrap() {
                        ParserInnerType::Struct(x) => ParserInnerType::Struct(self.resolve(
                            scope,
                            &x,
                            ResolutionOptions::typing(),
                        )?),
                        x => x,
                    },
                    Vec::new(),
                )
            }
            IdentifierType::Generic(PotentialGenericTypeIdentifier::Identifier(x))
            | IdentifierType::Dollar(x) => {
                let resolved =
                    self.resolve(scope, x, ResolutionOptions::default().with_dollar())?;

                (
                    match ParserInnerType::from_str(&resolved).unwrap() {
                        ParserInnerType::Struct(x) => ParserInnerType::Struct(self.resolve(
                            scope,
                            &x,
                            ResolutionOptions::typing(),
                        )?),
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
                    .map(|x| self.resolve_data_type(scope, x, ResolutionOptions::typing()))
                    .collect::<Result<Vec<_>, _>>()?;

                let resolved = self.resolve(
                    scope,
                    identifier,
                    ResolutionOptions::default().with_dollar(),
                )?;

                if self.symbols.variables.contains_key(&resolved) {
                    return Err(self.context.err_at_current(MiddleErr::Object(resolved)));
                }

                (
                    match ParserInnerType::from_str(&resolved).unwrap() {
                        ParserInnerType::Struct(x) => ParserInnerType::Struct(self.resolve(
                            scope,
                            &x,
                            ResolutionOptions::typing(),
                        )?),
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
        scope: ScopeId,
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
                    *mutability,
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
                    err: Box::new(self.resolve_data_type(scope, err.as_ref(), options)?),
                    ok: Box::new(self.resolve_data_type(scope, ok.as_ref(), options)?),
                },
                span: self.context.current_span(),
            },
            ParserInnerType::Scope(x) => {
                let mut lst = Vec::new();

                for x in x {
                    lst.push(
                        self.resolve_data_type(scope, x, options)
                            .unwrap_or(x.clone()),
                    );
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
                    let AstNodeType::DataType { data_type } = node.node_type.clone() else {
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
                        .iter()
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
