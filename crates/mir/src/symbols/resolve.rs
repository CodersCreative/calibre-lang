use crate::{
    ast::types::MirDataType,
    environment::MiddleEnvironment,
    errors::MiddleErr::{self},
    scoping::{FullyQualifiedPath, ScopeId},
    symbols::{TypeKey, VariableKey},
    typing::{MiddleTrait, MiddleTypeDefType},
};
use calibre_parser::ast::{
    idents::{ParserText, PotentialDollarIdentifier, PotentialGenericTypeIdentifier},
    nodes::{AstNode, AstNodeType, literals::AstDataType},
    types::{ParserDataType, ParserInnerType},
};
use rustc_hash::{FxHashMap, FxHashSet};
use serde::{Deserialize, Serialize};
use std::{fmt::Display, str::FromStr, sync::Arc, write};
use tracing::{instrument, trace, warn};
use ustr::Ustr;

#[derive(PartialEq)]
pub enum KeyOrAstNode {
    Key(Key),
    Node(Box<AstNode>),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum Key {
    TypeKey(TypeKey),
    VariableKey(VariableKey),
}

impl Display for Key {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::TypeKey(k) => write!(f, "{}", k),
            Self::VariableKey(k) => write!(f, "{}", k),
        }
    }
}

impl Key {
    pub fn unwrap_typing(self) -> TypeKey {
        match self {
            Self::TypeKey(x) => x,
            Self::VariableKey(x) => panic!("Called unwrap_typing on variable_key : {}", x),
        }
    }

    pub fn unwrap_variable(self) -> VariableKey {
        match self {
            Self::VariableKey(x) => x,
            Self::TypeKey(x) => panic!("Called unwrap_variable on type_key : {}", x),
        }
    }

    pub fn unwrap_typing_ref(&self) -> &TypeKey {
        match self {
            Self::TypeKey(x) => x,
            Self::VariableKey(x) => panic!("Called unwrap_typing on variable_key : {}", x),
        }
    }

    pub fn unwrap_variable_ref(&self) -> &VariableKey {
        match self {
            Self::VariableKey(x) => x,
            Self::TypeKey(x) => panic!("Called unwrap_variable on type_key : {}", x),
        }
    }

    pub fn unwrap_dollar(self) -> Ustr {
        match self {
            Self::TypeKey(x) => x.fully_qualified_path.name.unwrap(),
            Self::VariableKey(x) => x.fully_qualified_path.name.unwrap(),
        }
    }
}

pub enum IdentifierType<'a> {
    Generic(&'a PotentialGenericTypeIdentifier),
    Dollar(&'a PotentialDollarIdentifier),
    Ident(&'a dyn ToString),
    Ustr(Ustr),
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
                Self::Ustr(x) => x.to_string(),
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

impl<'a> From<&'a Ustr> for IdentifierType<'a> {
    fn from(val: &'a Ustr) -> IdentifierType<'a> {
        IdentifierType::Ustr(*val)
    }
}

impl<'a> From<Ustr> for IdentifierType<'a> {
    fn from(val: Ustr) -> IdentifierType<'a> {
        IdentifierType::Ustr(val)
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
        ty: &MirDataType,
        member: &impl AsRef<str>,
    ) -> Option<MirDataType> {
        self.resolve_member_fn_name(ty, member)
            .and_then(|name| self.symbols.variables.get(&name))
            .map(|var| var.data_type.clone())
    }

    pub fn resolve_member_field_type(
        &mut self,
        scope: ScopeId,
        base: &MirDataType,
        member: &Ustr,
    ) -> Option<MirDataType> {
        fn trait_member_type(
            defs: &FxHashMap<TypeKey, MiddleTrait>,
            trait_name: &TypeKey,
            member: &Ustr,
        ) -> Option<MirDataType> {
            let root = defs
                .iter()
                .find(|(name, _)| name == &trait_name)
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
                    stack.push(implied.clone());
                }
            }

            None
        }

        let out = match base {
            MirDataType::Struct { identifier, .. } => self
                .typing
                .find_object_for_struct_name(identifier)
                .and_then(|obj| match &obj.object_type {
                    MiddleTypeDefType::Struct(fields) => {
                        fields.get(member).map(|(ty, _)| ty.clone())
                    }
                    _ => None,
                }),
            MirDataType::Tuple(values) => member
                .parse::<usize>()
                .ok()
                .and_then(|idx| values.get(idx).cloned()),
            MirDataType::Option(inner) | MirDataType::Ptr(inner)
                if member == "next" || member == "0" =>
            {
                Some((**inner).clone())
            }
            MirDataType::Result { ok, err } => {
                if member == "ok" || member == "0" {
                    Some((**ok).clone())
                } else if member == "err" || member == "1" {
                    Some((**err).clone())
                } else if member == "next" {
                    if ok.loose_eq(err) {
                        Some((**ok).clone())
                    } else {
                        Some(MirDataType::Dynamic)
                    }
                } else {
                    None
                }
            }
            MirDataType::DynamicTraits(traits) => {
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
            let parser_type = ParserInnerType::from(out.clone());
            return Some(
                self.resolve_data_type(scope, &parser_type, ResolutionOptions::typing())
                    .unwrap_or(out),
            );
        }

        if let Some(imp) = self.typing.find_inherent_impl_for_type(base)
            && let Some(mapped_member) = imp.get_member(member, &[])
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
        ty: &MirDataType,
        member: &impl AsRef<str>,
    ) -> Option<VariableKey> {
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
        scope: ScopeId,
        ident: impl Into<IdentifierType<'a>>,
        options: ResolutionOptions,
    ) -> Result<Key, MiddleErr> {
        Ok(match self.resolve_potential_node(scope, ident, options)? {
            KeyOrAstNode::Node(x) => {
                return Err(self
                    .context
                    .err_at_current(MiddleErr::UnexpectedMacroArgType(x.to_string())));
            }
            KeyOrAstNode::Key(x) => x,
        })
    }

    #[instrument(skip_all)]
    pub fn resolve_potential_node<'a>(
        &'a self,
        scope: ScopeId,
        ident: impl Into<IdentifierType<'a>>,
        options: ResolutionOptions,
    ) -> Result<KeyOrAstNode, MiddleErr> {
        let ident = ident.into();
        trace!(ident = %ident, "Resolving identifier");

        let ident: Ustr = match ident {
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
                        .err_at_current(MiddleErr::InternalNoDollarResolutionAllowed));
                }
                let resolved = self
                    .scoping
                    .resolve_macro_arg(scope, &Ustr::from(&x.text))
                    .ok_or_else(|| {
                        self.context
                            .err_at_current(MiddleErr::MacroArg(x.to_string()))
                    })?;

                match &resolved.node_type {
                    AstNodeType::Identifier(x) => match x.value.get_ident() {
                        PotentialDollarIdentifier::Identifier(x) => Ustr::from(&x.text),
                        PotentialDollarIdentifier::DollarIdentifier(x) => Ustr::from(&x.text),
                    },
                    _ => {
                        return Ok(KeyOrAstNode::Node(Box::new(resolved.clone())));
                    }
                }
            }
            IdentifierType::Generic(x) => {
                let inner_ident = x.get_ident();
                Ustr::from(&inner_ident.to_string())
            }
            IdentifierType::Dollar(PotentialDollarIdentifier::Identifier(x)) => Ustr::from(&x.text),
            IdentifierType::Ident(x) => Ustr::from(&x.to_string()),
            IdentifierType::Ustr(x) => x,
        };

        if options.type_resolution {
            match ParserInnerType::from_str(&ident) {
                Ok(ParserInnerType::Struct(_) | ParserInnerType::StructWithGenerics { .. })
                | Err(_) => {}
                _ => {
                    let type_key = TypeKey {
                        fully_qualified_path: Arc::new(FullyQualifiedPath {
                            name: Some(ident),
                            parent: None,
                        }),
                    };
                    return Ok(KeyOrAstNode::Key(Key::TypeKey(type_key)));
                }
            }
        }

        trace!(ident = %ident, "Identifier resolution complete");

        for current_scope in scope.ancestors(&self.scoping.scopes) {
            if options.type_resolution {
                if let Some(key) = self
                    .typing
                    .objects
                    .keys()
                    .find(|k| k.fully_qualified_path.name == Some(ident))
                {
                    return Ok(KeyOrAstNode::Key(Key::TypeKey(key.clone())));
                }

                if let Some(key) = self
                    .typing
                    .trait_defs
                    .keys()
                    .find(|k| k.fully_qualified_path.name == Some(ident))
                {
                    return Ok(KeyOrAstNode::Key(Key::TypeKey(key.clone())));
                }

                if let Some(key) = self
                    .typing
                    .trait_impls
                    .keys()
                    .find(|k| k.fully_qualified_path.name == Some(ident))
                {
                    return Ok(KeyOrAstNode::Key(Key::TypeKey(key.clone())));
                }

                let ty = ParserDataType::from(
                    ParserInnerType::from_str(&ident)
                        .unwrap_or(ParserInnerType::Struct(ident.to_string())),
                );

                if ty.clone().is_native() {
                    let type_key = TypeKey {
                        fully_qualified_path: Arc::new(FullyQualifiedPath {
                            name: Some(ident),
                            parent: None,
                        }),
                    };
                    return Ok(KeyOrAstNode::Key(Key::TypeKey(type_key)));
                }

                let scope_ref = self.scoping.scope_or_err(current_scope)?;

                if let Some(x) = scope_ref
                    .type_mappings
                    .get(&Ustr::from(&ty.impl_name()))
                    .cloned()
                {
                    let mapped_name = Ustr::from(&ParserDataType::from(x).impl_name());
                    let type_key = TypeKey {
                        fully_qualified_path: Arc::new(FullyQualifiedPath {
                            name: Some(mapped_name),
                            parent: None,
                        }),
                    };
                    return Ok(KeyOrAstNode::Key(Key::TypeKey(type_key)));
                }

                if options.name_resolution {
                    // Try to find variable in current scope using composite key
                    let scope_fqp = &scope_ref.fully_qualified_path;
                    let mut found_var_key = None;
                    let mut highest_counter = None;

                    for key in self.symbols.variables.keys() {
                        if key.name() == &ident
                            && key.fully_qualified_path.as_ref() == scope_fqp.as_ref()
                        {
                            match (highest_counter, key.shadow_counter) {
                                (None, Some(counter)) => {
                                    highest_counter = Some(counter);
                                    found_var_key = Some(key.clone());
                                }
                                (Some(highest), Some(counter)) if counter > highest => {
                                    highest_counter = Some(counter);
                                    found_var_key = Some(key.clone());
                                }
                                (None, None) => {
                                    found_var_key = Some(key.clone());
                                }
                                _ => {}
                            }
                        }
                    }

                    if let Some(key) = found_var_key {
                        return Ok(KeyOrAstNode::Key(Key::VariableKey(key)));
                    }

                    if let Some(x) = scope_ref.mappings.get(&ident).cloned() {
                        return Ok(KeyOrAstNode::Key(Key::VariableKey(x)));
                    }
                }
            } else if options.name_resolution {
                // Try to find variable in current scope using composite key
                let scope_ref = self.scoping.scope_or_err(current_scope)?;
                let scope_fqp = &scope_ref.fully_qualified_path;
                let mut found_var_key = None;
                let mut highest_counter = None;

                for key in self.symbols.variables.keys() {
                    if key.name() == &ident
                        && key.fully_qualified_path.as_ref() == scope_fqp.as_ref()
                    {
                        match (highest_counter, key.shadow_counter) {
                            (None, Some(counter)) => {
                                highest_counter = Some(counter);
                                found_var_key = Some(key.clone());
                            }
                            (Some(highest), Some(counter)) if counter > highest => {
                                highest_counter = Some(counter);
                                found_var_key = Some(key.clone());
                            }
                            (None, None) => {
                                found_var_key = Some(key.clone());
                            }
                            _ => {}
                        }
                    }
                }

                if let Some(key) = found_var_key {
                    return Ok(KeyOrAstNode::Key(Key::VariableKey(key)));
                }

                if let Some(x) = scope_ref.mappings.get(&ident).cloned() {
                    return Ok(KeyOrAstNode::Key(Key::VariableKey(x)));
                }
            } else {
                break;
            }
        }

        if options.type_resolution {
            for key in self.typing.trait_defs.keys() {
                if key.fully_qualified_path.name == Some(ident) {
                    return Ok(KeyOrAstNode::Key(Key::TypeKey(key.clone())));
                }
            }

            for key in self.typing.objects.keys() {
                if key.fully_qualified_path.name == Some(ident) {
                    return Ok(KeyOrAstNode::Key(Key::TypeKey(key.clone())));
                }
            }

            for key in self.typing.trait_impls.keys() {
                if key.fully_qualified_path.name == Some(ident) {
                    return Ok(KeyOrAstNode::Key(Key::TypeKey(key.clone())));
                }
            }

            if self.scoping.all_time_generics.contains(&ident) {
                let type_key = TypeKey {
                    fully_qualified_path: Arc::new(FullyQualifiedPath {
                        name: Some(ident),
                        parent: None,
                    }),
                };
                return Ok(KeyOrAstNode::Key(Key::TypeKey(type_key)));
            }

            if options.name_resolution {
                for key in self.symbols.variables.keys() {
                    if key.name() == &ident {
                        return Ok(KeyOrAstNode::Key(Key::VariableKey(key.clone())));
                    }
                }
            }

            match ParserInnerType::from_str(&ident) {
                Ok(ParserInnerType::Struct(_) | ParserInnerType::StructWithGenerics { .. })
                | Err(_) => {}
                _ => {
                    let type_key = TypeKey {
                        fully_qualified_path: Arc::new(FullyQualifiedPath {
                            name: Some(ident),
                            parent: None,
                        }),
                    };
                    return Ok(KeyOrAstNode::Key(Key::TypeKey(type_key)));
                }
            }

            return Err(self
                .context
                .err_at_current(MiddleErr::Object(ident.to_string())));
        }

        if !options.name_resolution {
            let var_key = VariableKey {
                fully_qualified_path: Arc::new(FullyQualifiedPath {
                    name: Some(ident),
                    parent: None,
                }),
                shadow_counter: None,
            };
            return Ok(KeyOrAstNode::Key(Key::VariableKey(var_key)));
        }

        for key in self.symbols.variables.keys() {
            if key.name() == &ident {
                return Ok(KeyOrAstNode::Key(Key::VariableKey(key.clone())));
            }
        }

        Err(self
            .context
            .err_at_current(MiddleErr::Variable(ident.to_string())))
    }

    #[instrument(skip_all)]
    pub fn resolve_to_data_type<'a>(
        &'a mut self,
        scope: ScopeId,
        ident: impl Into<IdentifierType<'a>>,
    ) -> Result<MirDataType, MiddleErr> {
        let ident = ident.into();
        trace!(ident = %ident, "Resolving identifier");

        match ident {
            IdentifierType::Ident(x) => {
                let x = Ustr::from(&x.to_string());
                let resolved = self
                    .resolve(scope, x, ResolutionOptions::default().with_dollar())?
                    .unwrap_dollar();

                let parser_ty = match ParserInnerType::from_str(&resolved)
                    .unwrap_or(ParserInnerType::Struct(resolved.to_string()))
                {
                    ParserInnerType::Struct(x) => ParserInnerType::Struct(
                        self.resolve(scope, &x, ResolutionOptions::typing())?
                            .to_string(),
                    ),
                    x => x,
                };
                self.resolve_data_type(scope, &parser_ty, ResolutionOptions::typing())
            }
            IdentifierType::Ustr(x) => {
                let resolved = self
                    .resolve(scope, x, ResolutionOptions::default().with_dollar())?
                    .unwrap_dollar();

                let parser_ty = match ParserInnerType::from_str(&resolved)
                    .unwrap_or(ParserInnerType::Struct(resolved.to_string()))
                {
                    ParserInnerType::Struct(x) => ParserInnerType::Struct(
                        self.resolve(scope, &x, ResolutionOptions::typing())?
                            .to_string(),
                    ),
                    x => x,
                };
                self.resolve_data_type(scope, &parser_ty, ResolutionOptions::typing())
            }
            IdentifierType::Generic(PotentialGenericTypeIdentifier::Identifier(x))
            | IdentifierType::Dollar(x) => {
                let resolved = self
                    .resolve(scope, x, ResolutionOptions::default().with_dollar())?
                    .unwrap_dollar();

                let parser_ty = match ParserInnerType::from_str(&resolved)
                    .unwrap_or(ParserInnerType::Struct(resolved.to_string()))
                {
                    ParserInnerType::Struct(x) => ParserInnerType::Struct(
                        self.resolve(scope, &x, ResolutionOptions::typing())?
                            .to_string(),
                    ),
                    x => x,
                };
                self.resolve_data_type(scope, &parser_ty, ResolutionOptions::typing())
            }
            IdentifierType::Generic(PotentialGenericTypeIdentifier::Generic {
                identifier,
                generic_types,
            }) => {
                let resolved_gens: Vec<MirDataType> = generic_types
                    .iter()
                    .map(|x| self.resolve_data_type(scope, x, ResolutionOptions::typing()))
                    .collect::<Result<Vec<_>, _>>()?;

                let resolved = self.resolve(
                    scope,
                    identifier,
                    ResolutionOptions::default().with_dollar(),
                )?;

                let type_key = match resolved {
                    Key::TypeKey(k) => k,
                    Key::VariableKey(_) => {
                        return Err(self
                            .context
                            .err_at_current(MiddleErr::Object(resolved.to_string())));
                    }
                };

                let name_str = type_key.name().as_str();
                if name_str == "ptr" && resolved_gens.len() == 1 {
                    return Ok(MirDataType::Ptr(Box::new(
                        resolved_gens.into_iter().next().unwrap(),
                    )));
                }
                if name_str == "list" && resolved_gens.len() == 1 {
                    return Ok(MirDataType::List(Box::new(
                        resolved_gens.into_iter().next().unwrap(),
                    )));
                }
                if name_str == "gen" && resolved_gens.len() == 1 {
                    return Ok(MirDataType::Gen(Box::new(
                        resolved_gens.into_iter().next().unwrap(),
                    )));
                }

                Ok(MirDataType::Struct {
                    identifier: type_key,
                    generic_types: resolved_gens,
                })
            }
        }
    }

    #[instrument(skip_all)]
    pub fn resolve_data_type<'a>(
        &'a self,
        scope: ScopeId,
        data_type: impl Into<&'a ParserInnerType>,
        options: ResolutionOptions,
    ) -> Result<MirDataType, MiddleErr> {
        let data_type = data_type.into();
        trace!(data_type = %data_type, "Resolving type");

        Ok(match data_type {
            ParserInnerType::Struct(identifier) => MirDataType::Struct {
                identifier: match self.resolve(scope, identifier, options)? {
                    Key::TypeKey(x) => x,
                    _ => return Err(MiddleErr::Object(identifier.to_string())),
                },
                generic_types: Vec::new(),
            },
            ParserInnerType::StructWithGenerics {
                identifier,
                generic_types,
            } => {
                let mut resolved_gens: Vec<MirDataType> = Vec::new();
                for g in generic_types {
                    resolved_gens.push(self.resolve_data_type(scope, g, options)?);
                }

                if identifier == "ptr" && resolved_gens.len() == 1 {
                    return Ok(MirDataType::Ptr(Box::new(resolved_gens.remove(0))));
                }

                if identifier == "list" && resolved_gens.len() == 1 {
                    return Ok(MirDataType::List(Box::new(resolved_gens.remove(0))));
                }

                if identifier == "gen" && resolved_gens.len() == 1 {
                    return Ok(MirDataType::Gen(Box::new(resolved_gens.remove(0))));
                }

                let identifier = match self.resolve(scope, identifier, options)? {
                    Key::TypeKey(x) => x,
                    _ => return Err(MiddleErr::Object(identifier.to_string())),
                };

                MirDataType::Struct {
                    identifier,
                    generic_types: resolved_gens,
                }
            }
            ParserInnerType::Tuple(x) => {
                let mut lst = Vec::new();

                for x in x {
                    lst.push(self.resolve_data_type(scope, x, options)?);
                }

                MirDataType::Tuple(lst)
            }
            ParserInnerType::Function {
                return_type,
                parameters,
            } => MirDataType::Function {
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
            ParserInnerType::Ref(d_type, mutability) => MirDataType::Ref(
                Box::new(self.resolve_data_type(scope, d_type.as_ref(), options)?),
                *mutability,
            ),
            ParserInnerType::List(x) => MirDataType::List(Box::new(self.resolve_data_type(
                scope,
                x.as_ref(),
                options,
            )?)),
            ParserInnerType::Ptr(x) => MirDataType::Ptr(Box::new(self.resolve_data_type(
                scope,
                x.as_ref(),
                options,
            )?)),
            ParserInnerType::Option(x) => MirDataType::Option(Box::new(self.resolve_data_type(
                scope,
                x.as_ref(),
                options,
            )?)),
            ParserInnerType::Gen(x) => MirDataType::Gen(Box::new(self.resolve_data_type(
                scope,
                x.as_ref(),
                options,
            )?)),
            ParserInnerType::Result { ok, err } => MirDataType::Result {
                err: Box::new(self.resolve_data_type(scope, err.as_ref(), options)?),
                ok: Box::new(self.resolve_data_type(scope, ok.as_ref(), options)?),
            },
            ParserInnerType::Scope(types) => {
                let mut resolved_types = Vec::new();
                for ty in types {
                    resolved_types.push(self.resolve_data_type(scope, ty, options)?);
                }

                if resolved_types.len() == 2
                    && let MirDataType::Struct { identifier, .. } = &resolved_types[1]
                    && let Some(associated) = self
                        .typing
                        .resolve_associated_type(&resolved_types[0], identifier.name())
                {
                    return Ok(associated);
                }

                resolved_types
                    .into_iter()
                    .next()
                    .unwrap_or(MirDataType::Null)
            }
            ParserInnerType::DollarIdentifier(x) => {
                if let Some(node) = self.scoping.resolve_macro_arg(scope, &Ustr::from(x)) {
                    let AstNodeType::DataType(AstDataType { data_type }) = node.node_type.clone()
                    else {
                        unimplemented!()
                    };

                    self.resolve_data_type(scope, &data_type, options)?
                } else {
                    return Err(self.context.err_at_current(MiddleErr::MacroArg(x.clone())));
                }
            }
            ParserInnerType::DynamicTraits(traits) => MirDataType::DynamicTraits(
                traits
                    .iter()
                    .map(|t| {
                        self.resolve(scope, t, ResolutionOptions::typing())
                            .map(|x| x.unwrap_typing())
                    })
                    .collect::<Result<Vec<_>, MiddleErr>>()?,
            ),
            x => x.into(),
        })
    }
}
