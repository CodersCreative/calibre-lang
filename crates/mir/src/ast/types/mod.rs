use calibre_parser::{
    IdentifiersUsed, Span,
    ast::{
        RefMutability,
        ffi::ParserFfiInnerType,
        idents::ParserText,
        nodes::{
            AstNode, AstNodeType,
            functions::CallArg,
            lists::AstList,
            literals::{AstChar, AstFloat, AstRange, AstString, AstTuple},
        },
        types::{ParserDataType, ParserInnerType},
    },
};
use rustc_hash::FxHashMap;
use serde::{Deserialize, Serialize};
use std::{fmt::Display, hash::Hash, ops::Deref, str::FromStr};

use crate::symbols::TypeKey;

impl MirDataType {
    pub fn member_base_name_candidates(&self) -> Vec<String> {
        let mut names = Vec::new();
        let base = self.key();
        let base_key = base.to_string();

        names.push(base_key.clone());
        if let Some(x) = ParserText::get_temp_name_suffix(&base_key) {
            names.push(x);
        }

        match &base {
            MirDataType::Struct(name) => {
                names.push(name.clone());
                if let Some(x) = ParserText::get_temp_name_suffix(name) {
                    names.push(x);
                }
            }
            MirDataType::StructWithGenerics { identifier, .. } => {
                names.push(identifier.clone());
                if let Some(x) = ParserText::get_temp_name_suffix(identifier) {
                    names.push(x);
                }
            }
            _ => {}
        }

        names
    }

    pub fn canonical_args_key(args: &[MirDataType]) -> String {
        args.iter()
            .map(|x| x.to_string())
            .collect::<Vec<_>>()
            .join(", ")
    }

    pub fn impl_name(&self) -> String {
        self.impl_name()
    }

    pub fn substitute(&self, subst: &FxHashMap<String, MirDataType>) -> MirDataType {
        match &self.data_type {
            MirDataType::Struct(s) if subst.contains_key(s) => subst
                .get(s)
                .map(|dt| dt.data_type.clone())
                .unwrap_or_else(|| self.data_type.clone()),
            MirDataType::Tuple(xs) => {
                MirDataType::Tuple(xs.iter().map(|x| x.substitute(subst)).collect())
            }
            MirDataType::List(x) => MirDataType::List(Box::new(x.substitute(subst))),
            MirDataType::Ptr(x) => MirDataType::Ptr(Box::new(x.substitute(subst))),
            MirDataType::Option(x) => MirDataType::Option(Box::new(x.substitute(subst))),
            MirDataType::Result { ok, err } => MirDataType::Result {
                ok: Box::new(ok.substitute(subst)),
                err: Box::new(err.substitute(subst)),
            },
            MirDataType::Function {
                return_type,
                parameters,
            } => MirDataType::Function {
                return_type: Box::new(return_type.substitute(subst)),
                parameters: parameters.iter().map(|p| p.substitute(subst)).collect(),
            },
            MirDataType::Ref(x, m) => MirDataType::Ref(Box::new(x.substitute(subst)), *m),
            MirDataType::StructWithGenerics {
                identifier,
                generic_types,
            } => MirDataType::StructWithGenerics {
                identifier: identifier.clone(),
                generic_types: generic_types.iter().map(|g| g.substitute(subst)).collect(),
            },
            _ => self.data_type.clone(),
        }
    }
}

impl IdentifiersUsed for MirDataType {
    fn identifiers_used(&self) -> Vec<&String> {
        let mut types = Vec::new();
        match &self.data_type {
            MirDataType::Struct(name) => {
                types.push(name);
            }
            MirDataType::StructWithGenerics { identifier, .. } => {
                types.push(identifier);
            }
            MirDataType::Function {
                return_type,
                parameters,
            } => {
                types.extend(return_type.identifiers_used());
                for param in parameters {
                    types.extend(param.identifiers_used());
                }
            }
            MirDataType::Option(inner) => {
                types.extend(inner.identifiers_used());
            }
            MirDataType::Result { ok, err } => {
                types.extend(ok.identifiers_used());
                types.extend(err.identifiers_used());
            }
            _ => {}
        }
        types
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize, Default)]
pub enum MirDataType {
    Float,
    UInt,
    Byte,
    Int,
    Big,
    #[default]
    Null,
    Bool,
    Str,
    Char,
    Host,
    Dynamic,
    DynamicTraits(Vec<TypeKey>),
    Tuple(Vec<MirDataType>),
    List(Box<MirDataType>),
    Gen(Box<MirDataType>),
    Range,
    Option(Box<MirDataType>),
    Result {
        ok: Box<MirDataType>,
        err: Box<MirDataType>,
    },
    Function {
        return_type: Box<MirDataType>,
        parameters: Vec<MirDataType>,
    },
    Ref(Box<MirDataType>, RefMutability),
    Struct {
        identifier: TypeKey,
        generic_types: Vec<MirDataType>,
    },
    FfiType(ParserFfiInnerType),
    NativeFunction {
        return_type: Box<MirDataType>,
        parameters: Vec<MirDataType>,
    },
    Ptr(Box<MirDataType>),
}

impl From<MirDataType> for ParserDataType {
    fn from(value: MirDataType) -> Self {
        ParserDataType {
            data_type: value.into(),
            span: Span::default(),
        }
    }
}

impl From<MirDataType> for ParserInnerType {
    fn from(value: MirDataType) -> Self {
        match value {
            MirDataType::Big => ParserInnerType::Big,
            MirDataType::Bool => ParserInnerType::Bool,
            MirDataType::Byte => ParserInnerType::Byte,
            MirDataType::Char => ParserInnerType::Char,
            MirDataType::Dynamic => ParserInnerType::Dynamic,
            MirDataType::Float => ParserInnerType::Float,
            MirDataType::Int => ParserInnerType::Int,
            MirDataType::Host => ParserInnerType::Host,
            MirDataType::Str => ParserInnerType::Str,
            MirDataType::Range => ParserInnerType::Range,
            MirDataType::Null => ParserInnerType::Null,
            MirDataType::UInt => ParserInnerType::UInt,
            MirDataType::DynamicTraits(x) => ParserInnerType::DynamicTraits(
                x.into_iter()
                    .map(|x| x.fully_qualified_path.name.unwrap_or_default().to_string())
                    .collect(),
            ),
            MirDataType::FfiType(x) => ParserInnerType::FfiType(x),
            MirDataType::Function {
                return_type,
                parameters,
            } => ParserInnerType::Function {
                return_type: Box::new(return_type.into()),
                parameters: parameters.into_iter().map(ParserDataType::from).collect(),
            },
            MirDataType::NativeFunction {
                return_type,
                parameters,
            } => ParserInnerType::NativeFunction {
                return_type: Box::new(return_type.into()),
                parameters: parameters.into_iter().map(ParserDataType::from).collect(),
            },
            MirDataType::Gen(x) => ParserInnerType::Gen(Box::new(x.into())),
            MirDataType::List(x) => ParserInnerType::List(Box::new(x.into())),
            MirDataType::Option(x) => ParserInnerType::Option(Box::new(x.into())),
            MirDataType::Ptr(x) => ParserInnerType::Ptr(Box::new(x.into())),
            MirDataType::Ref(x, mutability) => ParserInnerType::Ref(Box::new(x.into()), mutability),
            MirDataType::Result { ok, err } => ParserInnerType::Result {
                ok: Box::new(ok.into()),
                err: Box::new(err.into()),
            },
            MirDataType::Tuple(values) => {
                ParserInnerType::Tuple(values.into_iter().map(ParserDataType::from).collect())
            }
            MirDataType::Struct {
                identifier,
                generic_types,
            } if generic_types.is_empty() => ParserInnerType::Struct(
                identifier
                    .fully_qualified_path
                    .name
                    .unwrap_or_default()
                    .to_string(),
            ),
            MirDataType::Struct {
                identifier,
                generic_types,
            } => ParserInnerType::StructWithGenerics {
                identifier: identifier
                    .fully_qualified_path
                    .name
                    .unwrap_or_default()
                    .to_string(),
                generic_types: generic_types
                    .into_iter()
                    .map(ParserDataType::from)
                    .collect(),
            },
        }
    }
}

impl AlphaRenamable for MirDataType {
    fn rename(&mut self, state: &mut AlphaRenameState) {
        match self {
            MirDataType::Auto(_)
            | MirDataType::Big
            | MirDataType::Byte
            | MirDataType::Bool
            | MirDataType::Char
            | MirDataType::DollarIdentifier(_)
            | MirDataType::FfiType(_)
            | MirDataType::Float
            | MirDataType::Host
            | MirDataType::Int
            | MirDataType::Null
            | MirDataType::Range
            | MirDataType::UInt
            | MirDataType::Str
            | MirDataType::Dynamic => {}
            MirDataType::Struct(x) => {
                *x = state.mapped_str_or_original(x);
            }
            MirDataType::StructWithGenerics {
                identifier,
                generic_types,
            } => {
                *identifier = state.mapped_str_or_original(identifier);
                for g in generic_types {
                    g.rename(state);
                }
            }
            MirDataType::DynamicTraits(x) => {
                for item in x {
                    *item = state.mapped_str_or_original(item);
                }
            }
            MirDataType::List(x) => x.rename(state),
            MirDataType::Option(x) => x.rename(state),
            MirDataType::Result { ok, err } => {
                ok.rename(state);
                err.rename(state);
            }
            MirDataType::Ptr(x) => x.rename(state),
            MirDataType::NativeFunction {
                return_type,
                parameters,
            } => {
                return_type.rename(state);
                for p in parameters {
                    p.rename(state);
                }
            }
            MirDataType::Function {
                return_type,
                parameters,
            } => {
                return_type.rename(state);
                for p in parameters {
                    p.rename(state);
                }
            }
            MirDataType::Tuple(x) => {
                for item in x {
                    item.rename(state);
                }
            }
            MirDataType::Gen(x) => x.rename(state),
            MirDataType::Ref(x, _) => x.rename(state),
            // TODO Implement
            MirDataType::Scope(_) => {}
        }
    }
}

impl MirDataType {
    pub fn null(span: Span) -> Self {
        MirDataType::Null
    }

    pub fn object(identifier: TypeKey) -> Self {
        MirDataType::Struct {
            identifier,
            generic_types: Vec::new(),
        }
    }

    pub fn loose_eq(&self, other: &Self) -> bool {
        self.data_type.loose_eq(&other.data_type) || self.key().loose_eq(&other.key())
    }

    pub fn function(
        span: Span,
        parameters: Vec<MirDataType>,
        return_type: MirDataType,
    ) -> MirDataType {
        MirDataType::Function {
            return_type: Box::new(return_type),
            parameters,
        }
    }

    pub fn unwrap_all_refs(self) -> Self {
        Self {
            data_type: self.data_type.unwrap_all_refs().clone(),
            span: self.span,
        }
    }

    pub fn contains_auto(&self) -> bool {
        self.data_type.contains_auto()
    }

    pub fn unwrap_one_result(&self) -> Option<&Self> {
        self.data_type.unwrap_one_result()
    }

    pub fn get_gen(self) -> Option<MirDataType> {
        match self.unwrap_all_refs().data_type {
            MirDataType::Gen(x) => Some(*x),
            _ => None,
        }
    }

    pub fn is_int(self) -> bool {
        matches!(
            self.unwrap_all_refs().resolve_ffi().data_type,
            MirDataType::Int | MirDataType::UInt | MirDataType::Byte
        )
    }

    pub fn is_native(self) -> bool {
        !matches!(
            self.unwrap_all_refs().resolve_ffi().data_type,
            MirDataType::Struct(_) | MirDataType::StructWithGenerics { .. }
        )
    }

    pub fn default_node(&self, span: Span) -> Option<AstNode> {
        match self {
            MirDataType::Int => Some(AstNode::int(span, 0)),
            MirDataType::UInt => Some(AstNode::int(span, "0u")),
            MirDataType::Byte => Some(AstNode::int(span, "0b")),
            MirDataType::Str => Some(AstNode::new(
                span,
                AstNodeType::StringLiteral(AstString {
                    value: ParserText::new(self.span, ""),
                }),
            )),
            MirDataType::Char => Some(AstNode::new(
                span,
                AstNodeType::CharLiteral(AstChar { value: '\0' }),
            )),
            MirDataType::Float => Some(AstNode::new(
                span,
                AstNodeType::FloatLiteral(AstFloat {
                    value: 0.0,
                    format: None,
                }),
            )),
            MirDataType::Auto(_) => Some(AstNode::new(span, AstNodeType::Null)),
            MirDataType::Dynamic => Some(AstNode::new(span, AstNodeType::Null)),
            MirDataType::Null => Some(AstNode::new(span, AstNodeType::Null)),
            MirDataType::List(t) => Some(AstNode::new(
                span,
                AstNodeType::ListLiteral(AstList {
                    data_type: *t.clone(),
                    values: Vec::new(),
                }),
            )),
            MirDataType::Range => Some(AstNode::new(
                span,
                AstNodeType::RangeDeclaration(AstRange {
                    from: Box::new(AstNode::int(span, 0)),
                    to: Box::new(AstNode::int(span, 0)),
                    inclusive: true,
                }),
            )),
            MirDataType::Bool => Some(AstNode::bool(span, false)),
            MirDataType::Tuple(values) => Some(AstNode::new(
                span,
                AstNodeType::TupleLiteral(AstTuple {
                    values: values.iter().filter_map(|x| x.default_node(span)).collect(),
                }),
            )),
            MirDataType::Option(_) => Some(AstNode::none(self.span)),
            MirDataType::Result { ok, .. } => Some(AstNode::call(
                span,
                AstNode::identifier(span, "ok"),
                vec![CallArg::Value(ok.default_node(span)?)],
            )),
            _ => None,
        }
    }

    pub fn verify(self) -> Self {
        Self {
            data_type: self.data_type.verify(),
            span: self.span,
        }
    }

    pub fn resolve_ffi(self) -> Self {
        Self {
            data_type: self.data_type.resolve_ffi(),
            span: self.span,
        }
    }
}

impl MirDataType {
    pub fn unwrap_all_refs(&self) -> &Self {
        match self {
            Self::Ref(x, _) => x.data_type.unwrap_all_refs(),
            _ => self,
        }
    }

    pub fn unwrap_one_result(&self) -> Option<&MirDataType> {
        match self {
            MirDataType::Result { ok, err: _ } => Some(ok),
            _ => None,
        }
    }

    pub fn is_callable(&self) -> bool {
        matches!(
            self.unwrap_all_refs(),
            MirDataType::Function { .. } | MirDataType::NativeFunction { .. }
        )
    }

    pub fn key(&self) -> MirDataType {
        match self.unwrap_all_refs().clone() {
            MirDataType::StructWithGenerics {
                identifier,
                generic_types: _,
            } => MirDataType::Struct(identifier),
            MirDataType::List(_) => MirDataType::Struct(String::from("list")),
            MirDataType::Ptr(_) => MirDataType::Struct(String::from("ptr")),
            MirDataType::Gen(_) => MirDataType::Struct(String::from("gen")),
            MirDataType::Option(_) => MirDataType::Struct(String::from("option")),
            MirDataType::Result { .. } => MirDataType::Struct(String::from("result")),
            x => x,
        }
    }

    pub fn impl_name(&self) -> String {
        match self.key() {
            MirDataType::StructWithGenerics { identifier, .. }
            | MirDataType::Struct(identifier) => identifier,
            other => other.to_string(),
        }
    }

    pub fn is_auto(&self) -> bool {
        matches!(self, Self::Auto(_))
    }

    pub fn is_dyn(&self) -> bool {
        matches!(self, Self::Dynamic)
    }

    pub fn is_dyn_trait(&self) -> bool {
        matches!(self, Self::DynamicTraits { .. })
    }

    pub fn is_result(&self) -> bool {
        matches!(self, Self::Result { .. })
    }

    pub fn is_option(&self) -> bool {
        matches!(self, Self::Option(_))
    }

    pub fn is_ref(&self) -> bool {
        matches!(self, Self::Ref(_, _))
    }

    pub fn is_bool(&self) -> bool {
        matches!(self, Self::Bool)
    }

    pub fn is_null(&self) -> bool {
        matches!(self, Self::Null)
    }

    pub fn is_host(&self) -> bool {
        matches!(self, Self::Host)
    }

    pub fn is_list(&self) -> bool {
        matches!(self, Self::List(_))
    }

    pub fn is_dyn_list(&self) -> bool {
        matches!(self, Self::List(x) if x.is_dyn() || x.is_dyn_trait() || x.is_dyn_list())
    }

    pub fn is_tuple(&self) -> bool {
        matches!(self, Self::Tuple(_))
    }

    pub fn loose_eq(&self, other: &Self) -> bool {
        other.is_auto()
            || other.is_tuple()
            || other.is_host()
            || other.is_dyn()
            || other.is_dyn_list()
            || other.is_dyn_trait()
            || self.is_auto()
            || self.is_tuple()
            || self.is_host()
            || self.is_dyn()
            || self.is_dyn_list()
            || self.is_dyn_trait()
            || other == self
            || self.impl_name() == other.impl_name()
            || self.clone().resolve_ffi() == other.clone().resolve_ffi()
    }

    #[inline]
    pub fn is_gen(&self) -> bool {
        let short =
            ParserText::get_temp_name_suffix(&self.impl_name()).unwrap_or_else(|| self.impl_name());
        short == "gen" || short.starts_with("gen:<")
    }

    pub fn verify(self) -> Self {
        match self {
            Self::Result { ok, err } => Self::Result {
                ok: Box::new(ok.verify()),
                err: Box::new(err.verify()),
            },
            Self::Ref(x, y) => Self::Ref(Box::new(x.verify()), y),
            Self::Ptr(x) => Self::Ptr(Box::new(x.verify())),
            Self::Option(x) => Self::Option(Box::new(x.verify())),
            Self::List(x) => Self::List(Box::new(x.verify())),
            Self::Tuple(x) => Self::Tuple(x.into_iter().map(|x| x.verify()).collect()),
            Self::DynamicTraits(traits) => {
                let mut normalized = traits
                    .into_iter()
                    .map(|s| s.trim().to_string())
                    .filter(|s| !s.is_empty())
                    .collect::<Vec<_>>();
                normalized.sort();
                normalized.dedup();
                if normalized.is_empty() {
                    Self::Dynamic
                } else {
                    Self::DynamicTraits(normalized)
                }
            }
            ty => ty,
        }
    }

    pub fn contains_auto(&self) -> bool {
        match self {
            MirDataType::Auto(_) => true,
            MirDataType::Tuple(xs) => xs.iter().any(|x| x.contains_auto()),
            MirDataType::List(x) => x.contains_auto(),
            MirDataType::Ptr(x) => x.contains_auto(),
            MirDataType::Option(x) => x.contains_auto(),
            MirDataType::Result { ok, err } => ok.contains_auto() || err.contains_auto(),
            MirDataType::Function {
                return_type,
                parameters,
                ..
            } => return_type.contains_auto() || parameters.iter().any(|x| x.contains_auto()),
            MirDataType::Ref(x, _) => x.contains_auto(),
            MirDataType::StructWithGenerics { generic_types, .. } => {
                generic_types.iter().any(|x| x.contains_auto())
            }
            MirDataType::Scope(x) => x.iter().any(|x| x.contains_auto()),
            MirDataType::DynamicTraits(_) => false,
            _ => false,
        }
    }

    pub fn resolve_ffi(self) -> Self {
        match self {
            Self::FfiType(ffi) => ffi.into(),
            Self::Result { ok, err } => Self::Result {
                ok: Box::new(ok.resolve_ffi()),
                err: Box::new(err.resolve_ffi()),
            },
            Self::Ref(x, m) => Self::Ref(Box::new(x.resolve_ffi()), m),
            Self::Ptr(x) => Self::Ptr(Box::new(x.resolve_ffi())),
            Self::Option(x) => Self::Option(Box::new(x.resolve_ffi())),
            Self::List(x) => Self::List(Box::new(x.resolve_ffi())),
            Self::Tuple(x) => Self::Tuple(x.into_iter().map(|x| x.resolve_ffi()).collect()),
            Self::Function {
                return_type,
                parameters,
            } => Self::Function {
                return_type: Box::new(return_type.resolve_ffi()),
                parameters: parameters.into_iter().map(|x| x.resolve_ffi()).collect(),
            },
            Self::StructWithGenerics {
                identifier,
                generic_types,
            } => Self::StructWithGenerics {
                identifier,
                generic_types: generic_types.into_iter().map(|x| x.resolve_ffi()).collect(),
            },
            Self::Scope(x) => Self::Scope(x.into_iter().map(|x| x.resolve_ffi()).collect()),
            Self::DynamicTraits(x) => Self::DynamicTraits(x),
            x => x,
        }
    }

    #[inline]
    pub fn apply_callable(&self) -> Option<MirDataType> {
        match self {
            MirDataType::Function { return_type, .. }
            | MirDataType::NativeFunction { return_type, .. } => Some(*return_type.clone()),
            _ => None,
        }
    }

    pub fn matches(&self, other: &Self, generic_params: &[&str]) -> bool {
        match (self, other) {
            (MirDataType::Struct(a), _) if generic_params.contains(&a.as_str()) => true,
            (MirDataType::Struct(a), MirDataType::Struct(b)) if a == b => true,
            (MirDataType::StructWithGenerics { identifier: a, .. }, MirDataType::Struct(b))
                if b == a =>
            {
                true
            }
            (MirDataType::Struct(a), MirDataType::StructWithGenerics { identifier: b, .. }) => {
                a == b
            }
            (
                MirDataType::StructWithGenerics {
                    identifier: a,
                    generic_types: ag,
                },
                MirDataType::StructWithGenerics {
                    identifier: b,
                    generic_types: bg,
                },
            ) => {
                if a != b || ag.len() != bg.len() {
                    return false;
                }
                ag.iter()
                    .zip(bg.iter())
                    .all(|(x, y)| x.data_type.matches(&y.data_type, generic_params))
            }
            (MirDataType::List(a), MirDataType::List(b)) => {
                a.data_type.matches(&b.data_type, generic_params)
            }
            (MirDataType::Option(a), MirDataType::Option(b)) => {
                a.data_type.matches(&b.data_type, generic_params)
            }
            (MirDataType::Result { ok: ao, err: ae }, MirDataType::Result { ok: bo, err: be }) => {
                ao.data_type.matches(&bo.data_type, generic_params)
                    && ae.data_type.matches(&be.data_type, generic_params)
            }
            (MirDataType::Ptr(a), MirDataType::Ptr(b)) => {
                a.data_type.matches(&b.data_type, generic_params)
            }
            (MirDataType::Ref(a, _), MirDataType::Ref(b, _)) => {
                a.data_type.matches(&b.data_type, generic_params)
            }
            (MirDataType::Tuple(a), MirDataType::Tuple(b)) => {
                if a.len() != b.len() {
                    return false;
                }
                a.iter()
                    .zip(b.iter())
                    .all(|(x, y)| x.data_type.matches(&y.data_type, generic_params))
            }
            (x, y) => x == y,
        }
    }
}

impl Display for MirDataType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.data_type)
    }
}
