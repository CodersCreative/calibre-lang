use crate::{
    IdentifiersUsed, ParserError, Span,
    ast::{
        RefMutability,
        ffi::ParserFfiInnerType,
        idents::{ParserText, PotentialDollarIdentifier},
        nodes::{AstNode, AstNodeType, CallArg},
    },
};
use rustc_hash::FxHashMap;
use serde::{Deserialize, Serialize};
use std::{fmt::Display, hash::Hash, ops::Deref, str::FromStr};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ParserDataType {
    pub data_type: ParserInnerType,
    pub span: Span,
}

impl PartialEq for ParserDataType {
    fn eq(&self, other: &Self) -> bool {
        self.data_type == other.data_type
    }
}

impl Eq for ParserDataType {}

impl Hash for ParserDataType {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.data_type.hash(state);
    }
}

impl From<ParserInnerType> for ParserDataType {
    fn from(value: ParserInnerType) -> Self {
        Self {
            data_type: value,
            span: Span::default(),
        }
    }
}

impl<'a> From<&'a ParserDataType> for &'a ParserInnerType {
    fn from(value: &'a ParserDataType) -> Self {
        &value.data_type
    }
}

impl ParserDataType {
    pub fn new(span: Span, data_type: ParserInnerType) -> Self {
        Self { data_type, span }
    }

    pub fn key(&self) -> ParserInnerType {
        self.data_type.key()
    }

    pub fn member_base_name_candidates(&self) -> Vec<String> {
        let mut names = Vec::new();
        let base = self.key();
        let base_key = base.to_string();

        names.push(base_key.clone());
        if let Some(x) = ParserText::get_temp_name_suffix(&base_key) {
            names.push(x);
        }

        match &base {
            ParserInnerType::Struct(name) => {
                names.push(name.clone());
                if let Some(x) = ParserText::get_temp_name_suffix(name) {
                    names.push(x);
                }
            }
            ParserInnerType::StructWithGenerics { identifier, .. } => {
                names.push(identifier.clone());
                if let Some(x) = ParserText::get_temp_name_suffix(identifier) {
                    names.push(x);
                }
            }
            _ => {}
        }

        names
    }

    pub fn canonical_args_key(args: &[ParserDataType]) -> String {
        args.iter()
            .map(|x| x.to_string())
            .collect::<Vec<_>>()
            .join(", ")
    }

    pub fn impl_name(&self) -> String {
        self.data_type.impl_name()
    }

    pub fn substitute(&self, subst: &FxHashMap<String, ParserDataType>) -> ParserDataType {
        let span = self.span;
        let data_type = match &self.data_type {
            ParserInnerType::Struct(s) if subst.contains_key(s) => subst
                .get(s)
                .map(|dt| dt.data_type.clone())
                .unwrap_or_else(|| self.data_type.clone()),
            ParserInnerType::Tuple(xs) => {
                ParserInnerType::Tuple(xs.iter().map(|x| x.substitute(subst)).collect())
            }
            ParserInnerType::List(x) => ParserInnerType::List(Box::new(x.substitute(subst))),
            ParserInnerType::Ptr(x) => ParserInnerType::Ptr(Box::new(x.substitute(subst))),
            ParserInnerType::Option(x) => ParserInnerType::Option(Box::new(x.substitute(subst))),
            ParserInnerType::Result { ok, err } => ParserInnerType::Result {
                ok: Box::new(ok.substitute(subst)),
                err: Box::new(err.substitute(subst)),
            },
            ParserInnerType::Function {
                return_type,
                parameters,
            } => ParserInnerType::Function {
                return_type: Box::new(return_type.substitute(subst)),
                parameters: parameters.iter().map(|p| p.substitute(subst)).collect(),
            },
            ParserInnerType::Ref(x, m) => ParserInnerType::Ref(Box::new(x.substitute(subst)), *m),
            ParserInnerType::StructWithGenerics {
                identifier,
                generic_types,
            } => ParserInnerType::StructWithGenerics {
                identifier: identifier.clone(),
                generic_types: generic_types.iter().map(|g| g.substitute(subst)).collect(),
            },
            _ => self.data_type.clone(),
        };

        ParserDataType { data_type, span }
    }
}

impl IdentifiersUsed for ParserDataType {
    fn identifiers_used(&self) -> Vec<&String> {
        let mut types = Vec::new();
        match &self.data_type {
            ParserInnerType::Struct(name) => {
                types.push(name);
            }
            ParserInnerType::StructWithGenerics { identifier, .. } => {
                types.push(identifier);
            }
            ParserInnerType::Function {
                return_type,
                parameters,
            } => {
                types.extend(return_type.identifiers_used());
                for param in parameters {
                    types.extend(param.identifiers_used());
                }
            }
            ParserInnerType::Option(inner) => {
                types.extend(inner.identifiers_used());
            }
            ParserInnerType::Result { ok, err } => {
                types.extend(ok.identifiers_used());
                types.extend(err.identifiers_used());
            }
            _ => {}
        }
        types
    }
}

impl Deref for ParserDataType {
    type Target = ParserInnerType;
    fn deref(&self) -> &Self::Target {
        &self.data_type
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum ParserInnerType {
    Float,
    UInt,
    Byte,
    Int,
    Big,
    Null,
    Bool,
    Str,
    Char,
    Host,
    Dynamic,
    DynamicTraits(Vec<String>),
    Tuple(Vec<ParserDataType>),
    List(Box<ParserDataType>),
    Gen(Box<ParserDataType>),
    Scope(Vec<ParserDataType>),
    Auto(Option<u16>),
    Range,
    DollarIdentifier(String),
    Option(Box<ParserDataType>),
    Result {
        ok: Box<ParserDataType>,
        err: Box<ParserDataType>,
    },
    Function {
        return_type: Box<ParserDataType>,
        parameters: Vec<ParserDataType>,
    },
    Ref(Box<ParserDataType>, RefMutability),
    Struct(String),
    StructWithGenerics {
        identifier: String,
        generic_types: Vec<ParserDataType>,
    },
    FfiType(ParserFfiInnerType),
    NativeFunction {
        return_type: Box<ParserDataType>,
        parameters: Vec<ParserDataType>,
    },
    Ptr(Box<ParserDataType>),
}

impl ParserDataType {
    pub fn null(span: Span) -> Self {
        ParserDataType {
            data_type: ParserInnerType::Null,
            span,
        }
    }

    pub fn auto(span: Span) -> Self {
        ParserDataType {
            data_type: ParserInnerType::Auto(None),
            span,
        }
    }

    pub fn object(span: Span, identifier: impl ToString) -> Self {
        ParserDataType {
            data_type: ParserInnerType::Struct(identifier.to_string()),
            span,
        }
    }

    pub fn loose_eq(&self, other: &Self) -> bool {
        self.key().loose_eq(&other.key())
    }

    pub fn function(
        span: Span,
        parameters: Vec<ParserDataType>,
        return_type: ParserDataType,
    ) -> ParserDataType {
        ParserDataType::new(
            span,
            ParserInnerType::Function {
                return_type: Box::new(return_type),
                parameters,
            },
        )
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

    pub fn get_gen(self) -> Option<ParserDataType> {
        match self.unwrap_all_refs().data_type {
            ParserInnerType::Gen(x) => Some(*x),
            _ => None,
        }
    }

    pub fn default_node(&self) -> Option<AstNode> {
        match &self.data_type {
            ParserInnerType::Int => Some(AstNode::int(self.span, 0)),
            ParserInnerType::UInt => Some(AstNode::int(self.span, "0u")),
            ParserInnerType::Byte => Some(AstNode::int(self.span, "0b")),
            ParserInnerType::Str => Some(AstNode::new(
                self.span,
                AstNodeType::StringLiteral(ParserText::new(self.span, "")),
            )),
            ParserInnerType::Char => Some(AstNode::new(self.span, AstNodeType::CharLiteral('\0'))),
            ParserInnerType::Float => Some(AstNode::new(self.span, AstNodeType::FloatLiteral(0.0))),
            ParserInnerType::Auto(_) => Some(AstNode::new(self.span, AstNodeType::Null)),
            ParserInnerType::Dynamic => Some(AstNode::new(self.span, AstNodeType::Null)),
            ParserInnerType::Null => Some(AstNode::new(self.span, AstNodeType::Null)),
            ParserInnerType::List(t) => Some(AstNode::new(
                self.span,
                AstNodeType::ListLiteral(*t.clone(), Vec::new()),
            )),
            ParserInnerType::Range => Some(AstNode::new(
                self.span,
                AstNodeType::RangeDeclaration {
                    from: Box::new(AstNode::int(self.span, 0)),
                    to: Box::new(AstNode::int(self.span, 0)),
                    inclusive: true,
                },
            )),
            ParserInnerType::Bool => Some(AstNode::bool(self.span, false)),
            ParserInnerType::Tuple(values) => Some(AstNode::new(
                self.span,
                AstNodeType::TupleLiteral {
                    values: values.iter().filter_map(|x| x.default_node()).collect(),
                },
            )),
            ParserInnerType::Option(_) => Some(AstNode::none(self.span)),
            ParserInnerType::Result { ok, .. } => Some(AstNode::call(
                self.span,
                AstNode::identifier(self.span, "ok"),
                vec![CallArg::Value(ok.default_node()?)],
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

impl FromStr for ParserInnerType {
    type Err = ParserError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(match s {
            "int" => Self::Int,
            "big" => Self::Big,
            "uint" => Self::UInt,
            "byte" => Self::Byte,
            "float" => Self::Float,
            "bool" => Self::Bool,
            "range" => Self::Range,
            "str" => Self::Str,
            "char" => Self::Char,
            "dyn" => Self::Dynamic,
            "gen" => Self::Gen(Box::new(ParserDataType::auto(Span::default()))),
            "option" => Self::Option(Box::new(ParserDataType::auto(Span::default()))),
            "result" => Self::Result {
                ok: Box::new(ParserDataType::auto(Span::default())),
                err: Box::new(ParserDataType::auto(Span::default())),
            },
            "ptr" => Self::Ptr(Box::new(ParserDataType::auto(Span::default()))),
            "list" => Self::List(Box::new(ParserDataType::auto(Span::default()))),
            "host" => Self::Host,
            "null" => Self::Null,
            "auto" => Self::Auto(None),
            _ => Self::Struct(s.to_string()),
        })
    }
}

impl ParserInnerType {
    pub fn unwrap_all_refs(&self) -> &Self {
        match self {
            Self::Ref(x, _) => x.data_type.unwrap_all_refs(),
            _ => self,
        }
    }

    pub fn unwrap_one_result(&self) -> Option<&ParserDataType> {
        match self {
            ParserInnerType::Result { ok, err: _ } => Some(ok),
            _ => None,
        }
    }

    pub fn is_callable(&self) -> bool {
        matches!(
            self.unwrap_all_refs(),
            ParserInnerType::Function { .. } | ParserInnerType::NativeFunction { .. }
        )
    }

    pub fn key(&self) -> ParserInnerType {
        match self.unwrap_all_refs().clone() {
            ParserInnerType::StructWithGenerics {
                identifier,
                generic_types: _,
            } => ParserInnerType::Struct(identifier),
            ParserInnerType::List(_) => ParserInnerType::Struct(String::from("list")),
            ParserInnerType::Ptr(_) => ParserInnerType::Struct(String::from("ptr")),
            ParserInnerType::Gen(_) => ParserInnerType::Struct(String::from("gen")),
            ParserInnerType::Option(_) => ParserInnerType::Struct(String::from("option")),
            ParserInnerType::Result { .. } => ParserInnerType::Struct(String::from("result")),
            x => x,
        }
    }

    pub fn impl_name(&self) -> String {
        match self.key() {
            ParserInnerType::StructWithGenerics { identifier, .. }
            | ParserInnerType::Struct(identifier) => identifier,
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

    pub fn is_bool(&self) -> bool {
        matches!(self, Self::Bool)
    }

    pub fn is_null(&self) -> bool {
        matches!(self, Self::Null)
    }

    pub fn is_list(&self) -> bool {
        matches!(self, Self::List(_))
    }

    pub fn loose_eq(&self, other: &Self) -> bool {
        other.is_auto()
            || other.is_dyn()
            || other.is_dyn_trait()
            || self.is_dyn()
            || self.is_dyn_trait()
            || other == self
            || self.impl_name() == other.impl_name()
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
            Self::Struct(x) => Self::from_str(&x).unwrap_or(Self::Struct(x)),
            ty => ty,
        }
    }

    pub fn contains_auto(&self) -> bool {
        match self {
            ParserInnerType::Auto(_) => true,
            ParserInnerType::Tuple(xs) => xs.iter().any(|x| x.contains_auto()),
            ParserInnerType::List(x) => x.contains_auto(),
            ParserInnerType::Ptr(x) => x.contains_auto(),
            ParserInnerType::Option(x) => x.contains_auto(),
            ParserInnerType::Result { ok, err } => ok.contains_auto() || err.contains_auto(),
            ParserInnerType::Function {
                return_type,
                parameters,
                ..
            } => return_type.contains_auto() || parameters.iter().any(|x| x.contains_auto()),
            ParserInnerType::Ref(x, _) => x.contains_auto(),
            ParserInnerType::StructWithGenerics { generic_types, .. } => {
                generic_types.iter().any(|x| x.contains_auto())
            }
            ParserInnerType::Scope(x) => x.iter().any(|x| x.contains_auto()),
            ParserInnerType::DynamicTraits(_) => false,
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
    pub fn apply_callable(&self) -> Option<ParserDataType> {
        match self {
            ParserInnerType::Function { return_type, .. }
            | ParserInnerType::NativeFunction { return_type, .. } => Some(*return_type.clone()),
            _ => None,
        }
    }

    pub fn matches(&self, other: &Self, generic_params: &[String]) -> bool {
        match (self, other) {
            (ParserInnerType::Struct(a), _) if generic_params.contains(a) => true,
            (ParserInnerType::Struct(a), ParserInnerType::Struct(b)) if a == b => true,
            (
                ParserInnerType::StructWithGenerics { identifier: a, .. },
                ParserInnerType::Struct(b),
            ) if b == a => true,
            (
                ParserInnerType::Struct(a),
                ParserInnerType::StructWithGenerics { identifier: b, .. },
            ) => a == b,
            (
                ParserInnerType::StructWithGenerics {
                    identifier: a,
                    generic_types: ag,
                },
                ParserInnerType::StructWithGenerics {
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
            (ParserInnerType::List(a), ParserInnerType::List(b)) => {
                a.data_type.matches(&b.data_type, generic_params)
            }
            (ParserInnerType::Option(a), ParserInnerType::Option(b)) => {
                a.data_type.matches(&b.data_type, generic_params)
            }
            (
                ParserInnerType::Result { ok: ao, err: ae },
                ParserInnerType::Result { ok: bo, err: be },
            ) => {
                ao.data_type.matches(&bo.data_type, generic_params)
                    && ae.data_type.matches(&be.data_type, generic_params)
            }
            (ParserInnerType::Ptr(a), ParserInnerType::Ptr(b)) => {
                a.data_type.matches(&b.data_type, generic_params)
            }
            (ParserInnerType::Ref(a, _), ParserInnerType::Ref(b, _)) => {
                a.data_type.matches(&b.data_type, generic_params)
            }
            (ParserInnerType::Tuple(a), ParserInnerType::Tuple(b)) => {
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

impl Display for ParserDataType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.data_type)
    }
}

impl Display for ParserInnerType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Float => write!(f, "float"),
            Self::Int => write!(f, "int"),
            Self::Big => write!(f, "big"),
            Self::UInt => write!(f, "uint"),
            Self::Byte => write!(f, "byte"),
            Self::Null => write!(f, "null"),
            Self::Host => write!(f, "host"),
            Self::Dynamic => write!(f, "dyn"),
            Self::DynamicTraits(traits) => {
                if traits.is_empty() {
                    write!(f, "dyn")
                } else {
                    write!(f, "dyn:<{}>", traits.join(", "))
                }
            }
            Self::Bool => write!(f, "bool"),
            Self::Str => write!(f, "str"),
            Self::Char => write!(f, "char"),
            Self::Range => write!(f, "range"),
            Self::Auto(_) => write!(f, "auto"),
            Self::DollarIdentifier(x) => write!(f, "${}", x),
            Self::Ref(typ, mutability) => {
                write!(f, "{}", mutability.fmt_with_val(&typ.to_string()))
            }
            Self::Result { err, ok } => write!(f, "{}!{}", err, ok),
            Self::Option(x) => write!(f, "{}?", x),
            Self::Ptr(x) => write!(f, "ptr:<{}>", x),
            Self::Gen(x) => write!(f, "gen:<{}>", x),
            Self::Struct(x) => write!(f, "{}", x),
            Self::StructWithGenerics {
                identifier,
                generic_types,
            } => {
                if generic_types.is_empty() {
                    write!(f, "{}", identifier)
                } else {
                    write!(
                        f,
                        "{}:<{}>",
                        identifier,
                        generic_types
                            .iter()
                            .map(|x| x.to_string())
                            .collect::<Vec<_>>()
                            .join(", ")
                    )
                }
            }
            Self::FfiType(x) => write!(f, "{}", x),
            Self::List(x) => write!(f, "list:<{}>", x),
            Self::Tuple(types) => {
                write!(
                    f,
                    "<{}>",
                    types
                        .iter()
                        .map(|x| x.to_string())
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            }
            Self::Scope(values) => {
                write!(
                    f,
                    "{}",
                    values
                        .iter()
                        .map(|x| x.to_string())
                        .collect::<Vec<_>>()
                        .join("::")
                )
            }
            Self::NativeFunction {
                return_type,
                parameters,
            }
            | Self::Function {
                return_type,
                parameters,
            } => {
                let mut txt = format!(
                    "fn ({})",
                    parameters
                        .iter()
                        .map(|x| x.to_string())
                        .collect::<Vec<_>>()
                        .join(", ")
                );

                if return_type.data_type != ParserInnerType::Null {
                    txt.push_str(&format!(" -> {}", return_type));
                }

                write!(f, "{}", txt)
            }
        }
    }
}

// TODO Evaluate use of this
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct GenericTypes(pub Vec<GenericType>);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct GenericType {
    pub identifier: PotentialDollarIdentifier,
    pub trait_constraints: Vec<PotentialDollarIdentifier>,
}
