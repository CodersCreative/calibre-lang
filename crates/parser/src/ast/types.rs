use crate::{
    IdentifiersUsed, ParserError, Span,
    ast::{
        RefMutability,
        ffi::ParserFfiInnerType,
        idents::{ParserText, PotentialDollarIdentifier},
        nodes::{CallArg, Node, NodeType, Overload, TypeDefType},
    },
    qualified_name_base, qualified_name_tail,
};
use rustc_hash::FxHashMap;
use serde::{Deserialize, Serialize};
use std::{fmt::Display, ops::Deref, str::FromStr};

#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct ParserDataType {
    pub data_type: ParserInnerType,
    pub span: Span,
}

impl From<ParserInnerType> for ParserDataType {
    fn from(value: ParserInnerType) -> Self {
        Self {
            data_type: value,
            span: Span::default(),
        }
    }
}

impl ParserDataType {
    pub fn new(span: Span, data_type: ParserInnerType) -> Self {
        Self { data_type, span }
    }

    pub fn key(&self) -> ParserInnerType {
        self.clone().unwrap_all_refs().data_type
    }

    pub fn member_base_name_candidates(&self) -> Vec<String> {
        let mut names = Vec::new();
        let base = self.key();
        let base_key = base.to_string();
        names.push(base_key.clone());

        match &base {
            ParserInnerType::Struct(name) => {
                let de_prefixed = qualified_name_tail(name);
                if de_prefixed != name {
                    names.push(de_prefixed.to_string());
                }
                let short = name.rsplit_once("::").map(|(_, rhs)| rhs).unwrap_or(name);
                if short != name {
                    names.push(short.to_string());
                }
            }
            ParserInnerType::StructWithGenerics { identifier, .. } => {
                names.push(identifier.clone());
                let de_prefixed = qualified_name_tail(identifier);
                if de_prefixed != identifier {
                    names.push(de_prefixed.to_string());
                }
                let short = identifier
                    .rsplit_once("::")
                    .map(|(_, rhs)| rhs)
                    .unwrap_or(identifier);
                if short != identifier {
                    names.push(short.to_string());
                }
            }
            _ => {
                let short = base_key
                    .rsplit_once("::")
                    .map(|(_, rhs)| rhs)
                    .unwrap_or(base_key.as_str());
                if short != base_key {
                    names.push(short.to_string());
                }
            }
        }

        names
    }

    fn canonical_key(&self) -> String {
        match &self.data_type {
            ParserInnerType::Struct(s) => format!("struct_{}", s),
            ParserInnerType::List(x) => format!("list_{}", x.canonical_key()),
            ParserInnerType::Ptr(x) => format!("ptr_{}", x.canonical_key()),
            ParserInnerType::Option(x) => format!("opt_{}", x.canonical_key()),
            ParserInnerType::Result { ok, err } => {
                format!("res_{}_{}", err.canonical_key(), ok.canonical_key())
            }
            ParserInnerType::Tuple(xs) => {
                let inner = xs
                    .iter()
                    .map(Self::canonical_key)
                    .collect::<Vec<_>>()
                    .join("_");
                format!("tup_{}", inner)
            }
            ParserInnerType::Ref(x, m) => format!("ref{}_{}", m, x.canonical_key()),
            ParserInnerType::StructWithGenerics {
                identifier,
                generic_types,
            } => {
                let inner = generic_types
                    .iter()
                    .map(Self::canonical_key)
                    .collect::<Vec<_>>()
                    .join("_");
                format!("gen_{}_{}", identifier, inner)
            }
            ParserInnerType::Function {
                return_type,
                parameters,
            } => {
                let params = parameters
                    .iter()
                    .map(Self::canonical_key)
                    .collect::<Vec<_>>()
                    .join("_");
                format!("fn_{}_ret_{}", params, return_type.canonical_key())
            }
            ParserInnerType::Auto(_)
            | ParserInnerType::DollarIdentifier(_)
            | ParserInnerType::Scope(_)
            | ParserInnerType::NativeFunction(_) => {
                format!("other_{}", self.impl_name())
            }
            _ => self.impl_name(),
        }
    }

    pub fn canonical_args_key(args: &[ParserDataType]) -> String {
        args.iter()
            .map(Self::canonical_key)
            .collect::<Vec<_>>()
            .join("__")
    }

    pub fn impl_name(&self) -> String {
        match self.key() {
            ParserInnerType::Struct(name) => name,
            ParserInnerType::StructWithGenerics { identifier, .. } => identifier,
            ParserInnerType::Int => String::from("int"),
            ParserInnerType::UInt => String::from("uint"),
            ParserInnerType::Byte => String::from("byte"),
            ParserInnerType::Float => String::from("float"),
            ParserInnerType::Bool => String::from("bool"),
            ParserInnerType::Char => String::from("char"),
            ParserInnerType::Str => String::from("str"),
            ParserInnerType::Range => String::from("range"),
            other => other.to_string(),
        }
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
            ParserInnerType::Ref(x, m) => {
                ParserInnerType::Ref(Box::new(x.substitute(subst)), m.clone())
            }
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
    Null,
    Bool,
    Str,
    Char,
    Dynamic,
    DynamicTraits(Vec<String>),
    Tuple(Vec<ParserDataType>),
    List(Box<ParserDataType>),
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
    NativeFunction(Box<ParserDataType>),
    Ptr(Box<ParserDataType>),
}

impl ParserDataType {
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

    pub fn default_node(&self) -> Option<Node> {
        match &self.data_type {
            ParserInnerType::Int => Some(Node::int(self.span, 0)),
            ParserInnerType::UInt => Some(Node::new(
                self.span,
                NodeType::IntLiteral(String::from("0u")),
            )),
            ParserInnerType::Byte => Some(Node::new(
                self.span,
                NodeType::IntLiteral(String::from("0b")),
            )),
            ParserInnerType::Str => Some(Node::new(
                self.span,
                NodeType::StringLiteral(ParserText::new(self.span, "")),
            )),
            ParserInnerType::Char => Some(Node::new(self.span, NodeType::CharLiteral('\0'))),
            ParserInnerType::Float => Some(Node::new(self.span, NodeType::FloatLiteral(0.0))),
            ParserInnerType::Auto(_) => Some(Node::new(self.span, NodeType::Null)),
            ParserInnerType::Dynamic => Some(Node::new(self.span, NodeType::Null)),
            ParserInnerType::Null => Some(Node::new(self.span, NodeType::Null)),
            ParserInnerType::List(t) => Some(Node::new(
                self.span,
                NodeType::ListLiteral((*t.clone()).into(), Vec::new()),
            )),
            ParserInnerType::Range => Some(Node::new(
                self.span,
                NodeType::RangeDeclaration {
                    from: Box::new(Node::int(self.span, 0)),
                    to: Box::new(Node::int(self.span, 0)),
                    inclusive: true,
                },
            )),
            ParserInnerType::Bool => Some(Node::bool(self.span, false)),
            ParserInnerType::Tuple(values) => Some(Node::new(
                self.span,
                NodeType::TupleLiteral {
                    values: values.iter().filter_map(|x| x.default_node()).collect(),
                },
            )),
            ParserInnerType::Option(_) => Some(Node::none(self.span)),
            ParserInnerType::Result { ok, .. } => Some(Node::call(
                self.span,
                Node::identifier(self.span, "ok"),
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
            "uint" => Self::UInt,
            "byte" => Self::Byte,
            "float" => Self::Float,
            "bool" => Self::Bool,
            "str" => Self::Str,
            "char" => Self::Char,
            "dyn" => Self::Dynamic,
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
            ParserInnerType::Result { ok, err: _ } => Some(&ok),
            _ => None,
        }
    }

    pub fn is_callable(&self) -> bool {
        matches!(
            self.unwrap_all_refs(),
            ParserInnerType::Function { .. } | ParserInnerType::NativeFunction(_)
        )
    }

    pub fn is_auto(&self) -> bool {
        match self {
            Self::Auto(_) => true,
            _ => false,
        }
    }

    pub fn is_result(&self) -> bool {
        match self {
            Self::Result { .. } => true,
            _ => false,
        }
    }

    pub fn is_bool(&self) -> bool {
        match self {
            Self::Bool => true,
            _ => false,
        }
    }

    pub fn is_null(&self) -> bool {
        match self {
            Self::Null => true,
            _ => false,
        }
    }

    pub fn is_list(&self) -> bool {
        match self {
            Self::List(_) => true,
            Self::Ref(x, _) => x.is_list(),
            _ => false,
        }
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
    pub fn apply_callable(
        self,
        args_len: usize,
        implicit_params: usize,
        span: Span,
    ) -> Option<ParserDataType> {
        match self {
            ParserInnerType::Function {
                return_type,
                parameters,
            } if parameters.len() > args_len + implicit_params => Some(ParserDataType {
                data_type: ParserInnerType::Function {
                    return_type,
                    parameters: parameters
                        .into_iter()
                        .skip(args_len + implicit_params)
                        .collect(),
                },
                span,
            }),
            ParserInnerType::Function { return_type, .. } => Some(*return_type),
            ParserInnerType::NativeFunction(ret) => Some(*ret),
            _ => None,
        }
    }

    pub fn matches(&self, other: &Self, generic_params: &[String]) -> bool {
        fn struct_base(name: &str) -> &str {
            let short = name.rsplit_once("::").map(|(lhs, _)| lhs).unwrap_or(name);
            qualified_name_base(short)
        }

        match (self, other) {
            (ParserInnerType::Struct(s), target)
                if ParserInnerType::from_str(struct_base(s)).as_ref() == Ok(target) =>
            {
                true
            }
            (ParserInnerType::Struct(a), _) if generic_params.contains(a) => true,
            (ParserInnerType::Struct(a), ParserInnerType::Struct(b))
                if b == a
                    || b.starts_with(&format!("{}->", a))
                    || struct_base(a) == struct_base(b) =>
            {
                true
            }
            (
                ParserInnerType::StructWithGenerics { identifier: a, .. },
                ParserInnerType::Struct(b),
            ) if b == a || b.starts_with(&format!("{}->", a)) || struct_base(b) == a => true,
            (
                ParserInnerType::Struct(a),
                ParserInnerType::StructWithGenerics { identifier: b, .. },
            ) => a == b || struct_base(a) == b,
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
                if struct_base(a) != struct_base(b) || ag.len() != bg.len() {
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

#[derive(Debug, Clone, PartialEq)]
pub enum PotentialNewType {
    NewType {
        identifier: PotentialDollarIdentifier,
        type_def: TypeDefType,
        overloads: Vec<Overload>,
    },
    DataType(ParserDataType),
}

impl PotentialNewType {
    pub fn unwrap_or_auto(self) -> ParserDataType {
        match self {
            PotentialNewType::DataType(data_type) => data_type,
            _ => ParserDataType::new(*self.span(), ParserInnerType::Auto(None)),
        }
    }

    pub fn auto(sp: Span) -> Self {
        ParserDataType::new(sp, ParserInnerType::Auto(None)).into()
    }

    pub fn null(sp: Span) -> Self {
        ParserDataType::new(sp, ParserInnerType::Null).into()
    }

    pub fn is_auto(&self) -> bool {
        match self {
            Self::DataType(x) => x.is_auto(),
            _ => false,
        }
    }

    pub fn is_result(&self) -> bool {
        match self {
            Self::DataType(x) => x.is_result(),
            _ => false,
        }
    }

    pub fn is_bool(&self) -> bool {
        match self {
            Self::DataType(x) => x.is_bool(),
            _ => false,
        }
    }

    pub fn is_null(&self) -> bool {
        match self {
            Self::DataType(x) => x.is_null(),
            _ => false,
        }
    }

    pub fn substitute(&self, subst: &FxHashMap<String, ParserDataType>) -> PotentialNewType {
        match self {
            PotentialNewType::DataType(dt) => PotentialNewType::DataType(dt.substitute(subst)),
            _ => self.clone(),
        }
    }
}

impl IdentifiersUsed for PotentialNewType {
    fn identifiers_used(&self) -> Vec<&String> {
        let mut names = Vec::new();
        match self {
            PotentialNewType::NewType { type_def, .. } => {
                names.extend(type_def.identifiers_used());
            }
            PotentialNewType::DataType(data_type) => {
                names.extend(data_type.identifiers_used());
            }
        }
        names
    }
}

impl Display for PotentialNewType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::DataType(x) => write!(f, "{}", x),
            Self::NewType { identifier, .. } => write!(f, "type {}", identifier),
        }
    }
}

impl PotentialNewType {
    pub fn span(&self) -> &Span {
        match self {
            Self::NewType { identifier, .. } => identifier.span(),
            Self::DataType(x) => &x.span,
        }
    }
}

impl From<ParserDataType> for PotentialNewType {
    fn from(value: ParserDataType) -> Self {
        Self::DataType(value)
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
            Self::UInt => write!(f, "uint"),
            Self::Byte => write!(f, "byte"),
            Self::Null => write!(f, "null"),
            Self::Dynamic => write!(f, "dyn"),
            Self::DynamicTraits(traits) => {
                if traits.is_empty() {
                    write!(f, "dyn")
                } else {
                    let mut txt = String::new();
                    for (i, tr) in traits.iter().enumerate() {
                        if i > 0 {
                            txt.push_str(", ");
                        }
                        txt.push_str(tr);
                    }
                    write!(f, "dyn:<{}>", txt)
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
            Self::NativeFunction(x) => write!(f, "native -> {}", x),
            Self::Ptr(x) => write!(f, "ptr:<{}>", x),
            Self::Struct(x) => write!(f, "{}", x),
            Self::StructWithGenerics {
                identifier,
                generic_types,
            } => {
                if generic_types.is_empty() {
                    write!(f, "{}", identifier)
                } else {
                    let mut txt = format!(
                        "{}<{}",
                        identifier,
                        generic_types
                            .first()
                            .map(|x| x.to_string())
                            .unwrap_or(String::new())
                    );

                    for typ in generic_types.iter().skip(1) {
                        txt.push_str(&format!(", {}", typ));
                    }

                    txt.push_str(">");

                    write!(f, "{}", txt)
                }
            }
            Self::FfiType(x) => write!(f, "@{}", x),
            Self::List(x) => write!(f, "list:<{}>", x),
            Self::Tuple(types) => {
                let mut txt = format!(
                    "<{}",
                    types.get(0).map(|x| x.to_string()).unwrap_or(String::new())
                );
                for typ in types.iter().skip(1) {
                    txt.push_str(&format!(", {}", typ));
                }

                txt.push_str(">");

                write!(f, "{}", txt)
            }
            Self::Scope(values) => {
                let mut txt = values[0].to_string();

                for typ in values.iter().skip(1) {
                    txt.push_str(&format!("::{}", typ));
                }

                write!(f, "{}", txt)
            }
            Self::Function {
                return_type,
                parameters,
            } => {
                let mut txt = String::from("fn (");

                txt.push_str(
                    &parameters
                        .get(0)
                        .map(|x| x.to_string())
                        .unwrap_or(String::new()),
                );

                for typ in parameters.iter().skip(1) {
                    txt.push_str(&format!(", {}", typ));
                }

                txt.push_str(")");

                if return_type.data_type != ParserInnerType::Null {
                    txt.push_str(&format!(" -> {}", return_type));
                }

                write!(f, "{}", txt)
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct GenericTypes(pub Vec<GenericType>);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct GenericType {
    pub identifier: PotentialDollarIdentifier,
    pub trait_constraints: Vec<PotentialDollarIdentifier>,
}
