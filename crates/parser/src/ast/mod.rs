pub mod binary;
pub mod comparison;
pub mod formatter;
use crate::{
    Span,
    ast::{comparison::BooleanOperator, formatter::Formatter},
};
use binary::BinaryOperator;
use comparison::ComparisonOperator;
use rustc_hash::FxHashMap;
use serde::{Deserialize, Serialize};
use std::{
    cmp::Ordering,
    fmt::{Debug, Display},
    ops::{Deref, DerefMut},
    str::FromStr,
    string::ParseError,
};

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub enum RefMutability {
    Value,
    Ref,
    MutRef,
    MutValue,
}

impl RefMutability {
    pub fn fmt_with_val(&self, val: &str) -> String {
        match self {
            Self::MutRef | Self::MutValue => {
                format!("{} {}", self, val)
            }
            _ => format!("{}{}", self, val),
        }
    }
}

impl Display for RefMutability {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Value => write!(f, ""),
            Self::Ref => write!(f, "&"),
            Self::MutRef => write!(f, "&mut"),
            Self::MutValue => write!(f, "mut"),
        }
    }
}

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub enum AsFailureMode {
    Result,
    Panic,
    Option,
}

#[derive(Debug, Clone, PartialEq)]
pub enum LoopType {
    Let {
        value: Node,
        pattern: (Vec<MatchArmType>, Vec<Node>),
    },
    While(Node),
    For(PotentialDollarIdentifier, Node),
    Loop,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ParserFfiDataType {
    pub data_type: ParserFfiInnerType,
    pub span: Span,
}

impl Display for ParserFfiDataType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.data_type)
    }
}

impl From<ParserFfiInnerType> for ParserFfiDataType {
    fn from(value: ParserFfiInnerType) -> Self {
        Self {
            data_type: value,
            span: Span::default(),
        }
    }
}

impl ParserFfiDataType {
    pub fn new(span: Span, data_type: ParserFfiInnerType) -> Self {
        Self { data_type, span }
    }
}

impl Deref for ParserFfiDataType {
    type Target = ParserFfiInnerType;
    fn deref(&self) -> &Self::Target {
        &self.data_type
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum ParserFfiInnerType {
    F32,
    F64,
    LongDouble,
    U8,
    I8,
    U16,
    I16,
    U32,
    I32,
    U64,
    I64,
    USize,
    ISize,
    SChar,
    UChar,
    Int,
    UInt,
    Short,
    UShort,
    Long,
    ULong,
    LongLong,
    ULongLong,
}

impl Into<ParserInnerType> for ParserFfiInnerType {
    fn into(self) -> ParserInnerType {
        match self {
            Self::F32 | Self::F64 | Self::LongDouble => ParserInnerType::Float,
            Self::SChar | Self::UChar => ParserInnerType::Char,
            Self::U16
            | Self::U8
            | Self::U32
            | Self::U64
            | Self::USize
            | Self::UInt
            | Self::UShort
            | Self::ULong
            | Self::ULongLong => ParserInnerType::UInt,
            _ => ParserInnerType::Int,
        }
    }
}

impl Display for ParserFfiInnerType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "@{}",
            match self {
                Self::U8 => "u8",
                Self::I8 => "i8",
                Self::U16 => "u16",
                Self::I16 => "i16",
                Self::U32 => "u32",
                Self::I32 => "i32",
                Self::U64 => "u64",
                Self::I64 => "i64",
                Self::USize => "usize",
                Self::ISize => "isize",
                Self::UInt => "uint",
                Self::Int => "int",
                Self::UShort => "ushort",
                Self::Short => "short",
                Self::ULong => "ulong",
                Self::Long => "long",
                Self::ULongLong => "ulonglong",
                Self::LongLong => "longlong",
                Self::LongDouble => "longdouble",
                Self::F32 => "f32",
                Self::F64 => "f64",
                Self::SChar => "schar",
                Self::UChar => "uchar",
            }
        )
    }
}

impl FromStr for ParserFfiInnerType {
    type Err = ();
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(match s.trim_start_matches("@") {
            "u8" => Self::U8,
            "i8" => Self::I8,
            "u16" => Self::U16,
            "i16" => Self::I16,
            "u32" => Self::U32,
            "i32" => Self::I32,
            "u64" => Self::U64,
            "i64" => Self::I64,
            "usize" => Self::USize,
            "isize" => Self::ISize,
            "uint" => Self::UInt,
            "int" => Self::Int,
            "ushort" => Self::UShort,
            "short" => Self::Short,
            "ulong" => Self::ULong,
            "long" => Self::Long,
            "ulonglong" => Self::ULongLong,
            "longlong" => Self::LongLong,
            "f32" => Self::F32,
            "f64" => Self::F64,
            "schar" => Self::SChar,
            "uchar" => Self::UChar,
            _ => return Err(()),
        })
    }
}

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
    Int,
    Null,
    Bool,
    Str,
    Char,
    Dynamic,
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
            data_type: self.data_type.unwrap_all_refs(),
            span: self.span,
        }
    }

    pub fn contains_auto(&self) -> bool {
        self.data_type.contains_auto()
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

impl From<ParserFfiDataType> for ParserDataType {
    fn from(value: ParserFfiDataType) -> Self {
        Self {
            span: value.span,
            data_type: value.data_type.into(),
        }
    }
}

impl ParserInnerType {
    pub fn unwrap_all_refs(self) -> Self {
        match self {
            Self::Ref(x, _) => x.data_type.unwrap_all_refs(),
            _ => self,
        }
    }

    pub fn is_auto(&self) -> bool {
        match self {
            Self::Auto(_) => true,
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
            x => x,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum PotentialNewTypeFfiType {
    DataType(PotentialNewType),
    Ffi(ParserFfiDataType),
}

impl PotentialNewTypeFfiType {
    pub fn is_auto(&self) -> bool {
        match self {
            Self::DataType(x) => x.is_auto(),
            _ => false,
        }
    }
}

impl Display for PotentialNewTypeFfiType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::DataType(x) => write!(f, "{}", x),
            Self::Ffi(x) => write!(f, "@{}", x),
        }
    }
}

impl PotentialNewTypeFfiType {
    pub fn span(&self) -> &Span {
        match self {
            Self::Ffi(x) => &x.span,
            Self::DataType(x) => x.span(),
        }
    }
}

impl From<ParserDataType> for PotentialNewTypeFfiType {
    fn from(value: ParserDataType) -> Self {
        Self::DataType(value.into())
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
    pub fn is_auto(&self) -> bool {
        match self {
            Self::DataType(x) => x.is_auto(),
            _ => false,
        }
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
            Self::Null => write!(f, "null"),
            Self::Dynamic => write!(f, "dyn"),
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
                            .get(0)
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

#[derive(Debug, Clone, PartialEq)]
pub enum TraitMemberKind {
    Const,
    Type,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TraitMember {
    pub kind: TraitMemberKind,
    pub identifier: PotentialDollarIdentifier,
    pub data_type: PotentialNewType,
    pub value: Option<Box<Node>>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum PotentialGenericTypeIdentifier {
    Identifier(PotentialDollarIdentifier),
    Generic {
        identifier: PotentialDollarIdentifier,
        generic_types: Vec<PotentialNewType>,
    },
}

impl Display for PotentialGenericTypeIdentifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Identifier(x) => write!(f, "{}", x),
            Self::Generic {
                identifier,
                generic_types,
            } => {
                if generic_types.is_empty() {
                    return write!(f, "{}", identifier);
                }
                let mut txt = format!(
                    "{}:<{}",
                    identifier,
                    generic_types
                        .get(0)
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
    }
}

impl PotentialGenericTypeIdentifier {
    pub fn get_ident(&self) -> &PotentialDollarIdentifier {
        match self {
            Self::Identifier(x) => x,
            Self::Generic {
                identifier,
                generic_types: _,
            } => identifier,
        }
    }
}

impl Into<PotentialDollarIdentifier> for PotentialGenericTypeIdentifier {
    fn into(self) -> PotentialDollarIdentifier {
        match self {
            Self::Identifier(x) => x,
            Self::Generic {
                identifier,
                generic_types: _,
            } => identifier,
        }
    }
}

impl Into<Node> for PotentialGenericTypeIdentifier {
    fn into(self) -> Node {
        Node {
            span: *self.span(),
            node_type: NodeType::Identifier(self),
        }
    }
}

impl From<ParserText> for PotentialGenericTypeIdentifier {
    fn from(value: ParserText) -> Self {
        Self::Identifier(value.into())
    }
}

impl PotentialGenericTypeIdentifier {
    pub fn span(&self) -> &Span {
        match self {
            Self::Identifier(x) => x.span(),
            Self::Generic {
                identifier,
                generic_types: _,
            } => identifier.span(),
        }
    }
}

impl FromStr for ParserInnerType {
    type Err = ParseError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(match s {
            "int" => Self::Int,
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

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum ObjectType<T> {
    Map(Vec<(String, T)>),
    Tuple(Vec<T>),
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum DestructurePattern {
    Tuple(Vec<Option<(VarType, PotentialDollarIdentifier)>>),
    Struct(Vec<(String, VarType, PotentialDollarIdentifier)>),
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ObjectMap<T>(pub Vec<(String, T)>);

impl<T> ObjectMap<T> {
    pub fn get(&self, key: &str) -> Option<&T> {
        self.0.iter().find(|x| &x.0 == key).map(|x| &x.1)
    }

    pub fn remove(&mut self, key: &str) -> Option<T> {
        let index = self.0.iter().position(|x| &x.0 == key)?;
        Some(self.0.remove(index).1)
    }

    pub fn contains_key(&self, key: &str) -> bool {
        self.0.iter().find(|x| &x.0 == key).is_some()
    }
}

impl<T> From<FxHashMap<String, T>> for ObjectMap<T> {
    fn from(value: FxHashMap<String, T>) -> Self {
        Self(value.into_iter().collect())
    }
}

impl<T> From<Vec<(String, T)>> for ObjectMap<T> {
    fn from(value: Vec<(String, T)>) -> Self {
        Self(value)
    }
}

impl<T> From<Vec<T>> for ObjectMap<T> {
    fn from(value: Vec<T>) -> Self {
        Self(
            value
                .into_iter()
                .enumerate()
                .map(|x| (x.0.to_string(), x.1))
                .collect(),
        )
    }
}

impl<T> From<ObjectType<T>> for ObjectMap<T> {
    fn from(value: ObjectType<T>) -> Self {
        match value {
            ObjectType::Map(x) => Self(x),
            ObjectType::Tuple(x) => x.into(),
        }
    }
}

impl<T> Deref for ObjectMap<T> {
    type Target = Vec<(String, T)>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T> DerefMut for ObjectMap<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
pub enum VarType {
    Mutable,
    Immutable,
    Constant,
}

impl VarType {
    pub fn print_only_ends(&self) -> String {
        format!(
            "{}",
            match self {
                Self::Mutable => "mut",
                Self::Immutable => "let",
                Self::Constant => "const",
            }
        )
    }
}

impl Display for VarType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Mutable => write!(f, "let mut"),
            Self::Immutable => write!(f, "let"),
            Self::Constant => write!(f, "const"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypeDefType {
    Enum(Vec<(PotentialDollarIdentifier, Option<PotentialNewType>)>),
    Struct(ObjectType<PotentialNewType>),
    NewType(Box<PotentialNewType>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Node {
    pub node_type: NodeType,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
pub struct ParserText {
    pub text: String,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
pub enum PotentialDollarIdentifier {
    DollarIdentifier(ParserText),
    Identifier(ParserText),
}

impl Into<PotentialGenericTypeIdentifier> for PotentialDollarIdentifier {
    fn into(self) -> PotentialGenericTypeIdentifier {
        PotentialGenericTypeIdentifier::Identifier(self)
    }
}

impl Into<Node> for PotentialDollarIdentifier {
    fn into(self) -> Node {
        Node {
            span: *self.span(),
            node_type: NodeType::Identifier(PotentialGenericTypeIdentifier::Identifier(self)),
        }
    }
}

impl From<ParserText> for PotentialDollarIdentifier {
    fn from(value: ParserText) -> Self {
        Self::Identifier(value)
    }
}

impl PotentialDollarIdentifier {
    pub fn span(&self) -> &Span {
        match self {
            Self::Identifier(x) => &x.span,
            Self::DollarIdentifier(x) => &x.span,
        }
    }
}

impl Display for PotentialDollarIdentifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Identifier(x) => write!(f, "{}", x),
            Self::DollarIdentifier(x) => write!(f, "${}", x),
        }
    }
}

impl Deref for ParserText {
    type Target = String;
    fn deref(&self) -> &Self::Target {
        &self.text
    }
}

impl DerefMut for ParserText {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.text
    }
}

impl ParserText {
    pub fn new(span: Span, text: String) -> Self {
        Self { text, span }
    }
}

impl From<String> for ParserText {
    fn from(value: String) -> Self {
        Self::new(Span::default(), value)
    }
}

impl FromStr for ParserText {
    type Err = ParseError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(Self::from(s.to_string()))
    }
}

impl Display for ParserText {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.text)
    }
}

impl Node {
    pub fn new(span: Span, node_type: NodeType) -> Self {
        Self { node_type, span }
    }
}

#[repr(u8)]
#[derive(Clone, Debug, PartialEq)]
pub enum MatchTupleItem {
    Rest(Span),
    Wildcard(Span),
    Value(Node),
    Enum {
        value: PotentialDollarIdentifier,
        var_type: VarType,
        name: Option<PotentialDollarIdentifier>,
        destructure: Option<DestructurePattern>,
        pattern: Option<Box<MatchArmType>>,
    },
    Binding {
        var_type: VarType,
        name: PotentialDollarIdentifier,
    },
}

#[derive(Clone, Debug, PartialEq)]
pub enum MatchStructFieldPattern {
    Value {
        field: String,
        value: Node,
    },
    Binding {
        field: String,
        var_type: VarType,
        name: PotentialDollarIdentifier,
    },
}

#[repr(u8)]
#[derive(Clone, Debug, PartialEq)]
pub enum MatchArmType {
    Enum {
        value: PotentialDollarIdentifier,
        var_type: VarType,
        name: Option<PotentialDollarIdentifier>,
        destructure: Option<DestructurePattern>,
        pattern: Option<Box<MatchArmType>>,
    },
    TuplePattern(Vec<MatchTupleItem>),
    StructPattern(Vec<MatchStructFieldPattern>),
    Let {
        var_type: VarType,
        name: PotentialDollarIdentifier,
    },
    Value(Node),
    Wildcard(Span),
}

impl MatchArmType {
    pub fn span(&self) -> &Span {
        match self {
            Self::Enum { value, .. } => value.span(),
            Self::TuplePattern(items) => {
                for item in items {
                    match item {
                        MatchTupleItem::Rest(sp) | MatchTupleItem::Wildcard(sp) => return sp,
                        MatchTupleItem::Value(node) => return &node.span,
                        MatchTupleItem::Enum { value, .. } => return value.span(),
                        MatchTupleItem::Binding { name, .. } => return name.span(),
                    }
                }
                static DEFAULT: Span = Span {
                    from: crate::Position { line: 0, col: 0 },
                    to: crate::Position { line: 0, col: 0 },
                };
                &DEFAULT
            }
            Self::StructPattern(fields) => {
                for field in fields {
                    match field {
                        MatchStructFieldPattern::Value { value, .. } => return &value.span,
                        MatchStructFieldPattern::Binding { name, .. } => return name.span(),
                    }
                }
                static DEFAULT: Span = Span {
                    from: crate::Position { line: 0, col: 0 },
                    to: crate::Position { line: 0, col: 0 },
                };
                &DEFAULT
            }
            Self::Let { var_type: _, name } => name.span(),
            Self::Value(x) => &x.span,
            Self::Wildcard(x) => &x,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct TryCatch {
    pub name: Option<PotentialDollarIdentifier>,
    pub body: Box<Node>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct NamedScope {
    pub name: PotentialDollarIdentifier,
    pub args: Vec<(PotentialDollarIdentifier, Node)>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Overload {
    pub operator: ParserText,
    pub body: Box<Node>,
    pub header: FunctionHeader,
}

impl Into<Node> for Overload {
    fn into(self) -> Node {
        Node::new(
            self.operator.span,
            NodeType::FunctionDeclaration {
                header: self.header,
                body: self.body,
            },
        )
    }
}

impl Overload {
    pub fn span(&self) -> &Span {
        &self.operator.span
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct FunctionHeader {
    pub generics: GenericTypes,
    pub parameters: Vec<(PotentialDollarIdentifier, PotentialNewType)>,
    pub return_type: PotentialNewType,
    pub param_destructures: Vec<(usize, DestructurePattern)>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum CallArg {
    Value(Node),
    Named(PotentialDollarIdentifier, Node),
}

impl Into<Node> for CallArg {
    fn into(self) -> Node {
        match self {
            Self::Value(x) => x,
            Self::Named(_, x) => x,
        }
    }
}

#[repr(u8)]
#[derive(Clone, Debug, PartialEq)]
pub enum NodeType {
    Break {
        label: Option<PotentialDollarIdentifier>,
        value: Option<Box<Node>>,
    },
    Continue {
        label: Option<PotentialDollarIdentifier>,
    },
    EmptyLine,
    Null,
    Spawn {
        items: Vec<Node>,
    },
    Use {
        identifiers: Vec<PotentialDollarIdentifier>,
        value: Box<Node>,
    },
    SelectStatement {
        arms: Vec<SelectArm>,
    },
    RefStatement {
        mutability: RefMutability,
        value: Box<Node>,
    },
    Identifier(PotentialGenericTypeIdentifier),
    DataType {
        data_type: PotentialNewType,
    },
    DerefStatement {
        value: Box<Node>,
    },
    Drop(PotentialDollarIdentifier),
    MoveExpression {
        value: Box<Node>,
    },
    Defer {
        value: Box<Node>,
        function: bool,
    },
    ParenExpression {
        value: Box<Node>,
    },
    VariableDeclaration {
        var_type: VarType,
        identifier: PotentialDollarIdentifier,
        value: Box<Node>,
        data_type: PotentialNewType,
    },
    ImplDeclaration {
        generics: GenericTypes,
        target: PotentialNewType,
        variables: Vec<Node>,
    },
    ImplTraitDeclaration {
        generics: GenericTypes,
        trait_ident: PotentialGenericTypeIdentifier,
        target: PotentialNewType,
        variables: Vec<Node>,
    },
    TraitDeclaration {
        identifier: PotentialGenericTypeIdentifier,
        implied_traits: Vec<PotentialDollarIdentifier>,
        members: Vec<TraitMember>,
    },
    TypeDeclaration {
        identifier: PotentialGenericTypeIdentifier,
        object: TypeDefType,
        overloads: Vec<Overload>,
    },
    EnumExpression {
        identifier: PotentialGenericTypeIdentifier,
        value: PotentialDollarIdentifier,
        data: Option<Box<Node>>,
    },
    TupleLiteral {
        values: Vec<Node>,
    },
    ScopeAlias {
        identifier: PotentialDollarIdentifier,
        value: NamedScope,
        create_new_scope: Option<bool>,
    },
    ScopeDeclaration {
        body: Option<Vec<Node>>,
        named: Option<NamedScope>,
        is_temp: bool,
        create_new_scope: Option<bool>,
        define: bool,
    },
    MatchStatement {
        value: Option<Box<Node>>,
        body: Vec<(MatchArmType, Vec<Node>, Box<Node>)>,
    },
    FnMatchDeclaration {
        header: FunctionHeader,
        body: Vec<(MatchArmType, Vec<Node>, Box<Node>)>,
    },
    FunctionDeclaration {
        header: FunctionHeader,
        body: Box<Node>,
    },
    ExternFunctionDeclaration {
        abi: String,
        identifier: PotentialDollarIdentifier,
        parameters: Vec<ParserDataType>,
        return_type: ParserDataType,
        library: String,
        symbol: Option<String>,
    },
    AssignmentExpression {
        identifier: Box<Node>,
        value: Box<Node>,
    },
    DestructureDeclaration {
        var_type: VarType,
        pattern: DestructurePattern,
        value: Box<Node>,
    },
    DestructureAssignment {
        pattern: DestructurePattern,
        value: Box<Node>,
    },
    NotExpression {
        value: Box<Node>,
    },
    NegExpression {
        value: Box<Node>,
    },
    DebugExpression {
        value: Box<Node>,
    },
    AsExpression {
        value: Box<Node>,
        data_type: PotentialNewType,
        failure_mode: AsFailureMode,
    },
    InDeclaration {
        identifier: Box<Node>,
        value: Box<Node>,
    },
    RangeDeclaration {
        from: Box<Node>,
        to: Box<Node>,
        inclusive: bool,
    },
    IterExpression {
        data_type: PotentialNewType,
        map: Box<Node>,
        spawned: bool,
        loop_type: Box<LoopType>,
        conditionals: Vec<Node>,
        until: Option<Box<Node>>,
    },
    InlineGenerator {
        map: Box<Node>,
        data_type: Option<PotentialNewType>,
        loop_type: Box<LoopType>,
        conditionals: Vec<Node>,
        until: Option<Box<Node>>,
    },
    LoopDeclaration {
        loop_type: Box<LoopType>,
        body: Box<Node>,
        until: Option<Box<Node>>,
        label: Option<PotentialDollarIdentifier>,
        else_body: Option<Box<Node>>,
    },
    TestDeclaration {
        identifier: PotentialDollarIdentifier,
        body: Box<Node>,
    },
    BenchDeclaration {
        identifier: PotentialDollarIdentifier,
        body: Box<Node>,
    },
    Try {
        value: Box<Node>,
        catch: Option<TryCatch>,
    },
    Return {
        value: Option<Box<Node>>,
    },
    Until {
        condition: Box<Node>,
    },
    StringLiteral(ParserText),
    ListLiteral(PotentialNewType, Vec<Node>),
    CharLiteral(char),
    FloatLiteral(f64),
    IntLiteral(String),
    MemberExpression {
        path: Vec<(Node, bool)>,
    },
    ScopeMemberExpression {
        path: Vec<Node>,
    },
    CallExpression {
        string_fn: Option<ParserText>,
        caller: Box<Node>,
        generic_types: Vec<PotentialNewType>,
        args: Vec<CallArg>,
        reverse_args: Vec<Node>,
    },
    BinaryExpression {
        left: Box<Node>,
        right: Box<Node>,
        operator: BinaryOperator,
    },
    ComparisonExpression {
        left: Box<Node>,
        right: Box<Node>,
        operator: ComparisonOperator,
    },
    PipeExpression(Vec<PipeSegment>),
    BooleanExpression {
        left: Box<Node>,
        right: Box<Node>,
        operator: BooleanOperator,
    },
    IfStatement {
        comparison: Box<IfComparisonType>,
        then: Box<Node>,
        otherwise: Option<Box<Node>>,
    },
    Ternary {
        comparison: Box<Node>,
        then: Box<Node>,
        otherwise: Box<Node>,
    },
    ImportStatement {
        module: Vec<PotentialDollarIdentifier>,
        alias: Option<PotentialDollarIdentifier>,
        values: Vec<PotentialDollarIdentifier>,
    },
    StructLiteral {
        identifier: PotentialGenericTypeIdentifier,
        value: ObjectType<Node>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum SelectArmKind {
    Recv,
    Send,
    Default,
}

#[derive(Debug, Clone, PartialEq)]
pub struct SelectArm {
    pub patterns: Vec<(SelectArmKind, Option<Node>, Option<Node>)>,
    pub conditionals: Vec<Node>,
    pub body: Node,
}

#[derive(Clone, Debug, PartialEq)]
pub enum PipeSegment {
    Unnamed(Node),
    Named {
        identifier: PotentialDollarIdentifier,
        node: Node,
    },
}

impl PipeSegment {
    pub fn is_named(&self) -> bool {
        match self {
            Self::Unnamed(_) => false,
            _ => true,
        }
    }
    pub fn span(&self) -> &Span {
        match self {
            Self::Unnamed(x) => &x.span,
            Self::Named {
                identifier: _,
                node,
            } => &node.span,
        }
    }

    pub fn get_node(&self) -> &Node {
        match self {
            Self::Unnamed(x) => x,
            Self::Named {
                identifier: _,
                node,
            } => node,
        }
    }
}

impl Into<Node> for PipeSegment {
    fn into(self) -> Node {
        match self {
            Self::Unnamed(x) => x,
            Self::Named {
                identifier: _,
                node,
            } => node,
        }
    }
}

impl NodeType {
    pub fn unwrap(self) -> NodeType {
        self
    }

    pub fn is_call(&self) -> bool {
        match self {
            Self::CallExpression { .. } => true,
            Self::RefStatement { value, .. } | Self::DerefStatement { value } => {
                value.node_type.is_call()
            }

            _ => false,
        }
    }
}

impl Display for NodeType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut formatter = Formatter::default();
        let fake_node = Node {
            node_type: self.clone(),
            span: Span::default(),
        };
        write!(f, "{}", formatter.format(&fake_node))
    }
}

impl Display for Node {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut formatter = Formatter::default();
        write!(f, "{}", formatter.format(&self))
    }
}

impl Display for LoopType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut formatter = Formatter::default();
        write!(f, "{}", formatter.fmt_loop_type(&self))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum IfComparisonType {
    IfLet {
        value: Node,
        pattern: (Vec<MatchArmType>, Vec<Node>),
    },
    If(Node),
}

impl<T: PartialEq> PartialOrd for ObjectType<T> {
    fn gt(&self, _other: &Self) -> bool {
        false
    }

    fn lt(&self, _other: &Self) -> bool {
        false
    }

    fn ge(&self, _other: &Self) -> bool {
        true
    }

    fn le(&self, _other: &Self) -> bool {
        true
    }

    fn partial_cmp(&self, _other: &Self) -> Option<std::cmp::Ordering> {
        Some(Ordering::Equal)
    }
}

impl<T: PartialEq + ToString> ToString for ObjectType<T> {
    fn to_string(&self) -> String {
        match self {
            ObjectType::Map(x) => {
                let mut txt = String::from("{");
                for (k, v) in x {
                    txt.push_str(&format!("{k} : {}, ", v.to_string()));
                }

                let mut txt = txt.trim_end().trim_end_matches(",").to_string();
                txt.push_str("}");

                txt
            }
            ObjectType::Tuple(data) => {
                let lst: Vec<&T> = data.iter().collect();
                print_list(&lst, '(', ')')
            }
        }
    }
}

impl<T: PartialEq> PartialOrd for ObjectMap<T> {
    fn gt(&self, _other: &Self) -> bool {
        false
    }

    fn lt(&self, _other: &Self) -> bool {
        false
    }

    fn ge(&self, _other: &Self) -> bool {
        true
    }

    fn le(&self, _other: &Self) -> bool {
        true
    }

    fn partial_cmp(&self, _other: &Self) -> Option<std::cmp::Ordering> {
        Some(Ordering::Equal)
    }
}

impl<T: PartialEq + ToString> ToString for ObjectMap<T> {
    fn to_string(&self) -> String {
        if !self.0.is_empty() {
            if self.get("0").is_some() {
                let lst: Vec<&T> = self.0.iter().map(|x| &x.1).collect();
                return print_list(&lst, '(', ')');
            }
        }
        let mut txt = String::from("{");
        for (k, v) in self.0.iter() {
            txt.push_str(&format!("{k} : {}, ", v.to_string()));
        }

        let mut txt = txt.trim_end().trim_end_matches(",").to_string();
        txt.push_str("}");

        txt
    }
}

fn print_list<T: ToString>(data: &[&T], open: char, close: char) -> String {
    let mut txt = String::from(open);

    for val in data.iter() {
        txt.push_str(&format!("{}, ", val.to_string()));
    }

    let mut txt = txt.trim_end().trim_end_matches(",").to_string();
    txt.push(close);

    txt
}
