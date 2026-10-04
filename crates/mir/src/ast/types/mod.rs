use crate::symbols::TypeKey;
use crate::{MirRenamable, MirRenameState, MirTypeKeysUsed, scoping::FullyQualifiedPath};
use calibre_parser::{
    Span,
    ast::{
        RefMutability,
        ffi::ParserFfiInnerType,
        idents::ParserText,
        nodes::{
            AstNode, AstNodeType,
            functions::CallArg,
            lists::AstList,
            literals::{AstChar, AstRange, AstString, AstTuple},
        },
        types::{ParserDataType, ParserInnerType},
    },
};
use rustc_hash::FxHashMap;
use serde::{Deserialize, Serialize};
use std::{fmt::Display, sync::Arc};
use ustr::Ustr;

pub mod unify;

impl MirDataType {
    pub fn member_base_name_candidates(&self) -> Vec<String> {
        let mut names = Vec::new();
        let base = self.key();
        let base_key = base.to_string();

        names.push(base_key.clone());

        match self {
            MirDataType::Struct { identifier, .. }
                if let Some(name) = identifier.fully_qualified_path.name.as_ref() =>
            {
                names.push(name.to_string())
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

    pub fn substitute(&self, subst: &FxHashMap<String, MirDataType>) -> MirDataType {
        match self {
            MirDataType::Struct {
                identifier,
                generic_types,
            } => {
                if let Some(name) = identifier.fully_qualified_path.name.as_ref() {
                    let name_str = name.to_string();
                    if subst.contains_key(&name_str) {
                        return subst
                            .get(&name_str)
                            .cloned()
                            .unwrap_or_else(|| self.clone());
                    }
                }
                MirDataType::Struct {
                    identifier: identifier.clone(),
                    generic_types: generic_types.iter().map(|g| g.substitute(subst)).collect(),
                }
            }
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
            _ => self.clone(),
        }
    }
}

impl MirTypeKeysUsed for MirDataType {
    fn identifiers_used(&self) -> Vec<&TypeKey> {
        let mut types = Vec::new();
        match self {
            MirDataType::Struct { identifier, .. } => {
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
    Tuple(Vec<MirDataType>),
    List(Box<MirDataType>),
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
    NativeFunction {
        return_type: Box<MirDataType>,
        parameters: Vec<MirDataType>,
    },
    Ptr(Box<MirDataType>),
}

impl From<ParserFfiInnerType> for MirDataType {
    fn from(val: ParserFfiInnerType) -> MirDataType {
        match val {
            ParserFfiInnerType::F32 | ParserFfiInnerType::F64 | ParserFfiInnerType::LongDouble => {
                MirDataType::Float
            }
            ParserFfiInnerType::SChar | ParserFfiInnerType::UChar => MirDataType::Char,
            ParserFfiInnerType::U16
            | ParserFfiInnerType::U8
            | ParserFfiInnerType::U32
            | ParserFfiInnerType::U64
            | ParserFfiInnerType::USize
            | ParserFfiInnerType::UInt
            | ParserFfiInnerType::UShort
            | ParserFfiInnerType::ULong
            | ParserFfiInnerType::ULongLong => MirDataType::UInt,
            _ => MirDataType::Int,
        }
    }
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
            MirDataType::Function {
                return_type,
                parameters,
            } => ParserInnerType::Function {
                return_type: Box::new((*return_type).into()),
                parameters: parameters.into_iter().map(ParserDataType::from).collect(),
            },
            MirDataType::NativeFunction {
                return_type,
                parameters,
            } => ParserInnerType::NativeFunction {
                return_type: Box::new((*return_type).into()),
                parameters: parameters.into_iter().map(ParserDataType::from).collect(),
            },
            MirDataType::List(x) => ParserInnerType::List(Box::new((*x).into())),
            MirDataType::Option(x) => ParserInnerType::Option(Box::new((*x).into())),
            MirDataType::Ptr(x) => ParserInnerType::Ptr(Box::new((*x).into())),
            MirDataType::Ref(x, mutability) => {
                ParserInnerType::Ref(Box::new((*x).into()), mutability)
            }
            MirDataType::Result { ok, err } => ParserInnerType::Result {
                ok: Box::new((*ok).into()),
                err: Box::new((*err).into()),
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

impl From<ParserDataType> for MirDataType {
    fn from(value: ParserDataType) -> Self {
        value.data_type.into()
    }
}

impl From<&ParserDataType> for MirDataType {
    fn from(value: &ParserDataType) -> Self {
        (&value.data_type).into()
    }
}

impl From<ParserInnerType> for MirDataType {
    fn from(value: ParserInnerType) -> Self {
        match value {
            ParserInnerType::Float => MirDataType::Float,
            ParserInnerType::UInt => MirDataType::UInt,
            ParserInnerType::Byte => MirDataType::Byte,
            ParserInnerType::Int => MirDataType::Int,
            ParserInnerType::Big => MirDataType::Big,
            ParserInnerType::Null => MirDataType::Null,
            ParserInnerType::Bool => MirDataType::Bool,
            ParserInnerType::Str => MirDataType::Str,
            ParserInnerType::Char => MirDataType::Char,
            ParserInnerType::Host => MirDataType::Host,
            ParserInnerType::Dynamic => MirDataType::Dynamic,
            ParserInnerType::Range => MirDataType::Range,
            ParserInnerType::Tuple(xs) => {
                MirDataType::Tuple(xs.into_iter().map(MirDataType::from).collect())
            }
            ParserInnerType::List(x) => MirDataType::List(Box::new((*x).into())),
            ParserInnerType::Option(x) => MirDataType::Option(Box::new((*x).into())),
            ParserInnerType::Result { ok, err } => MirDataType::Result {
                ok: Box::new((*ok).into()),
                err: Box::new((*err).into()),
            },
            ParserInnerType::Function {
                return_type,
                parameters,
            } => MirDataType::Function {
                return_type: Box::new((*return_type).into()),
                parameters: parameters.into_iter().map(MirDataType::from).collect(),
            },
            ParserInnerType::Ref(x, m) => MirDataType::Ref(Box::new((*x).into()), m),
            ParserInnerType::Ptr(x) => MirDataType::Ptr(Box::new((*x).into())),
            ParserInnerType::Struct(name) => MirDataType::Struct {
                identifier: TypeKey {
                    fully_qualified_path: Arc::new(FullyQualifiedPath {
                        name: Some(Ustr::from(&name)),
                        parent: None,
                    }),
                },
                generic_types: Vec::new(),
            },
            ParserInnerType::StructWithGenerics {
                identifier,
                generic_types,
            } => {
                if identifier == "ptr" && generic_types.len() == 1 {
                    MirDataType::Ptr(Box::new(generic_types[0].clone().into()))
                } else if identifier == "list" && generic_types.len() == 1 {
                    MirDataType::List(Box::new(generic_types[0].clone().into()))
                } else {
                    MirDataType::Struct {
                        identifier: TypeKey {
                            fully_qualified_path: Arc::new(FullyQualifiedPath {
                                name: Some(Ustr::from(&identifier)),
                                parent: None,
                            }),
                        },
                        generic_types: generic_types.into_iter().map(MirDataType::from).collect(),
                    }
                }
            }
            ParserInnerType::Scope(types) => types
                .into_iter()
                .next()
                .map_or(MirDataType::Null, |t| t.into()),
            _ => MirDataType::Null,
        }
    }
}

impl MirRenamable for MirDataType {
    fn rename(&mut self, state: &mut MirRenameState) {
        match self {
            MirDataType::Big
            | MirDataType::Byte
            | MirDataType::Bool
            | MirDataType::Char
            | MirDataType::Float
            | MirDataType::Host
            | MirDataType::Int
            | MirDataType::Null
            | MirDataType::Range
            | MirDataType::UInt
            | MirDataType::Str
            | MirDataType::Dynamic => {}
            MirDataType::Struct {
                identifier,
                generic_types,
            } => {
                *identifier = state.mapped_type_or_original(identifier.clone());
                for g in generic_types {
                    g.rename(state);
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
            MirDataType::Ref(x, _) => x.rename(state),
        }
    }
}

impl From<&ParserInnerType> for MirDataType {
    fn from(value: &ParserInnerType) -> Self {
        value.clone().into()
    }
}

impl MirDataType {
    pub fn object(identifier: TypeKey) -> Self {
        MirDataType::Struct {
            identifier,
            generic_types: Vec::new(),
        }
    }

    pub fn function(parameters: Vec<MirDataType>, return_type: MirDataType) -> MirDataType {
        MirDataType::Function {
            return_type: Box::new(return_type),
            parameters,
        }
    }

    pub fn get_gen(&self) -> Option<MirDataType> {
        match self.unwrap_all_refs() {
            MirDataType::Struct {
                identifier,
                generic_types,
            } if identifier.name() == "gen" => generic_types.first().cloned(),
            _ => None,
        }
    }

    pub fn is_int(self) -> bool {
        matches!(
            self.unwrap_all_refs(),
            MirDataType::Int | MirDataType::UInt | MirDataType::Byte
        )
    }

    pub fn is_native(self) -> bool {
        !matches!(self.unwrap_all_refs(), MirDataType::Struct { .. })
    }

    pub fn default_node(&self, span: Span) -> Option<AstNode> {
        match self {
            MirDataType::Int => Some(AstNode::int(span, 0)),
            MirDataType::UInt => Some(AstNode::int(span, "0u")),
            MirDataType::Byte => Some(AstNode::int(span, "0b")),
            MirDataType::Str => Some(AstNode::new(
                span,
                AstNodeType::StringLiteral(AstString {
                    value: ParserText::new(span, ""),
                }),
            )),
            MirDataType::Char => Some(AstNode::new(
                span,
                AstNodeType::CharLiteral(AstChar { value: '\0' }),
            )),
            MirDataType::Dynamic => Some(AstNode::new(span, AstNodeType::Null)),
            MirDataType::Null => Some(AstNode::new(span, AstNodeType::Null)),
            MirDataType::List(t) => Some(AstNode::new(
                span,
                AstNodeType::ListLiteral(AstList {
                    data_type: (*t.clone()).into(),
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
            MirDataType::Option(_) => Some(AstNode::none(span)),
            MirDataType::Result { ok, .. } => Some(AstNode::call(
                span,
                AstNode::identifier(span, "ok"),
                vec![CallArg::Value(ok.default_node(span)?)],
            )),
            _ => None,
        }
    }
}

impl MirDataType {
    pub fn unwrap_all_refs(&self) -> &Self {
        match self {
            Self::Ref(x, _) => x.unwrap_all_refs(),
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
        match self.unwrap_all_refs() {
            MirDataType::Struct { identifier, .. } => MirDataType::Struct {
                identifier: identifier.clone(),
                generic_types: Vec::new(),
            },
            x => x.clone(),
        }
    }

    // TODO Try and replace all instances of :
    // #[deprecated(note = "If an impl is required use TypeImplKey::from")]
    pub fn impl_name(&self) -> String {
        match self.key() {
            MirDataType::Struct { identifier, .. } => identifier.name().to_string(),
            other => other.to_string(),
        }
    }

    pub fn is_dyn(&self) -> bool {
        matches!(self, Self::Dynamic)
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
        matches!(self, Self::List(x) if x.is_dyn() || x.is_dyn_list())
    }

    pub fn is_tuple(&self) -> bool {
        matches!(self, Self::Tuple(_))
    }

    #[inline]
    pub fn is_gen(&self) -> bool {
        matches!(self, Self::Struct { identifier, .. } if identifier.name() == "gen")
    }

    pub fn loose_eq(&self, other: &Self) -> bool {
        let result = other.is_host()
            || other.is_dyn()
            || other.is_dyn_list()
            || self.is_host()
            || self.is_dyn()
            || self.is_dyn_list()
            || other == self;

        if result {
            return true;
        }

        match (self, other) {
            (MirDataType::List(a), MirDataType::List(b)) => a.loose_eq(b),
            (MirDataType::Option(a), MirDataType::Option(b)) => a.loose_eq(b),
            (MirDataType::Result { ok: ao, err: ae }, MirDataType::Result { ok: bo, err: be }) => {
                ao.loose_eq(bo) && ae.loose_eq(be)
            }
            (MirDataType::Ptr(a), MirDataType::Ptr(b)) => a.loose_eq(b),
            (MirDataType::Ref(a, _), MirDataType::Ref(b, _)) => a.loose_eq(b),
            (MirDataType::Ref(a, _), b) => a.loose_eq(b),
            (a, MirDataType::Ref(b, _)) => a.loose_eq(b),
            (MirDataType::Tuple(a), MirDataType::Tuple(b)) => {
                if a.len() != b.len() {
                    return false;
                }
                a.iter().zip(b.iter()).all(|(x, y)| x.loose_eq(y))
            }
            _ => false,
        }
    }

    pub fn contains_auto(&self) -> bool {
        match self {
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
            MirDataType::Struct { generic_types, .. } => {
                generic_types.iter().any(|x| x.contains_auto())
            }
            _ => false,
        }
    }

    pub fn matches(&self, other: &Self, _generic_params: &[&str]) -> bool {
        match (self, other) {
            (
                MirDataType::Struct {
                    identifier: a,
                    generic_types: ag,
                },
                MirDataType::Struct {
                    identifier: b,
                    generic_types: bg,
                },
            ) => {
                if a != b || ag.len() != bg.len() {
                    return false;
                }
                ag.iter()
                    .zip(bg.iter())
                    .all(|(x, y)| x.matches(y, _generic_params))
            }
            (MirDataType::List(a), MirDataType::List(b)) => a.matches(b, _generic_params),
            (MirDataType::Option(a), MirDataType::Option(b)) => a.matches(b, _generic_params),
            (MirDataType::Result { ok: ao, err: ae }, MirDataType::Result { ok: bo, err: be }) => {
                ao.matches(bo, _generic_params) && ae.matches(be, _generic_params)
            }
            (MirDataType::Ptr(a), MirDataType::Ptr(b)) => a.matches(b, _generic_params),
            (MirDataType::Ref(a, _), MirDataType::Ref(b, _)) => a.matches(b, _generic_params),
            (MirDataType::Ref(a, _), b) => a.matches(b, _generic_params),
            (a, MirDataType::Ref(b, _)) => a.matches(b, _generic_params),
            (MirDataType::Tuple(a), MirDataType::Tuple(b)) => {
                if a.len() != b.len() {
                    return false;
                }
                a.iter()
                    .zip(b.iter())
                    .all(|(x, y)| x.matches(y, _generic_params))
            }
            (x, y) => x == y,
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
}

impl Display for MirDataType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", ParserInnerType::from(self.clone()))
    }
}
