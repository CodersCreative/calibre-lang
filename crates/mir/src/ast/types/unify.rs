use crate::ast::types::MirDataType;
use crate::symbols::TypeKey;
use rustc_hash::FxHashMap;
use serde::{Deserialize, Serialize};
use ustr::Ustr;

#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum TypeImplKey {
    Nominal(TypeKey),
    Primitive(Ustr),
    List,
    Tuple,
    Option,
    Result,
    Ptr,
    Gen,
    Function,
    Dynamic,
}

impl From<&MirDataType> for TypeImplKey {
    fn from(value: &MirDataType) -> Self {
        match value.unwrap_all_refs() {
            MirDataType::Struct { identifier, .. } => {
                TypeImplKey::Nominal(identifier.clone())
            }
            MirDataType::List(_) => TypeImplKey::List,
            MirDataType::Tuple(_) => TypeImplKey::Tuple,
            MirDataType::Option(_) => TypeImplKey::Option,
            MirDataType::Result { .. } => TypeImplKey::Result,
            MirDataType::Ptr(_) => TypeImplKey::Ptr,
            MirDataType::Gen(_) => TypeImplKey::Gen,
            MirDataType::Function { .. } | MirDataType::NativeFunction { .. } => {
                TypeImplKey::Function
            }
            MirDataType::Dynamic | MirDataType::DynamicTraits(_) => TypeImplKey::Dynamic,
            other => TypeImplKey::Primitive(Ustr::from(&other.to_string())),
        }
    }
}

impl MirDataType {
    pub fn can_unify(
        &self,
        other: &MirDataType,
        generic_params: &[Ustr],
        bindings: &mut FxHashMap<String, MirDataType>,
    ) -> bool {
        if let MirDataType::Struct {
            identifier,
            generic_types,
        } = self
        {
            if generic_types.is_empty()
                && let Some(name) = identifier.fully_qualified_path.name
                && generic_params.contains(&name)
            {
                let name_str = name.to_string();
                if let Some(existing) = bindings.get(&name_str) {
                    return existing == other;
                } else {
                    bindings.insert(name_str, other.clone());
                    return true;
                }
            }
        }

        match (self, other) {
            (MirDataType::Int, MirDataType::Int)
            | (MirDataType::UInt, MirDataType::UInt)
            | (MirDataType::Byte, MirDataType::Byte)
            | (MirDataType::Float, MirDataType::Float)
            | (MirDataType::Big, MirDataType::Big)
            | (MirDataType::Bool, MirDataType::Bool)
            | (MirDataType::Str, MirDataType::Str)
            | (MirDataType::Char, MirDataType::Char)
            | (MirDataType::Null, MirDataType::Null)
            | (MirDataType::Dynamic, MirDataType::Dynamic)
            | (MirDataType::Range, MirDataType::Range)
            | (MirDataType::Host, MirDataType::Host) => true,

            (MirDataType::List(p), MirDataType::List(c)) => {
                p.can_unify(c, generic_params, bindings)
            }
            (MirDataType::Ptr(p), MirDataType::Ptr(c)) => p.can_unify(c, generic_params, bindings),
            (MirDataType::Gen(p), MirDataType::Gen(c)) => p.can_unify(c, generic_params, bindings),
            (MirDataType::Option(p), MirDataType::Option(c)) => {
                p.can_unify(c, generic_params, bindings)
            }
            (
                MirDataType::Result {
                    ok: ok_p,
                    err: err_p,
                },
                MirDataType::Result {
                    ok: ok_c,
                    err: err_c,
                },
            ) => {
                ok_p.can_unify(ok_c, generic_params, bindings)
                    && err_p.can_unify(err_c, generic_params, bindings)
            }
            (MirDataType::Tuple(ps), MirDataType::Tuple(cs)) => {
                ps.len() == cs.len()
                    && ps
                        .iter()
                        .zip(cs)
                        .all(|(p, c)| p.can_unify(c, generic_params, bindings))
            }
            (MirDataType::Ref(p, m_p), MirDataType::Ref(c, m_c)) => {
                m_p == m_c && p.can_unify(c, generic_params, bindings)
            }
            (
                MirDataType::Struct {
                    identifier: id_p,
                    generic_types: g_p,
                },
                MirDataType::Struct {
                    identifier: id_c,
                    generic_types: g_c,
                },
            ) => {
                id_p == id_c
                    && g_p.len() == g_c.len()
                    && g_p
                        .iter()
                        .zip(g_c)
                        .all(|(p, c)| p.can_unify(c, generic_params, bindings))
            }
            (
                MirDataType::Function {
                    return_type: ret_p,
                    parameters: param_p,
                },
                MirDataType::Function {
                    return_type: ret_c,
                    parameters: param_c,
                },
            )
            | (
                MirDataType::NativeFunction {
                    return_type: ret_p,
                    parameters: param_p,
                },
                MirDataType::NativeFunction {
                    return_type: ret_c,
                    parameters: param_c,
                },
            ) => {
                ret_p.can_unify(ret_c, generic_params, bindings)
                    && param_p.len() == param_c.len()
                    && param_p
                        .iter()
                        .zip(param_c)
                        .all(|(p, c)| p.can_unify(c, generic_params, bindings))
            }
            (MirDataType::DynamicTraits(traits_p), MirDataType::DynamicTraits(traits_c)) => {
                traits_p == traits_c
            }
            _ => false,
        }
    }

    pub fn specificity(&self, generic_params: &[Ustr]) -> usize {
        if let MirDataType::Struct {
            identifier,
            generic_types,
        } = self
        {
            if generic_types.is_empty() {
                if let Some(name) = identifier.fully_qualified_path.name {
                    if generic_params.contains(&name) {
                        return 1;
                    }
                }
            }
        }

        match self {
            MirDataType::Int
            | MirDataType::UInt
            | MirDataType::Byte
            | MirDataType::Float
            | MirDataType::Big
            | MirDataType::Bool
            | MirDataType::Str
            | MirDataType::Char
            | MirDataType::Null
            | MirDataType::Dynamic
            | MirDataType::Range
            | MirDataType::Host => 10,

            MirDataType::List(inner)
            | MirDataType::Ptr(inner)
            | MirDataType::Gen(inner)
            | MirDataType::Option(inner)
            | MirDataType::Ref(inner, _) => 10 + inner.specificity(generic_params),

            MirDataType::Result { ok, err } => {
                10 + ok.specificity(generic_params) + err.specificity(generic_params)
            }

            MirDataType::Tuple(items) => {
                10 + items
                    .iter()
                    .map(|item| item.specificity(generic_params))
                    .sum::<usize>()
            }

            MirDataType::Struct { generic_types, .. } => {
                10 + generic_types
                    .iter()
                    .map(|item| item.specificity(generic_params))
                    .sum::<usize>()
            }

            MirDataType::Function {
                return_type,
                parameters,
            }
            | MirDataType::NativeFunction {
                return_type,
                parameters,
            } => {
                10 + return_type.specificity(generic_params)
                    + parameters
                        .iter()
                        .map(|item| item.specificity(generic_params))
                        .sum::<usize>()
            }

            MirDataType::DynamicTraits(traits) => 10 + traits.len() * 5,
        }
    }
}
