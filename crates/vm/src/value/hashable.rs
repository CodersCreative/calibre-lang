use std::{
    hash::{Hash, Hasher},
    sync::Arc,
};

use crate::{
    error::RuntimeError,
    value::{GcVec, Host, RuntimeValue},
};
use calibre_parser::ast::{ObjectMap, types::ParserInnerType};
use dumpster::sync::Gc;
use rustc_hash::{FxHashMap, FxHashSet};
use ustr::Ustr;
use wasm_sync::Mutex;

#[derive(Debug, Clone)]
pub enum HashKey {
    Null,
    Int(i64),
    UInt(u64),
    Bool(bool),
    Char(char),
    Str(Ustr),
    Float(u64),
    Big(Ustr),
    Ptr(u64),
    Range(i64, i64),
    List(Vec<HashKey>),
    Aggregate(Option<Ustr>, Vec<(Ustr, HashKey)>),
    Option(Option<Box<HashKey>>),
    Result(Box<HashKey>, bool),
    Function(Ustr, Option<Vec<HashKey>>),
    Host(Host),
}

impl Hash for HashKey {
    fn hash<H: Hasher>(&self, state: &mut H) {
        core::mem::discriminant(self).hash(state);

        match self {
            HashKey::Null => {}
            HashKey::Int(v) => v.hash(state),
            HashKey::UInt(v) => v.hash(state),
            HashKey::Bool(v) => v.hash(state),
            HashKey::Char(v) => v.hash(state),
            HashKey::Str(v) => v.hash(state),
            HashKey::Float(v) => v.hash(state),
            HashKey::Big(v) => v.hash(state),
            HashKey::Ptr(v) => v.hash(state),
            HashKey::Range(a, b) => {
                a.hash(state);
                b.hash(state);
            }
            HashKey::List(v) => v.hash(state),
            HashKey::Aggregate(name, fields) => {
                name.hash(state);
                fields.hash(state);
            }
            HashKey::Option(opt) => opt.hash(state),
            HashKey::Result(res, b) => {
                res.hash(state);
                b.hash(state);
            }
            HashKey::Function(name, args) => {
                name.hash(state);
                args.hash(state);
            }
            HashKey::Host(host) => {
                let guard = host.lock().unwrap();
                dyn_hash::DynHash::dyn_hash(&*guard, state);
            }
        }
    }
}

impl PartialEq for HashKey {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (HashKey::Null, HashKey::Null) => true,
            (HashKey::Int(a), HashKey::Int(b)) => a == b,
            (HashKey::UInt(a), HashKey::UInt(b)) => a == b,
            (HashKey::Bool(a), HashKey::Bool(b)) => a == b,
            (HashKey::Char(a), HashKey::Char(b)) => a == b,
            (HashKey::Str(a), HashKey::Str(b)) => a == b,
            (HashKey::Float(a), HashKey::Float(b)) => a == b,
            (HashKey::Big(a), HashKey::Big(b)) => a == b,
            (HashKey::Ptr(a), HashKey::Ptr(b)) => a == b,
            (HashKey::Range(a1, a2), HashKey::Range(b1, b2)) => a1 == b1 && a2 == b2,
            (HashKey::List(a), HashKey::List(b)) => a == b,
            (HashKey::Aggregate(n1, f1), HashKey::Aggregate(n2, f2)) => n1 == n2 && f1 == f2,
            (HashKey::Option(a), HashKey::Option(b)) => a == b,
            (HashKey::Result(a1, a2), HashKey::Result(b1, b2)) => a1 == b1 && a2 == b2,
            (HashKey::Function(n1, a1), HashKey::Function(n2, a2)) => n1 == n2 && a1 == a2,
            (HashKey::Host(a), HashKey::Host(b)) => Arc::ptr_eq(a, b),
            _ => false,
        }
    }
}

impl Eq for HashKey {}

impl TryFrom<RuntimeValue> for HashKey {
    type Error = RuntimeError;
    fn try_from(value: RuntimeValue) -> Result<Self, Self::Error> {
        match value {
            RuntimeValue::Null => Ok(Self::Null),
            RuntimeValue::Int(x) => Ok(Self::Int(x)),
            RuntimeValue::Float(x) => Ok(Self::Float(x.to_bits())),
            RuntimeValue::Big(x) => Ok(Self::Big(Ustr::from(&x.to_string()))),
            RuntimeValue::Function { name, captures } => {
                let mut converted = Vec::new();

                for (_, cap_val) in captures.iter() {
                    converted.push(HashKey::try_from(cap_val.clone())?);
                }

                Ok(Self::Function(name, Some(converted)))
            }
            RuntimeValue::UInt(x) => Ok(Self::UInt(x)),
            RuntimeValue::Byte(x) => Ok(Self::UInt(x as u64)),
            RuntimeValue::Bool(x) => Ok(Self::Bool(x)),
            RuntimeValue::Char(x) => Ok(Self::Char(x)),
            RuntimeValue::Str(x) => Ok(Self::Str(x)),
            RuntimeValue::List(lst) => {
                let mut out = Vec::with_capacity(lst.as_ref().0.len());

                for v in &lst.as_ref().0 {
                    out.push(HashKey::try_from(v.clone())?);
                }

                Ok(Self::List(out))
            }
            RuntimeValue::Aggregate(name, map) => {
                let mut entries = Vec::with_capacity(map.as_ref().0.0.len());

                for (k, v) in map.as_ref().0.0.iter() {
                    let key = Ustr::from(k.as_str());
                    let hk = HashKey::try_from(v.clone())?;
                    entries.push((key, hk));
                }

                Ok(Self::Aggregate(name, entries))
            }
            RuntimeValue::Option(opt) => match opt {
                Some(inner) => Ok(Self::Option(Some(Box::new(HashKey::try_from(
                    inner.as_ref().clone(),
                )?)))),
                None => Ok(Self::Option(None)),
            },
            RuntimeValue::Result(res) => match res {
                Ok(value) => Ok(Self::Result(
                    Box::new(HashKey::try_from(value.as_ref().clone())?),
                    false,
                )),
                Err(value) => Ok(Self::Result(
                    Box::new(HashKey::try_from(value.as_ref().clone())?),
                    true,
                )),
            },
            RuntimeValue::Ptr(id) => Ok(Self::Ptr(id)),
            RuntimeValue::Range(a, b) => Ok(Self::Range(a, b)),
            RuntimeValue::Host(x) => Ok(Self::Host(x)),
            other => Err(RuntimeError::UnexpectedTypeInConversion {
                value: Box::new(other),
                target_type: ParserInnerType::Str,
            }),
        }
    }
}

impl From<HashKey> for RuntimeValue {
    fn from(value: HashKey) -> Self {
        match value {
            HashKey::Null => RuntimeValue::Null,
            HashKey::Int(x) => RuntimeValue::Int(x),
            HashKey::UInt(x) => RuntimeValue::UInt(x),
            HashKey::Bool(x) => RuntimeValue::Bool(x),
            HashKey::Char(x) => RuntimeValue::Char(x),
            HashKey::Str(x) => RuntimeValue::Str(x),
            HashKey::Float(bits) => RuntimeValue::Float(f64::from_bits(bits)),
            HashKey::Big(s) => RuntimeValue::Str(s),
            HashKey::Ptr(p) => RuntimeValue::Ptr(p),
            HashKey::Range(a, b) => RuntimeValue::Range(a, b),
            HashKey::List(values) => RuntimeValue::List(Gc::new(GcVec(
                values.into_iter().map(RuntimeValue::from).collect(),
            ))),
            HashKey::Option(opt) => match opt {
                Some(bx) => RuntimeValue::Option(Some(Gc::new(RuntimeValue::from(*bx)))),
                None => RuntimeValue::Option(None),
            },
            HashKey::Result(bx, is_err) => {
                let v = RuntimeValue::from(*bx);
                if is_err {
                    RuntimeValue::Result(Err(Gc::new(v)))
                } else {
                    RuntimeValue::Result(Ok(Gc::new(v)))
                }
            }
            HashKey::Aggregate(name, values) => {
                let mut entries = Vec::with_capacity(values.len());

                for (k, v) in values {
                    entries.push((k, RuntimeValue::from(v)));
                }

                RuntimeValue::Aggregate(name, Gc::new(super::GcMap(ObjectMap(entries))))
            }
            HashKey::Function(name, captures) => RuntimeValue::Function {
                name,
                captures: Arc::new(
                    captures
                        .map(|b| {
                            b.into_iter()
                                .map(|k| (Ustr::default(), RuntimeValue::from(k)))
                                .collect()
                        })
                        .unwrap_or_default(),
                ),
            },
            HashKey::Host(x) => RuntimeValue::Host(x),
        }
    }
}

#[derive(Debug, Clone, Default)]
pub struct RuntimeHashMap {
    pub map: Arc<Mutex<FxHashMap<HashKey, RuntimeValue>>>,
}

#[derive(Debug, Clone, Default)]
pub struct RuntimeHashSet {
    pub set: Arc<Mutex<FxHashSet<HashKey>>>,
}
