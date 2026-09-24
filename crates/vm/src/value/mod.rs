#[cfg(feature = "native")]
use crate::value::ffi::ExternFunction;
use crate::{
    VM,
    conversion::{Reg, VMLiteral},
    native::{NativeFunction, stdlib::generator::GeneratorState},
    value::{
        hashable::{RuntimeHashMap, RuntimeHashSet},
        spawn::{ChannelInner, MutexGuardInner, MutexInner, WaitGroupInner},
    },
};
use astro_float::{BigFloat, RoundingMode};
use calibre_lir::ast::BlockId;
use calibre_parser::ast::ObjectMap;
use dumpster::sync::Gc;
use dumpster::{TraceWith, Visitor};

use rustc_hash::FxHashMap;
use tracing::instrument;
use ustr::{Ustr, UstrMap};

use dyn_hash::DynHash;
use std::any::Any;
use std::{
    fmt::{Debug, Display},
    sync::Arc,
};
use wasm_sync::Mutex;

mod bridge;
pub mod conversion;
mod display;
pub mod embedded;
pub mod hashable;
pub mod natives;
pub mod spawn;

#[cfg(feature = "native")]
pub mod ffi;

pub mod operation;
pub use bridge::TerminateValue;

pub const BIG_PRECISION: usize = 128;
pub const BIG_ROUNDING: RoundingMode = RoundingMode::ToEven;

#[derive(Debug, Clone)]
pub struct GcVec(pub Vec<RuntimeValue>);

#[derive(Debug, Clone)]
pub struct GcMap(pub ObjectMap<RuntimeValue>);

unsafe impl<V: Visitor> TraceWith<V> for GcVec {
    fn accept(&self, visitor: &mut V) -> Result<(), ()> {
        for item in self.0.iter() {
            item.accept(visitor)?;
        }
        Ok(())
    }
}

unsafe impl<V: Visitor> TraceWith<V> for GcMap {
    fn accept(&self, visitor: &mut V) -> Result<(), ()> {
        for (_, value) in self.0.0.iter() {
            value.accept(visitor)?;
        }
        Ok(())
    }
}

pub trait HostInner: Debug + Any + Send + DynHash {
    fn as_any(&self) -> &dyn Any;
    fn as_any_mut(&mut self) -> &mut dyn Any;
}

impl<T: Debug + Any + Send + DynHash> HostInner for T {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn as_any_mut(&mut self) -> &mut dyn Any {
        self
    }
}

dyn_hash::hash_trait_object!(HostInner);

pub type Host = Arc<Mutex<dyn HostInner + Send>>;

#[derive(Debug, Clone, Default)]
pub enum RuntimeValue {
    #[default]
    Null,
    Float(f64),
    Big(BigFloat),
    Int(i64),
    UInt(u64),
    Byte(u8),
    Ptr(u64),
    Range(i64, i64),
    Bool(bool),
    Str(Ustr),
    Char(char),
    Aggregate(Option<Ustr>, Gc<GcMap>),
    Enum(Ustr, usize, Option<Gc<RuntimeValue>>),
    Ref(Ustr),
    VarRef(usize),
    RegRef {
        frame: usize,
        reg: Reg,
    },
    List(Gc<GcVec>),
    Option(Option<Gc<RuntimeValue>>),
    Result(Result<Gc<RuntimeValue>, Gc<RuntimeValue>>),
    Channel(Arc<ChannelInner>),
    WaitGroup(Arc<WaitGroupInner>),
    Mutex(Arc<MutexInner>),
    MutexGuard(Arc<MutexGuardInner>),
    HashMap(RuntimeHashMap),
    HashSet(RuntimeHashSet),
    NativeFunction(Arc<dyn NativeFunction>),
    #[cfg(feature = "native")]
    ExternFunction(Arc<ExternFunction>),
    Function {
        name: Ustr,
        captures: Arc<Vec<(Ustr, RuntimeValue)>>,
    },
    Generator {
        type_name: Ustr,
        state: Arc<Mutex<GeneratorState>>,
    },
    DynObject {
        type_name: Ustr,
        constraints: Arc<Vec<Ustr>>,
        value: Gc<RuntimeValue>,
        vtable: Arc<UstrMap<Ustr>>,
    },
    BoundMethod {
        callee: Box<RuntimeValue>,
        receiver: Gc<RuntimeValue>,
    },
    GeneratorSuspend(Box<RuntimeValue>),
    Host(Host),
}

unsafe impl<V: Visitor> TraceWith<V> for RuntimeValue {
    fn accept(&self, visitor: &mut V) -> Result<(), ()> {
        match self {
            RuntimeValue::Aggregate(_, map) => map.accept(visitor),
            RuntimeValue::Enum(_, _, Some(x)) => x.accept(visitor),
            RuntimeValue::List(x) => x.accept(visitor),
            RuntimeValue::Option(Some(x)) => x.accept(visitor),
            RuntimeValue::Result(Ok(x)) => x.accept(visitor),
            RuntimeValue::Result(Err(x)) => x.accept(visitor),
            RuntimeValue::Channel(ch) => {
                if let Ok(queue) = ch.queue.try_lock() {
                    for item in queue.iter() {
                        item.accept(visitor)?;
                    }
                }
                Ok(())
            }
            RuntimeValue::WaitGroup(_) => Ok(()),
            RuntimeValue::Mutex(m) => {
                let guard = m.lock();
                guard.get_clone().accept(visitor)
            }
            RuntimeValue::MutexGuard(guard) => guard.get_clone().accept(visitor),
            RuntimeValue::HashMap(map) => {
                if let Ok(guard) = map.map.try_lock() {
                    for value in guard.values() {
                        value.accept(visitor)?;
                    }
                }
                Ok(())
            }
            RuntimeValue::HashSet(_) => Ok(()),
            RuntimeValue::Host(_) => Ok(()),
            RuntimeValue::Function { captures, .. } => {
                for (_, value) in captures.as_ref().iter() {
                    value.accept(visitor)?;
                }
                Ok(())
            }
            RuntimeValue::Generator { .. } => Ok(()),
            RuntimeValue::DynObject { value, .. } => value.accept(visitor),
            RuntimeValue::BoundMethod { callee, receiver } => {
                callee.accept(visitor)?;
                receiver.accept(visitor)
            }
            RuntimeValue::GeneratorSuspend(value) => value.accept(visitor),
            #[cfg(feature = "native")]
            RuntimeValue::ExternFunction(_) => Ok(()),
            RuntimeValue::Ptr(_) => Ok(()),
            RuntimeValue::VarRef(_) => Ok(()),
            _ => Ok(()),
        }
    }
}

impl RuntimeValue {
    #[instrument(skip_all)]
    pub fn replace_list_aliases(&mut self, old_list: &Gc<GcVec>, new_list: &Gc<GcVec>) {
        match self {
            RuntimeValue::List(list) => {
                if VM::list_identity_eq(list, old_list) {
                    *list = new_list.clone();
                }
            }
            RuntimeValue::Aggregate(_, map) => {
                let entries = &mut Gc::make_mut(map).0.0;
                for (_, field) in entries.iter_mut() {
                    field.replace_list_aliases(old_list, new_list);
                }
            }
            RuntimeValue::Option(Some(inner))
            | RuntimeValue::Result(Ok(inner))
            | RuntimeValue::Result(Err(inner))
            | RuntimeValue::Enum(_, _, Some(inner)) => {
                Gc::make_mut(inner).replace_list_aliases(old_list, new_list);
            }
            RuntimeValue::DynObject { value: inner, .. } => {
                Gc::make_mut(inner).replace_list_aliases(old_list, new_list);
            }
            _ => {}
        }
    }

    #[inline]
    pub(crate) fn is_callable(&self) -> bool {
        let val = matches!(
            self,
            RuntimeValue::Function { .. }
                | RuntimeValue::NativeFunction(_)
                | RuntimeValue::Channel(_)
                | RuntimeValue::BoundMethod { .. }
        );

        #[cfg(feature = "native")]
        return val || matches!(self, RuntimeValue::ExternFunction(_));

        #[cfg(not(feature = "native"))]
        return val;
    }

    #[inline]
    pub fn might_contain_list(&self) -> bool {
        matches!(
            self,
            RuntimeValue::List(_)
                | RuntimeValue::Aggregate(_, _)
                | RuntimeValue::Enum(_, _, _)
                | RuntimeValue::Option(_)
                | RuntimeValue::Result(_)
                | RuntimeValue::DynObject { .. }
                | RuntimeValue::BoundMethod { .. }
        )
    }

    #[inline]
    pub fn bind_if_callable(self, receiver: RuntimeValue) -> RuntimeValue {
        match self {
            RuntimeValue::Function { .. } | RuntimeValue::NativeFunction(_) => {
                RuntimeValue::BoundMethod {
                    callee: Box::new(self),
                    receiver: Gc::new(receiver),
                }
            }
            #[cfg(feature = "native")]
            RuntimeValue::ExternFunction(_) => RuntimeValue::BoundMethod {
                callee: Box::new(self),
                receiver: Gc::new(receiver),
            },
            other => other,
        }
    }

    pub fn is_null(&self) -> bool {
        matches!(self, RuntimeValue::Null)
    }

    #[inline]
    pub fn is_ref_like(&self) -> bool {
        matches!(
            self,
            RuntimeValue::Ref(_)
                | RuntimeValue::VarRef(_)
                | RuntimeValue::RegRef { .. }
                | RuntimeValue::MutexGuard(_)
        )
    }

    #[inline]
    pub fn should_pass_by_reg_ref(&self) -> bool {
        matches!(
            self,
            RuntimeValue::Aggregate(_, _)
                | RuntimeValue::List(_)
                | RuntimeValue::Enum(_, _, _)
                | RuntimeValue::Option(_)
                | RuntimeValue::Result(_)
                | RuntimeValue::Ptr(_)
        )
    }

    pub fn impl_name(&self) -> Option<Ustr> {
        match self {
            RuntimeValue::Big(_) => Some("big"),
            RuntimeValue::Int(_) => Some("int"),
            RuntimeValue::UInt(_) => Some("uint"),
            RuntimeValue::Byte(_) => Some("byte"),
            RuntimeValue::Float(_) => Some("float"),
            RuntimeValue::Bool(_) => Some("bool"),
            RuntimeValue::Str(_) => Some("str"),
            RuntimeValue::Char(_) => Some("char"),
            RuntimeValue::Range(_, _) => Some("range"),
            RuntimeValue::Ptr(_) => Some("ptr"),
            RuntimeValue::Aggregate(Some(name), _) | RuntimeValue::Enum(name, _, _) => {
                return Some(*name);
            }
            RuntimeValue::Generator { type_name, .. } => return Some(*type_name),
            RuntimeValue::DynObject { type_name, .. } => return Some(*type_name),
            RuntimeValue::List(_) => Some("list"),
            RuntimeValue::Option(_) => Some("option"),
            RuntimeValue::Result(_) => Some("result"),
            RuntimeValue::Null => Some("null"),
            _ => None,
        }
        .map(Ustr::from)
    }
}

impl From<VMLiteral> for RuntimeValue {
    fn from(value: VMLiteral) -> Self {
        match value {
            VMLiteral::Bool(x) => Self::Bool(x),
            VMLiteral::Big(x) => Self::Big(x),
            VMLiteral::Int(x) => Self::Int(x),
            VMLiteral::UInt(x) => Self::UInt(x),
            VMLiteral::Byte(x) => Self::Byte(x),
            VMLiteral::Float(x) => Self::Float(x),
            VMLiteral::Char(x) => Self::Char(x),
            VMLiteral::String(x) => Self::Str(x),
            VMLiteral::Null => Self::Null,
            VMLiteral::Closure { label, captures: _ } => Self::Function {
                name: label,
                captures: Arc::new(Vec::new()),
            },
            VMLiteral::ExternFunction { .. } => Self::Null,
        }
    }
}
