#[cfg(feature = "native")]
use crate::value::ffi::ExternFunction;
use crate::{
    VM,
    native::{NativeFunction, stdlib::generator::GeneratorState},
    value::{
        hashable::{RuntimeHashMap, RuntimeHashSet},
        spawn::{ChannelInner, MutexGuardInner, MutexInner, WaitGroupInner},
    },
};
use astro_float::{BigFloat, Consts, RoundingMode};
use calibre_bytecode::{Reg, VMLiteral};
use calibre_lir::{MirDataType, TypeImplKey, TypeKey, VariableKey, ast::BlockId};
use calibre_parser::ast::ObjectMap;
use dumpster::sync::Gc;
use dumpster::{TraceWith, Visitor};
use ustr::Ustr;

use dyn_hash::DynHash;
use std::{any::Any, ops::DerefMut};
use std::{
    fmt::{Debug, Display},
    ops::Deref,
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
pub mod write_back;

#[cfg(feature = "native")]
pub mod ffi;

pub mod operation;
pub use bridge::TerminateValue;

pub const BIG_PRECISION: usize = 128;
pub const BIG_ROUNDING: RoundingMode = RoundingMode::ToEven;

#[derive(Debug, Clone)]
pub struct ValueSlot(Arc<RuntimeValue>);

impl ValueSlot {
    #[inline]
    pub fn new(value: RuntimeValue) -> Self {
        Self(Arc::new(value))
    }

    #[inline]
    pub fn make_mut(&mut self) -> &mut RuntimeValue {
        Arc::make_mut(&mut self.0)
    }
}

impl From<ValueSlot> for RuntimeValue {
    fn from(value: ValueSlot) -> Self {
        value.as_ref().clone()
    }
}

impl AsRef<RuntimeValue> for ValueSlot {
    fn as_ref(&self) -> &RuntimeValue {
        self.0.as_ref()
    }
}

impl From<RuntimeValue> for ValueSlot {
    fn from(value: RuntimeValue) -> Self {
        Self::new(value)
    }
}

impl Deref for ValueSlot {
    type Target = RuntimeValue;

    fn deref(&self) -> &Self::Target {
        self.as_ref()
    }
}

unsafe impl<V: Visitor> TraceWith<V> for ValueSlot {
    fn accept(&self, visitor: &mut V) -> Result<(), ()> {
        self.as_ref().accept(visitor)
    }
}

#[derive(Debug, Clone)]
pub struct GcVec(pub Vec<ValueSlot>);

impl GcVec {
    pub fn new(values: Vec<RuntimeValue>) -> Self {
        Self(values.into_iter().map(ValueSlot::new).collect())
    }
}

impl Deref for GcVec {
    type Target = Vec<ValueSlot>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for GcVec {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl From<Vec<ValueSlot>> for GcVec {
    fn from(value: Vec<ValueSlot>) -> Self {
        Self(value)
    }
}

#[derive(Debug, Clone)]
pub struct GcMap(pub ObjectMap<ValueSlot>);

impl GcMap {
    pub fn new(map: ObjectMap<RuntimeValue>) -> Self {
        Self(ObjectMap(
            map.0
                .into_iter()
                .map(|(key, value)| (key, ValueSlot::new(value)))
                .collect(),
        ))
    }
}

impl Deref for GcMap {
    type Target = ObjectMap<ValueSlot>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for GcMap {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl From<ObjectMap<ValueSlot>> for GcMap {
    fn from(value: ObjectMap<ValueSlot>) -> Self {
        Self(value)
    }
}

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
    Aggregate(Option<TypeKey>, Arc<GcMap>),
    Enum(TypeKey, usize, Option<Gc<RuntimeValue>>),
    Ref(VariableKey),
    VarRef(usize),
    RegRef {
        frame: usize,
        reg: Reg,
    },
    List(Arc<GcVec>),
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
        name: VariableKey,
        captures: Arc<Vec<(VariableKey, RuntimeValue)>>,
    },
    Generator {
        type_name: TypeImplKey,
        state: Arc<Mutex<GeneratorState>>,
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
            RuntimeValue::Aggregate(_, map) => map.as_ref().accept(visitor),
            RuntimeValue::Enum(_, _, Some(x)) => x.accept(visitor),
            RuntimeValue::List(x) => x.as_ref().accept(visitor),
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
                for value in map.map.values() {
                    value.accept(visitor)?;
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

    pub fn to_type(&self) -> Option<MirDataType> {
        todo!()
    }

    pub fn impl_key(&self) -> Option<TypeImplKey> {
        match self {
            RuntimeValue::Big(_) => Some(TypeImplKey::Primitive(Ustr::from("big"))),
            RuntimeValue::Int(_) => Some(TypeImplKey::Primitive(Ustr::from("int"))),
            RuntimeValue::UInt(_) => Some(TypeImplKey::Primitive(Ustr::from("uint"))),
            RuntimeValue::Byte(_) => Some(TypeImplKey::Primitive(Ustr::from("byte"))),
            RuntimeValue::Float(_) => Some(TypeImplKey::Primitive(Ustr::from("float"))),
            RuntimeValue::Bool(_) => Some(TypeImplKey::Primitive(Ustr::from("bool"))),
            RuntimeValue::Str(_) => Some(TypeImplKey::Primitive(Ustr::from("str"))),
            RuntimeValue::Char(_) => Some(TypeImplKey::Primitive(Ustr::from("char"))),
            RuntimeValue::Range(_, _) => Some(TypeImplKey::Primitive(Ustr::from("range"))),
            RuntimeValue::Ptr(_) => Some(TypeImplKey::Ptr),
            RuntimeValue::Aggregate(Some(name), _) | RuntimeValue::Enum(name, _, _) => {
                Some(TypeImplKey::from(name.clone()))
            }
            RuntimeValue::Generator { type_name, .. } => Some(type_name.clone()),
            RuntimeValue::List(_) => Some(TypeImplKey::List),
            RuntimeValue::Option(_) => Some(TypeImplKey::Option),
            RuntimeValue::Result(_) => Some(TypeImplKey::Result),
            RuntimeValue::Null => Some(TypeImplKey::Primitive(Ustr::from("null"))),
            _ => None,
        }
    }

    pub fn from_vm_literal(value: VMLiteral, cc: &mut Consts) -> Self {
        match value {
            VMLiteral::Bool(x) => Self::Bool(x),
            VMLiteral::Big(x) => Self::Big(BigFloat::parse(
                &x,
                astro_float::Radix::Dec,
                BIG_PRECISION,
                BIG_ROUNDING,
                cc,
            )),
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
