use std::{
    cell::UnsafeCell,
    collections::VecDeque,
    fmt::{Debug, Display, Write},
    net::{TcpListener, TcpStream},
    sync::{
        Arc, Condvar, Mutex,
        atomic::{AtomicBool, AtomicIsize, Ordering},
    },
};

use calibre_lir::BlockId;
use calibre_parser::ast::{ObjectMap, ParserDataType, ParserFfiInnerType, ParserInnerType};
use dumpster::sync::Gc;
use dumpster::{TraceWith, Visitor};
use libffi::middle::{Arg, Cif, CodePtr, Type};
use libloading::Library;
use rustc_hash::FxHashMap;
use std::ffi::{CStr, CString};
use std::os::raw::c_char;
use std::os::raw::c_void;

use crate::{
    TaskState, VM,
    conversion::{Reg, VMLiteral},
    error::RuntimeError,
    native::{self, NativeFunction, stdlib},
};

mod bridge;
pub mod conversion;
mod display;
mod ffi;
pub mod operation;

pub use bridge::TerminateValue;

#[derive(Debug, Clone)]
pub struct GcVec(pub Vec<RuntimeValue>);

#[derive(Debug, Clone)]
pub struct GcMap(pub ObjectMap<RuntimeValue>);

#[derive(Debug, Clone)]
pub struct GeneratorState {
    pub vm: VM,
    pub function_name: Arc<String>,
    pub captures: std::sync::Arc<Vec<(String, RuntimeValue)>>,
    pub task_state: TaskState,
    pub index: i64,
    pub completed: bool,
}

#[derive(Debug, Clone)]
pub struct GeneratorResumeFn {
    pub state: Arc<Mutex<GeneratorState>>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum HashKey {
    Int(i64),
    UInt(u64),
    Bool(bool),
    Char(char),
    Str(Arc<String>),
}

impl TryFrom<RuntimeValue> for HashKey {
    type Error = RuntimeError;
    fn try_from(value: RuntimeValue) -> Result<Self, Self::Error> {
        match value {
            RuntimeValue::Int(x) => Ok(Self::Int(x)),
            RuntimeValue::UInt(x) => Ok(Self::UInt(x)),
            RuntimeValue::Byte(x) => Ok(Self::UInt(x as u64)),
            RuntimeValue::Bool(x) => Ok(Self::Bool(x)),
            RuntimeValue::Char(x) => Ok(Self::Char(x)),
            RuntimeValue::Str(x) => Ok(Self::Str(x)),
            other => Err(RuntimeError::UnexpectedType(other)),
        }
    }
}

impl From<HashKey> for RuntimeValue {
    fn from(value: HashKey) -> Self {
        match value {
            HashKey::Int(x) => RuntimeValue::Int(x),
            HashKey::UInt(x) => RuntimeValue::UInt(x),
            HashKey::Bool(x) => RuntimeValue::Bool(x),
            HashKey::Char(x) => RuntimeValue::Char(x),
            HashKey::Str(x) => RuntimeValue::Str(x),
        }
    }
}

#[derive(Debug)]
pub struct ChannelInner {
    pub queue: Mutex<VecDeque<RuntimeValue>>,
    pub closed: AtomicBool,
    pub cvar: Condvar,
}

impl ChannelInner {
    pub fn new() -> Self {
        Self {
            queue: Mutex::new(VecDeque::new()),
            closed: AtomicBool::new(false),
            cvar: Condvar::new(),
        }
    }
}

#[derive(Debug)]
pub struct WaitGroupInner {
    pub count: AtomicIsize,
    pub mutex: Mutex<()>,
    pub cvar: Condvar,
    pub joined: Mutex<Vec<Arc<WaitGroupInner>>>,
}

impl WaitGroupInner {
    pub fn new() -> Self {
        Self {
            count: AtomicIsize::new(0),
            mutex: Mutex::new(()),
            cvar: Condvar::new(),
            joined: Mutex::new(Vec::new()),
        }
    }

    pub fn done(&self) {
        let remaining = self.count.fetch_sub(1, Ordering::AcqRel) - 1;
        if remaining <= 0 {
            self.cvar.notify_all();
        }
    }

    pub fn wait(&self) -> Result<(), RuntimeError> {
        let mut guard = self
            .mutex
            .lock()
            .map_err(|_| RuntimeError::UnexpectedType(RuntimeValue::Null))?;
        while self.count.load(Ordering::Acquire) > 0 {
            guard = self
                .cvar
                .wait(guard)
                .map_err(|_| RuntimeError::UnexpectedType(RuntimeValue::Null))?;
        }
        drop(guard);
        let joined = self
            .joined
            .lock()
            .map_err(|_| RuntimeError::UnexpectedType(RuntimeValue::Null))?;
        for inner in joined.iter() {
            inner.wait()?;
        }
        Ok(())
    }
}

#[derive(Debug)]
pub struct MutexInner {
    locked: AtomicBool,
    mutex: Mutex<()>,
    cvar: Condvar,
    value: UnsafeCell<RuntimeValue>,
}

unsafe impl Send for MutexInner {}
unsafe impl Sync for MutexInner {}

impl MutexInner {
    pub fn new(value: RuntimeValue) -> Self {
        Self {
            locked: AtomicBool::new(false),
            mutex: Mutex::new(()),
            cvar: Condvar::new(),
            value: UnsafeCell::new(value),
        }
    }

    pub fn lock(self: &Arc<Self>) -> MutexGuardInner {
        let mut guard = self.mutex.lock().unwrap_or_else(|e| e.into_inner());
        while self.locked.load(Ordering::Acquire) {
            guard = self.cvar.wait(guard).unwrap_or_else(|e| e.into_inner());
        }
        self.locked.store(true, Ordering::Release);
        drop(guard);
        MutexGuardInner {
            inner: self.clone(),
            released: AtomicBool::new(false),
        }
    }

    fn unlock(&self) {
        self.locked.store(false, Ordering::Release);
        self.cvar.notify_one();
    }

    fn get_clone(&self) -> RuntimeValue {
        unsafe { (*self.value.get()).clone() }
    }

    fn set_value(&self, value: RuntimeValue) {
        unsafe {
            *self.value.get() = value;
        }
    }
}

#[derive(Debug)]
pub struct MutexGuardInner {
    inner: Arc<MutexInner>,
    released: AtomicBool,
}

impl MutexGuardInner {
    pub fn get_clone(&self) -> RuntimeValue {
        self.inner.get_clone()
    }

    pub fn set_value(&self, value: RuntimeValue) {
        self.inner.set_value(value);
    }
}

impl Drop for MutexGuardInner {
    fn drop(&mut self) {
        if !self.released.swap(true, Ordering::AcqRel) {
            self.inner.unlock();
        }
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

#[derive(Debug, Clone, Default)]
pub enum RuntimeValue {
    #[default]
    Null,
    Float(f64),
    Int(i64),
    UInt(u64),
    Byte(u8),
    Ptr(u64),
    Range(i64, i64),
    Bool(bool),
    Str(Arc<String>),
    Char(char),
    Aggregate(Option<String>, Gc<GcMap>),
    Enum(String, usize, Option<Gc<RuntimeValue>>),
    Ref(String),
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
    HashMap(Arc<Mutex<FxHashMap<HashKey, RuntimeValue>>>),
    HashSet(Arc<Mutex<rustc_hash::FxHashSet<HashKey>>>),
    TcpStream(Arc<Mutex<TcpStream>>),
    TcpListener(Arc<TcpListener>),
    NativeFunction(Arc<dyn NativeFunction>),
    ExternFunction(Arc<ExternFunction>),
    Function {
        name: Arc<String>,
        captures: std::sync::Arc<Vec<(String, RuntimeValue)>>,
    },
    Generator {
        type_name: Arc<String>,
        state: Arc<Mutex<GeneratorState>>,
    },
    DynObject {
        type_name: Arc<String>,
        constraints: Arc<Vec<String>>,
        value: Gc<RuntimeValue>,
        vtable: Arc<FxHashMap<String, String>>,
    },
    BoundMethod {
        callee: Box<RuntimeValue>,
        receiver: Gc<RuntimeValue>,
    },
    GeneratorSuspend(Box<RuntimeValue>),
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
                if let Ok(queue) = ch.queue.lock() {
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
                if let Ok(guard) = map.lock() {
                    for (_, value) in guard.iter() {
                        value.accept(visitor)?;
                    }
                }
                Ok(())
            }
            RuntimeValue::HashSet(_) => Ok(()),
            RuntimeValue::TcpStream(_) => Ok(()),
            RuntimeValue::TcpListener(_) => Ok(()),
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
            RuntimeValue::ExternFunction(_) => Ok(()),
            RuntimeValue::Ptr(_) => Ok(()),
            RuntimeValue::VarRef(_) => Ok(()),
            _ => Ok(()),
        }
    }
}

#[derive(Debug, Clone)]
pub struct ExternFunction {
    pub abi: String,
    pub library: String,
    pub symbol: String,
    pub parameters: Vec<ParserDataType>,
    pub return_type: ParserDataType,
    pub handle: Arc<Library>,
}

#[derive(Debug)]
enum FfiArg {
    U8(u8),
    I8(i8),
    U16(u16),
    I16(i16),
    U32(u32),
    I32(i32),
    U64(u64),
    I64(i64),
    F32(f32),
    F64(f64),
    Bool(u8),
    Char(u8),
    Ptr(*const c_void),
    CString { value: CString, ptr: *const c_void },
    Bytes { value: Vec<u8>, ptr: *const c_void },
    Struct { backing: Vec<u64> },
}

impl NativeFunction for GeneratorResumeFn {
    fn run(&self, _env: &mut VM, _args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        let mut state = self
            .state
            .lock()
            .map_err(|_| RuntimeError::UnexpectedType(RuntimeValue::Null))?;

        if state.completed {
            return Ok(RuntimeValue::Option(None));
        }

        let Some(func) = state
            .vm
            .resolve_function_by_name(state.function_name.as_str())
        else {
            state.completed = true;
            return Ok(RuntimeValue::Option(None));
        };

        let captures = state.captures.clone();
        let mut task_state = std::mem::take(&mut state.task_state);
        let status = state.vm.run_function_with_budget(
            func.as_ref(),
            Vec::<RuntimeValue>::new(),
            captures,
            usize::MAX,
            &mut task_state,
        )?;
        state.task_state = task_state;

        if let Some(yielded) = state.task_state.yielded.take() {
            state.index += 1;
            return Ok(RuntimeValue::Option(Some(Gc::new(yielded))));
        }

        if status.is_some() {
            state.completed = true;
        }

        Ok(RuntimeValue::Option(None))
    }

    fn name(&self) -> String {
        String::from("gen_resume")
    }
}

impl RuntimeValue {
    pub fn constants() -> FxHashMap<String, Self> {
        [
            ("true", RuntimeValue::Bool(true)),
            ("false", RuntimeValue::Bool(false)),
            ("none", RuntimeValue::Option(None)),
            ("INT_MIN", RuntimeValue::Int(i64::MIN)),
            ("INT_MAX", RuntimeValue::Int(i64::MAX)),
        ]
        .into_iter()
        .map(|(name, value)| (name.to_string(), value))
        .collect()
    }

    pub fn natives() -> FxHashMap<String, Self> {
        let lst: Vec<(&str, Arc<dyn NativeFunction>)> = vec![
            ("console_output", Arc::new(native::global::ConsoleOutput())),
            ("ok", Arc::new(native::global::OkFn())),
            ("err", Arc::new(native::global::ErrFn())),
            ("some", Arc::new(native::global::SomeFn())),
            ("repr", Arc::new(native::global::Repr())),
            ("len", Arc::new(native::global::Len())),
            ("trim", Arc::new(native::global::Trim())),
            ("str.split", Arc::new(stdlib::str::StrSplit())),
            ("str.contains", Arc::new(stdlib::str::StrContains())),
            ("str.starts_with", Arc::new(stdlib::str::StrStartsWith())),
            ("str.ends_with", Arc::new(stdlib::str::StrEndsWith())),
            ("env.get", Arc::new(stdlib::env::EnvGet)),
            ("env.var", Arc::new(stdlib::env::EnvVar)),
            ("env.set_var", Arc::new(stdlib::env::EnvSetVar)),
            ("env.remove_var", Arc::new(stdlib::env::EnvRemoveVar)),
            ("env.vars", Arc::new(stdlib::env::EnvVars)),
            ("fs.read_dir", Arc::new(stdlib::fs::FsReadDir)),
            ("discriminant", Arc::new(native::global::DiscriminantFn())),
            ("tuple", Arc::new(native::global::TupleFn())),
            ("panic", Arc::new(native::global::PanicFn())),
            ("assert", Arc::new(native::global::AssertFn())),
            ("gen_suspend", Arc::new(native::global::GenSuspendFn())),
            ("min_or_zero", Arc::new(native::global::MinOrZero())),
            ("async.channel_new", Arc::new(stdlib::r#async::ChannelNew())),
            (
                "async.channel_send",
                Arc::new(stdlib::r#async::ChannelSend()),
            ),
            ("async.channel_get", Arc::new(stdlib::r#async::ChannelGet())),
            (
                "async.channel_try_get",
                Arc::new(stdlib::r#async::ChannelTryGet()),
            ),
            (
                "async.channel_try_send",
                Arc::new(stdlib::r#async::ChannelTrySend()),
            ),
            (
                "async.channel_close",
                Arc::new(stdlib::r#async::ChannelClose()),
            ),
            (
                "async.channel_closed",
                Arc::new(stdlib::r#async::ChannelClosed()),
            ),
            ("crypto.sha256", Arc::new(stdlib::crypto::Sha256Fn)),
            ("crypto.sha512", Arc::new(stdlib::crypto::Sha512Fn)),
            ("crypto.blake3", Arc::new(stdlib::crypto::Blake3Fn)),
            ("regex.is_match", Arc::new(stdlib::regex::IsMatchFn)),
            ("regex.find", Arc::new(stdlib::regex::FindFn)),
            ("regex.replace", Arc::new(stdlib::regex::ReplaceFn)),
            (
                "process.raw_exec",
                Arc::new(stdlib::process::ProcessRawExec),
            ),
            (
                "collections.hashmap_new",
                Arc::new(stdlib::collections::HashMapNew),
            ),
            (
                "collections.hashmap_set",
                Arc::new(stdlib::collections::HashMapSet),
            ),
            (
                "collections.hashmap_get",
                Arc::new(stdlib::collections::HashMapGet),
            ),
            (
                "collections.hashmap_remove",
                Arc::new(stdlib::collections::HashMapRemove),
            ),
            (
                "collections.hashmap_contains",
                Arc::new(stdlib::collections::HashMapContains),
            ),
            (
                "collections.hashmap_len",
                Arc::new(stdlib::collections::HashMapLen),
            ),
            (
                "collections.hashmap_keys",
                Arc::new(stdlib::collections::HashMapKeys),
            ),
            (
                "collections.hashmap_values",
                Arc::new(stdlib::collections::HashMapValues),
            ),
            (
                "collections.hashmap_entries",
                Arc::new(stdlib::collections::HashMapEntries),
            ),
            (
                "collections.hashmap_clear",
                Arc::new(stdlib::collections::HashMapClear),
            ),
            (
                "collections.hashset_new",
                Arc::new(stdlib::collections::HashSetNew),
            ),
            (
                "collections.hashset_add",
                Arc::new(stdlib::collections::HashSetAdd),
            ),
            (
                "collections.hashset_remove",
                Arc::new(stdlib::collections::HashSetRemove),
            ),
            (
                "collections.hashset_contains",
                Arc::new(stdlib::collections::HashSetContains),
            ),
            (
                "collections.hashset_len",
                Arc::new(stdlib::collections::HashSetLen),
            ),
            (
                "collections.hashset_values",
                Arc::new(stdlib::collections::HashSetValues),
            ),
            (
                "collections.hashset_clear",
                Arc::new(stdlib::collections::HashSetClear),
            ),
            ("list.sort_by", Arc::new(stdlib::list::ListSortBy)),
            (
                "list.binary_search_by",
                Arc::new(stdlib::list::ListBinarySearchBy),
            ),
            ("list.raw_remove", Arc::new(stdlib::list::ListRawRemove)),
            ("net.tcp_connect", Arc::new(stdlib::net::TcpConnect)),
            ("net.tcp_listen", Arc::new(stdlib::net::TcpListen)),
            ("net.tcp_accept", Arc::new(stdlib::net::TcpAccept)),
            ("net.tcp_read", Arc::new(stdlib::net::TcpRead)),
            ("net.tcp_write", Arc::new(stdlib::net::TcpWrite)),
            ("net.tcp_close", Arc::new(stdlib::net::TcpClose)),
            ("net.http_request_raw", Arc::new(stdlib::net::HttpRequest)),
            (
                "net.http_request_try",
                Arc::new(stdlib::net::HttpRequestTry),
            ),
            ("http_request_raw", Arc::new(stdlib::net::HttpRequest)),
            ("http_request_try", Arc::new(stdlib::net::HttpRequestTry)),
            (
                "async.waitgroup_new",
                Arc::new(stdlib::r#async::WaitGroupNew()),
            ),
            (
                "async.waitgroup_raw_add",
                Arc::new(stdlib::r#async::WaitGroupRawAdd()),
            ),
            (
                "async.waitgroup_raw_done",
                Arc::new(stdlib::r#async::WaitGroupRawDone()),
            ),
            (
                "async.waitgroup_join",
                Arc::new(stdlib::r#async::WaitGroupJoin()),
            ),
            (
                "async.waitgroup_wait",
                Arc::new(stdlib::r#async::WaitGroupWait()),
            ),
            (
                "async.waitgroup_count",
                Arc::new(stdlib::r#async::WaitGroupCount()),
            ),
            ("async.mutex_new", Arc::new(stdlib::r#async::MutexNew())),
            ("async.mutex_get", Arc::new(stdlib::r#async::MutexGet())),
            ("async.mutex_set", Arc::new(stdlib::r#async::MutexSet())),
            ("async.mutex_with", Arc::new(stdlib::r#async::MutexWith())),
            ("async.mutex_write", Arc::new(stdlib::r#async::MutexWrite())),
        ];

        lst.into_iter()
            .map(|(name, func)| (name.to_string(), RuntimeValue::NativeFunction(func)))
            .collect()
    }
}

impl From<VMLiteral> for RuntimeValue {
    fn from(value: VMLiteral) -> Self {
        match value {
            VMLiteral::Int(x) => Self::Int(x),
            VMLiteral::UInt(x) => Self::UInt(x),
            VMLiteral::Byte(x) => Self::Byte(x),
            VMLiteral::Float(x) => Self::Float(x),
            VMLiteral::Char(x) => Self::Char(x),
            VMLiteral::String(x) => Self::Str(x.into()),
            VMLiteral::Null => Self::Null,
            VMLiteral::Closure { label, captures: _ } => Self::Function {
                name: label.into(),
                captures: std::sync::Arc::new(Vec::new()),
            },
            VMLiteral::ExternFunction { .. } => Self::Null,
        }
    }
}
