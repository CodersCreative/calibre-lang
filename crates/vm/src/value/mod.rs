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
use calibre_parser::ast::{ObjectMap, ParserFfiInnerType, ParserInnerType, PotentialFfiDataType};
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

pub mod conversion;
pub mod operation;

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
    GeneratorSuspend(Box<RuntimeValue>),
}

impl RuntimeValue {
    #[inline]
    pub fn clone_for_arg(&self) -> RuntimeValue {
        match self {
            RuntimeValue::Aggregate(_, _)
            | RuntimeValue::List(_)
            | RuntimeValue::Enum(_, _, _)
            | RuntimeValue::Option(_)
            | RuntimeValue::Result(_)
            | RuntimeValue::Ptr(_) => self.clone(),
            RuntimeValue::Str(x) => RuntimeValue::Str(x.clone()),
            RuntimeValue::Channel(x) => RuntimeValue::Channel(x.clone()),
            RuntimeValue::WaitGroup(x) => RuntimeValue::WaitGroup(x.clone()),
            RuntimeValue::Mutex(x) => RuntimeValue::Mutex(x.clone()),
            RuntimeValue::MutexGuard(x) => RuntimeValue::MutexGuard(x.clone()),
            RuntimeValue::HashMap(x) => RuntimeValue::HashMap(x.clone()),
            RuntimeValue::HashSet(x) => RuntimeValue::HashSet(x.clone()),
            RuntimeValue::TcpStream(x) => RuntimeValue::TcpStream(x.clone()),
            RuntimeValue::TcpListener(x) => RuntimeValue::TcpListener(x.clone()),
            RuntimeValue::NativeFunction(x) => RuntimeValue::NativeFunction(x.clone()),
            RuntimeValue::ExternFunction(x) => RuntimeValue::ExternFunction(x.clone()),
            RuntimeValue::Function { name, captures } => RuntimeValue::Function {
                name: name.clone(),
                captures: captures.clone(),
            },
            RuntimeValue::Generator { type_name, state } => RuntimeValue::Generator {
                type_name: type_name.clone(),
                state: state.clone(),
            },
            RuntimeValue::GeneratorSuspend(value) => {
                RuntimeValue::GeneratorSuspend(Box::new(value.as_ref().clone_for_arg()))
            }
            RuntimeValue::Ref(x) => RuntimeValue::Ref(x.clone()),
            RuntimeValue::VarRef(x) => RuntimeValue::VarRef(*x),
            RuntimeValue::RegRef { frame, reg } => RuntimeValue::RegRef {
                frame: *frame,
                reg: *reg,
            },
            RuntimeValue::Range(a, b) => RuntimeValue::Range(*a, *b),
            RuntimeValue::Float(x) => RuntimeValue::Float(*x),
            RuntimeValue::Int(x) => RuntimeValue::Int(*x),
            RuntimeValue::UInt(x) => RuntimeValue::UInt(*x),
            RuntimeValue::Bool(x) => RuntimeValue::Bool(*x),
            RuntimeValue::Char(x) => RuntimeValue::Char(*x),
            RuntimeValue::Null => RuntimeValue::Null,
        }
    }
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
    pub parameters: Vec<PotentialFfiDataType>,
    pub return_type: PotentialFfiDataType,
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

fn resolve_function_by_name(vm: &VM, name: &str) -> Option<Arc<crate::conversion::VMFunction>> {
    if let Some(found) = vm.get_function(name) {
        return Some(found);
    }
    if let Some((prefix, _)) = name.split_once("->")
        && let Some(found) = vm
            .registry
            .functions
            .iter()
            .filter(|(k, _)| !vm.moved_functions.contains(*k))
            .find(|(k, _)| k.starts_with(prefix))
            .map(|(_, v)| v.clone())
    {
        return Some(found);
    }
    if let Some((_, short)) = name.rsplit_once(':') {
        let suffix = format!(":{}", short);
        let mut found = None;
        for (k, v) in vm.registry.functions.iter() {
            if vm.moved_functions.contains(k) {
                continue;
            }
            if k.ends_with(&suffix) {
                if found.is_some() {
                    return None;
                }
                found = Some(v.clone());
            }
        }
        return found;
    }
    None
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

        let Some(func) = resolve_function_by_name(&state.vm, state.function_name.as_str()) else {
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
        let lst = [
            ("true", RuntimeValue::Bool(true)),
            ("false", RuntimeValue::Bool(false)),
            ("none", RuntimeValue::Option(None)),
            ("INT_MIN", RuntimeValue::Int(i64::MIN)),
            ("INT_MAX", RuntimeValue::Int(i64::MAX)),
        ];

        let mut map = FxHashMap::default();

        for val in lst {
            map.insert(val.0.to_string(), val.1);
        }

        map
    }

    pub fn natives() -> FxHashMap<String, Self> {
        let lst: Vec<(&'static str, Arc<dyn NativeFunction>)> = vec![
            ("console_output", Arc::new(native::global::ConsoleOutput())),
            ("ok", Arc::new(native::global::OkFn())),
            ("err", Arc::new(native::global::ErrFn())),
            ("some", Arc::new(native::global::SomeFn())),
            ("len", Arc::new(native::global::Len())),
            ("trim", Arc::new(native::global::Trim())),
            ("str.split", Arc::new(stdlib::str::StrSplit())),
            ("str.contains", Arc::new(stdlib::str::StrContains())),
            ("str.starts_with", Arc::new(stdlib::str::StrStartsWith())),
            ("str.ends_with", Arc::new(stdlib::str::StrEndsWith())),
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

        let mut map = FxHashMap::default();

        for val in lst {
            map.insert(val.0.to_string(), RuntimeValue::NativeFunction(val.1));
        }

        map
    }
}

impl From<VMLiteral> for RuntimeValue {
    fn from(value: VMLiteral) -> Self {
        match value {
            VMLiteral::Int(x) => Self::Int(x),
            VMLiteral::UInt(x) => Self::UInt(x),
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

fn print_list_from_iter<I>(mut iter: I, open: char, close: char) -> String
where
    I: Iterator<Item = String>,
{
    let mut txt = String::new();
    txt.push(open);
    if let Some(first) = iter.next() {
        txt.push_str(&first);
        for s in iter {
            txt.push_str(", ");
            txt.push_str(&s);
        }
    }
    txt.push(close);
    txt
}

fn pretty_name(name: &str) -> &str {
    name.rsplitn(2, ':').next().unwrap_or(name)
}

impl RuntimeValue {
    pub fn display(&self, vm: &VM) -> String {
        match self {
            Self::Ref(x) => match vm.variables.get(x) {
                Some(value) => value.display(vm),
                None => RuntimeValue::Null.display(vm),
            },
            Self::VarRef(id) => vm
                .variables
                .get_by_id(*id)
                .unwrap_or(RuntimeValue::Null)
                .display(vm),
            Self::RegRef { frame, reg } => vm.get_reg_value_in_frame(*frame, *reg).display(vm),
            Self::Channel(_) => String::from("Channel"),
            Self::WaitGroup(_) => String::from("WaitGroup"),
            Self::Mutex(_) => String::from("Mutex"),
            Self::MutexGuard(_) => String::from("MutexGuard"),
            Self::HashMap(map) => {
                if let Ok(guard) = map.lock() {
                    let mut parts = Vec::new();
                    for (k, v) in guard.iter() {
                        parts.push(format!(
                            "{} : {}",
                            RuntimeValue::from(k.clone()).display(vm),
                            v.display(vm)
                        ));
                    }
                    format!("HashMap {{ {} }}", parts.join(", "))
                } else {
                    String::from("HashMap")
                }
            }
            Self::HashSet(set) => {
                if let Ok(guard) = set.lock() {
                    let mut parts = Vec::new();
                    for k in guard.iter() {
                        parts.push(RuntimeValue::from(k.clone()).display(vm));
                    }
                    format!("HashSet [{}]", parts.join(", "))
                } else {
                    String::from("HashSet")
                }
            }
            Self::TcpStream(_) => String::from("TcpStream"),
            Self::TcpListener(_) => String::from("TcpListener"),
            Self::List(x) => {
                let mut txt = String::new();
                txt.push('[');
                for (i, item) in x.0.iter().enumerate() {
                    if i > 0 {
                        txt.push_str(", ");
                    }
                    txt.push_str(&item.display(vm));
                }
                txt.push(']');
                txt
            }
            Self::Generator { type_name, .. } => format!("{} {{ ... }}", pretty_name(type_name)),
            Self::GeneratorSuspend(value) => format!("<gen-suspend {}>", value.display(vm)),
            Self::Option(Some(x)) => format!("Some : {}", x.display(vm)),
            Self::Result(Ok(x)) => format!("Ok : {}", x.display(vm)),
            Self::Result(Err(x)) => format!("Err : {}", x.display(vm)),
            Self::Enum(x, y, Some(z)) => format!("{}[{}] : {}", pretty_name(x), y, z.display(vm)),
            Self::Enum(x, y, _) => format!("{}[{}]", pretty_name(x), y),
            Self::Aggregate(x, data) => {
                if x.is_none() {
                    let iter = data.as_ref().0.0.iter().map(|x| x.1.display(vm));
                    print_list_from_iter(iter, '(', ')')
                } else if data.as_ref().0.is_empty() {
                    let name = x.as_deref().unwrap_or("tuple");
                    format!("{} {{}}", name)
                } else {
                    let name = pretty_name(x.as_deref().unwrap_or("tuple"));
                    let mut txt = format!("{} {{\n", name);

                    for val in data.as_ref().0.iter() {
                        let _ = write!(txt, "\t{} : {},\n", val.0, val.1.display(vm));
                    }

                    let trimmed = txt.trim_end_matches(',').trim_end();
                    let mut out = trimmed.to_string();
                    out.push_str("\n}");

                    out
                }
            }
            x => x.to_string(),
        }
    }
}

impl Display for RuntimeValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Null => write!(f, "null"),
            Self::Float(x) => write!(f, "{}f", x),
            Self::UInt(x) => write!(f, "{}u", x),
            Self::Ptr(x) => write!(f, "ptr -> {}", x),
            Self::Int(x) => write!(f, "{}", x),
            Self::Enum(x, y, Some(z)) => write!(f, "{}[{}] : {}", x, y, z.as_ref()),
            Self::Enum(x, y, _) => write!(f, "{}[{}]", x, y),
            Self::Range(from, to) => write!(f, "{}..{}", from, to),
            Self::Ref(x) => write!(f, "ref -> {}", x),
            Self::VarRef(id) => write!(f, "varref -> {}", id),
            Self::RegRef { frame, reg } => write!(f, "regref -> {}:{}", frame, reg),
            Self::Bool(x) => write!(f, "{}", if *x { "true" } else { "false" }),
            Self::Aggregate(x, data) => {
                if x.is_none() {
                    let mut txt = String::new();
                    txt.push('(');
                    for (i, val) in data.as_ref().0.0.iter().enumerate() {
                        if i > 0 {
                            txt.push_str(", ");
                        }
                        let _ = write!(txt, "{}", &val.1);
                    }
                    txt.push(')');
                    write!(f, "{}", txt)
                } else if data.as_ref().0.is_empty() {
                    let name = x.as_deref().unwrap_or("tuple");
                    write!(f, "{}{{}}", name)
                } else {
                    let name = x.as_deref().unwrap_or("tuple");
                    let mut txt = format!("{}{{\n", name);

                    for val in data.as_ref().0.iter() {
                        txt.push_str(&format!("\t{} : {},\n", val.0, val.1));
                    }

                    txt = txt.trim().trim_end_matches(",").trim().to_string();
                    txt.push('}');

                    write!(f, "{}", txt)
                }
            }
            Self::List(x) => {
                let mut txt = String::new();
                txt.push('[');
                for (i, val) in x.as_ref().0.iter().enumerate() {
                    if i > 0 {
                        txt.push_str(", ");
                    }
                    let _ = write!(txt, "{}", val);
                }
                txt.push(']');
                write!(f, "{}", txt)
            }
            Self::NativeFunction(x) => write!(f, "fn {} ...", x.name()),
            Self::ExternFunction(x) => write!(f, "extern fn {} ...", x.symbol),
            Self::Option(Some(x)) => write!(f, "Some : {}", x.as_ref()),
            Self::Option(_) => write!(f, "None"),
            Self::Result(Ok(x)) => write!(f, "Ok : {}", x.as_ref()),
            Self::Result(Err(x)) => write!(f, "Err : {}", x.as_ref()),
            Self::Channel(_) => write!(f, "Channel"),
            Self::WaitGroup(_) => write!(f, "WaitGroup"),
            Self::Mutex(_) => write!(f, "Mutex"),
            Self::MutexGuard(_) => write!(f, "MutexGuard"),
            Self::HashMap(_) => write!(f, "HashMap"),
            Self::HashSet(_) => write!(f, "HashSet"),
            Self::TcpStream(_) => write!(f, "TcpStream"),
            Self::TcpListener(_) => write!(f, "TcpListener"),
            Self::Str(x) => write!(f, "{}", x),
            Self::Char(x) => write!(f, "{}", x),
            Self::Function { name, captures: _ } => write!(f, "fn {} ...", name),
            Self::Generator { type_name, .. } => write!(f, "{}{{ ... }}", type_name),
            Self::GeneratorSuspend(value) => write!(f, "<gen-suspend {}>", value),
        }
    }
}

pub enum TerminateValue {
    None,
    Jump(BlockId),
    Return(RuntimeValue),
    Yield {
        block: BlockId,
        ip: usize,
        prev_block: Option<BlockId>,
        yielded: Option<RuntimeValue>,
    },
}

impl VM {
    pub fn move_saveable_into_runtime_var(&mut self, value: RuntimeValue) -> RuntimeValue {
        match value {
            RuntimeValue::Ref(pointer) => self
                .variables
                .get(&pointer)
                .cloned()
                .unwrap_or(RuntimeValue::Ref(pointer)),
            RuntimeValue::VarRef(id) => self
                .variables
                .get_by_id(id)
                .unwrap_or(RuntimeValue::VarRef(id)),
            RuntimeValue::RegRef { frame, reg } => self.get_reg_value_in_frame(frame, reg),
            other => other,
        }
    }

    pub fn copy_saveable_into_runtime_var(&self, value: RuntimeValue) -> RuntimeValue {
        match value {
            RuntimeValue::Ref(pointer) => self
                .variables
                .get(&pointer)
                .cloned()
                .unwrap_or(RuntimeValue::Ref(pointer)),
            RuntimeValue::VarRef(id) => self
                .variables
                .get_by_id(id)
                .unwrap_or(RuntimeValue::VarRef(id)),
            RuntimeValue::RegRef { frame, reg } => self.get_reg_value_in_frame(frame, reg),
            other => other,
        }
    }

    pub fn convert_runtime_var_into_saveable(&mut self, value: RuntimeValue) -> RuntimeValue {
        fn transform(env: &mut VM, val: RuntimeValue) -> RuntimeValue {
            match val {
                RuntimeValue::Ref(name) => {
                    if let Some(inner) = env.variables.get(&name).cloned() {
                        transform(env, inner)
                    } else {
                        RuntimeValue::Null
                    }
                }
                RuntimeValue::VarRef(id) => {
                    if let Some(inner) = env.variables.get_by_id(id) {
                        transform(env, inner)
                    } else {
                        RuntimeValue::Null
                    }
                }
                RuntimeValue::RegRef { frame, reg } => {
                    transform(env, env.get_reg_value_in_frame(frame, reg))
                }
                RuntimeValue::Aggregate(x, map) => {
                    let mut new_map = Vec::new();
                    for (k, v) in map.as_ref().0.0.iter().cloned() {
                        new_map.push((k, transform(env, v)));
                    }
                    RuntimeValue::Aggregate(x, Gc::new(GcMap(ObjectMap(new_map))))
                }
                RuntimeValue::List(data) => {
                    let mut lst = Vec::new();
                    for v in data.as_ref().0.iter().cloned() {
                        lst.push(transform(env, v));
                    }
                    RuntimeValue::List(Gc::new(GcVec(lst)))
                }
                RuntimeValue::Enum(x, y, Some(data)) => {
                    let inner_val = transform(env, data.as_ref().clone());
                    RuntimeValue::Enum(x, y, Some(Gc::new(inner_val)))
                }
                RuntimeValue::Option(Some(data)) => {
                    let inner_val = transform(env, data.as_ref().clone());
                    RuntimeValue::Option(Some(Gc::new(inner_val)))
                }
                RuntimeValue::Result(Ok(data)) => {
                    let inner_val = transform(env, data.as_ref().clone());
                    RuntimeValue::Result(Ok(Gc::new(inner_val)))
                }
                RuntimeValue::Result(Err(data)) => {
                    let inner_val = transform(env, data.as_ref().clone());
                    RuntimeValue::Result(Err(Gc::new(inner_val)))
                }
                RuntimeValue::HashMap(map) => {
                    let mut new_map = FxHashMap::default();
                    if let Ok(guard) = map.lock() {
                        for (k, v) in guard.iter() {
                            new_map.insert(k.clone(), transform(env, v.clone()));
                        }
                    }
                    RuntimeValue::HashMap(Arc::new(Mutex::new(new_map)))
                }
                RuntimeValue::HashSet(set) => {
                    let mut new_set = rustc_hash::FxHashSet::default();
                    if let Ok(guard) = set.lock() {
                        for k in guard.iter() {
                            new_set.insert(k.clone());
                        }
                    }
                    RuntimeValue::HashSet(Arc::new(Mutex::new(new_set)))
                }
                RuntimeValue::Generator { type_name, state } => RuntimeValue::Generator {
                    type_name,
                    state,
                },
                RuntimeValue::GeneratorSuspend(value) => {
                    RuntimeValue::GeneratorSuspend(Box::new(transform(env, *value)))
                }
                other => other,
            }
        }

        transform(self, value)
    }
}

impl ExternFunction {
    fn resolve_value(env: &mut VM, value: RuntimeValue) -> RuntimeValue {
        match value {
            RuntimeValue::Ref(name) => env
                .variables
                .get(&name)
                .cloned()
                .unwrap_or(RuntimeValue::Ref(name)),
            RuntimeValue::VarRef(id) => env
                .variables
                .get_by_id(id)
                .unwrap_or(RuntimeValue::VarRef(id)),
            RuntimeValue::RegRef { frame, reg } => env.get_reg_value_in_frame(frame, reg),
            RuntimeValue::Ptr(id) => env
                .ptr_heap
                .get(&id)
                .cloned()
                .unwrap_or(RuntimeValue::Ptr(id)),
            RuntimeValue::MutexGuard(guard) => guard.get_clone(),
            other => other,
        }
    }
    fn type_to_libffi_type(typ: &PotentialFfiDataType) -> Type {
        match typ {
            PotentialFfiDataType::Normal(x) => match x.data_type {
                ParserInnerType::Int => Type::i64(),
                ParserInnerType::UInt => Type::u64(),
                ParserInnerType::Float => Type::f64(),
                ParserInnerType::Bool => Type::u8(),
                ParserInnerType::Char => Type::u8(),
                ParserInnerType::Str => Type::pointer(),
                ParserInnerType::Ptr(_) => Type::pointer(),
                ParserInnerType::Null => Type::void(),
                _ => Type::pointer(),
            },
            PotentialFfiDataType::Ffi(x) => match x.data_type {
                ParserFfiInnerType::F32 => Type::f32(),
                ParserFfiInnerType::F64 | ParserFfiInnerType::LongDouble => Type::f64(),
                ParserFfiInnerType::U8 | ParserFfiInnerType::UChar => Type::u8(),
                ParserFfiInnerType::I8 | ParserFfiInnerType::SChar => Type::i8(),
                ParserFfiInnerType::U16 | ParserFfiInnerType::UShort => Type::u16(),
                ParserFfiInnerType::I16 | ParserFfiInnerType::Short => Type::i16(),
                ParserFfiInnerType::U32 | ParserFfiInnerType::UInt => Type::u32(),
                ParserFfiInnerType::I32 | ParserFfiInnerType::Int => Type::i32(),
                ParserFfiInnerType::U64
                | ParserFfiInnerType::ULong
                | ParserFfiInnerType::ULongLong => Type::u64(),
                ParserFfiInnerType::I64
                | ParserFfiInnerType::Long
                | ParserFfiInnerType::LongLong => Type::i64(),
                ParserFfiInnerType::USize => Type::u64(),
                ParserFfiInnerType::ISize => Type::i64(),
            },
        }
    }

    fn push_arg(ffi_args: &mut Vec<FfiArg>, libffi_args: &mut Vec<Arg>, arg: FfiArg) {
        ffi_args.push(arg);
        let Some(last) = ffi_args.last() else {
            return;
        };
        match last {
            FfiArg::U8(x) => libffi_args.push(Arg::new(x)),
            FfiArg::I8(x) => libffi_args.push(Arg::new(x)),
            FfiArg::U16(x) => libffi_args.push(Arg::new(x)),
            FfiArg::I16(x) => libffi_args.push(Arg::new(x)),
            FfiArg::U32(x) => libffi_args.push(Arg::new(x)),
            FfiArg::I32(x) => libffi_args.push(Arg::new(x)),
            FfiArg::U64(x) => libffi_args.push(Arg::new(x)),
            FfiArg::I64(x) => libffi_args.push(Arg::new(x)),
            FfiArg::F32(x) => libffi_args.push(Arg::new(x)),
            FfiArg::F64(x) => libffi_args.push(Arg::new(x)),
            FfiArg::Bool(x) => libffi_args.push(Arg::new(x)),
            FfiArg::Char(x) => libffi_args.push(Arg::new(x)),
            FfiArg::Ptr(x) => libffi_args.push(Arg::new(x)),
            FfiArg::CString { value, ptr } => {
                let _ = value.as_bytes();
                libffi_args.push(Arg::new(ptr));
            }
            FfiArg::Bytes { value, ptr } => {
                let _ = value.as_slice();
                libffi_args.push(Arg::new(ptr));
            }
            FfiArg::Struct { backing } => {
                let _ = backing.as_slice();
                if let Some(first) = backing.first() {
                    libffi_args.push(Arg::new(first));
                }
            }
        }
    }

    fn struct_field_to_bytes(value: RuntimeValue) -> Option<(Vec<u8>, Type, usize, usize)> {
        match value {
            RuntimeValue::UInt(x) => {
                let size = std::mem::size_of::<u64>();
                Some((x.to_le_bytes().to_vec(), Type::u64(), size, size))
            }
            RuntimeValue::Int(x) => {
                let size = std::mem::size_of::<i64>();
                Some((x.to_le_bytes().to_vec(), Type::i64(), size, size))
            }
            RuntimeValue::Float(x) => {
                let size = std::mem::size_of::<f64>();
                Some((x.to_le_bytes().to_vec(), Type::f64(), size, size))
            }
            RuntimeValue::Bool(x) => Some((vec![x as u8], Type::u8(), 1, 1)),
            RuntimeValue::Char(x) => Some((vec![x as u8], Type::u8(), 1, 1)),
            RuntimeValue::Ptr(id) => {
                let size = std::mem::size_of::<*const c_void>();
                let bytes = (id as usize).to_le_bytes().to_vec();
                Some((bytes, Type::pointer(), size, size))
            }
            _ => None,
        }
    }

    fn pack_struct_arg(env: &mut VM, value: RuntimeValue) -> Option<(Vec<u8>, Type)> {
        match value {
            RuntimeValue::Aggregate(_, data) => {
                let field_count = data.as_ref().0.0.len();
                let mut bytes = Vec::with_capacity(field_count.saturating_mul(8));
                let mut fields = Vec::with_capacity(field_count);
                let mut offset = 0usize;
                let mut max_align = 1usize;

                for (_, field) in data.as_ref().0.0.iter() {
                    let resolved = Self::resolve_value(env, field.clone());
                    let (field_bytes, field_ty, size, align) =
                        Self::struct_field_to_bytes(resolved)?;
                    max_align = max_align.max(align);
                    let padding = (align - (offset % align)) % align;
                    if padding > 0 {
                        bytes.extend(std::iter::repeat(0u8).take(padding));
                        offset += padding;
                    }
                    bytes.extend_from_slice(&field_bytes);
                    offset += size;
                    fields.push(field_ty);
                }

                let tail_padding = (max_align - (offset % max_align)) % max_align;
                if tail_padding > 0 {
                    bytes.extend(std::iter::repeat(0u8).take(tail_padding));
                }

                Some((bytes, Type::structure(fields)))
            }
            _ => None,
        }
    }

    fn pack_aggregate_bytes(value: &RuntimeValue) -> Option<Vec<u8>> {
        fn push_number(bytes: &mut Vec<u8>, value: &RuntimeValue) -> bool {
            match value {
                RuntimeValue::UInt(x) => {
                    bytes.extend_from_slice(&x.to_le_bytes());
                    true
                }
                RuntimeValue::Int(x) => {
                    bytes.extend_from_slice(&x.to_le_bytes());
                    true
                }
                RuntimeValue::Float(x) => {
                    bytes.extend_from_slice(&x.to_le_bytes());
                    true
                }
                RuntimeValue::Bool(x) => {
                    bytes.push(*x as u8);
                    true
                }
                RuntimeValue::Char(x) => {
                    bytes.push(*x as u8);
                    true
                }
                _ => false,
            }
        }

        match value {
            RuntimeValue::Aggregate(_, data) => {
                let field_count = data.as_ref().0.0.len();
                let mut bytes = Vec::with_capacity(field_count.saturating_mul(8));
                for (_, field) in data.as_ref().0.0.iter() {
                    if !push_number(&mut bytes, field) {
                        return None;
                    }
                }
                Some(bytes)
            }
            _ => None,
        }
    }

    pub fn call(
        &self,
        env: &mut VM,
        args: Vec<RuntimeValue>,
    ) -> Result<RuntimeValue, RuntimeError> {
        let mut arg_types = Vec::with_capacity(self.parameters.len());
        let mut ffi_args: Vec<FfiArg> = Vec::with_capacity(self.parameters.len());
        let mut libffi_args: Vec<Arg> = Vec::with_capacity(self.parameters.len());

        if args.len() != self.parameters.len() {
            return Err(RuntimeError::InvalidFunctionCall);
        }

        for (param, value) in self.parameters.iter().zip(args.into_iter()) {
            match param {
                PotentialFfiDataType::Normal(x) => {
                    match (Self::resolve_value(env, value), &x.data_type) {
                        (RuntimeValue::Str(x), ParserInnerType::Str) => {
                            arg_types.push(Type::pointer());
                            let value = CString::new(x.as_str())
                                .map_err(|_| RuntimeError::InvalidFunctionCall)?;
                            let ptr = value.as_ptr() as *const c_void;
                            Self::push_arg(
                                &mut ffi_args,
                                &mut libffi_args,
                                FfiArg::CString { value, ptr },
                            );
                        }
                        (RuntimeValue::UInt(x), ParserInnerType::UInt) => {
                            arg_types.push(Type::u64());
                            Self::push_arg(&mut ffi_args, &mut libffi_args, FfiArg::U64(x));
                        }
                        (RuntimeValue::UInt(x), ParserInnerType::Int) => {
                            arg_types.push(Type::i64());
                            Self::push_arg(&mut ffi_args, &mut libffi_args, FfiArg::I64(x as i64));
                        }
                        (RuntimeValue::UInt(x), ParserInnerType::Float) => {
                            arg_types.push(Type::f64());
                            Self::push_arg(&mut ffi_args, &mut libffi_args, FfiArg::F64(x as f64));
                        }
                        (RuntimeValue::Int(x), ParserInnerType::Int) => {
                            arg_types.push(Type::i64());
                            Self::push_arg(&mut ffi_args, &mut libffi_args, FfiArg::I64(x));
                        }
                        (RuntimeValue::Int(x), ParserInnerType::Float) => {
                            arg_types.push(Type::f64());
                            Self::push_arg(&mut ffi_args, &mut libffi_args, FfiArg::F64(x as f64));
                        }
                        (RuntimeValue::Int(x), ParserInnerType::UInt) => {
                            arg_types.push(Type::u64());
                            Self::push_arg(&mut ffi_args, &mut libffi_args, FfiArg::U64(x as u64));
                        }
                        (RuntimeValue::Float(x), ParserInnerType::Float) => {
                            arg_types.push(Type::f64());
                            Self::push_arg(&mut ffi_args, &mut libffi_args, FfiArg::F64(x));
                        }
                        (RuntimeValue::Float(x), ParserInnerType::UInt) => {
                            arg_types.push(Type::u64());
                            Self::push_arg(&mut ffi_args, &mut libffi_args, FfiArg::U64(x as u64));
                        }
                        (RuntimeValue::Float(x), ParserInnerType::Int) => {
                            arg_types.push(Type::i64());
                            Self::push_arg(&mut ffi_args, &mut libffi_args, FfiArg::I64(x as i64));
                        }
                        (RuntimeValue::Char(x), ParserInnerType::Char) => {
                            arg_types.push(Type::u8());
                            Self::push_arg(&mut ffi_args, &mut libffi_args, FfiArg::Char(x as u8));
                        }
                        (RuntimeValue::Bool(x), ParserInnerType::Bool) => {
                            arg_types.push(Type::u8());
                            Self::push_arg(&mut ffi_args, &mut libffi_args, FfiArg::Bool(x as u8));
                        }
                        (RuntimeValue::Bool(x), ParserInnerType::UInt) => {
                            arg_types.push(Type::u64());
                            Self::push_arg(&mut ffi_args, &mut libffi_args, FfiArg::U64(x as u64));
                        }
                        (RuntimeValue::Bool(x), ParserInnerType::Int) => {
                            arg_types.push(Type::i64());
                            Self::push_arg(&mut ffi_args, &mut libffi_args, FfiArg::I64(x as i64));
                        }
                        (RuntimeValue::UInt(x), ParserInnerType::Ptr(_)) => {
                            arg_types.push(Type::pointer());
                            Self::push_arg(
                                &mut ffi_args,
                                &mut libffi_args,
                                FfiArg::Ptr(x as *const c_void),
                            );
                        }
                        (RuntimeValue::Int(x), ParserInnerType::Ptr(_)) => {
                            arg_types.push(Type::pointer());
                            Self::push_arg(
                                &mut ffi_args,
                                &mut libffi_args,
                                FfiArg::Ptr(x as usize as *const c_void),
                            );
                        }
                        (RuntimeValue::Str(x), ParserInnerType::Ptr(_)) => {
                            arg_types.push(Type::pointer());
                            let value = CString::new(x.as_str())
                                .map_err(|_| RuntimeError::InvalidFunctionCall)?;
                            let ptr = value.as_ptr() as *const c_void;
                            Self::push_arg(
                                &mut ffi_args,
                                &mut libffi_args,
                                FfiArg::CString { value, ptr },
                            );
                        }
                        (RuntimeValue::Ptr(id), ParserInnerType::Ptr(_)) => {
                            arg_types.push(Type::pointer());
                            Self::push_arg(
                                &mut ffi_args,
                                &mut libffi_args,
                                FfiArg::Ptr(id as *const c_void),
                            );
                        }
                        (RuntimeValue::List(list), ParserInnerType::Ptr(_)) => {
                            arg_types.push(Type::pointer());
                            let mut bytes = Vec::new();
                            for item in list.as_ref().0.iter() {
                                let item = Self::resolve_value(env, item.clone());
                                match item {
                                    RuntimeValue::UInt(x) => bytes.push(x as u8),
                                    RuntimeValue::Int(x) => bytes.push(x as u8),
                                    RuntimeValue::Float(x) => bytes.push(x as u8),
                                    RuntimeValue::Bool(x) => bytes.push(x as u8),
                                    RuntimeValue::Char(x) => bytes.push(x as u8),
                                    _ => {
                                        return Err(RuntimeError::InvalidFunctionCall);
                                    }
                                }
                            }
                            let ptr = bytes.as_ptr() as *const c_void;
                            Self::push_arg(
                                &mut ffi_args,
                                &mut libffi_args,
                                FfiArg::Bytes { value: bytes, ptr },
                            );
                        }
                        (value, ParserInnerType::Ptr(_)) => {
                            arg_types.push(Type::pointer());
                            if let Some(bytes) =
                                Self::pack_aggregate_bytes(&Self::resolve_value(env, value))
                            {
                                let ptr = bytes.as_ptr() as *const c_void;
                                Self::push_arg(
                                    &mut ffi_args,
                                    &mut libffi_args,
                                    FfiArg::Bytes { value: bytes, ptr },
                                );
                            } else {
                                return Err(RuntimeError::InvalidFunctionCall);
                            }
                        }
                        (value, ParserInnerType::Struct(_))
                        | (value, ParserInnerType::StructWithGenerics { .. }) => {
                            let resolved = Self::resolve_value(env, value);
                            let (bytes, ty) = match Self::pack_struct_arg(env, resolved.clone()) {
                                Some(data) => data,
                                None => {
                                    return Err(RuntimeError::Ffi(format!(
                                        "unsupported struct arg {:?}",
                                        resolved
                                    )));
                                }
                            };
                            arg_types.push(ty.clone());
                            let mut backing = vec![0u64; (bytes.len() + 7) / 8];
                            if !bytes.is_empty() {
                                let raw = backing.as_mut_ptr() as *mut u8;
                                let raw_len = backing.len() * std::mem::size_of::<u64>();
                                let dst = unsafe { std::slice::from_raw_parts_mut(raw, raw_len) };
                                dst[..bytes.len()].copy_from_slice(&bytes);
                            }
                            Self::push_arg(
                                &mut ffi_args,
                                &mut libffi_args,
                                FfiArg::Struct { backing },
                            );
                        }
                        _ => return Err(RuntimeError::InvalidFunctionCall),
                    }
                }
                PotentialFfiDataType::Ffi(x) => {
                    arg_types.push(Self::type_to_libffi_type(param));
                    let value = Self::resolve_value(env, value);
                    let arg = match (&x.data_type, value) {
                        (
                            ParserFfiInnerType::U8 | ParserFfiInnerType::UChar,
                            RuntimeValue::UInt(x),
                        ) => FfiArg::U8(x as u8),
                        (
                            ParserFfiInnerType::U8 | ParserFfiInnerType::UChar,
                            RuntimeValue::Int(x),
                        ) => FfiArg::U8(x as u8),
                        (
                            ParserFfiInnerType::U8 | ParserFfiInnerType::UChar,
                            RuntimeValue::Float(x),
                        ) => FfiArg::U8(x as u8),
                        (
                            ParserFfiInnerType::I8 | ParserFfiInnerType::SChar,
                            RuntimeValue::Int(x),
                        ) => FfiArg::I8(x as i8),
                        (
                            ParserFfiInnerType::I8 | ParserFfiInnerType::SChar,
                            RuntimeValue::UInt(x),
                        ) => FfiArg::I8(x as i8),
                        (
                            ParserFfiInnerType::I8 | ParserFfiInnerType::SChar,
                            RuntimeValue::Float(x),
                        ) => FfiArg::I8(x as i8),
                        (
                            ParserFfiInnerType::U16 | ParserFfiInnerType::UShort,
                            RuntimeValue::UInt(x),
                        ) => FfiArg::U16(x as u16),
                        (
                            ParserFfiInnerType::U16 | ParserFfiInnerType::UShort,
                            RuntimeValue::Int(x),
                        ) => FfiArg::U16(x as u16),
                        (
                            ParserFfiInnerType::U16 | ParserFfiInnerType::UShort,
                            RuntimeValue::Float(x),
                        ) => FfiArg::U16(x as u16),
                        (
                            ParserFfiInnerType::I16 | ParserFfiInnerType::Short,
                            RuntimeValue::Int(x),
                        ) => FfiArg::I16(x as i16),
                        (
                            ParserFfiInnerType::I16 | ParserFfiInnerType::Short,
                            RuntimeValue::UInt(x),
                        ) => FfiArg::I16(x as i16),
                        (
                            ParserFfiInnerType::I16 | ParserFfiInnerType::Short,
                            RuntimeValue::Float(x),
                        ) => FfiArg::I16(x as i16),
                        (
                            ParserFfiInnerType::U32 | ParserFfiInnerType::UInt,
                            RuntimeValue::UInt(x),
                        ) => FfiArg::U32(x as u32),
                        (
                            ParserFfiInnerType::U32 | ParserFfiInnerType::UInt,
                            RuntimeValue::Int(x),
                        ) => FfiArg::U32(x as u32),
                        (
                            ParserFfiInnerType::U32 | ParserFfiInnerType::UInt,
                            RuntimeValue::Float(x),
                        ) => FfiArg::U32(x as u32),
                        (
                            ParserFfiInnerType::I32 | ParserFfiInnerType::Int,
                            RuntimeValue::Int(x),
                        ) => FfiArg::I32(x as i32),
                        (
                            ParserFfiInnerType::I32 | ParserFfiInnerType::Int,
                            RuntimeValue::UInt(x),
                        ) => FfiArg::I32(x as i32),
                        (
                            ParserFfiInnerType::I32 | ParserFfiInnerType::Int,
                            RuntimeValue::Float(x),
                        ) => FfiArg::I32(x as i32),
                        (
                            ParserFfiInnerType::U64
                            | ParserFfiInnerType::ULong
                            | ParserFfiInnerType::ULongLong,
                            RuntimeValue::UInt(x),
                        ) => FfiArg::U64(x as u64),
                        (
                            ParserFfiInnerType::U64
                            | ParserFfiInnerType::ULong
                            | ParserFfiInnerType::ULongLong,
                            RuntimeValue::Int(x),
                        ) => FfiArg::U64(x as u64),
                        (
                            ParserFfiInnerType::U64
                            | ParserFfiInnerType::ULong
                            | ParserFfiInnerType::ULongLong,
                            RuntimeValue::Float(x),
                        ) => FfiArg::U64(x as u64),
                        (
                            ParserFfiInnerType::I64
                            | ParserFfiInnerType::Long
                            | ParserFfiInnerType::LongLong,
                            RuntimeValue::Int(x),
                        ) => FfiArg::I64(x as i64),
                        (
                            ParserFfiInnerType::I64
                            | ParserFfiInnerType::Long
                            | ParserFfiInnerType::LongLong,
                            RuntimeValue::UInt(x),
                        ) => FfiArg::I64(x as i64),
                        (
                            ParserFfiInnerType::I64
                            | ParserFfiInnerType::Long
                            | ParserFfiInnerType::LongLong,
                            RuntimeValue::Float(x),
                        ) => FfiArg::I64(x as i64),
                        (ParserFfiInnerType::USize, RuntimeValue::UInt(x)) => FfiArg::U64(x as u64),
                        (ParserFfiInnerType::USize, RuntimeValue::Int(x)) => FfiArg::U64(x as u64),
                        (ParserFfiInnerType::USize, RuntimeValue::Float(x)) => {
                            FfiArg::U64(x as u64)
                        }
                        (ParserFfiInnerType::ISize, RuntimeValue::Int(x)) => FfiArg::I64(x as i64),
                        (ParserFfiInnerType::ISize, RuntimeValue::UInt(x)) => FfiArg::I64(x as i64),
                        (ParserFfiInnerType::ISize, RuntimeValue::Float(x)) => {
                            FfiArg::I64(x as i64)
                        }
                        (ParserFfiInnerType::F32, RuntimeValue::Float(x)) => FfiArg::F32(x as f32),
                        (ParserFfiInnerType::F32, RuntimeValue::Int(x)) => FfiArg::F32(x as f32),
                        (ParserFfiInnerType::F32, RuntimeValue::UInt(x)) => FfiArg::F32(x as f32),
                        (
                            ParserFfiInnerType::F64 | ParserFfiInnerType::LongDouble,
                            RuntimeValue::Float(x),
                        ) => FfiArg::F64(x as f64),
                        (
                            ParserFfiInnerType::F64 | ParserFfiInnerType::LongDouble,
                            RuntimeValue::Int(x),
                        ) => FfiArg::F64(x as f64),
                        (
                            ParserFfiInnerType::F64 | ParserFfiInnerType::LongDouble,
                            RuntimeValue::UInt(x),
                        ) => FfiArg::F64(x as f64),
                        (
                            ParserFfiInnerType::U8 | ParserFfiInnerType::UChar,
                            RuntimeValue::Bool(x),
                        ) => FfiArg::U8(x as u8),
                        (
                            ParserFfiInnerType::I8 | ParserFfiInnerType::SChar,
                            RuntimeValue::Bool(x),
                        ) => FfiArg::I8(x as i8),
                        (
                            ParserFfiInnerType::U8 | ParserFfiInnerType::UChar,
                            RuntimeValue::Char(x),
                        ) => FfiArg::U8(x as u8),
                        (
                            ParserFfiInnerType::I8 | ParserFfiInnerType::SChar,
                            RuntimeValue::Char(x),
                        ) => FfiArg::I8(x as i8),
                        _ => return Err(RuntimeError::InvalidFunctionCall),
                    };
                    Self::push_arg(&mut ffi_args, &mut libffi_args, arg);
                }
            }
        }

        let cif = Cif::new(arg_types, Self::type_to_libffi_type(&self.return_type));

        let symbol = unsafe {
            self.handle
                .get::<*const c_void>(self.symbol.as_bytes())
                .map_err(|_| RuntimeError::InvalidFunctionCall)?
        };

        let code = CodePtr::from_ptr(*symbol as *mut c_void);

        unsafe {
            match &self.return_type {
                PotentialFfiDataType::Normal(x) => match x.data_type {
                    ParserInnerType::Float => {
                        Ok(RuntimeValue::Float(cif.call(code, &mut libffi_args)))
                    }
                    ParserInnerType::UInt => {
                        Ok(RuntimeValue::UInt(cif.call(code, &mut libffi_args)))
                    }
                    ParserInnerType::Int => Ok(RuntimeValue::Int(cif.call(code, &mut libffi_args))),
                    ParserInnerType::Bool => {
                        let res: u8 = cif.call(code, &mut libffi_args);
                        Ok(RuntimeValue::Bool(res != 0))
                    }
                    ParserInnerType::Null => {
                        let _: () = cif.call(code, &mut libffi_args);
                        Ok(RuntimeValue::Null)
                    }
                    ParserInnerType::Char => {
                        let res: u8 = cif.call(code, &mut libffi_args);
                        Ok(RuntimeValue::Char(res as char))
                    }
                    ParserInnerType::Str => {
                        let res: *const c_char = cif.call(code, &mut libffi_args);
                        if res.is_null() {
                            Ok(RuntimeValue::Str(std::sync::Arc::new(String::new())))
                        } else {
                            let c_str = CStr::from_ptr(res);
                            Ok(RuntimeValue::Str(std::sync::Arc::new(
                                c_str.to_string_lossy().to_string(),
                            )))
                        }
                    }
                    ParserInnerType::Ptr(_) => {
                        let res: *const c_void = cif.call(code, &mut libffi_args);
                        Ok(RuntimeValue::UInt(res as u64))
                    }
                    _ => Err(RuntimeError::InvalidFunctionCall),
                },
                PotentialFfiDataType::Ffi(x) => match x.data_type {
                    ParserFfiInnerType::F32 => {
                        let res: f32 = cif.call(code, &mut libffi_args);
                        Ok(RuntimeValue::Float(res as f64))
                    }
                    ParserFfiInnerType::F64 | ParserFfiInnerType::LongDouble => {
                        Ok(RuntimeValue::Float(cif.call(code, &mut libffi_args)))
                    }
                    ParserFfiInnerType::U8
                    | ParserFfiInnerType::U16
                    | ParserFfiInnerType::U32
                    | ParserFfiInnerType::U64
                    | ParserFfiInnerType::USize
                    | ParserFfiInnerType::UInt
                    | ParserFfiInnerType::UShort
                    | ParserFfiInnerType::ULong
                    | ParserFfiInnerType::ULongLong
                    | ParserFfiInnerType::UChar => {
                        let res: u64 = cif.call(code, &mut libffi_args);
                        Ok(RuntimeValue::UInt(res))
                    }
                    ParserFfiInnerType::I8
                    | ParserFfiInnerType::I16
                    | ParserFfiInnerType::I32
                    | ParserFfiInnerType::I64
                    | ParserFfiInnerType::ISize
                    | ParserFfiInnerType::Int
                    | ParserFfiInnerType::Short
                    | ParserFfiInnerType::Long
                    | ParserFfiInnerType::LongLong
                    | ParserFfiInnerType::SChar => {
                        let res: i64 = cif.call(code, &mut libffi_args);
                        Ok(RuntimeValue::Int(res))
                    }
                },
            }
        }
    }
}
