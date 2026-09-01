use crate::{
    VM,
    conversion::{Reg, VMLiteral},
    error::RuntimeError,
    native::{
        self, NativeFunction,
        stdlib::{self, generator::GeneratorState},
    },
};
use astro_float::{BigFloat, RoundingMode};
use calibre_lir::ast::BlockId;
use calibre_parser::ast::{
    ObjectMap,
    ffi::ParserFfiInnerType,
    types::{ParserDataType, ParserInnerType},
};
use dumpster::sync::Gc;
use dumpster::{TraceWith, Visitor};

#[cfg(feature = "native")]
use libffi::middle::{Arg, Cif, CodePtr, Type};
#[cfg(feature = "native")]
use libloading::Library;

use rustc_hash::{FxHashMap, FxHashSet};
use ustr::{Ustr, UstrMap};

#[cfg(feature = "native")]
use std::os::raw::c_char;
#[cfg(feature = "native")]
use std::os::raw::c_void;

use std::any::Any;
#[cfg(feature = "native")]
use std::ffi::{CStr, CString};
use std::{
    cell::UnsafeCell,
    collections::VecDeque,
    fmt::{Debug, Display, Write},
    sync::{
        Arc, OnceLock,
        atomic::{AtomicBool, AtomicIsize, Ordering},
    },
};
use wasm_sync::{Condvar, Mutex};

mod bridge;
pub mod conversion;
mod display;
pub mod embedded;

#[cfg(feature = "native")]
mod ffi;

pub mod operation;
pub use bridge::TerminateValue;

pub const BIG_PRECISION: usize = 128;
pub const BIG_ROUNDING: RoundingMode = RoundingMode::ToEven;

#[derive(Debug, Clone)]
pub struct GcVec(pub Vec<RuntimeValue>);

#[derive(Debug, Clone)]
pub struct GcMap(pub ObjectMap<RuntimeValue>);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum HashKey {
    Int(i64),
    UInt(u64),
    Bool(bool),
    Char(char),
    Str(Ustr),
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
            other => Err(RuntimeError::UnexpectedType(Box::new(other))),
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

#[derive(Debug, Default)]
pub struct ChannelInner {
    pub queue: Mutex<VecDeque<RuntimeValue>>,
    pub closed: AtomicBool,
    pub cvar: Condvar,
}

#[derive(Debug, Default)]
pub struct WaitGroupInner {
    pub count: AtomicIsize,
    pub mutex: Mutex<()>,
    pub cvar: Condvar,
    pub joined: Mutex<Vec<Arc<WaitGroupInner>>>,
}

impl WaitGroupInner {
    pub fn done(&self) {
        let remaining = self.count.fetch_sub(1, Ordering::AcqRel) - 1;
        if remaining <= 0 {
            self.cvar.notify_all();
        }
    }

    pub fn wait(&self) -> Result<(), RuntimeError> {
        let mut guard = self.mutex.lock().unwrap();

        while self.count.load(Ordering::Acquire) > 0 {
            guard = self.cvar.wait(guard).unwrap();
        }

        drop(guard);

        let joined = self.joined.lock().unwrap();

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
        let mut guard = self.mutex.lock().unwrap();
        while self.locked.load(Ordering::Acquire) {
            guard = self.cvar.wait(guard).unwrap();
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

pub type HostInner = dyn Any + Send;
pub type Host = Arc<Mutex<HostInner>>;
pub type RuntimeHashMap = Arc<Mutex<FxHashMap<HashKey, RuntimeValue>>>;
pub type RuntimeHashSet = Arc<Mutex<FxHashSet<HashKey>>>;

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
                if let Ok(guard) = map.try_lock() {
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

#[cfg(feature = "native")]
#[derive(Debug, Clone)]
pub struct ExternFunction {
    pub abi: Ustr,
    pub library: Ustr,
    pub symbol: Ustr,
    pub parameters: Vec<ParserDataType>,
    pub return_type: ParserDataType,
    pub handle: Arc<Library>,
}

#[cfg(feature = "native")]
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

    pub fn constants() -> &'static FxHashMap<String, Self> {
        static CONSTANTS: OnceLock<FxHashMap<String, RuntimeValue>> = OnceLock::new();

        CONSTANTS.get_or_init(|| {
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
        })
    }

    pub fn natives() -> &'static FxHashMap<String, Self> {
        static NATIVES: OnceLock<FxHashMap<String, RuntimeValue>> = OnceLock::new();

        NATIVES.get_or_init(|| {
            let lst: Vec<(&str, Arc<dyn NativeFunction>)> = vec![
                ("console_output", Arc::new(native::global::ConsoleOutput)),
                ("console_input", Arc::new(native::global::ConsoleInput)),
                ("ok", Arc::new(native::global::OkFn)),
                ("err", Arc::new(native::global::ErrFn)),
                ("some", Arc::new(native::global::SomeFn)),
                ("repr", Arc::new(native::global::Repr)),
                ("display", Arc::new(native::global::Display)),
                ("len", Arc::new(native::global::Len)),
                ("trim", Arc::new(native::global::Trim)),
                ("wait", Arc::new(native::global::Wait)),
                ("random.rand", Arc::new(stdlib::random::Rand)),
                ("random.seed", Arc::new(stdlib::random::Seed)),
                ("str.split", Arc::new(stdlib::str::StrSplit)),
                ("str.contains", Arc::new(stdlib::str::StrContains)),
                ("str.starts_with", Arc::new(stdlib::str::StrStartsWith)),
                ("str.ends_with", Arc::new(stdlib::str::StrEndsWith)),
                ("str.char_lowercase", Arc::new(stdlib::str::CharLowercase)),
                ("str.char_uppercase", Arc::new(stdlib::str::CharUppercase)),
                ("env.get", Arc::new(stdlib::env::EnvGet)),
                ("env.var", Arc::new(stdlib::env::EnvVar)),
                ("env.set_var", Arc::new(stdlib::env::EnvSetVar)),
                ("env.remove_var", Arc::new(stdlib::env::EnvRemoveVar)),
                ("env.vars", Arc::new(stdlib::env::EnvVars)),
                #[cfg(feature = "native")]
                ("fs.dir_create", Arc::new(stdlib::fs::FsDirCreate)),
                #[cfg(feature = "native")]
                ("fs.dir_create_all", Arc::new(stdlib::fs::FsDirCreateAll)),
                #[cfg(feature = "native")]
                ("fs.dir_remove", Arc::new(stdlib::fs::FsDirRemove)),
                #[cfg(feature = "native")]
                ("fs.dir_remove_all", Arc::new(stdlib::fs::FsDirRemoveAll)),
                #[cfg(feature = "native")]
                ("fs.path_new", Arc::new(stdlib::fs::FsPathNew)),
                #[cfg(feature = "native")]
                ("fs.path_as_str", Arc::new(stdlib::fs::FsPathAsStr)),
                #[cfg(feature = "native")]
                ("fs.path_exists", Arc::new(stdlib::fs::FsPathExists)),
                #[cfg(feature = "native")]
                ("fs.path_is_file", Arc::new(stdlib::fs::FsPathIsFile)),
                #[cfg(feature = "native")]
                ("fs.path_is_dir", Arc::new(stdlib::fs::FsPathIsDir)),
                #[cfg(feature = "native")]
                (
                    "fs.path_canonicalize",
                    Arc::new(stdlib::fs::FsPathCanonicalize),
                ),
                #[cfg(feature = "native")]
                ("fs.path_parent", Arc::new(stdlib::fs::FsPathParent)),
                #[cfg(feature = "native")]
                ("fs.path_file_name", Arc::new(stdlib::fs::FsPathFileName)),
                #[cfg(feature = "native")]
                ("fs.path_extension", Arc::new(stdlib::fs::FsPathExtension)),
                #[cfg(feature = "native")]
                ("fs.path_stem", Arc::new(stdlib::fs::FsPathStem)),
                #[cfg(feature = "native")]
                ("fs.path_join", Arc::new(stdlib::fs::FsPathJoin)),
                #[cfg(feature = "native")]
                (
                    "fs.path_with_extension",
                    Arc::new(stdlib::fs::FsPathWithExtension),
                ),
                #[cfg(feature = "native")]
                (
                    "fs.path_with_file_name",
                    Arc::new(stdlib::fs::FsPathWithFileName),
                ),
                #[cfg(feature = "native")]
                ("fs.path_read_dir", Arc::new(stdlib::fs::FsPathReadDir)),
                #[cfg(feature = "native")]
                ("fs.direntry_path", Arc::new(stdlib::fs::FsDirEntryPath)),
                #[cfg(feature = "native")]
                (
                    "fs.direntry_file_name",
                    Arc::new(stdlib::fs::FsDirEntryFileName),
                ),
                #[cfg(feature = "native")]
                (
                    "fs.direntry_file_type",
                    Arc::new(stdlib::fs::FsDirEntryFileType),
                ),
                #[cfg(feature = "native")]
                (
                    "fs.direntry_metadata",
                    Arc::new(stdlib::fs::FsDirEntryMetadata),
                ),
                #[cfg(feature = "native")]
                (
                    "fs.filetype_is_file",
                    Arc::new(stdlib::fs::FsFileTypeIsFile),
                ),
                #[cfg(feature = "native")]
                ("fs.filetype_is_dir", Arc::new(stdlib::fs::FsFileTypeIsDir)),
                #[cfg(feature = "native")]
                (
                    "fs.filetype_is_symlink",
                    Arc::new(stdlib::fs::FsFileTypeIsSymlink),
                ),
                #[cfg(feature = "native")]
                (
                    "fs.metadata_is_file",
                    Arc::new(stdlib::fs::FsMetadataIsFile),
                ),
                #[cfg(feature = "native")]
                ("fs.metadata_is_dir", Arc::new(stdlib::fs::FsMetadataIsDir)),
                #[cfg(feature = "native")]
                ("fs.metadata_len", Arc::new(stdlib::fs::FsMetadataLen)),
                #[cfg(feature = "native")]
                (
                    "fs.metadata_modified",
                    Arc::new(stdlib::fs::FsMetadataModified),
                ),
                #[cfg(feature = "native")]
                (
                    "fs.metadata_created",
                    Arc::new(stdlib::fs::FsMetadataCreated),
                ),
                #[cfg(feature = "native")]
                (
                    "fs.metadata_accessed",
                    Arc::new(stdlib::fs::FsMetadataAccessed),
                ),
                #[cfg(feature = "native")]
                (
                    "fs.metadata_is_readonly",
                    Arc::new(stdlib::fs::FsMetadataIsReadOnly),
                ),
                #[cfg(feature = "native")]
                ("fs.file_open", Arc::new(stdlib::fs::FsFileOpen)),
                #[cfg(feature = "native")]
                ("fs.file_close", Arc::new(stdlib::fs::FsFileClose)),
                #[cfg(feature = "native")]
                ("fs.file_write", Arc::new(stdlib::fs::FsFileWrite)),
                #[cfg(feature = "native")]
                ("fs.file_write_line", Arc::new(stdlib::fs::FsFileWriteLine)),
                #[cfg(feature = "native")]
                ("fs.file_read_all", Arc::new(stdlib::fs::FsFileReadAll)),
                #[cfg(feature = "native")]
                ("fs.file_flush", Arc::new(stdlib::fs::FsFileFlush)),
                #[cfg(feature = "native")]
                ("discriminant", Arc::new(native::global::DiscriminantFn)),
                ("tuple", Arc::new(native::global::TupleFn)),
                ("panic", Arc::new(native::global::PanicFn)),
                ("assert", Arc::new(native::global::AssertFn)),
                (
                    "gen_suspend",
                    Arc::new(stdlib::generator::GeneratorSuspendFn),
                ),
                ("min_or_zero", Arc::new(native::global::MinOrZero)),
                ("async.channel_new", Arc::new(stdlib::r#async::ChannelNew)),
                ("async.channel_send", Arc::new(stdlib::r#async::ChannelSend)),
                ("async.channel_get", Arc::new(stdlib::r#async::ChannelGet)),
                (
                    "async.channel_try_get",
                    Arc::new(stdlib::r#async::ChannelTryGet),
                ),
                (
                    "async.channel_try_send",
                    Arc::new(stdlib::r#async::ChannelTrySend),
                ),
                (
                    "async.channel_close",
                    Arc::new(stdlib::r#async::ChannelClose),
                ),
                (
                    "async.channel_closed",
                    Arc::new(stdlib::r#async::ChannelClosed),
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
                #[cfg(feature = "native")]
                ("net.tcp_connect", Arc::new(stdlib::net::TcpConnect)),
                #[cfg(feature = "native")]
                ("net.tcp_listen", Arc::new(stdlib::net::TcpListen)),
                #[cfg(feature = "native")]
                ("net.tcp_accept", Arc::new(stdlib::net::TcpAccept)),
                #[cfg(feature = "native")]
                ("net.tcp_read", Arc::new(stdlib::net::TcpRead)),
                #[cfg(feature = "native")]
                ("net.tcp_write", Arc::new(stdlib::net::TcpWrite)),
                #[cfg(feature = "native")]
                ("net.tcp_close", Arc::new(stdlib::net::TcpClose)),
                #[cfg(feature = "native")]
                ("net.http_request_raw", Arc::new(stdlib::net::HttpRequest)),
                #[cfg(feature = "native")]
                (
                    "net.http_request_try",
                    Arc::new(stdlib::net::HttpRequestTry),
                ),
                #[cfg(feature = "native")]
                ("http_request_raw", Arc::new(stdlib::net::HttpRequest)),
                #[cfg(feature = "native")]
                ("http_request_try", Arc::new(stdlib::net::HttpRequestTry)),
                (
                    "async.waitgroup_new",
                    Arc::new(stdlib::r#async::WaitGroupNew),
                ),
                (
                    "async.waitgroup_raw_add",
                    Arc::new(stdlib::r#async::WaitGroupRawAdd),
                ),
                (
                    "async.waitgroup_raw_done",
                    Arc::new(stdlib::r#async::WaitGroupRawDone),
                ),
                (
                    "async.waitgroup_join",
                    Arc::new(stdlib::r#async::WaitGroupJoin),
                ),
                (
                    "async.waitgroup_wait",
                    Arc::new(stdlib::r#async::WaitGroupWait),
                ),
                (
                    "async.waitgroup_count",
                    Arc::new(stdlib::r#async::WaitGroupCount),
                ),
                ("async.mutex_new", Arc::new(stdlib::r#async::MutexNew)),
                ("async.mutex_get", Arc::new(stdlib::r#async::MutexGet)),
                ("async.mutex_set", Arc::new(stdlib::r#async::MutexSet)),
                ("async.mutex_with", Arc::new(stdlib::r#async::MutexWith)),
                ("async.mutex_write", Arc::new(stdlib::r#async::MutexWrite)),
            ];

            lst.into_iter()
                .map(|(name, func)| (name.to_string(), RuntimeValue::NativeFunction(func)))
                .collect()
        })
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
