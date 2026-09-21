use crate::{
    native::{self, NativeFunction, stdlib},
    value::RuntimeValue,
};
use rustc_hash::FxHashMap;
use std::sync::{Arc, OnceLock};

impl RuntimeValue {
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
