use std::sync::OnceLock;

use crate::{
    Span,
    ast::{
        RefMutability,
        types::{ParserDataType, ParserInnerType},
    },
};
use rustc_hash::FxHashMap;

impl ParserDataType {
    fn native_type(inner: ParserInnerType) -> ParserDataType {
        ParserDataType::new(Span::default(), inner)
    }

    pub fn constants() -> &'static FxHashMap<String, Self> {
        static CONSTANTS: OnceLock<FxHashMap<String, ParserDataType>> = OnceLock::new();

        CONSTANTS.get_or_init(|| {
            let lst = [
                ("true", ParserInnerType::Bool),
                ("false", ParserInnerType::Bool),
                (
                    "none",
                    ParserInnerType::Option(Box::new(ParserDataType::native_type(
                        ParserInnerType::Dynamic,
                    ))),
                ),
                ("INT_MIN", ParserInnerType::Int),
                ("INT_MAX", ParserInnerType::Int),
            ];

            lst.into_iter()
                .map(|(name, inner)| (name.to_string(), Self::native_type(inner)))
                .collect()
        })
    }

    pub fn natives() -> &'static FxHashMap<String, ParserDataType> {
        static NATIVES: OnceLock<FxHashMap<String, ParserDataType>> = OnceLock::new();

        NATIVES.get_or_init(|| {
            let lst: Vec<(&str, Vec<ParserInnerType>, ParserInnerType)> = vec![
                (
                    "console_output",
                    vec![ParserInnerType::List(Box::new(Self::native_type(
                        ParserInnerType::Dynamic,
                    )))],
                    ParserInnerType::Null,
                ),
                ("console_input", Vec::new(), ParserInnerType::Str),
                (
                    "ok",
                    vec![ParserInnerType::Dynamic],
                    ParserInnerType::Result {
                        err: Box::new(Self::native_type(ParserInnerType::Dynamic)),
                        ok: Box::new(Self::native_type(ParserInnerType::Dynamic)),
                    },
                ),
                (
                    "err",
                    vec![ParserInnerType::Dynamic],
                    ParserInnerType::Result {
                        err: Box::new(Self::native_type(ParserInnerType::Dynamic)),
                        ok: Box::new(Self::native_type(ParserInnerType::Dynamic)),
                    },
                ),
                (
                    "some",
                    vec![ParserInnerType::Dynamic],
                    ParserInnerType::Option(Box::new(Self::native_type(ParserInnerType::Dynamic))),
                ),
                (
                    "min_or_zero",
                    vec![ParserInnerType::Dynamic],
                    ParserInnerType::Int,
                ),
                #[cfg(feature = "native")]
                ("libc.get_c_errno", vec![], ParserInnerType::Int),
                #[cfg(feature = "native")]
                (
                    "libc.set_c_errno",
                    vec![ParserInnerType::Int],
                    ParserInnerType::Null,
                ),
                #[cfg(feature = "native")]
                ("libc.get_c_errno_description", vec![], ParserInnerType::Str),
                (
                    "random.rand",
                    vec![ParserInnerType::Range],
                    ParserInnerType::Float,
                ),
                (
                    "random.seed",
                    vec![ParserInnerType::Int],
                    ParserInnerType::Null,
                ),
                ("len", vec![ParserInnerType::Dynamic], ParserInnerType::Int),
                ("wait", vec![ParserInnerType::Int], ParserInnerType::Null),
                (
                    "panic",
                    vec![ParserInnerType::List(Box::new(Self::native_type(
                        ParserInnerType::Dynamic,
                    )))],
                    ParserInnerType::Null,
                ),
                ("repr", vec![ParserInnerType::Dynamic], ParserInnerType::Str),
                (
                    "display",
                    vec![ParserInnerType::Dynamic],
                    ParserInnerType::Str,
                ),
                ("assert", vec![ParserInnerType::Bool], ParserInnerType::Null),
                ("gen_suspend", vec![], ParserInnerType::Dynamic),
                (
                    "tuple",
                    vec![ParserInnerType::List(Box::new(Self::native_type(
                        ParserInnerType::Dynamic,
                    )))],
                    ParserInnerType::Dynamic,
                ),
                ("trim", vec![ParserInnerType::Str], ParserInnerType::Str),
                (
                    "str.split",
                    vec![ParserInnerType::Str, ParserInnerType::Str],
                    ParserInnerType::List(Box::new(Self::native_type(ParserInnerType::Str))),
                ),
                (
                    "str.contains",
                    vec![ParserInnerType::Str, ParserInnerType::Str],
                    ParserInnerType::Bool,
                ),
                (
                    "str.starts_with",
                    vec![ParserInnerType::Str, ParserInnerType::Str],
                    ParserInnerType::Bool,
                ),
                (
                    "str.ends_with",
                    vec![ParserInnerType::Str, ParserInnerType::Str],
                    ParserInnerType::Bool,
                ),
                (
                    "str.char_lowercase",
                    vec![ParserInnerType::Char],
                    ParserInnerType::Char,
                ),
                (
                    "str.char_uppercase",
                    vec![ParserInnerType::Char],
                    ParserInnerType::Char,
                ),
                (
                    "env.get",
                    vec![ParserInnerType::UInt],
                    ParserInnerType::Option(Box::new(Self::native_type(ParserInnerType::Str))),
                ),
                (
                    "env.var",
                    vec![ParserInnerType::Str],
                    ParserInnerType::Option(Box::new(Self::native_type(ParserInnerType::Str))),
                ),
                (
                    "env.set_var",
                    vec![ParserInnerType::Str, ParserInnerType::Str],
                    ParserInnerType::Null,
                ),
                (
                    "env.remove_var",
                    vec![ParserInnerType::Str],
                    ParserInnerType::Null,
                ),
                (
                    "env.vars",
                    vec![],
                    ParserInnerType::List(Box::new(Self::native_type(ParserInnerType::Str))),
                ),
                // TODO Finish off params
                #[cfg(feature = "native")]
                (
                    "fs.dir_create",
                    vec![],
                    ParserInnerType::Result {
                        err: Box::new(Self::native_type(ParserInnerType::Str)),
                        ok: Box::new(Self::native_type(ParserInnerType::Null)),
                    },
                ),
                #[cfg(feature = "native")]
                (
                    "fs.dir_create_all",
                    vec![],
                    ParserInnerType::Result {
                        err: Box::new(Self::native_type(ParserInnerType::Str)),
                        ok: Box::new(Self::native_type(ParserInnerType::Null)),
                    },
                ),
                #[cfg(feature = "native")]
                (
                    "fs.dir_remove",
                    vec![],
                    ParserInnerType::Result {
                        err: Box::new(Self::native_type(ParserInnerType::Str)),
                        ok: Box::new(Self::native_type(ParserInnerType::Null)),
                    },
                ),
                #[cfg(feature = "native")]
                (
                    "fs.dir_remove_all",
                    vec![],
                    ParserInnerType::Result {
                        err: Box::new(Self::native_type(ParserInnerType::Str)),
                        ok: Box::new(Self::native_type(ParserInnerType::Null)),
                    },
                ),
                #[cfg(feature = "native")]
                ("fs.path_new", vec![], ParserInnerType::Dynamic),
                #[cfg(feature = "native")]
                ("fs.path_as_str", vec![], ParserInnerType::Str),
                #[cfg(feature = "native")]
                ("fs.path_exists", vec![], ParserInnerType::Bool),
                #[cfg(feature = "native")]
                ("fs.path_is_file", vec![], ParserInnerType::Bool),
                #[cfg(feature = "native")]
                ("fs.path_is_dir", vec![], ParserInnerType::Bool),
                #[cfg(feature = "native")]
                (
                    "fs.path_canonicalize",
                    vec![],
                    ParserInnerType::Result {
                        err: Box::new(Self::native_type(ParserInnerType::Str)),
                        ok: Box::new(Self::native_type(ParserInnerType::Dynamic)),
                    },
                ),
                #[cfg(feature = "native")]
                (
                    "fs.path_parent",
                    vec![],
                    ParserInnerType::Option(Box::new(Self::native_type(ParserInnerType::Dynamic))),
                ),
                #[cfg(feature = "native")]
                (
                    "fs.path_file_name",
                    vec![],
                    ParserInnerType::Option(Box::new(Self::native_type(ParserInnerType::Str))),
                ),
                #[cfg(feature = "native")]
                (
                    "fs.path_extension",
                    vec![],
                    ParserInnerType::Option(Box::new(Self::native_type(ParserInnerType::Str))),
                ),
                #[cfg(feature = "native")]
                (
                    "fs.path_stem",
                    vec![],
                    ParserInnerType::Option(Box::new(Self::native_type(ParserInnerType::Str))),
                ),
                #[cfg(feature = "native")]
                ("fs.path_join", vec![], ParserInnerType::Null),
                #[cfg(feature = "native")]
                ("fs.path_with_extension", vec![], ParserInnerType::Dynamic),
                #[cfg(feature = "native")]
                ("fs.path_with_file_name", vec![], ParserInnerType::Dynamic),
                #[cfg(feature = "native")]
                (
                    "fs.path_read_dir",
                    vec![],
                    ParserInnerType::Result {
                        err: Box::new(Self::native_type(ParserInnerType::Str)),
                        ok: Box::new(Self::native_type(ParserInnerType::List(Box::new(
                            Self::native_type(ParserInnerType::Dynamic),
                        )))),
                    },
                ),
                #[cfg(feature = "native")]
                ("fs.direntry_path", vec![], ParserInnerType::Dynamic),
                #[cfg(feature = "native")]
                ("fs.direntry_file_name", vec![], ParserInnerType::Str),
                #[cfg(feature = "native")]
                (
                    "fs.direntry_file_type",
                    vec![],
                    ParserInnerType::Result {
                        err: Box::new(Self::native_type(ParserInnerType::Str)),
                        ok: Box::new(Self::native_type(ParserInnerType::Dynamic)),
                    },
                ),
                #[cfg(feature = "native")]
                (
                    "fs.direntry_metadata",
                    vec![],
                    ParserInnerType::Result {
                        err: Box::new(Self::native_type(ParserInnerType::Str)),
                        ok: Box::new(Self::native_type(ParserInnerType::Dynamic)),
                    },
                ),
                #[cfg(feature = "native")]
                ("fs.filetype_is_file", vec![], ParserInnerType::Bool),
                #[cfg(feature = "native")]
                ("fs.filetype_is_dir", vec![], ParserInnerType::Bool),
                #[cfg(feature = "native")]
                ("fs.filetype_is_symlink", vec![], ParserInnerType::Bool),
                #[cfg(feature = "native")]
                ("fs.metadata_is_file", vec![], ParserInnerType::Bool),
                #[cfg(feature = "native")]
                ("fs.metadata_is_dir", vec![], ParserInnerType::Bool),
                #[cfg(feature = "native")]
                ("fs.metadata_len", vec![], ParserInnerType::UInt),
                #[cfg(feature = "native")]
                (
                    "fs.metadata_modified",
                    vec![],
                    ParserInnerType::Result {
                        err: Box::new(Self::native_type(ParserInnerType::Str)),
                        ok: Box::new(Self::native_type(ParserInnerType::UInt)),
                    },
                ),
                #[cfg(feature = "native")]
                (
                    "fs.metadata_created",
                    vec![],
                    ParserInnerType::Result {
                        err: Box::new(Self::native_type(ParserInnerType::Str)),
                        ok: Box::new(Self::native_type(ParserInnerType::UInt)),
                    },
                ),
                #[cfg(feature = "native")]
                (
                    "fs.metadata_accessed",
                    vec![],
                    ParserInnerType::Result {
                        err: Box::new(Self::native_type(ParserInnerType::Str)),
                        ok: Box::new(Self::native_type(ParserInnerType::UInt)),
                    },
                ),
                #[cfg(feature = "native")]
                ("fs.metadata_is_readonly", vec![], ParserInnerType::Bool),
                #[cfg(feature = "native")]
                (
                    "fs.file_open",
                    vec![],
                    ParserInnerType::Result {
                        err: Box::new(Self::native_type(ParserInnerType::Str)),
                        ok: Box::new(Self::native_type(ParserInnerType::Dynamic)),
                    },
                ),
                #[cfg(feature = "native")]
                (
                    "fs.file_close",
                    vec![],
                    ParserInnerType::Result {
                        err: Box::new(Self::native_type(ParserInnerType::Str)),
                        ok: Box::new(Self::native_type(ParserInnerType::Null)),
                    },
                ),
                #[cfg(feature = "native")]
                (
                    "fs.file_write",
                    vec![],
                    ParserInnerType::Result {
                        err: Box::new(Self::native_type(ParserInnerType::Str)),
                        ok: Box::new(Self::native_type(ParserInnerType::Null)),
                    },
                ),
                #[cfg(feature = "native")]
                (
                    "fs.file_write_line",
                    vec![],
                    ParserInnerType::Result {
                        err: Box::new(Self::native_type(ParserInnerType::Str)),
                        ok: Box::new(Self::native_type(ParserInnerType::Null)),
                    },
                ),
                #[cfg(feature = "native")]
                (
                    "fs.file_read_all",
                    vec![],
                    ParserInnerType::Result {
                        err: Box::new(Self::native_type(ParserInnerType::Str)),
                        ok: Box::new(Self::native_type(ParserInnerType::Str)),
                    },
                ),
                #[cfg(feature = "native")]
                (
                    "fs.file_flush",
                    vec![],
                    ParserInnerType::Result {
                        err: Box::new(Self::native_type(ParserInnerType::Str)),
                        ok: Box::new(Self::native_type(ParserInnerType::Null)),
                    },
                ),
                ("discriminant", vec![], ParserInnerType::Int),
                ("async.channel_new", vec![], ParserInnerType::Dynamic),
                ("async.channel_send", vec![], ParserInnerType::Null),
                (
                    "async.channel_get",
                    vec![],
                    ParserInnerType::Option(Box::new(Self::native_type(ParserInnerType::Dynamic))),
                ),
                (
                    "async.channel_try_get",
                    vec![],
                    ParserInnerType::Option(Box::new(Self::native_type(ParserInnerType::Dynamic))),
                ),
                ("async.channel_try_send", vec![], ParserInnerType::Bool),
                ("async.channel_close", vec![], ParserInnerType::Null),
                ("async.channel_closed", vec![], ParserInnerType::Bool),
                ("async.waitgroup_new", vec![], ParserInnerType::Dynamic),
                ("async.waitgroup_raw_add", vec![], ParserInnerType::Null),
                ("async.waitgroup_raw_done", vec![], ParserInnerType::Null),
                ("async.waitgroup_join", vec![], ParserInnerType::Null),
                ("async.waitgroup_wait", vec![], ParserInnerType::Null),
                ("async.waitgroup_count", vec![], ParserInnerType::Int),
                ("async.mutex_new", vec![], ParserInnerType::Null),
                ("async.mutex_get", vec![], ParserInnerType::Null),
                ("async.mutex_set", vec![], ParserInnerType::Null),
                ("async.mutex_with", vec![], ParserInnerType::Null),
                (
                    "async.mutex_write",
                    vec![],
                    ParserInnerType::Ref(
                        Box::new(Self::native_type(ParserInnerType::Dynamic)),
                        RefMutability::MutRef,
                    ),
                ),
                ("crypto.sha256", vec![], ParserInnerType::Str),
                ("crypto.sha512", vec![], ParserInnerType::Str),
                ("crypto.blake3", vec![], ParserInnerType::Str),
                ("regex.is_match", vec![], ParserInnerType::Bool),
                (
                    "regex.find",
                    vec![],
                    ParserInnerType::Option(Box::new(Self::native_type(ParserInnerType::Str))),
                ),
                ("regex.replace", vec![], ParserInnerType::Str),
                (
                    "process.raw_exec",
                    vec![],
                    ParserInnerType::Result {
                        err: Box::new(Self::native_type(ParserInnerType::Str)),
                        ok: Box::new(Self::native_type(ParserInnerType::Struct(String::from(
                            "ProcessResult",
                        )))),
                    },
                ),
                (
                    "collections.hashmap_new",
                    vec![],
                    ParserInnerType::StructWithGenerics {
                        identifier: String::from("HashMap"),
                        generic_types: vec![
                            Self::native_type(ParserInnerType::Dynamic),
                            Self::native_type(ParserInnerType::Dynamic),
                        ],
                    },
                ),
                ("collections.hashmap_set", vec![], ParserInnerType::Null),
                (
                    "collections.hashmap_get",
                    vec![],
                    ParserInnerType::Option(Box::new(Self::native_type(ParserInnerType::Dynamic))),
                ),
                (
                    "collections.hashmap_remove",
                    vec![],
                    ParserInnerType::Option(Box::new(Self::native_type(ParserInnerType::Dynamic))),
                ),
                (
                    "collections.hashmap_contains",
                    vec![],
                    ParserInnerType::Bool,
                ),
                ("collections.hashmap_len", vec![], ParserInnerType::Int),
                (
                    "collections.hashmap_keys",
                    vec![],
                    ParserInnerType::List(Box::new(Self::native_type(ParserInnerType::Dynamic))),
                ),
                (
                    "collections.hashmap_values",
                    vec![],
                    ParserInnerType::List(Box::new(Self::native_type(ParserInnerType::Dynamic))),
                ),
                (
                    "collections.hashmap_entries",
                    vec![],
                    ParserInnerType::List(Box::new(Self::native_type(ParserInnerType::Dynamic))),
                ),
                ("collections.hashmap_clear", vec![], ParserInnerType::Null),
                (
                    "collections.hashset_new",
                    vec![],
                    ParserInnerType::StructWithGenerics {
                        identifier: String::from("HashSet"),
                        generic_types: vec![Self::native_type(ParserInnerType::Dynamic)],
                    },
                ),
                ("collections.hashset_add", vec![], ParserInnerType::Bool),
                ("collections.hashset_remove", vec![], ParserInnerType::Bool),
                (
                    "collections.hashset_contains",
                    vec![],
                    ParserInnerType::Bool,
                ),
                ("collections.hashset_len", vec![], ParserInnerType::Int),
                (
                    "collections.hashset_values",
                    vec![],
                    ParserInnerType::List(Box::new(Self::native_type(ParserInnerType::Dynamic))),
                ),
                ("collections.hashset_clear", vec![], ParserInnerType::Null),
                (
                    "list.sort_by",
                    vec![],
                    ParserInnerType::List(Box::new(Self::native_type(ParserInnerType::Dynamic))),
                ),
                (
                    "list.binary_search_by",
                    vec![],
                    ParserInnerType::Option(Box::new(Self::native_type(ParserInnerType::Int))),
                ),
                (
                    "list.raw_remove",
                    vec![],
                    ParserInnerType::Option(Box::new(Self::native_type(ParserInnerType::Dynamic))),
                ),
                #[cfg(feature = "native")]
                ("net.http_request_raw", vec![], ParserInnerType::Str),
                #[cfg(feature = "native")]
                (
                    "net.http_request_try",
                    vec![],
                    ParserInnerType::Result {
                        err: Box::new(Self::native_type(ParserInnerType::Str)),
                        ok: Box::new(Self::native_type(ParserInnerType::Str)),
                    },
                ),
                #[cfg(feature = "native")]
                (
                    "http_request_try",
                    vec![],
                    ParserInnerType::Result {
                        err: Box::new(Self::native_type(ParserInnerType::Str)),
                        ok: Box::new(Self::native_type(ParserInnerType::Str)),
                    },
                ),
                #[cfg(feature = "native")]
                (
                    "net.tcp_connect",
                    vec![],
                    ParserInnerType::Struct(String::from("TcpStream")),
                ),
                #[cfg(feature = "native")]
                (
                    "net.tcp_listen",
                    vec![],
                    ParserInnerType::Struct(String::from("TcpListener")),
                ),
                #[cfg(feature = "native")]
                (
                    "net.tcp_accept",
                    vec![],
                    ParserInnerType::Struct(String::from("TcpStream")),
                ),
                #[cfg(feature = "native")]
                ("net.tcp_read", vec![], ParserInnerType::Str),
                #[cfg(feature = "native")]
                ("net.tcp_write", vec![], ParserInnerType::Int),
                #[cfg(feature = "native")]
                ("net.tcp_close", vec![], ParserInnerType::Null),
            ];

            lst.into_iter()
                .map(|(name, parameters, return_type)| {
                    (
                        name.to_string(),
                        Self::native_type(ParserInnerType::NativeFunction {
                            parameters: parameters.into_iter().map(Self::native_type).collect(),
                            return_type: Box::new(Self::native_type(return_type)),
                        }),
                    )
                })
                .collect()
        })
    }
}
