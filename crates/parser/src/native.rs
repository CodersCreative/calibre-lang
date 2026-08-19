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

    pub fn constants() -> FxHashMap<String, Self> {
        let lst = [
            ("true", ParserInnerType::Bool),
            ("false", ParserInnerType::Bool),
            (
                "none",
                ParserInnerType::Option(Box::new(Self::native_type(ParserInnerType::Dynamic))),
            ),
            ("INT_MIN", ParserInnerType::Int),
            ("INT_MAX", ParserInnerType::Int),
        ];

        let mut map = FxHashMap::with_capacity_and_hasher(lst.len(), Default::default());

        for (name, inner) in lst {
            map.insert(name.to_string(), Self::native_type(inner));
        }

        map
    }

    pub fn natives() -> FxHashMap<String, ParserDataType> {
        let lst = [
            ("console_output", ParserInnerType::Null),
            (
                "ok",
                ParserInnerType::Result {
                    err: Box::new(Self::native_type(ParserInnerType::Dynamic)),
                    ok: Box::new(Self::native_type(ParserInnerType::Dynamic)),
                },
            ),
            (
                "err",
                ParserInnerType::Result {
                    err: Box::new(Self::native_type(ParserInnerType::Dynamic)),
                    ok: Box::new(Self::native_type(ParserInnerType::Dynamic)),
                },
            ),
            (
                "some",
                ParserInnerType::Option(Box::new(Self::native_type(ParserInnerType::Dynamic))),
            ),
            ("min_or_zero", ParserInnerType::Int),
            ("libc.get_c_errno", ParserInnerType::Int),
            ("libc.set_c_errno", ParserInnerType::Null),
            ("libc.get_c_errno_description", ParserInnerType::Str),
            ("len", ParserInnerType::Int),
            ("panic", ParserInnerType::Null),
            ("repr", ParserInnerType::Str),
            ("display", ParserInnerType::Str),
            ("assert", ParserInnerType::Null),
            ("gen_suspend", ParserInnerType::Dynamic),
            ("tuple", ParserInnerType::Dynamic),
            ("trim", ParserInnerType::Str),
            (
                "str.split",
                ParserInnerType::List(Box::new(Self::native_type(ParserInnerType::Str))),
            ),
            ("str.contains", ParserInnerType::Bool),
            ("str.starts_with", ParserInnerType::Bool),
            ("str.ends_with", ParserInnerType::Bool),
            ("str.char_lowercase", ParserInnerType::Char),
            ("str.char_uppercase", ParserInnerType::Char),
            (
                "env.get",
                ParserInnerType::Option(Box::new(Self::native_type(ParserInnerType::Str))),
            ),
            (
                "env.var",
                ParserInnerType::Option(Box::new(Self::native_type(ParserInnerType::Str))),
            ),
            ("env.set_var", ParserInnerType::Null),
            ("env.remove_var", ParserInnerType::Null),
            (
                "env.vars",
                ParserInnerType::List(Box::new(Self::native_type(ParserInnerType::Str))),
            ),
            (
                "fs.dir_create",
                ParserInnerType::Result {
                    err: Box::new(Self::native_type(ParserInnerType::Str)),
                    ok: Box::new(Self::native_type(ParserInnerType::Null)),
                },
            ),
            (
                "fs.dir_create_all",
                ParserInnerType::Result {
                    err: Box::new(Self::native_type(ParserInnerType::Str)),
                    ok: Box::new(Self::native_type(ParserInnerType::Null)),
                },
            ),
            (
                "fs.dir_remove",
                ParserInnerType::Result {
                    err: Box::new(Self::native_type(ParserInnerType::Str)),
                    ok: Box::new(Self::native_type(ParserInnerType::Null)),
                },
            ),
            (
                "fs.dir_remove_all",
                ParserInnerType::Result {
                    err: Box::new(Self::native_type(ParserInnerType::Str)),
                    ok: Box::new(Self::native_type(ParserInnerType::Null)),
                },
            ),
            ("fs.path_new", ParserInnerType::Dynamic),
            ("fs.path_as_str", ParserInnerType::Str),
            ("fs.path_exists", ParserInnerType::Bool),
            ("fs.path_is_file", ParserInnerType::Bool),
            ("fs.path_is_dir", ParserInnerType::Bool),
            (
                "fs.path_canonicalize",
                ParserInnerType::Result {
                    err: Box::new(Self::native_type(ParserInnerType::Str)),
                    ok: Box::new(Self::native_type(ParserInnerType::Dynamic)),
                },
            ),
            (
                "fs.path_parent",
                ParserInnerType::Option(Box::new(Self::native_type(ParserInnerType::Dynamic))),
            ),
            (
                "fs.path_file_name",
                ParserInnerType::Option(Box::new(Self::native_type(ParserInnerType::Str))),
            ),
            (
                "fs.path_extension",
                ParserInnerType::Option(Box::new(Self::native_type(ParserInnerType::Str))),
            ),
            (
                "fs.path_stem",
                ParserInnerType::Option(Box::new(Self::native_type(ParserInnerType::Str))),
            ),
            ("fs.path_join", ParserInnerType::Null),
            ("fs.path_with_extension", ParserInnerType::Dynamic),
            ("fs.path_with_file_name", ParserInnerType::Dynamic),
            (
                "fs.path_read_dir",
                ParserInnerType::Result {
                    err: Box::new(Self::native_type(ParserInnerType::Str)),
                    ok: Box::new(Self::native_type(ParserInnerType::List(Box::new(
                        Self::native_type(ParserInnerType::Dynamic),
                    )))),
                },
            ),
            ("fs.direntry_path", ParserInnerType::Dynamic),
            ("fs.direntry_file_name", ParserInnerType::Str),
            (
                "fs.direntry_file_type",
                ParserInnerType::Result {
                    err: Box::new(Self::native_type(ParserInnerType::Str)),
                    ok: Box::new(Self::native_type(ParserInnerType::Dynamic)),
                },
            ),
            (
                "fs.direntry_metadata",
                ParserInnerType::Result {
                    err: Box::new(Self::native_type(ParserInnerType::Str)),
                    ok: Box::new(Self::native_type(ParserInnerType::Dynamic)),
                },
            ),
            ("fs.filetype_is_file", ParserInnerType::Bool),
            ("fs.filetype_is_dir", ParserInnerType::Bool),
            ("fs.filetype_is_symlink", ParserInnerType::Bool),
            ("fs.metadata_is_file", ParserInnerType::Bool),
            ("fs.metadata_is_dir", ParserInnerType::Bool),
            ("fs.metadata_len", ParserInnerType::UInt),
            (
                "fs.metadata_modified",
                ParserInnerType::Result {
                    err: Box::new(Self::native_type(ParserInnerType::Str)),
                    ok: Box::new(Self::native_type(ParserInnerType::UInt)),
                },
            ),
            (
                "fs.metadata_created",
                ParserInnerType::Result {
                    err: Box::new(Self::native_type(ParserInnerType::Str)),
                    ok: Box::new(Self::native_type(ParserInnerType::UInt)),
                },
            ),
            (
                "fs.metadata_accessed",
                ParserInnerType::Result {
                    err: Box::new(Self::native_type(ParserInnerType::Str)),
                    ok: Box::new(Self::native_type(ParserInnerType::UInt)),
                },
            ),
            ("fs.metadata_is_readonly", ParserInnerType::Bool),
            (
                "fs.file_open",
                ParserInnerType::Result {
                    err: Box::new(Self::native_type(ParserInnerType::Str)),
                    ok: Box::new(Self::native_type(ParserInnerType::Dynamic)),
                },
            ),
            (
                "fs.file_close",
                ParserInnerType::Result {
                    err: Box::new(Self::native_type(ParserInnerType::Str)),
                    ok: Box::new(Self::native_type(ParserInnerType::Null)),
                },
            ),
            (
                "fs.file_write",
                ParserInnerType::Result {
                    err: Box::new(Self::native_type(ParserInnerType::Str)),
                    ok: Box::new(Self::native_type(ParserInnerType::Null)),
                },
            ),
            (
                "fs.file_write_line",
                ParserInnerType::Result {
                    err: Box::new(Self::native_type(ParserInnerType::Str)),
                    ok: Box::new(Self::native_type(ParserInnerType::Null)),
                },
            ),
            (
                "fs.file_read_all",
                ParserInnerType::Result {
                    err: Box::new(Self::native_type(ParserInnerType::Str)),
                    ok: Box::new(Self::native_type(ParserInnerType::Str)),
                },
            ),
            (
                "fs.file_flush",
                ParserInnerType::Result {
                    err: Box::new(Self::native_type(ParserInnerType::Str)),
                    ok: Box::new(Self::native_type(ParserInnerType::Null)),
                },
            ),
            ("discriminant", ParserInnerType::Int),
            ("async.channel_new", ParserInnerType::Dynamic),
            ("async.channel_send", ParserInnerType::Null),
            (
                "async.channel_get",
                ParserInnerType::Option(Box::new(Self::native_type(ParserInnerType::Dynamic))),
            ),
            (
                "async.channel_try_get",
                ParserInnerType::Option(Box::new(Self::native_type(ParserInnerType::Dynamic))),
            ),
            ("async.channel_try_send", ParserInnerType::Bool),
            ("async.channel_close", ParserInnerType::Null),
            ("async.channel_closed", ParserInnerType::Bool),
            ("async.waitgroup_new", ParserInnerType::Dynamic),
            ("async.waitgroup_raw_add", ParserInnerType::Null),
            ("async.waitgroup_raw_done", ParserInnerType::Null),
            ("async.waitgroup_join", ParserInnerType::Null),
            ("async.waitgroup_wait", ParserInnerType::Null),
            ("async.waitgroup_count", ParserInnerType::Int),
            ("async.mutex_new", ParserInnerType::Null),
            ("async.mutex_get", ParserInnerType::Null),
            ("async.mutex_set", ParserInnerType::Null),
            ("async.mutex_with", ParserInnerType::Null),
            (
                "async.mutex_write",
                ParserInnerType::Ref(
                    Box::new(Self::native_type(ParserInnerType::Dynamic)),
                    RefMutability::MutRef,
                ),
            ),
            ("crypto.sha256", ParserInnerType::Str),
            ("crypto.sha512", ParserInnerType::Str),
            ("crypto.blake3", ParserInnerType::Str),
            ("regex.is_match", ParserInnerType::Bool),
            (
                "regex.find",
                ParserInnerType::Option(Box::new(Self::native_type(ParserInnerType::Str))),
            ),
            ("regex.replace", ParserInnerType::Str),
            (
                "process.raw_exec",
                ParserInnerType::Result {
                    err: Box::new(Self::native_type(ParserInnerType::Str)),
                    ok: Box::new(Self::native_type(ParserInnerType::Struct(String::from(
                        "ProcessResult",
                    )))),
                },
            ),
            (
                "collections.hashmap_new",
                ParserInnerType::StructWithGenerics {
                    identifier: String::from("HashMap"),
                    generic_types: vec![
                        Self::native_type(ParserInnerType::Dynamic),
                        Self::native_type(ParserInnerType::Dynamic),
                    ],
                },
            ),
            ("collections.hashmap_set", ParserInnerType::Null),
            (
                "collections.hashmap_get",
                ParserInnerType::Option(Box::new(Self::native_type(ParserInnerType::Dynamic))),
            ),
            (
                "collections.hashmap_remove",
                ParserInnerType::Option(Box::new(Self::native_type(ParserInnerType::Dynamic))),
            ),
            ("collections.hashmap_contains", ParserInnerType::Bool),
            ("collections.hashmap_len", ParserInnerType::Int),
            (
                "collections.hashmap_keys",
                ParserInnerType::List(Box::new(Self::native_type(ParserInnerType::Dynamic))),
            ),
            (
                "collections.hashmap_values",
                ParserInnerType::List(Box::new(Self::native_type(ParserInnerType::Dynamic))),
            ),
            (
                "collections.hashmap_entries",
                ParserInnerType::List(Box::new(Self::native_type(ParserInnerType::Dynamic))),
            ),
            ("collections.hashmap_clear", ParserInnerType::Null),
            (
                "collections.hashset_new",
                ParserInnerType::StructWithGenerics {
                    identifier: String::from("HashSet"),
                    generic_types: vec![Self::native_type(ParserInnerType::Dynamic)],
                },
            ),
            ("collections.hashset_add", ParserInnerType::Bool),
            ("collections.hashset_remove", ParserInnerType::Bool),
            ("collections.hashset_contains", ParserInnerType::Bool),
            ("collections.hashset_len", ParserInnerType::Int),
            (
                "collections.hashset_values",
                ParserInnerType::List(Box::new(Self::native_type(ParserInnerType::Dynamic))),
            ),
            ("collections.hashset_clear", ParserInnerType::Null),
            (
                "list.sort_by",
                ParserInnerType::List(Box::new(Self::native_type(ParserInnerType::Dynamic))),
            ),
            (
                "list.binary_search_by",
                ParserInnerType::Option(Box::new(Self::native_type(ParserInnerType::Int))),
            ),
            (
                "list.raw_remove",
                ParserInnerType::Option(Box::new(Self::native_type(ParserInnerType::Dynamic))),
            ),
            ("net.http_request_raw", ParserInnerType::Str),
            ("http_request_raw", ParserInnerType::Str),
            (
                "net.http_request_try",
                ParserInnerType::Result {
                    err: Box::new(Self::native_type(ParserInnerType::Str)),
                    ok: Box::new(Self::native_type(ParserInnerType::Str)),
                },
            ),
            (
                "http_request_try",
                ParserInnerType::Result {
                    err: Box::new(Self::native_type(ParserInnerType::Str)),
                    ok: Box::new(Self::native_type(ParserInnerType::Str)),
                },
            ),
            (
                "net.tcp_connect",
                ParserInnerType::Struct(String::from("TcpStream")),
            ),
            (
                "net.tcp_listen",
                ParserInnerType::Struct(String::from("TcpListener")),
            ),
            (
                "net.tcp_accept",
                ParserInnerType::Struct(String::from("TcpStream")),
            ),
            ("net.tcp_read", ParserInnerType::Str),
            ("net.tcp_write", ParserInnerType::Int),
            ("net.tcp_close", ParserInnerType::Null),
        ];

        let mut map = FxHashMap::with_capacity_and_hasher(lst.len(), Default::default());

        for (name, inner) in lst {
            map.insert(
                name.to_string(),
                Self::native_type(ParserInnerType::NativeFunction(Box::new(
                    Self::native_type(inner),
                ))),
            );
        }

        map
    }
}
