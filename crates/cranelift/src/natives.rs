use calibre_lir::{Key, MirDataType, TypeKey, VariableKey, environment::LirRegistry};
use calibre_parser::ast::RefMutability;
use cranelift::prelude::*;
use rustc_hash::FxHashMap;
use std::sync::OnceLock;

#[derive(Clone, Copy, Debug)]
pub enum ArgumentKind {
    Direct,
    TagPayload,
}

#[derive(Clone, Debug)]
pub struct RuntimeFunction {
    pub params: Vec<(Type, ArgumentKind)>,
    pub returns: Option<Type>,
    pub typed_return: Option<MirDataType>,
}

pub(crate) fn runtime_functions() -> &'static FxHashMap<&'static str, RuntimeFunction> {
    static SIGNATURES: OnceLock<FxHashMap<&'static str, RuntimeFunction>> = OnceLock::new();

    SIGNATURES.get_or_init(|| {
        let lst = [
            (
                "calibre_gc_alloc_list",
                RuntimeFunction {
                    params: vec![(types::I64, ArgumentKind::Direct)],
                    returns: Some(types::I64),
                    typed_return: Some(MirDataType::List(Box::new(MirDataType::Dynamic))),
                },
            ),
            (
                "calibre_gc_alloc_string",
                RuntimeFunction {
                    params: vec![(types::I64, ArgumentKind::Direct)],
                    returns: Some(types::I64),
                    typed_return: Some(MirDataType::Str),
                },
            ),
            (
                "calibre_gc_alloc_big_from_float",
                RuntimeFunction {
                    params: vec![(types::F64, ArgumentKind::Direct)],
                    returns: Some(types::I64),
                    typed_return: Some(MirDataType::Big),
                },
            ),
            (
                "calibre_gc_alloc_big",
                RuntimeFunction {
                    params: vec![(types::I64, ArgumentKind::Direct)],
                    returns: Some(types::I64),
                    typed_return: Some(MirDataType::Big),
                },
            ),
            (
                "calibre_gc_alloc_aggregate",
                RuntimeFunction {
                    params: vec![(types::I64, ArgumentKind::Direct)],
                    returns: Some(types::I64),
                    typed_return: Some(MirDataType::Dynamic),
                },
            ),
            (
                "calibre_aggregate_set_field",
                RuntimeFunction {
                    params: vec![
                        (types::I64, ArgumentKind::Direct),
                        (types::I64, ArgumentKind::Direct),
                        (types::I64, ArgumentKind::TagPayload),
                    ],
                    returns: None,
                    typed_return: None,
                },
            ),
            (
                "calibre_deref",
                RuntimeFunction {
                    params: vec![(types::I64, ArgumentKind::Direct)],
                    returns: Some(types::I64),
                    typed_return: Some(MirDataType::Dynamic),
                },
            ),
            (
                "calibre_set_ref",
                RuntimeFunction {
                    params: vec![
                        (types::I64, ArgumentKind::Direct),
                        (types::I64, ArgumentKind::TagPayload),
                    ],
                    returns: Some(types::I64),
                    typed_return: Some(MirDataType::Dynamic),
                },
            ),
            (
                "calibre_list_set_index",
                RuntimeFunction {
                    params: vec![
                        (types::I64, ArgumentKind::Direct),
                        (types::I64, ArgumentKind::Direct),
                        (types::I64, ArgumentKind::TagPayload),
                    ],
                    returns: None,
                    typed_return: None,
                },
            ),
            (
                "calibre_list_push",
                RuntimeFunction {
                    params: vec![
                        (types::I64, ArgumentKind::Direct),
                        (types::I64, ArgumentKind::TagPayload),
                    ],
                    returns: None,
                    typed_return: None,
                },
            ),
            (
                "calibre_list_index",
                RuntimeFunction {
                    params: vec![
                        (types::I64, ArgumentKind::Direct),
                        (types::I64, ArgumentKind::Direct),
                    ],
                    returns: Some(types::I64),
                    typed_return: Some(MirDataType::Ref(
                        Box::new(MirDataType::Dynamic),
                        RefMutability::MutRef,
                    )),
                },
            ),
            (
                "calibre_list_ref_index",
                RuntimeFunction {
                    params: vec![
                        (types::I64, ArgumentKind::Direct),
                        (types::I64, ArgumentKind::Direct),
                    ],
                    returns: Some(types::I64),
                    typed_return: Some(MirDataType::Ref(
                        Box::new(MirDataType::Dynamic),
                        RefMutability::MutRef,
                    )),
                },
            ),
            (
                "calibre_list_slice",
                RuntimeFunction {
                    params: vec![
                        (types::I64, ArgumentKind::Direct),
                        (types::I64, ArgumentKind::Direct),
                    ],
                    returns: Some(types::I64),
                    typed_return: Some(MirDataType::List(Box::new(MirDataType::Dynamic))),
                },
            ),
            (
                "calibre_gc_collect",
                RuntimeFunction {
                    params: Vec::new(),
                    returns: None,
                    typed_return: None,
                },
            ),
            (
                "calibre_gc_stats",
                RuntimeFunction {
                    params: Vec::new(),
                    returns: None,
                    typed_return: None,
                },
            ),
            (
                "calibre_gc_add_root",
                RuntimeFunction {
                    params: vec![(types::I64, ArgumentKind::Direct)],
                    returns: None,
                    typed_return: None,
                },
            ),
            (
                "calibre_gc_remove_root",
                RuntimeFunction {
                    params: vec![(types::I64, ArgumentKind::Direct)],
                    returns: None,
                    typed_return: None,
                },
            ),
            (
                "calibre_range_index",
                RuntimeFunction {
                    params: vec![
                        (types::I64, ArgumentKind::Direct),
                        (types::I64, ArgumentKind::Direct),
                    ],
                    returns: Some(types::I64),
                    typed_return: Some(MirDataType::Int),
                },
            ),
            (
                "calibre_range_slice",
                RuntimeFunction {
                    params: vec![
                        (types::I64, ArgumentKind::Direct),
                        (types::I64, ArgumentKind::Direct),
                    ],
                    returns: Some(types::I64),
                    typed_return: Some(MirDataType::Range),
                },
            ),
            (
                "calibre_comparison",
                RuntimeFunction {
                    params: vec![
                        (types::I64, ArgumentKind::Direct),
                        (types::I64, ArgumentKind::TagPayload),
                        (types::I64, ArgumentKind::TagPayload),
                    ],
                    returns: Some(types::I8),
                    typed_return: Some(MirDataType::Bool),
                },
            ),
            (
                "calibre_binary",
                RuntimeFunction {
                    params: vec![
                        (types::I64, ArgumentKind::Direct),
                        (types::I64, ArgumentKind::TagPayload),
                        (types::I64, ArgumentKind::TagPayload),
                    ],
                    returns: Some(types::I64),
                    typed_return: Some(MirDataType::Dynamic),
                },
            ),
            (
                "calibre_option_none",
                RuntimeFunction {
                    params: Vec::new(),
                    returns: Some(types::I64),
                    typed_return: Some(MirDataType::Option(Box::new(MirDataType::Dynamic))),
                },
            ),
            (
                "calibre_option_some",
                RuntimeFunction {
                    params: vec![(types::I64, ArgumentKind::TagPayload)],
                    returns: Some(types::I64),
                    typed_return: Some(MirDataType::Option(Box::new(MirDataType::Struct {
                        identifier: TypeKey::from("1"),
                        generic_types: Vec::new(),
                    }))),
                },
            ),
            (
                "calibre_result_ok",
                RuntimeFunction {
                    params: vec![(types::I64, ArgumentKind::TagPayload)],
                    returns: Some(types::I64),
                    typed_return: Some(MirDataType::Result {
                        ok: Box::new(MirDataType::Struct {
                            identifier: TypeKey::from("1"),
                            generic_types: Vec::new(),
                        }),
                        err: Box::new(MirDataType::Dynamic),
                    }),
                },
            ),
            (
                "calibre_result_err",
                RuntimeFunction {
                    params: vec![(types::I64, ArgumentKind::TagPayload)],
                    returns: Some(types::I64),
                    typed_return: Some(MirDataType::Result {
                        ok: Box::new(MirDataType::Dynamic),
                        err: Box::new(MirDataType::Struct {
                            identifier: TypeKey::from("1"),
                            generic_types: Vec::new(),
                        }),
                    }),
                },
            ),
            (
                "calibre_enum_get",
                RuntimeFunction {
                    params: vec![(types::I64, ArgumentKind::Direct)],
                    returns: Some(types::I64),
                    typed_return: Some(MirDataType::Ref(
                        Box::new(MirDataType::Dynamic),
                        RefMutability::MutRef,
                    )),
                },
            ),
            (
                "calibre_enum_ref_get",
                RuntimeFunction {
                    params: vec![(types::I64, ArgumentKind::Direct)],
                    returns: Some(types::I64),
                    typed_return: Some(MirDataType::Ref(
                        Box::new(MirDataType::Dynamic),
                        RefMutability::MutRef,
                    )),
                },
            ),
            (
                "calibre_enum_discriminant",
                RuntimeFunction {
                    params: vec![(types::I64, ArgumentKind::Direct)],
                    returns: Some(types::I64),
                    typed_return: Some(MirDataType::UInt),
                },
            ),
            (
                "calibre_gc_alloc_enum",
                RuntimeFunction {
                    params: vec![
                        (types::I64, ArgumentKind::Direct),
                        (types::I64, ArgumentKind::TagPayload),
                    ],
                    returns: Some(types::I64),
                    typed_return: Some(MirDataType::Struct {
                        identifier: TypeKey::from("1"),
                        generic_types: Vec::new(),
                    }),
                },
            ),
            (
                "calibre_panic",
                RuntimeFunction {
                    params: vec![(types::I64, ArgumentKind::TagPayload)],
                    returns: None,
                    typed_return: None,
                },
            ),
            (
                "calibre_console_input",
                RuntimeFunction {
                    params: Vec::new(),
                    returns: Some(types::I64),
                    typed_return: Some(MirDataType::Str),
                },
            ),
            (
                "calibre_console_output",
                RuntimeFunction {
                    params: vec![
                        (types::I64, ArgumentKind::Direct),
                        (types::I64, ArgumentKind::Direct),
                    ],
                    returns: None,
                    typed_return: None,
                },
            ),
            (
                "calibre_to_string",
                RuntimeFunction {
                    params: vec![(types::I64, ArgumentKind::TagPayload)],
                    returns: Some(types::I64),
                    typed_return: Some(MirDataType::Str),
                },
            ),
            (
                "calibre_string_index",
                RuntimeFunction {
                    params: vec![
                        (types::I64, ArgumentKind::Direct),
                        (types::I64, ArgumentKind::Direct),
                    ],
                    returns: Some(types::I32),
                    typed_return: Some(MirDataType::Char),
                },
            ),
            (
                "calibre_string_slice",
                RuntimeFunction {
                    params: vec![
                        (types::I64, ArgumentKind::Direct),
                        (types::I64, ArgumentKind::Direct),
                    ],
                    returns: Some(types::I64),
                    typed_return: Some(MirDataType::Str),
                },
            ),
            (
                "calibre_string_push",
                RuntimeFunction {
                    params: vec![
                        (types::I64, ArgumentKind::Direct),
                        (types::I32, ArgumentKind::Direct),
                    ],
                    returns: None,
                    typed_return: None,
                },
            ),
            (
                "calibre_trim",
                RuntimeFunction {
                    params: vec![(types::I64, ArgumentKind::Direct)],
                    returns: Some(types::I64),
                    typed_return: Some(MirDataType::Str),
                },
            ),
            (
                "calibre_wait",
                RuntimeFunction {
                    params: vec![(types::I64, ArgumentKind::Direct)],
                    returns: None,
                    typed_return: None,
                },
            ),
            (
                "calibre_len",
                RuntimeFunction {
                    params: vec![(types::I64, ArgumentKind::TagPayload)],
                    returns: Some(types::I64),
                    typed_return: Some(MirDataType::UInt),
                },
            ),
            (
                "calibre_min_or_zero",
                RuntimeFunction {
                    params: vec![(types::I64, ArgumentKind::TagPayload)],
                    returns: Some(types::I64),
                    typed_return: Some(MirDataType::Int),
                },
            ),
            (
                "calibre_gen_suspend",
                RuntimeFunction {
                    params: vec![(types::I64, ArgumentKind::Direct)],
                    returns: Some(types::I64),
                    typed_return: Some(MirDataType::Dynamic),
                },
            ),
            // env
            (
                "calibre_env_vars",
                RuntimeFunction {
                    params: Vec::new(),
                    returns: Some(types::I64),
                    typed_return: Some(MirDataType::List(Box::new(MirDataType::Tuple(vec![
                        MirDataType::Str,
                        MirDataType::Str,
                    ])))),
                },
            ),
            // str
            (
                "calibre_str_char_lowercase",
                RuntimeFunction {
                    params: vec![(types::I32, ArgumentKind::Direct)],
                    returns: Some(types::I32),
                    typed_return: Some(MirDataType::Char),
                },
            ),
            (
                "calibre_str_char_uppercase",
                RuntimeFunction {
                    params: vec![(types::I32, ArgumentKind::Direct)],
                    returns: Some(types::I32),
                    typed_return: Some(MirDataType::Char),
                },
            ),
            (
                "calibre_str_split",
                RuntimeFunction {
                    params: vec![
                        (types::I64, ArgumentKind::Direct),
                        (types::I64, ArgumentKind::Direct),
                    ],
                    returns: Some(types::I64),
                    typed_return: Some(MirDataType::List(Box::new(MirDataType::Str))),
                },
            ),
            (
                "calibre_str_contains",
                RuntimeFunction {
                    params: vec![
                        (types::I64, ArgumentKind::Direct),
                        (types::I64, ArgumentKind::Direct),
                    ],
                    returns: Some(types::I8),
                    typed_return: Some(MirDataType::Bool),
                },
            ),
            (
                "calibre_str_starts_with",
                RuntimeFunction {
                    params: vec![
                        (types::I64, ArgumentKind::Direct),
                        (types::I64, ArgumentKind::Direct),
                    ],
                    returns: Some(types::I8),
                    typed_return: Some(MirDataType::Bool),
                },
            ),
            (
                "calibre_str_ends_with",
                RuntimeFunction {
                    params: vec![
                        (types::I64, ArgumentKind::Direct),
                        (types::I64, ArgumentKind::Direct),
                    ],
                    returns: Some(types::I8),
                    typed_return: Some(MirDataType::Bool),
                },
            ),
            // crypto
            (
                "calibre_crypto_sha256",
                RuntimeFunction {
                    params: vec![(types::I64, ArgumentKind::Direct)],
                    returns: Some(types::I64),
                    typed_return: Some(MirDataType::Str),
                },
            ),
            (
                "calibre_crypto_sha512",
                RuntimeFunction {
                    params: vec![(types::I64, ArgumentKind::Direct)],
                    returns: Some(types::I64),
                    typed_return: Some(MirDataType::Str),
                },
            ),
            (
                "calibre_crypto_blake3",
                RuntimeFunction {
                    params: vec![(types::I64, ArgumentKind::Direct)],
                    returns: Some(types::I64),
                    typed_return: Some(MirDataType::Str),
                },
            ),
            // libc
            (
                "calibre_libc_get_c_errno",
                RuntimeFunction {
                    params: Vec::new(),
                    returns: Some(types::I64),
                    typed_return: Some(MirDataType::Int),
                },
            ),
            (
                "calibre_libc_get_c_errno_description",
                RuntimeFunction {
                    params: Vec::new(),
                    returns: Some(types::I64),
                    typed_return: Some(MirDataType::Str),
                },
            ),
            (
                "calibre_libc_set_c_errno",
                RuntimeFunction {
                    params: vec![(types::I64, ArgumentKind::Direct)],
                    returns: None,
                    typed_return: None,
                },
            ),
            // list
            (
                "calibre_list_raw_remove",
                RuntimeFunction {
                    params: vec![
                        (types::I64, ArgumentKind::Direct),
                        (types::I64, ArgumentKind::Direct),
                    ],
                    returns: Some(types::I64),
                    typed_return: Some(MirDataType::Option(Box::new(MirDataType::Dynamic))),
                },
            ),
            (
                "calibre_list_raw_remove",
                RuntimeFunction {
                    params: vec![
                        (types::I64, ArgumentKind::Direct),
                        (types::I64, ArgumentKind::Direct),
                        (types::I64, ArgumentKind::TagPayload),
                    ],
                    returns: Some(types::I64),
                    typed_return: Some(MirDataType::Bool),
                },
            ),
            // random
            (
                "calibre_random_rand",
                RuntimeFunction {
                    params: vec![(types::I64, ArgumentKind::Direct)],
                    returns: Some(types::F64),
                    typed_return: Some(MirDataType::Float),
                },
            ),
            (
                "calibre_random_seed",
                RuntimeFunction {
                    params: vec![(types::I64, ArgumentKind::Direct)],
                    returns: None,
                    typed_return: None,
                },
            ),
            // regex
            (
                "calibre_regex_is_match",
                RuntimeFunction {
                    params: vec![
                        (types::I64, ArgumentKind::Direct),
                        (types::I64, ArgumentKind::Direct),
                    ],
                    returns: Some(types::I8),
                    typed_return: Some(MirDataType::Bool),
                },
            ),
            (
                "calibre_regex_find",
                RuntimeFunction {
                    params: vec![
                        (types::I64, ArgumentKind::Direct),
                        (types::I64, ArgumentKind::Direct),
                    ],
                    returns: Some(types::I64),
                    typed_return: Some(MirDataType::Option(Box::new(MirDataType::Str))),
                },
            ),
            (
                "calibre_regex_replace",
                RuntimeFunction {
                    params: vec![
                        (types::I64, ArgumentKind::Direct),
                        (types::I64, ArgumentKind::Direct),
                        (types::I64, ArgumentKind::Direct),
                    ],
                    returns: Some(types::I64),
                    typed_return: Some(MirDataType::Str),
                },
            ),
            // hasher
            (
                "calibre_new_hasher",
                RuntimeFunction {
                    params: Vec::new(),
                    returns: Some(types::I64),
                    typed_return: None,
                },
            ),
            (
                "calibre_hash_add",
                RuntimeFunction {
                    params: vec![
                        (types::I64, ArgumentKind::Direct),
                        (types::I64, ArgumentKind::Direct),
                        (types::I64, ArgumentKind::Direct),
                    ],
                    returns: None,
                    typed_return: None,
                },
            ),
            (
                "calibre_hash_finish",
                RuntimeFunction {
                    params: vec![(types::I64, ArgumentKind::Direct)],
                    returns: Some(types::I64),
                    typed_return: None,
                },
            ),
            (
                "calibre_memo_contains",
                RuntimeFunction {
                    params: vec![(types::I64, ArgumentKind::Direct)],
                    returns: Some(types::I8),
                    typed_return: Some(MirDataType::Bool),
                },
            ),
            (
                "calibre_memo_get",
                RuntimeFunction {
                    params: vec![(types::I64, ArgumentKind::Direct)],
                    returns: Some(types::I64),
                    typed_return: None,
                },
            ),
            (
                "calibre_memo_set",
                RuntimeFunction {
                    params: vec![
                        (types::I64, ArgumentKind::Direct),
                        (types::I64, ArgumentKind::Direct),
                    ],
                    returns: None,
                    typed_return: None,
                },
            ),
            // fs
            (
                "calibre_fs_path_new",
                RuntimeFunction {
                    params: vec![(types::I64, ArgumentKind::Direct)],
                    returns: Some(types::I64),
                    typed_return: Some(MirDataType::Host),
                },
            ),
            (
                "calibre_fs_path_as_str",
                RuntimeFunction {
                    params: vec![(types::I64, ArgumentKind::Direct)],
                    returns: Some(types::I64),
                    typed_return: Some(MirDataType::Str),
                },
            ),
            (
                "calibre_fs_path_exists",
                RuntimeFunction {
                    params: vec![(types::I64, ArgumentKind::Direct)],
                    returns: Some(types::I8),
                    typed_return: Some(MirDataType::Bool),
                },
            ),
            (
                "calibre_fs_path_is_file",
                RuntimeFunction {
                    params: vec![(types::I64, ArgumentKind::Direct)],
                    returns: Some(types::I8),
                    typed_return: Some(MirDataType::Bool),
                },
            ),
            (
                "calibre_fs_path_is_dir",
                RuntimeFunction {
                    params: vec![(types::I64, ArgumentKind::Direct)],
                    returns: Some(types::I8),
                    typed_return: Some(MirDataType::Bool),
                },
            ),
            (
                "calibre_fs_path_canonicalize",
                RuntimeFunction {
                    params: vec![(types::I64, ArgumentKind::Direct)],
                    returns: Some(types::I64),
                    typed_return: Some(MirDataType::Result {
                        ok: Box::new(MirDataType::Host),
                        err: Box::new(MirDataType::Str),
                    }),
                },
            ),
            (
                "calibre_fs_path_parent",
                RuntimeFunction {
                    params: vec![(types::I64, ArgumentKind::Direct)],
                    returns: Some(types::I64),
                    typed_return: Some(MirDataType::Option(Box::new(MirDataType::Host))),
                },
            ),
            (
                "calibre_fs_path_file_name",
                RuntimeFunction {
                    params: vec![(types::I64, ArgumentKind::Direct)],
                    returns: Some(types::I64),
                    typed_return: Some(MirDataType::Option(Box::new(MirDataType::Str))),
                },
            ),
            (
                "calibre_fs_path_extension",
                RuntimeFunction {
                    params: vec![(types::I64, ArgumentKind::Direct)],
                    returns: Some(types::I64),
                    typed_return: Some(MirDataType::Option(Box::new(MirDataType::Str))),
                },
            ),
            (
                "calibre_fs_path_stem",
                RuntimeFunction {
                    params: vec![(types::I64, ArgumentKind::Direct)],
                    returns: Some(types::I64),
                    typed_return: Some(MirDataType::Option(Box::new(MirDataType::Str))),
                },
            ),
            (
                "calibre_fs_path_join",
                RuntimeFunction {
                    params: vec![
                        (types::I64, ArgumentKind::Direct),
                        (types::I64, ArgumentKind::Direct),
                    ],
                    returns: None,
                    typed_return: None,
                },
            ),
            (
                "calibre_fs_path_with_extension",
                RuntimeFunction {
                    params: vec![
                        (types::I64, ArgumentKind::Direct),
                        (types::I64, ArgumentKind::Direct),
                    ],
                    returns: Some(types::I64),
                    typed_return: Some(MirDataType::Host),
                },
            ),
            (
                "calibre_fs_path_with_file_name",
                RuntimeFunction {
                    params: vec![
                        (types::I64, ArgumentKind::Direct),
                        (types::I64, ArgumentKind::Direct),
                    ],
                    returns: Some(types::I64),
                    typed_return: Some(MirDataType::Host),
                },
            ),
            (
                "calibre_fs_path_read_dir",
                RuntimeFunction {
                    params: vec![(types::I64, ArgumentKind::Direct)],
                    returns: Some(types::I64),
                    typed_return: Some(MirDataType::Result {
                        ok: Box::new(MirDataType::List(Box::new(MirDataType::Host))),
                        err: Box::new(MirDataType::Str),
                    }),
                },
            ),
            (
                "calibre_fs_direntry_path",
                RuntimeFunction {
                    params: vec![(types::I64, ArgumentKind::Direct)],
                    returns: Some(types::I64),
                    typed_return: Some(MirDataType::Host),
                },
            ),
            (
                "calibre_fs_direntry_file_name",
                RuntimeFunction {
                    params: vec![(types::I64, ArgumentKind::Direct)],
                    returns: Some(types::I64),
                    typed_return: Some(MirDataType::Str),
                },
            ),
            (
                "calibre_fs_direntry_file_type",
                RuntimeFunction {
                    params: vec![(types::I64, ArgumentKind::Direct)],
                    returns: Some(types::I64),
                    typed_return: Some(MirDataType::Result {
                        ok: Box::new(MirDataType::Host),
                        err: Box::new(MirDataType::Str),
                    }),
                },
            ),
            (
                "calibre_fs_direntry_metadata",
                RuntimeFunction {
                    params: vec![(types::I64, ArgumentKind::Direct)],
                    returns: Some(types::I64),
                    typed_return: Some(MirDataType::Result {
                        ok: Box::new(MirDataType::Host),
                        err: Box::new(MirDataType::Str),
                    }),
                },
            ),
            (
                "calibre_fs_filetype_is_file",
                RuntimeFunction {
                    params: vec![(types::I64, ArgumentKind::Direct)],
                    returns: Some(types::I8),
                    typed_return: Some(MirDataType::Bool),
                },
            ),
            (
                "calibre_fs_filetype_is_dir",
                RuntimeFunction {
                    params: vec![(types::I64, ArgumentKind::Direct)],
                    returns: Some(types::I8),
                    typed_return: Some(MirDataType::Bool),
                },
            ),
            (
                "calibre_fs_filetype_is_symlink",
                RuntimeFunction {
                    params: vec![(types::I64, ArgumentKind::Direct)],
                    returns: Some(types::I8),
                    typed_return: Some(MirDataType::Bool),
                },
            ),
            (
                "calibre_fs_metadata_is_file",
                RuntimeFunction {
                    params: vec![(types::I64, ArgumentKind::Direct)],
                    returns: Some(types::I8),
                    typed_return: Some(MirDataType::Bool),
                },
            ),
            (
                "calibre_fs_metadata_is_dir",
                RuntimeFunction {
                    params: vec![(types::I64, ArgumentKind::Direct)],
                    returns: Some(types::I8),
                    typed_return: Some(MirDataType::Bool),
                },
            ),
            (
                "calibre_fs_metadata_len",
                RuntimeFunction {
                    params: vec![(types::I64, ArgumentKind::Direct)],
                    returns: Some(types::I64),
                    typed_return: Some(MirDataType::UInt),
                },
            ),
            (
                "calibre_fs_metadata_modified",
                RuntimeFunction {
                    params: vec![(types::I64, ArgumentKind::Direct)],
                    returns: Some(types::I64),
                    typed_return: Some(MirDataType::Result {
                        ok: Box::new(MirDataType::UInt),
                        err: Box::new(MirDataType::Str),
                    }),
                },
            ),
            (
                "calibre_fs_metadata_created",
                RuntimeFunction {
                    params: vec![(types::I64, ArgumentKind::Direct)],
                    returns: Some(types::I64),
                    typed_return: Some(MirDataType::Result {
                        ok: Box::new(MirDataType::UInt),
                        err: Box::new(MirDataType::Str),
                    }),
                },
            ),
            (
                "calibre_fs_metadata_accessed",
                RuntimeFunction {
                    params: vec![(types::I64, ArgumentKind::Direct)],
                    returns: Some(types::I64),
                    typed_return: Some(MirDataType::Result {
                        ok: Box::new(MirDataType::UInt),
                        err: Box::new(MirDataType::Str),
                    }),
                },
            ),
            (
                "calibre_fs_metadata_is_readonly",
                RuntimeFunction {
                    params: vec![(types::I64, ArgumentKind::Direct)],
                    returns: Some(types::I8),
                    typed_return: Some(MirDataType::Bool),
                },
            ),
            (
                "calibre_fs_file_open",
                RuntimeFunction {
                    params: vec![
                        (types::I64, ArgumentKind::Direct),
                        (types::I64, ArgumentKind::Direct),
                    ],
                    returns: Some(types::I64),
                    typed_return: Some(MirDataType::Result {
                        ok: Box::new(MirDataType::Host),
                        err: Box::new(MirDataType::Str),
                    }),
                },
            ),
            (
                "calibre_fs_file_close",
                RuntimeFunction {
                    params: vec![(types::I64, ArgumentKind::Direct)],
                    returns: Some(types::I64),
                    typed_return: Some(MirDataType::Result {
                        ok: Box::new(MirDataType::Null),
                        err: Box::new(MirDataType::Str),
                    }),
                },
            ),
            (
                "calibre_fs_file_write",
                RuntimeFunction {
                    params: vec![
                        (types::I64, ArgumentKind::Direct),
                        (types::I64, ArgumentKind::Direct),
                    ],
                    returns: Some(types::I64),
                    typed_return: Some(MirDataType::Result {
                        ok: Box::new(MirDataType::Null),
                        err: Box::new(MirDataType::Str),
                    }),
                },
            ),
            (
                "calibre_fs_file_write_line",
                RuntimeFunction {
                    params: vec![
                        (types::I64, ArgumentKind::Direct),
                        (types::I64, ArgumentKind::Direct),
                    ],
                    returns: Some(types::I64),
                    typed_return: Some(MirDataType::Result {
                        ok: Box::new(MirDataType::Null),
                        err: Box::new(MirDataType::Str),
                    }),
                },
            ),
            (
                "calibre_fs_file_read_all",
                RuntimeFunction {
                    params: vec![(types::I64, ArgumentKind::Direct)],
                    returns: Some(types::I64),
                    typed_return: Some(MirDataType::Result {
                        ok: Box::new(MirDataType::Str),
                        err: Box::new(MirDataType::Str),
                    }),
                },
            ),
            (
                "calibre_fs_file_flush",
                RuntimeFunction {
                    params: vec![(types::I64, ArgumentKind::Direct)],
                    returns: Some(types::I64),
                    typed_return: Some(MirDataType::Result {
                        ok: Box::new(MirDataType::Null),
                        err: Box::new(MirDataType::Str),
                    }),
                },
            ),
            (
                "calibre_fs_dir_create",
                RuntimeFunction {
                    params: vec![(types::I64, ArgumentKind::Direct)],
                    returns: Some(types::I64),
                    typed_return: Some(MirDataType::Result {
                        ok: Box::new(MirDataType::Null),
                        err: Box::new(MirDataType::Str),
                    }),
                },
            ),
            (
                "calibre_fs_dir_create_all",
                RuntimeFunction {
                    params: vec![(types::I64, ArgumentKind::Direct)],
                    returns: Some(types::I64),
                    typed_return: Some(MirDataType::Result {
                        ok: Box::new(MirDataType::Null),
                        err: Box::new(MirDataType::Str),
                    }),
                },
            ),
            (
                "calibre_fs_dir_remove",
                RuntimeFunction {
                    params: vec![(types::I64, ArgumentKind::Direct)],
                    returns: Some(types::I64),
                    typed_return: Some(MirDataType::Result {
                        ok: Box::new(MirDataType::Null),
                        err: Box::new(MirDataType::Str),
                    }),
                },
            ),
            (
                "calibre_fs_dir_remove_all",
                RuntimeFunction {
                    params: vec![(types::I64, ArgumentKind::Direct)],
                    returns: Some(types::I64),
                    typed_return: Some(MirDataType::Result {
                        ok: Box::new(MirDataType::Null),
                        err: Box::new(MirDataType::Str),
                    }),
                },
            ),
        ];

        lst.into_iter().collect()
    })
}

pub(crate) fn natives() -> &'static FxHashMap<&'static str, &'static str> {
    static NATIVES: OnceLock<FxHashMap<&'static str, &'static str>> = OnceLock::new();

    NATIVES.get_or_init(|| {
        let lst = [
            ("ok", "calibre_result_ok"),
            ("err", "calibre_result_err"),
            ("some", "calibre_option_some"),
            ("panic", "calibre_panic"),
            ("console_output", "calibre_console_output"),
            ("console_input", "calibre_console_input"),
            ("trim", "calibre_trim"),
            ("wait", "calibre_wait"),
            ("len", "calibre_len"),
            ("min_or_zero", "calibre_min_or_zero"),
            ("gen_suspend", "calibre_gen_suspend"),
            // env
            ("env.vars", "calibre_env_vars"),
            // str
            ("str.split", "calibre_str_split"),
            ("str.contains", "calibre_str_contains"),
            ("str.starts_with", "calibre_str_starts_with"),
            ("str.ends_with", "calibre_str_ends_with"),
            ("str.char_lowercase", "calibre_str_char_lowercase"),
            ("str.char_uppercase", "calibre_str_char_uppercase"),
            // crypto
            ("crypto.sha256", "calibre_crypto_sha256"),
            ("crypto.sha512", "calibre_crypto_sha512"),
            ("crypto.blake3", "calibre_crypto_blake3"),
            // libc
            ("libc.get_c_errno", "calibre_libc_get_c_errno"),
            (
                "libc.get_c_errno_description",
                "calibre_libc_get_c_errno_description",
            ),
            ("libc.set_c_errno", "calibre_libc_set_c_errno"),
            // list
            ("list.raw_remove", "calibre_list_raw_remove"),
            ("list.raw_insert", "calibre_list_raw_insert"),
            // random
            ("random.rand", "calibre_random_rand"),
            ("random.seed", "calibre_random_seed"),
            // regex
            ("regex.is_match", "calibre_regex_is_match"),
            ("regex.find", "calibre_regex_find"),
            ("regex.replace", "calibre_regex_replace"),
            // fs
            ("fs.path_new", "calibre_fs_path_new"),
            ("fs.path_as_str", "calibre_fs_path_as_str"),
            ("fs.path_exists", "calibre_fs_path_exists"),
            ("fs.path_is_file", "calibre_fs_path_is_file"),
            ("fs.path_is_dir", "calibre_fs_path_is_dir"),
            ("fs.path_canonicalize", "calibre_fs_path_canonicalize"),
            ("fs.path_parent", "calibre_fs_path_parent"),
            ("fs.path_file_name", "calibre_fs_path_file_name"),
            ("fs.path_extension", "calibre_fs_path_extension"),
            ("fs.path_stem", "calibre_fs_path_stem"),
            ("fs.path_join", "calibre_fs_path_join"),
            ("fs.path_with_extension", "calibre_fs_path_with_extension"),
            ("fs.path_with_file_name", "calibre_fs_path_with_file_name"),
            ("fs.path_read_dir", "calibre_fs_path_read_dir"),
            ("fs.direntry_path", "calibre_fs_direntry_path"),
            ("fs.direntry_file_name", "calibre_fs_direntry_file_name"),
            ("fs.direntry_file_type", "calibre_fs_direntry_file_type"),
            ("fs.direntry_metadata", "calibre_fs_direntry_metadata"),
            ("fs.filetype_is_file", "calibre_fs_filetype_is_file"),
            ("fs.filetype_is_dir", "calibre_fs_filetype_is_dir"),
            ("fs.filetype_is_symlink", "calibre_fs_filetype_is_symlink"),
            ("fs.metadata_is_file", "calibre_fs_metadata_is_file"),
            ("fs.metadata_is_dir", "calibre_fs_metadata_is_dir"),
            ("fs.metadata_len", "calibre_fs_metadata_len"),
            ("fs.metadata_modified", "calibre_fs_metadata_modified"),
            ("fs.metadata_created", "calibre_fs_metadata_created"),
            ("fs.metadata_accessed", "calibre_fs_metadata_accessed"),
            ("fs.metadata_is_readonly", "calibre_fs_metadata_is_readonly"),
            ("fs.file_open", "calibre_fs_file_open"),
            ("fs.file_close", "calibre_fs_file_close"),
            ("fs.file_write", "calibre_fs_file_write"),
            ("fs.file_write_line", "calibre_fs_file_write_line"),
            ("fs.file_read_all", "calibre_fs_file_read_all"),
            ("fs.file_flush", "calibre_fs_file_flush"),
            ("fs.dir_create", "calibre_fs_dir_create"),
            ("fs.dir_create_all", "calibre_fs_dir_create_all"),
            ("fs.dir_remove", "calibre_fs_dir_remove"),
            ("fs.dir_remove_all", "calibre_fs_dir_remove_all"),
        ];

        lst.into_iter().collect()
    })
}

pub type Natives = FxHashMap<VariableKey, &'static str>;

pub(crate) fn build_native_mapping(registry: &LirRegistry) -> Natives {
    let mut map = FxHashMap::default();

    for (name, key) in &registry.natives {
        if let Key::VariableKey(var_key) = key
            && let Some(name) = natives().get(name.as_str())
        {
            map.insert(var_key.clone(), *name);
        }
    }

    map
}
