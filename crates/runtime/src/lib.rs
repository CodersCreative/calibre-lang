use crate::{native::fs, values::{aggregate, enums, hasher, list, operation, range, string}};
use astro_float::RoundingMode;

pub mod gc;
pub mod native;
pub mod values;

pub const BIG_PRECISION: usize = 128;
pub const BIG_ROUNDING: RoundingMode = RoundingMode::ToEven;

#[derive(Debug, Clone, Copy)]
pub struct RuntimeSymbol {
    pub name: &'static str,
    pub ptr: *const u8,
}

unsafe impl Send for RuntimeSymbol {}
unsafe impl Sync for RuntimeSymbol {}

pub fn get_runtime_symbols() -> &'static [RuntimeSymbol] {
    &[
        // memory
        RuntimeSymbol {
            name: "calibre_deref",
            ptr: values::calibre_deref as *const u8,
        },
        RuntimeSymbol {
            name: "calibre_set_ref",
            ptr: values::calibre_set_ref as *const u8,
        },
        // operation
        RuntimeSymbol {
            name: "calibre_comparison",
            ptr: operation::calibre_comparison as *const u8,
        },
        RuntimeSymbol {
            name: "calibre_binary",
            ptr: operation::calibre_binary as *const u8,
        },
        // gc
        RuntimeSymbol {
            name: "calibre_gc_collect",
            ptr: gc::calibre_gc_collect as *const u8,
        },
        RuntimeSymbol {
            name: "calibre_gc_stats",
            ptr: gc::calibre_gc_stats as *const u8,
        },
        RuntimeSymbol {
            name: "calibre_gc_add_root",
            ptr: gc::calibre_gc_add_root as *const u8,
        },
        RuntimeSymbol {
            name: "calibre_gc_remove_root",
            ptr: gc::calibre_gc_remove_root as *const u8,
        },
        // range
        RuntimeSymbol {
            name: "calibre_range_slice",
            ptr: range::calibre_range_slice as *const u8,
        },
        RuntimeSymbol {
            name: "calibre_range_index",
            ptr: range::calibre_range_index as *const u8,
        },
        // string
        RuntimeSymbol {
            name: "calibre_gc_alloc_string",
            ptr: string::calibre_gc_alloc_string as *const u8,
        },
        RuntimeSymbol {
            name: "calibre_string_push",
            ptr: string::calibre_string_push as *const u8,
        },
        RuntimeSymbol {
            name: "calibre_to_string",
            ptr: string::calibre_to_string as *const u8,
        },
        RuntimeSymbol {
            name: "calibre_string_index",
            ptr: string::calibre_string_index as *const u8,
        },
        RuntimeSymbol {
            name: "calibre_string_slice",
            ptr: string::calibre_string_slice as *const u8,
        },
        // aggregate
        RuntimeSymbol {
            name: "calibre_gc_alloc_aggregate",
            ptr: aggregate::calibre_gc_alloc_aggregate as *const u8,
        },
        RuntimeSymbol {
            name: "calibre_aggregate_set_field",
            ptr: aggregate::calibre_aggregate_set_field as *const u8,
        },
        // list
        RuntimeSymbol {
            name: "calibre_gc_alloc_list",
            ptr: list::calibre_gc_alloc_list as *const u8,
        },
        RuntimeSymbol {
            name: "calibre_list_set_index",
            ptr: list::calibre_list_set_index as *const u8,
        },
        RuntimeSymbol {
            name: "calibre_list_push",
            ptr: list::calibre_list_push as *const u8,
        },
        RuntimeSymbol {
            name: "calibre_list_index",
            ptr: list::calibre_list_index as *const u8,
        },
        RuntimeSymbol {
            name: "calibre_list_ref_index",
            ptr: list::calibre_list_ref_index as *const u8,
        },
        RuntimeSymbol {
            name: "calibre_list_slice",
            ptr: list::calibre_list_slice as *const u8,
        },
        // enums
        RuntimeSymbol {
            name: "calibre_option_none",
            ptr: enums::calibre_option_none as *const u8,
        },
        RuntimeSymbol {
            name: "calibre_option_some",
            ptr: enums::calibre_option_some as *const u8,
        },
        RuntimeSymbol {
            name: "calibre_result_ok",
            ptr: enums::calibre_result_ok as *const u8,
        },
        RuntimeSymbol {
            name: "calibre_result_err",
            ptr: enums::calibre_result_err as *const u8,
        },
        RuntimeSymbol {
            name: "calibre_enum_get",
            ptr: enums::calibre_enum_get as *const u8,
        },
        RuntimeSymbol {
            name: "calibre_enum_ref_get",
            ptr: enums::calibre_enum_ref_get as *const u8,
        },
        RuntimeSymbol {
            name: "calibre_enum_discriminant",
            ptr: enums::calibre_enum_discriminant as *const u8,
        },
        RuntimeSymbol {
            name: "calibre_gc_alloc_enum",
            ptr: enums::calibre_gc_alloc_enum as *const u8,
        },
        // native
        RuntimeSymbol {
            name: "calibre_console_input",
            ptr: native::calibre_console_input as *const u8,
        },
        RuntimeSymbol {
            name: "calibre_console_output",
            ptr: native::calibre_console_output as *const u8,
        },
        RuntimeSymbol {
            name: "calibre_len",
            ptr: native::calibre_len as *const u8,
        },
        RuntimeSymbol {
            name: "calibre_min_or_zero",
            ptr: native::calibre_min_or_zero as *const u8,
        },
        RuntimeSymbol {
            name: "calibre_gen_suspend",
            ptr: native::calibre_gen_suspend as *const u8,
        },
        RuntimeSymbol {
            name: "calibre_panic",
            ptr: native::calibre_panic as *const u8,
        },
        RuntimeSymbol {
            name: "calibre_trim",
            ptr: native::calibre_trim as *const u8,
        },
        RuntimeSymbol {
            name: "calibre_wait",
            ptr: native::calibre_wait as *const u8,
        },
        // native env
        RuntimeSymbol {
            name: "calibre_env_vars",
            ptr: native::env::calibre_env_vars as *const u8,
        },
        // native str
        RuntimeSymbol {
            name: "calibre_str_char_lowercase",
            ptr: native::str::calibre_str_char_lowercase as *const u8,
        },
        RuntimeSymbol {
            name: "calibre_str_char_uppercase",
            ptr: native::str::calibre_str_char_uppercase as *const u8,
        },
        RuntimeSymbol {
            name: "calibre_str_split",
            ptr: native::str::calibre_str_split as *const u8,
        },
        RuntimeSymbol {
            name: "calibre_str_contains",
            ptr: native::str::calibre_str_contains as *const u8,
        },
        RuntimeSymbol {
            name: "calibre_str_starts_with",
            ptr: native::str::calibre_str_starts_with as *const u8,
        },
        RuntimeSymbol {
            name: "calibre_str_ends_with",
            ptr: native::str::calibre_str_ends_with as *const u8,
        },
        // native crypto
        RuntimeSymbol {
            name: "calibre_crypto_sha256",
            ptr: native::crypto::calibre_crypto_sha256 as *const u8,
        },
        RuntimeSymbol {
            name: "calibre_crypto_sha512",
            ptr: native::crypto::calibre_crypto_sha512 as *const u8,
        },
        RuntimeSymbol {
            name: "calibre_crypto_blake3",
            ptr: native::crypto::calibre_crypto_blake3 as *const u8,
        },
        // native libc
        RuntimeSymbol {
            name: "calibre_libc_get_c_errno",
            ptr: native::libc::calibre_libc_get_c_errno as *const u8,
        },
        RuntimeSymbol {
            name: "calibre_libc_get_c_errno_description",
            ptr: native::libc::calibre_libc_get_c_errno_description as *const u8,
        },
        RuntimeSymbol {
            name: "calibre_libc_set_c_errno",
            ptr: native::libc::calibre_libc_set_c_errno as *const u8,
        },
        // native list
        RuntimeSymbol {
            name: "calibre_list_raw_remove",
            ptr: native::list::calibre_list_raw_remove as *const u8,
        },
        RuntimeSymbol {
            name: "calibre_list_raw_insert",
            ptr: native::list::calibre_list_raw_insert as *const u8,
        },
        // native random
        RuntimeSymbol {
            name: "calibre_random_rand",
            ptr: native::random::calibre_random_rand as *const u8,
        },
        RuntimeSymbol {
            name: "calibre_random_seed",
            ptr: native::random::calibre_random_seed as *const u8,
        },
        // native regex
        RuntimeSymbol {
            name: "calibre_regex_is_match",
            ptr: native::regex::calibre_regex_is_match as *const u8,
        },
        RuntimeSymbol {
            name: "calibre_regex_find",
            ptr: native::regex::calibre_regex_find as *const u8,
        },
        RuntimeSymbol {
            name: "calibre_regex_replace",
            ptr: native::regex::calibre_regex_replace as *const u8,
        },
        // hasher
        RuntimeSymbol {
            name: "calibre_new_hasher",
            ptr: hasher::calibre_new_hasher as *const u8,
        },
        RuntimeSymbol {
            name: "calibre_hash_add",
            ptr: hasher::calibre_hash_add as *const u8,
        },
        RuntimeSymbol {
            name: "calibre_hash_finish",
            ptr: hasher::calibre_hash_finish as *const u8,
        },
        RuntimeSymbol {
            name: "calibre_memo_contains",
            ptr: hasher::calibre_memo_contains as *const u8,
        },
        RuntimeSymbol {
            name: "calibre_memo_get",
            ptr: hasher::calibre_memo_get as *const u8,
        },
        RuntimeSymbol {
            name: "calibre_memo_set",
            ptr: hasher::calibre_memo_set as *const u8,
        },
        // native fs
        RuntimeSymbol {
            name: "calibre_fs_path_new",
            ptr: fs::calibre_fs_path_new as *const u8,
        },
        RuntimeSymbol {
            name: "calibre_fs_path_as_str",
            ptr: fs::calibre_fs_path_as_str as *const u8,
        },
        RuntimeSymbol {
            name: "calibre_fs_path_exists",
            ptr: fs::calibre_fs_path_exists as *const u8,
        },
        RuntimeSymbol {
            name: "calibre_fs_path_is_file",
            ptr: fs::calibre_fs_path_is_file as *const u8,
        },
        RuntimeSymbol {
            name: "calibre_fs_path_is_dir",
            ptr: fs::calibre_fs_path_is_dir as *const u8,
        },
        RuntimeSymbol {
            name: "calibre_fs_path_canonicalize",
            ptr: fs::calibre_fs_path_canonicalize as *const u8,
        },
        RuntimeSymbol {
            name: "calibre_fs_path_parent",
            ptr: fs::calibre_fs_path_parent as *const u8,
        },
        RuntimeSymbol {
            name: "calibre_fs_path_file_name",
            ptr: fs::calibre_fs_path_file_name as *const u8,
        },
        RuntimeSymbol {
            name: "calibre_fs_path_extension",
            ptr: fs::calibre_fs_path_extension as *const u8,
        },
        RuntimeSymbol {
            name: "calibre_fs_path_stem",
            ptr: fs::calibre_fs_path_stem as *const u8,
        },
        RuntimeSymbol {
            name: "calibre_fs_path_join",
            ptr: fs::calibre_fs_path_join as *const u8,
        },
        RuntimeSymbol {
            name: "calibre_fs_path_with_extension",
            ptr: fs::calibre_fs_path_with_extension as *const u8,
        },
        RuntimeSymbol {
            name: "calibre_fs_path_with_file_name",
            ptr: fs::calibre_fs_path_with_file_name as *const u8,
        },
        RuntimeSymbol {
            name: "calibre_fs_path_read_dir",
            ptr: fs::calibre_fs_path_read_dir as *const u8,
        },
        RuntimeSymbol {
            name: "calibre_fs_direntry_path",
            ptr: fs::calibre_fs_direntry_path as *const u8,
        },
        RuntimeSymbol {
            name: "calibre_fs_direntry_file_name",
            ptr: fs::calibre_fs_direntry_file_name as *const u8,
        },
        RuntimeSymbol {
            name: "calibre_fs_direntry_file_type",
            ptr: fs::calibre_fs_direntry_file_type as *const u8,
        },
        RuntimeSymbol {
            name: "calibre_fs_direntry_metadata",
            ptr: fs::calibre_fs_direntry_metadata as *const u8,
        },
        RuntimeSymbol {
            name: "calibre_fs_filetype_is_file",
            ptr: fs::calibre_fs_filetype_is_file as *const u8,
        },
        RuntimeSymbol {
            name: "calibre_fs_filetype_is_dir",
            ptr: fs::calibre_fs_filetype_is_dir as *const u8,
        },
        RuntimeSymbol {
            name: "calibre_fs_filetype_is_symlink",
            ptr: fs::calibre_fs_filetype_is_symlink as *const u8,
        },
        RuntimeSymbol {
            name: "calibre_fs_metadata_is_file",
            ptr: fs::calibre_fs_metadata_is_file as *const u8,
        },
        RuntimeSymbol {
            name: "calibre_fs_metadata_is_dir",
            ptr: fs::calibre_fs_metadata_is_dir as *const u8,
        },
        RuntimeSymbol {
            name: "calibre_fs_metadata_len",
            ptr: fs::calibre_fs_metadata_len as *const u8,
        },
        RuntimeSymbol {
            name: "calibre_fs_metadata_modified",
            ptr: fs::calibre_fs_metadata_modified as *const u8,
        },
        RuntimeSymbol {
            name: "calibre_fs_metadata_created",
            ptr: fs::calibre_fs_metadata_created as *const u8,
        },
        RuntimeSymbol {
            name: "calibre_fs_metadata_accessed",
            ptr: fs::calibre_fs_metadata_accessed as *const u8,
        },
        RuntimeSymbol {
            name: "calibre_fs_metadata_is_readonly",
            ptr: fs::calibre_fs_metadata_is_readonly as *const u8,
        },
        RuntimeSymbol {
            name: "calibre_fs_file_open",
            ptr: fs::calibre_fs_file_open as *const u8,
        },
        RuntimeSymbol {
            name: "calibre_fs_file_close",
            ptr: fs::calibre_fs_file_close as *const u8,
        },
        RuntimeSymbol {
            name: "calibre_fs_file_write",
            ptr: fs::calibre_fs_file_write as *const u8,
        },
        RuntimeSymbol {
            name: "calibre_fs_file_write_line",
            ptr: fs::calibre_fs_file_write_line as *const u8,
        },
        RuntimeSymbol {
            name: "calibre_fs_file_read_all",
            ptr: fs::calibre_fs_file_read_all as *const u8,
        },
        RuntimeSymbol {
            name: "calibre_fs_file_flush",
            ptr: fs::calibre_fs_file_flush as *const u8,
        },
        RuntimeSymbol {
            name: "calibre_fs_dir_create",
            ptr: fs::calibre_fs_dir_create as *const u8,
        },
        RuntimeSymbol {
            name: "calibre_fs_dir_create_all",
            ptr: fs::calibre_fs_dir_create_all as *const u8,
        },
        RuntimeSymbol {
            name: "calibre_fs_dir_remove",
            ptr: fs::calibre_fs_dir_remove as *const u8,
        },
        RuntimeSymbol {
            name: "calibre_fs_dir_remove_all",
            ptr: fs::calibre_fs_dir_remove_all as *const u8,
        },
    ]
}
