use calibre_lir::{Key, VariableKey, environment::LirRegistry};
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
    pub returns: Vec<Type>,
}

pub(crate) fn runtime_functions() -> &'static FxHashMap<&'static str, RuntimeFunction> {
    static SIGNATURES: OnceLock<FxHashMap<&'static str, RuntimeFunction>> = OnceLock::new();

    SIGNATURES.get_or_init(|| {
        let lst = [
            (
                "calibre_gc_alloc_list",
                RuntimeFunction {
                    params: vec![(types::I64, ArgumentKind::Direct)],
                    returns: vec![types::I64],
                },
            ),
            (
                "calibre_gc_alloc_string",
                RuntimeFunction {
                    params: vec![(types::I64, ArgumentKind::Direct)],
                    returns: vec![types::I64],
                },
            ),
            (
                "calibre_gc_alloc_aggregate",
                RuntimeFunction {
                    params: vec![(types::I64, ArgumentKind::Direct)],
                    returns: vec![types::I64],
                },
            ),
            (
                "calibre_gc_collect",
                RuntimeFunction {
                    params: vec![],
                    returns: vec![],
                },
            ),
            (
                "calibre_gc_stats",
                RuntimeFunction {
                    params: vec![],
                    returns: vec![],
                },
            ),
            (
                "calibre_gc_add_root",
                RuntimeFunction {
                    params: vec![(types::I64, ArgumentKind::Direct)],
                    returns: vec![],
                },
            ),
            (
                "calibre_gc_remove_root",
                RuntimeFunction {
                    params: vec![(types::I64, ArgumentKind::Direct)],
                    returns: vec![],
                },
            ),
            (
                "calibre_option_none",
                RuntimeFunction {
                    params: vec![],
                    returns: vec![types::I64],
                },
            ),
            (
                "calibre_option_some",
                RuntimeFunction {
                    params: vec![(types::I64, ArgumentKind::TagPayload)],
                    returns: vec![types::I64],
                },
            ),
            (
                "calibre_option_is_some",
                RuntimeFunction {
                    params: vec![(types::I64, ArgumentKind::Direct)],
                    returns: vec![types::I8],
                },
            ),
            (
                "calibre_option_is_none",
                RuntimeFunction {
                    params: vec![(types::I64, ArgumentKind::Direct)],
                    returns: vec![types::I8],
                },
            ),
            (
                "calibre_option_unwrap",
                RuntimeFunction {
                    params: vec![(types::I64, ArgumentKind::Direct)],
                    returns: vec![types::I64],
                },
            ),
            (
                "calibre_result_ok",
                RuntimeFunction {
                    params: vec![(types::I64, ArgumentKind::TagPayload)],
                    returns: vec![types::I64],
                },
            ),
            (
                "calibre_result_err",
                RuntimeFunction {
                    params: vec![(types::I64, ArgumentKind::TagPayload)],
                    returns: vec![types::I64],
                },
            ),
            (
                "calibre_result_is_ok",
                RuntimeFunction {
                    params: vec![(types::I64, ArgumentKind::Direct)],
                    returns: vec![types::I8],
                },
            ),
            (
                "calibre_result_is_err",
                RuntimeFunction {
                    params: vec![(types::I64, ArgumentKind::Direct)],
                    returns: vec![types::I8],
                },
            ),
            (
                "calibre_result_unwrap_ok",
                RuntimeFunction {
                    params: vec![(types::I64, ArgumentKind::Direct)],
                    returns: vec![types::I64],
                },
            ),
            (
                "calibre_result_unwrap_err",
                RuntimeFunction {
                    params: vec![(types::I64, ArgumentKind::Direct)],
                    returns: vec![types::I64],
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
