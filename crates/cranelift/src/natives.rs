use calibre_lir::{Key, MirDataType, TypeKey, VariableKey, environment::LirRegistry};
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
                    typed_return: Some(MirDataType::List(Box::new(MirDataType::Str))),
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
                "calibre_gc_collect",
                RuntimeFunction {
                    params: vec![],
                    returns: None,
                    typed_return: None,
                },
            ),
            (
                "calibre_gc_stats",
                RuntimeFunction {
                    params: vec![],
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
                "calibre_option_none",
                RuntimeFunction {
                    params: vec![],
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
                    typed_return: Some(MirDataType::Dynamic),
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

pub(crate) fn pointer_returning_functions() -> &'static [&'static str] {
    &[
        "calibre_option_some",
        "calibre_result_ok",
        "calibre_result_err",
        "calibre_option_none",
    ]
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
