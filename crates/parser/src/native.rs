use crate::ast::{ParserDataType, ParserInnerType, RefMutability};
use rustc_hash::FxHashMap;

impl ParserDataType {
    pub fn constants() -> FxHashMap<String, Self> {
        let lst: Vec<(&'static str, ParserInnerType)> = vec![
            ("true", ParserInnerType::Bool),
            ("false", ParserInnerType::Bool),
            (
                "none",
                ParserInnerType::Option(Box::new(ParserDataType::from(ParserInnerType::Dynamic))),
            ),
            ("INT_MIN", ParserInnerType::Int),
            ("INT_MAX", ParserInnerType::Int),
        ];

        let mut map = FxHashMap::default();

        for val in lst {
            map.insert(val.0.to_string(), ParserDataType::from(val.1));
        }

        map
    }

    pub fn natives() -> FxHashMap<String, ParserDataType> {
        let lst: Vec<(&'static str, ParserInnerType)> = vec![
            ("console_output", ParserInnerType::Null),
            (
                "ok",
                ParserInnerType::Result {
                    err: Box::new(ParserDataType::from(ParserInnerType::Dynamic)),
                    ok: Box::new(ParserDataType::from(ParserInnerType::Dynamic)),
                },
            ),
            (
                "err",
                ParserInnerType::Result {
                    err: Box::new(ParserDataType::from(ParserInnerType::Dynamic)),
                    ok: Box::new(ParserDataType::from(ParserInnerType::Dynamic)),
                },
            ),
            (
                "some",
                ParserInnerType::Option(Box::new(ParserDataType::from(ParserInnerType::Dynamic))),
            ),
            ("min_or_zero", ParserInnerType::Int),
            ("len", ParserInnerType::Int),
            ("panic", ParserInnerType::Null),
            ("tuple", ParserInnerType::Dynamic),
            ("trim", ParserInnerType::Str),
            (
                "str_split",
                ParserInnerType::List(Box::new(ParserDataType::from(ParserInnerType::Str))),
            ),
            ("str_contains", ParserInnerType::Bool),
            ("str_starts_with", ParserInnerType::Bool),
            ("str_ends_with", ParserInnerType::Bool),
            ("discriminant", ParserInnerType::Int),
            ("channel_new", ParserInnerType::Dynamic),
            ("channel_send", ParserInnerType::Null),
            (
                "channel_get",
                ParserInnerType::Option(Box::new(ParserDataType::from(ParserInnerType::Dynamic))),
            ),
            ("channel_close", ParserInnerType::Null),
            ("channel_closed", ParserInnerType::Bool),
            ("waitgroup_new", ParserInnerType::Dynamic),
            ("waitgroup_add", ParserInnerType::Null),
            ("waitgroup_done", ParserInnerType::Null),
            ("waitgroup_wait", ParserInnerType::Null),
            ("waitgroup_count", ParserInnerType::Int),
            ("mutex_new", ParserInnerType::Null),
            ("mutex_get", ParserInnerType::Null),
            ("mutex_set", ParserInnerType::Null),
            ("mutex_with", ParserInnerType::Null),
            (
                "mutex_write",
                ParserInnerType::Ref(
                    Box::new(ParserDataType::from(ParserInnerType::Dynamic)),
                    RefMutability::MutRef,
                ),
            ),
        ];

        let mut map = FxHashMap::default();

        for val in lst {
            map.insert(
                val.0.to_string(),
                ParserDataType::from(ParserInnerType::NativeFunction(Box::new(
                    ParserDataType::from(val.1),
                ))),
            );
        }

        map
    }
}
