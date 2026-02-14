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
                "str.split",
                ParserInnerType::List(Box::new(ParserDataType::from(ParserInnerType::Str))),
            ),
            ("str.contains", ParserInnerType::Bool),
            ("str.starts_with", ParserInnerType::Bool),
            ("str.ends_with", ParserInnerType::Bool),
            ("discriminant", ParserInnerType::Int),
            ("async.channel_new", ParserInnerType::Dynamic),
            ("async.channel_send", ParserInnerType::Null),
            (
                "async.channel_get",
                ParserInnerType::Option(Box::new(ParserDataType::from(ParserInnerType::Dynamic))),
            ),
            ("async.channel_close", ParserInnerType::Null),
            ("async.channel_closed", ParserInnerType::Bool),
            ("async.waitgroup_new", ParserInnerType::Dynamic),
            ("async.waitgroup_add", ParserInnerType::Null),
            ("async.waitgroup_done", ParserInnerType::Null),
            ("async.waitgroup_wait", ParserInnerType::Null),
            ("async.waitgroup_count", ParserInnerType::Int),
            ("async.mutex_new", ParserInnerType::Null),
            ("async.mutex_get", ParserInnerType::Null),
            ("async.mutex_set", ParserInnerType::Null),
            ("async.mutex_with", ParserInnerType::Null),
            (
                "async.mutex_write",
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
