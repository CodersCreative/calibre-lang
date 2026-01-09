use std::collections::HashMap;

use crate::ast::{ParserDataType, ParserInnerType};

impl ParserDataType {
    pub fn constants() -> std::collections::HashMap<String, Self> {
        let lst: Vec<(&'static str, ParserInnerType)> = vec![
            ("PI", ParserInnerType::Float),
            ("FLOAT_MAX", ParserInnerType::Float),
            ("INT_MAX", ParserInnerType::Int),
            ("FLOAT_MIN", ParserInnerType::Float),
            ("INT_MIN", ParserInnerType::Int),
            ("true", ParserInnerType::Bool),
            ("false", ParserInnerType::Bool),
            (
                "none",
                ParserInnerType::Option(Box::new(ParserDataType::from(ParserInnerType::Dynamic))),
            ),
        ];

        let mut map = HashMap::new();

        for val in lst {
            map.insert(val.0.to_string(), ParserDataType::from(val.1));
        }

        map
    }

    pub fn natives() -> HashMap<String, ParserDataType> {
        let lst: Vec<(&'static str, ParserInnerType)> = vec![
            ("print", ParserInnerType::Null),
            (
                "ok",
                ParserInnerType::Result(
                    Box::new(ParserDataType::from(ParserInnerType::Dynamic)),
                    Box::new(ParserDataType::from(ParserInnerType::Dynamic)),
                ),
            ),
            (
                "err",
                ParserInnerType::Result(
                    Box::new(ParserDataType::from(ParserInnerType::Dynamic)),
                    Box::new(ParserDataType::from(ParserInnerType::Dynamic)),
                ),
            ),
            (
                "some",
                ParserInnerType::Option(Box::new(ParserDataType::from(ParserInnerType::Dynamic))),
            ),
            ("len", ParserInnerType::Int),
            ("panic", ParserInnerType::Null),
            ("tuple", ParserInnerType::Dynamic),
            ("trim", ParserInnerType::Str),
            ("console.out", ParserInnerType::Null),
            ("console.input", ParserInnerType::Str),
            ("console.err", ParserInnerType::Null),
            ("console.clear", ParserInnerType::Null),
            ("thread.wait", ParserInnerType::Null),
            ("random.generate", ParserInnerType::Float),
            ("random.bool", ParserInnerType::Bool),
            ("random.ratio", ParserInnerType::Bool),
        ];

        let mut map = HashMap::new();

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
