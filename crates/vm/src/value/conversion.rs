use calibre_parser::ast::{ParserDataType, ParserInnerType};

use crate::{VM, error::RuntimeError, value::RuntimeValue};

impl RuntimeValue {
    pub fn convert(
        self,
        env: &VM,
        data_type: &ParserInnerType,
    ) -> Result<RuntimeValue, RuntimeError> {
        if data_type == &ParserInnerType::Dynamic {
            return Ok(self);
        }

        match (self, data_type) {
            (RuntimeValue::Int(x), ParserInnerType::Int) => Ok(RuntimeValue::Int(x)),
            (RuntimeValue::Int(x), ParserInnerType::Float) => Ok(RuntimeValue::Float(x as f64)),
            (RuntimeValue::Int(x), ParserInnerType::Char) => {
                Ok(RuntimeValue::Char((x as u8) as char))
            }
            (RuntimeValue::Int(x), ParserInnerType::Str) => Ok(RuntimeValue::Str(x.to_string())),
            (RuntimeValue::Float(x), ParserInnerType::Float) => Ok(RuntimeValue::Float(x)),
            (RuntimeValue::Float(x), ParserInnerType::Int) => Ok(RuntimeValue::Int(x as i64)),
            (RuntimeValue::Float(x), ParserInnerType::Char) => {
                Ok(RuntimeValue::Char((x as u8) as char))
            }
            (RuntimeValue::Float(x), ParserInnerType::Str) => Ok(RuntimeValue::Str(x.to_string())),
            (RuntimeValue::Range(from, to), ParserInnerType::Range) => {
                Ok(RuntimeValue::Range(from, to))
            }
            (RuntimeValue::Range(_, x), ParserInnerType::Int) => Ok(RuntimeValue::Int(x)),
            (RuntimeValue::Range(_, x), ParserInnerType::Float) => {
                Ok(RuntimeValue::Float(x as f64))
            }
            (RuntimeValue::Bool(x), ParserInnerType::Bool) => Ok(RuntimeValue::Bool(x)),
            (RuntimeValue::Bool(x), ParserInnerType::Int) => {
                Ok(RuntimeValue::Int(if x { 1 } else { 0 }))
            }
            (RuntimeValue::Bool(x), ParserInnerType::Float) => {
                Ok(RuntimeValue::Float(if x { 1.0 } else { 0.0 }))
            }
            (RuntimeValue::Bool(x), ParserInnerType::Str) => Ok(RuntimeValue::Str(x.to_string())),
            (RuntimeValue::Char(x), ParserInnerType::Char) => Ok(RuntimeValue::Char(x)),
            (RuntimeValue::Char(x), ParserInnerType::Int) => Ok(RuntimeValue::Int(x as i64)),
            (RuntimeValue::Char(x), ParserInnerType::Float) => {
                Ok(RuntimeValue::Float((x as u8) as f64))
            }
            (RuntimeValue::Char(x), ParserInnerType::Str) => Ok(RuntimeValue::Str(x.to_string())),
            (RuntimeValue::Str(x), ParserInnerType::Str) => Ok(RuntimeValue::Str(x)),
            (RuntimeValue::Str(x), ParserInnerType::Char) => {
                let ch = x.chars().next().ok_or_else(|| {
                    RuntimeError::CantConvert(RuntimeValue::Str(x.clone()), ParserInnerType::Char)
                })?;
                Ok(RuntimeValue::Char(ch))
            }
            (RuntimeValue::Null, ParserInnerType::Null) => Ok(RuntimeValue::Null),
            (RuntimeValue::Aggregate(Some(x), z), ParserInnerType::Struct(y)) if &x == y => {
                Ok(RuntimeValue::Aggregate(Some(x), z))
            }
            (RuntimeValue::Enum(x, z, w), ParserInnerType::Struct(y)) if &x == y => {
                Ok(RuntimeValue::Enum(x, z, w))
            }
            (RuntimeValue::Aggregate(None, x), ParserInnerType::Tuple(_)) => {
                Ok(RuntimeValue::Aggregate(None, x))
            }
            (RuntimeValue::List(data), ParserInnerType::List(t)) => {
                let mut lst = Vec::new();

                for d in data {
                    lst.push(d.convert(env, &t.data_type)?);
                }

                Ok(RuntimeValue::List(lst))
            }
            (x, ParserInnerType::List(t)) => {
                let x = x.convert(env, &t)?;
                Ok(RuntimeValue::List(vec![x]))
            }
            (RuntimeValue::Option(x), ParserInnerType::Option(t)) => {
                if let Some(x) = x {
                    let x = x.convert(env, &t)?;
                    Ok(RuntimeValue::Option(Some(Box::new(x))))
                } else {
                    Ok(RuntimeValue::Option(None))
                }
            }
            (x, ParserInnerType::Option(t)) => {
                let x = x.convert(env, &t)?;
                Ok(RuntimeValue::Option(Some(Box::new(x))))
            }
            (RuntimeValue::Result(x), ParserInnerType::Result { ok, err }) => match x {
                Ok(x) => {
                    let x = x.convert(env, &ok)?;
                    Ok(RuntimeValue::Result(Ok(Box::new(x))))
                }
                Err(x) => {
                    let x = x.convert(env, &err)?;
                    Ok(RuntimeValue::Result(Err(Box::new(x))))
                }
            },
            (x, ParserInnerType::Result { ok, err }) => {
                let x = x.convert(env, &ok)?;
                Ok(RuntimeValue::Result(Ok(Box::new(x))))
            }
            (x, t) => Err(RuntimeError::CantConvert(x, t.clone())),
        }
    }
}
