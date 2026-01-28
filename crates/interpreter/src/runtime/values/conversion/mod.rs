use crate::runtime::{
    scope::InterpreterEnvironment,
    values::{RuntimeType, RuntimeValue},
};
use calibre_common::{environment::InterpreterFrom, errors::ValueErr};
use calibre_mir::environment::MiddleTypeDefType;
use calibre_parser::ast::ObjectMap;
use numbers::NumberValue;
use std::{collections::HashMap, mem::discriminant};

pub mod ast;
pub mod check;
pub mod numbers;

impl RuntimeValue {
    pub fn into_type(
        &self,
        env: &InterpreterEnvironment,
        t: &RuntimeType,
    ) -> Result<RuntimeValue, ValueErr<RuntimeValue, RuntimeType>> {
        if t == &RuntimeType::Dynamic {
            return Ok(self.clone());
        }
        let typ = t.clone();
        let panic_type = || {
            return Err(ValueErr::Conversion(self.clone(), typ));
        };

        let result_case = || {
            if let RuntimeType::Result { ok, err } = t.clone() {
                if self.is_type(env, &*ok) || self.is_type(env, &*err) {
                    return Ok(RuntimeValue::Result(Ok(Box::new(self.clone())), t.clone()));
                }

                if let Ok(data) = self.into_type(env, &*ok) {
                    return Ok(RuntimeValue::Result(Ok(Box::new(data)), t.clone()));
                } else {
                    return Ok(RuntimeValue::Result(
                        Err(Box::new(self.into_type(env, &*err)?)),
                        t.clone(),
                    ));
                }
            }
            return Err(ValueErr::Conversion(self.clone(), t.clone()));
        };

        let option_case = || {
            if let RuntimeType::Option(x) = t.clone() {
                return Ok(RuntimeValue::Option(
                    Some(Box::new(self.into_type(env, &*x)?)),
                    t.clone(),
                ));
            }
            return Err(ValueErr::Conversion(self.clone(), t.clone()));
        };

        let list_case = || {
            if let RuntimeType::List(x) = t.clone() {
                Ok(RuntimeValue::List {
                    data: vec![self.into_type(env, &x)?],
                    data_type: x.clone(),
                })
            } else {
                return Err(ValueErr::Conversion(self.clone(), t.clone()));
            }
        };

        macro_rules! number_cast {
            ($val:expr, $target:ident) => {
                match t {
                    RuntimeType::Float => Ok(RuntimeValue::Float($val.as_f64())),
                    RuntimeType::Int => Ok(RuntimeValue::Int($val.as_i64())),
                    RuntimeType::Bool => Ok(RuntimeValue::Bool($val.as_bool())),
                    RuntimeType::Str => Ok(RuntimeValue::Str($val.to_string())),
                    RuntimeType::Range => Ok(RuntimeValue::Range(0, $val.as_i64())),
                    RuntimeType::List(_) => list_case(),
                    RuntimeType::Result { .. } => result_case(),
                    RuntimeType::Option(_) => option_case(),
                    _ => panic_type(),
                }
            };
        }

        match self {
            RuntimeValue::Float(x) => number_cast!(x, t),
            RuntimeValue::Int(x) => number_cast!(x, t),
            RuntimeValue::Range(from, to) => match t {
                RuntimeType::Range => Ok(self.clone()),
                RuntimeType::Int => Ok(RuntimeValue::Int(*to as i64)),
                RuntimeType::Float => Ok(RuntimeValue::Float(*to as f64)),
                RuntimeType::List(_) => Ok(RuntimeValue::List {
                    data: (*from..*to)
                        .into_iter()
                        .map(|x| RuntimeValue::Int(x as i64))
                        .collect(),
                    data_type: Box::new(RuntimeType::Int),
                }),
                RuntimeType::Result { .. } => result_case(),
                RuntimeType::Option(_) => option_case(),
                _ => panic_type(),
            },
            RuntimeValue::Str(x) => match t {
                RuntimeType::Int => Ok(RuntimeValue::Int(x.parse()?)),
                RuntimeType::Float => Ok(RuntimeValue::Float(x.parse()?)),
                RuntimeType::Str => Ok(self.clone()),
                RuntimeType::Char => Ok(RuntimeValue::Char(x.chars().nth(0).unwrap_or(' '))),
                RuntimeType::List(typ) if **typ == RuntimeType::Char => Ok(RuntimeValue::List {
                    data: x.chars().map(|c| RuntimeValue::Char(c)).collect(),
                    data_type: Box::new(RuntimeType::Char),
                }),
                RuntimeType::List(_) => list_case(),
                RuntimeType::Result { .. } => result_case(),
                RuntimeType::Option(_) => option_case(),
                _ => panic_type(),
            },

            RuntimeValue::Null => panic_type(),
            RuntimeValue::Char(x) => {
                RuntimeValue::into_type(&RuntimeValue::Str(x.clone().to_string()), env, t)
            }
            RuntimeValue::Enum(o, _, z) => match t.clone() {
                RuntimeType::Struct(y) => {
                    if let Some(z) = z {
                        if z.is_type(env, &t) {
                            Ok(*z.clone())
                        } else {
                            panic_type()
                        }
                    } else {
                        self.into_type(env, &RuntimeType::Enum(y))
                    }
                }
                RuntimeType::Enum(y) => {
                    if o == &y {
                        Ok(self.clone())
                    } else {
                        panic_type()
                    }
                }
                RuntimeType::Str => Ok(RuntimeValue::Str(self.to_string())),
                RuntimeType::Char => panic_type(),
                RuntimeType::Range => panic_type(),
                RuntimeType::List(_) => list_case(),
                RuntimeType::Result { .. } => result_case(),
                RuntimeType::Option(_) => option_case(),
                _ => panic_type(),
            },

            RuntimeValue::Aggregate(_o, map) => match t {
                RuntimeType::Tuple(data_types) => {
                    if map.len() == data_types.len() {
                        let mut valid = Vec::new();
                        for i in 0..map.len() {
                            valid.push((
                                i.to_string(),
                                map.get(&i.to_string())
                                    .unwrap()
                                    .into_type(env, &data_types[i])?,
                            ));
                        }
                        Ok(RuntimeValue::Aggregate(None, ObjectMap(valid)))
                    } else {
                        panic_type()
                    }
                }
                RuntimeType::Str => Ok(RuntimeValue::Str(self.to_string())),
                RuntimeType::Char => panic_type(),
                RuntimeType::List(_) => list_case(),
                RuntimeType::Result { .. } => result_case(),
                RuntimeType::Option(_) => option_case(),
                RuntimeType::Range => panic_type(),
                RuntimeType::Struct(t_o) => {
                    let Some(MiddleTypeDefType::Struct(ObjectMap(properties))) =
                        env.objects.get(t_o).map(|x| &x.object_type)
                    else {
                        return panic_type();
                    };
                    let mut new_values = Vec::new();

                    for (i, property) in properties.iter() {
                        if let Some(val) = map.get(i) {
                            new_values.push((
                                i.to_string(),
                                val.into_type(
                                    env,
                                    &RuntimeType::interpreter_from(env, &0, property.clone())?,
                                )?,
                            ));
                        } else {
                            return panic_type();
                        }
                    }

                    Ok(RuntimeValue::Aggregate(
                        Some(t_o.clone()),
                        ObjectMap(new_values),
                    ))
                }
                RuntimeType::Function { .. } => match t {
                    RuntimeType::List(_) => list_case(),
                    RuntimeType::Result { .. } => result_case(),
                    RuntimeType::Option(_) => option_case(),
                    _ => panic_type(),
                },
                _ => panic_type(),
            },
            RuntimeValue::Function {
                parameters: val_parameters,
                body: _,
                return_type: val_type,
                is_async: val_is_async,
            } => match t {
                RuntimeType::Function {
                    return_type,
                    parameters,
                    is_async,
                } => {
                    if *is_async != *val_is_async {
                        return panic_type();
                    };

                    if **return_type != *val_type {
                        return panic_type();
                    }

                    if val_parameters.len() != parameters.len() {
                        return panic_type();
                    };

                    Ok(self.clone())
                }
                RuntimeType::Result { .. } => result_case(),
                RuntimeType::Option(_) => option_case(),
                _ => panic_type(),
            },
            RuntimeValue::List { data, data_type } => {
                let t = match t {
                    RuntimeType::Result { .. } => return result_case(),
                    RuntimeType::Option(_) => return option_case(),
                    RuntimeType::List(x) => *x.clone(),
                    _ => return panic_type(),
                };
                if let RuntimeType::Struct(o) = *data_type.clone() {
                    if let RuntimeType::Struct(w) = t.clone() {
                        if w == o {
                            return Ok(RuntimeValue::Aggregate(Some(w), data.to_vec().into()));
                        }
                    }
                }
                if data.len() > 0 {
                    let t2 = discriminant(&data[0]);
                    let filtered: Vec<&RuntimeValue> =
                        data.iter().filter(|x| discriminant(*x) == t2).collect();

                    Ok(if data.len() == filtered.len() {
                        RuntimeValue::List {
                            data: data.clone(),
                            data_type: Box::new(t.clone()),
                        }
                    } else {
                        let mut dta = Vec::new();
                        for d in data {
                            dta.push(d.into_type(env, &t)?);
                        }
                        RuntimeValue::List {
                            data: dta,
                            data_type: Box::new(t.clone()),
                        }
                    })
                } else {
                    Ok(RuntimeValue::List {
                        data: Vec::new(),
                        data_type: Box::new(t.clone()),
                    })
                }
            }
            RuntimeValue::Result(x, typ) => {
                if let Ok(x) = x {
                    x.into_type(env, t)
                } else if t == typ {
                    Ok(self.clone())
                } else {
                    match x {
                        Ok(x) => {
                            return x.into_type(env, t);
                        }
                        Err(x) => {
                            return x.into_type(env, t);
                        }
                    }
                }
            }
            RuntimeValue::Option(x, typ) => {
                if let Some(x) = x {
                    x.into_type(env, t)
                } else if t == &RuntimeType::Option(Box::new(typ.clone())) {
                    Ok(RuntimeValue::Option(x.clone(), t.clone()))
                } else {
                    match x {
                        Some(x) => {
                            return x.into_type(env, t);
                        }
                        None => panic_type(),
                    }
                }
            }
            RuntimeValue::Bool(x) => match t {
                RuntimeType::Bool => Ok(RuntimeValue::Bool(*x)),
                RuntimeType::List(_) => list_case(),
                RuntimeType::Result { .. } => result_case(),
                RuntimeType::Option(_) => option_case(),
                _ => RuntimeValue::Int(if *x { 1 } else { 0 }).into_type(env, t),
            },
            _ => panic_type(),
        }
    }
}
