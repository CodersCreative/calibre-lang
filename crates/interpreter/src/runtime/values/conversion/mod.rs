use crate::runtime::{
    scope::InterpreterEnvironment,
    values::{RuntimeType, RuntimeValue},
};
use calibre_common::{environment::Type, errors::ValueErr};
use calibre_parser::ast::ObjectType;
use numbers::NumberValue;
use std::{collections::HashMap, mem::discriminant};

pub mod check;
pub mod numbers;

impl RuntimeValue {
    pub fn into_type(
        &self,
        env: &InterpreterEnvironment,
        scope: &u64,
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
            if let RuntimeType::Result(x, y) = t.clone() {
                if self.is_type(env, scope, &*x) || self.is_type(env, scope, &*y) {
                    return Ok(RuntimeValue::Result(Ok(Box::new(self.clone())), t.clone()));
                }

                if let Ok(data) = self.into_type(env, scope, &*x) {
                    return Ok(RuntimeValue::Result(Ok(Box::new(data)), t.clone()));
                } else {
                    return Ok(RuntimeValue::Result(
                        Ok(Box::new(self.into_type(env, scope, &*y)?)),
                        t.clone(),
                    ));
                }
            }
            return Err(ValueErr::Conversion(self.clone(), t.clone()));
        };

        let option_case = || {
            if let RuntimeType::Option(x) = t.clone() {
                return Ok(RuntimeValue::Option(
                    Some(Box::new(self.into_type(env, scope, &*x)?)),
                    t.clone(),
                ));
            }
            return Err(ValueErr::Conversion(self.clone(), t.clone()));
        };

        let list_case = || {
            if let RuntimeType::List(x) = t.clone() {
                Ok(RuntimeValue::List {
                    data: vec![if let Some(x) = x.clone() {
                        self.into_type(env, scope, &x)?
                    } else {
                        self.clone()
                    }],
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
                    RuntimeType::Result(_, _) => result_case(),
                    RuntimeType::Option(_) => option_case(),
                    _ => panic_type(),
                }
            };
        }

        match self {
            RuntimeValue::Float(x) => number_cast!(x, t),
            RuntimeValue::Int(x) => number_cast!(x, t),
            RuntimeValue::Tuple(data) => match t {
                RuntimeType::Tuple(data_types) => {
                    if data.len() == data_types.len() {
                        let mut valid = Vec::new();
                        for i in 0..data.len() {
                            valid.push(data[i].into_type(env, scope, &data_types[i])?);
                        }
                        Ok(RuntimeValue::Tuple(valid))
                    } else {
                        panic_type()
                    }
                }
                _ => panic_type(),
            },
            RuntimeValue::Range(from, to) => match t {
                RuntimeType::Range => Ok(self.clone()),
                RuntimeType::Int => Ok(RuntimeValue::Int(*to as i64)),
                RuntimeType::Float => Ok(RuntimeValue::Float(*to as f64)),
                RuntimeType::List(_) => Ok(RuntimeValue::List {
                    data: (*from..*to)
                        .into_iter()
                        .map(|x| RuntimeValue::Int(x as i64))
                        .collect(),
                    data_type: Some(Box::new(RuntimeType::Int)),
                }),
                RuntimeType::Result(_, _) => result_case(),
                RuntimeType::Option(_) => option_case(),
                _ => panic_type(),
            },
            RuntimeValue::Str(x) => match t {
                RuntimeType::Int => Ok(RuntimeValue::Int(x.parse()?)),
                RuntimeType::Float => Ok(RuntimeValue::Float(x.parse()?)),
                RuntimeType::Str => Ok(self.clone()),
                RuntimeType::Char => Ok(RuntimeValue::Char(x.chars().nth(0).unwrap_or(' '))),
                RuntimeType::List(Some(typ)) if **typ == RuntimeType::Char => {
                    Ok(RuntimeValue::List {
                        data: x.chars().map(|c| RuntimeValue::Char(c)).collect(),
                        data_type: Some(Box::new(RuntimeType::Char)),
                    })
                }
                RuntimeType::List(_) => list_case(),
                RuntimeType::Result(_, _) => result_case(),
                RuntimeType::Option(_) => option_case(),
                _ => panic_type(),
            },

            RuntimeValue::Null => panic_type(),
            RuntimeValue::Char(x) => {
                RuntimeValue::into_type(&RuntimeValue::Str(x.clone().to_string()), env, scope, t)
            }
            RuntimeValue::Enum(o, _, z) => match t.clone() {
                RuntimeType::Struct(None) => {
                    if let Some(z) = z {
                        Ok(RuntimeValue::Struct(None, z.clone()))
                    } else {
                        panic_type()
                    }
                }
                RuntimeType::Struct(Some(y)) => {
                    if let Some(z) = z {
                        match RuntimeValue::Struct(None, z.clone()).into_type(env, scope, t) {
                            Ok(x) => Ok(x),
                            Err(_) => self.into_type(env, scope, &RuntimeType::Enum(y)),
                        }
                    } else {
                        self.into_type(env, scope, &RuntimeType::Enum(y))
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
                RuntimeType::Result(_, _) => result_case(),
                RuntimeType::Option(_) => option_case(),
                _ => panic_type(),
            },
            RuntimeValue::Struct(_o, ObjectType::Tuple(x)) => match t {
                RuntimeType::Struct(None) => Ok(self.clone()),
                RuntimeType::Str => Ok(RuntimeValue::Str(self.to_string())),
                RuntimeType::Char => panic_type(),
                RuntimeType::List(_) => list_case(),
                RuntimeType::Result(_, _) => result_case(),
                RuntimeType::Option(_) => option_case(),
                RuntimeType::Range => panic_type(),
                RuntimeType::Struct(Some(t_o)) => {
                    let Some(Type::Struct(ObjectType::Tuple(properties))) =
                        env.objects.get(t_o).map(|x| &x.object_type)
                    else {
                        return panic_type();
                    };
                    let mut new_values = Vec::new();

                    for (i, property) in properties.iter().enumerate() {
                        if let Some(val) = x.get(i as usize) {
                            new_values.push(val.into_type(env, scope, &property)?);
                        } else {
                            return panic_type();
                        }
                    }

                    Ok(RuntimeValue::Struct(
                        Some(t_o.clone()),
                        ObjectType::Tuple(new_values),
                    ))
                }
                RuntimeType::Function { .. } => match t {
                    RuntimeType::List(_) => list_case(),
                    RuntimeType::Result(_, _) => result_case(),
                    RuntimeType::Option(_) => option_case(),
                    _ => panic_type(),
                },
                _ => panic_type(),
            },
            RuntimeValue::Struct(_o, ObjectType::Map(x)) => match t {
                RuntimeType::Struct(None) => Ok(self.clone()),
                RuntimeType::Str => Ok(RuntimeValue::Str(self.to_string())),
                RuntimeType::Char => panic_type(),
                RuntimeType::List(_) => list_case(),
                RuntimeType::Result(_, _) => result_case(),
                RuntimeType::Option(_) => option_case(),
                RuntimeType::Range => panic_type(),
                RuntimeType::Struct(Some(t_o)) => {
                    let Ok(Type::Struct(ObjectType::Map(properties))) = env.get_object_type(t_o)
                    else {
                        return panic_type();
                    };
                    let mut new_values = HashMap::new();

                    for property in properties.iter() {
                        if let Some(val) = x.get(property.0) {
                            new_values.insert(
                                property.0.clone(),
                                val.into_type(env, scope, &property.1)?,
                            );
                        } else {
                            return panic_type();
                        }
                    }

                    Ok(RuntimeValue::Struct(
                        Some(*t_o),
                        ObjectType::Map(new_values),
                    ))
                }
                RuntimeType::Function { .. } => match t {
                    RuntimeType::List(_) => list_case(),
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

                    if let Some(x) = return_type {
                        if let Some(y) = val_type {
                            if **x != *y {
                                return panic_type();
                            }
                        } else {
                            return panic_type();
                        }
                    }

                    if val_parameters.len() != parameters.len() {
                        return panic_type();
                    };

                    Ok(self.clone())
                }
                RuntimeType::Result(_, _) => result_case(),
                RuntimeType::Option(_) => option_case(),
                _ => panic_type(),
            },
            RuntimeValue::List { data, data_type } => {
                let t = match t {
                    RuntimeType::Result(_, _) => return result_case(),
                    RuntimeType::Option(_) => return option_case(),
                    RuntimeType::List(Some(x)) => *x.clone(),
                    RuntimeType::List(None) => return Ok(self.clone()),
                    _ => return panic_type(),
                };
                if let Some(RuntimeType::Struct(o)) = data_type.clone().map(|x| *x) {
                    if let RuntimeType::Struct(w) = t {
                        if w == o {
                            return Ok(RuntimeValue::Struct(w, ObjectType::Tuple(data.to_vec())));
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
                            data_type: Some(Box::new(t.clone())),
                        }
                    } else {
                        let mut dta = Vec::new();
                        for d in data {
                            dta.push(d.into_type(env, scope, &t)?);
                        }
                        RuntimeValue::List {
                            data: dta,
                            data_type: Some(Box::new(t.clone())),
                        }
                    })
                } else {
                    Ok(RuntimeValue::List {
                        data: Vec::new(),
                        data_type: Some(Box::new(t.clone())),
                    })
                }
            }
            RuntimeValue::Result(x, typ) => {
                if let Ok(x) = x {
                    x.into_type(env, scope, t)
                } else if t == typ {
                    Ok(self.clone())
                } else {
                    match x {
                        Ok(x) => {
                            return x.into_type(env, scope, t);
                        }
                        Err(x) => {
                            return x.into_type(env, scope, t);
                        }
                    }
                }
            }
            RuntimeValue::Option(x, typ) => {
                if let Some(x) = x {
                    x.into_type(env, scope, t)
                } else if t == &RuntimeType::Option(Box::new(typ.clone())) {
                    Ok(RuntimeValue::Option(x.clone(), t.clone()))
                } else {
                    match x {
                        Some(x) => {
                            return x.into_type(env, scope, t);
                        }
                        None => panic_type(),
                    }
                }
            }
            RuntimeValue::Bool(x) => match t {
                RuntimeType::Bool => Ok(RuntimeValue::Bool(*x)),
                RuntimeType::List(_) => list_case(),
                RuntimeType::Result(_, _) => result_case(),
                RuntimeType::Option(_) => option_case(),
                _ => RuntimeValue::Int(if *x { 1 } else { 0 }).into_type(env, scope, t),
            },
            _ => panic_type(),
        }
    }
}
