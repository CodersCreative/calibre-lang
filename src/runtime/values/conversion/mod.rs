use crate::runtime::{
    scope::{
        Object, Scope,
        links::get_link_path,
        objects::{get_object, get_object_vec, resolve_object},
    },
    values::{RuntimeType, RuntimeValue, ValueErr, helper::ObjectType},
};
use numbers::NumberValue;
use rand::seq::IndexedRandom;
use std::{cell::RefCell, collections::HashMap, mem::discriminant, rc::Rc};

pub mod check;
pub mod numbers;
pub mod similar;

impl RuntimeValue {
    pub fn unwrap(&self, scope: &Rc<RefCell<Scope>>) -> Result<RuntimeValue, ValueErr> {
        match self {
            RuntimeValue::Result(Ok(x), _) => x.unwrap(scope),
            RuntimeValue::Result(Err(x), _) => x.unwrap(scope),
            RuntimeValue::Option(Some(x), _) => x.unwrap(scope),
            RuntimeValue::Link(path, _) => get_link_path(scope, path).unwrap().unwrap(scope),
            _ => Ok(self.clone()),
        }
    }
    pub fn into_type(
        &self,
        scope: &Rc<RefCell<Scope>>,
        t: &RuntimeType,
    ) -> Result<RuntimeValue, ValueErr> {
        if t == &RuntimeType::Dynamic {
            return Ok(self.clone());
        }
        let typ = t.clone();
        let panic_type = || {
            return Err(ValueErr::Conversion(self.clone(), typ));
        };

        let result_case = || {
            if let RuntimeType::Result(x, y) = t.clone() {
                if self.is_type(scope, &*x) || self.is_type(scope, &*y) {
                    return Ok(RuntimeValue::Result(Ok(Box::new(self.clone())), t.clone()));
                }

                if let Ok(data) = self.into_type(scope, &*x) {
                    return Ok(RuntimeValue::Result(Ok(Box::new(data)), t.clone()));
                } else {
                    return Ok(RuntimeValue::Result(
                        Ok(Box::new(self.into_type(scope, &*y)?)),
                        t.clone(),
                    ));
                }
            }
            return Err(ValueErr::Conversion(self.clone(), t.clone()));
        };

        let option_case = || {
            if let RuntimeType::Option(x) = t.clone() {
                return Ok(RuntimeValue::Option(
                    Some(Box::new(self.into_type(scope, &*x)?)),
                    t.clone(),
                ));
            }
            return Err(ValueErr::Conversion(self.clone(), t.clone()));
        };

        let list_case = || {
            if let RuntimeType::List(x) = t.clone() {
                Ok(RuntimeValue::List {
                    data: vec![if let Some(x) = &*x {
                        self.into_type(scope, &x)?
                    } else {
                        self.clone()
                    }],
                    data_type: x,
                })
            } else {
                return Err(ValueErr::Conversion(self.clone(), t.clone()));
            }
        };

        macro_rules! number_cast {
            ($val:expr, $target:ident) => {
                match t {
                    RuntimeType::Float => Ok(RuntimeValue::Float($val.as_f32())),
                    RuntimeType::Double => Ok(RuntimeValue::Double($val.as_f64())),
                    RuntimeType::Int => Ok(RuntimeValue::Int($val.as_i64())),
                    RuntimeType::Long => Ok(RuntimeValue::Long($val.as_i128())),
                    RuntimeType::UInt => Ok(RuntimeValue::UInt($val.as_u64())),
                    RuntimeType::ULong => Ok(RuntimeValue::ULong($val.as_u128())),
                    RuntimeType::Bool => Ok(RuntimeValue::Bool($val.as_bool())),
                    RuntimeType::Str => Ok(RuntimeValue::Str($val.to_string())),
                    RuntimeType::Range => Ok(RuntimeValue::Range(0, $val.as_i32())),
                    RuntimeType::List(_) => list_case(),
                    RuntimeType::Result(_, _) => result_case(),
                    RuntimeType::Option(_) => option_case(),
                    _ => panic_type(),
                }
            };
        }

        match self {
            RuntimeValue::Float(x) => number_cast!(x, t),
            RuntimeValue::Double(x) => number_cast!(x, t),
            RuntimeValue::Int(x) => number_cast!(x, t),
            RuntimeValue::Long(x) => number_cast!(x, t),
            RuntimeValue::UInt(x) => number_cast!(x, t),
            RuntimeValue::ULong(x) => number_cast!(x, t),
            RuntimeValue::Tuple(data) => match t {
                RuntimeType::Tuple(data_types) => {
                    if data.len() == data_types.len() {
                        let mut valid = Vec::new();
                        for i in 0..data.len() {
                            valid.push(data[i].into_type(scope, &data_types[i])?);
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
                RuntimeType::UInt => Ok(RuntimeValue::UInt(*to as u64)),
                RuntimeType::Long => Ok(RuntimeValue::Long(*to as i128)),
                RuntimeType::ULong => Ok(RuntimeValue::ULong(*to as u128)),
                RuntimeType::Float => Ok(RuntimeValue::Float(*to as f32)),
                RuntimeType::Double => Ok(RuntimeValue::Double(*to as f64)),
                RuntimeType::List(_) => Ok(RuntimeValue::List {
                    data: (*from..*to)
                        .into_iter()
                        .map(|x| RuntimeValue::Int(x as i64))
                        .collect(),
                    data_type: Box::new(Some(RuntimeType::Int)),
                }),
                RuntimeType::Result(_, _) => result_case(),
                RuntimeType::Option(_) => option_case(),
                _ => panic_type(),
            },
            RuntimeValue::Str(x) => match t {
                RuntimeType::Int => Ok(RuntimeValue::Int(x.parse()?)),
                RuntimeType::UInt => Ok(RuntimeValue::UInt(x.parse()?)),
                RuntimeType::Long => Ok(RuntimeValue::Long(x.parse()?)),
                RuntimeType::ULong => Ok(RuntimeValue::ULong(x.parse()?)),
                RuntimeType::Double => Ok(RuntimeValue::Double(x.parse()?)),
                RuntimeType::Float => Ok(RuntimeValue::Float(x.parse()?)),
                RuntimeType::Str => Ok(self.clone()),
                RuntimeType::Char => Ok(RuntimeValue::Char(x.chars().nth(0).unwrap_or(' '))),
                RuntimeType::List(typ) if **typ == Some(RuntimeType::Char) => {
                    Ok(RuntimeValue::List {
                        data: x.chars().map(|c| RuntimeValue::Char(c)).collect(),
                        data_type: Box::new(Some(RuntimeType::Char)),
                    })
                }
                RuntimeType::List(_) => list_case(),
                RuntimeType::Result(_, _) => result_case(),
                RuntimeType::Option(_) => option_case(),
                _ => panic_type(),
            },

            RuntimeValue::Null => panic_type(),
            RuntimeValue::Char(x) => {
                RuntimeValue::into_type(&RuntimeValue::Str(x.clone().to_string()), scope, t)
            }
            RuntimeValue::Enum(x, _, z) => match t.clone() {
                RuntimeType::Struct(x) if x.is_empty() => {
                    if let Some(z) = z {
                        Ok(RuntimeValue::Struct(z.clone(), Vec::new()))
                    } else {
                        panic_type()
                    }
                }
                RuntimeType::Struct(y) => {
                    if let Some(z) = z {
                        match RuntimeValue::Struct(z.clone(), Vec::new()).into_type(scope, t) {
                            Ok(x) => Ok(x),
                            Err(_) => self.into_type(scope, &RuntimeType::Enum(y)),
                        }
                    } else {
                        self.into_type(scope, &RuntimeType::Enum(y))
                    }
                }
                RuntimeType::Enum(y) => {
                    if x == &y {
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
            RuntimeValue::Struct(ObjectType::Tuple(x), _) => match t {
                RuntimeType::Struct(x) if x.is_empty() => Ok(self.clone()),
                RuntimeType::Str => Ok(RuntimeValue::Str(self.to_string())),
                RuntimeType::Char => panic_type(),
                RuntimeType::List(_) => list_case(),
                RuntimeType::Result(_, _) => result_case(),
                RuntimeType::Option(_) => option_case(),
                RuntimeType::Range => panic_type(),
                RuntimeType::Struct(identifier) => {
                    let Object::Struct(ObjectType::Tuple(properties)) =
                        get_object_vec(&scope, &identifier)?
                    else {
                        return panic_type();
                    };
                    let mut new_values = Vec::new();

                    for (i, property) in properties.iter().enumerate() {
                        if let Some(val) = x.get(i as usize) {
                            new_values.push(val.into_type(scope, &property)?);
                        } else {
                            return panic_type();
                        }
                    }

                    Ok(RuntimeValue::Struct(
                        ObjectType::Tuple(new_values),
                        identifier.to_vec(),
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
            RuntimeValue::Struct(ObjectType::Map(x), _) => match t {
                RuntimeType::Struct(x) if x.is_empty() => Ok(self.clone()),
                RuntimeType::Str => Ok(RuntimeValue::Str(self.to_string())),
                RuntimeType::Char => panic_type(),
                RuntimeType::List(_) => list_case(),
                RuntimeType::Result(_, _) => result_case(),
                RuntimeType::Option(_) => option_case(),
                RuntimeType::Range => panic_type(),
                RuntimeType::Struct(identifier) => {
                    let Object::Struct(ObjectType::Map(properties)) =
                        get_object_vec(&scope, &identifier)?
                    else {
                        return panic_type();
                    };
                    let mut new_values = HashMap::new();

                    for property in &properties {
                        if let Some(val) = x.get(property.0) {
                            new_values
                                .insert(property.0.clone(), val.into_type(scope, &property.1)?);
                        } else {
                            return panic_type();
                        }
                    }

                    Ok(RuntimeValue::Struct(
                        ObjectType::Map(new_values),
                        identifier.to_vec(),
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

                    if let Some(x) = &**return_type {
                        if let Some(y) = val_type {
                            if x != y {
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
                match t {
                    RuntimeType::Result(_, _) => return result_case(),
                    RuntimeType::Option(_) => return option_case(),
                    _ => {}
                }
                if let Some(RuntimeType::Struct(x)) = *data_type.clone() {
                    if let RuntimeType::Struct(y) = t {
                        if &x == y {
                            return Ok(RuntimeValue::Struct(ObjectType::Tuple(data.to_vec()), x));
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
                            data_type: Box::new(Some(t.clone())),
                        }
                    } else {
                        let mut dta = Vec::new();
                        for d in data {
                            dta.push(d.into_type(scope, &t)?);
                        }
                        RuntimeValue::List {
                            data: dta,
                            data_type: Box::new(Some(t.clone())),
                        }
                    })
                } else {
                    Ok(RuntimeValue::List {
                        data: Vec::new(),
                        data_type: Box::new(Some(t.clone())),
                    })
                }
            }
            RuntimeValue::Result(x, typ) => {
                if let Ok(x) = x {
                    x.into_type(scope, t)
                } else if t == typ {
                    Ok(self.clone())
                } else {
                    match x {
                        Ok(x) => {
                            return x.into_type(scope, t);
                        }
                        Err(x) => {
                            return x.into_type(scope, t);
                        }
                    }
                }
            }
            RuntimeValue::Option(x, typ) => {
                if let Some(x) = x {
                    x.into_type(scope, t)
                } else if t == typ {
                    Ok(RuntimeValue::Option(x.clone(), t.clone()))
                } else {
                    match x {
                        Some(x) => {
                            return x.into_type(scope, t);
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
                _ => RuntimeValue::UInt(if *x { 1 } else { 0 }).into_type(scope, t),
            },
            _ => panic_type(),
        }
    }
}
