use crate::runtime::{
    scope::{
        Object, Scope,
        objects::{get_object, resolve_object},
    },
    values::{RuntimeType, RuntimeValue, ValueErr, helper::ObjectType},
};
use numbers::NumberValue;
use std::{cell::RefCell, collections::HashMap, mem::discriminant, rc::Rc};

pub mod check;
pub mod numbers;
pub mod similar;

impl RuntimeValue {
    pub fn into_type(
        &self,
        scope: Rc<RefCell<Scope>>,
        t: RuntimeType,
    ) -> Result<RuntimeValue, ValueErr> {
        if t == RuntimeType::Dynamic {
            return Ok(self.clone());
        }
        let typ = t.clone();
        let panic_type = || {
            return Err(ValueErr::Conversion(self.clone(), typ));
        };

        let result_case = || {
            if let RuntimeType::Result(x, y) = t.clone() {
                if self.is_type(scope.clone(), *x.clone())
                    || self.is_type(scope.clone(), *y.clone())
                {
                    return Ok(RuntimeValue::Result(Ok(Box::new(self.clone())), t.clone()));
                }

                if let Ok(data) = self.into_type(scope.clone(), *x.clone()) {
                    return Ok(RuntimeValue::Result(Ok(Box::new(data)), t.clone()));
                } else {
                    return Ok(RuntimeValue::Result(
                        Ok(Box::new(self.into_type(scope.clone(), *y.clone())?)),
                        t.clone(),
                    ));
                }
            }
            return Err(ValueErr::Conversion(self.clone(), t.clone()));
        };

        let option_case = || {
            if let RuntimeType::Option(x) = t.clone() {
                return Ok(RuntimeValue::Option(
                    Some(Box::new(self.into_type(scope.clone(), *x.clone())?)),
                    t.clone(),
                ));
            }
            return Err(ValueErr::Conversion(self.clone(), t.clone()));
        };

        let list_case = || {
            if let RuntimeType::List(x) = t.clone() {
                Ok(RuntimeValue::List {
                    data: vec![if let Some(x) = *x.clone() {
                        self.into_type(scope.clone(), x)?
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
                            valid.push(data[i].into_type(scope.clone(), data_types[i].clone())?);
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
                RuntimeType::Int => Ok(RuntimeValue::Int(x.parse().unwrap())),
                RuntimeType::UInt => Ok(RuntimeValue::UInt(x.parse().unwrap())),
                RuntimeType::Long => Ok(RuntimeValue::Long(x.parse().unwrap())),
                RuntimeType::ULong => Ok(RuntimeValue::ULong(x.parse().unwrap())),
                RuntimeType::Double => Ok(RuntimeValue::Double(x.parse().unwrap())),
                RuntimeType::Float => Ok(RuntimeValue::Float(x.parse().unwrap())),
                RuntimeType::Str => Ok(self.clone()),
                RuntimeType::Char => Ok(RuntimeValue::Char(x.chars().nth(0).unwrap())),
                RuntimeType::List(typ) if *typ == Some(RuntimeType::Char) => {
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
                RuntimeType::Struct(None) => {
                    if let Some(z) = z {
                        Ok(RuntimeValue::Struct(z.clone(), None))
                    } else {
                        panic_type()
                    }
                }
                RuntimeType::Struct(Some(y)) => {
                    if let Some(z) = z {
                        match RuntimeValue::Struct(z.clone(), None).into_type(scope.clone(), t) {
                            Ok(x) => Ok(x),
                            Err(_) => self.into_type(scope, RuntimeType::Enum(y)),
                        }
                    } else {
                        self.into_type(scope, RuntimeType::Enum(y))
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
                RuntimeType::Struct(None) => Ok(self.clone()),
                RuntimeType::Str => Ok(RuntimeValue::Str(self.to_string())),
                RuntimeType::Char => panic_type(),
                RuntimeType::List(_) => list_case(),
                RuntimeType::Result(_, _) => result_case(),
                RuntimeType::Option(_) => option_case(),
                RuntimeType::Range => panic_type(),
                RuntimeType::Struct(Some(identifier)) => {
                    let Object::Struct(ObjectType::Tuple(properties)) =
                        get_object(resolve_object(scope.clone(), &identifier)?, &identifier)?
                    else {
                        return panic_type();
                    };
                    let mut new_values = Vec::new();

                    for (i, property) in properties.iter().enumerate() {
                        if let Some(val) = x.get(i as usize) {
                            new_values.push(val.into_type(scope.clone(), property.clone())?);
                        } else {
                            return panic_type();
                        }
                    }

                    Ok(RuntimeValue::Struct(
                        ObjectType::Tuple(new_values),
                        Some(identifier),
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
                RuntimeType::Struct(None) => Ok(self.clone()),
                RuntimeType::Str => Ok(RuntimeValue::Str(self.to_string())),
                RuntimeType::Char => panic_type(),
                RuntimeType::List(_) => list_case(),
                RuntimeType::Result(_, _) => result_case(),
                RuntimeType::Option(_) => option_case(),
                RuntimeType::Range => panic_type(),
                RuntimeType::Struct(Some(identifier)) => {
                    let Object::Struct(ObjectType::Map(properties)) =
                        get_object(resolve_object(scope.clone(), &identifier)?, &identifier)?
                    else {
                        return panic_type();
                    };
                    let mut new_values = HashMap::new();

                    for property in &properties {
                        if let Some(val) = x.get(property.0) {
                            new_values.insert(
                                property.0.clone(),
                                val.into_type(scope.clone(), property.1.clone())?,
                            );
                        } else {
                            return panic_type();
                        }
                    }

                    Ok(RuntimeValue::Struct(
                        ObjectType::Map(new_values),
                        Some(identifier),
                    ))
                }
                RuntimeType::Function { .. } => match t {
                    RuntimeType::List(_) => list_case(),
                    _ => panic_type(),
                },
                _ => panic_type(),
            },
            RuntimeValue::Function {
                identifier: _,
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
                    if is_async != *val_is_async {
                        return panic_type();
                    };

                    if let Some(x) = *return_type {
                        if let Some(y) = val_type {
                            if x != *y {
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
                if let Some(RuntimeType::Struct(Some(x))) = *data_type.clone() {
                    if let RuntimeType::Struct(Some(ref y)) = t {
                        if &x == y {
                            return Ok(RuntimeValue::Struct(
                                ObjectType::Tuple(data.to_vec()),
                                Some(x),
                            ));
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
                            data_type: Box::new(Some(t)),
                        }
                    } else {
                        let mut dta = Vec::new();
                        for d in data {
                            dta.push(d.into_type(scope.clone(), t.clone())?);
                        }
                        RuntimeValue::List {
                            data: dta,
                            data_type: Box::new(Some(t)),
                        }
                    })
                } else {
                    Ok(RuntimeValue::List {
                        data: Vec::new(),
                        data_type: Box::new(Some(t)),
                    })
                }
            }
            RuntimeValue::Result(x, typ) => {
                if &t == typ {
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
                if x.is_none() || &t == typ {
                    Ok(RuntimeValue::Option(x.clone(), t))
                } else {
                    match x {
                        Some(x) => {
                            return x.into_type(scope, t);
                        }
                        None => panic_type(),
                    }
                }
            }
            _ => panic_type(),
        }
    }
}
