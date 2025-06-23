use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::{
    ast::NodeType,
    runtime::{
        interpreter::{InterpreterErr, evaluate},
        scope::{Object, Scope, objects::get_object, variables::get_var},
        values::{RuntimeValue, helper::ObjectType},
    },
};

pub fn evaluate_struct_expression(
    obj: NodeType,
    scope: Rc<RefCell<Scope>>,
) -> Result<RuntimeValue, InterpreterErr> {
    if let NodeType::StructLiteral(props) = obj {
        if let ObjectType::Map(props) = props {
            let mut properties = HashMap::new();
            for (k, v) in props {
                let value = if let Some(value) = v {
                    evaluate(value, scope.clone())?
                } else {
                    get_var(scope.clone(), &k)?.0
                };

                properties.insert(k, value);
            }

            return Ok(RuntimeValue::Struct(ObjectType::Map(properties), None));
        } else if let ObjectType::Tuple(props) = props {
            let mut properties = Vec::new();
            for v in props {
                if let Some(value) = v {
                    properties.push(evaluate(value, scope.clone())?);
                }
            }
            return Ok(RuntimeValue::Struct(ObjectType::Tuple(properties), None));
        }
    }

    Ok(RuntimeValue::Null)
}

pub fn evaluate_enum_expression(
    exp: NodeType,
    scope: Rc<RefCell<Scope>>,
) -> Result<RuntimeValue, InterpreterErr> {
    if let NodeType::EnumExpression {
        identifier,
        value,
        data,
    } = exp
    {
        let Object::Enum(enm_class) = get_object(scope.clone(), &identifier)? else {
            panic!()
        };

        if let Some((i, enm)) = enm_class.iter().enumerate().find(|x| &x.1.0 == &value) {
            if let Some(ObjectType::Map(properties)) = &enm.1 {
                let mut data_vals = HashMap::new();
                if let Some(ObjectType::Map(data)) = data {
                    for (k, v) in data {
                        let value = if let Some(value) = v {
                            evaluate(value, scope.clone())?
                        } else {
                            get_var(scope.clone(), &k)?.0
                        };

                        data_vals.insert(k, value);
                    }
                }

                let mut new_data_vals = HashMap::new();
                for property in properties {
                    if let Some(val) = data_vals.get(property.0) {
                        new_data_vals.insert(
                            property.0.clone(),
                            val.into_type(scope.clone(), property.1.clone())?,
                        );
                    } else {
                        panic!();
                    }
                }

                let data = if new_data_vals.is_empty() {
                    None
                } else {
                    Some(ObjectType::Map(new_data_vals))
                };

                return Ok(RuntimeValue::Enum(identifier, i, data));
            } else if let Some(ObjectType::Tuple(properties)) = &enm.1 {
                let mut data_vals = Vec::new();

                if let Some(ObjectType::Tuple(data)) = data {
                    for v in data {
                        if let Some(value) = v {
                            data_vals.push(evaluate(value, scope.clone())?);
                        };
                    }
                }

                let mut new_data_vals = Vec::new();
                for (i, property) in properties.into_iter().enumerate() {
                    new_data_vals.push(data_vals[i].into_type(scope.clone(), property.clone())?);
                }

                let data = if new_data_vals.is_empty() {
                    None
                } else {
                    Some(ObjectType::Tuple(new_data_vals))
                };

                return Ok(RuntimeValue::Enum(identifier, i, data));
            }
            return Ok(RuntimeValue::Enum(identifier, i, None));
        } else {
            Err(InterpreterErr::UnexpectedEnumItem(identifier, value))
        }
    } else {
        Err(InterpreterErr::NotImplemented(exp))
    }
}
