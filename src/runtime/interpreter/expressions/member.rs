use std::{cell::RefCell, rc::Rc};

use crate::{
    ast::NodeType,
    runtime::{
        interpreter::{InterpreterErr, evaluate, expressions::call::evaluate_function},
        scope::{
            Object, Scope, ScopeErr,
            objects::{get_function, get_object},
            variables::get_var,
        },
        values::{RuntimeType, RuntimeValue, ValueErr, helper::ObjectType},
    },
};

pub fn assign_member_expression(
    member: NodeType,
    value: RuntimeValue,
    scope: Rc<RefCell<Scope>>,
) -> Result<(), InterpreterErr> {
    match member {
        NodeType::MemberExpression {
            object, property, ..
        } => {
            let mut object_val = evaluate(*object.clone(), scope.clone())?;
            match *property {
                NodeType::MemberExpression { .. } => {
                    assign_member_expression(*property, value, scope)
                }
                NodeType::Identifier(prop) => {
                    if let RuntimeValue::Struct(ObjectType::Map(ref mut map), _) = object_val {
                        if let Some(field) = map.get_mut(&prop) {
                            *field = value;
                            if let NodeType::Identifier(obj_name) = *object {
                                scope.borrow_mut().assign_var(&obj_name, object_val)?;
                            }
                            Ok(())
                        } else {
                            Err(InterpreterErr::Value(ValueErr::Scope(ScopeErr::Variable(
                                prop,
                            ))))
                        }
                    } else if let RuntimeValue::List { ref mut data, .. } = object_val {
                        let idx_val = evaluate(NodeType::Identifier(prop.clone()), scope.clone())?
                            .into_type(scope.clone(), RuntimeType::Integer)?;
                        if let RuntimeValue::Integer(idx) = idx_val {
                            if let Some(elem) = data.get_mut(idx as usize) {
                                *elem = value;
                                if let NodeType::Identifier(obj_name) = *object {
                                    scope.borrow_mut().assign_var(&obj_name, object_val)?;
                                }
                                Ok(())
                            } else {
                                Err(InterpreterErr::IndexNonList(NodeType::Identifier(prop)))
                            }
                        } else {
                            Err(InterpreterErr::IndexNonList(NodeType::Identifier(prop)))
                        }
                    } else {
                        Err(InterpreterErr::UnexpectedType(object_val))
                    }
                }
                _ => Err(InterpreterErr::NotImplemented(*property)),
            }
        }
        _ => Err(InterpreterErr::NotImplemented(member)),
    }
}

pub fn evaluate_member_expression(
    exp: NodeType,
    scope: Rc<RefCell<Scope>>,
) -> Result<RuntimeValue, InterpreterErr> {
    match exp {
        NodeType::MemberExpression {
            object, property, ..
        } => {
            let object_val = match *object.clone() {
                NodeType::Identifier(object_name) => {
                    if let Ok(var) = get_var(scope.clone(), &object_name) {
                        var.0
                    } else if let Ok(obj) = get_object(scope.clone(), &object_name) {
                        match obj {
                            Object::Enum(_) => {
                                if let NodeType::Identifier(value) = *property {
                                    return evaluate(
                                        NodeType::EnumExpression {
                                            identifier: object_name,
                                            value,
                                            data: None,
                                        },
                                        scope,
                                    );
                                } else {
                                    return Err(InterpreterErr::UnexpectedNode(*property));
                                }
                            }
                            Object::Struct(_) => {
                                if let NodeType::CallExpression(caller, args) = *property {
                                    if let NodeType::Identifier(ref method_name) = *caller {
                                        if let Ok(val) =
                                            get_function(scope.clone(), &object_name, method_name)
                                        {
                                            return evaluate_function(scope, val.0, *args);
                                        } else {
                                            return Err(InterpreterErr::Value(ValueErr::Scope(
                                                ScopeErr::Function(method_name.to_string()),
                                            )));
                                        }
                                    } else {
                                        return Err(InterpreterErr::UnexpectedNode(*caller));
                                    }
                                } else {
                                    return Err(InterpreterErr::UnexpectedNode(*property));
                                }
                            }
                            _ => todo!(),
                        }
                    } else {
                        return Err(InterpreterErr::Value(ValueErr::Scope(ScopeErr::Variable(
                            object_name,
                        ))));
                    }
                }
                _ => return Err(InterpreterErr::UnexpectedNode(*object)),
            };

            match *property {
                NodeType::MemberExpression { .. } => evaluate_member_expression(*property, scope),
                NodeType::Identifier(ref prop) => {
                    if let RuntimeValue::Struct(ObjectType::Map(ref map), _) = object_val {
                        if let Some(val) = map.get(prop) {
                            Ok(val.clone())
                        } else {
                            Err(InterpreterErr::Value(ValueErr::Scope(ScopeErr::Variable(
                                prop.clone(),
                            ))))
                        }
                    } else if let RuntimeValue::List { ref data, .. } = object_val {
                        let idx_val = evaluate(NodeType::Identifier(prop.clone()), scope.clone())?
                            .into_type(scope.clone(), RuntimeType::Integer)?;
                        if let RuntimeValue::Integer(idx) = idx_val {
                            data.get(idx as usize).cloned().ok_or_else(|| {
                                InterpreterErr::IndexNonList(NodeType::Identifier(prop.clone()))
                            })
                        } else {
                            Err(InterpreterErr::IndexNonList(NodeType::Identifier(
                                prop.clone(),
                            )))
                        }
                    } else {
                        Err(InterpreterErr::UnexpectedType(object_val))
                    }
                }
                NodeType::IntegerLiteral(ref index) => {
                    if let RuntimeValue::Struct(ObjectType::Tuple(ref map), _) = object_val {
                        if let Some(val) = map.get(*index as usize) {
                            Ok(val.clone())
                        } else {
                            panic!()
                            // Err(InterpreterErr::Value(ValueErr::Scope(ScopeErr::Variable(
                            //     prop.clone(),
                            // ))))
                        }
                    } else {
                        Err(InterpreterErr::UnexpectedType(object_val))
                    }
                }
                NodeType::CallExpression(caller, args) => {
                    if let NodeType::Identifier(ref var_name) = *object {
                        if let NodeType::Identifier(ref method_name) = *caller {
                            if let RuntimeValue::Struct(_, Some(ref struct_name)) = object_val {
                                if let Ok(val) =
                                    get_function(scope.clone(), struct_name, method_name)
                                {
                                    let mut arguments = Vec::new();

                                    if val.1 {
                                        arguments =
                                            vec![(NodeType::Identifier(var_name.clone()), None)];
                                    }

                                    arguments.extend(*args);
                                    return evaluate_function(scope, val.0, arguments);
                                }
                            }
                        }
                    }

                    panic!()
                }
                _ => Err(InterpreterErr::NotImplemented(*property)),
            }
        }
        _ => Err(InterpreterErr::NotImplemented(exp)),
    }
}
