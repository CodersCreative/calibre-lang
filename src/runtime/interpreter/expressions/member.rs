use std::{ascii::AsciiExt, cell::RefCell, rc::Rc};

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

use super::structs::evaluate_enum_expression;

fn assign_to_struct_field(
    object_val: &mut RuntimeValue,
    field: &str,
    value: RuntimeValue,
    object: &NodeType,
    scope: Rc<RefCell<Scope>>,
) -> Result<(), InterpreterErr> {
    if let RuntimeValue::Struct(ObjectType::Map(map), _) = object_val {
        if let Some(field_val) = map.get_mut(field) {
            *field_val = value;
            if let NodeType::Identifier(obj_name) = object {
                scope
                    .borrow_mut()
                    .assign_var(obj_name, object_val.clone())?;
            }
            Ok(())
        } else {
            Err(InterpreterErr::Value(ValueErr::Scope(ScopeErr::Variable(
                field.to_string(),
            ))))
        }
    } else {
        Err(InterpreterErr::UnexpectedType(object_val.clone()))
    }
}

fn assign_to_list_index(
    object_val: &mut RuntimeValue,
    index: usize,
    value: RuntimeValue,
    object: &NodeType,
    scope: Rc<RefCell<Scope>>,
) -> Result<(), InterpreterErr> {
    if let RuntimeValue::List { data, .. } = object_val {
        if let Some(elem) = data.get_mut(index) {
            *elem = value;
            if let NodeType::Identifier(obj_name) = object {
                scope
                    .borrow_mut()
                    .assign_var(obj_name, object_val.clone())?;
            }
            Ok(())
        } else {
            Err(InterpreterErr::IndexNonList(object.clone()))
        }
    } else {
        Err(InterpreterErr::UnexpectedType(object_val.clone()))
    }
}

fn assign_to_tuple_index(
    object_val: &mut RuntimeValue,
    index: usize,
    value: RuntimeValue,
    object: &NodeType,
    scope: Rc<RefCell<Scope>>,
) -> Result<(), InterpreterErr> {
    if let RuntimeValue::Struct(ObjectType::Tuple(data), _) = object_val {
        if let Some(field) = data.get_mut(index) {
            *field = value;
            if let NodeType::Identifier(obj_name) = object {
                scope
                    .borrow_mut()
                    .assign_var(obj_name, object_val.clone())?;
            }
            Ok(())
        } else {
            Err(InterpreterErr::IndexNonList(object.clone()))
        }
    } else {
        Err(InterpreterErr::UnexpectedType(object_val.clone()))
    }
}

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
                NodeType::IntegerLiteral(index) => assign_to_tuple_index(
                    &mut object_val,
                    index as usize,
                    value.clone(),
                    &*object,
                    scope.clone(),
                )
                .or_else(|_| {
                    assign_to_list_index(&mut object_val, index as usize, value, &*object, scope)
                }),
                NodeType::Identifier(ref prop) => assign_to_struct_field(
                    &mut object_val,
                    prop,
                    value.clone(),
                    &*object,
                    scope.clone(),
                )
                .or_else(|_| {
                    let idx_val = evaluate(NodeType::Identifier(prop.clone()), scope.clone())?
                        .into_type(scope.clone(), RuntimeType::Integer)?;
                    if let RuntimeValue::Integer(idx) = idx_val {
                        assign_to_list_index(&mut object_val, idx as usize, value, &*object, scope)
                    } else {
                        Err(InterpreterErr::IndexNonList(NodeType::Identifier(
                            prop.clone(),
                        )))
                    }
                }),
                _ => Err(InterpreterErr::NotImplemented(*property)),
            }
        }
        _ => Err(InterpreterErr::NotImplemented(member)),
    }
}

fn get_struct_field(
    object_val: &RuntimeValue,
    field: &str,
) -> Result<RuntimeValue, InterpreterErr> {
    if let RuntimeValue::Struct(ObjectType::Map(map), _) = object_val {
        map.get(field).cloned().ok_or_else(|| {
            InterpreterErr::Value(ValueErr::Scope(ScopeErr::Variable(field.to_string())))
        })
    } else {
        Err(InterpreterErr::UnexpectedType(object_val.clone()))
    }
}

fn get_list_index(
    object_val: &RuntimeValue,
    index: usize,
    prop: NodeType,
) -> Result<RuntimeValue, InterpreterErr> {
    if let RuntimeValue::List { data, .. } = object_val {
        data.get(index)
            .cloned()
            .ok_or_else(|| InterpreterErr::IndexNonList(prop))
    } else {
        Err(InterpreterErr::UnexpectedType(object_val.clone()))
    }
}

fn get_tuple_index(
    object_val: &RuntimeValue,
    index: usize,
    prop: NodeType,
) -> Result<RuntimeValue, InterpreterErr> {
    if let RuntimeValue::Struct(ObjectType::Tuple(data), _) = object_val {
        data.get(index)
            .cloned()
            .ok_or_else(|| InterpreterErr::IndexNonList(prop))
    } else {
        Err(InterpreterErr::UnexpectedType(object_val.clone()))
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
                NodeType::Identifier(ref object_name) => {
                    if let Ok(var) = get_var(scope.clone(), object_name) {
                        var.0
                    } else if let Ok(obj) = get_object(scope.clone(), object_name) {
                        match obj {
                            Object::Enum(enm_name) => {
                                if let NodeType::Identifier(value) = *property {
                                    return evaluate(
                                        NodeType::EnumExpression {
                                            identifier: object_name.to_string(),
                                            value,
                                            data: None,
                                        },
                                        scope,
                                    );
                                } else if let NodeType::CallExpression(caller, args) = *property {
                                    if let NodeType::Identifier(ref method_name) = *caller {
                                        if let Ok(val) =
                                            get_function(scope.clone(), &object_name, method_name)
                                        {
                                            return evaluate_function(scope, val.0, args);
                                        } else {
                                            return evaluate_enum_expression(NodeType::EnumExpression { identifier: object_name.to_string(), value: method_name.to_string(), data: Some(ObjectType::Tuple(args.into_iter().map(|x| Some(x.0)).collect())) }, scope);
                                            // return Err(InterpreterErr::Value(ValueErr::Scope(
                                            //     ScopeErr::Function(method_name.to_string()),
                                            // )));
                                        }
                                    } else {
                                        return Err(InterpreterErr::UnexpectedNode(*caller));
                                    }
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
                                            return evaluate_function(scope, val.0, args);
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
                            _ => return Err(InterpreterErr::NotImplemented(*object.clone())),
                        }
                    } else {
                        return Err(InterpreterErr::Value(ValueErr::Scope(ScopeErr::Variable(
                            object_name.clone(),
                        ))));
                    }
                }
                _ => return Err(InterpreterErr::UnexpectedNode(*object)),
            };

            match *property {
                NodeType::Identifier(ref prop) => {
                    get_struct_field(&object_val, prop).or_else(|_| {
                        let idx_val = evaluate(NodeType::Identifier(prop.clone()), scope.clone())?
                            .into_type(scope.clone(), RuntimeType::Integer)?;
                        if let RuntimeValue::Integer(idx) = idx_val {
                            get_list_index(
                                &object_val,
                                idx as usize,
                                NodeType::Identifier(prop.clone()),
                            )
                        } else {
                            Err(InterpreterErr::IndexNonList(NodeType::Identifier(
                                prop.clone(),
                            )))
                        }
                    })
                }
                NodeType::MemberExpression {
                    object,
                    property,
                    is_computed,
                } => {
                    if is_computed {
                        if let RuntimeValue::Integer(index) =
                            evaluate(*property.clone(), scope.clone())?
                        {
                            get_tuple_index(
                                &object_val,
                                index as usize,
                                NodeType::IntegerLiteral(index),
                            )
                            .or_else(|_| {
                                get_list_index(
                                    &object_val,
                                    index as usize,
                                    NodeType::IntegerLiteral(index),
                                )
                            })
                        } else {
                            evaluate_member_expression(*property, scope)
                        }
                    } else {
                        evaluate_member_expression(*property, scope)
                    }
                }
                NodeType::CallExpression(caller, args) => {
                    if let NodeType::Identifier(ref var_name) = *object {
                        if let NodeType::Identifier(ref method_name) = *caller {
                            let struct_name = match object_val.clone() {
                                RuntimeValue::Struct(_, Some(ref name)) => name.to_string(),
                                RuntimeValue::Enum(x, _, _) => x.to_string(),
                                RuntimeValue::List { data: _, data_type } => match *data_type {
                                    Some(RuntimeType::Enum(ref x)) => x.clone(),
                                    Some(RuntimeType::Struct(Some(ref x))) => x.clone(),
                                    _ => return Err(InterpreterErr::NotImplemented(*caller)),
                                },
                                _ => return Err(InterpreterErr::NotImplemented(*caller)),
                            };
                            if let Ok(val) = get_function(scope.clone(), &struct_name, method_name)
                            {
                                let mut arguments = Vec::new();
                                if val.1 {
                                    arguments.push((NodeType::Identifier(var_name.clone()), None));
                                }
                                arguments.extend(args);
                                return evaluate_function(scope, val.0, arguments);
                            }
                        }
                    }
                    Err(InterpreterErr::NotImplemented(*caller))
                }
                _ => match evaluate(*property, scope)? {
                    RuntimeValue::Integer(index) => get_tuple_index(
                        &object_val,
                        index as usize,
                        NodeType::IntegerLiteral(index),
                    )
                    .or_else(|_| {
                        get_list_index(&object_val, index as usize, NodeType::IntegerLiteral(index))
                    }),
                    RuntimeValue::Float(index) => get_tuple_index(
                        &object_val,
                        index as usize,
                        NodeType::IntegerLiteral(index as i64),
                    )
                    .or_else(|_| {
                        get_list_index(
                            &object_val,
                            index as usize,
                            NodeType::IntegerLiteral(index as i64),
                        )
                    }),
                    RuntimeValue::Str(field) => get_struct_field(&object_val, &field),
                    _ => panic!(),
                },
                _ => Err(InterpreterErr::NotImplemented(*property)),
            }
        }
        _ => Err(InterpreterErr::NotImplemented(exp)),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::NodeType;
    use crate::runtime::scope::Scope;
    use crate::runtime::values::helper::VarType;
    use crate::runtime::values::{RuntimeValue, helper::ObjectType};
    use std::cell::RefCell;
    use std::rc::Rc;

    fn new_scope() -> Rc<RefCell<Scope>> {
        Rc::new(RefCell::new(Scope::new(None)))
    }

    #[test]
    fn test_assign_member_expression_struct_field() {
        let scope = new_scope();
        let mut map = std::collections::HashMap::new();
        map.insert("field".to_string(), RuntimeValue::Integer(1));
        let struct_val = RuntimeValue::Struct(ObjectType::Map(map.clone()), None);
        scope
            .borrow_mut()
            .push_var(
                "obj".to_string(),
                struct_val.clone(),
                VarType::Mutable(None),
            )
            .unwrap();

        let member = NodeType::MemberExpression {
            object: Box::new(NodeType::Identifier("obj".to_string())),
            property: Box::new(NodeType::Identifier("field".to_string())),
            is_computed: false,
        };
        assign_member_expression(member, RuntimeValue::Integer(42), scope.clone()).unwrap();

        let updated = scope.borrow().variables.get("obj").unwrap().0.clone();
        if let RuntimeValue::Struct(ObjectType::Map(m), _) = updated {
            assert_eq!(m.get("field"), Some(&RuntimeValue::Integer(42)));
        } else {
            panic!("Expected struct value");
        }
    }

    #[test]
    fn test_assign_member_expression_list_index() {
        let scope = new_scope();
        let list_val = RuntimeValue::List {
            data: vec![RuntimeValue::Integer(1), RuntimeValue::Integer(2)],
            data_type: Box::new(Some(crate::runtime::values::RuntimeType::Integer)),
        };
        scope
            .borrow_mut()
            .push_var("lst".to_string(), list_val.clone(), VarType::Mutable(None))
            .unwrap();

        let member = NodeType::MemberExpression {
            object: Box::new(NodeType::Identifier("lst".to_string())),
            property: Box::new(NodeType::IntegerLiteral(1)),
            is_computed: false,
        };
        assign_member_expression(member, RuntimeValue::Integer(99), scope.clone()).unwrap();

        let updated = scope.borrow().variables.get("lst").unwrap().0.clone();
        if let RuntimeValue::List { data, .. } = updated {
            assert_eq!(data[1], RuntimeValue::Integer(99));
        } else {
            panic!("Expected list value");
        }
    }

    #[test]
    fn test_evaluate_member_expression_struct_field() {
        let scope = new_scope();
        let mut map = std::collections::HashMap::new();
        map.insert("foo".to_string(), RuntimeValue::Integer(123));
        let struct_val = RuntimeValue::Struct(ObjectType::Map(map.clone()), None);
        scope
            .borrow_mut()
            .push_var("obj".to_string(), struct_val, VarType::Mutable(None))
            .unwrap();

        let member = NodeType::MemberExpression {
            object: Box::new(NodeType::Identifier("obj".to_string())),
            property: Box::new(NodeType::Identifier("foo".to_string())),
            is_computed: false,
        };
        let result = evaluate_member_expression(member, scope).unwrap();
        assert_eq!(result, RuntimeValue::Integer(123));
    }

    #[test]
    fn test_evaluate_member_expression_list_index() {
        let scope = new_scope();
        let list_val = RuntimeValue::List {
            data: vec![RuntimeValue::Integer(10), RuntimeValue::Integer(20)],
            data_type: Box::new(Some(crate::runtime::values::RuntimeType::Integer)),
        };
        scope
            .borrow_mut()
            .push_var("lst".to_string(), list_val, VarType::Mutable(None))
            .unwrap();

        let member = NodeType::MemberExpression {
            object: Box::new(NodeType::Identifier("lst".to_string())),
            property: Box::new(NodeType::IntegerLiteral(1)),
            is_computed: false,
        };
        let result = evaluate_member_expression(member, scope).unwrap();
        assert_eq!(result, RuntimeValue::Integer(20));
    }

    #[test]
    fn test_evaluate_member_expression_struct_field_not_found() {
        let scope = new_scope();
        let map = std::collections::HashMap::new();
        let struct_val = RuntimeValue::Struct(ObjectType::Map(map), None);
        scope
            .borrow_mut()
            .push_var("obj".to_string(), struct_val, VarType::Mutable(None))
            .unwrap();

        let member = NodeType::MemberExpression {
            object: Box::new(NodeType::Identifier("obj".to_string())),
            property: Box::new(NodeType::Identifier("missing".to_string())),
            is_computed: false,
        };
        let result = evaluate_member_expression(member, scope);
        assert!(result.is_err());
    }
}
