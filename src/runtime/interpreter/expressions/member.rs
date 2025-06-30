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
                NodeType::IntegerLiteral(index) => {
                    if let RuntimeValue::Struct(ObjectType::Tuple(ref mut map), _) = object_val {
                        if let Some(field) = map.get_mut(index as usize) {
                            *field = value;
                            if let NodeType::Identifier(obj_name) = *object {
                                scope.borrow_mut().assign_var(&obj_name, object_val)?;
                            }
                            Ok(())
                        } else {
                            panic!()
                        }
                    } else if let RuntimeValue::List { ref mut data, .. } = object_val {
                        if let Some(elem) = data.get_mut(index as usize) {
                            *elem = value;
                            if let NodeType::Identifier(obj_name) = *object {
                                scope.borrow_mut().assign_var(&obj_name, object_val)?;
                            }
                            Ok(())
                        } else {
                            panic!()
                        }
                    } else {
                        Err(InterpreterErr::UnexpectedType(object_val))
                    }
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
            let (object_val, object_type) = match *object.clone() {
                NodeType::Identifier(object_name) => {
                    if let Ok(var) = get_var(scope.clone(), &object_name) {
                        (var.0, var.1)
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
                                } else if let NodeType::CallExpression(caller, args) = *property {
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
                    println!("yes");
                    if let RuntimeValue::Struct(ObjectType::Tuple(ref map), _) = object_val {
                        if let Some(val) = map.get(*index as usize) {
                            Ok(val.clone())
                        } else {
                            panic!()
                            // Err(InterpreterErr::Value(ValueErr::Scope(ScopeErr::Variable(
                            //     prop.clone(),
                            // ))))
                        }
                    } else if let RuntimeValue::Enum(name, i, Some(ObjectType::Tuple(data))) =
                        object_val
                    {
                        if let Some(val) = data.get(*index as usize) {
                            Ok(val.clone())
                        } else {
                            panic!()
                            // Err(InterpreterErr::Value(ValueErr::Scope(ScopeErr::Variable(
                            //     prop.clone(),
                            // ))))
                        }
                    } else if let RuntimeValue::List { data, data_type: _ } = object_val {
                        if let Some(val) = data.get(*index as usize) {
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
                            let struct_name = match object_val.clone() {
                                RuntimeValue::Struct(_, Some(ref name)) => name.to_string(),
                                RuntimeValue::List { data, data_type } => match *data_type {
                                    Some(RuntimeType::Enum(x)) => x,
                                    Some(RuntimeType::Struct(Some(x))) => x,
                                    _ => panic!(),
                                },
                                _ => panic!(),
                            };
                            if let Ok(val) = get_function(scope.clone(), &struct_name, method_name)
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
                    panic!("{:?}.{:?}", object, caller);
                }
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
