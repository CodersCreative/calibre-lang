use std::{ascii::AsciiExt, cell::RefCell, rc::Rc};

use rand::seq::IndexedRandom;

use crate::{
    ast::NodeType,
    runtime::{
        interpreter::{InterpreterErr, evaluate, expressions::call::evaluate_function},
        scope::{
            Object, Scope, ScopeErr,
            children::get_next_scope,
            links::{get_link, get_link_path, update_link_path},
            objects::{get_function, get_object},
            variables::{assign_var, get_var},
        },
        values::{RuntimeType, RuntimeValue, ValueErr, helper::ObjectType},
    },
};

use super::structs::evaluate_enum_expression;

enum MembrExprPathRes {
    Value(RuntimeValue),
    Path(Vec<String>),
}

fn get_member_expression_path(
    og_path: Vec<(NodeType, bool)>,
    scope: &Rc<RefCell<Scope>>,
) -> Result<MembrExprPathRes, InterpreterErr> {
    let mut path = Vec::new();
    if let (NodeType::Identifier(x), false) = &og_path[0] {
        if path.is_empty() {
            if let Ok(s) = get_next_scope(scope, &x) {
                if let Ok(x) = evaluate(og_path[1].0.clone(), &s) {
                    return Ok(MembrExprPathRes::Value(x));
                }
                match get_member_expression_path(og_path[1..].to_vec(), &s) {
                    Ok(x) => return Ok(x),
                    _ => {}
                }
            }
        }
    }

    for (node, computed) in og_path.into_iter() {
        if !computed {
            match node {
                NodeType::Identifier(x) => {
                    path.push(x.to_string());
                    continue;
                }
                NodeType::FloatLiteral(x) => {
                    path.push(x.to_string());
                    continue;
                }
                NodeType::IntLiteral(x) => {
                    path.push(x.to_string());
                    continue;
                }
                _ => {}
            }
        }

        match evaluate(node.clone(), &scope) {
            Ok(mut value) => loop {
                match value {
                    RuntimeValue::Int(x) => path.push(x.to_string()),
                    RuntimeValue::UInt(x) => path.push(x.to_string()),
                    RuntimeValue::Float(x) => path.push(x.to_string()),
                    RuntimeValue::Double(x) => path.push(x.to_string()),
                    RuntimeValue::Long(x) => path.push(x.to_string()),
                    RuntimeValue::ULong(x) => path.push(x.to_string()),
                    RuntimeValue::Str(x) => path.push(x.to_string()),
                    RuntimeValue::Char(x) => path.push(x.to_string()),
                    RuntimeValue::Link(path, _) => {
                        value = get_link_path(scope, &path)?;
                        continue;
                    }
                    x => return Ok(MembrExprPathRes::Value(x)), // x => unimplemented!("{:?}", x),
                }
                break;
            },
            Err(e) if path.len() == 1 => match node {
                NodeType::Identifier(value) => {
                    return Ok(MembrExprPathRes::Value(evaluate(
                        NodeType::EnumExpression {
                            identifier: path.remove(0),
                            value,
                            data: None,
                        },
                        scope,
                    )?));
                }
                NodeType::CallExpression(value, args) => match *value {
                    NodeType::Identifier(value) => {
                        return Ok(MembrExprPathRes::Value(evaluate(
                            NodeType::EnumExpression {
                                identifier: path.remove(0),
                                value: value.to_string(),
                                data: Some(ObjectType::Tuple(
                                    args.into_iter().map(|x| Some(x.0.clone())).collect(),
                                )),
                            },
                            scope,
                        )?));
                    }
                    _ => return Err(e),
                },

                _ => return Err(e),
            },
            Err(e) => return Err(e),
        }
    }

    Ok(MembrExprPathRes::Path(path))
}

pub fn assign_member_expression(
    member: NodeType,
    value: RuntimeValue,
    scope: &Rc<RefCell<Scope>>,
) -> Result<RuntimeValue, InterpreterErr> {
    match member {
        NodeType::MemberExpression { path: og_path } => {
            let path = match get_member_expression_path(og_path, scope)? {
                MembrExprPathRes::Path(x) => x,
                MembrExprPathRes::Value(x) => return Ok(x),
            };

            let _ = update_link_path(scope, &path, |x| {
                *x = value.clone();
                Ok(())
            })?;

            Ok(value)
        }
        _ => Err(InterpreterErr::NotImplemented(member)),
    }
}

pub fn evaluate_member_expression(
    exp: NodeType,
    scope: &Rc<RefCell<Scope>>,
) -> Result<RuntimeValue, InterpreterErr> {
    match exp {
        NodeType::MemberExpression { path: og_path } => {
            let mut path = match get_member_expression_path(og_path, scope)? {
                MembrExprPathRes::Path(x) => x,
                MembrExprPathRes::Value(x) => return Ok(x),
            };

            match get_link_path(scope, &path) {
                Ok(x) => Ok(x),
                Err(e) if path.len() == 2 => {
                    return evaluate(
                        NodeType::EnumExpression {
                            identifier: path.remove(0),
                            value: path.remove(0),
                            data: None,
                        },
                        scope,
                    );
                }
                Err(e) => Err(e),
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
        map.insert("field".to_string(), RuntimeValue::Int(1));
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
        assign_member_expression(member, RuntimeValue::Int(42), scope.clone()).unwrap();

        let updated = scope.borrow().variables.get("obj").unwrap().0.clone();
        if let RuntimeValue::Struct(ObjectType::Map(m), _) = updated {
            assert_eq!(m.get("field"), Some(&RuntimeValue::Int(42)));
        } else {
            panic!("Expected struct value");
        }
    }

    #[test]
    fn test_assign_member_expression_list_index() {
        let scope = new_scope();
        let list_val = RuntimeValue::List {
            data: vec![RuntimeValue::Int(1), RuntimeValue::Int(2)],
            data_type: Box::new(Some(crate::runtime::values::RuntimeType::Int)),
        };
        scope
            .borrow_mut()
            .push_var("lst".to_string(), list_val.clone(), VarType::Mutable(None))
            .unwrap();

        let member = NodeType::MemberExpression {
            object: Box::new(NodeType::Identifier("lst".to_string())),
            property: Box::new(NodeType::IntLiteral(1)),
            is_computed: false,
        };
        assign_member_expression(member, RuntimeValue::Int(99), scope.clone()).unwrap();

        let updated = scope.borrow().variables.get("lst").unwrap().0.clone();
        if let RuntimeValue::List { data, .. } = updated {
            assert_eq!(data[1], RuntimeValue::Int(99));
        } else {
            panic!("Expected list value");
        }
    }

    #[test]
    fn test_evaluate_member_expression_struct_field() {
        let scope = new_scope();
        let mut map = std::collections::HashMap::new();
        map.insert("foo".to_string(), RuntimeValue::Int(123));
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
        assert_eq!(result, RuntimeValue::Int(123));
    }

    #[test]
    fn test_evaluate_member_expression_list_index() {
        let scope = new_scope();
        let list_val = RuntimeValue::List {
            data: vec![RuntimeValue::Int(10), RuntimeValue::Int(20)],
            data_type: Box::new(Some(crate::runtime::values::RuntimeType::Int)),
        };
        scope
            .borrow_mut()
            .push_var("lst".to_string(), list_val, VarType::Mutable(None))
            .unwrap();

        let member = NodeType::MemberExpression {
            object: Box::new(NodeType::Identifier("lst".to_string())),
            property: Box::new(NodeType::IntLiteral(1)),
            is_computed: false,
        };
        let result = evaluate_member_expression(member, scope).unwrap();
        assert_eq!(result, RuntimeValue::Int(20));
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
