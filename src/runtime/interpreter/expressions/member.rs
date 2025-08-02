use rand::seq::IndexedRandom;

use crate::{
    ast::NodeType,
    runtime::{
        interpreter::InterpreterErr,
        scope::Environment,
        values::{RuntimeValue, helper::ObjectType},
    },
};

pub enum MembrExprPathRes {
    Value(RuntimeValue),
    Path(Vec<String>),
}

impl Environment {
    pub fn get_member_expression_path(
        &mut self,
        scope: &u64,
        og_path: Vec<(NodeType, bool)>,
    ) -> Result<MembrExprPathRes, InterpreterErr> {
        let mut path = Vec::new();
        if let (NodeType::Identifier(x), false) = &og_path[0] {
            if let Ok(s) = self.get_next_scope(*scope, &x) {
                if let Ok(x) = self.evaluate(&s, og_path[1].0.clone()) {
                    return Ok(MembrExprPathRes::Value(x));
                }
                match self.get_member_expression_path(&s, og_path[1..].to_vec()) {
                    Ok(x) => return Ok(x),
                    _ => {}
                }
            }
        }

        for (node, computed) in og_path.into_iter() {
            match &node {
                NodeType::MemberExpression { path: p } => {
                    match self.get_member_expression_path(scope, p.to_vec())? {
                        MembrExprPathRes::Value(x) => return Ok(MembrExprPathRes::Value(x)),
                        MembrExprPathRes::Path(mut p) => path.append(&mut p),
                    }
                    continue;
                }
                _ if computed => {}
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

            match self.evaluate(&scope, node.clone()) {
                Ok(mut value) => loop {
                    match value.unwrap(self, scope)? {
                        RuntimeValue::Int(x) => path.push(x.to_string()),
                        RuntimeValue::UInt(x) => path.push(x.to_string()),
                        RuntimeValue::Float(x) => path.push(x.to_string()),
                        RuntimeValue::Double(x) => path.push(x.to_string()),
                        RuntimeValue::Long(x) => path.push(x.to_string()),
                        RuntimeValue::ULong(x) => path.push(x.to_string()),
                        RuntimeValue::Str(x) => path.push(x.to_string()),
                        RuntimeValue::Char(x) => path.push(x.to_string()),
                        RuntimeValue::Link(s, path, _) => {
                            value = self.get_link_path(&s, &path)?.clone();
                            continue;
                        }
                        x => return Ok(MembrExprPathRes::Value(x)), // x => unimplemented!("{:?}", x),
                    }
                    break;
                },
                Err(e) if path.len() == 1 => match node {
                    NodeType::Identifier(value) => {
                        return Ok(MembrExprPathRes::Value(self.evaluate(
                            scope,
                            NodeType::EnumExpression {
                                identifier: path.remove(0),
                                value,
                                data: None,
                            },
                        )?));
                    }
                    NodeType::CallExpression(value, mut args) => match *value {
                        NodeType::Identifier(value) => {
                            return Ok(MembrExprPathRes::Value(
                                match self.evaluate(
                                    scope,
                                    NodeType::EnumExpression {
                                        identifier: path[0].clone(),
                                        value: value.to_string(),
                                        data: Some(ObjectType::Tuple(
                                            args.clone()
                                                .into_iter()
                                                .map(|x| Some(x.0.clone()))
                                                .collect(),
                                        )),
                                    },
                                ) {
                                    Ok(x) => x,
                                    Err(e) => {
                                        if let Ok(x) = self.get_function(scope, &path[0], &value) {
                                            self.evaluate_function(scope, x.0.clone(), args)?
                                        } else {
                                            let obj = match match self.get_var(scope, &path[0]) {
                                                Ok(x) => x.value.unwrap(self, scope)?,
                                                _ => return Err(e),
                                            } {
                                                RuntimeValue::Struct(_, p, _) => p.unwrap().clone(),
                                                RuntimeValue::Enum(_, p, _, _) => p.clone(),
                                                _ => return Err(e),
                                            };

                                            if let Ok(x) = self.get_function(scope, &obj, &value) {
                                                if x.1 {
                                                    args.insert(
                                                        0,
                                                        (
                                                            NodeType::Identifier(path[0].clone()),
                                                            None,
                                                        ),
                                                    );
                                                }

                                                self.evaluate_function(scope, x.0.clone(), args)?
                                            } else {
                                                return Err(e);
                                            }
                                        }
                                    }
                                },
                            ));
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
        &mut self,
        scope: &u64,
        member: NodeType,
        value: RuntimeValue,
    ) -> Result<RuntimeValue, InterpreterErr> {
        match member {
            NodeType::MemberExpression { path: og_path } => {
                let path = match self.get_member_expression_path(scope, og_path)? {
                    MembrExprPathRes::Path(x) => x,
                    MembrExprPathRes::Value(x) => return Ok(x),
                };

                let _ = self
                    .update_link_path(scope, &path, |x| {
                        *x = value.clone();
                        Ok(())
                    })
                    .unwrap();

                Ok(value)
            }
            _ => Err(InterpreterErr::NotImplemented(member)),
        }
    }

    pub fn evaluate_member_expression(
        &mut self,
        scope: &u64,
        exp: NodeType,
    ) -> Result<RuntimeValue, InterpreterErr> {
        match exp {
            NodeType::MemberExpression { path: og_path } => {
                let mut path = match self.get_member_expression_path(scope, og_path)? {
                    MembrExprPathRes::Path(x) => x,
                    MembrExprPathRes::Value(x) => return Ok(x),
                };

                match self.get_link_path(scope, &path) {
                    Ok(x) => Ok(x.clone()),
                    Err(e) if path.len() == 2 => {
                        return self.evaluate(
                            scope,
                            NodeType::EnumExpression {
                                identifier: path.remove(0),
                                value: path.remove(0),
                                data: None,
                            },
                        );
                    }
                    Err(e) => Err(e),
                }
            }
            _ => Err(InterpreterErr::NotImplemented(exp)),
        }
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
