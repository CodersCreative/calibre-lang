use std::collections::HashMap;

use crate::{
    ast::NodeType,
    runtime::{
        interpreter::InterpreterErr,
        scope::{Environment, ScopeErr, Type},
        values::{RuntimeValue, ValueErr, helper::ObjectType},
    },
};

impl Environment {
    pub fn evaluate_struct_expression(
        &mut self,
        scope: &u64,
        obj: NodeType,
    ) -> Result<RuntimeValue, InterpreterErr> {
        if let NodeType::StructLiteral(props) = obj {
            if let ObjectType::Map(props) = props {
                let mut properties = HashMap::new();
                for (k, v) in props {
                    let value = if let Some(value) = v {
                        self.evaluate(scope, value)?
                    } else {
                        self.get_var(scope, &k)?.value.clone()
                    };

                    properties.insert(k, value);
                }

                return Ok(RuntimeValue::Struct(
                    *scope,
                    None,
                    ObjectType::Map(properties),
                ));
            } else if let ObjectType::Tuple(props) = props {
                let mut properties = Vec::new();
                for v in props {
                    if let Some(value) = v {
                        properties.push(self.evaluate(scope, value)?);
                    }
                }
                return Ok(RuntimeValue::Struct(
                    *scope,
                    None,
                    ObjectType::Tuple(properties),
                ));
            }
        }

        Ok(RuntimeValue::Null)
    }

    pub fn evaluate_enum_expression(
        &mut self,
        scope: &u64,
        exp: NodeType,
    ) -> Result<RuntimeValue, InterpreterErr> {
        if let NodeType::EnumExpression {
            identifier,
            value,
            data,
        } = exp
        {
            let enm_class = match self.get_object_type(&scope, &identifier) {
                Ok(Type::Enum(x)) => x.clone(),
                _ => {
                    if let Some(ObjectType::Tuple(args)) = data {
                        if let Ok(s) = self.get_next_scope(*scope, &identifier) {
                            return self.evaluate(
                                &s,
                                NodeType::CallExpression(
                                    Box::new(NodeType::Identifier(value)),
                                    args.into_iter().map(|x| (x.unwrap(), None)).collect(),
                                ),
                            );
                        }
                    }

                    return Err(InterpreterErr::Value(ValueErr::Scope(ScopeErr::Object(
                        identifier,
                    ))));
                }
            };

            if let Some((i, enm)) = enm_class.iter().enumerate().find(|x| &x.1.0 == &value) {
                if let Some(ObjectType::Map(properties)) = &enm.1 {
                    let mut data_vals = HashMap::new();
                    if let Some(ObjectType::Map(data)) = data {
                        for (k, v) in data {
                            let value = if let Some(value) = v {
                                self.evaluate(scope, value)?
                            } else {
                                self.get_var(scope, &k)?.value.clone()
                            };

                            data_vals.insert(k, value);
                        }
                    }

                    let mut new_data_vals = HashMap::new();
                    for property in properties {
                        if let Some(val) = data_vals.get(property.0) {
                            new_data_vals.insert(
                                property.0.clone(),
                                val.into_type(self, scope, &property.1)?,
                            );
                        } else {
                            return Err(InterpreterErr::PropertyNotFound(property.0.to_string()));
                        }
                    }

                    let data = if new_data_vals.is_empty() {
                        None
                    } else {
                        Some(ObjectType::Map(new_data_vals))
                    };

                    return Ok(RuntimeValue::Enum(*scope, identifier, i, data));
                } else if let Some(ObjectType::Tuple(properties)) = &enm.1 {
                    let mut data_vals = Vec::new();

                    if let Some(ObjectType::Tuple(data)) = data {
                        for v in data {
                            if let Some(value) = v {
                                data_vals.push(self.evaluate(scope, value)?);
                            };
                        }
                    }

                    let mut new_data_vals = Vec::new();
                    for (i, property) in properties.into_iter().enumerate() {
                        if data_vals.len() <= i {
                            return Err(InterpreterErr::OutOfBounds(
                                String::from("Tuple Object Type"),
                                i as i16,
                            ));
                        }
                        new_data_vals.push(
                            data_vals[i]
                                .unwrap(self, scope)?
                                .into_type(self, scope, property)
                                .unwrap(),
                        );
                    }

                    let data = if new_data_vals.is_empty() {
                        None
                    } else {
                        Some(ObjectType::Tuple(new_data_vals))
                    };

                    return Ok(RuntimeValue::Enum(*scope, identifier, i, data));
                }
                return Ok(RuntimeValue::Enum(*scope, identifier, i, None));
            } else {
                Err(InterpreterErr::UnexpectedEnumItem(identifier, value))
            }
        } else {
            Err(InterpreterErr::NotImplemented(exp))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::NodeType;
    use crate::runtime::interpreter::statements::structs::{
        evaluate_enum_declaration, evaluate_struct_declaration,
    };
    use crate::runtime::scope::{Object, Scope};
    use crate::runtime::values::RuntimeType;
    use crate::runtime::values::{RuntimeValue, helper::ObjectType};
    use std::cell::RefCell;
    use std::collections::HashMap;
    use std::rc::Rc;

    fn new_scope() -> Rc<RefCell<Scope>> {
        Rc::new(RefCell::new(Scope::new(None)))
    }

    #[test]
    fn test_evaluate_struct_expression_map() {
        let scope = new_scope();
        let mut props = HashMap::new();
        props.insert("a".to_string(), Some(NodeType::IntLiteral(1)));
        props.insert("b".to_string(), Some(NodeType::IntLiteral(2)));
        let node = NodeType::StructLiteral(ObjectType::Map(props));
        let result = evaluate_struct_expression(node, scope).unwrap();
        match result {
            RuntimeValue::Struct(ObjectType::Map(map), _) => {
                assert_eq!(map.get("a"), Some(&RuntimeValue::Int(1)));
                assert_eq!(map.get("b"), Some(&RuntimeValue::Int(2)));
            }
            _ => panic!("Expected Struct(Map)"),
        }
    }

    #[test]
    fn test_evaluate_struct_expression_tuple() {
        let scope = new_scope();
        let node = NodeType::StructLiteral(ObjectType::Tuple(vec![
            Some(NodeType::IntLiteral(1)),
            Some(NodeType::IntLiteral(2)),
        ]));
        let result = evaluate_struct_expression(node, scope).unwrap();
        match result {
            RuntimeValue::Struct(ObjectType::Tuple(vals), _) => {
                assert_eq!(vals, vec![RuntimeValue::Int(1), RuntimeValue::Int(2)]);
            }
            _ => panic!("Expected Struct(Tuple)"),
        }
    }

    #[test]
    fn test_evaluate_struct_expression_null() {
        let scope = new_scope();
        let node = NodeType::IntLiteral(42);
        let result = evaluate_struct_expression(node, scope).unwrap();
        assert_eq!(result, RuntimeValue::Null);
    }

    #[test]
    fn test_evaluate_enum_expression_map() {
        let scope = new_scope();
        let mut enum_props = HashMap::new();
        enum_props.insert("y".to_string(), RuntimeType::Int);
        let enum_node = NodeType::EnumDeclaration {
            identifier: "MyEnum".to_string(),
            options: vec![(String::from("x"), Some(ObjectType::Map(enum_props)))],
        };
        evaluate_enum_declaration(enum_node, scope.clone()).unwrap();

        let exp = NodeType::EnumExpression {
            identifier: "MyEnum".to_string(),
            value: "x".to_string(),
            data: Some(ObjectType::Map(HashMap::from([(
                "y".to_string(),
                Some(NodeType::IntLiteral(10)),
            )]))),
        };
        let result = evaluate_enum_expression(exp, scope).unwrap();
        match result {
            RuntimeValue::Enum(ident, variant_index, data) => {
                assert_eq!(ident, "MyEnum");
                assert_eq!(variant_index, 0);
                assert!(data.is_some());
            }
            _ => panic!("Expected Enum"),
        }
    }

    #[test]
    fn test_evaluate_enum_expression_tuple() {
        let scope = new_scope();
        let enum_node = NodeType::EnumDeclaration {
            identifier: "MyEnum".to_string(),
            options: vec![(
                String::from("Foo"),
                Some(ObjectType::Tuple(vec![RuntimeType::Int])),
            )],
        };
        evaluate_enum_declaration(enum_node, scope.clone()).unwrap();

        let exp = NodeType::EnumExpression {
            identifier: "MyEnum".to_string(),
            value: "Foo".to_string(),
            data: Some(ObjectType::Tuple(vec![Some(NodeType::IntLiteral(10))])),
        };
        let result = evaluate_enum_expression(exp, scope).unwrap();
        match result {
            RuntimeValue::Enum(ident, variant_index, data) => {
                assert_eq!(ident, "MyEnum");
                assert_eq!(variant_index, 0);
                assert!(data.is_some());
            }
            _ => panic!("Expected Enum"),
        }
    }
}
