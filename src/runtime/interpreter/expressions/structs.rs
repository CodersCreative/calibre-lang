use crate::{
    ast::NodeType,
    runtime::{
        interpreter::InterpreterErr,
        scope::{Environment, ScopeErr, Type},
        values::{RuntimeValue, ValueErr, helper::ObjectType},
    },
};
use std::collections::HashMap;

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
                        self.evaluate(scope, value)?.unwrap(self, scope)?.clone()
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
                        properties.push(self.evaluate(scope, value)?.unwrap(self, scope)?.clone());
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
                                self.evaluate(scope, value)?.unwrap(self, scope)?.clone()
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
                                data_vals.push(
                                    self.evaluate(scope, value)?.unwrap(self, scope)?.clone(),
                                );
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
                        new_data_vals.push(data_vals[i].into_type(self, scope, property).unwrap());
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
    use crate::runtime::values::RuntimeType;
    use crate::runtime::values::{RuntimeValue, helper::ObjectType};
    use std::collections::HashMap;
    use std::path::PathBuf;
    use std::str::FromStr;

    fn get_new_env() -> (Environment, u64) {
        let mut env = Environment::new();
        let scope = env.new_scope_with_stdlib(None, PathBuf::from_str("./main.cl").unwrap(), None);
        (env, scope)
    }

    #[test]
    fn test_evaluate_struct_expression_map() {
        let (mut env, scope) = get_new_env();
        let mut props = HashMap::new();
        props.insert("a".to_string(), Some(NodeType::IntLiteral(1)));
        props.insert("b".to_string(), Some(NodeType::IntLiteral(2)));
        let node = NodeType::StructLiteral(ObjectType::Map(props));
        let result = env.evaluate_struct_expression(&scope, node).unwrap();

        match result {
            RuntimeValue::Struct(_, _, ObjectType::Map(map)) => {
                assert_eq!(map.get("a"), Some(&RuntimeValue::UInt(1)));
                assert_eq!(map.get("b"), Some(&RuntimeValue::UInt(2)));
            }
            _ => panic!("Expected Struct(Map)"),
        }
    }

    #[test]
    fn test_evaluate_struct_expression_tuple() {
        let (mut env, scope) = get_new_env();
        let node = NodeType::StructLiteral(ObjectType::Tuple(vec![
            Some(NodeType::IntLiteral(1)),
            Some(NodeType::IntLiteral(2)),
        ]));
        let result = env.evaluate_struct_expression(&scope, node).unwrap();
        match result {
            RuntimeValue::Struct(_, _, ObjectType::Tuple(vals)) => {
                assert_eq!(vals, vec![RuntimeValue::UInt(1), RuntimeValue::UInt(2)]);
            }
            _ => panic!("Expected Struct(Tuple)"),
        }
    }

    #[test]
    fn test_evaluate_struct_expression_null() {
        let (mut env, scope) = get_new_env();
        let node = NodeType::IntLiteral(42);
        let result = env.evaluate_struct_expression(&scope, node).unwrap();
        assert_eq!(result, RuntimeValue::Null);
    }

    #[test]
    fn test_evaluate_enum_expression_map() {
        let (mut env, scope) = get_new_env();
        let mut enum_props = HashMap::new();
        enum_props.insert("y".to_string(), RuntimeType::Int);
        let enum_node = NodeType::TypeDeclaration {
            identifier: "MyEnum".to_string(),
            object: Type::Enum(vec![(String::from("x"), Some(ObjectType::Map(enum_props)))]),
        };

        env.evaluate(&scope, enum_node).unwrap();

        let exp = NodeType::EnumExpression {
            identifier: "MyEnum".to_string(),
            value: "x".to_string(),
            data: Some(ObjectType::Map(HashMap::from([(
                "y".to_string(),
                Some(NodeType::IntLiteral(10)),
            )]))),
        };
        let result = env.evaluate_enum_expression(&scope, exp).unwrap();
        match result {
            RuntimeValue::Enum(_, ident, variant_index, data) => {
                assert_eq!(ident, "MyEnum");
                assert_eq!(variant_index, 0);
                assert!(data.is_some());
            }
            _ => panic!("Expected Enum"),
        }
    }

    #[test]
    fn test_evaluate_enum_expression_tuple() {
        let (mut env, scope) = get_new_env();
        let enum_node = NodeType::TypeDeclaration {
            identifier: "MyEnum".to_string(),
            object: Type::Enum(vec![(
                String::from("Foo"),
                Some(ObjectType::Tuple(vec![RuntimeType::Int])),
            )]),
        };

        env.evaluate(&scope, enum_node).unwrap();

        let exp = NodeType::EnumExpression {
            identifier: "MyEnum".to_string(),
            value: "Foo".to_string(),
            data: Some(ObjectType::Tuple(vec![Some(NodeType::IntLiteral(10))])),
        };
        let result = env.evaluate_enum_expression(&scope, exp).unwrap();
        match result {
            RuntimeValue::Enum(_, ident, variant_index, data) => {
                assert_eq!(ident, "MyEnum");
                assert_eq!(variant_index, 0);
                assert!(data.is_some());
            }
            _ => panic!("Expected Enum"),
        }
    }
}
