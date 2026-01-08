use crate::runtime::{
    interpreter::InterpreterErr, scope::InterpreterEnvironment, values::RuntimeValue,
};
use calibre_common::{
    environment::Type,
    errors::{ScopeErr, ValueErr},
};
use calibre_parser::ast::{Node, ObjectType};
use std::collections::HashMap;

impl InterpreterEnvironment {
    pub fn evaluate_struct_expression(
        &mut self,
        scope: &u64,
        props: ObjectType<Option<Node>>,
    ) -> Result<RuntimeValue, InterpreterErr> {
        if let ObjectType::Map(props) = props {
            let mut properties = HashMap::new();
            for (k, v) in props {
                let value = if let Some(value) = v {
                    self.evaluate(scope, value)?
                } else {
                    let pointer = self.get_var_pointer(scope, &k)?;
                    self.get_var(&pointer)?.value.clone()
                };

                properties.insert(k, value);
            }

            return Ok(RuntimeValue::Struct(None, ObjectType::Map(properties)));
        } else if let ObjectType::Tuple(props) = props {
            let mut properties = Vec::new();
            for v in props {
                if let Some(value) = v {
                    properties.push(self.evaluate(scope, value)?);
                }
            }
            return Ok(RuntimeValue::Struct(None, ObjectType::Tuple(properties)));
        }
        Ok(RuntimeValue::Null)
    }

    pub fn evaluate_enum_expression(
        &mut self,
        scope: &u64,
        identifier: String,
        value: String,
        data: Option<ObjectType<Option<Node>>>,
    ) -> Result<RuntimeValue, InterpreterErr> {
        let pointer = self.get_object_pointer(scope, &identifier)?;

        let enm_class = match self.get_object_type(&pointer) {
            Ok(Type::Enum(x)) => x.clone(),
            Err(e) => return Err(e.into()),
            _ => {
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
                            let pointer = self.get_var_pointer(scope, &k)?;
                            self.get_var(&pointer)?.value.clone()
                        };

                        data_vals.insert(k, value);
                    }
                }

                let mut new_data_vals = HashMap::new();
                for property in properties {
                    if let Some(val) = data_vals.remove(property.0) {
                        new_data_vals.insert(property.0.clone(), val);
                    } else {
                        return Err(InterpreterErr::PropertyNotFound(property.0.to_string()));
                    }
                }

                let data = if new_data_vals.is_empty() {
                    None
                } else {
                    Some(ObjectType::Map(new_data_vals))
                };

                return Ok(RuntimeValue::Enum(pointer, i, data));
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
                for (i, _property) in properties.into_iter().enumerate() {
                    if data_vals.len() <= 0 {
                        return Err(InterpreterErr::InvalidIndex(i as i64));
                    }
                    new_data_vals.push(data_vals.remove(0));
                }

                let data = if new_data_vals.is_empty() {
                    None
                } else {
                    Some(ObjectType::Tuple(new_data_vals))
                };

                return Ok(RuntimeValue::Enum(pointer, i, data));
            }
            return Ok(RuntimeValue::Enum(pointer, i, None));
        } else {
            Err(InterpreterErr::UnexpectedEnumItem(identifier, value))
        }
    }
}

#[cfg(test)]
mod tests {
    use calibre_parser::ast::ParserDataType;
    use calibre_parser::ast::TypeDefType;

    use super::*;
    use crate::runtime::values::RuntimeValue;
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
        let result = env.evaluate(&scope, node).unwrap();

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
        let result = env.evaluate(&scope, node).unwrap();
        match result {
            RuntimeValue::Struct(_, _, ObjectType::Tuple(vals)) => {
                assert_eq!(vals, vec![RuntimeValue::UInt(1), RuntimeValue::UInt(2)]);
            }
            _ => panic!("Expected Struct(Tuple)"),
        }
    }

    #[test]
    fn test_evaluate_enum_expression_map() {
        let (mut env, scope) = get_new_env();
        let mut enum_props = HashMap::new();
        enum_props.insert("y".to_string(), ParserDataType::Int);
        let enum_node = NodeType::TypeDeclaration {
            identifier: "MyEnum".to_string(),
            object: TypeDefType::Enum(vec![(String::from("x"), Some(ObjectType::Map(enum_props)))]),
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
        let result = env.evaluate(&scope, exp).unwrap();
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
            object: TypeDefType::Enum(vec![(
                String::from("Foo"),
                Some(ObjectType::Tuple(vec![ParserDataType::Int])),
            )]),
        };

        env.evaluate(&scope, enum_node).unwrap();

        let exp = NodeType::EnumExpression {
            identifier: "MyEnum".to_string(),
            value: "Foo".to_string(),
            data: Some(ObjectType::Tuple(vec![Some(NodeType::IntLiteral(10))])),
        };
        let result = env.evaluate(&scope, exp).unwrap();
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
