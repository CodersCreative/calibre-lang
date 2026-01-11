use crate::runtime::{
    interpreter::InterpreterErr,
    scope::InterpreterEnvironment,
    values::{RuntimeType, RuntimeValue},
};
use calibre_common::{
    environment::InterpreterFrom,
    errors::{ScopeErr, ValueErr},
};
use calibre_mir::{ast::MiddleNode, environment::MiddleTypeDefType};
use calibre_parser::ast::{ObjectMap, ObjectType};
use std::collections::HashMap;

impl InterpreterEnvironment {
    pub fn evaluate_aggregate_expression(
        &mut self,
        scope: &u64,
        identifier: Option<String>,
        props: ObjectMap<MiddleNode>,
    ) -> Result<RuntimeValue, InterpreterErr> {
        let mut properties = HashMap::new();
        for (k, value) in props.0 {
            let value = self.evaluate(scope, value)?;

            properties.insert(k.to_string(), value);
        }

        Ok(RuntimeValue::Aggregate(identifier, ObjectMap(properties)))
    }

    pub fn evaluate_enum_expression(
        &mut self,
        scope: &u64,
        identifier: String,
        value: String,
        data: Option<Box<MiddleNode>>,
    ) -> Result<RuntimeValue, InterpreterErr> {
        let enm_class = match self.get_object_type(&identifier) {
            Ok(MiddleTypeDefType::Enum(x)) => x.clone(),
            Err(e) => return Err(e.into()),
            _ => {
                return Err(InterpreterErr::Value(ValueErr::Scope(ScopeErr::Object(
                    identifier,
                ))));
            }
        };

        if let Some((i, enm)) = enm_class.iter().enumerate().find(|x| &x.1.0.text == &value) {
            if let (Some(value), Some(typ)) = (data, &enm.1) {
                let value = self.evaluate(scope, *value)?;
                if value.is_type(
                    self,
                    &RuntimeType::interpreter_from(self, scope, typ.clone())?,
                ) {
                    return Ok(RuntimeValue::Enum(identifier, i, Some(Box::new(value))));
                }
            }

            if enm.1.is_none() {
                return Ok(RuntimeValue::Enum(identifier, i, None));
            }
        }

        Err(InterpreterErr::UnexpectedEnumItem(identifier, value))
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
