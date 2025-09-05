use std::collections::HashMap;

use calibre_parser::ast::NodeType;

use crate::runtime::{
    interpreter::InterpreterErr,
    scope::{Environment, Object},
    values::RuntimeValue,
};

impl Environment {
    pub fn evaluate_impl_declaration(
        &mut self,
        scope: &u64,
        declaration: NodeType,
    ) -> Result<RuntimeValue, InterpreterErr> {
        if let NodeType::ImplDeclaration {
            identifier,
            functions,
        } = declaration
        {
            for function in functions {
                let scope_2 = self.new_scope_from_parent_shallow(scope.clone());

                if let NodeType::VariableDeclaration {
                    identifier: iden,
                    value,
                    ..
                } = function.0
                {
                    let func = self.evaluate(&scope_2, *value)?.unwrap_val(self, scope)?;

                    let _ = self.push_function(scope, &identifier, (iden, func, function.1))?;
                    continue;
                }

                self.remove_scope(&scope_2);

                return Err(InterpreterErr::ExpectedFunctions);
            }

            Ok(RuntimeValue::Null)
        } else {
            Err(InterpreterErr::NotImplemented(declaration))
        }
    }

    pub fn evaluate_type_declaration(
        &mut self,
        scope: &u64,
        declaration: NodeType,
    ) -> Result<RuntimeValue, InterpreterErr> {
        if let NodeType::TypeDeclaration { identifier, object } = declaration {
            let _ = self.push_object(
                scope,
                identifier,
                Object {
                    object_type: object.into(),
                    functions: HashMap::new(),
                    traits: Vec::new(),
                },
            )?;
            Ok(RuntimeValue::Null)
        } else {
            Err(InterpreterErr::NotImplemented(declaration))
        }
    }
}

#[cfg(test)]
mod tests {
    use calibre_parser::ast::{NodeType, ObjectType};

    use crate::runtime::{
        scope::{Environment, Type},
        values::RuntimeValue,
    };
    use std::{path::PathBuf, str::FromStr};

    fn get_new_env() -> (Environment, u64) {
        let mut env = Environment::new();
        let scope = env.new_scope_with_stdlib(None, PathBuf::from_str("./main.cl").unwrap(), None);
        (env, scope)
    }

    #[test]
    fn test_evaluate_struct_declaration() {
        let (mut env, scope) = get_new_env();
        let node = NodeType::TypeDeclaration {
            identifier: "Point".to_string(),
            object: Type::Struct(ObjectType::Tuple(vec![])),
        };
        let result = env.evaluate_type_declaration(&scope, node).unwrap();

        assert_eq!(result, RuntimeValue::Null);
        assert!(env.get_object(&scope, "Point").is_ok());
    }

    #[test]
    fn test_evaluate_enum_declaration() {
        let (mut env, scope) = get_new_env();
        let node = NodeType::TypeDeclaration {
            identifier: "Color".to_string(),
            object: Type::Enum(vec![("Red".to_string(), None), ("Blue".to_string(), None)]),
        };
        let result = env.evaluate_type_declaration(&scope, node).unwrap();

        assert_eq!(result, RuntimeValue::Null);
        assert!(env.get_object(&scope, "Color").is_ok());
    }
}
