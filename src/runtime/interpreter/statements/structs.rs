use std::{cell::RefCell, rc::Rc};

use crate::{
    ast::NodeType,
    runtime::{
        interpreter::{InterpreterErr, evaluate},
        scope::{Object, Scope, children::get_scope_path},
        values::{RuntimeType, RuntimeValue, helper::ObjectType},
    },
};

pub fn evaluate_impl_declaration(
    declaration: NodeType,
    scope: &Rc<RefCell<Scope>>,
) -> Result<RuntimeValue, InterpreterErr> {
    if let NodeType::ImplDeclaration {
        identifier,
        functions,
    } = declaration
    {
        for function in functions {
            let scope_2 = Scope::new_from_parent_shallow(scope.clone());

            if let NodeType::VariableDeclaration {
                var_type,
                identifier: iden,
                value: Some(value),
                data_type,
            } = function.0
            {
                let func = evaluate(*value, &scope_2)?;

                let _ = scope
                    .borrow_mut()
                    .push_function(identifier.clone(), (iden, func, function.1))?;
                continue;
            }

            return Err(InterpreterErr::ExpectedFunctions);
        }

        Ok(RuntimeValue::Null)
    } else {
        Err(InterpreterErr::NotImplemented(declaration))
    }
}

pub fn evaluate_type_declaration(
    declaration: NodeType,
    scope: &Rc<RefCell<Scope>>,
) -> Result<RuntimeValue, InterpreterErr> {
    if let NodeType::TypeDeclaration { identifier, object } = declaration {
        let _ = scope.borrow_mut().push_object(identifier, object)?;
        Ok(RuntimeValue::Null)
    } else {
        Err(InterpreterErr::NotImplemented(declaration))
    }
}

#[cfg(test)]
mod tests {
    use std::{cell::RefCell, rc::Rc};

    use crate::{
        ast::NodeType,
        runtime::{
            interpreter::statements::structs::{
                evaluate_enum_declaration, evaluate_struct_declaration,
            },
            scope::Scope,
            values::{RuntimeValue, helper::ObjectType},
        },
    };

    fn new_scope() -> Rc<RefCell<Scope>> {
        Scope::new(None)
    }
    #[test]
    fn test_evaluate_struct_declaration() {
        let scope = new_scope();
        let node = NodeType::StructDeclaration {
            identifier: "Point".to_string(),
            properties: ObjectType::Tuple(vec![]),
        };
        let result = evaluate_struct_declaration(node, &scope).unwrap();
        assert_eq!(result, RuntimeValue::Null);
        assert!(scope.borrow().objects.contains_key("Point"));
    }

    #[test]
    fn test_evaluate_enum_declaration() {
        let scope = new_scope();
        let node = NodeType::EnumDeclaration {
            identifier: "Color".to_string(),
            options: vec![("Red".to_string(), None), ("Blue".to_string(), None)],
        };
        let result = evaluate_enum_declaration(node, &scope).unwrap();
        assert_eq!(result, RuntimeValue::Null);
        assert!(scope.borrow().objects.contains_key("Color"));
    }
}
