use std::{cell::RefCell, rc::Rc};

use crate::{
    ast::NodeType,
    runtime::{
        interpreter::{InterpreterErr, evaluate},
        scope::{Object, Scope},
        values::RuntimeValue,
    },
};

pub fn evaluate_impl_declaration(
    declaration: NodeType,
    scope: Rc<RefCell<Scope>>,
) -> Result<RuntimeValue, InterpreterErr> {
    if let NodeType::ImplDeclaration {
        identifier,
        functions,
    } = declaration
    {
        for function in functions {
            let scope_2 = Rc::new(RefCell::new(Scope::new(Some(scope.clone()))));
            let func = evaluate(function.0, scope_2)?;

            if let RuntimeValue::Function {
                identifier: iden, ..
            } = func.clone()
            {
                let _ = scope
                    .borrow_mut()
                    .push_function(identifier.clone(), (iden, func, function.1))?;
            } else {
                return Err(InterpreterErr::ExpectedFunctions);
            }
        }

        Ok(RuntimeValue::Null)
    } else {
        Err(InterpreterErr::NotImplemented(declaration))
    }
}

pub fn evaluate_enum_declaration(
    declaration: NodeType,
    scope: Rc<RefCell<Scope>>,
) -> Result<RuntimeValue, InterpreterErr> {
    if let NodeType::EnumDeclaration {
        identifier,
        options,
    } = declaration
    {
        let _ = scope
            .borrow_mut()
            .push_object(identifier, Object::Enum(options))?;
        Ok(RuntimeValue::Null)
    } else {
        Err(InterpreterErr::NotImplemented(declaration))
    }
}

pub fn evaluate_struct_declaration(
    declaration: NodeType,
    scope: Rc<RefCell<Scope>>,
) -> Result<RuntimeValue, InterpreterErr> {
    if let NodeType::StructDeclaration {
        identifier,
        properties,
    } = declaration
    {
        let _ = scope
            .borrow_mut()
            .push_object(identifier, Object::Struct(properties))?;
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
        Rc::new(RefCell::new(Scope::new(None)))
    }
    #[test]
    fn test_evaluate_struct_declaration() {
        let scope = new_scope();
        let node = NodeType::StructDeclaration {
            identifier: "Point".to_string(),
            properties: ObjectType::Tuple(vec![]),
        };
        let result = evaluate_struct_declaration(node, scope.clone()).unwrap();
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
        let result = evaluate_enum_declaration(node, scope.clone()).unwrap();
        assert_eq!(result, RuntimeValue::Null);
        assert!(scope.borrow().objects.contains_key("Color"));
    }
}
