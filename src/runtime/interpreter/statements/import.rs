use std::{cell::RefCell, rc::Rc};

use crate::{ast::NodeType, runtime::{interpreter::InterpreterErr, scope::Scope, values::RuntimeValue}};

pub fn evaluate_import_statement(
    declaration: NodeType,
    scope: &Rc<RefCell<Scope>>,
) -> Result<RuntimeValue, InterpreterErr> {
    if let NodeType::ImportStatement { module, values } = declaration
    {
        let mut path = scope.borrow().path.clone();

        let _ = Scope::new(scope.clone(), path, values.last());
        Ok(RuntimeValue::Null)
    } else {
        Err(InterpreterErr::NotImplemented(declaration))
    }
}
