use std::{cell::RefCell, rc::Rc};

use rand::seq::IndexedRandom;

use crate::{
    ast::NodeType,
    runtime::{
        interpreter::{InterpreterErr, evaluate},
        scope::{
            Scope,
            children::{get_scope_list, import_scope_list},
            variables::get_var,
        },
        values::RuntimeValue,
    },
};

pub fn evaluate_import_statement(
    declaration: NodeType,
    scope: &Rc<RefCell<Scope>>,
) -> Result<RuntimeValue, InterpreterErr> {
    if let NodeType::ImportStatement {
        module,
        alias,
        values,
    } = declaration
    {
        let new_scope = if let Some(alias) = alias {
            if ["super", "root"].contains(&alias.as_str()) {
                return Ok(RuntimeValue::Null);
            }

            let _ = scope
                .borrow_mut()
                .push_child(alias, get_scope_list(scope, module)?);

            return Ok(RuntimeValue::Null);
        } else if !values.is_empty() {
            get_scope_list(scope, module.clone())?
        } else {
            let _ = import_scope_list(scope, module.clone());
            return Ok(RuntimeValue::Null);
        };

        if &values[0] == "*" {
            for (value, var) in new_scope.borrow().variables.iter() {
                scope.borrow_mut().push_var(
                    value.clone(),
                    RuntimeValue::Link(
                        [module.clone(), vec![value.to_string()]].concat(),
                        (&var.0).into(),
                    ),
                    var.1.clone(),
                )?;
            }

            for (value, obj) in new_scope.borrow().objects.iter() {
                scope.borrow_mut().push_object(value.clone(), obj.clone())?;
            }
        } else {
            for value in values {
                if let Some(var) = new_scope.borrow().variables.get(&value) {
                    scope.borrow_mut().push_var(
                        value.clone(),
                        RuntimeValue::Link([module.clone(), vec![value]].concat(), (&var.0).into()),
                        var.1.clone(),
                    )?;
                } else if let Some(obj) = new_scope.borrow().objects.get(&value) {
                    scope.borrow_mut().push_object(value.clone(), obj.clone())?;
                } else {
                    panic!()
                }
            }
        }

        Ok(RuntimeValue::Null)
    } else {
        Err(InterpreterErr::NotImplemented(declaration))
    }
}
