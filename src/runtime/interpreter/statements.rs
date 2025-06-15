use crate::{
    ast::NodeType,
    runtime::{interpreter::evaluate, scope::Scope, values::RuntimeValue},
};

pub fn evaluate_program(exp: NodeType, scope: &mut Scope) -> RuntimeValue {
    let mut last = RuntimeValue::Null;

    if let NodeType::Program(body) = exp {
        for statement in body.into_iter() {
            last = evaluate(statement, scope);
        }
    } else {
        panic!("Tried to evaluate non-program node using evaluate_program.")
    }

    last
}
pub fn evaluate_variable_declaration(declaration: NodeType, scope: &mut Scope) -> RuntimeValue {
    if let NodeType::VariableDeclaration {
        is_mutable,
        identifier,
        value,
    } = declaration
    {
        let value = match value {
            Some(x) => evaluate(*x, scope),
            None => RuntimeValue::Null,
        };

        scope.push_var(identifier, &value, is_mutable);

        value
    } else {
        panic!("Tried to evaluate non-declaration node using evaluate_variable_declaration.")
    }
}
