use calibre_mir::ast::MiddleNode;

use crate::runtime::{
    interpreter::InterpreterErr,
    scope::InterpreterEnvironment,
    values::{RuntimeValue, helper::StopValue},
};

impl InterpreterEnvironment {
    pub fn evaluate_loop_declaration(
        &mut self,
        scope: &u64,
        state: Option<MiddleNode>,
        body: MiddleNode,
    ) -> Result<RuntimeValue, InterpreterErr> {
        let mut result = RuntimeValue::Null;
        let new_scope = self.get_new_scope(scope, Vec::new(), Vec::new())?;

        if let Some(state) = state {
            let _ = self.evaluate(scope, state)?;
        }

        loop {
            result = self.evaluate(&new_scope, body.clone())?;
            self.remove_scope(&new_scope);

            match self.stop {
                Some(StopValue::Break) => break,
                Some(StopValue::Return) => {
                    self.stop = Some(StopValue::Return);
                    break;
                }
                _ => {}
            };
        }

        Ok(result)
    }
}

#[cfg(test)]
mod tests {
    use std::{path::PathBuf, str::FromStr};

    use calibre_parser::ast::{LoopType, NodeType, VarType};

    use crate::runtime::{scope::Environment, values::RuntimeValue};

    fn get_new_env() -> (Environment, u64) {
        let mut env = Environment::new();
        let scope = env.new_scope_with_stdlib(None, PathBuf::from_str("./main.cl").unwrap(), None);
        (env, scope)
    }

    #[test]
    fn test_evaluate_loop_declaration_for_range() {
        let (mut env, scope) = get_new_env();

        let loop_type = Box::new(LoopType::For(
            "i".to_string(),
            NodeType::RangeDeclaration {
                from: Box::new(NodeType::IntLiteral(0)),
                to: Box::new(NodeType::IntLiteral(3)),
                inclusive: false,
            },
        ));

        let body = Box::new(NodeType::VariableDeclaration {
            var_type: VarType::Mutable,
            identifier: "x".to_string(),
            value: Box::new(NodeType::Identifier("i".to_string())),
            data_type: None,
        });

        let node = NodeType::LoopDeclaration { loop_type, body };
        let result = env
            .evaluate(&scope, node)
            .unwrap()
            .unwrap_val(&env, &scope)
            .unwrap();

        assert_eq!(result, RuntimeValue::Int(2));
    }
}
