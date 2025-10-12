use std::{path::PathBuf, str::FromStr, time::SystemTime};

use calibre_common::testing::{TestingParams, TestingResult};
use calibre_parser::lexer::Tokenizer;

use crate::runtime::{
    interpreter::InterpreterErr,
    scope::InterpreterEnvironment,
    values::{RuntimeType, RuntimeValue},
};

impl InterpreterEnvironment {
    pub fn test(
        program: String,
        params: TestingParams<RuntimeValue, RuntimeType>,
    ) -> Result<(Self, TestingResult<RuntimeValue>), InterpreterErr> {
        let mut env = InterpreterEnvironment::new();
        let mut parser = calibre_parser::Parser::default();
        let scope = env.new_scope_with_stdlib(None, PathBuf::from_str("./main.cl").unwrap(), None);
        let mut tokenizer = Tokenizer::default();
        let start = SystemTime::now();
        let program = parser
            .produce_ast(tokenizer.tokenize(program).unwrap())
            .unwrap();
        let result = env.evaluate(&scope, program)?;

        let duration = SystemTime::now().duration_since(start).unwrap();

        if let Some(r) = params.result {
            if result != r {
                return Ok((env, TestingResult::Result(result)));
            }
        }

        if let Some(d) = params.duration {
            if d < duration {
                return Ok((env, TestingResult::Duration));
            }
        }

        for var in params.variables {
            let v = env.get_var(&scope, &var.0)?;
            if v.value != var.1.value || v.var_type != var.1.var_type {
                return Ok((env, TestingResult::Variable(var.0)));
            }
        }

        for obj in params.objects {
            let o = env.get_object(&scope, &obj.0)?;
            if o.object_type != obj.1.object_type
                || o.functions != obj.1.functions
                || o.traits != obj.1.traits
            {
                return Ok((env, TestingResult::Object(obj.0)));
            }
        }

        Ok((env, TestingResult::Ok))
    }
}
