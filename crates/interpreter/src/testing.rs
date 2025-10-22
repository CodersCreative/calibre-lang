use std::{path::PathBuf, str::FromStr, time::SystemTime};

use calibre_common::testing::{Testable, TestingError, TestingParams};
use calibre_parser::lexer::Tokenizer;

use crate::runtime::{
    scope::InterpreterEnvironment,
    values::{RuntimeType, RuntimeValue},
};

impl Testable<RuntimeValue, RuntimeType> for InterpreterEnvironment {
    fn test(
        program: String,
        params: TestingParams<RuntimeValue, RuntimeType>,
    ) -> (Vec<TestingError<RuntimeValue, RuntimeType>>, Self) {
        let mut env = InterpreterEnvironment::new();
        let mut parser = calibre_parser::Parser::default();
        let scope = env.new_scope_with_stdlib(None, PathBuf::from_str("./main.cl").unwrap(), None);
        let mut tokenizer = Tokenizer::default();
        let start = SystemTime::now();
        let program = parser.produce_ast(tokenizer.tokenize(program).unwrap());

        if parser.errors.len() > 0 {
            return (
                parser
                    .errors
                    .into_iter()
                    .map(|x| TestingError::Runtime(x.into()))
                    .collect(),
                env,
            );
        }

        let result = match env.evaluate(&scope, program) {
            Ok(x) => x,
            Err(e) => return (vec![TestingError::Runtime(e)], env),
        };

        let duration = SystemTime::now().duration_since(start).unwrap();

        let mut errs = Vec::new();
        if let Some(d) = params.duration {
            if d < duration {
                errs.push(TestingError::Duration(duration));
            }
        }

        if let Some(r) = params.result {
            if result != r {
                errs.push(TestingError::Result(result));
            }
        }

        for var in params.variables {
            let v = match env.get_var(&scope, &var.0) {
                Ok(x) => x,
                Err(e) => {
                    errs.push(TestingError::Runtime(e.into()));
                    continue;
                }
            };
            if v.value != var.1.value || v.var_type != var.1.var_type {
                errs.push(TestingError::Variable(var.0));
            }
        }

        for obj in params.objects {
            let o = match env.get_object(&scope, &obj.0) {
                Ok(x) => x,
                Err(e) => {
                    errs.push(TestingError::Runtime(e.into()));
                    continue;
                }
            };

            if o.object_type != obj.1.object_type
                || o.functions != obj.1.functions
                || o.traits != obj.1.traits
            {
                errs.push(TestingError::Object(obj.0));
            }
        }

        (errs, env)
    }
}
