use std::{path::PathBuf, str::FromStr, time::SystemTime};

use calibre_common::testing::{Testable, TestingError, TestingParams};
use calibre_parser::lexer::Tokenizer;

use crate::runtime::{scope::CheckerEnvironment, values::RuntimeType};

impl Testable<RuntimeType, RuntimeType> for CheckerEnvironment {
    fn test(
        program: String,
        params: TestingParams<RuntimeType, RuntimeType>,
    ) -> (
        Vec<TestingError<RuntimeType, RuntimeType>>,
        CheckerEnvironment,
    ) {
        let mut env = CheckerEnvironment::new();
        let mut parser = calibre_parser::Parser::default();
        let scope = env.new_scope_with_stdlib(None, PathBuf::from_str("./main.cl").unwrap(), None);
        let mut tokenizer = Tokenizer::default();
        let start = SystemTime::now();
        let program = parser.produce_ast(tokenizer.tokenize(program).unwrap());

        env.add_parser_errors(parser.errors);
        let result = env.start_evaluate(&scope, program);

        let mut errs = Vec::new();
        errs.append(&mut env.errors);
        let mut errs: Vec<TestingError<RuntimeType, RuntimeType>> =
            errs.into_iter().map(|x| TestingError::Runtime(x)).collect();

        let duration = SystemTime::now().duration_since(start).unwrap();

        if let Some(r) = params.result {
            if result != r {
                errs.push(TestingError::Result(result));
            }
        }

        if let Some(d) = params.duration {
            if d < duration {
                errs.push(TestingError::Duration(duration));
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
