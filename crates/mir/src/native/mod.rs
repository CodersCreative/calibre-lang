use std::{collections::HashMap, fs, path::PathBuf};

use calibre_parser::{
    Parser,
    ast::{ParserDataType, VarType},
    lexer::Tokenizer,
};
use calibre_std::{get_globals_path, get_stdlib_path};

use crate::environment::{MiddleEnvironment, MiddleScope, MiddleVariable, get_disamubiguous_name};

impl MiddleEnvironment {
    pub fn new_scope_with_stdlib<'a>(
        &'a mut self,
        parent: Option<u64>,
        path: PathBuf,
        namespace: Option<&str>,
    ) -> u64 {
        // TODO finish this
        let scope = 0;
        let counter = self.scope_counter;

        self.add_scope(MiddleScope {
            id: 0,
            macros: HashMap::new(),
            macro_args: HashMap::new(),
            namespace: namespace.unwrap_or(&counter.to_string()).to_string(),
            parent,
            children: HashMap::new(),
            path: path.clone(),
            mappings: HashMap::new(),
        });

        self.setup_global(&scope);
        let mut parser = Parser::default();
        let mut tokenizer = Tokenizer::default();
        let program = parser.produce_ast(
            tokenizer
                .tokenize(fs::read_to_string(get_globals_path()).unwrap_or(String::new()))
                .unwrap(),
        );

        let _ = self.evaluate(&scope, program);

        let std = self.new_scope(Some(scope), get_stdlib_path(), Some("std"));

        self.setup_std(&std);

        let root = self.new_scope(Some(scope), path, Some("root"));

        root
    }

    pub fn setup_global(&mut self, scope: &u64) {
        let funcs: Vec<&str> = vec![
            "ok",
            "err",
            "some",
            "trim",
            "print",
            "len",
            "panic",
            "tuple",
            "discriminant",
        ];

        let map = ParserDataType::natives();

        let mut funcs = funcs
            .into_iter()
            .map(|x| (String::from(x), map.get(x).unwrap().clone()))
            .collect();

        let mut vars: Vec<(String, ParserDataType)> =
            ParserDataType::constants().into_iter().map(|x| x).collect();
        vars.append(&mut funcs);

        for var in vars {
            let name = get_disamubiguous_name(scope, Some(&var.0), None);

            let _ = self.variables.insert(
                name.clone(),
                MiddleVariable {
                    data_type: var.1,
                    var_type: VarType::Constant,
                    location: None,
                },
            );

            self.scopes
                .get_mut(&scope)
                .unwrap()
                .mappings
                .insert(var.0, name);
        }
    }

    pub fn setup_std(&mut self, scope: &u64) {
        let mut parser = Parser::default();
        let mut tokenizer = Tokenizer::default();

        let program = parser.produce_ast(
            tokenizer
                .tokenize(
                    fs::read_to_string(self.scopes.get(scope).unwrap().path.clone())
                        .unwrap_or(String::new()),
                )
                .unwrap(),
        );

        let _ = self.evaluate(scope, program);

        self.setup_std_scope(scope, "thread", &["wait"]);
        self.setup_std_scope(scope, "console", &["out", "input", "err", "clear"]);
        self.setup_std_scope(scope, "random", &["generate", "bool", "ratio"]);
    }

    pub fn setup_std_scope(&mut self, parent: &u64, name: &str, funcs: &[&'static str]) {
        let scope = self.new_scope(
            Some(*parent),
            self.scopes.get(parent).unwrap().path.clone(),
            Some(name),
        );
        let map: std::collections::HashMap<String, ParserDataType> = ParserDataType::natives();

        let funcs = funcs.into_iter().map(|x| {
            (
                String::from(*x),
                map.get(&format!("{}.{}", name, x)).clone().unwrap(),
            )
        });

        for var in funcs {
            let name = get_disamubiguous_name(&scope, Some(&var.0), None);
            let _ = self.variables.insert(
                name.clone(),
                MiddleVariable {
                    data_type: var.1.clone(),
                    var_type: VarType::Constant,
                    location: None,
                },
            );

            self.scopes
                .get_mut(&scope)
                .unwrap()
                .mappings
                .insert(var.0, name);
        }
    }
}
