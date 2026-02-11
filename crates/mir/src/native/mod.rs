use calibre_parser::{
    Parser,
    ast::{ParserDataType, VarType},
    lexer::Tokenizer,
};
use calibre_std::{get_globals_path, get_stdlib_module_path, get_stdlib_path};
use rustc_hash::FxHashMap;
use std::{fs, path::PathBuf};

use crate::environment::{MiddleEnvironment, MiddleScope, MiddleVariable, get_disamubiguous_name};

impl MiddleEnvironment {
    pub fn new_scope_with_stdlib(
        &mut self,
        parent: Option<u64>,
        path: PathBuf,
        namespace: Option<&str>,
    ) -> u64 {
        let scope = 0;
        let counter = self.scope_counter;

        self.add_scope(MiddleScope {
            id: 0,
            macros: FxHashMap::default(),
            macro_args: FxHashMap::default(),
            namespace: namespace.unwrap_or(&counter.to_string()).to_string(),
            parent,
            children: FxHashMap::default(),
            path: path.clone(),
            mappings: FxHashMap::default(),
            defined: Vec::new(),
            defers: Vec::new(),
        });

        self.setup_global(&scope);
        self.stdlib_nodes.clear();
        let mut parser = Parser::default();
        let mut tokenizer = Tokenizer::default();
        if let Ok(globals) = fs::read_to_string(get_globals_path()) {
            if let Ok(tokens) = tokenizer.tokenize(&globals) {
                let program = parser.produce_ast(tokens);
                let middle = self.evaluate(&scope, program);
                self.stdlib_nodes.push(middle);
            }
        }

        let std = self.new_scope(Some(scope), get_stdlib_path(), Some("std"));

        self.setup_std(&std);

        let root = self.new_scope(Some(scope), path, Some("root"));

        root
    }

    pub fn setup_global(&mut self, scope: &u64) {
        let funcs: Vec<&str> = vec![
            "console_output",
            "ok",
            "err",
            "some",
            "trim",
            "str_split",
            "str_contains",
            "str_starts_with",
            "str_ends_with",
            "print",
            "len",
            "panic",
            "tuple",
            "discriminant",
            "min_or_zero",
        ];

        let map = ParserDataType::natives();

        let mut funcs = funcs
            .into_iter()
            .filter_map(|x| map.get(x).cloned().map(|t| (String::from(x), t)))
            .collect();

        let mut vars: Vec<(String, ParserDataType)> =
            ParserDataType::constants().into_iter().map(|x| x).collect();
        vars.append(&mut funcs);

        for var in vars {
            let name = var.0.clone();

            let _ = self.variables.insert(
                name.clone(),
                MiddleVariable {
                    data_type: var.1,
                    var_type: VarType::Constant,
                    location: None,
                },
            );

            if let Some(scope_ref) = self.scopes.get_mut(&scope) {
                scope_ref.mappings.insert(var.0, name);
            }
        }

        // builtin types are resolved via ParserDataType rather than seeded objects
    }

    pub fn setup_std(&mut self, scope: &u64) {
        let mut parser = Parser::default();
        let mut tokenizer = Tokenizer::default();

        if let Some(scope_ref) = self.scopes.get(scope) {
            if let Ok(stdlib) = fs::read_to_string(scope_ref.path.clone()) {
                if let Ok(tokens) = tokenizer.tokenize(&stdlib) {
                    let program = parser.produce_ast(tokens);
                    let middle = self.evaluate(scope, program);
                    self.stdlib_nodes.push(middle);
                }
            }
        }

        self.setup_std_module(scope, "thread", &[]);
        self.setup_std_module(scope, "console", &[]);
        self.setup_std_module(scope, "random", &[]);
        self.setup_std_module(scope, "file", &[]);
        self.setup_std_module(scope, "str", &[]);
        self.setup_std_module(scope, "list", &[]);
        self.setup_std_module(scope, "range", &[]);
    }

    pub fn setup_std_module(&mut self, parent: &u64, name: &str, funcs: &[&'static str]) {
        let scope_path = get_stdlib_module_path(name);
        let scope = self.new_scope(Some(*parent), scope_path.clone(), Some(name));

        let mut parser = Parser::default();
        let mut tokenizer = Tokenizer::default();
        if let Ok(stdlib) = fs::read_to_string(scope_path) {
            if let Ok(tokens) = tokenizer.tokenize(&stdlib) {
                let program = parser.produce_ast(tokens);
                let middle = self.evaluate(&scope, program);
                self.stdlib_nodes.push(middle);
            }
        }

        let map: FxHashMap<String, ParserDataType> = ParserDataType::natives();

        let funcs = funcs.into_iter().filter_map(|x| {
            map.get(&format!("{}.{}", name, x))
                .cloned()
                .map(|ty| (String::from(*x), ty))
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

            if let Some(scope_ref) = self.scopes.get_mut(&scope) {
                scope_ref.mappings.insert(var.0, name);
            }
        }
    }
}
