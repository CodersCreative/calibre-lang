use crate::{
    environment::MiddleEnvironment,
    errors::MiddleErr,
    scoping::{MiddleScope, Scoping},
    symbols::MiddleVariable,
};
use calibre_parser::{
    Parser,
    ast::{idents::ParserText, nodes::VarType, types::ParserDataType},
};
use calibre_std::{get_globals_path, get_stdlib_module_path, get_stdlib_path};
use rustc_hash::FxHashMap;
use std::{fs, path::PathBuf};

impl Scoping {
    pub fn new_root_scope_no_std(
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
            type_mappings: FxHashMap::default(),
            defers: Vec::new(),
        });

        self.new_scope(Some(scope), path, Some("root"))
    }
}

impl MiddleEnvironment {
    pub fn new_root_scope_with_std(
        &mut self,
        parent: Option<u64>,
        path: PathBuf,
        namespace: Option<&str>,
    ) -> u64 {
        let scope = 0;
        let counter = self.scoping.scope_counter;

        self.scoping.add_scope(MiddleScope {
            id: 0,
            macros: FxHashMap::default(),
            macro_args: FxHashMap::default(),
            namespace: namespace.unwrap_or(&counter.to_string()).to_string(),
            parent,
            children: FxHashMap::default(),
            path: path.clone(),
            mappings: FxHashMap::default(),
            type_mappings: FxHashMap::default(),
            defers: Vec::new(),
        });

        self.setup_global(&scope);
        self.context.stdlib_nodes.clear();
        let mut parser = Parser::default();
        let global_path = get_globals_path();
        if let Ok(globals) = fs::read_to_string(global_path.clone()) {
            let program = parser.produce_ast(&globals);
            let error_count_before = self.context.errors.len();
            let middle = self.evaluate(&scope, program);

            if self.context.errors.len() > error_count_before {
                let new_errors: Vec<_> = self.context.errors.drain(error_count_before..).collect();
                for err in new_errors {
                    self.context.errors.push(MiddleErr::InFile {
                        path: global_path.clone(),
                        contents: globals.clone(),
                        error: Box::new(err),
                    });
                }
            }

            self.context.stdlib_nodes.push(middle);
        }

        let std = self
            .scoping
            .new_scope(Some(scope), get_stdlib_path(), Some("std"));

        self.setup_std(&std);

        self.scoping.new_scope(Some(scope), path, Some("root"))
    }

    pub fn setup_global(&mut self, scope: &u64) {
        let mut funcs = ParserDataType::natives()
            .into_iter()
            .filter(|x| !x.0.contains("."))
            .collect();

        let mut vars: Vec<(String, ParserDataType)> =
            ParserDataType::constants().into_iter().collect();
        vars.append(&mut funcs);

        for var in vars {
            let name = var.0.clone();

            let _ = self.symbols.variables.insert(
                name.clone(),
                MiddleVariable {
                    data_type: var.1,
                    var_type: VarType::Constant,
                    location: None,
                },
            );

            self.symbols
                .native_mappings
                .insert(var.0.clone(), name.clone());

            if let Some(scope_ref) = self.scoping.scopes.get_mut(scope) {
                scope_ref.mappings.insert(var.0, name);
            }
        }

        self.register_tag_handlers();
    }

    pub fn setup_std(&mut self, scope: &u64) {
        let mut parser = Parser::default();

        if let Some(scope_ref) = self.scoping.scopes.get(scope)
            && let Ok(stdlib) = fs::read_to_string(&scope_ref.path)
        {
            let scope_path = scope_ref.path.clone();
            let program = parser.produce_ast(&stdlib);
            let error_count_before = self.context.errors.len();
            let middle = self.evaluate(scope, program);
            self.context.stdlib_nodes.push(middle);
            self.scoping.loaded_scopes.insert(*scope);

            if self.context.errors.len() > error_count_before {
                let new_errors: Vec<_> = self.context.errors.drain(error_count_before..).collect();
                for err in new_errors {
                    self.context.errors.push(MiddleErr::InFile {
                        path: scope_path.clone(),
                        contents: stdlib.clone(),
                        error: Box::new(err),
                    });
                }
            }
        }

        let mut add = |name, load| self.setup_std_module(scope, name, load);

        add("traits", true);
        add("thread", true);
        add("libc", true);
        add("console", false);
        add("async", true);
        add("random", false);
        add("fs", false);
        add("math", true);
        add("list", true);
        add("collections", true);
        add("str", true);
        add("env", true);
        add("range", true);
        add("generators", true);
        add("crypto", false);
        add("regex", false);
        add("process", false);
        add("net", false);
        add("option", true);
        add("result", true);
        add("json", false);
    }

    pub fn setup_std_module(&mut self, parent: &u64, name: &str, load_source: bool) {
        let scope_path = get_stdlib_module_path(name);
        let scope = self
            .scoping
            .new_scope(Some(*parent), scope_path.clone(), Some(name));

        let funcs: Vec<(String, ParserDataType)> = ParserDataType::natives()
            .into_iter()
            .filter(|x| x.0.contains(&format!("{}.", name)))
            .collect();

        for (original_name, var) in funcs {
            let short_name = original_name
                .split_once(".")
                .map(|x| x.1)
                .unwrap_or(&original_name)
                .trim()
                .to_string();
            let name = ParserText::temp_name_with_suffix(&short_name, var.span).text;

            let _ = self.symbols.variables.insert(
                name.clone(),
                MiddleVariable {
                    data_type: var.clone(),
                    var_type: VarType::Constant,
                    location: None,
                },
            );

            self.symbols
                .native_mappings
                .insert(original_name.clone(), name.clone());

            if let Some(scope_ref) = self.scoping.scopes.get_mut(&scope) {
                scope_ref.mappings.insert(short_name, name);
            }
        }

        if load_source {
            let mut parser = Parser::default();
            if let Ok(stdlib) = fs::read_to_string(&scope_path) {
                let scope_path_clone = scope_path.clone();
                parser.set_source_path(Some(scope_path.clone()));
                let program = parser.produce_ast(&stdlib);
                if !parser.errors.is_empty() {
                    self.context.errors.push(MiddleErr::ParserErrors {
                        path: scope_path.clone(),
                        contents: stdlib,
                        errors: std::mem::take(&mut parser.errors),
                    });
                    return;
                }

                let error_count_before = self.context.errors.len();
                let middle = self.evaluate(&scope, program);

                self.context.stdlib_nodes.push(middle);
                self.scoping.loaded_scopes.insert(scope);

                if self.context.errors.len() > error_count_before {
                    let new_errors: Vec<_> =
                        self.context.errors.drain(error_count_before..).collect();
                    for err in new_errors {
                        self.context.errors.push(MiddleErr::InFile {
                            path: scope_path_clone.clone(),
                            contents: stdlib.clone(),
                            error: Box::new(err),
                        });
                    }
                }
            }
        }
    }
}
