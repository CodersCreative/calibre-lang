use crate::environment::{MiddleEnvironment, MiddleScope, MiddleVariable, get_disamubiguous_name};
use calibre_parser::{
    Parser,
    ast::{ParserDataType, VarType},
};
use calibre_std::{get_globals_path, get_stdlib_module_path, get_stdlib_path};
use rustc_hash::FxHashMap;
use std::{fs, path::PathBuf};

impl MiddleEnvironment {
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
            defined: Vec::new(),
            defers: Vec::new(),
        });

        self.new_scope(Some(scope), path, Some("root"))
    }

    pub fn new_root_scope_with_std(
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
        let global_path = get_globals_path();
        if let Ok(globals) = fs::read_to_string(global_path.clone()) {
            let program = parser.produce_ast(&globals);
            let error_count_before = self.errors.len();
            let middle = self.evaluate(&scope, program);

            if self.errors.len() > error_count_before {
                let new_errors: Vec<_> = self.errors.drain(error_count_before..).collect();
                for err in new_errors {
                    self.errors.push(crate::errors::MiddleErr::InFile {
                        path: global_path.clone(),
                        contents: globals.clone(),
                        error: Box::new(err),
                    });
                }
            }

            self.stdlib_nodes.push(middle);
        }

        let std = self.new_scope(Some(scope), get_stdlib_path(), Some("std"));

        self.setup_std(&std);

        self.new_scope(Some(scope), path, Some("root"))
    }

    pub fn setup_global(&mut self, scope: &u64) {
        let funcs = [
            "console_output",
            "ok",
            "err",
            "some",
            "trim",
            "repr",
            "print",
            "len",
            "panic",
            "assert",
            "gen_suspend",
            "tuple",
            "discriminant",
            "min_or_zero",
            "http_request_raw",
            "http_request_try",
        ];

        let map = ParserDataType::natives();

        let mut funcs = funcs
            .iter()
            .filter_map(|x| map.get(*x).cloned().map(|t| (String::from(*x), t)))
            .collect();

        let mut vars: Vec<(String, ParserDataType)> =
            ParserDataType::constants().into_iter().collect();
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

            if let Some(scope_ref) = self.scopes.get_mut(scope) {
                scope_ref.mappings.insert(var.0, name);
            }
        }

        self.register_tag_handlers();
    }

    pub fn setup_std(&mut self, scope: &u64) {
        let mut parser = Parser::default();

        if let Some(scope_ref) = self.scopes.get(scope)
            && let Ok(stdlib) = fs::read_to_string(&scope_ref.path)
        {
            let scope_path = scope_ref.path.clone();
            let program = parser.produce_ast(&stdlib);
            let error_count_before = self.errors.len();
            let middle = self.evaluate(scope, program);
            self.stdlib_nodes.push(middle);
            self.loaded_scopes.insert(*scope);

            if self.errors.len() > error_count_before {
                let new_errors: Vec<_> = self.errors.drain(error_count_before..).collect();
                for err in new_errors {
                    self.errors.push(crate::errors::MiddleErr::InFile {
                        path: scope_path.clone(),
                        contents: stdlib.clone(),
                        error: Box::new(err),
                    });
                }
            }
        }

        let mut add = |name, funcs, load| self.setup_std_module(scope, name, funcs, load);

        add("traits", &[], true);
        add("thread", &[], true);
        add("console", &[], false);
        add(
            "async",
            &[
                "channel_new",
                "channel_send",
                "channel_get",
                "channel_try_get",
                "channel_try_send",
                "channel_close",
                "channel_closed",
                "waitgroup_new",
                "waitgroup_raw_add",
                "waitgroup_raw_done",
                "waitgroup_join",
                "waitgroup_wait",
                "waitgroup_count",
                "mutex_new",
                "mutex_get",
                "mutex_set",
                "mutex_with",
                "mutex_write",
            ],
            true,
        );
        add("random", &[], false);
        add("fs", &["read_dir"], false);
        add("list", &["sort_by", "binary_search_by", "raw_remove"], true);
        add(
            "collections",
            &[
                "hashmap_new",
                "hashmap_set",
                "hashmap_get",
                "hashmap_remove",
                "hashmap_contains",
                "hashmap_len",
                "hashmap_keys",
                "hashmap_values",
                "hashmap_entries",
                "hashmap_clear",
                "hashset_new",
                "hashset_add",
                "hashset_remove",
                "hashset_contains",
                "hashset_len",
                "hashset_values",
                "hashset_clear",
            ],
            true,
        );
        add(
            "str",
            &[
                "split",
                "contains",
                "starts_with",
                "ends_with",
                "char_lowercase",
                "char_uppercase",
            ],
            true,
        );
        add(
            "env",
            &["get", "var", "set_var", "remove_var", "vars"],
            true,
        );
        add("range", &[], true);
        add("generators", &[], true);
        add("crypto", &["sha256", "sha512", "blake3"], false);
        add("regex", &["is_match", "find", "replace"], false);
        add("process", &["raw_exec"], false);
        add("math", &[], true);
        add(
            "net",
            &[
                "tcp_connect",
                "tcp_listen",
                "tcp_accept",
                "tcp_read",
                "tcp_write",
                "tcp_close",
                "http_request_raw",
                "http_request_try",
            ],
            false,
        );
        add("option", &[], true);
        add("result", &[], true);
        add("json", &[], false);
    }

    pub fn setup_std_module(
        &mut self,
        parent: &u64,
        name: &str,
        funcs: &[&'static str],
        load_source: bool,
    ) {
        let scope_path = get_stdlib_module_path(name);
        let scope = self.new_scope(Some(*parent), scope_path.clone(), Some(name));

        let map: FxHashMap<String, ParserDataType> = ParserDataType::natives();
        let funcs: Vec<(String, ParserDataType)> = funcs
            .iter()
            .filter_map(|x| {
                map.get(&format!("{}.{}", name, x))
                    .cloned()
                    .map(|ty| (String::from(*x), ty))
            })
            .collect();

        for var in funcs.iter().cloned() {
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
                scope_ref.mappings.insert(var.0.clone(), name);
            }
        }

        if load_source {
            let mut parser = Parser::default();
            if let Ok(stdlib) = fs::read_to_string(&scope_path) {
                let scope_path_clone = scope_path.clone();
                parser.set_source_path(Some(scope_path.clone()));
                let program = parser.produce_ast(&stdlib);
                if !parser.errors.is_empty() {
                    self.errors.push(crate::errors::MiddleErr::ParserErrors {
                        path: scope_path.clone(),
                        contents: stdlib,
                        errors: std::mem::take(&mut parser.errors),
                    });
                    return;
                }

                let error_count_before = self.errors.len();
                let middle = self.evaluate(&scope, program);

                self.stdlib_nodes.push(middle);
                self.loaded_scopes.insert(scope);

                if self.errors.len() > error_count_before {
                    let new_errors: Vec<_> = self.errors.drain(error_count_before..).collect();
                    for err in new_errors {
                        self.errors.push(crate::errors::MiddleErr::InFile {
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
