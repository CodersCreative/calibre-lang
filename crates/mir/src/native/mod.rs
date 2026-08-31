use crate::{
    environment::MiddleEnvironment,
    errors::MiddleErr,
    scoping::{MiddleScope, ScopeId, Scoping},
};
use calibre_parser::{
    Parser,
    ast::{
        idents::ParserText,
        nodes::{AstNodeType, VarType},
        types::ParserDataType,
    },
};
use calibre_std::{get_globals_path, get_stdlib_module_path, get_stdlib_path};
use std::{fs, path::PathBuf};
use ustr::{Ustr, UstrMap};

impl Scoping {
    pub fn new_root_scope_no_std(
        &mut self,
        parent: Option<ScopeId>,
        path: PathBuf,
        namespace: Option<&Ustr>,
    ) -> ScopeId {
        let scope = self.add_scope(
            MiddleScope {
                macros: UstrMap::default(),
                macro_args: UstrMap::default(),
                namespace: namespace.cloned().unwrap_or_default(),
                path: path.clone(),
                mappings: UstrMap::default(),
                type_mappings: UstrMap::default(),
                children: UstrMap::default(),
                defers: Vec::new(),
                built: false,
            },
            parent,
        );

        self.new_scope(Some(scope), path, Some(&Ustr::from("root")))
    }
}

impl MiddleEnvironment {
    pub fn new_root_scope_with_std(
        &mut self,
        parent: Option<ScopeId>,
        path: PathBuf,
        namespace: Option<&Ustr>,
    ) -> ScopeId {
        self.register_tag_handlers();
        let scope = self.scoping.add_scope(
            MiddleScope {
                macros: UstrMap::default(),
                macro_args: UstrMap::default(),
                namespace: namespace.cloned().unwrap_or_default(),
                path: path.clone(),
                mappings: UstrMap::default(),
                type_mappings: UstrMap::default(),
                children: UstrMap::default(),
                defers: Vec::new(),
                built: false,
            },
            parent,
        );

        self.setup_global(scope);
        self.context.stdlib_nodes.clear();
        let mut parser = Parser::default();
        let global_path = get_globals_path();
        if let Ok(globals) = fs::read_to_string(global_path.clone()) {
            let program = parser.produce_ast(&globals);

            if !parser.errors.is_empty() {
                let errors = std::mem::take(&mut parser.errors);
                self.context.errors.push(MiddleErr::ParserErrors {
                    path: global_path.clone(),
                    contents: globals.clone(),
                    errors,
                });
            }

            let error_count_before = self.context.errors.len();
            let middle = self.evaluate(scope, program);

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
            .new_scope(Some(scope), get_stdlib_path(), Some(&Ustr::from("std")));

        self.setup_std(std);

        self.scoping
            .new_scope(Some(scope), path, Some(&Ustr::from("root")))
    }

    pub fn setup_global(&mut self, scope: ScopeId) {
        let mut funcs = ParserDataType::natives()
            .iter()
            .filter(|x| !x.0.contains("."))
            .collect();

        let mut vars: Vec<(&String, &ParserDataType)> =
            ParserDataType::constants().iter().collect();
        vars.append(&mut funcs);

        for (name, var) in vars {
            let name = Ustr::from(name);
            let new_name = Ustr::from(&ParserText::temp_name_with_suffix(name, var.span).text);

            let _ = self.register_variable(scope, name, new_name, var.clone(), VarType::Constant);

            self.symbols.native_mappings.insert(name, new_name);
        }
    }

    pub fn setup_std(&mut self, scope: ScopeId) {
        let mut parser = Parser::default();

        if let Ok(scope_ref) = self.scoping.scope_or_err(scope)
            && let Ok(stdlib) = fs::read_to_string(&scope_ref.path)
        {
            let scope_path = scope_ref.path.clone();
            let mut program = parser.produce_ast(&stdlib);

            if !parser.errors.is_empty() {
                let errors = std::mem::take(&mut parser.errors);
                self.context.errors.push(MiddleErr::ParserErrors {
                    path: scope_path.clone(),
                    contents: stdlib.clone(),
                    errors,
                });
            }

            if let AstNodeType::ScopeDeclaration {
                body: Some(body), ..
            } = &mut program.node_type
            {
                self.predeclare_nodes(scope, body);
            }

            let error_count_before = self.context.errors.len();
            let middle = self.evaluate(scope, program);
            self.context.stdlib_nodes.push(middle);

            if let Ok(x) = self.scoping.scope_mut_or_err(scope) {
                x.built = true
            };

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

        let mut add = |name, load| self.setup_std_module(scope, Ustr::from(name), load);

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

    pub fn setup_std_module(&mut self, parent: ScopeId, name: Ustr, load_source: bool) {
        let scope_path = get_stdlib_module_path(&name);
        let scope = self
            .scoping
            .new_scope(Some(parent), scope_path.clone(), Some(&name));

        let funcs: Vec<(&String, &ParserDataType)> = ParserDataType::natives()
            .iter()
            .filter(|x| x.0.contains(&format!("{}.", name)))
            .collect();

        for (original_name, var) in funcs {
            let short_name = Ustr::from(
                original_name
                    .rsplit_once(".")
                    .map(|x| x.1)
                    .unwrap_or(original_name)
                    .trim(),
            );

            let name = Ustr::from(&ParserText::temp_name_with_suffix(&short_name, var.span).text);

            let _ = self.register_variable(
                scope,
                short_name,
                name.clone(),
                var.clone(),
                VarType::Constant,
            );

            self.symbols
                .native_mappings
                .insert(Ustr::from(original_name), name);
        }

        if load_source {
            let mut parser = Parser::default();
            if let Ok(stdlib) = fs::read_to_string(&scope_path) {
                let scope_path_clone = scope_path.clone();
                parser.set_source_path(Some(scope_path.clone()));
                let mut program = parser.produce_ast(&stdlib);

                if !parser.errors.is_empty() {
                    self.context.errors.push(MiddleErr::ParserErrors {
                        path: scope_path.clone(),
                        contents: stdlib,
                        errors: std::mem::take(&mut parser.errors),
                    });
                    return;
                }

                if let AstNodeType::ScopeDeclaration {
                    body: Some(body), ..
                } = &mut program.node_type
                {
                    self.predeclare_nodes(scope, body);
                }

                let error_count_before = self.context.errors.len();
                let middle = self.evaluate(scope, program);

                self.context.stdlib_nodes.push(middle);
                if let Ok(x) = self.scoping.scope_mut_or_err(scope) {
                    x.built = true
                };

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
