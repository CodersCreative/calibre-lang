use std::{collections::HashMap, fs, path::PathBuf};

use calibre_parser::{
    Parser,
    ast::{Node, NodeType, ParserDataType, ParserInnerType, ParserText, TypeDefType, VarType},
    lexer::{Location, Span, Tokenizer},
};
use rand::random_range;

use crate::{ast::MiddleNode, errors::MiddleErr};

#[derive(Debug, Clone, PartialEq)]
pub struct MiddleObject {
    pub object_type: TypeDefType,
    pub functions: HashMap<String, (MiddleNode, Option<Location>, bool)>,
    pub traits: Vec<String>,
    pub location: Option<Location>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct MiddleVariable {
    pub data_type: ParserDataType,
    pub var_type: VarType,
    pub location: Option<Location>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct MiddleEnvironment {
    pub scope_counter: u64,
    pub scopes: HashMap<u64, MiddleScope>,
    pub variables: HashMap<String, MiddleVariable>,
    pub resolved_variables: Vec<String>,
    pub objects: HashMap<String, MiddleObject>,
    pub current_location: Option<Location>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct MiddleScope {
    pub id: u64,
    pub parent: Option<u64>,
    pub mappings: HashMap<String, String>,
    pub children: HashMap<String, u64>,
    pub namespace: String,
    pub path: PathBuf,
}

pub fn get_disamubiguous_name(
    scope: &u64,
    name: Option<&str>,
    var_type: Option<&VarType>,
) -> String {
    format!(
        "{0}-{1}-{2}:{3}",
        match var_type {
            Some(VarType::Mutable) => "mut",
            Some(VarType::Immutable) => "let",
            _ => "const",
        },
        scope,
        random_range(0..100000),
        name.unwrap_or("anon")
    )
}

impl MiddleEnvironment {
    pub fn new() -> MiddleEnvironment {
        MiddleEnvironment {
            scope_counter: 0,
            scopes: HashMap::new(),
            resolved_variables: Vec::new(),
            variables: HashMap::new(),
            objects: HashMap::new(),
            current_location: None,
        }
    }

    pub fn get_location(&self, scope: &u64, span: Span) -> Option<Location> {
        Some(Location {
            path: self.scopes.get(scope).unwrap().path.clone(),
            span,
        })
    }

    pub fn resolve_str(&self, scope: &u64, iden: &str) -> Option<&String> {
        let scope = self.scopes.get(scope)?;

        if let Some(x) = scope.mappings.get(iden) {
            Some(x)
        } else if let Some(parent) = scope.parent.as_ref() {
            self.resolve_str(parent, iden)
        } else {
            None
        }
    }

    pub fn resolve_data_type(&self, scope: &u64, data_type: ParserDataType) -> ParserDataType {
        match data_type.data_type {
            ParserInnerType::Tuple(x) => {
                let mut lst = Vec::new();

                for x in x {
                    lst.push(self.resolve_data_type(scope, x));
                }

                ParserDataType {
                    data_type: ParserInnerType::Tuple(lst),
                    ..data_type
                }
            }
            ParserInnerType::Struct(x) => ParserDataType {
                data_type: ParserInnerType::Struct(if let Some(x) = x {
                    self.resolve_str(scope, &x).map(|x| x.to_string())
                } else {
                    None
                }),
                ..data_type
            },
            ParserInnerType::Ref(d_type, mutability) => ParserDataType {
                data_type: ParserInnerType::Ref(
                    Box::new(self.resolve_data_type(scope, *d_type)),
                    mutability,
                ),
                ..data_type
            },
            ParserInnerType::List(x) => ParserDataType {
                data_type: ParserInnerType::List(if let Some(x) = x {
                    Some(Box::new(self.resolve_data_type(scope, *x)))
                } else {
                    None
                }),
                ..data_type
            },
            ParserInnerType::Option(x) => ParserDataType {
                data_type: ParserInnerType::Option(Box::new(self.resolve_data_type(scope, *x))),
                ..data_type
            },
            ParserInnerType::Result(x, e) => ParserDataType {
                data_type: ParserInnerType::Result(
                    Box::new(self.resolve_data_type(scope, *x)),
                    Box::new(self.resolve_data_type(scope, *e)),
                ),
                ..data_type
            },
            ParserInnerType::Scope(x) => {
                let mut lst = Vec::new();

                for x in x {
                    lst.push(self.resolve_data_type(scope, x));
                }

                ParserDataType {
                    data_type: ParserInnerType::Scope(lst),
                    ..data_type
                }
            }
            _ => data_type,
        }
    }

    pub fn resolve_parser_text(&self, scope: &u64, iden: &ParserText) -> Option<ParserText> {
        Some(ParserText {
            text: self.resolve_str(scope, &iden.text)?.to_string(),
            span: iden.span,
        })
    }

    pub fn add_scope(&mut self, mut scope: MiddleScope) {
        scope.id = self.scope_counter;

        let name_name = get_disamubiguous_name(&scope.id, Some("__file__"), None);
        self.variables.insert(
            name_name.clone(),
            MiddleVariable {
                data_type: ParserDataType::new(ParserInnerType::Str, Span::default()),
                var_type: VarType::Constant,
                location: None,
            },
        );

        let file_name = get_disamubiguous_name(&scope.id, Some("__file__"), None);
        self.variables.insert(
            file_name.clone(),
            MiddleVariable {
                data_type: ParserDataType::new(ParserInnerType::Str, Span::default()),
                var_type: VarType::Constant,
                location: None,
            },
        );

        scope.mappings.insert("__name__".to_string(), name_name);
        scope.mappings.insert("__file__".to_string(), file_name);
        self.scopes.insert(scope.id, scope);
        self.scope_counter += 1;
    }

    pub fn get_scope_from_path(
        &self,
        path: &[String],
        mut parent: Option<u64>,
    ) -> Result<u64, MiddleErr> {
        let mut skip = 0;
        if let None = parent {
            parent = Some(
                self.scopes
                    .iter()
                    .find(|(_, v)| v.namespace == path[0])
                    .map(|x| x.0)
                    .unwrap()
                    .clone(),
            );
            skip = 1;
        }

        for name in path.iter().skip(skip) {
            if let Some(p) = parent {
                parent = Some(self.get_scope_from_parent(p, name)?);
            }
        }

        Ok(parent.unwrap())
    }

    pub fn get_scope_from_parent(&self, parent: u64, namespace: &str) -> Result<u64, MiddleErr> {
        for (_, child) in self.scopes.get(&parent).unwrap().children.iter() {
            if let Some(x) = self.scopes.get(&child) {
                if x.namespace == namespace {
                    return Ok(x.id);
                }
            }
        }
        panic!()
    }

    pub fn new_scope_from_parent_shallow(&mut self, parent: u64) -> u64 {
        let path = self.scopes.get(&parent).unwrap().path.clone();
        self.new_scope(Some(parent), path, None)
    }

    pub fn new_scope_from_parent(&mut self, parent: u64, namespace: &str) -> u64 {
        if let Ok(scope) = self.get_scope_from_parent(parent, namespace) {
            return scope;
        }

        let path = self.scopes.get(&parent).unwrap().path.clone();
        let parent_name = path.file_name().unwrap();
        let folder = path.parent().unwrap().to_path_buf();

        let extra = if parent_name == "main.cl" {
            String::new()
        } else {
            format!(
                "{}/",
                parent_name.to_str().unwrap().split(".").nth(0).unwrap()
            )
        };

        let mut path1 = folder.clone();
        path1 = path1.join(format!("{extra}{namespace}.cl"));

        let mut path2 = folder.clone();
        path2 = path2.join(format!("{extra}{namespace}/main.cl"));

        if path1.exists() {
            self.new_scope(Some(parent), path1, Some(namespace))
        } else if path2.exists() {
            self.new_scope(Some(parent), path2, Some(namespace))
        } else {
            panic!("Tried:\n{path1:?}\n{path2:?}")
        }
    }

    pub fn new_scope(
        &mut self,
        parent: Option<u64>,
        path: PathBuf,
        namespace: Option<&str>,
    ) -> u64 {
        if let Some(parent) = parent {
            let scope = MiddleScope {
                id: self.scope_counter,
                namespace: namespace
                    .unwrap_or(&self.scope_counter.to_string())
                    .to_string(),
                parent: Some(parent),
                children: HashMap::new(),
                mappings: HashMap::new(),
                path,
            };
            let _ = self.add_scope(scope);

            self.scope_counter - 1
        } else {
            todo!()
        }
    }

    pub fn get_scope_list(&self, scope: u64, mut list: Vec<String>) -> Result<u64, MiddleErr> {
        if list.len() <= 0 {
            return Ok(scope);
        }
        let first = list.remove(0);
        let scope = self.get_next_scope(scope, first.as_str())?;
        self.get_scope_list(scope, list)
    }

    pub fn import_scope_list(
        &mut self,
        scope: u64,
        mut list: Vec<String>,
    ) -> Result<(u64, Option<MiddleNode>), MiddleErr> {
        if list.len() <= 0 {
            return Ok((scope, None));
        }
        let first = list.remove(0);
        let scope = self.import_next_scope(scope, first.as_str())?;
        self.import_scope_list(scope.0, list)
    }

    pub fn import_next_scope(
        &mut self,
        scope: u64,
        key: &str,
    ) -> Result<(u64, Option<MiddleNode>), MiddleErr> {
        Ok(match key {
            "super" => (
                self.scopes.get(&scope).unwrap().parent.clone().unwrap(),
                None,
            ),
            _ => {
                let current = self.scopes.get(&scope).unwrap().clone();
                if let Some(x) = current.children.get(key) {
                    (x.clone(), None)
                } else {
                    if let Some(s) = self.get_global_scope().children.get(key) {
                        (s.clone(), None)
                    } else {
                        let scope = self.new_scope_from_parent(current.id, key);
                        let mut parser = Parser::default();

                        let mut tokenizer = Tokenizer::default();
                        let program = parser.produce_ast(
                            tokenizer
                                .tokenize(
                                    fs::read_to_string(
                                        self.scopes.get(&scope).unwrap().path.clone(),
                                    )
                                    .unwrap(),
                                )
                                .unwrap(),
                        );

                        if !parser.errors.is_empty() {
                            return Err(parser.errors.into());
                        }

                        (scope, Some(self.evaluate(&scope, program)?))
                    }
                }
            }
        })
    }

    pub fn get_next_scope(&self, scope: u64, key: &str) -> Result<u64, MiddleErr> {
        Ok(match key {
            "super" => self.scopes.get(&scope).unwrap().parent.clone().unwrap(),
            _ => {
                let current = self.scopes.get(&scope).unwrap();
                if let Some(x) = current.children.get(key) {
                    x.clone()
                } else {
                    if let Some(s) = self.get_global_scope().children.get(key) {
                        s.clone()
                    } else {
                        return Err(MiddleErr::Scope(key.to_string()));
                    }
                }
            }
        })
    }

    pub fn get_global_scope<'a>(&'a self) -> &'a MiddleScope {
        self.scopes.get(&0).unwrap()
    }

    pub fn get_root_scope<'a>(&'a self) -> &'a MiddleScope {
        for i in 1..self.scope_counter {
            if let Some(scope) = self.scopes.get(&i) {
                if scope.namespace == "root" {
                    return scope;
                }
            }
        }

        todo!()
    }

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
            namespace: namespace.unwrap_or(&counter.to_string()).to_string(),
            parent,
            children: HashMap::new(),
            path: path.clone(),
            mappings: HashMap::new(),
        });

        let std = self.new_scope(Some(scope), path.clone(), Some("std"));

        let root = self.new_scope(Some(scope), path, Some("root"));

        root
    }

    pub fn resolve_type_from_node(&self, scope: &u64, node: &Node) -> Option<ParserDataType> {
        let typ = match &node.node_type {
            NodeType::Break
            | NodeType::Continue
            | NodeType::EmptyLine
            | NodeType::VariableDeclaration { .. }
            | NodeType::ImplDeclaration { .. }
            | NodeType::TypeDeclaration { .. }
            | NodeType::Return { .. }
            | NodeType::ImportStatement { .. }
            | NodeType::AssignmentExpression { .. }
            | NodeType::LoopDeclaration { .. } => None,
            NodeType::StructLiteral(_) => Some(ParserDataType {
                data_type: ParserInnerType::Struct(None),
                span: node.span,
            }),
            NodeType::RefStatement { mutability, value } => Some(ParserDataType {
                data_type: ParserInnerType::Ref(
                    Box::new(self.resolve_type_from_node(scope, value)?),
                    mutability.clone(),
                ),
                span: node.span,
            }),
            NodeType::ParenExpression { value } => self.resolve_type_from_node(scope, value),
            NodeType::ScopeDeclaration { body, is_temp: _ } => {
                self.resolve_type_from_node(scope, body.last()?)
            }
            NodeType::IfStatement {
                comparison: _,
                then,
                otherwise,
                special_delim: _,
            } => {
                if let Some(otherwise) = otherwise {
                    let otherwise =
                        if let NodeType::IfStatement { then: then2, .. } = &otherwise.node_type {
                            then2.clone()
                        } else {
                            otherwise.clone()
                        };

                    let ty = self.resolve_type_from_node(scope, then);
                    if ty == self.resolve_type_from_node(scope, &otherwise) {
                        ty
                    } else {
                        None
                    }
                } else {
                    self.resolve_type_from_node(scope, then)
                }
            }
            NodeType::EnumExpression {
                identifier,
                value: _,
                data: _,
            } => Some(ParserDataType {
                data_type: ParserInnerType::Struct(Some(identifier.text.clone())),
                span: identifier.span,
            }),
            NodeType::FunctionDeclaration {
                parameters,
                body,
                return_type,
                is_async,
            } => Some(ParserDataType {
                data_type: ParserInnerType::Function {
                    return_type: if let Some(x) = return_type {
                        Some(Box::new(x.clone()))
                    } else {
                        self.resolve_type_from_node(scope, body)
                            .map(|x| Box::new(x.clone()))
                    },
                    parameters: parameters.iter().map(|x| x.1.clone()).collect(),
                    is_async: *is_async,
                },
                span: node.span,
            }),
            NodeType::MatchDeclaration {
                parameters,
                body: _,
                return_type,
                is_async,
            } => Some(ParserDataType {
                data_type: ParserInnerType::Function {
                    return_type: if let Some(x) = return_type {
                        Some(Box::new(x.clone()))
                    } else {
                        // TODO get type
                        None
                    },
                    parameters: vec![parameters.1.clone()],
                    is_async: *is_async,
                },
                span: node.span,
            }),
            NodeType::BooleanExpression { .. }
            | NodeType::ComparisonExpression { .. }
            | NodeType::NotExpression { .. }
            | NodeType::InDeclaration { .. }
            | NodeType::IsDeclaration { .. } => Some(ParserDataType {
                data_type: ParserInnerType::Bool,
                span: node.span,
            }),
            NodeType::BinaryExpression { left, .. } => self.resolve_type_from_node(scope, left),
            NodeType::ListLiteral(x) => Some(ParserDataType {
                data_type: ParserInnerType::List(
                    self.resolve_type_from_node(scope, x.first()?).map(Box::new),
                ),
                span: node.span,
            }),
            NodeType::IterExpression {
                map,
                loop_type: _,
                conditionals: _,
            } => Some(ParserDataType {
                data_type: ParserInnerType::List(
                    self.resolve_type_from_node(scope, map).map(Box::new),
                ),
                span: node.span,
            }),
            NodeType::NegExpression { value } | NodeType::DebugExpression { value } => {
                self.resolve_type_from_node(scope, value)
            }
            NodeType::AsExpression { value: _, typ } => Some(typ.clone()),
            NodeType::RangeDeclaration { .. } => Some(ParserDataType {
                data_type: ParserInnerType::Range,
                span: node.span,
            }),
            NodeType::IntLiteral(_) => Some(ParserDataType {
                data_type: ParserInnerType::Int,
                span: node.span,
            }),
            NodeType::CharLiteral(_) => Some(ParserDataType {
                data_type: ParserInnerType::Char,
                span: node.span,
            }),
            NodeType::StringLiteral(_) => Some(ParserDataType {
                data_type: ParserInnerType::Str,
                span: node.span,
            }),
            NodeType::FloatLiteral(_) => Some(ParserDataType {
                data_type: ParserInnerType::Float,
                span: node.span,
            }),
            NodeType::Try { value } => match self.resolve_type_from_node(scope, value) {
                Some(ParserDataType {
                    data_type: ParserInnerType::Result(x, _),
                    ..
                })
                | Some(ParserDataType {
                    data_type: ParserInnerType::Option(x),
                    ..
                }) => Some(*x),
                x => x,
            },
            NodeType::CallExpression(caller, args) => {
                let mut caller_type = None;
                if let NodeType::Identifier(caller) = &caller.node_type {
                    if &caller.text == "tuple" {
                        let mut lst = Vec::new();

                        for arg in args {
                            lst.push(self.resolve_type_from_node(scope, &arg.0).unwrap());
                        }
                        return Some(ParserDataType {
                            data_type: ParserInnerType::Tuple(lst),
                            span: node.span,
                        });
                    }

                    if let Some(caller) = self.resolve_str(scope, caller) {
                        if self.objects.contains_key(caller) {
                            return Some(ParserDataType {
                                data_type: ParserInnerType::Struct(Some(caller.to_string())),
                                span: node.span,
                            });
                        } else if let Some(caller) = self.variables.get(caller) {
                            caller_type = Some(caller.data_type.clone());
                        }
                    }
                }

                let Some(caller_type) = caller_type else {
                    todo!()
                };

                match caller_type.data_type {
                    ParserInnerType::Function {
                        return_type,
                        parameters: _,
                        is_async: _,
                    } => return_type.clone().map(|x| *x),
                    _ => todo!(),
                }
            }
            x => todo!("{:?}", x),
        };

        if let Some(typ) = typ {
            Some(self.resolve_data_type(scope, typ))
        } else {
            None
        }
    }
}
