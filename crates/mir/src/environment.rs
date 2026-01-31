use std::{collections::HashMap, fs, path::PathBuf, str::FromStr};

use calibre_mir_ty::{MiddleNode, MiddleNodeType};
use calibre_parser::{
    Parser,
    ast::{
        Node, NodeType, ObjectMap, ParserDataType, ParserInnerType, ParserText,
        PotentialDollarIdentifier, PotentialGenericTypeIdentifier, PotentialNewType, TypeDefType,
        VarType,
        binary::BinaryOperator,
        comparison::{BooleanOperator, ComparisonOperator},
    },
    lexer::{Location, Span, Tokenizer},
};
use rand::random_range;

use crate::errors::MiddleErr;

#[derive(Debug, Clone, PartialEq)]
pub enum MiddleTypeDefType {
    Enum(Vec<(ParserText, Option<ParserDataType>)>),
    Struct(ObjectMap<ParserDataType>),
    NewType(ParserDataType),
}

#[derive(Debug, Clone, PartialEq)]
pub struct MiddleObject {
    pub object_type: MiddleTypeDefType,
    pub variables: HashMap<String, (String, bool)>,
    pub traits: Vec<String>,
    pub location: Option<Location>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct MiddleVariable {
    pub data_type: ParserDataType,
    pub var_type: VarType,
    pub location: Option<Location>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct MiddleOverload {
    pub operator: Operator,
    pub parameters: Vec<ParserDataType>,
    pub return_type: ParserDataType,
    pub func: Node,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Operator {
    Binary(BinaryOperator),
    Comparison(ComparisonOperator),
    Boolean(BooleanOperator),
}

impl FromStr for Operator {
    type Err = MiddleErr;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if let Some(x) = BinaryOperator::from_symbol(s) {
            Ok(Self::Binary(x))
        } else if let Some(x) = ComparisonOperator::from_operator(s) {
            Ok(Self::Comparison(x))
        } else if let Some(x) = BooleanOperator::from_operator(s) {
            Ok(Self::Boolean(x))
        } else {
            panic!()
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct MiddleEnvironment {
    pub scope_counter: u64,
    pub scopes: HashMap<u64, MiddleScope>,
    pub variables: HashMap<String, MiddleVariable>,
    pub resolved_variables: Vec<String>,
    pub overloads: Vec<MiddleOverload>,
    pub func_defers: Vec<Node>,
    pub objects: HashMap<String, MiddleObject>,
    pub current_location: Option<Location>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ScopeMacro {
    pub name: String,
    pub args: Vec<(PotentialDollarIdentifier, Node)>,
    pub body: Vec<Node>,
    pub create_new_scope: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub struct MiddleScope {
    pub id: u64,
    pub parent: Option<u64>,
    pub mappings: HashMap<String, String>,
    pub macros: HashMap<String, ScopeMacro>,
    pub macro_args: HashMap<String, Node>,
    pub children: HashMap<String, u64>,
    pub namespace: String,
    pub path: PathBuf,
    pub defined: Vec<String>,
    pub defers: Vec<Node>,
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
            func_defers: Vec::new(),
            overloads: Vec::new(),
            scope_counter: 0,
            scopes: HashMap::new(),
            resolved_variables: Vec::new(),
            variables: HashMap::new(),
            objects: HashMap::new(),
            current_location: None,
        }
    }

    pub fn type_def_type_into(&mut self, scope: &u64, value: TypeDefType) -> MiddleTypeDefType {
        match value {
            TypeDefType::Enum(x) => MiddleTypeDefType::Enum({
                let mut lst = Vec::new();

                for (k, v) in x {
                    lst.push((
                        self.resolve_dollar_ident_only(scope, &k).unwrap(),
                        if let Some(v) = v {
                            Some(self.resolve_potential_new_type(scope, v))
                        } else {
                            None
                        },
                    ));
                }
                lst
            }),
            TypeDefType::Struct(x) => MiddleTypeDefType::Struct({
                let data: ObjectMap<PotentialNewType> = x.into();
                let mut map = Vec::new();

                for (k, v) in data.0 {
                    map.push((k, self.resolve_potential_new_type(scope, v)));
                }

                ObjectMap(map)
            }),
            TypeDefType::NewType(x) => {
                MiddleTypeDefType::NewType(self.resolve_potential_new_type(scope, *x))
            }
        }
    }

    pub fn new_and_evaluate(
        node: Node,
        path: PathBuf,
    ) -> Result<(Self, u64, MiddleNode), MiddleErr> {
        let mut env = Self::new();
        let scope = env.new_scope_with_stdlib(None, path, None);
        let res = env.evaluate(&scope, node);
        res.map(|x| (env, scope, x))
    }

    pub fn get_location(&self, scope: &u64, span: Span) -> Option<Location> {
        Some(Location {
            path: self.scopes.get(scope).unwrap().path.clone(),
            span,
        })
    }

    pub fn resolve_str(&self, scope: &u64, iden: &str) -> Option<String> {
        if self.variables.contains_key(iden) || self.objects.contains_key(iden) {
            return Some(iden.to_string());
        }

        let scope = self.scopes.get(scope)?;

        if let Some(x) = scope.mappings.get(iden) {
            Some(x.to_string())
        } else if let Some(parent) = scope.parent.as_ref() {
            self.resolve_str(parent, iden)
        } else if iden.contains("-") {
            Some(iden.to_string())
        } else {
            None
        }
    }

    pub fn resolve_macro_arg(&self, scope: &u64, iden: &str) -> Option<&Node> {
        let scope = self.scopes.get(scope)?;

        if let Some(x) = scope.macro_args.get(iden) {
            Some(x)
        } else if let Some(parent) = scope.parent.as_ref() {
            self.resolve_macro_arg(parent, iden)
        } else {
            None
        }
    }

    pub fn resolve_macro(&self, scope: &u64, iden: &str) -> Option<&ScopeMacro> {
        let scope = self.scopes.get(scope)?;

        if let Some(x) = scope.macros.get(iden) {
            Some(x)
        } else if let Some(parent) = scope.parent.as_ref() {
            self.resolve_macro(parent, iden)
        } else {
            None
        }
    }

    pub fn resolve_parser_text(&self, scope: &u64, iden: &ParserText) -> Option<ParserText> {
        Some(ParserText {
            text: self.resolve_str(scope, &iden.text)?.to_string(),
            span: iden.span,
        })
    }

    pub fn resolve_potential_generic_ident(
        &self,
        scope: &u64,
        iden: &PotentialGenericTypeIdentifier,
    ) -> Option<ParserText> {
        match iden {
            PotentialGenericTypeIdentifier::Identifier(x) => {
                self.resolve_potential_dollar_ident(scope, &x)
            }
            PotentialGenericTypeIdentifier::Generic {
                identifier,
                generic_types,
            } => todo!(),
        }
    }

    pub fn resolve_dollar_ident_potential_generic_only(
        &self,
        scope: &u64,
        iden: &PotentialGenericTypeIdentifier,
    ) -> Option<ParserText> {
        match iden {
            PotentialGenericTypeIdentifier::Identifier(x) => {
                self.resolve_dollar_ident_only(scope, &x)
            }
            PotentialGenericTypeIdentifier::Generic {
                identifier,
                generic_types,
            } => todo!(),
        }
    }

    pub fn resolve_potential_dollar_ident(
        &self,
        scope: &u64,
        iden: &PotentialDollarIdentifier,
    ) -> Option<ParserText> {
        match iden {
            PotentialDollarIdentifier::Identifier(x) => self.resolve_parser_text(scope, x),
            PotentialDollarIdentifier::DollarIdentifier(x) => {
                if let Some(text) = self
                    .resolve_macro_arg(scope, x)
                    .map(|x| match &x.node_type {
                        NodeType::Identifier(x) => {
                            // TODO Handle generics
                            match x.get_ident() {
                                PotentialDollarIdentifier::DollarIdentifier(x) => Some(x.clone()),
                                PotentialDollarIdentifier::Identifier(x) => Some(x.clone()),
                            }
                        }
                        _ => None,
                    })
                    .flatten()
                {
                    self.resolve_parser_text(scope, &text)
                } else {
                    None
                }
            }
        }
    }

    pub fn resolve_dollar_ident_only(
        &self,
        scope: &u64,
        iden: &PotentialDollarIdentifier,
    ) -> Option<ParserText> {
        match iden {
            PotentialDollarIdentifier::Identifier(x) => Some(x.clone()),
            PotentialDollarIdentifier::DollarIdentifier(x) => self
                .resolve_macro_arg(scope, x)
                .map(|x| match &x.node_type {
                    NodeType::Identifier(x) => {
                        // TODO Handle generics
                        match x.get_ident() {
                            PotentialDollarIdentifier::DollarIdentifier(x) => Some(x.clone()),
                            PotentialDollarIdentifier::Identifier(x) => Some(x.clone()),
                        }
                    }
                    _ => None,
                })
                .flatten(),
        }
    }

    pub fn resolve_potential_new_type(
        &mut self,
        scope: &u64,
        data_type: PotentialNewType,
    ) -> ParserDataType {
        match data_type {
            PotentialNewType::DataType(x) => self.resolve_data_type(scope, x),
            PotentialNewType::NewType {
                identifier,
                type_def,
                overloads,
            } => {
                let identifier = self.resolve_dollar_ident_only(scope, &identifier).unwrap();
                let new_name = get_disamubiguous_name(scope, Some(identifier.text.trim()), None);
                let type_def = self.type_def_type_into(scope, type_def);
                self.objects.insert(
                    new_name.clone(),
                    MiddleObject {
                        object_type: type_def,
                        variables: HashMap::new(),
                        traits: Vec::new(),
                        location: self.current_location.clone(),
                    },
                );

                self.scopes
                    .get_mut(scope)
                    .unwrap()
                    .mappings
                    .insert(identifier.text, new_name.clone());

                let previous_self = self
                    .scopes
                    .get_mut(scope)
                    .unwrap()
                    .mappings
                    .insert(String::from("Self"), new_name.clone());

                for overload in overloads {
                    let overload = MiddleOverload {
                        operator: Operator::from_str(&overload.operator.text).unwrap(),
                        return_type: self
                            .resolve_potential_new_type(scope, overload.header.return_type.clone()),
                        parameters: {
                            let mut params = Vec::new();
                            let mut contains = false;

                            for param in overload.header.parameters.iter() {
                                let ty = self.resolve_potential_new_type(scope, param.1.clone());

                                if let ParserInnerType::Struct(x) =
                                    ty.data_type.clone().unwrap_all_refs()
                                {
                                    if x == new_name {
                                        contains = true;
                                    }
                                }

                                params.push(ty);
                            }

                            if !contains {
                                continue;
                            }

                            params
                        },
                        func: overload.into(),
                    };

                    self.overloads.push(overload);
                }

                if let Some(prev) = previous_self {
                    self.scopes
                        .get_mut(scope)
                        .unwrap()
                        .mappings
                        .insert(String::from("Self"), prev);
                }

                ParserDataType::from(ParserInnerType::Struct(new_name))
            }
        }
    }

    pub fn resolve_data_type(&mut self, scope: &u64, data_type: ParserDataType) -> ParserDataType {
        match data_type.data_type {
            ParserInnerType::Tuple(x) => {
                let mut lst = Vec::new();

                for x in x {
                    lst.push(self.resolve_data_type(scope, x));
                }

                ParserDataType {
                    data_type: ParserInnerType::Tuple(lst),
                    span: data_type.span,
                }
            }
            ParserInnerType::Struct(x) => ParserDataType {
                data_type: ParserInnerType::Struct(
                    self.resolve_str(scope, &x)
                        .map(|x| x.to_string())
                        .unwrap_or(x),
                ),
                span: data_type.span,
            },
            ParserInnerType::Function {
                return_type,
                parameters,
                is_async,
            } => ParserDataType {
                data_type: ParserInnerType::Function {
                    return_type: Box::new(self.resolve_data_type(scope, *return_type)),
                    parameters: {
                        let mut params = Vec::new();

                        for param in parameters {
                            params.push(self.resolve_data_type(scope, param));
                        }

                        params
                    },
                    is_async,
                },
                span: data_type.span,
            },
            ParserInnerType::Ref(d_type, mutability) => ParserDataType {
                data_type: ParserInnerType::Ref(
                    Box::new(self.resolve_data_type(scope, *d_type)),
                    mutability,
                ),
                span: data_type.span,
            },
            ParserInnerType::List(x) => ParserDataType {
                data_type: ParserInnerType::List(Box::new(self.resolve_data_type(scope, *x))),
                span: data_type.span,
            },
            ParserInnerType::Option(x) => ParserDataType {
                data_type: ParserInnerType::Option(Box::new(self.resolve_data_type(scope, *x))),
                span: data_type.span,
            },
            ParserInnerType::Result { ok, err } => ParserDataType {
                data_type: ParserInnerType::Result {
                    err: Box::new(self.resolve_data_type(scope, *err)),
                    ok: Box::new(self.resolve_data_type(scope, *ok)),
                },
                span: data_type.span,
            },
            ParserInnerType::Scope(x) => {
                let mut lst = Vec::new();

                for x in x {
                    lst.push(self.resolve_data_type(scope, x));
                }

                ParserDataType {
                    data_type: ParserInnerType::Scope(lst),
                    span: data_type.span,
                }
            }
            ParserInnerType::DollarIdentifier(x) => {
                let NodeType::DataType { data_type } =
                    self.resolve_macro_arg(scope, &x).unwrap().node_type.clone()
                else {
                    unimplemented!()
                };

                self.resolve_potential_new_type(scope, data_type.clone())
            }
            _ => data_type,
        }
    }

    pub fn add_scope(&mut self, mut scope: MiddleScope) {
        scope.id = self.scope_counter;

        let name_name = get_disamubiguous_name(&scope.id, Some("__name__"), None);
        self.variables.insert(
            name_name.clone(),
            MiddleVariable {
                data_type: ParserDataType::from(ParserInnerType::Str),
                var_type: VarType::Constant,
                location: None,
            },
        );

        let file_name = get_disamubiguous_name(&scope.id, Some("__file__"), None);
        self.variables.insert(
            file_name.clone(),
            MiddleVariable {
                data_type: ParserDataType::from(ParserInnerType::Str),
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
        Err(MiddleErr::Scope(namespace.to_string()))
    }

    pub fn new_scope_from_parent_shallow(&mut self, parent: u64) -> u64 {
        let path = self.scopes.get(&parent).unwrap().path.clone();
        self.new_scope(Some(parent), path, None)
    }

    pub fn new_build_scope_from_parent(&mut self, parent: u64, namespace: &str) -> Option<u64> {
        let path = self.scopes.get(&parent).unwrap().path.clone();
        let parent_name = path.file_name().unwrap();
        let folder = path.parent().unwrap().to_path_buf();

        let extra = if parent_name == "main.cl" || parent_name == "mod.cl" {
            String::new()
        } else {
            format!(
                "{}/",
                parent_name.to_str().unwrap().split(".").nth(0).unwrap()
            )
        };

        let mut path1 = folder.clone();
        path1 = path1.join(format!("{extra}{namespace}/build.cl"));

        if path1.exists() {
            Some(self.new_scope(Some(parent), path1, Some(namespace)))
        } else {
            None
        }
    }

    pub fn new_scope_from_parent(&mut self, parent: u64, namespace: &str) -> u64 {
        if let Ok(scope) = self.get_scope_from_parent(parent, namespace) {
            return scope;
        }

        let path = self.scopes.get(&parent).unwrap().path.clone();
        let parent_name = path.file_name().unwrap();
        let folder = path.parent().unwrap().to_path_buf();

        let extra = if parent_name == "main.cl" || parent_name == "mod.cl" {
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

        let mut path3 = folder.clone();
        path3 = path3.join(format!("{extra}{namespace}/mod.cl"));

        if path1.exists() {
            self.new_scope(Some(parent), path1, Some(namespace))
        } else if path2.exists() {
            self.new_scope(Some(parent), path2, Some(namespace))
        } else if path3.exists() {
            self.new_scope(Some(parent), path3, Some(namespace))
        } else {
            panic!("Tried:\n{path1:?}\n{path2:?}\n{path3:?}")
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
                macros: HashMap::new(),
                macro_args: HashMap::new(),
                id: self.scope_counter,
                namespace: namespace
                    .unwrap_or(&self.scope_counter.to_string())
                    .to_string(),
                parent: Some(parent),
                children: HashMap::new(),
                mappings: HashMap::new(),
                defined: Vec::new(),
                defers: Vec::new(),
                path,
            };
            let _ = self.add_scope(scope);

            self.scopes.get_mut(&parent).unwrap().children.insert(
                namespace
                    .map(String::from)
                    .unwrap_or((self.scope_counter - 1).to_string()),
                self.scope_counter - 1,
            );

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
        let first = list.remove(0);
        let scope = self.import_next_scope(scope, first.as_str());
        if list.is_empty() {
            scope
        } else {
            self.import_scope_list(scope?.0, list)
        }
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
                        let mut parser = Parser::default();
                        let mut tokenizer = Tokenizer::default();

                        let build_node = if let Some(scope) =
                            self.new_build_scope_from_parent(current.id, key)
                        {
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

                            let program = match program.node_type {
                                NodeType::ScopeDeclaration { body, .. } => Node {
                                    node_type: NodeType::ScopeDeclaration {
                                        body,
                                        named: None,
                                        is_temp: false,
                                        create_new_scope: Some(false),
                                        define: false,
                                    },
                                    ..program
                                },
                                _ => program,
                            };
                            Some(self.evaluate(&scope, program)?)
                        } else {
                            None
                        };

                        let scope = self.new_scope_from_parent(current.id, key);
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

                        let node = self.evaluate(&scope, program)?;

                        let node = match (node.node_type.clone(), build_node) {
                            (
                                MiddleNodeType::ScopeDeclaration { mut body, .. },
                                Some(build_node),
                            ) => MiddleNode {
                                node_type: MiddleNodeType::ScopeDeclaration {
                                    body: {
                                        body.insert(0, build_node);
                                        body
                                    },
                                    create_new_scope: true,
                                    is_temp: false,
                                },
                                ..node
                            },
                            (_, Some(build_node)) => {
                                MiddleNode::new_from_type(MiddleNodeType::ScopeDeclaration {
                                    body: vec![node, build_node],
                                    create_new_scope: false,
                                    is_temp: false,
                                })
                            }
                            _ => node,
                        };

                        (scope, Some(node))
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

    pub fn quick_resolve_potential_scope_member(
        &mut self,
        scope: &u64,
        node: Node,
    ) -> Result<Node, MiddleErr> {
        Ok(Node {
            node_type: match node.node_type {
                NodeType::ScopeMemberExpression { path } => {
                    return Ok(self.evaluate_scope_member_expression(scope, path)?.into());
                }
                _ => node.node_type,
            },
            span: node.span,
        })
    }

    pub fn resolve_type_from_node(&mut self, scope: &u64, node: &Node) -> Option<ParserDataType> {
        let typ = match &node.node_type {
            NodeType::Break
            | NodeType::Continue
            | NodeType::VariableDeclaration { .. }
            | NodeType::ImplDeclaration { .. }
            | NodeType::TypeDeclaration { .. }
            | NodeType::Return { .. }
            | NodeType::ImportStatement { .. }
            | NodeType::AssignmentExpression { .. }
            | NodeType::LoopDeclaration { .. }
            | NodeType::ScopeDeclaration { define: true, .. }
            | NodeType::ScopeAlias { .. }
            | NodeType::DataType { .. }
            | NodeType::Until { .. } => None,
            NodeType::Null | NodeType::Defer { .. } | NodeType::Drop(_) | NodeType::EmptyLine => {
                Some(ParserDataType::from(ParserInnerType::Null))
            }
            NodeType::Move(x) => {
                let ident = self.resolve_potential_dollar_ident(scope, &x)?.text;

                Some(self.variables.get(&ident)?.data_type.clone())
            }
            NodeType::RefStatement { mutability, value } => Some(ParserDataType {
                data_type: ParserInnerType::Ref(
                    Box::new(self.resolve_type_from_node(scope, value)?),
                    mutability.clone(),
                ),
                span: node.span,
            }),
            NodeType::ParenExpression { value } => self.resolve_type_from_node(scope, value),
            NodeType::ScopeDeclaration {
                body: Some(body), ..
            } => self.resolve_type_from_node(scope, body.last()?),
            NodeType::ScopeDeclaration {
                named: Some(named), ..
            } => {
                let name = self.resolve_dollar_ident_only(scope, &named.name)?;
                let resolved = self.resolve_macro(scope, &name)?.body.last()?.clone();
                self.resolve_type_from_node(scope, &resolved)
            }
            NodeType::IfStatement {
                comparison: _,
                then,
                otherwise,
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
            NodeType::MatchStatement { value: _, body } => {
                let mut return_type: Option<ParserDataType> = None;

                for arm in body.iter() {
                    if let Some(arm_ty) = self.resolve_type_from_node(scope, &arm.2) {
                        if let Some(ty) = return_type {
                            return_type = if ty.data_type == arm_ty.data_type
                                || arm_ty.data_type == ParserInnerType::Null
                            {
                                Some(ty)
                            } else {
                                None
                            };

                            if return_type.is_none() {
                                break;
                            }
                        } else {
                            return_type = Some(arm_ty.clone());
                        }
                    }
                }

                return_type
            }
            NodeType::EnumExpression { identifier, .. }
            | NodeType::StructLiteral { identifier, .. } => Some(ParserDataType {
                span: *identifier.span(),
                data_type: ParserInnerType::Struct(
                    self.resolve_potential_generic_ident(scope, &identifier)?
                        .to_string(),
                ),
            }),
            NodeType::FunctionDeclaration { header, .. }
            | NodeType::FnMatchDeclaration { header, .. } => Some(ParserDataType {
                data_type: ParserInnerType::Function {
                    return_type: Box::new(
                        self.resolve_potential_new_type(scope, header.return_type.clone()),
                    ),
                    parameters: {
                        let mut params = Vec::new();

                        for param in header.parameters.clone() {
                            params.push(self.resolve_potential_new_type(scope, param.1.clone()));
                        }

                        params
                    },
                    is_async: header.is_async,
                },
                span: node.span,
            }),

            NodeType::NotExpression { .. } | NodeType::InDeclaration { .. } => {
                Some(ParserDataType {
                    data_type: ParserInnerType::Bool,
                    span: node.span,
                })
            }
            NodeType::ComparisonExpression {
                left,
                right,
                operator,
            } => {
                if let Some(x) = self.get_operator_overload(
                    scope,
                    left,
                    right,
                    &Operator::Comparison(operator.clone()),
                ) {
                    Some(x.return_type.clone())
                } else {
                    Some(ParserDataType {
                        data_type: ParserInnerType::Bool,
                        span: node.span,
                    })
                }
            }
            NodeType::BooleanExpression {
                left,
                right,
                operator,
            } => {
                if let Some(x) = self.get_operator_overload(
                    scope,
                    left,
                    right,
                    &Operator::Boolean(operator.clone()),
                ) {
                    Some(x.return_type.clone())
                } else {
                    Some(ParserDataType {
                        data_type: ParserInnerType::Bool,
                        span: node.span,
                    })
                }
            }
            NodeType::BinaryExpression {
                left,
                right,
                operator,
            } => {
                if let Some(x) = self.get_operator_overload(
                    scope,
                    left,
                    right,
                    &Operator::Binary(operator.clone()),
                ) {
                    Some(x.return_type.clone())
                } else {
                    self.resolve_type_from_node(scope, left)
                }
            }
            NodeType::IterExpression { data_type, .. } | NodeType::ListLiteral(data_type, _) => {
                Some(ParserDataType {
                    data_type: ParserInnerType::List(Box::new(
                        self.resolve_potential_new_type(scope, data_type.clone()),
                    )),
                    span: node.span,
                })
            }
            NodeType::NegExpression { value }
            | NodeType::DebugExpression { value }
            | NodeType::Ternary { then: value, .. } => self.resolve_type_from_node(scope, value),
            NodeType::AsExpression {
                value: _,
                data_type,
            } => Some(self.resolve_potential_new_type(scope, data_type.clone())),
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
            NodeType::Try { value, .. } => match self.resolve_type_from_node(scope, value) {
                Some(ParserDataType {
                    data_type: ParserInnerType::Result { ok: x, err: _ },
                    ..
                })
                | Some(ParserDataType {
                    data_type: ParserInnerType::Option(x),
                    ..
                }) => Some(*x),
                x => x,
            },
            NodeType::CallExpression {
                caller,
                generic_types,
                args,
                ..
            } => {
                // TODO handle generics
                let caller = self
                    .quick_resolve_potential_scope_member(scope, *caller.clone())
                    .unwrap();
                let mut caller_type = None;
                if let NodeType::Identifier(caller) = &caller.node_type {
                    if &caller.to_string() == "tuple" {
                        let mut lst = Vec::new();

                        for arg in args {
                            lst.push(
                                self.resolve_type_from_node(scope, &arg.clone().into())
                                    .unwrap(),
                            );
                        }
                        return Some(ParserDataType {
                            data_type: ParserInnerType::Tuple(lst),
                            span: node.span,
                        });
                    }

                    if let Some(caller) = self.resolve_potential_generic_ident(scope, caller) {
                        if self.objects.contains_key(&caller.text) {
                            return Some(ParserDataType {
                                data_type: ParserInnerType::Struct(caller.to_string()),
                                span: node.span,
                            });
                        } else if let Some(caller) = self.variables.get(&caller.text) {
                            caller_type = Some(caller.data_type.clone());
                        }
                    }
                }

                let caller_type = caller_type?;

                match caller_type.data_type {
                    ParserInnerType::Function {
                        return_type,
                        parameters,
                        is_async,
                    } if parameters.len() > args.len() => Some(ParserDataType {
                        data_type: ParserInnerType::Function {
                            return_type,
                            parameters: parameters
                                .iter()
                                .enumerate()
                                .filter(|(i, _)| i >= &args.len())
                                .map(|x| x.1.clone())
                                .collect(),
                            is_async,
                        },
                        span: node.span,
                    }),
                    ParserInnerType::Function {
                        return_type,
                        parameters: _,
                        is_async: _,
                    } => Some(*return_type.clone()),
                    ParserInnerType::NativeFunction(x) => Some(*x),
                    _ => return None,
                }
            }
            NodeType::Identifier(x) => {
                if let Some(iden) = self.resolve_potential_generic_ident(scope, &x) {
                    if let Some(x) = self.variables.get(&iden.text) {
                        return Some(x.data_type.clone());
                    }
                }

                None
            }
            NodeType::MemberExpression { path } => {
                if path.len() == 1 {
                    return self.resolve_type_from_node(scope, &path[0].0);
                }
                // TODO resolve the type
                Some(ParserDataType::from(ParserInnerType::Dynamic))
            }
            NodeType::PipeExpression(path) => {
                self.resolve_type_from_node(scope, path.last().unwrap().get_node())
            }
            NodeType::DerefStatement { value } => {
                let typ = self.resolve_type_from_node(scope, &value)?;

                if let ParserInnerType::Ref(x, _) = typ.data_type {
                    Some(*x)
                } else {
                    Some(typ)
                }
            }
            NodeType::ScopeMemberExpression { .. } => todo!(),
            NodeType::ScopeDeclaration { .. } => unreachable!(),
        };

        if let Some(typ) = typ {
            Some(self.resolve_data_type(scope, typ))
        } else {
            None
        }
    }
}
