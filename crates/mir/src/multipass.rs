use crate::{
    environment::{MiddleEnvironment, get_disamubiguous_name},
    symbols::MiddleVariable,
};
use calibre_parser::{
    Span,
    ast::{
        ObjectType,
        idents::PotentialDollarIdentifier,
        nodes::{CallArg, Node, NodeType, TypeDefType, VarType},
        types::{ParserDataType, ParserInnerType, PotentialNewType},
    },
};
use rustc_hash::{FxHashMap, FxHashSet};

#[derive(Default)]
struct ReorderBuckets {
    imports: Vec<Node>,
    types: Vec<Node>,
    constants: Vec<Node>,
    normal: Vec<Node>,
    fin: Vec<Node>,
    init: Vec<Node>,
}

impl ReorderBuckets {
    fn append(&mut self, node: Node) {
        match classify_statement(&node) {
            StatementClass::Import => self.imports.push(node),
            StatementClass::Type => self.types.push(node),
            StatementClass::Constant => self.constants.push(node),
            StatementClass::Init => self.init.push(node),
            StatementClass::Fin => self.fin.push(node),
            StatementClass::Normal => self.normal.push(node),
        }
    }

    fn into_ordered(mut self) -> Vec<Node> {
        let mut out = Vec::new();
        out.append(&mut self.imports);

        out.extend(order_declarations_by_dependencies(
            &self.types,
            &self.constants,
        ));

        out.append(&mut self.init);
        out.append(&mut self.normal);
        out.append(&mut self.fin);
        out
    }
}

enum StatementClass {
    Import,
    Type,
    Constant,
    Init,
    Fin,
    Normal,
}

pub enum CompilationPhase {
    ImportsAndTypes,
    Normal,
    Fin,
    Init,
}

fn classify_statement(node: &Node) -> StatementClass {
    match &node.node_type {
        NodeType::ImportStatement { .. } => StatementClass::Import,
        NodeType::TypeDeclaration { .. }
        | NodeType::TraitDeclaration { .. }
        | NodeType::ImplDeclaration { .. }
        | NodeType::ImplTraitDeclaration { .. } => StatementClass::Type,
        NodeType::VariableDeclaration { var_type, .. } if *var_type == VarType::Constant => {
            StatementClass::Constant
        }
        NodeType::Tag { node, tag, .. } => match tag.text.as_str() {
            "init" => StatementClass::Init,
            "fin" => StatementClass::Fin,
            _ => classify_statement(node),
        },
        _ => StatementClass::Normal,
    }
}

pub fn compilation_phase(node: &Node) -> CompilationPhase {
    match classify_statement(node) {
        StatementClass::Import | StatementClass::Type | StatementClass::Constant => {
            CompilationPhase::ImportsAndTypes
        }
        StatementClass::Fin => CompilationPhase::Fin,
        StatementClass::Init => CompilationPhase::Init,
        StatementClass::Normal => CompilationPhase::Normal,
    }
}

fn order_declarations_by_dependencies(types: &[Node], constants: &[Node]) -> Vec<Node> {
    let mut all_declarations: Vec<Node> = types.to_vec();
    all_declarations.extend(constants.iter().cloned());

    let mut decl_names: FxHashMap<String, usize> = FxHashMap::default();
    for (i, node) in all_declarations.iter().enumerate() {
        match &node.node_type {
            NodeType::TypeDeclaration { identifier, .. } => {
                let name = identifier.to_string();
                decl_names.insert(name, i);
            }
            NodeType::VariableDeclaration {
                identifier,
                var_type,
                ..
            } if *var_type == VarType::Constant => {
                let name = identifier.to_string();
                decl_names.insert(name, i);
            }
            _ => {}
        }
    }

    let mut dependencies: Vec<FxHashSet<usize>> =
        vec![FxHashSet::default(); all_declarations.len()];
    let mut dependents: Vec<FxHashSet<usize>> = vec![FxHashSet::default(); all_declarations.len()];

    for (i, node) in all_declarations.iter().enumerate() {
        match &node.node_type {
            NodeType::TypeDeclaration { object, .. } => {
                let referenced_names = extract_referenced_names_from_type_def(object);

                for ref_name in referenced_names {
                    if let Some(&ref_idx) = decl_names.get(&ref_name)
                        && ref_idx != i
                    {
                        dependencies[i].insert(ref_idx);
                        dependents[ref_idx].insert(i);
                    }
                }
            }
            NodeType::VariableDeclaration {
                var_type,
                value,
                data_type,
                ..
            } if *var_type == VarType::Constant => {
                let mut referenced_names = extract_referenced_names_from_node(value);
                referenced_names
                    .extend(extract_referenced_names_from_potential_new_type(data_type));

                for ref_name in referenced_names {
                    if let Some(&ref_idx) = decl_names.get(&ref_name)
                        && ref_idx != i
                    {
                        dependencies[i].insert(ref_idx);
                        dependents[ref_idx].insert(i);
                    }
                }
            }
            _ => {}
        }
    }

    let mut ordered = Vec::new();
    let mut placed = FxHashSet::default();
    let mut to_process: Vec<usize> = (0..all_declarations.len())
        .filter(|i| dependencies[*i].is_empty())
        .collect();

    while !to_process.is_empty() {
        let current = to_process.remove(0);
        if placed.contains(&current) {
            continue;
        }
        placed.insert(current);
        ordered.push(all_declarations[current].clone());

        for &dep in &dependents[current] {
            if !placed.contains(&dep) {
                let remaining_deps: FxHashSet<_> = dependencies[dep]
                    .iter()
                    .filter(|d| !placed.contains(d))
                    .cloned()
                    .collect();
                if remaining_deps.is_empty() {
                    to_process.push(dep);
                }
            }
        }
    }

    for (i, x) in all_declarations.clone().into_iter().enumerate() {
        if !placed.contains(&i) {
            ordered.push(x);
        }
    }

    ordered
}

fn extract_referenced_names_from_type_def(type_def: &TypeDefType) -> Vec<String> {
    let mut names = Vec::new();
    match type_def {
        TypeDefType::Enum { variants, .. } => {
            for (_, potential_type) in variants {
                if let Some(potential) = potential_type {
                    names.extend(extract_referenced_names_from_potential_new_type(potential));
                }
            }
        }
        TypeDefType::Struct { fields } => {
            if let ObjectType::Map(field_map) = fields {
                for (_, (potential_type, default_value)) in field_map {
                    names.extend(extract_referenced_names_from_potential_new_type(
                        potential_type,
                    ));
                    if let Some(default) = default_value {
                        names.extend(extract_referenced_names_from_node(default));
                    }
                }
            }
        }
        TypeDefType::NewType(inner) => {
            names.extend(extract_referenced_names_from_potential_new_type(inner));
        }
    }
    names
}

fn extract_referenced_names_from_potential_new_type(potential: &PotentialNewType) -> Vec<String> {
    let mut names = Vec::new();
    match potential {
        PotentialNewType::NewType { type_def, .. } => {
            names.extend(extract_referenced_names_from_type_def(type_def));
        }
        PotentialNewType::DataType(data_type) => {
            names.extend(extract_referenced_types_from_parser_data_type(data_type));
        }
    }
    names
}

fn extract_referenced_types_from_parser_data_type(data_type: &ParserDataType) -> Vec<String> {
    let mut types = Vec::new();
    match &data_type.data_type {
        ParserInnerType::Struct(name) => {
            types.push(name.clone());
        }
        ParserInnerType::StructWithGenerics { identifier, .. } => {
            types.push(identifier.clone());
        }
        ParserInnerType::Function {
            return_type,
            parameters,
        } => {
            types.extend(extract_referenced_types_from_parser_data_type(return_type));
            for param in parameters {
                types.extend(extract_referenced_types_from_parser_data_type(param));
            }
        }
        ParserInnerType::Option(inner) => {
            types.extend(extract_referenced_types_from_parser_data_type(inner));
        }
        ParserInnerType::Result { ok, err } => {
            types.extend(extract_referenced_types_from_parser_data_type(ok));
            types.extend(extract_referenced_types_from_parser_data_type(err));
        }
        _ => {}
    }
    types
}

fn extract_referenced_names_from_node(node: &Node) -> Vec<String> {
    let mut names = Vec::new();
    match &node.node_type {
        NodeType::Identifier(text) => {
            names.push(text.to_string());
        }
        NodeType::MemberExpression { path, .. } => {
            if let Some((first, _)) = path.first() {
                names.extend(extract_referenced_names_from_node(first));
            }
        }
        NodeType::CallExpression { args, .. } => {
            for arg in args {
                match arg {
                    CallArg::Value(node) => {
                        names.extend(extract_referenced_names_from_node(node));
                    }
                    CallArg::Named(_, node) => {
                        names.extend(extract_referenced_names_from_node(node));
                    }
                }
            }
        }
        NodeType::ScopeDeclaration {
            body: Some(body), ..
        } => {
            for stmt in body {
                names.extend(extract_referenced_names_from_node(stmt));
            }
        }
        _ => {}
    }
    names
}

fn reorder_scope_body(body: Vec<Node>) -> Vec<Node> {
    let mut buckets = ReorderBuckets::default();

    for stmt in body {
        buckets.append(reorder_node(stmt));
    }

    buckets.into_ordered()
}

fn reorder_node(node: Node) -> Node {
    match node.node_type {
        NodeType::ScopeDeclaration {
            body,
            named,
            is_temp,
            create_new_scope,
            define,
        } => {
            let body = body.map(reorder_scope_body);
            Node::new(
                node.span,
                NodeType::ScopeDeclaration {
                    body,
                    named,
                    is_temp,
                    create_new_scope,
                    define,
                },
            )
        }
        other => Node::new(node.span, other),
    }
}

pub fn prepare_ast(node: Node) -> Node {
    match node.node_type {
        NodeType::ScopeDeclaration {
            body,
            named,
            is_temp,
            create_new_scope,
            define,
        } => {
            let body = body.map(reorder_scope_body);
            Node::new(
                node.span,
                NodeType::ScopeDeclaration {
                    body,
                    named,
                    is_temp,
                    create_new_scope,
                    define,
                },
            )
        }
        other => Node::new(node.span, other),
    }
}

impl MiddleEnvironment {
    pub fn predeclare_forward_refs(&mut self, scope: &u64, nodes: &[Node]) {
        for node in nodes {
            self.predeclare_forward_ref_node(scope, node);
        }
    }

    fn predeclare_forward_ref_node(&mut self, scope: &u64, node: &Node) {
        match &node.node_type {
            NodeType::Tag { node: inner, .. } => self.predeclare_forward_ref_node(scope, inner),
            NodeType::ScopeDeclaration {
                body: Some(body), ..
            } => {
                self.predeclare_forward_refs(scope, body);
            }
            NodeType::TypeDeclaration { identifier, .. } => {
                self.predeclare_type_binding(scope, &identifier.to_string());
            }
            NodeType::VariableDeclaration {
                var_type,
                identifier,
                value,
                data_type,
            } if *var_type == VarType::Constant => {
                self.predeclare_constant_binding(
                    scope, node.span, *var_type, identifier, value, data_type,
                );
            }
            NodeType::ExternFunctionDeclaration { identifier, .. } => {
                self.predeclare_extern_function_binding(scope, identifier);
            }
            _ => {}
        }
    }

    fn predeclare_type_binding(&mut self, scope: &u64, identifier: &str) {
        let new_name = get_disamubiguous_name(scope, Some(identifier.trim()), None);

        if self.symbols.variables.contains_key(&new_name) {
            return;
        }

        self.symbols.variables.insert(
            new_name.clone(),
            MiddleVariable {
                data_type: ParserDataType::new(Span::default(), ParserInnerType::Auto(None)),
                var_type: VarType::Immutable,
                location: None,
            },
        );

        if let Ok(scope_ref) = self.scoping.scope_mut_or_err(scope) {
            scope_ref
                .mappings
                .entry(identifier.to_string())
                .or_insert(new_name);
        }
    }

    fn predeclare_constant_binding(
        &mut self,
        scope: &u64,
        span: Span,
        var_type: VarType,
        identifier: &PotentialDollarIdentifier,
        value: &Node,
        data_type: &PotentialNewType,
    ) {
        let Some(identifier) = self.resolve_dollar_ident_only(scope, identifier) else {
            return;
        };

        let new_name = if identifier.text.contains("->") || identifier.text.contains("::") {
            identifier.text.clone()
        } else {
            get_disamubiguous_name(scope, Some(identifier.text.trim()), Some(&var_type))
        };

        if self.symbols.variables.contains_key(&new_name) {
            return;
        }

        let const_type = if data_type.is_auto() {
            self.resolve_type_from_node(scope, value)
                .unwrap_or_else(|| ParserDataType::new(span, ParserInnerType::Auto(None)))
        } else {
            self.resolve_potential_new_type(scope, data_type.clone())
        };

        self.symbols.variables.insert(
            new_name.clone(),
            MiddleVariable {
                data_type: const_type,
                var_type,
                location: self.scoping.get_location(scope, span),
            },
        );

        if let Ok(scope_ref) = self.scoping.scope_mut_or_err(scope) {
            scope_ref
                .mappings
                .entry(identifier.text.clone())
                .or_insert(new_name);
        }
    }

    fn predeclare_extern_function_binding(
        &mut self,
        scope: &u64,
        identifier: &PotentialDollarIdentifier,
    ) {
        let Some(identifier) = self.resolve_dollar_ident_only(scope, identifier) else {
            return;
        };

        let new_name = if identifier.text.contains("->") || identifier.text.contains("::") {
            identifier.text.clone()
        } else {
            get_disamubiguous_name(
                scope,
                Some(identifier.text.trim()),
                Some(&VarType::Constant),
            )
        };

        if self.symbols.variables.contains_key(&new_name) {
            return;
        }
        self.symbols.variables.insert(
            new_name.clone(),
            MiddleVariable {
                data_type: ParserDataType::new(Span::default(), ParserInnerType::Auto(None)),
                var_type: VarType::Constant,
                location: None,
            },
        );

        if let Ok(scope_ref) = self.scoping.scope_mut_or_err(scope) {
            scope_ref
                .mappings
                .entry(identifier.text.clone())
                .or_insert(new_name);
        }
    }
}
