use crate::ast::hm::{self, Subst, Type, TypeGenerator};
use crate::ast::{MiddleNode, MiddleNodeType};
use calibre_parser::{
    Parser,
    ast::{
        Node, NodeType, ObjectMap, Overload, ParserDataType, ParserInnerType, ParserText,
        PotentialDollarIdentifier, PotentialGenericTypeIdentifier, PotentialNewType, TypeDefType,
        VarType,
        binary::BinaryOperator,
        comparison::{BooleanOperator, ComparisonOperator},
    },
    lexer::{Location, Span, Tokenizer},
};
use rand::random_range;
use rustc_hash::FxHashMap;
use std::{fs, path::PathBuf, str::FromStr};

use crate::errors::MiddleErr;
use crate::infer::infer_node_type;

#[derive(Debug, Clone, PartialEq)]
pub enum MiddleTypeDefType {
    Enum(Vec<(ParserText, Option<ParserDataType>)>),
    Struct(ObjectMap<ParserDataType>),
    NewType(ParserDataType),
}

#[derive(Debug, Clone, PartialEq)]
pub struct MiddleObject {
    pub object_type: MiddleTypeDefType,
    pub variables: FxHashMap<String, (String, bool)>,
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
    pub scopes: FxHashMap<u64, MiddleScope>,
    pub variables: FxHashMap<String, MiddleVariable>,
    pub resolved_variables: Vec<String>,
    pub overloads: Vec<MiddleOverload>,
    pub func_defers: Vec<Node>,
    pub objects: FxHashMap<String, MiddleObject>,
    pub hm_env: FxHashMap<String, hm::TypeScheme>,
    pub generic_fn_templates:
        FxHashMap<String, (Vec<String>, calibre_parser::ast::FunctionHeader, Node)>,
    pub generic_type_templates: FxHashMap<String, (Vec<String>, TypeDefType, Vec<Overload>)>,
    pub type_specializations: FxHashMap<String, String>,
    pub fn_specializations: FxHashMap<String, String>,
    pub specialization_decls_by_scope: FxHashMap<u64, Vec<MiddleNode>>,
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
    pub mappings: FxHashMap<String, String>,
    pub macros: FxHashMap<String, ScopeMacro>,
    pub macro_args: FxHashMap<String, Node>,
    pub children: FxHashMap<String, u64>,
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
            scopes: FxHashMap::default(),
            resolved_variables: Vec::new(),
            variables: FxHashMap::default(),
            objects: FxHashMap::default(),
            hm_env: FxHashMap::default(),
            generic_fn_templates: FxHashMap::default(),
            generic_type_templates: FxHashMap::default(),
            type_specializations: FxHashMap::default(),
            fn_specializations: FxHashMap::default(),
            specialization_decls_by_scope: FxHashMap::default(),
            current_location: None,
        }
    }

    fn canonical_type_key(dt: &ParserDataType) -> String {
        match &dt.data_type {
            ParserInnerType::Int => "int".to_string(),
            ParserInnerType::Float => "float".to_string(),
            ParserInnerType::Bool => "bool".to_string(),
            ParserInnerType::Str => "str".to_string(),
            ParserInnerType::Char => "char".to_string(),
            ParserInnerType::Null => "null".to_string(),
            ParserInnerType::Dynamic => "dyn".to_string(),
            ParserInnerType::Range => "range".to_string(),
            ParserInnerType::Struct(s) => format!("struct_{}", s),
            ParserInnerType::List(x) => format!("list_{}", Self::canonical_type_key(x)),
            ParserInnerType::Option(x) => format!("opt_{}", Self::canonical_type_key(x)),
            ParserInnerType::Result { ok, err } => format!(
                "res_{}_{}",
                Self::canonical_type_key(err),
                Self::canonical_type_key(ok)
            ),
            ParserInnerType::Tuple(xs) => {
                let inner = xs
                    .iter()
                    .map(Self::canonical_type_key)
                    .collect::<Vec<_>>()
                    .join("_");
                format!("tup_{}", inner)
            }
            ParserInnerType::Ref(x, m) => format!("ref{}_{}", m, Self::canonical_type_key(x)),
            ParserInnerType::StructWithGenerics {
                identifier,
                generic_types,
            } => {
                let inner = generic_types
                    .iter()
                    .map(Self::canonical_type_key)
                    .collect::<Vec<_>>()
                    .join("_");
                format!("gen_{}_{}", identifier, inner)
            }
            ParserInnerType::Function {
                return_type,
                parameters,
                is_async: _,
            } => {
                let params = parameters
                    .iter()
                    .map(Self::canonical_type_key)
                    .collect::<Vec<_>>()
                    .join("_");
                format!(
                    "fn_{}_ret_{}",
                    params,
                    Self::canonical_type_key(return_type)
                )
            }
            ParserInnerType::Auto(_)
            | ParserInnerType::DollarIdentifier(_)
            | ParserInnerType::Scope(_)
            | ParserInnerType::NativeFunction(_) => {
                format!("other_{}", dt)
            }
        }
    }

    pub fn canonical_type_args_key(&self, args: &[ParserDataType]) -> String {
        args.iter()
            .map(Self::canonical_type_key)
            .collect::<Vec<_>>()
            .join("__")
    }

    pub fn substitute_potential_new_type(
        &mut self,
        pnt: &PotentialNewType,
        subst: &FxHashMap<String, ParserDataType>,
    ) -> PotentialNewType {
        match pnt {
            PotentialNewType::DataType(dt) => {
                PotentialNewType::DataType(self.substitute_data_type(dt, subst))
            }
            _ => pnt.clone(),
        }
    }

    pub fn substitute_type_def(
        &mut self,
        td: &TypeDefType,
        subst: &FxHashMap<String, ParserDataType>,
    ) -> TypeDefType {
        match td {
            TypeDefType::Struct(obj) => TypeDefType::Struct(match obj {
                calibre_parser::ast::ObjectType::Map(xs) => calibre_parser::ast::ObjectType::Map(
                    xs.iter()
                        .map(|(k, v)| (k.clone(), self.substitute_potential_new_type(v, subst)))
                        .collect(),
                ),
                calibre_parser::ast::ObjectType::Tuple(xs) => {
                    calibre_parser::ast::ObjectType::Tuple(
                        xs.iter()
                            .map(|v| self.substitute_potential_new_type(v, subst))
                            .collect(),
                    )
                }
            }),
            TypeDefType::Enum(xs) => TypeDefType::Enum(
                xs.iter()
                    .map(|(k, v)| {
                        (
                            k.clone(),
                            v.as_ref()
                                .map(|p| self.substitute_potential_new_type(p, subst)),
                        )
                    })
                    .collect(),
            ),
            TypeDefType::NewType(inner) => {
                TypeDefType::NewType(Box::new(self.substitute_potential_new_type(inner, subst)))
            }
        }
    }

    pub fn ensure_specialized_type(
        &mut self,
        _scope: &u64,
        base: &str,
        template_params: &[String],
        concrete_args: &[ParserDataType],
    ) -> Option<String> {
        let decl_scope = self.get_root_scope().id;
        if template_params.len() != concrete_args.len() {
            return None;
        }

        let key = format!(
            "type::{}::{}",
            base,
            self.canonical_type_args_key(concrete_args)
        );
        if let Some(existing) = self.type_specializations.get(&key) {
            return Some(existing.clone());
        }

        let (tpl_params, obj, overloads) = self.generic_type_templates.get(base)?.clone();
        if tpl_params.len() != template_params.len() {
            return None;
        }

        let mut subst: FxHashMap<String, ParserDataType> = FxHashMap::default();
        for (p, arg) in template_params.iter().zip(concrete_args.iter()) {
            subst.insert(p.clone(), arg.clone());
        }

        let specialized_name = format!("{}->{}", base, self.canonical_type_args_key(concrete_args));
        self.type_specializations
            .insert(key, specialized_name.clone());

        let new_obj = self.substitute_type_def(&obj, &subst);

        let decl_node = Node::new(
            self.current_span(),
            NodeType::TypeDeclaration {
                identifier: ParserText::from(specialized_name.clone()).into(),
                object: new_obj,
                overloads,
            },
        );
        let _ = self.evaluate(&decl_scope, decl_node);

        Some(specialized_name)
    }

    pub fn ensure_specialized_function(
        &mut self,
        _scope: &u64,
        base: &str,
        template_params: &[String],
        concrete_args: &[ParserDataType],
    ) -> Option<String> {
        let decl_scope = self.get_root_scope().id;
        if template_params.len() != concrete_args.len() {
            return None;
        }

        let key = format!(
            "fn::{}::{}",
            base,
            self.canonical_type_args_key(concrete_args)
        );
        if let Some(existing) = self.fn_specializations.get(&key) {
            return Some(existing.clone());
        }

        let (tpl_params, header, body) = self.generic_fn_templates.get(base)?.clone();
        if tpl_params.len() != template_params.len() {
            return None;
        }

        let mut subst: FxHashMap<String, ParserDataType> = FxHashMap::default();
        for (p, arg) in template_params.iter().zip(concrete_args.iter()) {
            subst.insert(p.clone(), arg.clone());
        }

        let specialized_name = format!("{}->{}", base, self.canonical_type_args_key(concrete_args));
        self.fn_specializations
            .insert(key, specialized_name.clone());

        let mut new_header = header.clone();
        new_header.generics = calibre_parser::ast::GenericTypes::default();
        new_header.parameters = new_header
            .parameters
            .iter()
            .map(|(n, t)| (n.clone(), self.substitute_potential_new_type(t, &subst)))
            .collect();
        new_header.return_type =
            self.substitute_potential_new_type(&new_header.return_type, &subst);

        let decl_node = Node::new(
            self.current_span(),
            NodeType::VariableDeclaration {
                var_type: VarType::Constant,
                identifier: ParserText::from(specialized_name.clone()).into(),
                data_type: PotentialNewType::DataType(ParserDataType::from(ParserInnerType::Auto(
                    None,
                ))),
                value: Box::new(Node::new(
                    self.current_span(),
                    NodeType::FunctionDeclaration {
                        header: new_header,
                        body: Box::new(body.clone()),
                    },
                )),
            },
        );

        if let Ok(mn) = self.evaluate(&decl_scope, decl_node) {
            self.specialization_decls_by_scope
                .entry(decl_scope)
                .or_default()
                .push(mn);
        }

        Some(specialized_name)
    }

    pub fn substitute_data_type(
        &mut self,
        dt: &ParserDataType,
        subst: &FxHashMap<String, ParserDataType>,
    ) -> ParserDataType {
        let span = dt.span;
        let data_type = match &dt.data_type {
            ParserInnerType::Struct(s) if subst.contains_key(s) => {
                subst.get(s).unwrap().data_type.clone()
            }
            ParserInnerType::Tuple(xs) => ParserInnerType::Tuple(
                xs.iter()
                    .map(|x| self.substitute_data_type(x, subst))
                    .collect(),
            ),
            ParserInnerType::List(x) => {
                ParserInnerType::List(Box::new(self.substitute_data_type(x, subst)))
            }
            ParserInnerType::Option(x) => {
                ParserInnerType::Option(Box::new(self.substitute_data_type(x, subst)))
            }
            ParserInnerType::Result { ok, err } => ParserInnerType::Result {
                ok: Box::new(self.substitute_data_type(ok, subst)),
                err: Box::new(self.substitute_data_type(err, subst)),
            },
            ParserInnerType::Function {
                return_type,
                parameters,
                is_async,
            } => ParserInnerType::Function {
                return_type: Box::new(self.substitute_data_type(return_type, subst)),
                parameters: parameters
                    .iter()
                    .map(|p| self.substitute_data_type(p, subst))
                    .collect(),
                is_async: *is_async,
            },
            ParserInnerType::Ref(x, m) => {
                ParserInnerType::Ref(Box::new(self.substitute_data_type(x, subst)), m.clone())
            }
            ParserInnerType::StructWithGenerics {
                identifier,
                generic_types,
            } => ParserInnerType::StructWithGenerics {
                identifier: identifier.clone(),
                generic_types: generic_types
                    .iter()
                    .map(|g| self.substitute_data_type(g, subst))
                    .collect(),
            },
            _ => dt.data_type.clone(),
        };

        ParserDataType { data_type, span }
    }

    pub fn infer_generic_args_from_call(
        &mut self,
        template_params: &[String],
        param_types: &[ParserDataType],
        arg_types: &[ParserDataType],
    ) -> Option<Vec<ParserDataType>> {
        fn unify_pat(
            env: &mut MiddleEnvironment,
            template_params: &[String],
            pat: &ParserDataType,
            arg: &ParserDataType,
            out: &mut FxHashMap<String, ParserDataType>,
        ) -> bool {
            match (&pat.data_type, &arg.data_type) {
                (ParserInnerType::Struct(s), _) if template_params.contains(s) => {
                    if let Some(existing) = out.get(s) {
                        existing.data_type == arg.data_type
                    } else {
                        out.insert(s.clone(), env.resolve_data_type(&0, arg.clone()));
                        true
                    }
                }
                (ParserInnerType::List(p), ParserInnerType::List(a)) => {
                    unify_pat(env, template_params, p, a, out)
                }
                (ParserInnerType::Option(p), ParserInnerType::Option(a)) => {
                    unify_pat(env, template_params, p, a, out)
                }
                (
                    ParserInnerType::Result { ok: pk, err: pe },
                    ParserInnerType::Result { ok: ak, err: ae },
                ) => {
                    unify_pat(env, template_params, pe, ae, out)
                        && unify_pat(env, template_params, pk, ak, out)
                }
                (ParserInnerType::Tuple(ps), ParserInnerType::Tuple(as_))
                    if ps.len() == as_.len() =>
                {
                    ps.iter()
                        .zip(as_.iter())
                        .all(|(p, a)| unify_pat(env, template_params, p, a, out))
                }
                (ParserInnerType::Ref(p, _), ParserInnerType::Ref(a, _)) => {
                    unify_pat(env, template_params, p, a, out)
                }
                _ => pat.data_type == arg.data_type,
            }
        }

        if param_types.len() != arg_types.len() {
            return None;
        }

        let mut mapping: FxHashMap<String, ParserDataType> = FxHashMap::default();
        for (p, a) in param_types.iter().zip(arg_types.iter()) {
            if !unify_pat(self, template_params, p, a, &mut mapping) {
                return None;
            }
        }

        let mut result = Vec::new();
        for tp in template_params.iter() {
            result.push(mapping.get(tp)?.clone());
        }
        Some(result)
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
        let mut same = 0;
        let mut middle = env.evaluate(&scope, node.clone())?;

        for _ in 0..5 {
            let before_vars: Vec<(String, calibre_parser::ast::ParserDataType)> = env
                .variables
                .iter()
                .map(|(k, v)| (k.clone(), v.data_type.clone()))
                .collect();

            env.apply_inferred_types_to_middlenode(&scope, &mut middle);

            let after_vars: Vec<(String, calibre_parser::ast::ParserDataType)> = env
                .variables
                .iter()
                .map(|(k, v)| (k.clone(), v.data_type.clone()))
                .collect();

            let new_middle = env.evaluate(&scope, node.clone())?;

            let old_str = middle.to_string();
            let new_str = new_middle.to_string();

            middle = new_middle;

            if before_vars == after_vars && old_str == new_str {
                same += 1;
                if same >= 2 {
                    break;
                }
            } else {
                same = 0;
            }
        }

        env.apply_inferred_types_to_middlenode(&scope, &mut middle);

        if let Some(mut decls) = env.specialization_decls_by_scope.remove(&scope) {
            if !decls.is_empty() {
                match &mut middle.node_type {
                    MiddleNodeType::ScopeDeclaration { body, .. } => {
                        let mut new_body = Vec::new();
                        new_body.append(&mut decls);
                        new_body.append(body);
                        *body = new_body;
                    }
                    _ => {
                        let mut body = Vec::new();
                        body.append(&mut decls);
                        let middle_span = middle.span;
                        body.push(middle);
                        middle = MiddleNode::new(
                            MiddleNodeType::ScopeDeclaration {
                                body,
                                create_new_scope: false,
                                is_temp: false,
                            },
                            middle_span,
                        );
                    }
                }
            }
        }

        Ok((env, scope, middle))
    }

    fn apply_inferred_types_to_middlenode(&mut self, scope: &u64, node: &mut MiddleNode) {
        match &mut node.node_type {
            MiddleNodeType::FunctionDeclaration {
                parameters,
                body,
                return_type: _,
                ..
            } => {
                for (name, p_ty) in parameters.iter_mut() {
                    if p_ty.is_auto() {
                        if let Some(v) = self.variables.get(&name.text) {
                            *p_ty = v.data_type.clone();
                        }
                    }
                }

                self.apply_inferred_types_to_middlenode(scope, body);
            }
            MiddleNodeType::VariableDeclaration {
                identifier,
                data_type,
                value,
                ..
            } => {
                fn contains_auto(dt: &ParserDataType) -> bool {
                    match &dt.data_type {
                        ParserInnerType::Auto(_) => true,
                        ParserInnerType::Tuple(xs) => xs.iter().any(contains_auto),
                        ParserInnerType::List(x) => contains_auto(x),
                        ParserInnerType::Option(x) => contains_auto(x),
                        ParserInnerType::Result { ok, err } => {
                            contains_auto(ok) || contains_auto(err)
                        }
                        ParserInnerType::Function {
                            return_type,
                            parameters,
                            ..
                        } => contains_auto(return_type) || parameters.iter().any(contains_auto),
                        ParserInnerType::Ref(x, _) => contains_auto(x),
                        ParserInnerType::StructWithGenerics { generic_types, .. } => {
                            generic_types.iter().any(contains_auto)
                        }
                        ParserInnerType::Scope(xs) => xs.iter().any(contains_auto),
                        _ => false,
                    }
                }

                if data_type.is_auto() || contains_auto(data_type) {
                    if let Some(v) = self.variables.get(&identifier.text) {
                        *data_type = v.data_type.clone();
                    }
                }

                if let MiddleNodeType::FunctionDeclaration {
                    parameters,
                    return_type,
                    ..
                } = &mut value.node_type
                {
                    if let Some(scheme) = self.hm_env.get(&identifier.text) {
                        let applied = scheme.ty.clone();
                        let parser_ty = hm::to_parser_data_type(&applied);

                        if let ParserInnerType::Function {
                            return_type: inf_ret,
                            parameters: inf_params,
                            ..
                        } = parser_ty.data_type
                        {
                            for (i, (_n, p_ty)) in parameters.iter_mut().enumerate() {
                                if i < inf_params.len() {
                                    *p_ty = inf_params[i].clone();

                                    if let Some(var) = self.variables.get_mut(&_n.text) {
                                        var.data_type = inf_params[i].clone();
                                    }
                                }
                            }
                            *return_type = *inf_ret.clone();
                        }
                    }

                    self.apply_inferred_types_to_middlenode(scope, value);
                } else {
                    self.apply_inferred_types_to_middlenode(scope, value);
                }
            }
            MiddleNodeType::RangeDeclaration { from, to, .. } => {
                self.apply_inferred_types_to_middlenode(scope, from);
                self.apply_inferred_types_to_middlenode(scope, to);
            }
            MiddleNodeType::MemberExpression { path } => {
                for (mn, _flag) in path.iter_mut() {
                    self.apply_inferred_types_to_middlenode(scope, mn);
                }

                if path.len() >= 2 {
                    if let MiddleNodeType::Identifier(first_ident) = &path[0].0.node_type {
                        let mut struct_name_opt: Option<String> = None;

                        if let Some(var) = self.variables.get(&first_ident.text) {
                            if let ParserInnerType::Struct(x) =
                                var.data_type.clone().unwrap_all_refs().data_type
                            {
                                struct_name_opt = Some(x.to_string());
                            }
                        }

                        if struct_name_opt.is_none() {
                            if self.objects.contains_key(&first_ident.text) {
                                struct_name_opt = Some(first_ident.text.clone());
                            }
                        }

                        if let Some(struct_name) = struct_name_opt {
                            if let Some(obj) = self.objects.get(&struct_name) {
                                for (mn, _flag) in path.iter_mut().skip(1) {
                                    if let MiddleNodeType::Identifier(member_ident) = &mn.node_type
                                    {
                                        if let Some((mapped, _)) =
                                            obj.variables.get(&member_ident.text)
                                        {
                                            *mn = MiddleNode::new(
                                                MiddleNodeType::Identifier(ParserText::from(
                                                    mapped.clone(),
                                                )),
                                                self.current_span(),
                                            );
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
            MiddleNodeType::ScopeDeclaration { body, .. } => {
                for stmt in body.iter_mut() {
                    self.apply_inferred_types_to_middlenode(scope, stmt);
                }
            }
            MiddleNodeType::IfStatement {
                comparison,
                then,
                otherwise,
            } => {
                self.apply_inferred_types_to_middlenode(scope, comparison);
                self.apply_inferred_types_to_middlenode(scope, then);
                if let Some(o) = otherwise {
                    self.apply_inferred_types_to_middlenode(scope, o);
                }
            }
            MiddleNodeType::CallExpression { caller, args } => {
                if let MiddleNodeType::Identifier(caller_ident) = &caller.node_type {
                    let base = caller_ident
                        .text
                        .rsplit(':')
                        .next()
                        .unwrap_or(&caller_ident.text)
                        .to_string();

                    let resolved_key = self
                        .resolve_str(scope, &base)
                        .unwrap_or_else(|| caller_ident.text.clone());

                    let scheme = self
                        .hm_env
                        .get(&resolved_key)
                        .cloned()
                        .or_else(|| self.hm_env.get(&caller_ident.text).cloned())
                        .or_else(|| {
                            self.hm_env
                                .iter()
                                .find(|(k, _)| k.rsplit(':').next() == Some(base.as_str()))
                                .map(|(_, v)| v.clone())
                        });

                    if let Some(scheme) = scheme {
                        let mut tg = TypeGenerator::default();
                        let mut subst = Subst::default();
                        fn contains_auto(dt: &calibre_parser::ast::ParserDataType) -> bool {
                            match &dt.data_type {
                                calibre_parser::ast::ParserInnerType::Auto(_) => true,
                                calibre_parser::ast::ParserInnerType::Tuple(xs) => {
                                    xs.iter().any(contains_auto)
                                }
                                calibre_parser::ast::ParserInnerType::List(x) => contains_auto(x),
                                calibre_parser::ast::ParserInnerType::Option(x) => contains_auto(x),
                                calibre_parser::ast::ParserInnerType::Result { ok, err } => {
                                    contains_auto(ok) || contains_auto(err)
                                }
                                calibre_parser::ast::ParserInnerType::Function {
                                    return_type,
                                    parameters,
                                    ..
                                } => {
                                    contains_auto(return_type)
                                        || parameters.iter().any(contains_auto)
                                }
                                calibre_parser::ast::ParserInnerType::Ref(x, _) => contains_auto(x),
                                calibre_parser::ast::ParserInnerType::StructWithGenerics {
                                    generic_types,
                                    ..
                                } => generic_types.iter().any(contains_auto),
                                calibre_parser::ast::ParserInnerType::Scope(xs) => {
                                    xs.iter().any(contains_auto)
                                }
                                _ => false,
                            }
                        }

                        let scheme_is_unresolved =
                            contains_auto(&hm::to_parser_data_type(&scheme.ty));
                        let mut inst = if scheme_is_unresolved {
                            scheme.ty.clone()
                        } else {
                            hm::instantiate(&scheme, &mut tg)
                        };

                        for arg in args.iter() {
                            let arg_node: Node = arg.clone().into();
                            if let Some(arg_ty) = self.resolve_type_from_node(scope, &arg_node) {
                                let arg_hm = hm::from_parser_data_type(&arg_ty, &mut tg);

                                match inst {
                                    Type::TArrow(param, rest) => {
                                        subst = hm::unify(subst.clone(), &param, &arg_hm)
                                            .unwrap_or(subst);
                                        inst = *rest;
                                    }
                                    _ => {
                                        let fresh_ret = tg.fresh();
                                        subst = hm::unify(
                                            subst.clone(),
                                            &inst,
                                            &Type::TArrow(
                                                Box::new(arg_hm.clone()),
                                                Box::new(fresh_ret.clone()),
                                            ),
                                        )
                                        .unwrap_or(subst);
                                        inst = fresh_ret;
                                    }
                                }
                            }
                        }

                        for (_, scheme) in self.hm_env.iter_mut() {
                            scheme.ty = hm::apply_subst(&subst, &scheme.ty);
                        }

                        hm::recompute_scheme_vars_all(&mut self.hm_env);

                        for (k, v) in self.variables.iter_mut() {
                            if let Some(scheme) = self.hm_env.get(k) {
                                v.data_type = hm::to_parser_data_type(&scheme.ty);
                            }
                        }
                    }
                }

                self.apply_inferred_types_to_middlenode(scope, caller);
                for a in args.iter_mut() {
                    self.apply_inferred_types_to_middlenode(scope, a);
                }
            }
            MiddleNodeType::Return { value: Some(value) } => {
                self.apply_inferred_types_to_middlenode(scope, value);
            }
            MiddleNodeType::AggregateExpression { value, .. } => {
                for (_, v) in value.0.iter_mut() {
                    self.apply_inferred_types_to_middlenode(scope, v);
                }
            }
            MiddleNodeType::ListLiteral(_, items) => {
                for it in items.iter_mut() {
                    self.apply_inferred_types_to_middlenode(scope, it);
                }
            }
            MiddleNodeType::BinaryExpression { left, right, .. }
            | MiddleNodeType::ComparisonExpression { left, right, .. }
            | MiddleNodeType::BooleanExpression { left, right, .. } => {
                self.apply_inferred_types_to_middlenode(scope, left);
                self.apply_inferred_types_to_middlenode(scope, right);
            }
            MiddleNodeType::RefStatement { value, .. }
            | MiddleNodeType::DerefStatement { value }
            | MiddleNodeType::NegExpression { value }
            | MiddleNodeType::DebugExpression { value, .. }
            | MiddleNodeType::AsExpression { value, .. } => {
                self.apply_inferred_types_to_middlenode(scope, value);
            }
            _ => {}
        }
    }

    pub fn get_location(&self, scope: &u64, span: Span) -> Option<Location> {
        Some(Location {
            path: self.scopes.get(scope).unwrap().path.clone(),
            span,
        })
    }

    pub fn err_at_current(&self, err: MiddleErr) -> MiddleErr {
        if let Some(location) = &self.current_location {
            MiddleErr::At(location.span, Box::new(err))
        } else {
            err
        }
    }

    pub fn current_span(&self) -> Span {
        self.current_location
            .as_ref()
            .map(|loc| loc.span)
            .unwrap_or_default()
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
        let resolved = self.resolve_str(scope, &iden.text);

        Some(ParserText {
            text: resolved?.to_string(),
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
                self.resolve_potential_dollar_ident(scope, x)
            }
            PotentialGenericTypeIdentifier::Generic {
                identifier,
                generic_types: _,
            } => self.resolve_potential_dollar_ident(scope, identifier),
        }
    }

    pub fn resolve_potential_generic_ident_to_data_type(
        &mut self,
        scope: &u64,
        iden: &PotentialGenericTypeIdentifier,
    ) -> Option<ParserDataType> {
        match iden {
            PotentialGenericTypeIdentifier::Identifier(x) => {
                let resolved = self.resolve_potential_dollar_ident(scope, x)?;
                Some(ParserDataType {
                    data_type: ParserInnerType::Struct(resolved.text.to_string()),
                    span: resolved.span,
                })
            }
            PotentialGenericTypeIdentifier::Generic {
                identifier,
                generic_types,
            } => {
                let base = self.resolve_potential_dollar_ident(scope, identifier)?;

                let mut gens: Vec<ParserDataType> = Vec::new();
                for g in generic_types.iter() {
                    gens.push(self.resolve_potential_new_type(scope, g.clone()));
                }

                Some(ParserDataType {
                    data_type: ParserInnerType::StructWithGenerics {
                        identifier: base.text.to_string(),
                        generic_types: gens,
                    },
                    span: *identifier.span(),
                })
            }
        }
    }

    pub fn resolve_dollar_ident_potential_generic_only(
        &self,
        scope: &u64,
        iden: &PotentialGenericTypeIdentifier,
    ) -> Option<ParserText> {
        match iden {
            PotentialGenericTypeIdentifier::Identifier(x) => {
                self.resolve_dollar_ident_only(scope, x)
            }
            PotentialGenericTypeIdentifier::Generic {
                identifier,
                generic_types: _,
            } => self.resolve_dollar_ident_only(scope, identifier),
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
                let text = self
                    .resolve_macro_arg(scope, x)
                    .map(|x| match &x.node_type {
                        NodeType::Identifier(x) => match x.get_ident() {
                            PotentialDollarIdentifier::DollarIdentifier(x) => Some(x.clone()),
                            PotentialDollarIdentifier::Identifier(x) => Some(x.clone()),
                        },
                        _ => None,
                    })
                    .flatten();

                if let Some(text) = text {
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
                    NodeType::Identifier(x) => match x.get_ident() {
                        PotentialDollarIdentifier::DollarIdentifier(x) => Some(x.clone()),
                        PotentialDollarIdentifier::Identifier(x) => Some(x.clone()),
                    },
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
                        variables: FxHashMap::default(),
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
            ParserInnerType::StructWithGenerics {
                identifier,
                generic_types,
            } => {
                let id = self.resolve_str(scope, &identifier).unwrap_or(identifier);
                let mut resolved_gens: Vec<ParserDataType> = Vec::new();
                for g in generic_types {
                    resolved_gens.push(self.resolve_data_type(scope, g));
                }

                if let Some((tpl_params, _, _)) = self.generic_type_templates.get(&id).cloned() {
                    if tpl_params.len() == resolved_gens.len()
                        && !resolved_gens.iter().any(|g| g.is_auto())
                    {
                        if let Some(spec) =
                            self.ensure_specialized_type(scope, &id, &tpl_params, &resolved_gens)
                        {
                            return ParserDataType {
                                data_type: ParserInnerType::Struct(spec),
                                span: data_type.span,
                            };
                        }
                    }
                }

                ParserDataType {
                    data_type: ParserInnerType::StructWithGenerics {
                        identifier: id,
                        generic_types: resolved_gens,
                    },
                    span: data_type.span,
                }
            }
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
        Err(self.err_at_current(MiddleErr::Scope(namespace.to_string())))
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
                macros: FxHashMap::default(),
                macro_args: FxHashMap::default(),
                id: self.scope_counter,
                namespace: namespace
                    .unwrap_or(&self.scope_counter.to_string())
                    .to_string(),
                parent: Some(parent),
                children: FxHashMap::default(),
                mappings: FxHashMap::default(),
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
                } else if let Some(s) = self.get_global_scope().children.get(key) {
                    (s.clone(), None)
                } else {
                    let mut parser = Parser::default();
                    let mut tokenizer = Tokenizer::default();

                    let build_node =
                        if let Some(scope) = self.new_build_scope_from_parent(current.id, key) {
                            let path = self.scopes.get(&scope).unwrap().path.clone();
                            let source = fs::read_to_string(path.clone()).unwrap();
                            let tokens = tokenizer.tokenize(&source).map_err(|error| {
                                MiddleErr::LexerError {
                                    path: path.clone(),
                                    contents: source.clone(),
                                    error,
                                }
                            })?;
                            let program = parser.produce_ast(tokens);

                            if !parser.errors.is_empty() {
                                let errors = std::mem::take(&mut parser.errors);
                                return Err(MiddleErr::ParserErrors {
                                    path,
                                    contents: source,
                                    errors,
                                });
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
                    let path = self.scopes.get(&scope).unwrap().path.clone();
                    let source = fs::read_to_string(path.clone()).unwrap();
                    let tokens =
                        tokenizer
                            .tokenize(&source)
                            .map_err(|error| MiddleErr::LexerError {
                                path: path.clone(),
                                contents: source.clone(),
                                error,
                            })?;
                    let program = parser.produce_ast(tokens);

                    if !parser.errors.is_empty() {
                        let errors = std::mem::take(&mut parser.errors);
                        return Err(MiddleErr::ParserErrors {
                            path,
                            contents: source,
                            errors,
                        });
                    }

                    let node = self.evaluate(&scope, program)?;

                    let node = match (node.node_type.clone(), build_node) {
                        (MiddleNodeType::ScopeDeclaration { mut body, .. }, Some(build_node)) => {
                            MiddleNode {
                                node_type: MiddleNodeType::ScopeDeclaration {
                                    body: {
                                        body.insert(0, build_node);
                                        body
                                    },
                                    create_new_scope: true,
                                    is_temp: false,
                                },
                                ..node
                            }
                        }
                        (_, Some(build_node)) => MiddleNode::new(
                            MiddleNodeType::ScopeDeclaration {
                                body: vec![node, build_node],
                                create_new_scope: false,
                                is_temp: false,
                            },
                            self.current_span(),
                        ),
                        _ => node,
                    };

                    (scope, Some(node))
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
                } else if let Some(s) = self.get_global_scope().children.get(key) {
                    s.clone()
                } else {
                    return Err(self.err_at_current(MiddleErr::Scope(key.to_string())));
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
                let ident = self.resolve_potential_dollar_ident(scope, x)?.text;

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
                data_type: match identifier {
                    PotentialGenericTypeIdentifier::Generic {
                        identifier: base,
                        generic_types,
                    } => {
                        let base = self.resolve_dollar_ident_only(scope, base)?;
                        let concrete: Vec<ParserDataType> = generic_types
                            .iter()
                            .map(|g| self.resolve_potential_new_type(scope, g.clone()))
                            .collect();

                        if let Some((tpl_params, _, _)) =
                            self.generic_type_templates.get(&base.text).cloned()
                        {
                            if let Some(spec) = self.ensure_specialized_type(
                                scope,
                                &base.text,
                                &tpl_params,
                                &concrete,
                            ) {
                                ParserInnerType::Struct(spec)
                            } else {
                                ParserInnerType::Struct(base.text)
                            }
                        } else {
                            ParserInnerType::Struct(base.text)
                        }
                    }
                    _ => ParserInnerType::Struct(
                        self.resolve_potential_generic_ident(scope, identifier)?
                            .to_string(),
                    ),
                },
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
                generic_types: _,
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

                    if let Some(parsed_caller_pd) =
                        self.resolve_potential_generic_ident_to_data_type(scope, caller)
                    {
                        match &parsed_caller_pd.data_type {
                            ParserInnerType::Struct(name) => {
                                if self.objects.contains_key(name) {
                                    return Some(ParserDataType {
                                        data_type: ParserInnerType::Struct(name.clone()),
                                        span: node.span,
                                    });
                                }
                            }
                            ParserInnerType::StructWithGenerics {
                                identifier,
                                generic_types,
                            } => {
                                if self.objects.contains_key(identifier) {
                                    return Some(ParserDataType {
                                        data_type: ParserInnerType::StructWithGenerics {
                                            identifier: identifier.clone(),
                                            generic_types: generic_types.clone(),
                                        },
                                        span: node.span,
                                    });
                                }
                            }
                            _ => {}
                        }

                        let base_ident = match parsed_caller_pd.data_type {
                            ParserInnerType::Struct(ref n) => n.clone(),
                            ParserInnerType::StructWithGenerics { ref identifier, .. } => {
                                identifier.clone()
                            }
                            _ => caller.to_string(),
                        };

                        if let Some(scheme) = self.hm_env.get(&base_ident) {
                            let mut tg = TypeGenerator::default();
                            let inst = hm::instantiate(scheme, &mut tg);
                            let p = hm::to_parser_data_type(&inst);
                            caller_type = Some(self.resolve_data_type(scope, p));
                        } else if let Some(var) = self.variables.get(&base_ident) {
                            caller_type = Some(var.data_type.clone());
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
                if let Some(iden) = self.resolve_potential_generic_ident(scope, x) {
                    if let Some(scheme) = self.hm_env.get(&iden.text) {
                        let mut tg = TypeGenerator::default();
                        let inst = hm::instantiate(scheme, &mut tg);
                        let p = hm::to_parser_data_type(&inst);
                        return Some(self.resolve_data_type(scope, p));
                    }

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

                if path.len() == 2 {
                    if let (NodeType::Identifier(_base_ident), NodeType::Identifier(field_ident)) =
                        (&path[0].0.node_type, &path[1].0.node_type)
                    {
                        let field_name = match field_ident {
                            PotentialGenericTypeIdentifier::Identifier(x) => x.to_string(),
                            PotentialGenericTypeIdentifier::Generic { identifier, .. } => {
                                identifier.to_string()
                            }
                        };

                        if let Some(base_ty) = self.resolve_type_from_node(scope, &path[0].0) {
                            let base_ty = self.resolve_data_type(scope, base_ty).unwrap_all_refs();
                            let struct_name_opt = match base_ty.data_type {
                                ParserInnerType::Struct(s) => Some(s),
                                ParserInnerType::StructWithGenerics { identifier, .. } => {
                                    Some(identifier)
                                }
                                _ => None,
                            };

                            if let Some(struct_name) = struct_name_opt {
                                if let Some(obj) = self.objects.get(&struct_name) {
                                    if let MiddleTypeDefType::Struct(map) = &obj.object_type {
                                        if let Some(field_ty) = map.get(&field_name) {
                                            return Some(field_ty.clone());
                                        }
                                    }
                                }
                            }
                        }
                    }
                }

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
            let resolved = self.resolve_data_type(scope, typ);

            fn contains_auto(dt: &ParserDataType) -> bool {
                match &dt.data_type {
                    ParserInnerType::Auto(_) => true,
                    ParserInnerType::Tuple(xs) => xs.iter().any(|x| contains_auto(x)),
                    ParserInnerType::List(x) => contains_auto(x),
                    ParserInnerType::Option(x) => contains_auto(x),
                    ParserInnerType::Result { ok, err } => contains_auto(ok) || contains_auto(err),
                    ParserInnerType::Function {
                        return_type,
                        parameters,
                        ..
                    } => contains_auto(return_type) || parameters.iter().any(|p| contains_auto(p)),
                    ParserInnerType::Ref(x, _) => contains_auto(x),
                    ParserInnerType::StructWithGenerics { generic_types, .. } => {
                        generic_types.iter().any(|g| contains_auto(g))
                    }
                    ParserInnerType::Scope(xs) => xs.iter().any(|x| contains_auto(x)),
                    _ => false,
                }
            }

            if contains_auto(&resolved) {
                if let Some(inferred) = infer_node_type(self, scope, &node) {
                    return Some(self.resolve_data_type(scope, inferred));
                }
            }

            Some(resolved)
        } else {
            None
        }
    }
}
