use crate::{
    IdentifiersUsed, Span,
    ast::{
        ObjectType, Operator, RefMutability,
        binary::BinaryOperator,
        comparison::{BooleanOperator, ComparisonOperator},
        formatter::Formatter,
        generics::TraitMember,
        idents::{ParserText, PotentialDollarIdentifier, PotentialGenericTypeIdentifier},
        matching::{MatchArmType, SelectArm, TryCatch},
        types::{GenericTypes, ParserDataType, PotentialNewType},
    },
};
use rustc_hash::FxHashMap;
use serde::{Deserialize, Serialize};
use std::{fmt::Display, str::FromStr};

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub enum AsFailureMode {
    Result,
    Panic,
    Option,
}

#[derive(Debug, Clone, PartialEq)]
pub enum LoopType {
    Let {
        value: Node,
        pattern: (Vec<MatchArmType>, Vec<Node>),
    },
    While(Node),
    For(PotentialDollarIdentifier, Node),
    Loop,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum DestructurePattern {
    Tuple(Vec<Option<(VarType, PotentialDollarIdentifier)>>),
    Struct(Vec<(String, VarType, PotentialDollarIdentifier)>),
}

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
pub enum VarType {
    Mutable,
    Immutable,
    Constant,
}

impl VarType {
    pub fn print_only_ends(&self) -> String {
        match self {
            Self::Mutable => "mut",
            Self::Immutable => "let",
            Self::Constant => "const",
        }
        .to_string()
    }
}

impl Display for VarType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Mutable => write!(f, "let mut"),
            Self::Immutable => write!(f, "let"),
            Self::Constant => write!(f, "const"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypeDefType {
    Enum {
        variants: Vec<(PotentialDollarIdentifier, Option<PotentialNewType>)>,
        default_variant: Option<usize>,
        default_value: Option<Box<Node>>,
    },
    Struct {
        fields: ObjectType<(PotentialNewType, Option<Node>)>,
    },
    NewType(Box<PotentialNewType>),
}

impl TypeDefType {
    pub fn substitute(&self, subst: &FxHashMap<String, ParserDataType>) -> TypeDefType {
        match self {
            TypeDefType::Struct { fields } => TypeDefType::Struct {
                fields: match fields {
                    ObjectType::Map(xs) => ObjectType::Map(
                        xs.iter()
                            .map(|(k, (v, _default))| (k.clone(), (v.substitute(subst), None)))
                            .collect(),
                    ),
                    ObjectType::Tuple(xs) => ObjectType::Tuple(
                        xs.iter()
                            .map(|(v, _default)| (v.substitute(subst), None))
                            .collect(),
                    ),
                },
            },
            TypeDefType::Enum {
                variants,
                default_variant,
                default_value,
            } => TypeDefType::Enum {
                variants: variants
                    .iter()
                    .map(|(k, v)| (k.clone(), v.as_ref().map(|p| p.substitute(subst))))
                    .collect(),
                default_variant: default_variant.clone(),
                default_value: default_value.clone(),
            },
            TypeDefType::NewType(inner) => TypeDefType::NewType(Box::new(inner.substitute(subst))),
        }
    }
}

impl IdentifiersUsed for TypeDefType {
    fn identifiers_used(&self) -> Vec<&String> {
        let mut names = Vec::new();
        match self {
            TypeDefType::Enum { variants, .. } => {
                for (_, potential_type) in variants {
                    if let Some(potential) = potential_type {
                        names.extend(potential.identifiers_used());
                    }
                }
            }
            TypeDefType::Struct { fields } => {
                if let ObjectType::Map(field_map) = fields {
                    for (_, (potential_type, default_value)) in field_map {
                        names.extend(potential_type.identifiers_used());
                        if let Some(default) = default_value {
                            names.extend(default.identifiers_used());
                        }
                    }
                }
            }
            TypeDefType::NewType(inner) => {
                names.extend(inner.identifiers_used());
            }
        }
        names
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Node {
    pub node_type: NodeType,
    pub span: Span,
}

impl Node {
    pub fn new(span: Span, node_type: NodeType) -> Self {
        Self { node_type, span }
    }

    pub fn none(span: Span) -> Self {
        Node::identifier(span, "none")
    }

    pub fn int(span: Span, value: impl ToString) -> Self {
        Node::new(
            span,
            NodeType::IntLiteral(ParserText::new(span, value.to_string())),
        )
    }

    pub fn nodes(self) -> Vec<Node> {
        match self.node_type {
            NodeType::ScopeDeclaration {
                body: Some(items), ..
            } => items,
            _ => vec![self],
        }
    }

    pub fn new_temp_scope(body: Vec<Node>) -> Node {
        Self::new_temp_scope_with_create(body, Some(true))
    }

    pub fn new_temp_scope_with_create(body: Vec<Node>, create_new_scope: Option<bool>) -> Self {
        if body.is_empty() {
            return Self::new(
                Span::default(),
                NodeType::ScopeDeclaration {
                    body: Some(Vec::new()),
                    named: None,
                    is_temp: true,
                    create_new_scope,
                    define: false,
                },
            );
        }
        let span = Span::new_from_spans(body.first().unwrap().span, body.last().unwrap().span);

        Self::new(
            span,
            NodeType::ScopeDeclaration {
                body: Some(body),
                named: None,
                is_temp: true,
                create_new_scope,
                define: false,
            },
        )
    }

    pub fn ret(node: Node) -> Self {
        Self::new(
            node.span,
            NodeType::Return {
                value: Some(Box::new(node)),
            },
        )
    }

    pub fn null(span: Span) -> Self {
        Node::new(span, NodeType::Null)
    }

    pub fn bool(span: Span, value: bool) -> Self {
        Self::identifier(span, if value { "true" } else { "false" })
    }

    #[inline]
    pub fn is_raw_option_value(&self) -> bool {
        match &self.node_type {
            NodeType::CallExpression { caller, .. } => matches!(
                &caller.node_type,
                NodeType::Identifier(x) if x.to_string() == "some"
            ),
            NodeType::Identifier(x) => x.to_string() == "none",
            _ => false,
        }
    }

    pub fn identifier(span: Span, text: impl ToString) -> Self {
        Self::new(
            span,
            NodeType::Identifier(PotentialGenericTypeIdentifier::Identifier(
                ParserText::from(text.to_string()).into(),
            )),
        )
    }

    pub fn member(span: Span, base: Self, member: impl ToString) -> Self {
        Self::new(
            span,
            NodeType::MemberExpression {
                path: vec![(base, false), (Self::identifier(span, member), false)],
            },
        )
    }

    pub fn call(span: Span, caller: Node, args: Vec<CallArg>) -> Self {
        Self::new(
            span,
            NodeType::CallExpression {
                string_fn: None,
                caller: Box::new(caller),
                generic_types: Vec::new(),
                args,
                reverse_args: Vec::new(),
            },
        )
    }

    pub fn call_with_generics(
        span: Span,
        caller: Node,
        generic_types: Vec<PotentialNewType>,
        args: Vec<CallArg>,
    ) -> Self {
        Self::new(
            span,
            NodeType::CallExpression {
                string_fn: None,
                caller: Box::new(caller),
                generic_types,
                args,
                reverse_args: Vec::new(),
            },
        )
    }

    pub fn call_full(
        span: Span,
        caller: Node,
        generic_types: Vec<PotentialNewType>,
        args: Vec<CallArg>,
        reverse_args: Vec<Node>,
        string_fn: Option<ParserText>,
    ) -> Self {
        Self::new(
            span,
            NodeType::CallExpression {
                string_fn,
                caller: Box::new(caller),
                generic_types,
                args,
                reverse_args,
            },
        )
    }

    pub fn len(span: Span, node: Node) -> Self {
        Self::call(
            span,
            Self::identifier(span, "len"),
            vec![CallArg::Value(node)],
        )
    }

    pub fn is_none(&self) -> bool {
        matches!(
            &self.node_type,
            NodeType::Identifier(id) if id.to_string() == "none"
        )
    }

    pub fn rewrite_main_emits_to_returns(self) -> Self {
        match self.node_type {
            NodeType::ScopeDeclaration {
                body: Some(body),
                create_new_scope,
                is_temp,
                named,
                define,
            } => Node {
                node_type: NodeType::ScopeDeclaration {
                    body: Some(
                        body.into_iter()
                            .map(|x| match x.node_type {
                                NodeType::Emit(EmitType::Scope(value)) => Node {
                                    node_type: NodeType::Return { value: Some(value) },
                                    span: self.span,
                                },
                                _ => x,
                            })
                            .collect(),
                    ),
                    create_new_scope,
                    is_temp,
                    named,
                    define,
                },
                span: self.span,
            },
            NodeType::Emit(EmitType::Scope(value)) => Node {
                node_type: NodeType::Return { value: Some(value) },
                span: self.span,
            },
            _ => self,
        }
    }
}

impl IdentifiersUsed for Node {
    fn identifiers_used(&self) -> Vec<&String> {
        let mut names = Vec::new();
        match &self.node_type {
            NodeType::Identifier(text) => {
                names.push(text.get_ident().text());
            }
            NodeType::MemberExpression { path, .. } => {
                if let Some((first, _)) = path.first() {
                    names.extend(first.identifiers_used());
                }
            }
            NodeType::CallExpression { args, .. } => {
                for arg in args {
                    match arg {
                        CallArg::Value(node) => {
                            names.extend(node.identifiers_used());
                        }
                        CallArg::Named(_, node) => {
                            names.extend(node.identifiers_used());
                        }
                    }
                }
            }
            NodeType::ScopeDeclaration {
                body: Some(body), ..
            } => {
                for stmt in body {
                    names.extend(stmt.identifiers_used());
                }
            }
            _ => {}
        }
        names
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct NamedScope {
    pub name: PotentialDollarIdentifier,
    pub args: Vec<(PotentialDollarIdentifier, Node)>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Overload {
    pub operator: ParserText,
    pub body: Box<Node>,
    pub header: FunctionHeader,
}

impl Into<Node> for Overload {
    fn into(self) -> Node {
        Node::new(
            self.operator.span,
            NodeType::FunctionDeclaration {
                header: self.header,
                body: self.body,
            },
        )
    }
}

impl Overload {
    pub fn span(&self) -> &Span {
        &self.operator.span
    }

    pub fn verify(&self) -> Result<(), String> {
        let operator = Operator::from_str(&self.operator.text)?;
        match operator {
            Operator::As if !self.header.return_type.is_result() => Err(format!(
                "Expect result return type (Err!Ok) found {}",
                self.header.return_type
            )),
            Operator::In if !self.header.return_type.is_bool() => Err(format!(
                "Expect bool return type found {}",
                self.header.return_type
            )),
            Operator::Binary(_) | Operator::Comparison(_) | Operator::Binary(_)
                if self.header.return_type.is_null() || self.header.return_type.is_auto() =>
            {
                Err(format!(
                    "Expect known non-null return type found {}",
                    self.header.return_type
                ))
            }
            Operator::Index if self.header.parameters.len() != 2 => Err(format!(
                "Expect 2 parameters found {}",
                self.header.parameters.len()
            )),
            Operator::IndexAssign if self.header.parameters.len() != 3 => Err(format!(
                "Expect 3 parameters found {}",
                self.header.parameters.len()
            )),
            _ => Ok(()),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct FunctionHeader {
    pub generics: GenericTypes,
    pub parameters: Vec<(
        PotentialDollarIdentifier,
        Option<PotentialNewType>,
        Option<Box<Node>>,
    )>,
    pub return_type: PotentialNewType,
    pub param_destructures: Vec<(usize, DestructurePattern)>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum CallArg {
    Value(Node),
    Named(PotentialDollarIdentifier, Node),
}

impl From<CallArg> for Node {
    fn from(value: CallArg) -> Self {
        match value {
            CallArg::Value(x) => x,
            CallArg::Named(_, x) => x,
        }
    }
}

impl<'a> From<&'a CallArg> for &'a Node {
    fn from(value: &'a CallArg) -> Self {
        match value {
            CallArg::Value(x) => x,
            CallArg::Named(_, x) => x,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum EmitType {
    Scope(Box<Node>),
    Channel {
        channel: Box<Node>,
        value: Box<Node>,
    },
}

#[repr(u8)]
#[derive(Clone, Debug, PartialEq)]
pub enum NodeType {
    Break {
        label: Option<PotentialDollarIdentifier>,
        value: Option<Box<Node>>,
    },
    Continue {
        label: Option<PotentialDollarIdentifier>,
    },
    EmptyLine,
    Null,
    Spawn {
        items: Vec<Node>,
        auto_wait: bool,
    },
    SelectStatement {
        arms: Vec<SelectArm>,
    },
    Emit(EmitType),
    RefStatement {
        mutability: RefMutability,
        value: Box<Node>,
    },
    Identifier(PotentialGenericTypeIdentifier),
    DataType {
        data_type: PotentialNewType,
    },
    DerefStatement {
        value: Box<Node>,
    },
    Drop(PotentialDollarIdentifier),
    MoveExpression {
        value: Box<Node>,
    },
    Defer {
        value: Box<Node>,
        function: bool,
    },
    ParenExpression {
        value: Box<Node>,
    },
    VariableDeclaration {
        var_type: VarType,
        identifier: PotentialDollarIdentifier,
        value: Box<Node>,
        data_type: PotentialNewType,
    },
    ImplDeclaration {
        generics: GenericTypes,
        target: PotentialNewType,
        variables: Vec<Node>,
    },
    ImplTraitDeclaration {
        generics: GenericTypes,
        trait_ident: PotentialGenericTypeIdentifier,
        target: PotentialNewType,
        variables: Vec<Node>,
    },
    TraitDeclaration {
        identifier: PotentialGenericTypeIdentifier,
        implied_traits: Vec<PotentialDollarIdentifier>,
        members: Vec<TraitMember>,
    },
    TypeDeclaration {
        identifier: PotentialGenericTypeIdentifier,
        object: TypeDefType,
        overloads: Vec<Overload>,
    },
    EnumExpression {
        identifier: PotentialGenericTypeIdentifier,
        value: PotentialDollarIdentifier,
        data: Option<Box<Node>>,
    },
    TupleLiteral {
        values: Vec<Node>,
    },
    ScopeAlias {
        identifier: PotentialDollarIdentifier,
        value: NamedScope,
        create_new_scope: Option<bool>,
    },
    ScopeDeclaration {
        body: Option<Vec<Node>>,
        named: Option<NamedScope>,
        is_temp: bool,
        create_new_scope: Option<bool>,
        define: bool,
    },
    MatchStatement {
        value: Option<Box<Node>>,
        body: Vec<(MatchArmType, Vec<Node>, Box<Node>)>,
    },
    FnMatchDeclaration {
        header: FunctionHeader,
        body: Vec<(MatchArmType, Vec<Node>, Box<Node>)>,
    },
    FunctionDeclaration {
        header: FunctionHeader,
        body: Box<Node>,
    },
    ExternFunctionDeclaration {
        abi: String,
        identifier: PotentialDollarIdentifier,
        parameters: Vec<ParserDataType>,
        return_type: ParserDataType,
        library: String,
        symbol: Option<String>,
    },
    AssignmentExpression {
        identifier: Box<Node>,
        value: Box<Node>,
    },
    DestructureDeclaration {
        var_type: VarType,
        pattern: DestructurePattern,
        value: Box<Node>,
    },
    DestructureAssignment {
        pattern: DestructurePattern,
        value: Box<Node>,
    },
    NotExpression {
        value: Box<Node>,
    },
    NegExpression {
        value: Box<Node>,
    },
    DebugExpression {
        value: Box<Node>,
    },
    AsExpression {
        value: Box<Node>,
        data_type: PotentialNewType,
        failure_mode: AsFailureMode,
    },
    IsExpression {
        value: Box<Node>,
        data_type: ParserDataType,
    },
    InDeclaration {
        identifier: Box<Node>,
        value: Box<Node>,
    },
    RangeDeclaration {
        from: Box<Node>,
        to: Box<Node>,
        inclusive: bool,
    },
    IterExpression {
        data_type: PotentialNewType,
        map: Box<Node>,
        spawned: bool,
        loop_type: Box<LoopType>,
        conditionals: Vec<Node>,
        until: Option<Box<Node>>,
    },
    InlineGenerator {
        map: Box<Node>,
        data_type: Option<PotentialNewType>,
        loop_type: Box<LoopType>,
        conditionals: Vec<Node>,
        until: Option<Box<Node>>,
    },
    LoopDeclaration {
        loop_type: Box<LoopType>,
        body: Box<Node>,
        until: Option<Box<Node>>,
        label: Option<PotentialDollarIdentifier>,
        else_body: Option<Box<Node>>,
    },
    TestDeclaration {
        identifier: ParserText,
        body: Box<Node>,
    },
    Try {
        value: Box<Node>,
        catch: Option<TryCatch>,
    },
    Return {
        value: Option<Box<Node>>,
    },
    Until {
        condition: Box<Node>,
    },
    StringLiteral(ParserText),
    ListLiteral(PotentialNewType, Vec<Node>),
    ListRepeatLiteral {
        data_type: PotentialNewType,
        value: Box<Node>,
        count: Box<Node>,
    },
    CharLiteral(char),
    FloatLiteral(f64),
    IntLiteral(ParserText),
    MemberExpression {
        path: Vec<(Node, bool)>,
    },
    ScopeMemberExpression {
        module: Vec<PotentialDollarIdentifier>,
        value: Box<Node>,
    },
    CallExpression {
        string_fn: Option<ParserText>,
        caller: Box<Node>,
        generic_types: Vec<PotentialNewType>,
        args: Vec<CallArg>,
        reverse_args: Vec<Node>,
    },
    BinaryExpression {
        left: Box<Node>,
        right: Box<Node>,
        operator: BinaryOperator,
    },
    ComparisonExpression {
        left: Box<Node>,
        right: Box<Node>,
        operator: ComparisonOperator,
    },
    PipeExpression(Vec<PipeSegment>),
    BooleanExpression {
        left: Box<Node>,
        right: Box<Node>,
        operator: BooleanOperator,
    },
    IfStatement {
        comparison: Box<IfComparisonType>,
        then: Box<Node>,
        otherwise: Option<Box<Node>>,
    },
    Ternary {
        comparison: Box<Node>,
        then: Box<Node>,
        otherwise: Box<Node>,
    },
    ImportStatement {
        module: Vec<PotentialDollarIdentifier>,
        alias: Option<PotentialDollarIdentifier>,
        values: Vec<PotentialDollarIdentifier>,
    },
    StructLiteral {
        identifier: PotentialGenericTypeIdentifier,
        value: ObjectType<Node>,
    },
    Tag {
        node: Box<Node>,
        tag: ParserText,
        arguments: Vec<Node>,
    },
}

#[derive(Clone, Debug, PartialEq)]
pub enum PipeSegment {
    Unnamed(Node),
    Named {
        identifier: PotentialDollarIdentifier,
        node: Node,
    },
}

impl PipeSegment {
    pub fn is_named(&self) -> bool {
        match self {
            Self::Unnamed(_) => false,
            _ => true,
        }
    }
    pub fn span(&self) -> &Span {
        match self {
            Self::Unnamed(x) => &x.span,
            Self::Named {
                identifier: _,
                node,
            } => &node.span,
        }
    }

    pub fn get_node(&self) -> &Node {
        match self {
            Self::Unnamed(x) => x,
            Self::Named {
                identifier: _,
                node,
            } => node,
        }
    }
}

impl Into<Node> for PipeSegment {
    fn into(self) -> Node {
        match self {
            Self::Unnamed(x) => x,
            Self::Named {
                identifier: _,
                node,
            } => node,
        }
    }
}

impl NodeType {
    pub fn unwrap(self) -> NodeType {
        self
    }

    pub fn is_call(&self) -> bool {
        match self {
            Self::CallExpression { .. } => true,
            Self::RefStatement { value, .. } | Self::DerefStatement { value } => {
                value.node_type.is_call()
            }

            _ => false,
        }
    }
}

impl Display for NodeType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut formatter = Formatter::default();
        let fake_node = Node {
            node_type: self.clone(),
            span: Span::default(),
        };
        write!(f, "{}", formatter.format(&fake_node))
    }
}

impl Display for Node {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut formatter = Formatter::default();
        write!(f, "{}", formatter.format(&self))
    }
}

impl Display for LoopType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut formatter = Formatter::default();
        write!(f, "{}", formatter.fmt_loop_type(&self))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum IfComparisonType {
    IfLet {
        value: Node,
        pattern: (Vec<MatchArmType>, Vec<Node>),
    },
    If(Node),
}
