use crate::{
    IdentifiersUsed, Span,
    ast::{
        ObjectType, Operator,
        binary::BinaryOperator,
        formatter::Formatter,
        generics::TraitMember,
        idents::{ParserText, PotentialDollarIdentifier, PotentialGenericTypeIdentifier},
        matching::{MatchArmType, SelectArm},
        nodes::{
            access::{AstField, AstIdentifier, AstIndex, AstScope},
            binary::{AstAs, AstBinary, AstBoolean, AstComparison, AstIn, AstIs},
            conditionals::{AstIf, AstTernary},
            flow::{AstBreak, AstContinue, AstDefer, AstEmit, AstPipe, AstReturn, AstTry},
            functions::{AstCall, AstCurry, AstExtern, AstFunction, CallArg, FunctionHeader},
            literals::{
                AstBig, AstChar, AstDataType, AstEnum, AstFloat, AstInt, AstRange, AstString,
                AstStruct, AstTuple,
            },
            loops::{AstList, AstListRepeat},
            memory::{AstDeref, AstDrop, AstMove, AstRef},
            unary::{AstNeg, AstNot},
        },
        types::{GenericTypes, ParserDataType},
    },
};
use rustc_hash::FxHashMap;
use serde::{Deserialize, Serialize};
use std::{fmt::Display, matches, str::FromStr};
use ustr::Ustr;

pub mod access;
pub mod assignment;
pub mod binary;
pub mod conditionals;
pub mod declaration;
pub mod flow;
pub mod functions;
pub mod generator;
pub mod lists;
pub mod literals;
pub mod loops;
pub mod matching;
pub mod memory;
pub mod misc;
pub mod scopes;
pub mod spawn;
pub mod types;
pub mod unary;

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum LoopType {
    Let {
        value: AstNode,
        pattern: (Vec<MatchArmType>, Vec<AstNode>),
    },
    While(AstNode),
    For(PotentialDollarIdentifier, AstNode),
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

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum TypeDefType {
    Enum {
        variants: Vec<(PotentialDollarIdentifier, Option<ParserDataType>)>,
        default_variant: Option<usize>,
        default_value: Option<Box<AstNode>>,
    },
    Struct {
        fields: ObjectType<(ParserDataType, Option<AstNode>)>,
    },
    NewType(Box<ParserDataType>),
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
                default_variant: *default_variant,
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

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct AstNode {
    pub node_type: AstNodeType,
    pub span: Span,
}

impl AstNode {
    pub fn new(span: Span, node_type: AstNodeType) -> Self {
        Self { node_type, span }
    }

    pub fn none(span: Span) -> Self {
        AstNode::identifier(span, "none")
    }

    pub fn int(span: Span, value: impl ToString) -> Self {
        AstNode::new(
            span,
            AstNodeType::IntLiteral(AstInt {
                value: ParserText::new(span, value.to_string()),
            }),
        )
    }

    pub fn nodes(self) -> Vec<AstNode> {
        match self.node_type {
            AstNodeType::ScopeDeclaration {
                body: Some(items), ..
            } => items,
            _ => vec![self],
        }
    }

    pub fn scope_access_path(&self, path: &mut Vec<Ustr>) -> bool {
        match &self.node_type {
            AstNodeType::Identifier(identifier) => {
                path.push(Ustr::from(identifier.value.get_ident().text()));
                true
            }
            AstNodeType::ScopeAccess(AstScope { base, field }) => {
                if !base.scope_access_path(path) {
                    return false;
                }
                path.push(Ustr::from(field.text()));
                true
            }
            _ => false,
        }
    }

    pub fn new_temp_scope(body: Vec<AstNode>) -> AstNode {
        Self::new_temp_scope_with_create(body, Some(true))
    }

    pub fn new_temp_scope_with_create(body: Vec<AstNode>, create_new_scope: Option<bool>) -> Self {
        if body.is_empty() {
            return Self::new(
                Span::default(),
                AstNodeType::ScopeDeclaration {
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
            AstNodeType::ScopeDeclaration {
                body: Some(body),
                named: None,
                is_temp: true,
                create_new_scope,
                define: false,
            },
        )
    }

    pub fn ret(node: AstNode) -> Self {
        Self::new(
            node.span,
            AstNodeType::Return(AstReturn {
                value: Some(Box::new(node)),
            }),
        )
    }

    pub fn emit(node: AstNode) -> Self {
        Self::new(node.span, AstNodeType::Emit(AstEmit::Scope(Box::new(node))))
    }

    pub fn null(span: Span) -> Self {
        AstNode::new(span, AstNodeType::Null)
    }

    pub fn bool(span: Span, value: bool) -> Self {
        Self::identifier(span, if value { "true" } else { "false" })
    }

    #[inline]
    pub fn is_raw_option_value(&self) -> bool {
        match &self.node_type {
            AstNodeType::CallExpression(AstCall { caller, .. }) => matches!(
                &caller.node_type,
                AstNodeType::Identifier(x) if x.value.get_ident().text() == "some"
            ),
            AstNodeType::Identifier(x) => x.value.get_ident().text() == "none",
            _ => false,
        }
    }

    pub fn identifier(span: Span, text: impl ToString) -> Self {
        Self::new(
            span,
            AstNodeType::Identifier(AstIdentifier {
                value: PotentialGenericTypeIdentifier::Identifier(
                    ParserText::from(text.to_string()).into(),
                ),
            }),
        )
    }

    pub fn member(span: Span, base: Self, member: impl ToString) -> Self {
        Self::new(
            span,
            AstNodeType::FieldAccess(AstField {
                base: Box::new(base),
                field: PotentialDollarIdentifier::new(span, member),
            }),
        )
    }

    pub fn call(span: Span, caller: AstNode, args: Vec<CallArg>) -> Self {
        Self::new(
            span,
            AstNodeType::CallExpression(AstCall {
                string_fn: None,
                caller: Box::new(caller),
                generic_types: Vec::new(),
                args,
                reverse_args: Vec::new(),
            }),
        )
    }

    pub fn call_with_generics(
        span: Span,
        caller: AstNode,
        generic_types: Vec<ParserDataType>,
        args: Vec<CallArg>,
    ) -> Self {
        Self::new(
            span,
            AstNodeType::CallExpression(AstCall {
                string_fn: None,
                caller: Box::new(caller),
                generic_types,
                args,
                reverse_args: Vec::new(),
            }),
        )
    }

    pub fn call_full(
        span: Span,
        caller: AstNode,
        generic_types: Vec<ParserDataType>,
        args: Vec<CallArg>,
        reverse_args: Vec<AstNode>,
        string_fn: Option<ParserText>,
    ) -> Self {
        Self::new(
            span,
            AstNodeType::CallExpression(AstCall {
                string_fn,
                caller: Box::new(caller),
                generic_types,
                args,
                reverse_args,
            }),
        )
    }

    pub fn len(span: Span, node: AstNode) -> Self {
        Self::call(
            span,
            Self::identifier(span, "len"),
            vec![CallArg::Value(node)],
        )
    }

    pub fn is_none(&self) -> bool {
        matches!(
            &self.node_type,
            AstNodeType::Identifier(id) if id.value.get_ident().text() == "none"
        )
    }

    pub fn unwrap_bit_ors(self) -> Vec<Self> {
        match self.node_type {
            AstNodeType::BinaryExpression(AstBinary {
                left,
                right,
                operator: BinaryOperator::BitOr,
            }) => {
                let mut left = left.unwrap_bit_ors();
                left.append(&mut right.unwrap_bit_ors());
                left
            }
            _ => vec![self],
        }
    }

    pub fn rewrite_main_emits_to_returns(self) -> Self {
        match self.node_type {
            AstNodeType::ScopeDeclaration {
                body: Some(body),
                create_new_scope,
                is_temp,
                named,
                define,
            } => AstNode {
                node_type: AstNodeType::ScopeDeclaration {
                    body: Some(
                        body.into_iter()
                            .map(|x| match x.node_type {
                                AstNodeType::Emit(AstEmit::Scope(value)) => AstNode {
                                    node_type: AstNodeType::Return(AstReturn {
                                        value: Some(value),
                                    }),
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
            AstNodeType::Emit(AstEmit::Scope(value)) => AstNode {
                node_type: AstNodeType::Return(AstReturn { value: Some(value) }),
                span: self.span,
            },
            _ => self,
        }
    }
}

impl IdentifiersUsed for AstNode {
    fn identifiers_used(&self) -> Vec<&String> {
        let mut names = Vec::new();
        match &self.node_type {
            AstNodeType::Identifier(AstIdentifier { value }) => {
                names.push(value.get_ident().text());
            }
            AstNodeType::FieldAccess(AstField { base, .. }) => {
                names.extend(base.identifiers_used());
            }
            AstNodeType::ScopeAccess(AstScope { base, .. }) => {
                names.extend(base.identifiers_used());
            }
            AstNodeType::IndexAccess(AstIndex { base, index }) => {
                names.extend(base.identifiers_used());
                names.extend(index.identifiers_used());
            }
            AstNodeType::CallExpression(AstCall { args, .. }) => {
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
            AstNodeType::ScopeDeclaration {
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

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct NamedScope {
    pub name: PotentialDollarIdentifier,
    pub args: Vec<(PotentialDollarIdentifier, AstNode)>,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct Overload {
    pub operator: ParserText,
    pub body: Box<AstNode>,
    pub header: FunctionHeader,
}

impl From<Overload> for AstNode {
    fn from(val: Overload) -> AstNode {
        AstNode::new(
            val.operator.span,
            AstNodeType::FunctionDeclaration(AstFunction {
                header: val.header,
                body: val.body,
            }),
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

// Flow

#[repr(u8)]
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub enum AstNodeType {
    EmptyLine,
    Null,

    // Flow
    Emit(AstEmit),
    Break(AstBreak),
    Continue(AstContinue),
    Try(AstTry),
    Return(AstReturn),
    Defer(AstDefer),
    PipeExpression(AstPipe),

    // Literals
    StructLiteral(AstStruct),
    EnumExpression(AstEnum),
    TupleLiteral(AstTuple),
    RangeDeclaration(AstRange),
    StringLiteral(AstString),
    CharLiteral(AstChar),
    FloatLiteral(AstFloat),
    IntLiteral(AstInt),
    BigLiteral(AstBig),
    DataType(AstDataType),

    // Lists
    ListLiteral(AstList),
    ListRepeatLiteral(AstListRepeat),

    // Conditionals
    IfStatement(AstIf),
    Ternary(AstTernary),

    // Functions
    FunctionDeclaration(AstFunction),
    ExternFunctionDeclaration(AstExtern),
    CallExpression(AstCall),
    CurryExpression(AstCurry),

    // Matching
    FnMatchDeclaration {
        header: FunctionHeader,
        body: Vec<(MatchArmType, Vec<AstNode>, Box<AstNode>)>,
    },
    MatchStatement {
        value: Option<Box<AstNode>>,
        body: Vec<(MatchArmType, Vec<AstNode>, Box<AstNode>)>,
    },

    // Unary
    NotExpression(AstNot),
    NegExpression(AstNeg),

    // Binary
    BinaryExpression(AstBinary),
    ComparisonExpression(AstComparison),
    BooleanExpression(AstBoolean),
    AsExpression(AstAs),
    IsExpression(AstIs),
    InDeclaration(AstIn),

    // Memory
    RefStatement(AstRef),
    DerefStatement(AstDeref),
    Drop(AstDrop),
    MoveExpression(AstMove),

    // Access
    Identifier(AstIdentifier),
    FieldAccess(AstField),
    ScopeAccess(AstScope),
    IndexAccess(AstIndex),

    // Async
    Spawn {
        items: Vec<AstNode>,
        auto_wait: bool,
    },
    SelectStatement {
        arms: Vec<SelectArm>,
    },

    // Declaration
    VariableDeclaration {
        var_type: VarType,
        identifier: PotentialDollarIdentifier,
        value: Box<AstNode>,
        data_type: ParserDataType,
    },
    DestructureDeclaration {
        var_type: VarType,
        pattern: DestructurePattern,
        value: Box<AstNode>,
    },

    // Assignment
    AssignmentExpression {
        identifier: Box<AstNode>,
        value: Box<AstNode>,
    },
    DestructureAssignment {
        pattern: DestructurePattern,
        value: Box<AstNode>,
    },

    // Types
    ImplDeclaration {
        generics: GenericTypes,
        target: ParserDataType,
        variables: Vec<AstNode>,
    },
    ImplTraitDeclaration {
        generics: GenericTypes,
        trait_ident: PotentialGenericTypeIdentifier,
        target: ParserDataType,
        variables: Vec<AstNode>,
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

    // Loops
    LoopDeclaration {
        loop_type: Box<LoopType>,
        body: Box<AstNode>,
        until: Option<Box<AstNode>>,
        label: Option<PotentialDollarIdentifier>,
        else_body: Option<Box<AstNode>>,
    },
    IterExpression {
        data_type: ParserDataType,
        map: Box<AstNode>,
        spawned: bool,
        loop_type: Box<LoopType>,
        conditionals: Vec<AstNode>,
        until: Option<Box<AstNode>>,
    },

    // Scopes
    ScopeAlias {
        identifier: PotentialDollarIdentifier,
        value: NamedScope,
        create_new_scope: Option<bool>,
    },
    ScopeDeclaration {
        body: Option<Vec<AstNode>>,
        named: Option<NamedScope>,
        is_temp: bool,
        create_new_scope: Option<bool>,
        define: bool,
    },

    // Generator
    InlineGenerator {
        map: Box<AstNode>,
        data_type: Option<ParserDataType>,
        loop_type: Box<LoopType>,
        conditionals: Vec<AstNode>,
        until: Option<Box<AstNode>>,
    },

    // Misc
    ParenExpression {
        value: Box<AstNode>,
    },
    TestDeclaration {
        identifier: ParserText,
        body: Box<AstNode>,
    },
    ImportStatement {
        module: Vec<PotentialDollarIdentifier>,
        alias: Option<PotentialDollarIdentifier>,
        values: Vec<PotentialDollarIdentifier>,
    },
    Tag {
        node: Box<AstNode>,
        tag: ParserText,
        arguments: Vec<AstNode>,
    },
}

impl AstNodeType {
    pub fn unwrap(self) -> AstNodeType {
        self
    }

    pub fn is_call(&self) -> bool {
        match self {
            Self::CallExpression { .. } => true,
            Self::RefStatement(AstRef { value, .. }) | Self::DerefStatement(AstDeref { value }) => {
                value.node_type.is_call()
            }

            _ => false,
        }
    }
}

impl Display for AstNodeType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut formatter = Formatter::default();
        let fake_node = AstNode {
            node_type: self.clone(),
            span: Span::default(),
        };
        write!(f, "{}", formatter.format(&fake_node))
    }
}

impl Display for AstNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut formatter = Formatter::default();
        write!(f, "{}", formatter.format(self))
    }
}

impl Display for LoopType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut formatter = Formatter::default();
        write!(f, "{}", formatter.fmt_loop_type(self))
    }
}
