use crate::{
    IdentifiersUsed, Span,
    ast::{
        binary::BinaryOperator,
        idents::{
            ParsedIntLiteral, ParserText, PotentialDollarIdentifier, PotentialGenericTypeIdentifier,
        },
        nodes::{
            access::{AstField, AstIdentifier, AstIndex, AstScope},
            assignment::{AstAssignDestructure, AstAssignment},
            binary::{AstAs, AstBinary, AstBoolean, AstComparison, AstIn, AstIs},
            conditionals::{AstIf, AstTernary},
            declaration::{AstDeclaration, AstDeclareDestructure},
            flow::{
                AstBreak, AstContinue, AstDefer, AstEmit, AstPipe, AstReturn, AstTry, TryCatch,
                TryType,
            },
            functions::{AstCall, AstCurry, AstExtern, AstFunction, CallArg},
            generator::AstGenerator,
            lists::AstList,
            literals::{
                AstBig, AstChar, AstDataType, AstEnum, AstFloat, AstInt, AstRange, AstString,
                AstStruct, AstTuple,
            },
            loops::{AstIter, AstLoop, LoopType},
            matching::{AstFnMatch, AstMatch},
            memory::{AstDeref, AstDrop, AstMove, AstRef},
            misc::{AstImport, AstParen, AstTag, AstTest},
            scopes::{AstScopeAlias, AstScopeDef},
            spawn::{AstSelect, AstSpawn},
            types::{AstImpl, AstType},
            unary::{AstNeg, AstNot},
        },
        types::ParserDataType,
    },
    formatter::{AstFormatting, Formatter},
};
use rustc_hash::FxHashMap;
use serde::{Deserialize, Serialize};
use std::{fmt::Display, matches, ops::Range};
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

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AstNode {
    pub node_type: AstNodeType,
    pub span: Span,
}

impl PartialEq for AstNode {
    fn eq(&self, other: &Self) -> bool {
        self.node_type.eq(&other.node_type)
    }
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
                value: ParsedIntLiteral::parse(value).unwrap_or_default(),
                format: None,
            }),
        )
    }

    pub fn range(span: Span, value: Range<usize>) -> Self {
        AstNode::new(
            span,
            AstNodeType::RangeDeclaration(AstRange {
                from: Box::new(AstNode::int(span, value.start)),
                to: Box::new(AstNode::int(span, value.end)),
                inclusive: false,
            }),
        )
    }

    pub fn nodes(self) -> Vec<AstNode> {
        match self.node_type {
            AstNodeType::ScopeDeclaration(AstScopeDef {
                body: Some(items), ..
            }) => items,
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
        Self::new(
            if body.is_empty() {
                Span::default()
            } else {
                Span::new_from_spans(body.first().unwrap().span, body.last().unwrap().span)
            },
            AstNodeType::ScopeDeclaration(AstScopeDef {
                body: Some(body),
                named: None,
                is_temp: true,
                create_new_scope,
                define: false,
            }),
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

    pub fn binary(span: Span, left: AstNode, right: AstNode, op: BinaryOperator) -> AstNode {
        AstNode::new(
            span,
            AstNodeType::BinaryExpression(AstBinary {
                left: Box::new(left),
                right: Box::new(right),
                operator: op,
            }),
        )
    }

    pub fn assign(span: Span, identifier: AstNode, value: AstNode) -> AstNode {
        AstNode::new(
            span,
            AstNodeType::AssignmentExpression(AstAssignment {
                identifier: Box::new(identifier),
                value: Box::new(value),
            }),
        )
    }

    pub fn var_decl(
        span: Span,
        identifier: PotentialDollarIdentifier,
        var_type: VarType,
        value: AstNode,
        data_type: ParserDataType,
    ) -> Self {
        AstNode::new(
            span,
            AstNodeType::VariableDeclaration(AstDeclaration {
                identifier,
                var_type,
                value: Box::new(value),
                data_type,
                declared: false,
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

    pub fn is_emit(&self) -> bool {
        matches!(&self.node_type, AstNodeType::Emit(_))
    }

    #[inline]
    pub fn unwrap_or(span: Span, value: AstNode, default: AstNode) -> AstNode {
        AstNode {
            node_type: AstNodeType::Try(AstTry {
                value: Box::new(value),
                catch: Some(TryCatch {
                    name: None,
                    body: Box::new(default),
                }),
                try_type: TryType::Normal,
            }),
            span,
        }
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
            AstNodeType::ScopeDeclaration(AstScopeDef {
                body: Some(body),
                create_new_scope,
                is_temp,
                named,
                define,
            }) => AstNode {
                node_type: AstNodeType::ScopeDeclaration(AstScopeDef {
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
                }),
                span: self.span,
            },
            AstNodeType::Emit(AstEmit::Scope(value)) => AstNode {
                node_type: AstNodeType::Return(AstReturn { value: Some(value) }),
                span: self.span,
            },
            _ => self,
        }
    }

    pub fn substitute(self, subst: &FxHashMap<String, ParserDataType>) -> Self {
        Self {
            node_type: self.node_type.substitute(subst),
            span: self.span,
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
            AstNodeType::IndexAccess(AstIndex { base, index, .. }) => {
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
            AstNodeType::ScopeDeclaration(AstScopeDef {
                body: Some(body), ..
            }) => {
                for stmt in body {
                    names.extend(stmt.identifiers_used());
                }
            }
            _ => {}
        }
        names
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

    // Conditionals
    IfStatement(AstIf),
    Ternary(AstTernary),

    // Functions
    FunctionDeclaration(AstFunction),
    ExternFunctionDeclaration(AstExtern),
    CallExpression(AstCall),
    CurryExpression(AstCurry),

    // Matching
    FnMatchDeclaration(AstFnMatch),
    MatchStatement(AstMatch),

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

    // Spawn
    Spawn(AstSpawn),
    SelectStatement(AstSelect),

    // Declaration
    VariableDeclaration(AstDeclaration),
    DestructureDeclaration(AstDeclareDestructure),

    // Assignment
    AssignmentExpression(AstAssignment),
    DestructureAssignment(AstAssignDestructure),

    // Types
    ImplDeclaration(AstImpl),
    TypeDeclaration(AstType),

    // Loops
    LoopDeclaration(AstLoop),
    IterExpression(AstIter),

    // Scopes
    ScopeAlias(AstScopeAlias),
    ScopeDeclaration(AstScopeDef),

    // Generator
    InlineGenerator(AstGenerator),

    // Misc
    ParenExpression(AstParen),
    TestDeclaration(AstTest),
    ImportStatement(AstImport),
    Tag(AstTag),
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

    pub fn substitute(self, subst: &FxHashMap<String, ParserDataType>) -> Self {
        // TODO
        match self {
            Self::AsExpression(AstAs {
                value,
                data_type,
                failure_mode,
            }) => Self::AsExpression(AstAs {
                value: Box::new(value.substitute(subst)),
                data_type: data_type.substitute(subst),
                failure_mode,
            }),
            Self::AssignmentExpression(AstAssignment { identifier, value }) => {
                Self::AssignmentExpression(AstAssignment {
                    identifier: Box::new(identifier.substitute(subst)),
                    value: Box::new(value.substitute(subst)),
                })
            }
            Self::BinaryExpression(AstBinary {
                left,
                right,
                operator,
            }) => Self::BinaryExpression(AstBinary {
                left: Box::new(left.substitute(subst)),
                right: Box::new(right.substitute(subst)),
                operator,
            }),
            Self::BooleanExpression(AstBoolean {
                left,
                right,
                operator,
            }) => Self::BooleanExpression(AstBoolean {
                left: Box::new(left.substitute(subst)),
                right: Box::new(right.substitute(subst)),
                operator,
            }),
            Self::ComparisonExpression(AstComparison {
                left,
                right,
                operator,
            }) => Self::ComparisonExpression(AstComparison {
                left: Box::new(left.substitute(subst)),
                right: Box::new(right.substitute(subst)),
                operator,
            }),
            Self::Break(AstBreak {
                label,
                value: Some(value),
            }) => Self::Break(AstBreak {
                label,
                value: Some(Box::new(value.substitute(subst))),
            }),
            Self::CallExpression(AstCall {
                string_fn,
                caller,
                generic_types,
                args,
                reverse_args,
            }) => Self::CallExpression(AstCall {
                string_fn,
                caller: Box::new(caller.substitute(subst)),
                generic_types,
                args: args.into_iter().map(|x| x.substitute(subst)).collect(),
                reverse_args: reverse_args
                    .into_iter()
                    .map(|x| x.substitute(subst))
                    .collect(),
            }),
            Self::DataType(AstDataType { data_type }) => Self::DataType(AstDataType {
                data_type: data_type.substitute(subst),
            }),
            Self::CurryExpression(AstCurry { value }) => Self::CurryExpression(AstCurry {
                value: Box::new(value.substitute(subst)),
            }),
            Self::Defer(AstDefer { value, function }) => Self::Defer(AstDefer {
                value: Box::new(value.substitute(subst)),
                function,
            }),
            Self::DerefStatement(AstDeref { value }) => Self::DerefStatement(AstDeref {
                value: Box::new(value.substitute(subst)),
            }),
            Self::DestructureAssignment(AstAssignDestructure { pattern, value }) => {
                Self::DestructureAssignment(AstAssignDestructure {
                    pattern,
                    value: Box::new(value.substitute(subst)),
                })
            }
            Self::DestructureDeclaration(AstDeclareDestructure {
                var_type,
                pattern,
                value,
            }) => Self::DestructureDeclaration(AstDeclareDestructure {
                var_type,
                pattern,
                value: Box::new(value.substitute(subst)),
            }),
            Self::Emit(AstEmit::Channel {
                left,
                right,
                left_channel,
            }) => Self::Emit(AstEmit::Channel {
                left: Box::new(left.substitute(subst)),
                right: Box::new(right.substitute(subst)),
                left_channel,
            }),
            Self::Emit(AstEmit::Scope(x)) => {
                Self::Emit(AstEmit::Scope(Box::new(x.substitute(subst))))
            }
            Self::EnumExpression(AstEnum {
                identifier,
                value,
                data,
            }) => Self::EnumExpression(AstEnum {
                identifier,
                value,
                data: data.map(|x| Box::new(x.substitute(subst))),
            }),
            Self::ExternFunctionDeclaration(AstExtern {
                abi,
                identifier,
                parameters,
                return_type,
                library,
                symbol,
                declared,
            }) => Self::ExternFunctionDeclaration(AstExtern {
                abi,
                identifier,
                parameters: parameters
                    .into_iter()
                    .map(|x| x.substitute(subst))
                    .collect(),
                return_type: return_type.substitute(subst),
                library,
                symbol,
                declared,
            }),
            Self::FnMatchDeclaration(AstFnMatch { header, body }) => {
                Self::FnMatchDeclaration(AstFnMatch {
                    header: header.substitute(subst),
                    body: body.substitute(subst),
                })
            }
            Self::FunctionDeclaration(AstFunction { header, body }) => {
                Self::FunctionDeclaration(AstFunction {
                    header: header.substitute(subst),
                    body: Box::new(body.substitute(subst)),
                })
            }
            Self::IfStatement(AstIf {
                comparison,
                then,
                otherwise,
            }) => Self::IfStatement(AstIf {
                comparison: Box::new(comparison.substitute(subst)),
                then: Box::new(then.substitute(subst)),
                otherwise: otherwise.map(|x| Box::new(x.substitute(subst))),
            }),
            Self::ImplDeclaration(AstImpl {
                generics,
                target,
                variables,
            }) => Self::ImplDeclaration(AstImpl {
                generics: generics.substitute(subst),
                target: target.substitute(subst),
                variables: variables.into_iter().map(|x| x.substitute(subst)).collect(),
            }),
            Self::InDeclaration(AstIn { identifier, value }) => Self::InDeclaration(AstIn {
                identifier: Box::new(identifier.substitute(subst)),
                value: Box::new(value.substitute(subst)),
            }),
            Self::IndexAccess(AstIndex { base, index, panic }) => Self::IndexAccess(AstIndex {
                base: Box::new(base.substitute(subst)),
                index: Box::new(index.substitute(subst)),
                panic,
            }),
            Self::InlineGenerator(AstGenerator {
                map,
                data_type,
                loop_type,
                conditionals,
                until,
            }) => Self::InlineGenerator(AstGenerator {
                map: Box::new(map.substitute(subst)),
                data_type: data_type.map(|x| x.substitute(subst)),
                loop_type: Box::new(loop_type.substitute(subst)),
                conditionals: conditionals
                    .into_iter()
                    .map(|x| x.substitute(subst))
                    .collect(),
                until: until.map(|x| Box::new(x.substitute(subst))),
            }),
            Self::IsExpression(AstIs { value, data_type }) => Self::IsExpression(AstIs {
                value: Box::new(value.substitute(subst)),
                data_type: data_type.substitute(subst),
            }),
            Self::IterExpression(AstIter {
                data_type,
                map,
                spawned,
                loop_type,
                conditionals,
                until,
            }) => Self::IterExpression(AstIter {
                data_type: data_type.substitute(subst),
                map: Box::new(map.substitute(subst)),
                spawned,
                loop_type: Box::new(loop_type.substitute(subst)),
                conditionals: conditionals
                    .into_iter()
                    .map(|x| x.substitute(subst))
                    .collect(),
                until: until.map(|x| Box::new(x.substitute(subst))),
            }),
            Self::ListLiteral(AstList { data_type, values }) => Self::ListLiteral(AstList {
                data_type: data_type.substitute(subst),
                values: values.into_iter().map(|x| x.substitute(subst)).collect(),
            }),
            Self::LoopDeclaration(AstLoop {
                loop_type,
                body,
                until,
                label,
                else_body,
            }) => Self::LoopDeclaration(AstLoop {
                loop_type: Box::new(loop_type.substitute(subst)),
                body: Box::new(body.substitute(subst)),
                until: until.map(|x| Box::new(x.substitute(subst))),
                label,
                else_body: else_body.map(|x| Box::new(x.substitute(subst))),
            }),
            Self::MatchStatement(AstMatch { value, body }) => Self::MatchStatement(AstMatch {
                value: value.map(|x| Box::new(x.substitute(subst))),
                body: body.substitute(subst),
            }),
            Self::NegExpression(AstNeg { value }) => Self::NegExpression(AstNeg {
                value: Box::new(value.substitute(subst)),
            }),
            Self::NotExpression(AstNot { value }) => Self::NotExpression(AstNot {
                value: Box::new(value.substitute(subst)),
            }),
            Self::ParenExpression(AstParen { value }) => Self::ParenExpression(AstParen {
                value: Box::new(value.substitute(subst)),
            }),
            Self::PipeExpression(AstPipe { values }) => Self::PipeExpression(AstPipe {
                values: values.into_iter().map(|x| x.substitute(subst)).collect(),
            }),
            x => x,
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
        write!(f, "{}", fake_node.format(&mut formatter))
    }
}

impl Display for AstNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut formatter = Formatter::default();
        write!(f, "{}", self.format(&mut formatter))
    }
}

impl Display for LoopType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut formatter = Formatter::default();
        write!(f, "{}", self.format(&mut formatter))
    }
}
