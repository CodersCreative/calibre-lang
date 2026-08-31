use calibre_parser::{
    Span,
    ast::{
        ObjectMap, ObjectType, RefMutability,
        binary::BinaryOperator,
        comparison::{BooleanOperator, ComparisonOperator},
        idents::{IntLiteralType, ParsedIntLiteral, ParserText, PotentialGenericTypeIdentifier},
        nodes::{
            AsFailureMode, AstNode, AstNodeType, CallArg, EmitType, FunctionHeader,
            IfComparisonType, LoopType, VarType,
        },
        types::{GenericTypes, ParserDataType},
    },
};
use derive_builder::Builder;
use std::fmt::Display;
use ustr::{Ustr, UstrMap};

use crate::{errors::MiddleErr, scoping::ScopeId};

pub mod identifiers;
pub mod renaming;

#[derive(Debug, Clone, PartialEq)]
pub struct MiddleNode {
    pub node_type: MiddleNodeType,
    pub span: Span,
}

impl MiddleNode {
    #[inline(always)]
    pub fn new(node_type: MiddleNodeType, span: Span) -> Self {
        Self { node_type, span }
    }

    #[inline(always)]
    pub fn identifier(span: Span, text: impl ToString) -> Self {
        Self::new(
            MiddleNodeType::Identifier(MirIdentifier {
                identifier: text.to_string().into(),
            }),
            span,
        )
    }

    pub fn member_field(&self) -> Result<Ustr, MiddleErr> {
        Ok(match &self.node_type {
            MiddleNodeType::Identifier(name) => name.identifier.clone(),
            MiddleNodeType::IntLiteral(MirInt {
                value: ParsedIntLiteral { value, int_type },
            }) => match int_type {
                IntLiteralType::Int => Ustr::from(&value.to_string()),
                IntLiteralType::UInt => Ustr::from(&format!("{value}u")),
                IntLiteralType::Byte => Ustr::from(&format!("{value}b")),
            },
            MiddleNodeType::FloatLiteral(x) => Ustr::from(&x.value.to_string()),
            _ => return Err(MiddleErr::InvalidMember),
        })
    }

    pub fn len(&self) -> usize {
        let mut count = 1;
        match &self.node_type {
            MiddleNodeType::AssignmentExpression(MirAssignment { identifier, value }) => {
                count += identifier.len();
                count += value.len();
            }
            MiddleNodeType::CallExpression(MirCall { caller, args }) => {
                count += caller.len();
                for a in args {
                    count += a.len();
                }
            }
            MiddleNodeType::BinaryExpression(MirBinary { left, right, .. })
            | MiddleNodeType::ComparisonExpression(MirComparison { left, right, .. })
            | MiddleNodeType::BooleanExpression(MirBoolean { left, right, .. })
            | MiddleNodeType::IndexAccess(MirIndex {
                base: left,
                index: right,
            })
            | MiddleNodeType::RangeDeclaration(MirRange {
                from: left,
                to: right,
                ..
            }) => {
                count += left.len();
                count += right.len();
            }
            MiddleNodeType::AsExpression(MirAs { value, .. })
            | MiddleNodeType::FieldAccess(MirField { base: value, .. })
            | MiddleNodeType::IsExpression(MirIs { value, .. })
            | MiddleNodeType::NegExpression(MirNeg { value })
            | MiddleNodeType::RefStatement(MirRef { value, .. })
            | MiddleNodeType::DerefStatement(MirDeref { value })
            | MiddleNodeType::DebugExpression(MirDebug { value, .. })
            | MiddleNodeType::Return(MirReturn { value: Some(value) })
            | MiddleNodeType::EnumExpression(MirEnum {
                data: Some(value), ..
            })
            | MiddleNodeType::VariableDeclaration(MirVarDecl { value, .. }) => count += value.len(),
            MiddleNodeType::ListLiteral(MirList {
                data_type: _,
                values,
            })
            | MiddleNodeType::ScopeDeclaration(MirScopeDecl { body: values, .. }) => {
                for v in values {
                    count += v.len();
                }
            }
            MiddleNodeType::LoopDeclaration(MirLoop { state, body, .. }) => {
                if let Some(s) = state.as_ref() {
                    count += s.len();
                }
                count += body.len();
            }
            _ => {}
        }
        count
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn substitute(&mut self, repl: &UstrMap<MiddleNode>) {
        match &mut self.node_type {
            MiddleNodeType::Identifier(MirIdentifier { identifier }) => {
                if let Some(replacement) = repl.get(&identifier) {
                    *self = replacement.clone();
                }
            }
            MiddleNodeType::FunctionDeclaration(_) => {}
            MiddleNodeType::ScopeDeclaration(MirScopeDecl { body, .. }) => {
                for stmt in body {
                    stmt.substitute(repl);
                }
            }
            MiddleNodeType::AssignmentExpression(MirAssignment { identifier, value }) => {
                identifier.substitute(repl);
                value.substitute(repl);
            }
            MiddleNodeType::CallExpression(MirCall { caller, args }) => {
                caller.substitute(repl);
                for a in args.iter_mut() {
                    a.substitute(repl);
                }
            }
            MiddleNodeType::Return(MirReturn { value }) => {
                if let Some(v) = value.as_mut() {
                    v.substitute(repl);
                }
            }
            MiddleNodeType::BinaryExpression(MirBinary { left, right, .. })
            | MiddleNodeType::ComparisonExpression(MirComparison { left, right, .. })
            | MiddleNodeType::BooleanExpression(MirBoolean { left, right, .. })
            | MiddleNodeType::RangeDeclaration(MirRange {
                from: left,
                to: right,
                ..
            }) => {
                left.substitute(repl);
                right.substitute(repl);
            }
            MiddleNodeType::AsExpression(MirAs { value, .. })
            | MiddleNodeType::IsExpression(MirIs { value, .. })
            | MiddleNodeType::NegExpression(MirNeg { value })
            | MiddleNodeType::RefStatement(MirRef { value, .. })
            | MiddleNodeType::DerefStatement(MirDeref { value })
            | MiddleNodeType::DebugExpression(MirDebug { value, .. })
            | MiddleNodeType::VariableDeclaration(MirVarDecl { value, .. }) => {
                value.substitute(repl)
            }
            MiddleNodeType::ListLiteral(MirList {
                data_type: _,
                values,
            }) => {
                for v in values {
                    v.substitute(repl);
                }
            }
            MiddleNodeType::LoopDeclaration(MirLoop { state, body, .. }) => {
                if let Some(s) = state.as_mut() {
                    s.substitute(repl);
                }
                body.substitute(repl);
            }
            MiddleNodeType::FieldAccess(MirField { base, .. }) => base.substitute(repl),
            MiddleNodeType::IndexAccess(MirIndex { base, index }) => {
                base.substitute(repl);
                index.substitute(repl);
            }
            MiddleNodeType::EnumExpression(MirEnum { data, .. }) => {
                if let Some(d) = data.as_mut() {
                    d.substitute(repl);
                }
            }
            _ => {}
        }
    }

    pub fn calls_self(&self, name: &Ustr) -> bool {
        match &self.node_type {
            MiddleNodeType::Identifier(MirIdentifier { identifier }) => identifier == name,
            MiddleNodeType::CallExpression(MirCall { caller, args }) => {
                if caller.calls_self(name) {
                    return true;
                }
                args.iter().any(|a| a.calls_self(name))
            }
            MiddleNodeType::FunctionDeclaration(_) => false,
            MiddleNodeType::ScopeDeclaration(MirScopeDecl { body, .. }) => {
                body.iter().any(|n| n.calls_self(name))
            }
            MiddleNodeType::Return(MirReturn { value }) => {
                value.as_ref().is_some_and(|v| v.calls_self(name))
            }
            MiddleNodeType::AssignmentExpression(MirAssignment { identifier, value }) => {
                identifier.calls_self(name) || value.calls_self(name)
            }
            MiddleNodeType::BinaryExpression(MirBinary { left, right, .. })
            | MiddleNodeType::ComparisonExpression(MirComparison { left, right, .. })
            | MiddleNodeType::BooleanExpression(MirBoolean { left, right, .. })
            | MiddleNodeType::RangeDeclaration(MirRange {
                from: left,
                to: right,
                ..
            }) => left.calls_self(name) || right.calls_self(name),
            MiddleNodeType::AsExpression(MirAs { value, .. })
            | MiddleNodeType::IsExpression(MirIs { value, .. })
            | MiddleNodeType::NegExpression(MirNeg { value })
            | MiddleNodeType::RefStatement(MirRef { value, .. })
            | MiddleNodeType::DerefStatement(MirDeref { value })
            | MiddleNodeType::DebugExpression(MirDebug { value, .. })
            | MiddleNodeType::VariableDeclaration(MirVarDecl { value, .. }) => {
                value.calls_self(name)
            }
            MiddleNodeType::ListLiteral(MirList {
                data_type: _,
                values,
            }) => values.iter().any(|v| v.calls_self(name)),
            MiddleNodeType::LoopDeclaration(MirLoop { state, body, .. }) => {
                state.as_ref().is_some_and(|s| s.calls_self(name)) || body.calls_self(name)
            }
            MiddleNodeType::FieldAccess(MirField { base, .. }) => base.calls_self(name),
            MiddleNodeType::IndexAccess(MirIndex { base, index }) => {
                base.calls_self(name) || index.calls_self(name)
            }
            MiddleNodeType::EnumExpression(MirEnum { data, .. }) => {
                data.as_ref().is_some_and(|d| d.calls_self(name))
            }
            _ => false,
        }
    }
}

// TODO split Data in the struct into their own structs
// Have those structs implement a Translate trait in the LIR

#[derive(Clone, Debug, PartialEq, Builder)]
pub struct MirBreak {
    pub label: Option<Ustr>,
    pub value: Option<Box<MiddleNode>>,
}

#[derive(Clone, Debug, PartialEq, Builder)]
pub struct MirContinue {
    pub label: Option<Ustr>,
}

#[derive(Clone, Debug, PartialEq, Builder)]
pub struct MirRef {
    pub mutability: RefMutability,
    pub value: Box<MiddleNode>,
}

#[derive(Clone, Debug, PartialEq, Builder)]
pub struct MirDrop {
    pub identifier: Ustr,
}

#[derive(Clone, Debug, PartialEq, Builder)]
pub struct MirMove {
    pub identifier: Ustr,
}

#[derive(Clone, Debug, PartialEq, Builder)]
pub struct MirSpawn {
    pub value: Box<MiddleNode>,
}

#[derive(Clone, Debug, PartialEq, Builder)]
pub struct MirDeref {
    pub value: Box<MiddleNode>,
}

#[derive(Clone, Debug, PartialEq, Builder)]
pub struct MirIdentifier {
    pub identifier: Ustr,
}

#[derive(Clone, Debug, PartialEq, Builder)]
pub struct MirString {
    pub value: Ustr,
}

#[derive(Clone, Debug, PartialEq, Builder)]
pub struct MirList {
    pub data_type: ParserDataType,
    pub values: Vec<MiddleNode>,
}

#[derive(Clone, Debug, PartialEq, Builder)]
pub struct MirChar {
    pub value: char,
}

#[derive(Clone, Debug, PartialEq, Builder)]
pub struct MirFloat {
    pub value: f64,
}

#[derive(Clone, Debug, PartialEq, Builder)]
pub struct MirInt {
    pub value: ParsedIntLiteral,
}

#[derive(Clone, Debug, PartialEq, Builder)]
pub struct MirBig {
    pub value: Ustr,
}

#[derive(Clone, Debug, PartialEq, Builder)]
pub struct MirBinary {
    pub left: Box<MiddleNode>,
    pub right: Box<MiddleNode>,
    pub operator: BinaryOperator,
}

#[derive(Clone, Debug, PartialEq, Builder)]
pub struct MirComparison {
    pub left: Box<MiddleNode>,
    pub right: Box<MiddleNode>,
    pub operator: ComparisonOperator,
}

#[derive(Clone, Debug, PartialEq, Builder)]
pub struct MirBoolean {
    pub left: Box<MiddleNode>,
    pub right: Box<MiddleNode>,
    pub operator: BooleanOperator,
}

#[derive(Clone, Debug, PartialEq, Builder)]
pub struct MirNeg {
    pub value: Box<MiddleNode>,
}

#[derive(Clone, Debug, PartialEq, Builder)]
pub struct MirAs {
    pub value: Box<MiddleNode>,
    pub data_type: ParserDataType,
    pub failure_mode: AsFailureMode,
}

#[derive(Clone, Debug, PartialEq, Builder)]
pub struct MirIs {
    pub value: Box<MiddleNode>,
    pub data_type: ParserDataType,
}

#[derive(Clone, Debug, PartialEq, Builder)]
pub struct MirConditional {
    pub comparison: Box<MiddleNode>,
    pub then: Box<MiddleNode>,
    pub otherwise: Option<Box<MiddleNode>>,
}

#[derive(Clone, Debug, PartialEq, Builder)]
pub struct MirRange {
    pub from: Box<MiddleNode>,
    pub to: Box<MiddleNode>,
    pub inclusive: bool,
}

#[derive(Clone, Debug, PartialEq, Builder)]
pub struct MirLoop {
    pub state: Option<Box<MiddleNode>>,
    pub body: Box<MiddleNode>,
    pub scope_id: ScopeId,
    pub label: Option<Ustr>,
}

#[derive(Clone, Debug, PartialEq, Builder)]
pub struct MirReturn {
    pub value: Option<Box<MiddleNode>>,
}

#[derive(Clone, Debug, PartialEq, Builder)]
pub struct MirEmit {
    pub value: Box<MiddleNode>,
}

#[derive(Clone, Debug, PartialEq, Builder)]
pub struct MirField {
    pub base: Box<MiddleNode>,
    pub field: Ustr,
}

#[derive(Clone, Debug, PartialEq, Builder)]
pub struct MirIndex {
    pub base: Box<MiddleNode>,
    pub index: Box<MiddleNode>,
}

#[derive(Clone, Debug, PartialEq, Builder)]
pub struct MirCall {
    pub caller: Box<MiddleNode>,
    pub args: Vec<MiddleNode>,
}

#[derive(Clone, Debug, PartialEq, Builder)]
pub struct MirAssignment {
    pub identifier: Box<MiddleNode>,
    pub value: Box<MiddleNode>,
}

#[derive(Clone, Debug, PartialEq, Builder)]
pub struct MirDebug {
    pub pretty_printed_str: Ustr,
    pub value: Box<MiddleNode>,
}

#[derive(Clone, Debug, PartialEq, Builder)]
pub struct MirAggregate {
    pub identifier: Option<Ustr>,
    pub value: ObjectMap<MiddleNode>,
}

#[derive(Clone, Debug, PartialEq, Builder)]
pub struct MirEnum {
    pub identifier: Ustr,
    pub value: Ustr,
    pub data: Option<Box<MiddleNode>>,
}

#[derive(Clone, Debug, PartialEq, Builder)]
pub struct MirVarDecl {
    pub var_type: VarType,
    pub identifier: Ustr,
    pub value: Box<MiddleNode>,
    pub data_type: ParserDataType,
}

#[derive(Clone, Debug, PartialEq, Builder)]
pub struct MirScopeDecl {
    pub body: Vec<MiddleNode>,
    pub create_new_scope: bool,
    pub is_temp: bool,
    pub scope_id: ScopeId,
}

#[derive(Clone, Debug, PartialEq, Builder)]
pub struct MirFunction {
    pub parameters: Vec<(Ustr, ParserDataType, Option<Box<MiddleNode>>)>,
    pub body: Box<MiddleNode>,
    pub return_type: ParserDataType,
    pub scope_id: ScopeId,
}

#[derive(Clone, Debug, PartialEq, Builder)]
pub struct MirExtern {
    pub abi: Ustr,
    pub library: Ustr,
    pub symbol: Ustr,
    pub parameters: Vec<ParserDataType>,
    pub return_type: ParserDataType,
}

#[repr(u8)]
#[derive(Clone, Debug, PartialEq)]
pub enum MiddleNodeType {
    EmptyLine,
    Null,

    Break(MirBreak),
    Continue(MirContinue),
    Conditional(MirConditional),
    RangeDeclaration(MirRange),
    LoopDeclaration(MirLoop),
    Return(MirReturn),
    Emit(MirEmit),

    RefStatement(MirRef),
    Drop(MirDrop),
    Move(MirMove),
    Spawn(MirSpawn),
    DerefStatement(MirDeref),

    Identifier(MirIdentifier),
    StringLiteral(MirString),
    ListLiteral(MirList),
    CharLiteral(MirChar),
    FloatLiteral(MirFloat),
    IntLiteral(MirInt),
    BigLiteral(MirBig),

    BinaryExpression(MirBinary),
    ComparisonExpression(MirComparison),
    BooleanExpression(MirBoolean),
    NegExpression(MirNeg),
    AsExpression(MirAs),
    IsExpression(MirIs),

    FieldAccess(MirField),
    IndexAccess(MirIndex),
    CallExpression(MirCall),

    AssignmentExpression(MirAssignment),
    DebugExpression(MirDebug),
    AggregateExpression(MirAggregate),
    EnumExpression(MirEnum),

    VariableDeclaration(MirVarDecl),
    ScopeDeclaration(MirScopeDecl),
    FunctionDeclaration(MirFunction),
    ExternFunction(MirExtern),
}

impl MiddleNodeType {
    #[inline]
    pub fn is_simple_function_fallback(&self) -> bool {
        matches!(
            self,
            MiddleNodeType::Identifier(_)
                | MiddleNodeType::IntLiteral { .. }
                | MiddleNodeType::FloatLiteral(_)
                | MiddleNodeType::BigLiteral(_)
                | MiddleNodeType::StringLiteral(_)
                | MiddleNodeType::CharLiteral(_)
                | MiddleNodeType::Null
                | MiddleNodeType::FieldAccess { .. }
                | MiddleNodeType::IndexAccess { .. }
                | MiddleNodeType::AggregateExpression { .. }
                | MiddleNodeType::ListLiteral(_)
                | MiddleNodeType::RangeDeclaration { .. }
        )
    }
}

impl Display for MiddleNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.node_type)
    }
}

impl Display for MiddleNodeType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let converted: AstNodeType = self.clone().into();
        write!(f, "{}", converted)
    }
}

impl From<MiddleNode> for AstNode {
    fn from(val: MiddleNode) -> AstNode {
        AstNode {
            node_type: val.node_type.into(),
            span: val.span,
        }
    }
}

impl From<MiddleNodeType> for AstNodeType {
    fn from(val: MiddleNodeType) -> Self {
        match val {
            MiddleNodeType::Emit(value) => {
                AstNodeType::Emit(EmitType::Scope(Box::new((*value.value).into())))
            }
            MiddleNodeType::Spawn(value) => AstNodeType::Spawn {
                items: vec![(*value.value).into()],
                auto_wait: false,
            },
            MiddleNodeType::Drop(value) => AstNodeType::Drop(value.identifier.into()),
            MiddleNodeType::Move(value) => AstNodeType::MoveExpression {
                value: Box::new(AstNode::new(
                    Span::default(),
                    AstNodeType::Identifier(value.identifier.into()),
                )),
            },
            MiddleNodeType::Break(value) => AstNodeType::Break {
                label: value.label.map(Into::into),
                value: value.value.map(|v| Box::new((*v).into())),
            },
            MiddleNodeType::Continue(value) => AstNodeType::Continue {
                label: value.label.map(Into::into),
            },
            MiddleNodeType::EmptyLine => AstNodeType::EmptyLine,
            MiddleNodeType::Null => AstNodeType::Null,
            MiddleNodeType::RefStatement(value) => AstNodeType::RefStatement {
                mutability: value.mutability,
                value: Box::new((*value.value).into()),
            },
            MiddleNodeType::DerefStatement(value) => AstNodeType::DerefStatement {
                value: Box::new((*value.value).into()),
            },
            MiddleNodeType::VariableDeclaration(value) => AstNodeType::VariableDeclaration {
                var_type: value.var_type,
                identifier: value.identifier.into(),
                value: Box::new((*value.value).into()),
                data_type: value.data_type,
            },
            MiddleNodeType::EnumExpression(value) => AstNodeType::EnumExpression {
                identifier: value.identifier.into(),
                value: value.value.into(),
                data: value.data.map(|data| Box::new((*data).into())),
            },
            MiddleNodeType::ScopeDeclaration(value) => AstNodeType::ScopeDeclaration {
                body: {
                    let mut lst = Vec::new();

                    for node in value.body {
                        lst.push(node.into());
                    }

                    Some(lst)
                },
                named: None,
                is_temp: value.is_temp,
                create_new_scope: Some(value.create_new_scope),
                define: false,
            },
            MiddleNodeType::FunctionDeclaration(value) => AstNodeType::FunctionDeclaration {
                header: FunctionHeader {
                    generics: GenericTypes::default(),
                    parameters: {
                        let mut lst = Vec::new();

                        for param in value.parameters {
                            lst.push((
                                param.0.into(),
                                Some(param.1),
                                param.2.map(|x| Box::new((*x).into())),
                            ));
                        }
                        lst
                    },
                    return_type: value.return_type,
                    param_destructures: Vec::new(),
                },
                body: Box::new((*value.body).into()),
            },
            MiddleNodeType::ExternFunction(value) => AstNodeType::ExternFunctionDeclaration {
                abi: value.abi.to_string(),
                identifier: ParserText::from(value.symbol).into(),
                parameters: value.parameters,
                return_type: value.return_type,
                library: value.library.to_string(),
                symbol: None,
            },
            MiddleNodeType::AssignmentExpression(value) => AstNodeType::AssignmentExpression {
                identifier: Box::new((*value.identifier).into()),
                value: Box::new((*value.value).into()),
            },
            MiddleNodeType::DebugExpression(value) => AstNodeType::DebugExpression {
                value: Box::new((*value.value).into()),
            },
            MiddleNodeType::NegExpression(value) => AstNodeType::NotExpression {
                value: Box::new((*value.value).into()),
            },
            MiddleNodeType::AsExpression(value) => AstNodeType::AsExpression {
                value: Box::new((*value.value).into()),
                data_type: value.data_type,
                failure_mode: value.failure_mode,
            },
            MiddleNodeType::IsExpression(value) => AstNodeType::IsExpression {
                value: Box::new((*value.value).into()),
                data_type: value.data_type,
            },
            MiddleNodeType::Conditional(value) => AstNodeType::IfStatement {
                comparison: Box::new(IfComparisonType::If((*value.comparison).into())),
                then: Box::new((*value.then).into()),
                otherwise: value
                    .otherwise
                    .map(|otherwise| Box::new((*otherwise).into())),
            },
            MiddleNodeType::RangeDeclaration(value) => AstNodeType::RangeDeclaration {
                from: Box::new((*value.from).into()),
                to: Box::new((*value.to).into()),
                inclusive: value.inclusive,
            },
            MiddleNodeType::LoopDeclaration(value) => AstNodeType::ScopeDeclaration {
                body: {
                    let mut lst = Vec::new();

                    if let Some(state) = value.state {
                        lst.push((*state).into());
                    }

                    lst.push(AstNode::new(
                        value.body.span,
                        AstNodeType::LoopDeclaration {
                            loop_type: Box::new(LoopType::Loop),
                            body: Box::new((*value.body).into()),
                            until: None,
                            label: value.label.map(Into::into),
                            else_body: None,
                        },
                    ));

                    Some(lst)
                },
                named: None,
                is_temp: true,
                create_new_scope: Some(false),
                define: false,
            },
            MiddleNodeType::Return(value) => AstNodeType::Return {
                value: value.value.map(|x| Box::new((*x).into())),
            },
            MiddleNodeType::Identifier(value) => AstNodeType::Identifier(value.identifier.into()),
            MiddleNodeType::StringLiteral(value) => {
                AstNodeType::StringLiteral(ParserText::from(value.value))
            }
            MiddleNodeType::ListLiteral(value) => AstNodeType::ListLiteral(value.data_type, {
                let mut lst = Vec::new();

                for node in value.values {
                    lst.push(node.into());
                }

                lst
            }),
            MiddleNodeType::CharLiteral(value) => AstNodeType::CharLiteral(value.value),
            MiddleNodeType::FloatLiteral(value) => AstNodeType::FloatLiteral(value.value),
            MiddleNodeType::BigLiteral(value) => {
                AstNodeType::BigLiteral(ParserText::from(value.value))
            }
            MiddleNodeType::IntLiteral(value) => {
                let mut out = value.value.value.to_string();
                match value.value.int_type {
                    IntLiteralType::Int => {}
                    IntLiteralType::UInt => out.push('u'),
                    IntLiteralType::Byte => out.push('b'),
                }
                AstNodeType::IntLiteral(ParserText::from(out))
            }
            MiddleNodeType::FieldAccess(value) => AstNodeType::FieldAccess {
                base: Box::new((*value.base).into()),
                field: value.field.into(),
            },
            MiddleNodeType::IndexAccess(value) => AstNodeType::IndexAccess {
                base: Box::new((*value.base).into()),
                index: Box::new((*value.index).into()),
            },
            MiddleNodeType::CallExpression(value) => AstNodeType::CallExpression {
                string_fn: None,
                generic_types: Vec::new(),
                caller: Box::new((*value.caller).into()),
                args: {
                    let mut lst = Vec::new();

                    for arg in value.args {
                        lst.push(CallArg::Value(arg.into()));
                    }
                    lst
                },
                reverse_args: Vec::new(),
            },
            MiddleNodeType::BinaryExpression(value) => AstNodeType::BinaryExpression {
                left: Box::new((*value.left).into()),
                right: Box::new((*value.right).into()),
                operator: value.operator,
            },
            MiddleNodeType::ComparisonExpression(value) => AstNodeType::ComparisonExpression {
                left: Box::new((*value.left).into()),
                right: Box::new((*value.right).into()),
                operator: value.operator,
            },
            MiddleNodeType::BooleanExpression(value) => AstNodeType::BooleanExpression {
                left: Box::new((*value.left).into()),
                right: Box::new((*value.right).into()),
                operator: value.operator,
            },
            MiddleNodeType::AggregateExpression(value) => {
                let is_tuple = if value.value.is_empty() {
                    true
                } else {
                    value.value.contains_key("0")
                };
                if is_tuple {
                    let caller_span = Span::default();

                    AstNodeType::CallExpression {
                        string_fn: None,
                        generic_types: Vec::new(),
                        caller: Box::new(AstNode::identifier(
                            caller_span,
                            if let Some(identifier) = value.identifier {
                                identifier.to_string()
                            } else {
                                String::from("tuple")
                            },
                        )),
                        args: {
                            let mut lst = Vec::new();
                            let mut value: Vec<(String, MiddleNode)> =
                                value.value.0.into_iter().collect();
                            value.sort_by(|a, b| a.0.cmp(&b.0));
                            for arg in value {
                                lst.push(CallArg::Value(arg.1.into()));
                            }
                            lst
                        },
                        reverse_args: Vec::new(),
                    }
                } else {
                    AstNodeType::StructLiteral {
                        identifier: PotentialGenericTypeIdentifier::new(
                            Span::default(),
                            value
                                .identifier
                                .map(|x| x.to_string())
                                .unwrap_or_else(|| "map".to_string()),
                        ),
                        value: ObjectType::Map(
                            value
                                .value
                                .0
                                .into_iter()
                                .map(|x| (x.0, x.1.into()))
                                .collect(),
                        ),
                    }
                }
            }
        }
    }
}
