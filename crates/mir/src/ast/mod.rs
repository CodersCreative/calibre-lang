use calibre_parser::{
    Span,
    ast::{
        ObjectMap, ObjectType, RefMutability,
        binary::BinaryOperator,
        comparison::{BooleanOperator, ComparisonOperator},
        idents::{IntLiteralType, ParsedIntLiteral, ParserText},
        nodes::{
            AsFailureMode, CallArg, EmitType, FunctionHeader, IfComparisonType, LoopType, Node,
            NodeType, VarType,
        },
        types::{GenericTypes, ParserDataType},
    },
};
use rustc_hash::FxHashMap;
use std::fmt::Display;

use crate::errors::MiddleErr;

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
        Self::new(MiddleNodeType::Identifier(text.to_string().into()), span)
    }

    pub fn member_field(&self) -> Result<Box<str>, MiddleErr> {
        Ok(match &self.node_type {
            MiddleNodeType::Identifier(name) => name.text.clone().into_boxed_str(),
            MiddleNodeType::IntLiteral(ParsedIntLiteral { value, int_type }) => match int_type {
                IntLiteralType::Int => value.to_string().into_boxed_str(),
                IntLiteralType::UInt => format!("{value}u").into_boxed_str(),
                IntLiteralType::Byte => format!("{value}b").into_boxed_str(),
            },
            MiddleNodeType::FloatLiteral(x) => x.to_string().into_boxed_str(),
            _ => return Err(MiddleErr::InvalidMember),
        })
    }

    pub fn len(&self) -> usize {
        let mut count = 1;
        match &self.node_type {
            MiddleNodeType::AssignmentExpression { identifier, value } => {
                count += identifier.len();
                count += value.len();
            }
            MiddleNodeType::CallExpression { caller, args } => {
                count += caller.len();
                for a in args {
                    count += a.len();
                }
            }
            MiddleNodeType::BinaryExpression { left, right, .. }
            | MiddleNodeType::ComparisonExpression { left, right, .. }
            | MiddleNodeType::BooleanExpression { left, right, .. }
            | MiddleNodeType::IndexAccess {
                base: left,
                index: right,
            }
            | MiddleNodeType::RangeDeclaration {
                from: left,
                to: right,
                ..
            } => {
                count += left.len();
                count += right.len();
            }
            MiddleNodeType::AsExpression { value, .. }
            | MiddleNodeType::FieldAccess { base: value, .. }
            | MiddleNodeType::ScopeAccess { base: value, .. }
            | MiddleNodeType::IsExpression { value, .. }
            | MiddleNodeType::NegExpression { value }
            | MiddleNodeType::RefStatement { value, .. }
            | MiddleNodeType::DerefStatement { value }
            | MiddleNodeType::DebugExpression { value, .. }
            | MiddleNodeType::Return { value: Some(value) }
            | MiddleNodeType::EnumExpression {
                data: Some(value), ..
            }
            | MiddleNodeType::VariableDeclaration { value, .. } => count += value.len(),
            MiddleNodeType::ListLiteral(_, values)
            | MiddleNodeType::ScopeDeclaration { body: values, .. } => {
                for v in values {
                    count += v.len();
                }
            }
            MiddleNodeType::LoopDeclaration { state, body, .. } => {
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

    pub fn substitute(&mut self, repl: &FxHashMap<String, MiddleNode>) {
        match &mut self.node_type {
            MiddleNodeType::Identifier(id) => {
                if let Some(replacement) = repl.get(&id.text) {
                    *self = replacement.clone();
                }
            }
            MiddleNodeType::FunctionDeclaration { .. } => {}
            MiddleNodeType::ScopeDeclaration { body, .. } => {
                for stmt in body {
                    stmt.substitute(repl);
                }
            }
            MiddleNodeType::AssignmentExpression { identifier, value } => {
                identifier.substitute(repl);
                value.substitute(repl);
            }
            MiddleNodeType::CallExpression { caller, args } => {
                caller.substitute(repl);
                for a in args.iter_mut() {
                    a.substitute(repl);
                }
            }
            MiddleNodeType::Return { value } => {
                if let Some(v) = value.as_mut() {
                    v.substitute(repl);
                }
            }
            MiddleNodeType::BinaryExpression { left, right, .. }
            | MiddleNodeType::ComparisonExpression { left, right, .. }
            | MiddleNodeType::BooleanExpression { left, right, .. }
            | MiddleNodeType::RangeDeclaration {
                from: left,
                to: right,
                ..
            } => {
                left.substitute(repl);
                right.substitute(repl);
            }
            MiddleNodeType::AsExpression { value, .. }
            | MiddleNodeType::IsExpression { value, .. }
            | MiddleNodeType::NegExpression { value }
            | MiddleNodeType::RefStatement { value, .. }
            | MiddleNodeType::DerefStatement { value }
            | MiddleNodeType::DebugExpression { value, .. }
            | MiddleNodeType::VariableDeclaration { value, .. } => value.substitute(repl),
            MiddleNodeType::ListLiteral(_, values) => {
                for v in values {
                    v.substitute(repl);
                }
            }
            MiddleNodeType::LoopDeclaration { state, body, .. } => {
                if let Some(s) = state.as_mut() {
                    s.substitute(repl);
                }
                body.substitute(repl);
            }
            MiddleNodeType::FieldAccess { base, .. } => base.substitute(repl),
            MiddleNodeType::ScopeAccess { base, .. } => base.substitute(repl),
            MiddleNodeType::IndexAccess { base, index } => {
                base.substitute(repl);
                index.substitute(repl);
            }
            MiddleNodeType::EnumExpression { data, .. } => {
                if let Some(d) = data.as_mut() {
                    d.substitute(repl);
                }
            }
            _ => {}
        }
    }

    pub fn calls_self(&self, name: &impl ToString) -> bool {
        match &self.node_type {
            MiddleNodeType::Identifier(id) => id.text == name.to_string(),
            MiddleNodeType::CallExpression { caller, args } => {
                if caller.calls_self(name) {
                    return true;
                }
                args.iter().any(|a| a.calls_self(name))
            }
            MiddleNodeType::FunctionDeclaration { .. } => false,
            MiddleNodeType::ScopeDeclaration { body, .. } => {
                body.iter().any(|n| n.calls_self(name))
            }
            MiddleNodeType::Return { value } => value.as_ref().is_some_and(|v| v.calls_self(name)),
            MiddleNodeType::AssignmentExpression { identifier, value } => {
                identifier.calls_self(name) || value.calls_self(name)
            }
            MiddleNodeType::BinaryExpression { left, right, .. }
            | MiddleNodeType::ComparisonExpression { left, right, .. }
            | MiddleNodeType::BooleanExpression { left, right, .. }
            | MiddleNodeType::RangeDeclaration {
                from: left,
                to: right,
                ..
            } => left.calls_self(name) || right.calls_self(name),
            MiddleNodeType::AsExpression { value, .. }
            | MiddleNodeType::IsExpression { value, .. }
            | MiddleNodeType::NegExpression { value }
            | MiddleNodeType::RefStatement { value, .. }
            | MiddleNodeType::DerefStatement { value }
            | MiddleNodeType::DebugExpression { value, .. }
            | MiddleNodeType::VariableDeclaration { value, .. } => value.calls_self(name),
            MiddleNodeType::ListLiteral(_, values) => values.iter().any(|v| v.calls_self(name)),
            MiddleNodeType::LoopDeclaration { state, body, .. } => {
                state.as_ref().is_some_and(|s| s.calls_self(name)) || body.calls_self(name)
            }
            MiddleNodeType::FieldAccess { base, .. } => base.calls_self(name),
            MiddleNodeType::ScopeAccess { base, .. } => base.calls_self(name),
            MiddleNodeType::IndexAccess { base, index } => {
                base.calls_self(name) || index.calls_self(name)
            }
            MiddleNodeType::EnumExpression { data, .. } => {
                data.as_ref().is_some_and(|d| d.calls_self(name))
            }
            _ => false,
        }
    }
}

#[repr(u8)]
#[derive(Clone, Debug, PartialEq)]
pub enum MiddleNodeType {
    Break {
        label: Option<ParserText>,
        value: Option<Box<MiddleNode>>,
    },
    Continue {
        label: Option<ParserText>,
    },
    EmptyLine,
    Null,
    RefStatement {
        mutability: RefMutability,
        value: Box<MiddleNode>,
    },
    Drop(ParserText),
    Move(ParserText),
    Spawn {
        value: Box<MiddleNode>,
    },
    DerefStatement {
        value: Box<MiddleNode>,
    },
    VariableDeclaration {
        var_type: VarType,
        identifier: ParserText,
        value: Box<MiddleNode>,
        data_type: ParserDataType,
    },
    EnumExpression {
        identifier: ParserText,
        value: ParserText,
        data: Option<Box<MiddleNode>>,
    },
    ScopeDeclaration {
        body: Vec<MiddleNode>,
        create_new_scope: bool,
        is_temp: bool,
        scope_id: u64,
    },
    FunctionDeclaration {
        parameters: Vec<(ParserText, ParserDataType, Option<Box<MiddleNode>>)>,
        body: Box<MiddleNode>,
        return_type: ParserDataType,
        scope_id: u64,
    },
    ExternFunction {
        abi: String,
        library: String,
        symbol: String,
        parameters: Vec<ParserDataType>,
        return_type: ParserDataType,
    },
    AssignmentExpression {
        identifier: Box<MiddleNode>,
        value: Box<MiddleNode>,
    },
    DebugExpression {
        pretty_printed_str: String,
        value: Box<MiddleNode>,
    },
    Emit {
        value: Box<MiddleNode>,
    },
    NegExpression {
        value: Box<MiddleNode>,
    },
    AsExpression {
        value: Box<MiddleNode>,
        data_type: ParserDataType,
        failure_mode: AsFailureMode,
    },
    IsExpression {
        value: Box<MiddleNode>,
        data_type: ParserDataType,
    },
    RangeDeclaration {
        from: Box<MiddleNode>,
        to: Box<MiddleNode>,
        inclusive: bool,
    },
    LoopDeclaration {
        state: Option<Box<MiddleNode>>,
        body: Box<MiddleNode>,
        scope_id: u64,
        label: Option<ParserText>,
    },
    Return {
        value: Option<Box<MiddleNode>>,
    },
    Identifier(ParserText),
    StringLiteral(ParserText),
    ListLiteral(ParserDataType, Vec<MiddleNode>),
    CharLiteral(char),
    FloatLiteral(f64),
    IntLiteral(ParsedIntLiteral),
    BigLiteral(ParserText),
    FieldAccess {
        base: Box<MiddleNode>,
        field: ParserText,
    },
    ScopeAccess {
        base: Box<MiddleNode>,
        field: ParserText,
    },
    IndexAccess {
        base: Box<MiddleNode>,
        index: Box<MiddleNode>,
    },
    CallExpression {
        caller: Box<MiddleNode>,
        args: Vec<MiddleNode>,
    },
    BinaryExpression {
        left: Box<MiddleNode>,
        right: Box<MiddleNode>,
        operator: BinaryOperator,
    },
    ComparisonExpression {
        left: Box<MiddleNode>,
        right: Box<MiddleNode>,
        operator: ComparisonOperator,
    },
    BooleanExpression {
        left: Box<MiddleNode>,
        right: Box<MiddleNode>,
        operator: BooleanOperator,
    },
    AggregateExpression {
        identifier: Option<ParserText>,
        value: ObjectMap<MiddleNode>,
    },
    Conditional {
        comparison: Box<MiddleNode>,
        then: Box<MiddleNode>,
        otherwise: Option<Box<MiddleNode>>,
    },
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
                | MiddleNodeType::ScopeAccess { .. }
                | MiddleNodeType::IndexAccess { .. }
                | MiddleNodeType::AggregateExpression { .. }
                | MiddleNodeType::ListLiteral(_, _)
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
        let converted: NodeType = self.clone().into();
        write!(f, "{}", converted)
    }
}

impl From<MiddleNode> for Node {
    fn from(val: MiddleNode) -> Node {
        Node {
            node_type: val.node_type.into(),
            span: val.span,
        }
    }
}

impl From<MiddleNodeType> for NodeType {
    fn from(val: MiddleNodeType) -> Self {
        match val {
            MiddleNodeType::Emit { value } => {
                NodeType::Emit(EmitType::Scope(Box::new((*value).into())))
            }
            MiddleNodeType::Spawn { value } => NodeType::Spawn {
                items: vec![(*value).into()],
                auto_wait: false,
            },
            MiddleNodeType::Drop(x) => NodeType::Drop(x.into()),
            MiddleNodeType::Move(x) => NodeType::MoveExpression {
                value: Box::new(Node::new(x.span, NodeType::Identifier(x.into()))),
            },
            MiddleNodeType::Break { label, value } => NodeType::Break {
                label: label.map(Into::into),
                value: value.map(|v| Box::new((*v).into())),
            },
            MiddleNodeType::Continue { label } => NodeType::Continue {
                label: label.map(Into::into),
            },
            MiddleNodeType::EmptyLine => NodeType::EmptyLine,
            MiddleNodeType::Null => NodeType::Null,
            MiddleNodeType::RefStatement { mutability, value } => NodeType::RefStatement {
                mutability,
                value: Box::new((*value).into()),
            },
            MiddleNodeType::DerefStatement { value } => NodeType::DerefStatement {
                value: Box::new((*value).into()),
            },
            MiddleNodeType::VariableDeclaration {
                var_type,
                identifier,
                value,
                data_type,
            } => NodeType::VariableDeclaration {
                var_type,
                identifier: identifier.into(),
                value: Box::new((*value).into()),
                data_type,
            },
            MiddleNodeType::EnumExpression {
                identifier,
                value,
                data,
            } => NodeType::EnumExpression {
                identifier: identifier.into(),
                value: value.into(),
                data: data.map(|data| Box::new((*data).into())),
            },
            MiddleNodeType::ScopeDeclaration {
                body,
                create_new_scope,
                is_temp,
                scope_id: _,
            } => NodeType::ScopeDeclaration {
                body: {
                    let mut lst = Vec::new();

                    for node in body {
                        lst.push(node.into());
                    }

                    Some(lst)
                },
                named: None,
                is_temp,
                create_new_scope: Some(create_new_scope),
                define: false,
            },
            MiddleNodeType::FunctionDeclaration {
                parameters,
                body,
                return_type,
                scope_id: _,
            } => NodeType::FunctionDeclaration {
                header: FunctionHeader {
                    generics: GenericTypes::default(),
                    parameters: {
                        let mut lst = Vec::new();

                        for param in parameters {
                            lst.push((
                                param.0.into(),
                                Some(param.1),
                                param.2.map(|x| Box::new((*x).into())),
                            ));
                        }
                        lst
                    },
                    return_type,
                    param_destructures: Vec::new(),
                },
                body: Box::new((*body).into()),
            },
            MiddleNodeType::ExternFunction {
                abi,
                library,
                symbol,
                parameters,
                return_type,
            } => NodeType::ExternFunctionDeclaration {
                abi,
                identifier: ParserText::from(symbol).into(),
                parameters,
                return_type,
                library,
                symbol: None,
            },
            MiddleNodeType::AssignmentExpression { identifier, value } => {
                NodeType::AssignmentExpression {
                    identifier: Box::new((*identifier).into()),
                    value: Box::new((*value).into()),
                }
            }
            MiddleNodeType::DebugExpression {
                pretty_printed_str: _,
                value,
            } => NodeType::DebugExpression {
                value: Box::new((*value).into()),
            },
            MiddleNodeType::NegExpression { value } => NodeType::NotExpression {
                value: Box::new((*value).into()),
            },
            MiddleNodeType::AsExpression {
                value,
                data_type,
                failure_mode,
            } => NodeType::AsExpression {
                value: Box::new((*value).into()),
                data_type,
                failure_mode,
            },
            MiddleNodeType::IsExpression { value, data_type } => NodeType::IsExpression {
                value: Box::new((*value).into()),
                data_type,
            },
            MiddleNodeType::Conditional {
                comparison,
                then,
                otherwise,
            } => NodeType::IfStatement {
                comparison: Box::new(IfComparisonType::If((*comparison).into())),
                then: Box::new((*then).into()),
                otherwise: otherwise.map(|otherwise| Box::new((*otherwise).into())),
            },
            MiddleNodeType::RangeDeclaration {
                from,
                to,
                inclusive,
            } => NodeType::RangeDeclaration {
                from: Box::new((*from).into()),
                to: Box::new((*to).into()),
                inclusive,
            },
            MiddleNodeType::LoopDeclaration {
                state,
                body,
                scope_id: _,
                label,
            } => NodeType::ScopeDeclaration {
                body: {
                    let mut lst = Vec::new();

                    if let Some(state) = state {
                        lst.push((*state).into());
                    }

                    lst.push(Node::new(
                        body.span,
                        NodeType::LoopDeclaration {
                            loop_type: Box::new(LoopType::Loop),
                            body: Box::new((*body).into()),
                            until: None,
                            label: label.map(Into::into),
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
            MiddleNodeType::Return { value: Some(value) } => NodeType::Return {
                value: Some(Box::new((*value).into())),
            },
            MiddleNodeType::Return { value: None } => NodeType::Return { value: None },
            MiddleNodeType::Identifier(x) => NodeType::Identifier(x.into()),
            MiddleNodeType::StringLiteral(x) => NodeType::StringLiteral(x),
            MiddleNodeType::ListLiteral(typ, data) => NodeType::ListLiteral(typ, {
                let mut lst = Vec::new();

                for node in data {
                    lst.push(node.into());
                }

                lst
            }),
            MiddleNodeType::CharLiteral(x) => NodeType::CharLiteral(x),
            MiddleNodeType::FloatLiteral(x) => NodeType::FloatLiteral(x),
            MiddleNodeType::BigLiteral(x) => NodeType::BigLiteral(x),
            MiddleNodeType::IntLiteral(x) => {
                let mut out = x.value.to_string();
                match x.int_type {
                    IntLiteralType::Int => {}
                    IntLiteralType::UInt => out.push('u'),
                    IntLiteralType::Byte => out.push('b'),
                }
                NodeType::IntLiteral(ParserText::from(out))
            }
            MiddleNodeType::FieldAccess { base, field } => NodeType::FieldAccess {
                base: Box::new((*base).into()),
                field: field.into(),
            },
            MiddleNodeType::ScopeAccess { base, field } => NodeType::ScopeAccess {
                base: Box::new((*base).into()),
                field: field.into(),
            },
            MiddleNodeType::IndexAccess { base, index } => NodeType::IndexAccess {
                base: Box::new((*base).into()),
                index: Box::new((*index).into()),
            },
            MiddleNodeType::CallExpression { caller, args } => NodeType::CallExpression {
                string_fn: None,
                generic_types: Vec::new(),
                caller: Box::new((*caller).into()),
                args: {
                    let mut lst = Vec::new();

                    for arg in args {
                        lst.push(CallArg::Value(arg.into()));
                    }
                    lst
                },
                reverse_args: Vec::new(),
            },
            MiddleNodeType::BinaryExpression {
                left,
                right,
                operator,
            } => NodeType::BinaryExpression {
                left: Box::new((*left).into()),
                right: Box::new((*right).into()),
                operator,
            },
            MiddleNodeType::ComparisonExpression {
                left,
                right,
                operator,
            } => NodeType::ComparisonExpression {
                left: Box::new((*left).into()),
                right: Box::new((*right).into()),
                operator,
            },
            MiddleNodeType::BooleanExpression {
                left,
                right,
                operator,
            } => NodeType::BooleanExpression {
                left: Box::new((*left).into()),
                right: Box::new((*right).into()),
                operator,
            },
            MiddleNodeType::AggregateExpression { identifier, value } => {
                let is_tuple = if value.is_empty() {
                    true
                } else {
                    value.contains_key("0")
                };
                if is_tuple {
                    let caller_span = identifier
                        .as_ref()
                        .map(|id| id.span)
                        .or_else(|| value.0.first().map(|(_, node)| node.span))
                        .unwrap_or_default();
                    NodeType::CallExpression {
                        string_fn: None,
                        generic_types: Vec::new(),
                        caller: Box::new(Node::new(
                            caller_span,
                            NodeType::Identifier(
                                if let Some(identifier) = identifier {
                                    identifier
                                } else {
                                    ParserText::from(String::from("tuple"))
                                }
                                .into(),
                            ),
                        )),
                        args: {
                            let mut lst = Vec::new();
                            let mut value: Vec<(String, MiddleNode)> =
                                value.0.into_iter().collect();
                            value.sort_by(|a, b| a.0.cmp(&b.0));
                            for arg in value {
                                lst.push(CallArg::Value(arg.1.into()));
                            }
                            lst
                        },
                        reverse_args: Vec::new(),
                    }
                } else {
                    NodeType::StructLiteral {
                        identifier: identifier
                            .unwrap_or_else(|| ParserText::new(Default::default(), "map").into())
                            .into(),
                        value: ObjectType::Map(
                            value.0.into_iter().map(|x| (x.0, x.1.into())).collect(),
                        ),
                    }
                }
            }
        }
    }
}
