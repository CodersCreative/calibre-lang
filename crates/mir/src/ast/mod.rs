use calibre_parser::{
    Span,
    ast::{
        AsFailureMode, CallArg, FunctionHeader, GenericTypes, IfComparisonType, LoopType, Node,
        NodeType, ObjectMap, ObjectType, ParserDataType, ParserText, RefMutability, VarType,
        binary::BinaryOperator,
        comparison::{BooleanOperator, ComparisonOperator},
    },
};
use std::fmt::Display;

pub mod hm;
pub mod identifiers;
pub mod renaming;

#[derive(Debug, Clone, PartialEq)]
pub struct MiddleNode {
    pub node_type: MiddleNodeType,
    pub span: Span,
}

impl MiddleNode {
    pub fn new(node_type: MiddleNodeType, span: Span) -> Self {
        Self { node_type, span }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum IntLiteralType {
    Int,
    UInt,
    Byte,
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
        parameters: Vec<(ParserText, ParserDataType)>,
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
    NegExpression {
        value: Box<MiddleNode>,
    },
    AsExpression {
        value: Box<MiddleNode>,
        data_type: ParserDataType,
        failure_mode: AsFailureMode,
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
    IntLiteral {
        value: i64,
        int_type: IntLiteralType,
    },
    MemberExpression {
        path: Vec<(MiddleNode, bool)>,
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

impl Into<Node> for MiddleNode {
    fn into(self) -> Node {
        Node {
            node_type: self.node_type.into(),
            span: self.span,
        }
    }
}

impl Into<NodeType> for MiddleNodeType {
    fn into(self) -> NodeType {
        match self {
            Self::Spawn { value } => NodeType::Spawn {
                items: vec![(*value).into()],
            },
            Self::Drop(x) => NodeType::Drop(x.into()),
            Self::Move(x) => NodeType::MoveExpression {
                value: Box::new(Node::new(x.span, NodeType::Identifier(x.into()))),
            },
            Self::Break { label, value } => NodeType::Break {
                label: label.map(Into::into),
                value: value.map(|v| Box::new((*v).into())),
            },
            Self::Continue { label } => NodeType::Continue {
                label: label.map(Into::into),
            },
            Self::EmptyLine => NodeType::EmptyLine,
            Self::Null => NodeType::Null,
            Self::RefStatement { mutability, value } => NodeType::RefStatement {
                mutability,
                value: Box::new((*value).into()),
            },
            Self::DerefStatement { value } => NodeType::DerefStatement {
                value: Box::new((*value).into()),
            },
            Self::VariableDeclaration {
                var_type,
                identifier,
                value,
                data_type,
            } => NodeType::VariableDeclaration {
                var_type,
                identifier: identifier.into(),
                value: Box::new((*value).into()),
                data_type: data_type.into(),
            },
            Self::EnumExpression {
                identifier,
                value,
                data,
            } => NodeType::EnumExpression {
                identifier: identifier.into(),
                value: value.into(),
                data: if let Some(data) = data {
                    Some(Box::new((*data).into()))
                } else {
                    None
                },
            },
            Self::ScopeDeclaration {
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
            Self::FunctionDeclaration {
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
                            lst.push((param.0.into(), param.1.into()));
                        }
                        lst
                    },
                    return_type: return_type.into(),
                    param_destructures: Vec::new(),
                },
                body: Box::new((*body).into()),
            },
            Self::ExternFunction {
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
            Self::AssignmentExpression { identifier, value } => NodeType::AssignmentExpression {
                identifier: Box::new((*identifier).into()),
                value: Box::new((*value).into()),
            },
            Self::DebugExpression {
                pretty_printed_str: _,
                value,
            } => NodeType::DebugExpression {
                value: Box::new((*value).into()),
            },
            Self::NegExpression { value } => NodeType::NotExpression {
                value: Box::new((*value).into()),
            },
            Self::AsExpression {
                value,
                data_type,
                failure_mode,
            } => NodeType::AsExpression {
                value: Box::new((*value).into()),
                data_type: data_type.into(),
                failure_mode,
            },
            Self::Conditional {
                comparison,
                then,
                otherwise,
            } => NodeType::IfStatement {
                comparison: Box::new(IfComparisonType::If((*comparison).into())),
                then: Box::new((*then).into()),
                otherwise: if let Some(otherwise) = otherwise {
                    Some(Box::new((*otherwise).into()))
                } else {
                    None
                },
            },
            Self::RangeDeclaration {
                from,
                to,
                inclusive,
            } => NodeType::RangeDeclaration {
                from: Box::new((*from).into()),
                to: Box::new((*to).into()),
                inclusive,
            },
            Self::LoopDeclaration {
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
            Self::Return { value: Some(value) } => NodeType::Return {
                value: Some(Box::new((*value).into())),
            },
            Self::Return { value: None } => NodeType::Return { value: None },
            Self::Identifier(x) => NodeType::Identifier(x.into()),
            Self::StringLiteral(x) => NodeType::StringLiteral(x),
            Self::ListLiteral(typ, data) => NodeType::ListLiteral(typ.into(), {
                let mut lst = Vec::new();

                for node in data {
                    lst.push(node.into());
                }

                lst
            }),
            Self::CharLiteral(x) => NodeType::CharLiteral(x),
            Self::FloatLiteral(x) => NodeType::FloatLiteral(x),
            Self::IntLiteral { value, int_type } => {
                let mut out = value.to_string();
                match int_type {
                    IntLiteralType::Int => {}
                    IntLiteralType::UInt => out.push('u'),
                    IntLiteralType::Byte => out.push('b'),
                }
                NodeType::IntLiteral(out)
            }
            Self::MemberExpression { path } => NodeType::MemberExpression {
                path: {
                    let mut lst = Vec::new();

                    for node in path {
                        lst.push((node.0.into(), node.1));
                    }

                    lst
                },
            },
            Self::CallExpression { caller, args } => NodeType::CallExpression {
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
            Self::BinaryExpression {
                left,
                right,
                operator,
            } => NodeType::BinaryExpression {
                left: Box::new((*left).into()),
                right: Box::new((*right).into()),
                operator,
            },
            Self::ComparisonExpression {
                left,
                right,
                operator,
            } => NodeType::ComparisonExpression {
                left: Box::new((*left).into()),
                right: Box::new((*right).into()),
                operator,
            },
            Self::BooleanExpression {
                left,
                right,
                operator,
            } => NodeType::BooleanExpression {
                left: Box::new((*left).into()),
                right: Box::new((*right).into()),
                operator,
            },
            Self::AggregateExpression { identifier, value } => {
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
                            .unwrap_or_else(|| ParserText::from(String::from("map")).into())
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
