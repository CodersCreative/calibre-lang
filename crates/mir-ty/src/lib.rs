use std::{collections::HashMap, fmt::Display};

use calibre_parser::{
    ast::{
        CompStage, IfComparisonType, LoopType, Node, NodeType, ObjectMap, ObjectType,
        ParserDataType, ParserText, RefMutability, VarType,
        binary::BinaryOperator,
        comparison::{BooleanOperation, Comparison},
    },
    lexer::Span,
};
pub mod identifiers;

#[derive(Debug, Clone, PartialEq)]
pub struct MiddleNode {
    pub node_type: MiddleNodeType,
    pub span: Span,
}

impl MiddleNode {
    pub fn new(node_type: MiddleNodeType, span: Span) -> Self {
        Self { node_type, span }
    }

    pub fn new_from_type(node_type: MiddleNodeType) -> Self {
        Self {
            node_type,
            span: Span::default(),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum MiddleNodeType {
    Break,
    Continue,
    EmptyLine,
    RefStatement {
        mutability: RefMutability,
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
    },
    FunctionDeclaration {
        parameters: Vec<(ParserText, ParserDataType, Option<MiddleNode>)>,
        body: Box<MiddleNode>,
        return_type: ParserDataType,
        is_async: bool,
    },
    AssignmentExpression {
        identifier: Box<MiddleNode>,
        value: Box<MiddleNode>,
    },
    NotExpression {
        value: Box<MiddleNode>,
    },
    Comp {
        stage: CompStage,
        body: Box<MiddleNode>,
    },
    DebugExpression {
        pretty_printed_str: String,
        value: Box<MiddleNode>,
    },
    DataType {
        data_type: ParserDataType,
    },
    NegExpression {
        value: Box<MiddleNode>,
    },
    AsExpression {
        value: Box<MiddleNode>,
        data_type: ParserDataType,
    },
    InDeclaration {
        identifier: Box<MiddleNode>,
        value: Box<MiddleNode>,
    },
    IsDeclaration {
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
    },
    Return {
        value: Option<Box<MiddleNode>>,
    },
    Identifier(ParserText),
    StringLiteral(ParserText),
    ListLiteral(ParserDataType, Vec<MiddleNode>),
    CharLiteral(char),
    FloatLiteral(f64),
    IntLiteral(i64),
    MemberExpression {
        path: Vec<(MiddleNode, bool)>,
    },
    CallExpression(Box<MiddleNode>, Vec<(MiddleNode, Option<MiddleNode>)>),
    BinaryExpression {
        left: Box<MiddleNode>,
        right: Box<MiddleNode>,
        operator: BinaryOperator,
    },
    ComparisonExpression {
        left: Box<MiddleNode>,
        right: Box<MiddleNode>,
        operator: Comparison,
    },
    BooleanExpression {
        left: Box<MiddleNode>,
        right: Box<MiddleNode>,
        operator: BooleanOperation,
    },
    AggregateExpression {
        identifier: Option<ParserText>,
        value: ObjectMap<MiddleNode>,
    },
    IfStatement {
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
            Self::Break => NodeType::Break,
            Self::Continue => NodeType::Continue,
            Self::EmptyLine => NodeType::EmptyLine,
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
                data_type: Some(data_type.into()),
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
                create_new_scope,
                define: false,
            },
            Self::FunctionDeclaration {
                parameters,
                body,
                return_type,
                is_async,
            } => NodeType::FunctionDeclaration {
                parameters: {
                    let mut lst = Vec::new();

                    for param in parameters {
                        lst.push((
                            param.0.into(),
                            param.1.into(),
                            if let Some(x) = param.2 {
                                Some(x.into())
                            } else {
                                None
                            },
                        ));
                    }
                    lst
                },
                body: Box::new((*body).into()),
                return_type: return_type.into(),
                is_async,
            },
            Self::AssignmentExpression { identifier, value } => NodeType::AssignmentExpression {
                identifier: Box::new((*identifier).into()),
                value: Box::new((*value).into()),
            },
            Self::NotExpression { value } => NodeType::NotExpression {
                value: Box::new((*value).into()),
            },
            Self::DebugExpression {
                pretty_printed_str: _,
                value,
            } => NodeType::DebugExpression {
                value: Box::new((*value).into()),
            },
            Self::DataType { data_type } => NodeType::DataType {
                data_type: data_type.into(),
            },
            Self::NegExpression { value } => NodeType::NotExpression {
                value: Box::new((*value).into()),
            },
            Self::AsExpression { value, data_type } => NodeType::AsExpression {
                value: Box::new((*value).into()),
                data_type: data_type.into(),
            },
            Self::IfStatement {
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
            Self::InDeclaration { identifier, value } => NodeType::InDeclaration {
                identifier: Box::new((*identifier).into()),
                value: Box::new((*value).into()),
            },
            Self::IsDeclaration { value, data_type } => NodeType::IsDeclaration {
                value: Box::new((*value).into()),
                data_type: data_type.into(),
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
            Self::LoopDeclaration { state, body } => NodeType::ScopeDeclaration {
                body: {
                    let mut lst = Vec::new();

                    if let Some(state) = state {
                        lst.push((*state).into());
                    }

                    lst.push(Node::new_from_type(NodeType::LoopDeclaration {
                        loop_type: Box::new(LoopType::Loop),
                        body: Box::new((*body).into()),
                    }));

                    Some(lst)
                },
                named: None,
                is_temp: true,
                create_new_scope: false,
                define: false,
            },
            Self::Return { value: Some(value) } => NodeType::Return {
                value: Some(Box::new((*value).into())),
            },
            Self::Return { value: None } => NodeType::Return { value: None },
            Self::Identifier(x) => NodeType::Identifier(x.into()),
            Self::StringLiteral(x) => NodeType::StringLiteral(x),
            Self::ListLiteral(typ, data) => NodeType::ListLiteral(Some(typ.into()), {
                let mut lst = Vec::new();

                for node in data {
                    lst.push(node.into());
                }

                lst
            }),
            Self::CharLiteral(x) => NodeType::CharLiteral(x),
            Self::FloatLiteral(x) => NodeType::FloatLiteral(x),
            Self::IntLiteral(x) => NodeType::IntLiteral(x),
            Self::MemberExpression { path } => NodeType::MemberExpression {
                path: {
                    let mut lst = Vec::new();

                    for node in path {
                        lst.push((node.0.into(), node.1));
                    }

                    lst
                },
            },
            Self::CallExpression(caller, args) => {
                NodeType::CallExpression(Box::new((*caller).into()), {
                    let mut lst = Vec::new();

                    for arg in args {
                        lst.push((
                            arg.0.into(),
                            if let Some(a) = arg.1 {
                                Some(a.into())
                            } else {
                                None
                            },
                        ));
                    }
                    lst
                })
            }
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
            Self::Comp { stage, body } => NodeType::Comp {
                stage,
                body: Box::new((*body).into()),
            },
            Self::AggregateExpression { identifier, value } => {
                let is_tuple = if value.is_empty() {
                    true
                } else {
                    value.0.contains_key("0")
                };
                if is_tuple {
                    NodeType::CallExpression(
                        Box::new(Node::new_from_type(NodeType::Identifier(
                            if let Some(identifier) = identifier {
                                identifier
                            } else {
                                ParserText::from(String::from("tuple"))
                            }
                            .into(),
                        ))),
                        {
                            let mut lst = Vec::new();
                            let mut value: Vec<(String, MiddleNode)> =
                                value.0.into_iter().collect();
                            value.sort_by(|a, b| a.0.cmp(&b.0));
                            for arg in value {
                                lst.push((arg.1.into(), None));
                            }
                            lst
                        },
                    )
                } else {
                    NodeType::StructLiteral {
                        identifier: identifier.unwrap().into(),
                        value: ObjectType::Map({
                            let mut map = HashMap::new();

                            for (k, v) in value.0 {
                                map.insert(k, v.into());
                            }

                            map
                        }),
                    }
                }
            }
        }
    }
}
