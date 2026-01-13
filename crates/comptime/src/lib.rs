use calibre_common::environment::InterpreterFrom;
use calibre_interpreter::runtime::{interpreter::InterpreterErr, scope::InterpreterEnvironment};
use calibre_mir::environment::MiddleEnvironment;
use calibre_mir_ty::{MiddleNode, MiddleNodeType};
use calibre_parser::ast::{CompStage, ObjectMap};
use std::collections::HashMap;

pub struct ComptimeEnvironment {
    pub changed_this_stage: usize,
    pub scope: u64,
    pub interpreter: InterpreterEnvironment,
}

impl ComptimeEnvironment {
    pub fn new(env: &MiddleEnvironment) -> Self {
        let mut interpreter = InterpreterEnvironment::new(env);
        let scope = interpreter.new_scope_with_stdlib(None);

        Self {
            changed_this_stage: 0,
            scope,
            interpreter,
        }
    }

    pub fn new_and_evaluate(
        node: MiddleNode,
        env: &MiddleEnvironment,
    ) -> Result<MiddleNode, InterpreterErr> {
        let mut env = Self::new(env);
        env.start_evaluate(node)
    }
}

impl ComptimeEnvironment {
    pub fn start_evaluate(&mut self, mut node: MiddleNode) -> Result<MiddleNode, InterpreterErr> {
        let mut stage = 0;
        let mut consecutive_no_changes = 0;
        let mut stop = false;

        loop {
            node = self.evaluate(node, stage)?;
            if self.changed_this_stage <= 0 {
                consecutive_no_changes += 1;
            } else {
                self.changed_this_stage = 0;
            }

            if stop {
                return Ok(node);
            } else if consecutive_no_changes >= 5 {
                stage = usize::MAX;
                stop = true;
            } else {
                stage += 1;
            }
        }
    }

    fn evaluate(&mut self, node: MiddleNode, stage: usize) -> Result<MiddleNode, InterpreterErr> {
        Ok(MiddleNode {
            span: node.span,
            node_type: match node.node_type {
                MiddleNodeType::RefStatement { mutability, value } => {
                    MiddleNodeType::RefStatement {
                        mutability,
                        value: Box::new(self.evaluate(*value, stage)?),
                    }
                }
                MiddleNodeType::DerefStatement { value } => MiddleNodeType::DerefStatement {
                    value: Box::new(self.evaluate(*value, stage)?),
                },
                MiddleNodeType::VariableDeclaration {
                    var_type,
                    identifier,
                    value,
                    data_type,
                } => MiddleNodeType::VariableDeclaration {
                    var_type,
                    identifier,
                    value: Box::new(self.evaluate(*value, stage)?),
                    data_type,
                },
                MiddleNodeType::EnumExpression {
                    identifier,
                    value,
                    data: Some(data),
                } => MiddleNodeType::EnumExpression {
                    identifier,
                    value,
                    data: Some(Box::new(self.evaluate(*data, stage)?)),
                },
                MiddleNodeType::ScopeDeclaration {
                    body,
                    create_new_scope,
                    is_temp,
                } => {
                    let mut stmts = Vec::new();

                    for stmt in body {
                        stmts.push(self.evaluate(stmt, stage)?);
                    }

                    MiddleNodeType::ScopeDeclaration {
                        body: stmts,
                        create_new_scope,
                        is_temp,
                    }
                }
                MiddleNodeType::NotExpression { value } => MiddleNodeType::NotExpression {
                    value: Box::new(self.evaluate(*value, stage)?),
                },
                MiddleNodeType::DataType { data_type } => MiddleNodeType::DataType { data_type },
                MiddleNodeType::FunctionDeclaration {
                    parameters,
                    body,
                    return_type,
                    is_async,
                } => MiddleNodeType::FunctionDeclaration {
                    parameters: {
                        let mut params = Vec::new();

                        for param in parameters {
                            params.push((
                                param.0,
                                param.1,
                                if let Some(value) = param.2 {
                                    Some(self.evaluate(value, stage)?)
                                } else {
                                    None
                                },
                            ));
                        }

                        params
                    },
                    body: Box::new(self.evaluate(*body, stage)?),
                    return_type,
                    is_async,
                },
                MiddleNodeType::AssignmentExpression { identifier, value } => {
                    MiddleNodeType::AssignmentExpression {
                        identifier: Box::new(self.evaluate(*identifier, stage)?),
                        value: Box::new(self.evaluate(*value, stage)?),
                    }
                }
                MiddleNodeType::DebugExpression {
                    pretty_printed_str,
                    value,
                } => MiddleNodeType::DebugExpression {
                    pretty_printed_str,
                    value: Box::new(self.evaluate(*value, stage)?),
                },
                MiddleNodeType::NegExpression { value } => MiddleNodeType::NegExpression {
                    value: Box::new(self.evaluate(*value, stage)?),
                },
                MiddleNodeType::AsExpression { value, typ } => MiddleNodeType::AsExpression {
                    value: Box::new(self.evaluate(*value, stage)?),
                    typ,
                },
                MiddleNodeType::InDeclaration {
                    identifier,
                    expression,
                } => MiddleNodeType::InDeclaration {
                    identifier: Box::new(self.evaluate(*identifier, stage)?),
                    expression: Box::new(self.evaluate(*expression, stage)?),
                },
                MiddleNodeType::AggregateExpression { identifier, value } => {
                    MiddleNodeType::AggregateExpression {
                        identifier,
                        value: {
                            let mut map = HashMap::new();

                            for (k, value) in value.0 {
                                map.insert(k, self.evaluate(value, stage)?);
                            }

                            ObjectMap(map)
                        },
                    }
                }
                MiddleNodeType::ListLiteral(data_type, data) => {
                    let mut lst = Vec::new();
                    for d in data {
                        lst.push(self.evaluate(d, stage)?);
                    }
                    MiddleNodeType::ListLiteral(data_type, lst)
                }

                MiddleNodeType::Return { value } => MiddleNodeType::Return {
                    value: if let Some(value) = value {
                        Some(Box::new(self.evaluate(*value, stage)?))
                    } else {
                        None
                    },
                },
                MiddleNodeType::LoopDeclaration { state, body } => {
                    MiddleNodeType::LoopDeclaration {
                        state: if let Some(state) = state {
                            Some(Box::new(self.evaluate(*state, stage)?))
                        } else {
                            None
                        },
                        body: Box::new(self.evaluate(*body, stage)?),
                    }
                }
                MiddleNodeType::IsDeclaration { value, data_type } => {
                    MiddleNodeType::IsDeclaration {
                        value: Box::new(self.evaluate(*value, stage)?),
                        data_type,
                    }
                }
                MiddleNodeType::CallExpression(caller, args) => {
                    MiddleNodeType::CallExpression(Box::new(self.evaluate(*caller, stage)?), {
                        let mut lst = Vec::new();
                        for arg in args {
                            lst.push((
                                self.evaluate(arg.0, stage)?,
                                if let Some(a) = arg.1 {
                                    Some(self.evaluate(a, stage)?)
                                } else {
                                    None
                                },
                            ));
                        }

                        lst
                    })
                }
                MiddleNodeType::BinaryExpression {
                    left,
                    right,
                    operator,
                } => MiddleNodeType::BinaryExpression {
                    left: Box::new(self.evaluate(*left, stage)?),
                    right: Box::new(self.evaluate(*right, stage)?),
                    operator,
                },
                MiddleNodeType::ComparisonExpression {
                    left,
                    right,
                    operator,
                } => MiddleNodeType::ComparisonExpression {
                    left: Box::new(self.evaluate(*left, stage)?),
                    right: Box::new(self.evaluate(*right, stage)?),
                    operator,
                },
                MiddleNodeType::BooleanExpression {
                    left,
                    right,
                    operator,
                } => MiddleNodeType::BooleanExpression {
                    left: Box::new(self.evaluate(*left, stage)?),
                    right: Box::new(self.evaluate(*right, stage)?),
                    operator,
                },
                MiddleNodeType::RangeDeclaration {
                    from,
                    to,
                    inclusive,
                } => MiddleNodeType::RangeDeclaration {
                    from: Box::new(self.evaluate(*from, stage)?),
                    to: Box::new(self.evaluate(*to, stage)?),
                    inclusive,
                },
                MiddleNodeType::IfStatement {
                    comparison,
                    then,
                    otherwise,
                } => MiddleNodeType::IfStatement {
                    comparison: Box::new(self.evaluate(*comparison, stage)?),
                    then: Box::new(self.evaluate(*then, stage)?),
                    otherwise: if let Some(otherwise) = otherwise {
                        Some(Box::new(self.evaluate(*otherwise, stage)?))
                    } else {
                        None
                    },
                },
                MiddleNodeType::Comp {
                    stage: CompStage::Specific(s),
                    body,
                } if stage >= s => {
                    self.changed_this_stage += 1;
                    let scope = self.scope.clone();
                    let result = self.interpreter.evaluate(&scope, *body)?;
                    MiddleNodeType::interpreter_from(&self.interpreter, &0, result)?
                }
                MiddleNodeType::Comp {
                    stage: CompStage::Wildcard,
                    body,
                } if stage <= 0 => {
                    let scope = self.scope.clone();
                    let _ = self.interpreter.evaluate(&scope, *body.clone());
                    MiddleNodeType::Comp {
                        stage: CompStage::Wildcard,
                        body: body,
                    }
                }
                MiddleNodeType::StringLiteral(_)
                | MiddleNodeType::EnumExpression { .. }
                | MiddleNodeType::Comp { .. }
                | MiddleNodeType::MemberExpression { path: _ }
                | MiddleNodeType::FloatLiteral(_)
                | MiddleNodeType::IntLiteral(_)
                | MiddleNodeType::CharLiteral(_)
                | MiddleNodeType::Identifier(_)
                | MiddleNodeType::Break
                | MiddleNodeType::Continue
                | MiddleNodeType::EmptyLine => node.node_type,
            },
        })
    }
}
