use crate::{
    environment::MiddleEnvironment, scoping::ScopeId, symbols::resolve::ResolutionOptions,
    translate::MirLowering,
};
use calibre_parser::ast::{
    nodes::{AstNode, AstNodeType, flow::AstEmit},
    types::{ParserDataType, ParserInnerType},
};

impl MiddleEnvironment {
    pub fn resolve_emit_type_from_node(
        &mut self,
        scope: ScopeId,
        node: &AstNode,
    ) -> Option<ParserDataType> {
        let typ = match &node.node_type {
            AstNodeType::IfStatement { .. } | AstNodeType::MatchStatement { .. } => {
                self.resolve_type_from_node(scope, node)
            }
            AstNodeType::Emit(AstEmit::Scope(x)) => self.resolve_type_from_node(scope, x),
            _ => None,
        };

        typ.and_then(|typ| {
            self.resolve_data_type(scope, &typ, ResolutionOptions::typing())
                .ok()
        })
    }

    pub fn resolve_curried_type(
        &mut self,
        scope: ScopeId,
        value: &AstNode,
    ) -> Option<ParserDataType> {
        match self.resolve_type_from_node(scope, value)?.data_type {
            ParserInnerType::Function {
                return_type,
                parameters,
            }
            | ParserInnerType::NativeFunction {
                return_type,
                parameters,
            } => {
                if parameters.is_empty() {
                    return Some(ParserDataType::function(
                        value.span,
                        parameters,
                        *return_type,
                    ));
                }

                let mut result = *return_type;
                for parameter in parameters.iter().rev() {
                    result = ParserDataType::function(value.span, vec![parameter.clone()], result);
                }

                Some(result)
            }
            other => Some(ParserDataType::function(
                value.span,
                Vec::new(),
                ParserDataType::new(value.span, other),
            )),
        }
    }

    pub fn resolve_type_from_node(
        &mut self,
        scope: ScopeId,
        node: &AstNode,
    ) -> Option<ParserDataType> {
        let typ = match &node.node_type {
            // Flow
            AstNodeType::Emit(x) => x.type_of(self, scope, node.span),
            AstNodeType::Try(x) => x.type_of(self, scope, node.span),
            AstNodeType::PipeExpression(x) => x.type_of(self, scope, node.span),

            // Literals
            AstNodeType::StructLiteral(x) => x.type_of(self, scope, node.span),
            AstNodeType::EnumExpression(x) => x.type_of(self, scope, node.span),
            AstNodeType::TupleLiteral(x) => x.type_of(self, scope, node.span),
            AstNodeType::StringLiteral(x) => x.type_of(self, scope, node.span),
            AstNodeType::RangeDeclaration(x) => x.type_of(self, scope, node.span),
            AstNodeType::IntLiteral(x) => x.type_of(self, scope, node.span),
            AstNodeType::CharLiteral(x) => x.type_of(self, scope, node.span),
            AstNodeType::BigLiteral(x) => x.type_of(self, scope, node.span),
            AstNodeType::FloatLiteral(x) => x.type_of(self, scope, node.span),

            // Lists
            AstNodeType::ListLiteral(x) => x.type_of(self, scope, node.span),
            AstNodeType::ListRepeatLiteral(x) => x.type_of(self, scope, node.span),

            // Conditionals
            AstNodeType::Ternary(x) => x.type_of(self, scope, node.span),
            AstNodeType::IfStatement(x) => x.type_of(self, scope, node.span),

            // Unary
            AstNodeType::NegExpression(x) => x.type_of(self, scope, node.span),
            AstNodeType::NotExpression(x) => x.type_of(self, scope, node.span),

            // Binary
            AstNodeType::InDeclaration(x) => x.type_of(self, scope, node.span),
            AstNodeType::ComparisonExpression(x) => x.type_of(self, scope, node.span),
            AstNodeType::BooleanExpression(x) => x.type_of(self, scope, node.span),
            AstNodeType::BinaryExpression(x) => x.type_of(self, scope, node.span),
            AstNodeType::AsExpression(x) => x.type_of(self, scope, node.span),
            AstNodeType::IsExpression(x) => x.type_of(self, scope, node.span),

            // Functions
            AstNodeType::CurryExpression(x) => x.type_of(self, scope, node.span),
            AstNodeType::FunctionDeclaration(x) => x.type_of(self, scope, node.span),
            AstNodeType::ExternFunctionDeclaration(x) => x.type_of(self, scope, node.span),
            AstNodeType::CallExpression(x) => x.type_of(self, scope, node.span),
            AstNodeType::FnMatchDeclaration { header, .. } => {
                header.type_of(self, scope, node.span)
            }

            // Access
            AstNodeType::FieldAccess(x) => x.type_of(self, scope, node.span),
            AstNodeType::ScopeAccess(x) => x.type_of(self, scope, node.span),
            AstNodeType::IndexAccess(x) => x.type_of(self, scope, node.span),
            AstNodeType::Identifier(x) => x.type_of(self, scope, node.span),

            // Memory
            AstNodeType::MoveExpression(x) => x.type_of(self, scope, node.span),
            AstNodeType::RefStatement(x) => x.type_of(self, scope, node.span),
            AstNodeType::DerefStatement(x) => x.type_of(self, scope, node.span),

            // TODO
            AstNodeType::Break { .. }
            | AstNodeType::Continue { .. }
            | AstNodeType::VariableDeclaration { .. }
            | AstNodeType::ImplDeclaration { .. }
            | AstNodeType::ImplTraitDeclaration { .. }
            | AstNodeType::TraitDeclaration { .. }
            | AstNodeType::TypeDeclaration { .. }
            | AstNodeType::Return { .. }
            | AstNodeType::ImportStatement { .. }
            | AstNodeType::AssignmentExpression { .. }
            | AstNodeType::DestructureDeclaration { .. }
            | AstNodeType::DestructureAssignment { .. }
            | AstNodeType::LoopDeclaration {
                else_body: None, ..
            }
            | AstNodeType::TestDeclaration { .. }
            | AstNodeType::ScopeDeclaration { define: true, .. }
            | AstNodeType::ScopeAlias { .. }
            | AstNodeType::DataType { .. }
            | AstNodeType::SelectStatement { .. } => None,
            AstNodeType::Spawn { auto_wait, .. } => Some(ParserDataType::new(
                node.span,
                if *auto_wait {
                    ParserInnerType::Null
                } else {
                    ParserInnerType::Struct(String::from("WaitGroup"))
                },
            )),
            AstNodeType::InlineGenerator { map, data_type, .. } => {
                let elem = match data_type {
                    Some(dt) => dt.clone(),
                    _ => self
                        .resolve_type_from_node(scope, map)
                        .unwrap_or(ParserDataType::new(node.span, ParserInnerType::Auto(None))),
                };

                Some(ParserDataType::new(
                    node.span,
                    ParserInnerType::Gen(Box::new(elem)),
                ))
            }
            AstNodeType::Null
            | AstNodeType::Defer { .. }
            | AstNodeType::Drop(_)
            | AstNodeType::EmptyLine => Some(ParserDataType::new(node.span, ParserInnerType::Null)),
            AstNodeType::ParenExpression { value } => self
                .resolve_type_from_node(scope, value)
                .map(|x| x.unwrap_all_refs()),
            AstNodeType::ScopeDeclaration {
                body: Some(body), ..
            } => {
                let mut typ = None;

                for node in body {
                    typ = self.resolve_emit_type_from_node(scope, node);
                    if typ.is_some() {
                        break;
                    }
                }

                typ
            }
            AstNodeType::ScopeDeclaration {
                named: Some(named), ..
            } => {
                let name = self
                    .resolve(
                        scope,
                        &named.name,
                        ResolutionOptions::default().with_dollar(),
                    )
                    .ok()?;
                let resolved = self
                    .scoping
                    .resolve_macro(scope, &name)?
                    .body
                    .last()?
                    .clone();
                self.resolve_type_from_node(scope, &resolved)
            }

            AstNodeType::LoopDeclaration {
                else_body: Some(body),
                ..
            } => self.resolve_type_from_node(scope, body),
            AstNodeType::MatchStatement { value: _, body } => {
                if let Some((_arm_type, _guards, arm_body)) = body.first() {
                    self.resolve_type_from_node(scope, arm_body)
                } else {
                    None
                }
            }
            AstNodeType::IterExpression {
                data_type, spawned, ..
            } => {
                let list_type = ParserDataType {
                    data_type: ParserInnerType::List(Box::new(
                        self.resolve_data_type(scope, data_type, ResolutionOptions::typing())
                            .ok()?,
                    )),
                    span: node.span,
                };
                if *spawned {
                    Some(ParserDataType {
                        data_type: ParserInnerType::StructWithGenerics {
                            identifier: String::from("Mutex"),
                            generic_types: vec![list_type],
                        },
                        span: node.span,
                    })
                } else {
                    Some(list_type)
                }
            }
            AstNodeType::ScopeDeclaration { .. } => unreachable!(),
            AstNodeType::Tag { .. } => {
                Some(ParserDataType::new(node.span, ParserInnerType::Auto(None)))
            }
        };

        typ.and_then(|typ| {
            self.resolve_data_type(scope, &typ, ResolutionOptions::typing())
                .ok()
        })
    }
}
