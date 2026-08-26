use calibre_parser::ast::{
    ObjectType,
    nodes::{AstNode, AstNodeType, CallArg, IfComparisonType, LoopType, PipeSegment},
};

pub trait NodeVisitor {
    fn visit(&mut self, node: AstNode) -> AstNode {
        let span = node.span;
        let node_type = self.visit_node_type(node.node_type);
        AstNode::new(span, node_type)
    }

    fn visit_node_type(&mut self, node_type: AstNodeType) -> AstNodeType {
        self.visit_children(node_type)
    }

    fn visit_children(&mut self, node_type: AstNodeType) -> AstNodeType {
        match node_type {
            AstNodeType::BinaryExpression {
                left,
                right,
                operator,
            } => AstNodeType::BinaryExpression {
                left: Box::new(self.visit(*left)),
                right: Box::new(self.visit(*right)),
                operator,
            },
            AstNodeType::BooleanExpression {
                left,
                right,
                operator,
            } => AstNodeType::BooleanExpression {
                left: Box::new(self.visit(*left)),
                right: Box::new(self.visit(*right)),
                operator,
            },
            AstNodeType::ComparisonExpression {
                left,
                right,
                operator,
            } => AstNodeType::ComparisonExpression {
                left: Box::new(self.visit(*left)),
                right: Box::new(self.visit(*right)),
                operator,
            },
            AstNodeType::AssignmentExpression { identifier, value } => {
                AstNodeType::AssignmentExpression {
                    identifier: Box::new(self.visit(*identifier)),
                    value: Box::new(self.visit(*value)),
                }
            }
            AstNodeType::CallExpression {
                string_fn,
                caller,
                generic_types,
                args,
                reverse_args,
            } => AstNodeType::CallExpression {
                string_fn,
                caller: Box::new(self.visit(*caller)),
                generic_types,
                args: args
                    .into_iter()
                    .map(|a| match a {
                        CallArg::Value(v) => CallArg::Value(self.visit(v)),
                        CallArg::Named(n, v) => CallArg::Named(n, self.visit(v)),
                    })
                    .collect(),
                reverse_args: reverse_args.into_iter().map(|n| self.visit(n)).collect(),
            },
            AstNodeType::IfStatement {
                comparison,
                then,
                otherwise,
            } => AstNodeType::IfStatement {
                comparison: Box::new(match *comparison {
                    IfComparisonType::If(n) => IfComparisonType::If(self.visit(n)),
                    IfComparisonType::IfLet { value, pattern } => IfComparisonType::IfLet {
                        value: self.visit(value),
                        pattern,
                    },
                }),
                then: Box::new(self.visit(*then)),
                otherwise: otherwise.map(|n| Box::new(self.visit(*n))),
            },
            AstNodeType::ScopeDeclaration {
                body,
                named,
                is_temp,
                create_new_scope,
                define,
            } => AstNodeType::ScopeDeclaration {
                body: body.map(|items| items.into_iter().map(|n| self.visit(n)).collect()),
                named,
                is_temp,
                create_new_scope,
                define,
            },
            AstNodeType::ParenExpression { value } => AstNodeType::ParenExpression {
                value: Box::new(self.visit(*value)),
            },
            AstNodeType::NotExpression { value } => AstNodeType::NotExpression {
                value: Box::new(self.visit(*value)),
            },
            AstNodeType::NegExpression { value } => AstNodeType::NegExpression {
                value: Box::new(self.visit(*value)),
            },
            AstNodeType::DebugExpression { value } => AstNodeType::DebugExpression {
                value: Box::new(self.visit(*value)),
            },
            AstNodeType::AsExpression {
                value,
                data_type,
                failure_mode,
            } => AstNodeType::AsExpression {
                value: Box::new(self.visit(*value)),
                data_type,
                failure_mode,
            },
            AstNodeType::IsExpression { value, data_type } => AstNodeType::IsExpression {
                value: Box::new(self.visit(*value)),
                data_type,
            },
            AstNodeType::TupleLiteral { values } => AstNodeType::TupleLiteral {
                values: values.into_iter().map(|n| self.visit(n)).collect(),
            },
            AstNodeType::StructLiteral { identifier, value } => AstNodeType::StructLiteral {
                identifier,
                value: match value {
                    ObjectType::Map(fields) => ObjectType::Map(
                        fields
                            .into_iter()
                            .map(|(k, v)| (k, self.visit(v)))
                            .collect(),
                    ),
                    ObjectType::Tuple(values) => {
                        ObjectType::Tuple(values.into_iter().map(|n| self.visit(n)).collect())
                    }
                },
            },
            AstNodeType::ListLiteral(data_type, values) => AstNodeType::ListLiteral(
                data_type,
                values.into_iter().map(|n| self.visit(n)).collect(),
            ),
            AstNodeType::DerefStatement { value } => AstNodeType::DerefStatement {
                value: Box::new(self.visit(*value)),
            },
            AstNodeType::VariableDeclaration {
                var_type,
                identifier,
                data_type,
                value,
            } => AstNodeType::VariableDeclaration {
                var_type,
                identifier,
                data_type,
                value: Box::new(self.visit(*value)),
            },
            AstNodeType::TypeDeclaration {
                identifier,
                object,
                overloads,
            } => AstNodeType::TypeDeclaration {
                identifier,
                object,
                overloads,
            },
            AstNodeType::FunctionDeclaration { header, body } => AstNodeType::FunctionDeclaration {
                header,
                body: Box::new(self.visit(*body)),
            },
            AstNodeType::LoopDeclaration {
                loop_type,
                body,
                until,
                label,
                else_body,
            } => AstNodeType::LoopDeclaration {
                loop_type: Box::new(self.visit_loop_type(*loop_type)),
                body: Box::new(self.visit(*body)),
                until: until.map(|n| Box::new(self.visit(*n))),
                label,
                else_body: else_body.map(|n| Box::new(self.visit(*n))),
            },
            AstNodeType::MatchStatement { value, body } => AstNodeType::MatchStatement {
                value: value.map(|n| Box::new(self.visit(*n))),
                body: body
                    .into_iter()
                    .map(|(arm, guards, body)| {
                        (
                            arm,
                            guards.into_iter().map(|n| self.visit(n)).collect(),
                            Box::new(self.visit(*body)),
                        )
                    })
                    .collect(),
            },
            AstNodeType::Ternary {
                comparison,
                then,
                otherwise,
            } => AstNodeType::Ternary {
                comparison: Box::new(self.visit(*comparison)),
                then: Box::new(self.visit(*then)),
                otherwise: Box::new(self.visit(*otherwise)),
            },
            AstNodeType::FieldAccess { base, field } => AstNodeType::FieldAccess {
                base: Box::new(self.visit(*base)),
                field,
            },
            AstNodeType::ScopeAccess { base, field } => AstNodeType::ScopeAccess {
                base: Box::new(self.visit(*base)),
                field,
            },
            AstNodeType::IndexAccess { base, index } => AstNodeType::IndexAccess {
                base: Box::new(self.visit(*base)),
                index: Box::new(self.visit(*index)),
            },
            AstNodeType::DestructureDeclaration {
                var_type,
                pattern,
                value,
            } => AstNodeType::DestructureDeclaration {
                var_type,
                pattern,
                value: Box::new(self.visit(*value)),
            },
            AstNodeType::DestructureAssignment { pattern, value } => {
                AstNodeType::DestructureAssignment {
                    pattern,
                    value: Box::new(self.visit(*value)),
                }
            }
            AstNodeType::Spawn { items, auto_wait } => AstNodeType::Spawn {
                items: items.into_iter().map(|n| self.visit(n)).collect(),
                auto_wait,
            },
            AstNodeType::MoveExpression { value } => AstNodeType::MoveExpression {
                value: Box::new(self.visit(*value)),
            },
            AstNodeType::InDeclaration { identifier, value } => AstNodeType::InDeclaration {
                identifier: Box::new(self.visit(*identifier)),
                value: Box::new(self.visit(*value)),
            },
            AstNodeType::ExternFunctionDeclaration {
                abi,
                identifier,
                parameters,
                return_type,
                library,
                symbol,
            } => AstNodeType::ExternFunctionDeclaration {
                abi,
                identifier,
                parameters,
                return_type,
                library,
                symbol,
            },
            AstNodeType::Return { value } => AstNodeType::Return {
                value: value.map(|n| Box::new(self.visit(*n))),
            },
            AstNodeType::Break { label, value } => AstNodeType::Break {
                label,
                value: value.map(|n| Box::new(self.visit(*n))),
            },
            AstNodeType::Continue { label } => AstNodeType::Continue { label },
            AstNodeType::EmptyLine => AstNodeType::EmptyLine,
            AstNodeType::Null => AstNodeType::Null,
            AstNodeType::Identifier(_)
            | AstNodeType::StringLiteral(_)
            | AstNodeType::IntLiteral(_)
            | AstNodeType::BigLiteral(_)
            | AstNodeType::FloatLiteral(_)
            | AstNodeType::CharLiteral(_) => node_type,
            AstNodeType::SelectStatement { arms } => AstNodeType::SelectStatement { arms },
            AstNodeType::Emit(emit_type) => AstNodeType::Emit(emit_type),
            AstNodeType::RefStatement { mutability, value } => AstNodeType::RefStatement {
                mutability,
                value: Box::new(self.visit(*value)),
            },
            AstNodeType::DataType { data_type } => AstNodeType::DataType { data_type },
            AstNodeType::Drop(identifier) => AstNodeType::Drop(identifier),
            AstNodeType::Defer { value, function } => AstNodeType::Defer {
                value: Box::new(self.visit(*value)),
                function,
            },
            AstNodeType::ImplDeclaration {
                generics,
                target,
                variables,
            } => AstNodeType::ImplDeclaration {
                generics,
                target,
                variables: variables.into_iter().map(|n| self.visit(n)).collect(),
            },
            AstNodeType::ImplTraitDeclaration {
                generics,
                trait_ident,
                target,
                variables,
            } => AstNodeType::ImplTraitDeclaration {
                generics,
                trait_ident,
                target,
                variables: variables.into_iter().map(|n| self.visit(n)).collect(),
            },
            AstNodeType::TraitDeclaration {
                identifier,
                implied_traits,
                members,
            } => AstNodeType::TraitDeclaration {
                identifier,
                implied_traits,
                members,
            },
            AstNodeType::EnumExpression {
                identifier,
                value,
                data,
            } => AstNodeType::EnumExpression {
                identifier,
                value,
                data: data.map(|n| Box::new(self.visit(*n))),
            },
            AstNodeType::ScopeAlias {
                identifier,
                value,
                create_new_scope,
            } => AstNodeType::ScopeAlias {
                identifier,
                value,
                create_new_scope,
            },
            AstNodeType::FnMatchDeclaration { header, body } => AstNodeType::FnMatchDeclaration {
                header,
                body: body
                    .into_iter()
                    .map(|(arm, guards, body)| {
                        (
                            arm,
                            guards.into_iter().map(|n| self.visit(n)).collect(),
                            Box::new(self.visit(*body)),
                        )
                    })
                    .collect(),
            },
            AstNodeType::RangeDeclaration {
                from,
                to,
                inclusive,
            } => AstNodeType::RangeDeclaration {
                from: Box::new(self.visit(*from)),
                to: Box::new(self.visit(*to)),
                inclusive,
            },
            AstNodeType::IterExpression {
                data_type,
                map,
                spawned,
                loop_type,
                conditionals,
                until,
            } => AstNodeType::IterExpression {
                data_type,
                map: Box::new(self.visit(*map)),
                spawned,
                loop_type: Box::new(self.visit_loop_type(*loop_type)),
                conditionals: conditionals.into_iter().map(|n| self.visit(n)).collect(),
                until: until.map(|n| Box::new(self.visit(*n))),
            },
            AstNodeType::InlineGenerator {
                map,
                data_type,
                loop_type,
                conditionals,
                until,
            } => AstNodeType::InlineGenerator {
                map: Box::new(self.visit(*map)),
                data_type,
                loop_type: Box::new(self.visit_loop_type(*loop_type)),
                conditionals: conditionals.into_iter().map(|n| self.visit(n)).collect(),
                until: until.map(|n| Box::new(self.visit(*n))),
            },
            AstNodeType::TestDeclaration { identifier, body } => AstNodeType::TestDeclaration {
                identifier,
                body: Box::new(self.visit(*body)),
            },
            AstNodeType::Try { value, catch } => AstNodeType::Try {
                value: Box::new(self.visit(*value)),
                catch,
            },
            AstNodeType::Until { condition } => AstNodeType::Until {
                condition: Box::new(self.visit(*condition)),
            },
            AstNodeType::ListRepeatLiteral {
                data_type,
                value,
                count,
            } => AstNodeType::ListRepeatLiteral {
                data_type,
                value: Box::new(self.visit(*value)),
                count: Box::new(self.visit(*count)),
            },
            AstNodeType::PipeExpression(segments) => AstNodeType::PipeExpression(
                segments
                    .into_iter()
                    .map(|s| match s {
                        PipeSegment::Unnamed(n) => PipeSegment::Unnamed(self.visit(n)),
                        PipeSegment::Named { identifier, node } => PipeSegment::Named {
                            identifier,
                            node: self.visit(node),
                        },
                    })
                    .collect(),
            ),
            AstNodeType::ImportStatement {
                module,
                alias,
                values,
            } => AstNodeType::ImportStatement {
                module,
                alias,
                values,
            },
            AstNodeType::Tag {
                node,
                tag,
                arguments,
            } => AstNodeType::Tag {
                node: Box::new(self.visit(*node)),
                tag,
                arguments: arguments.into_iter().map(|n| self.visit(n)).collect(),
            },
        }
    }

    fn visit_loop_type(&mut self, loop_type: LoopType) -> LoopType {
        match loop_type {
            LoopType::For(identifier, value) => LoopType::For(identifier, self.visit(value)),
            LoopType::While(condition) => LoopType::While(self.visit(condition)),
            LoopType::Let { pattern, value } => LoopType::Let {
                pattern,
                value: self.visit(value),
            },
            LoopType::Loop => LoopType::Loop,
        }
    }
}

pub trait NodeAnalyzer {
    fn analyze(&mut self, node: &AstNode) -> bool {
        self.analyze_node_type(&node.node_type)
    }

    fn analyze_node_type(&mut self, node_type: &AstNodeType) -> bool {
        self.analyze_children(node_type)
    }

    fn analyze_children(&mut self, node_type: &AstNodeType) -> bool {
        match node_type {
            AstNodeType::BinaryExpression { left, right, .. }
            | AstNodeType::BooleanExpression { left, right, .. }
            | AstNodeType::AssignmentExpression {
                identifier: left,
                value: right,
            }
            | AstNodeType::ComparisonExpression { left, right, .. }
            | AstNodeType::IndexAccess {
                base: left,
                index: right,
            }
            | AstNodeType::InDeclaration {
                identifier: left,
                value: right,
            } => self.analyze(left) && self.analyze(right),
            AstNodeType::CallExpression {
                caller,
                args,
                reverse_args,
                ..
            } => {
                self.analyze(caller)
                    && args.iter().all(|a| match a {
                        CallArg::Value(v) => self.analyze(v),
                        CallArg::Named(_, v) => self.analyze(v),
                    })
                    && reverse_args.iter().all(|n| self.analyze(n))
            }
            AstNodeType::IfStatement {
                comparison,
                then,
                otherwise,
            } => {
                let comp_ok = match comparison.as_ref() {
                    IfComparisonType::If(n) => self.analyze(n),
                    IfComparisonType::IfLet { value, .. } => self.analyze(value),
                };
                comp_ok && self.analyze(then) && otherwise.as_ref().is_none_or(|n| self.analyze(n))
            }
            AstNodeType::ScopeDeclaration { body, .. } => body
                .as_ref()
                .is_none_or(|items| items.iter().all(|n| self.analyze(n))),
            AstNodeType::ParenExpression { value }
            | AstNodeType::NotExpression { value }
            | AstNodeType::NegExpression { value }
            | AstNodeType::DebugExpression { value }
            | AstNodeType::AsExpression { value, .. }
            | AstNodeType::IsExpression { value, .. }
            | AstNodeType::FieldAccess { base: value, .. }
            | AstNodeType::ScopeAccess { base: value, .. }
            | AstNodeType::DerefStatement { value }
            | AstNodeType::DestructureDeclaration { value, .. }
            | AstNodeType::DestructureAssignment { value, .. }
            | AstNodeType::MoveExpression { value } => self.analyze(value),
            AstNodeType::TupleLiteral { values } => values.iter().all(|n| self.analyze(n)),
            AstNodeType::StructLiteral { value, .. } => match value {
                ObjectType::Map(fields) => fields.iter().all(|(_, n)| self.analyze(n)),
                ObjectType::Tuple(values) => values.iter().all(|n| self.analyze(n)),
            },
            AstNodeType::ListLiteral(_, values) => values.iter().all(|n| self.analyze(n)),
            AstNodeType::VariableDeclaration { value, .. } => self.analyze(value),
            AstNodeType::FunctionDeclaration { body, .. } => self.analyze(body),
            AstNodeType::LoopDeclaration {
                loop_type,
                body,
                until,
                else_body,
                ..
            } => {
                let loop_ok = match loop_type.as_ref() {
                    LoopType::For(_, value) => self.analyze(value),
                    LoopType::While(condition) => self.analyze(condition),
                    LoopType::Let { value, .. } => self.analyze(value),
                    LoopType::Loop => true,
                };
                loop_ok
                    && self.analyze(body)
                    && until.as_ref().is_none_or(|n| self.analyze(n))
                    && else_body.as_ref().is_none_or(|n| self.analyze(n))
            }
            AstNodeType::MatchStatement { value, body } => {
                let value_ok = value.as_ref().is_none_or(|n| self.analyze(n));
                let body_ok = body.iter().all(|(_arm, guards, body)| {
                    guards.iter().all(|n| self.analyze(n)) && self.analyze(body)
                });
                value_ok && body_ok
            }
            AstNodeType::Ternary {
                comparison,
                then,
                otherwise,
            } => self.analyze(comparison) && self.analyze(then) && self.analyze(otherwise),
            AstNodeType::Spawn { items, .. } => items.iter().all(|n| self.analyze(n)),
            AstNodeType::Return { value } | AstNodeType::Break { value, .. } => {
                value.as_ref().is_none_or(|n| self.analyze(n))
            }
            _ => true,
        }
    }
}
