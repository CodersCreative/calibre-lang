use calibre_parser::ast::{
    ObjectType,
    nodes::{CallArg, IfComparisonType, LoopType, Node, NodeType, PipeSegment},
};

pub trait NodeVisitor {
    fn visit(&mut self, node: Node) -> Node {
        let span = node.span;
        let node_type = self.visit_node_type(node.node_type);
        Node::new(span, node_type)
    }

    fn visit_node_type(&mut self, node_type: NodeType) -> NodeType {
        self.visit_children(node_type)
    }

    fn visit_children(&mut self, node_type: NodeType) -> NodeType {
        match node_type {
            NodeType::BinaryExpression {
                left,
                right,
                operator,
            } => NodeType::BinaryExpression {
                left: Box::new(self.visit(*left)),
                right: Box::new(self.visit(*right)),
                operator,
            },
            NodeType::BooleanExpression {
                left,
                right,
                operator,
            } => NodeType::BooleanExpression {
                left: Box::new(self.visit(*left)),
                right: Box::new(self.visit(*right)),
                operator,
            },
            NodeType::ComparisonExpression {
                left,
                right,
                operator,
            } => NodeType::ComparisonExpression {
                left: Box::new(self.visit(*left)),
                right: Box::new(self.visit(*right)),
                operator,
            },
            NodeType::AssignmentExpression { identifier, value } => {
                NodeType::AssignmentExpression {
                    identifier: Box::new(self.visit(*identifier)),
                    value: Box::new(self.visit(*value)),
                }
            }
            NodeType::CallExpression {
                string_fn,
                caller,
                generic_types,
                args,
                reverse_args,
            } => NodeType::CallExpression {
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
            NodeType::IfStatement {
                comparison,
                then,
                otherwise,
            } => NodeType::IfStatement {
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
            NodeType::ScopeDeclaration {
                body,
                named,
                is_temp,
                create_new_scope,
                define,
            } => NodeType::ScopeDeclaration {
                body: body.map(|items| items.into_iter().map(|n| self.visit(n)).collect()),
                named,
                is_temp,
                create_new_scope,
                define,
            },
            NodeType::ParenExpression { value } => NodeType::ParenExpression {
                value: Box::new(self.visit(*value)),
            },
            NodeType::NotExpression { value } => NodeType::NotExpression {
                value: Box::new(self.visit(*value)),
            },
            NodeType::NegExpression { value } => NodeType::NegExpression {
                value: Box::new(self.visit(*value)),
            },
            NodeType::DebugExpression { value } => NodeType::DebugExpression {
                value: Box::new(self.visit(*value)),
            },
            NodeType::AsExpression {
                value,
                data_type,
                failure_mode,
            } => NodeType::AsExpression {
                value: Box::new(self.visit(*value)),
                data_type,
                failure_mode,
            },
            NodeType::IsExpression { value, data_type } => NodeType::IsExpression {
                value: Box::new(self.visit(*value)),
                data_type,
            },
            NodeType::TupleLiteral { values } => NodeType::TupleLiteral {
                values: values.into_iter().map(|n| self.visit(n)).collect(),
            },
            NodeType::StructLiteral { identifier, value } => NodeType::StructLiteral {
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
            NodeType::ListLiteral(data_type, values) => NodeType::ListLiteral(
                data_type,
                values.into_iter().map(|n| self.visit(n)).collect(),
            ),
            NodeType::DerefStatement { value } => NodeType::DerefStatement {
                value: Box::new(self.visit(*value)),
            },
            NodeType::VariableDeclaration {
                var_type,
                identifier,
                data_type,
                value,
            } => NodeType::VariableDeclaration {
                var_type,
                identifier,
                data_type: data_type,
                value: Box::new(self.visit(*value)),
            },
            NodeType::TypeDeclaration {
                identifier,
                object,
                overloads,
            } => NodeType::TypeDeclaration {
                identifier,
                object,
                overloads,
            },
            NodeType::FunctionDeclaration { header, body } => NodeType::FunctionDeclaration {
                header,
                body: Box::new(self.visit(*body)),
            },
            NodeType::LoopDeclaration {
                loop_type,
                body,
                until,
                label,
                else_body,
            } => NodeType::LoopDeclaration {
                loop_type: Box::new(self.visit_loop_type(*loop_type)),
                body: Box::new(self.visit(*body)),
                until: until.map(|n| Box::new(self.visit(*n))),
                label,
                else_body: else_body.map(|n| Box::new(self.visit(*n))),
            },
            NodeType::MatchStatement { value, body } => NodeType::MatchStatement {
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
            NodeType::Ternary {
                comparison,
                then,
                otherwise,
            } => NodeType::Ternary {
                comparison: Box::new(self.visit(*comparison)),
                then: Box::new(self.visit(*then)),
                otherwise: Box::new(self.visit(*otherwise)),
            },
            NodeType::FieldAccess { base, field } => NodeType::FieldAccess {
                base: Box::new(self.visit(*base)),
                field,
            },
            NodeType::ScopeAccess { base, field } => NodeType::ScopeAccess {
                base: Box::new(self.visit(*base)),
                field,
            },
            NodeType::IndexAccess { base, index } => NodeType::IndexAccess {
                base: Box::new(self.visit(*base)),
                index: Box::new(self.visit(*index)),
            },
            NodeType::DestructureDeclaration {
                var_type,
                pattern,
                value,
            } => NodeType::DestructureDeclaration {
                var_type,
                pattern,
                value: Box::new(self.visit(*value)),
            },
            NodeType::DestructureAssignment { pattern, value } => NodeType::DestructureAssignment {
                pattern,
                value: Box::new(self.visit(*value)),
            },
            NodeType::Spawn { items, auto_wait } => NodeType::Spawn {
                items: items.into_iter().map(|n| self.visit(n)).collect(),
                auto_wait,
            },
            NodeType::MoveExpression { value } => NodeType::MoveExpression {
                value: Box::new(self.visit(*value)),
            },
            NodeType::InDeclaration { identifier, value } => NodeType::InDeclaration {
                identifier: Box::new(self.visit(*identifier)),
                value: Box::new(self.visit(*value)),
            },
            NodeType::ExternFunctionDeclaration {
                abi,
                identifier,
                parameters,
                return_type,
                library,
                symbol,
            } => NodeType::ExternFunctionDeclaration {
                abi,
                identifier,
                parameters,
                return_type,
                library,
                symbol,
            },
            NodeType::Return { value } => NodeType::Return {
                value: value.map(|n| Box::new(self.visit(*n))),
            },
            NodeType::Break { label, value } => NodeType::Break {
                label,
                value: value.map(|n| Box::new(self.visit(*n))),
            },
            NodeType::Continue { label } => NodeType::Continue { label },
            NodeType::EmptyLine => NodeType::EmptyLine,
            NodeType::Null => NodeType::Null,
            NodeType::Identifier(_)
            | NodeType::StringLiteral(_)
            | NodeType::IntLiteral(_)
            | NodeType::BigLiteral(_)
            | NodeType::FloatLiteral(_)
            | NodeType::CharLiteral(_) => node_type,
            NodeType::SelectStatement { arms } => NodeType::SelectStatement { arms },
            NodeType::Emit(emit_type) => NodeType::Emit(emit_type),
            NodeType::RefStatement { mutability, value } => NodeType::RefStatement {
                mutability,
                value: Box::new(self.visit(*value)),
            },
            NodeType::DataType { data_type } => NodeType::DataType { data_type },
            NodeType::Drop(identifier) => NodeType::Drop(identifier),
            NodeType::Defer { value, function } => NodeType::Defer {
                value: Box::new(self.visit(*value)),
                function,
            },
            NodeType::ImplDeclaration {
                generics,
                target,
                variables,
            } => NodeType::ImplDeclaration {
                generics,
                target,
                variables: variables.into_iter().map(|n| self.visit(n)).collect(),
            },
            NodeType::ImplTraitDeclaration {
                generics,
                trait_ident,
                target,
                variables,
            } => NodeType::ImplTraitDeclaration {
                generics,
                trait_ident,
                target,
                variables: variables.into_iter().map(|n| self.visit(n)).collect(),
            },
            NodeType::TraitDeclaration {
                identifier,
                implied_traits,
                members,
            } => NodeType::TraitDeclaration {
                identifier,
                implied_traits,
                members,
            },
            NodeType::EnumExpression {
                identifier,
                value,
                data,
            } => NodeType::EnumExpression {
                identifier,
                value,
                data: data.map(|n| Box::new(self.visit(*n))),
            },
            NodeType::ScopeAlias {
                identifier,
                value,
                create_new_scope,
            } => NodeType::ScopeAlias {
                identifier,
                value,
                create_new_scope,
            },
            NodeType::FnMatchDeclaration { header, body } => NodeType::FnMatchDeclaration {
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
            NodeType::RangeDeclaration {
                from,
                to,
                inclusive,
            } => NodeType::RangeDeclaration {
                from: Box::new(self.visit(*from)),
                to: Box::new(self.visit(*to)),
                inclusive,
            },
            NodeType::IterExpression {
                data_type,
                map,
                spawned,
                loop_type,
                conditionals,
                until,
            } => NodeType::IterExpression {
                data_type,
                map: Box::new(self.visit(*map)),
                spawned,
                loop_type: Box::new(self.visit_loop_type(*loop_type)),
                conditionals: conditionals.into_iter().map(|n| self.visit(n)).collect(),
                until: until.map(|n| Box::new(self.visit(*n))),
            },
            NodeType::InlineGenerator {
                map,
                data_type,
                loop_type,
                conditionals,
                until,
            } => NodeType::InlineGenerator {
                map: Box::new(self.visit(*map)),
                data_type,
                loop_type: Box::new(self.visit_loop_type(*loop_type)),
                conditionals: conditionals.into_iter().map(|n| self.visit(n)).collect(),
                until: until.map(|n| Box::new(self.visit(*n))),
            },
            NodeType::TestDeclaration { identifier, body } => NodeType::TestDeclaration {
                identifier,
                body: Box::new(self.visit(*body)),
            },
            NodeType::Try { value, catch } => NodeType::Try {
                value: Box::new(self.visit(*value)),
                catch,
            },
            NodeType::Until { condition } => NodeType::Until {
                condition: Box::new(self.visit(*condition)),
            },
            NodeType::ListRepeatLiteral {
                data_type,
                value,
                count,
            } => NodeType::ListRepeatLiteral {
                data_type,
                value: Box::new(self.visit(*value)),
                count: Box::new(self.visit(*count)),
            },
            NodeType::PipeExpression(segments) => NodeType::PipeExpression(
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
            NodeType::ImportStatement {
                module,
                alias,
                values,
            } => NodeType::ImportStatement {
                module,
                alias,
                values,
            },
            NodeType::Tag {
                node,
                tag,
                arguments,
            } => NodeType::Tag {
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
    fn analyze(&mut self, node: &Node) -> bool {
        self.analyze_node_type(&node.node_type)
    }

    fn analyze_node_type(&mut self, node_type: &NodeType) -> bool {
        self.analyze_children(node_type)
    }

    fn analyze_children(&mut self, node_type: &NodeType) -> bool {
        match node_type {
            NodeType::BinaryExpression { left, right, .. }
            | NodeType::BooleanExpression { left, right, .. }
            | NodeType::AssignmentExpression {
                identifier: left,
                value: right,
            }
            | NodeType::ComparisonExpression { left, right, .. }
            | NodeType::IndexAccess {
                base: left,
                index: right,
            }
            | NodeType::InDeclaration {
                identifier: left,
                value: right,
            } => self.analyze(left) && self.analyze(right),
            NodeType::CallExpression {
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
            NodeType::IfStatement {
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
            NodeType::ScopeDeclaration { body, .. } => body
                .as_ref()
                .is_none_or(|items| items.iter().all(|n| self.analyze(n))),
            NodeType::ParenExpression { value }
            | NodeType::NotExpression { value }
            | NodeType::NegExpression { value }
            | NodeType::DebugExpression { value }
            | NodeType::AsExpression { value, .. }
            | NodeType::IsExpression { value, .. }
            | NodeType::FieldAccess { base: value, .. }
            | NodeType::ScopeAccess { base: value, .. }
            | NodeType::DerefStatement { value }
            | NodeType::DestructureDeclaration { value, .. }
            | NodeType::DestructureAssignment { value, .. }
            | NodeType::MoveExpression { value } => self.analyze(value),
            NodeType::TupleLiteral { values } => values.iter().all(|n| self.analyze(n)),
            NodeType::StructLiteral { value, .. } => match value {
                ObjectType::Map(fields) => fields.iter().all(|(_, n)| self.analyze(n)),
                ObjectType::Tuple(values) => values.iter().all(|n| self.analyze(n)),
            },
            NodeType::ListLiteral(_, values) => values.iter().all(|n| self.analyze(n)),
            NodeType::VariableDeclaration { value, .. } => self.analyze(value),
            NodeType::FunctionDeclaration { body, .. } => self.analyze(body),
            NodeType::LoopDeclaration {
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
            NodeType::MatchStatement { value, body } => {
                let value_ok = value.as_ref().is_none_or(|n| self.analyze(n));
                let body_ok = body.iter().all(|(_arm, guards, body)| {
                    guards.iter().all(|n| self.analyze(n)) && self.analyze(body)
                });
                value_ok && body_ok
            }
            NodeType::Ternary {
                comparison,
                then,
                otherwise,
            } => self.analyze(comparison) && self.analyze(then) && self.analyze(otherwise),
            NodeType::Spawn { items, .. } => items.iter().all(|n| self.analyze(n)),
            NodeType::Return { value } | NodeType::Break { value, .. } => {
                value.as_ref().is_none_or(|n| self.analyze(n))
            }
            _ => true,
        }
    }
}
