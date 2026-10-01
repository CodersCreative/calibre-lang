use crate::{
    MiddleNode, MiddleNodeType, MirRenamable, MirRenameState,
    ast::{
        MirAggregate, MirAs, MirAssignment, MirBinary, MirBoolean, MirBreak, MirCall,
        MirComparison, MirConditional, MirDeref, MirDrop, MirEnum, MirField, MirFunction,
        MirIdentifier, MirIndex, MirIs, MirList, MirLoop, MirMove, MirNeg, MirRange, MirRef,
        MirReturn, MirScopeDecl, MirVarDecl,
    },
    scoping::FullyQualifiedPath,
    symbols::{MiddleOverload, MiddleVariable, VariableKey},
    typing::{
        MiddleImpl, MiddleImplMember, MiddleObject, MiddleTrait, MiddleTraitMember,
        MiddleTypeDefType,
    },
};
use std::sync::Arc;
use ustr::Ustr;

impl MirRenamable for MiddleNode {
    fn rename(&mut self, state: &mut MirRenameState) {
        self.node_type.rename(state);
    }
}

impl MirRenamable for MiddleNodeType {
    fn rename(&mut self, state: &mut MirRenameState) {
        match self {
            MiddleNodeType::Break(MirBreak {
                label: _,
                value: None,
            })
            | MiddleNodeType::Continue(_)
            | MiddleNodeType::EmptyLine
            | MiddleNodeType::Null
            | MiddleNodeType::EnumExpression(MirEnum { data: None, .. })
            | MiddleNodeType::CharLiteral(_)
            | MiddleNodeType::FloatLiteral(_)
            | MiddleNodeType::BigLiteral(_)
            | MiddleNodeType::IntLiteral { .. }
            | MiddleNodeType::StringLiteral(_)
            | MiddleNodeType::ExternFunction { .. } => {}
            MiddleNodeType::Break(MirBreak { label: _, value }) => {
                if let Some(v) = value {
                    v.rename(state);
                }
            }
            MiddleNodeType::Discriminant(value) => value.value.rename(state),
            MiddleNodeType::Emit(value) => value.value.rename(state),
            MiddleNodeType::Spawn(value) => value.value.rename(state),
            MiddleNodeType::RefStatement(MirRef {
                mutability: _,
                value,
            }) => value.rename(state),
            MiddleNodeType::DerefStatement(MirDeref { value }) => value.rename(state),
            MiddleNodeType::Drop(MirDrop { identifier }) => {
                *identifier = state.mapped_variable_or_original(identifier.clone());
            }
            MiddleNodeType::Move(MirMove { identifier }) => {
                *identifier = state.mapped_variable_or_original(identifier.clone());
            }
            MiddleNodeType::VariableDeclaration(MirVarDecl {
                var_type: _,
                identifier,
                value,
                data_type,
            }) => {
                if !state.dont_change_local {
                    let new_name = Ustr::from(&format!(
                        "{}->{}",
                        identifier.name(),
                        fastrand::u32(0..u32::MAX)
                    ));
                    state.variables.insert(
                        identifier.clone(),
                        VariableKey {
                            fully_qualified_path: FullyQualifiedPath::combine(
                                identifier.fully_qualified_path.parent.clone(),
                                new_name,
                            ),
                            shadow_counter: None,
                        },
                    );
                    let mut new_path = (*identifier.fully_qualified_path).clone();
                    new_path.name = Some(new_name);
                    identifier.fully_qualified_path = Arc::new(new_path);
                }

                value.rename(state);
                data_type.rename(state);
            }
            MiddleNodeType::EnumExpression(MirEnum {
                identifier,
                value: _,
                data,
            }) => {
                *identifier = state.mapped_type_or_original(identifier.clone());
                if let Some(d) = data {
                    d.rename(state);
                }
            }
            MiddleNodeType::ScopeDeclaration(MirScopeDecl {
                body,
                create_new_scope: _,
                is_temp: _,
                scope_id: _,
                function_body: _,
            }) => {
                for node in body {
                    node.rename(state);
                }
            }
            MiddleNodeType::FunctionDeclaration(MirFunction {
                parameters,
                body,
                return_type: _,
                scope_id: _,
                memo: _,
                memo_params,
                pure: _,
                default_args_id: _,
            }) => {
                for param in parameters {
                    let new_name = Ustr::from(&format!(
                        "{}->{}",
                        param.0.name(),
                        fastrand::u32(0..u32::MAX)
                    ));
                    state.variables.insert(
                        param.0.clone(),
                        VariableKey {
                            fully_qualified_path: FullyQualifiedPath::combine(
                                param.0.fully_qualified_path.parent.clone(),
                                new_name,
                            ),
                            shadow_counter: None,
                        },
                    );

                    if let Some(x) = memo_params.iter_mut().find(|x| *x == param.0.name()) {
                        *x = new_name;
                    }

                    let mut new_path = (*param.0.fully_qualified_path).clone();
                    new_path.name = Some(new_name);
                    param.0.fully_qualified_path = Arc::new(new_path);

                    if let Some(default_value) = &mut param.2 {
                        default_value.rename(state);
                    }
                }
                body.rename(state);
            }
            MiddleNodeType::AssignmentExpression(MirAssignment { identifier, value }) => {
                identifier.rename(state);
                value.rename(state);
            }
            MiddleNodeType::NegExpression(MirNeg { value }) => value.rename(state),
            MiddleNodeType::AsExpression(MirAs {
                value,
                data_type,
                failure_mode: _,
            }) => {
                value.rename(state);
                data_type.rename(state);
            }
            MiddleNodeType::IsExpression(MirIs { value, data_type }) => {
                value.rename(state);
                data_type.rename(state);
            }
            MiddleNodeType::RangeDeclaration(MirRange {
                from,
                to,
                inclusive: _,
            }) => {
                from.rename(state);
                to.rename(state);
            }
            MiddleNodeType::LoopDeclaration(MirLoop {
                body,
                scope_id: _,
                label: _,
            }) => {
                body.rename(state);
            }
            MiddleNodeType::Return(MirReturn { value }) => {
                if let Some(v) = value {
                    v.rename(state);
                }
            }
            MiddleNodeType::Identifier(MirIdentifier { identifier }) => {
                *identifier = state.mapped_variable_or_original(identifier.clone());
            }
            MiddleNodeType::ListLiteral(MirList { data_type, values }) => {
                for v in values {
                    v.rename(state);
                }
                data_type.rename(state);
            }
            MiddleNodeType::FieldAccess(MirField { base, field: _ }) => base.rename(state),
            MiddleNodeType::IndexAccess(MirIndex { base, index }) => {
                base.rename(state);
                index.rename(state);
            }
            MiddleNodeType::CallExpression(MirCall { caller, args }) => {
                caller.rename(state);
                for arg in args {
                    arg.rename(state);
                }
            }
            MiddleNodeType::BinaryExpression(MirBinary {
                left,
                right,
                operator: _,
            }) => {
                left.rename(state);
                right.rename(state);
            }
            MiddleNodeType::ComparisonExpression(MirComparison {
                left,
                right,
                operator: _,
            }) => {
                left.rename(state);
                right.rename(state);
            }
            MiddleNodeType::BooleanExpression(MirBoolean {
                left,
                right,
                operator: _,
            }) => {
                left.rename(state);
                right.rename(state);
            }
            MiddleNodeType::AggregateExpression(MirAggregate { identifier, value }) => {
                if let Some(id) = identifier {
                    *identifier = Some(state.mapped_type_or_original(id.clone()));
                }

                for (_, v) in &mut value.0 {
                    v.rename(state);
                }
            }
            MiddleNodeType::Conditional(MirConditional {
                comparison,
                then,
                otherwise,
            }) => {
                comparison.rename(state);
                then.rename(state);
                if let Some(otherwise) = otherwise {
                    otherwise.rename(state);
                }
            }
        }
    }
}

impl MirRenamable for MiddleImplMember {
    fn rename(&mut self, state: &mut MirRenameState) {
        self.symbol_name.rename(state);
    }
}

impl MirRenamable for MiddleImpl {
    fn rename(&mut self, state: &mut MirRenameState) {
        self.target.rename(state);

        if let Some(tk) = &mut self.trait_key {
            tk.rename(state);
        }

        for t in &mut self.traits {
            t.rename(state);
        }

        for members in self.members.values_mut() {
            for m in members {
                m.rename(state);
            }
        }

        for ty in self.assoc_types.values_mut() {
            ty.rename(state);
        }
    }
}

impl MirRenamable for MiddleTraitMember {
    fn rename(&mut self, state: &mut MirRenameState) {
        self.data_type.rename(state);
    }
}

impl MirRenamable for MiddleTrait {
    fn rename(&mut self, state: &mut MirRenameState) {
        for t in &mut self.implied_traits {
            t.rename(state);
        }

        for member in self.members.values_mut() {
            member.rename(state);
        }

        for assoc_type in self.assoc_types.values_mut() {
            assoc_type.rename(state);
        }
    }
}

impl MirRenamable for MiddleTypeDefType {
    fn rename(&mut self, state: &mut MirRenameState) {
        match self {
            MiddleTypeDefType::Enum { variants, .. } => {
                for (_, data_type) in variants {
                    if let Some(dt) = data_type {
                        dt.rename(state);
                    }
                }
            }
            MiddleTypeDefType::Struct(fields) => {
                for (_, (data_type, _)) in &mut fields.0 {
                    data_type.rename(state);
                }
            }
            MiddleTypeDefType::NewType(dt) => {
                dt.rename(state);
            }
            MiddleTypeDefType::Trait => {}
        }
    }
}

impl MirRenamable for MiddleObject {
    fn rename(&mut self, state: &mut MirRenameState) {
        self.object_type.rename(state);

        for (var_key, _) in self.variables.values_mut() {
            var_key.rename(state);
        }

        for t in &mut self.traits {
            t.rename(state);
        }
    }
}

impl MirRenamable for MiddleVariable {
    fn rename(&mut self, state: &mut MirRenameState) {
        self.key.rename(state);
        self.data_type.rename(state);
    }
}

impl MirRenamable for MiddleOverload {
    fn rename(&mut self, state: &mut MirRenameState) {
        self.return_type.rename(state);
        for p in &mut self.parameters {
            p.rename(state);
        }
    }
}
