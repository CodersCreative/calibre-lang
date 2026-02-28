use super::*;

impl<'a> BlockLoweringCtx<'a> {
    pub(super) fn symbol_tail(name: &str) -> &str {
        calibre_parser::qualified_name_tail(name)
    }

    pub(super) fn resolve_local_key(&self, name: &str) -> Option<String> {
        if self.locals.contains(name) {
            return Some(name.to_string());
        }
        if !name.contains(':') || name.contains("::") {
            return None;
        }

        let tail = Self::symbol_tail(name);
        self.locals
            .iter()
            .find(|candidate| Self::symbol_tail(candidate.as_str()) == tail)
            .cloned()
    }

    pub(super) fn resolve_mapped_reg(&self, name: &str) -> Option<Reg> {
        if let Some(reg) = self.map.get(name).copied() {
            return Some(reg);
        }
        if !name.contains(':') || name.contains("::") {
            return None;
        }

        let tail = Self::symbol_tail(name);
        self.map
            .iter()
            .find(|(candidate, _)| Self::symbol_tail(candidate.as_str()) == tail)
            .map(|(_, reg)| *reg)
    }

    pub(super) fn alloc_reg(&mut self) -> Reg {
        let r = *self.reg_count;
        *self.reg_count += 1;
        r
    }

    pub(super) fn add_literal(&mut self, lit: VMLiteral) -> u16 {
        match &lit {
            VMLiteral::Int(x) => {
                if let Some(idx) = self.int_literals.get(x).copied() {
                    return idx;
                }
            }
            VMLiteral::UInt(x) => {
                if let Some(idx) = self.uint_literals.get(x).copied() {
                    return idx;
                }
            }
            VMLiteral::Float(x) => {
                let bits = x.to_bits();
                if let Some(idx) = self.float_literals.get(&bits).copied() {
                    return idx;
                }
            }
            VMLiteral::Char(x) => {
                if let Some(idx) = self.char_literals.get(x).copied() {
                    return idx;
                }
            }
            VMLiteral::String(x) => {
                if let Some(idx) = self.string_literals.get(x).copied() {
                    return idx;
                }
            }
            _ => {}
        }
        self.block.local_literals.push(lit);
        let idx = (self.block.local_literals.len() - 1) as u16;
        match &self.block.local_literals[idx as usize] {
            VMLiteral::Int(x) => {
                self.int_literals.insert(*x, idx);
            }
            VMLiteral::UInt(x) => {
                self.uint_literals.insert(*x, idx);
            }
            VMLiteral::Float(x) => {
                self.float_literals.insert(x.to_bits(), idx);
            }
            VMLiteral::Char(x) => {
                self.char_literals.insert(*x, idx);
            }
            VMLiteral::String(x) => {
                self.string_literals.insert(x.clone(), idx);
            }
            _ => {}
        }
        idx
    }

    pub(super) fn add_string(&mut self, text: String) -> u16 {
        if let Some(idx) = self.string_map.get(&text).copied() {
            return idx;
        }
        self.block.local_strings.push(text);
        let idx = (self.block.local_strings.len() - 1) as u16;
        if let Some(text) = self.block.local_strings.get(idx as usize) {
            self.string_map.insert(text.clone(), idx);
        }
        idx
    }

    pub(super) fn emit(&mut self, instr: VMInstruction, span: Span) {
        self.block.instructions.push(instr);
        self.block.instruction_spans.push(span);
    }

    pub(super) fn lower_instr(&mut self, node: LirInstr, assigned: Option<Reg>, set_ret: bool) {
        match node.node_type {
            LirNodeType::Noop => {}
            LirNodeType::Declare { dest, value } => {
                let reg = self.lower_node(*value, node.span);
                if !self.is_global {
                    let target = assigned.unwrap_or(reg);
                    if target != reg {
                        self.emit(
                            VMInstruction::Copy {
                                dst: target,
                                src: reg,
                            },
                            node.span,
                        );
                    }
                    let name_idx = self.add_string(dest.to_string());
                    self.map.insert(dest.to_string(), target);
                    self.emit(
                        VMInstruction::SetLocalName {
                            name: name_idx,
                            src: target,
                        },
                        node.span,
                    );
                    let name = self.add_string(dest.to_string());
                    self.emit(VMInstruction::StoreGlobal { name, src: target }, node.span);
                } else {
                    let name = self.add_string(dest.to_string());
                    self.emit(VMInstruction::StoreGlobal { name, src: reg }, node.span);
                }
            }
            LirNodeType::Assign { dest, value } => match dest {
                LirLValue::Var(dest) => {
                    let reg = self.lower_node(*value, node.span);
                    if !self.is_global && self.locals.contains(dest.as_ref()) {
                        let target = assigned.unwrap_or(reg);
                        if target != reg {
                            self.emit(
                                VMInstruction::Copy {
                                    dst: target,
                                    src: reg,
                                },
                                node.span,
                            );
                        }
                        let name_idx = self.add_string(dest.to_string());
                        self.map.insert(dest.to_string(), target);
                        self.emit(
                            VMInstruction::SetLocalName {
                                name: name_idx,
                                src: target,
                            },
                            node.span,
                        );
                        let name = self.add_string(dest.to_string());
                        self.emit(VMInstruction::StoreGlobal { name, src: target }, node.span);
                    } else {
                        let name = self.add_string(dest.to_string());
                        self.emit(VMInstruction::StoreGlobal { name, src: reg }, node.span);
                    }
                }
                LirLValue::Ptr(ptr) => {
                    let value_reg = self.lower_node(*value, node.span);
                    match *ptr {
                        LirNodeType::Member(base, member) => {
                            let base_reg = self.lower_node(*base, node.span);
                            let member = self.add_string(member.to_string());
                            self.emit(
                                VMInstruction::SetMember {
                                    target: base_reg,
                                    member,
                                    value: value_reg,
                                },
                                node.span,
                            );
                        }
                        LirNodeType::Index(base, index) => {
                            let index_reg = self.lower_node(*index, node.span);
                            match *base {
                                LirNodeType::Member(owner, member) => {
                                    let owner_reg = self.lower_node(*owner, node.span);
                                    let member_idx = self.add_string(member.to_string());
                                    let member_val_reg = self.alloc_reg();
                                    self.emit(
                                        VMInstruction::LoadMember {
                                            dst: member_val_reg,
                                            value: owner_reg,
                                            member: member_idx,
                                        },
                                        node.span,
                                    );
                                    self.emit(
                                        VMInstruction::SetIndex {
                                            target: member_val_reg,
                                            index: index_reg,
                                            value: value_reg,
                                        },
                                        node.span,
                                    );
                                    self.emit(
                                        VMInstruction::SetMember {
                                            target: owner_reg,
                                            member: member_idx,
                                            value: member_val_reg,
                                        },
                                        node.span,
                                    );
                                }
                                other_base => {
                                    let base_reg = self.lower_node(other_base, node.span);
                                    self.emit(
                                        VMInstruction::SetIndex {
                                            target: base_reg,
                                            index: index_reg,
                                            value: value_reg,
                                        },
                                        node.span,
                                    );
                                }
                            }
                        }
                        other => {
                            let target_reg = self.lower_node(other, node.span);
                            self.emit(
                                VMInstruction::SetRef {
                                    target: target_reg,
                                    value: value_reg,
                                },
                                node.span,
                            );
                        }
                    }
                }
            },
            other => {
                let reg = self.lower_node(other, node.span);
                if set_ret {
                    if reg != self.ret_reg {
                        self.emit(
                            VMInstruction::Copy {
                                dst: self.ret_reg,
                                src: reg,
                            },
                            node.span,
                        );
                    }
                }
            }
        }
    }

    pub(super) fn lower_node(&mut self, node: LirNodeType, span: Span) -> Reg {
        match node {
            LirNodeType::Noop => self.null_reg,
            LirNodeType::Spawn { callee } => {
                let callee = self.lower_node(*callee, span);
                let dst = self.alloc_reg();
                self.emit(VMInstruction::Spawn { dst, callee }, span);
                dst
            }
            LirNodeType::Literal(x) => {
                if matches!(x, LirLiteral::Null) {
                    return self.null_reg;
                }
                let lit = self.add_literal(x.into());
                let dst = self.alloc_reg();
                self.emit(VMInstruction::LoadLiteral { dst, literal: lit }, span);
                dst
            }
            LirNodeType::Call { caller, args } => {
                let mut arg_regs = Vec::with_capacity(args.len());
                for arg in args {
                    arg_regs.push(self.lower_node(arg, span));
                }
                let dst = self.alloc_reg();
                match *caller {
                    LirNodeType::Load(name) | LirNodeType::Move(name) if !name.contains(':') => {
                        let current_name = self.current_fn_name.as_str();
                        let current_short = self.current_fn_short.as_str();
                        if name.as_ref() == current_name || name.as_ref() == current_short {
                            self.emit(
                                VMInstruction::CallSelf {
                                    dst,
                                    args: arg_regs,
                                },
                                span,
                            );
                        } else {
                            let idx = self.add_string(name.to_string());
                            self.emit(
                                VMInstruction::CallDirect {
                                    dst,
                                    name: idx,
                                    args: arg_regs,
                                },
                                span,
                            );
                        }
                    }
                    other => {
                        let callee = self.lower_node(other, span);
                        self.emit(
                            VMInstruction::Call {
                                dst,
                                callee,
                                args: arg_regs,
                            },
                            span,
                        );
                    }
                }
                dst
            }
            LirNodeType::List { elements, .. } => {
                let mut regs = Vec::with_capacity(elements.len());
                for item in elements {
                    regs.push(self.lower_node(item, span));
                }
                let dst = self.alloc_reg();
                self.emit(VMInstruction::List { dst, items: regs }, span);
                dst
            }
            LirNodeType::Move(name) => {
                if self.captures.contains(name.as_ref())
                    || self.resolve_local_key(name.as_ref()).is_none()
                {
                    let idx = self.add_string(name.to_string());
                    let dst = self.alloc_reg();
                    self.emit(VMInstruction::MoveGlobal { dst, name: idx }, span);
                    dst
                } else {
                    let reg = self
                        .resolve_mapped_reg(name.as_ref())
                        .unwrap_or(self.null_reg);
                    let key = self
                        .resolve_local_key(name.as_ref())
                        .unwrap_or_else(|| name.to_string());
                    self.map.insert(key, self.null_reg);
                    reg
                }
            }
            LirNodeType::Drop(name) => {
                if self.captures.contains(name.as_ref())
                    || self.resolve_local_key(name.as_ref()).is_none()
                {
                    let idx = self.add_string(name.to_string());
                    self.emit(VMInstruction::DropGlobal { name: idx }, span);
                } else {
                    let key = self
                        .resolve_local_key(name.as_ref())
                        .unwrap_or_else(|| name.to_string());
                    self.map.insert(key, self.null_reg);
                }
                self.null_reg
            }
            LirNodeType::Load(name) => {
                if self.resolve_local_key(name.as_ref()).is_some() {
                    if let Some(reg) = self.resolve_mapped_reg(name.as_ref()) {
                        reg
                    } else {
                        let idx = self.add_string(name.to_string());
                        let dst = self.alloc_reg();
                        self.emit(VMInstruction::LoadGlobal { dst, name: idx }, span);
                        dst
                    }
                } else {
                    let idx = self.add_string(name.to_string());
                    let dst = self.alloc_reg();
                    self.emit(VMInstruction::LoadGlobal { dst, name: idx }, span);
                    dst
                }
            }
            LirNodeType::Aggregate { name, fields } => {
                let mut layout = Vec::new();
                let mut values = Vec::new();
                for (k, item) in fields.0 {
                    values.push(self.lower_node(item, span));
                    layout.push(k.to_string());
                }
                self.block.aggregate_layouts.push(AggregateLayout {
                    name,
                    members: layout,
                });
                let index = self.block.aggregate_layouts.len() - 1;
                let dst = self.alloc_reg();
                self.emit(
                    VMInstruction::Aggregate {
                        dst,
                        layout: index as u16,
                        fields: values,
                    },
                    span,
                );
                dst
            }
            LirNodeType::Boolean {
                left,
                right,
                operator,
            } => {
                let left = self.lower_node(*left, span);
                let right = self.lower_node(*right, span);
                let dst = self.alloc_reg();
                self.emit(
                    VMInstruction::Boolean {
                        dst,
                        op: operator,
                        left,
                        right,
                    },
                    span,
                );
                dst
            }
            LirNodeType::Comparison {
                left,
                right,
                operator,
            } => {
                let left = self.lower_node(*left, span);
                let right = self.lower_node(*right, span);
                let dst = self.alloc_reg();
                self.emit(
                    VMInstruction::Comparison {
                        dst,
                        op: operator,
                        left,
                        right,
                    },
                    span,
                );
                dst
            }
            LirNodeType::Binary {
                left,
                right,
                operator,
            } => {
                let left = self.lower_node(*left, span);
                let right = self.lower_node(*right, span);
                let dst = self.alloc_reg();
                self.emit(VMInstruction::AccLoad { src: left }, span);
                self.emit(
                    VMInstruction::AccBinary {
                        op: operator,
                        right,
                    },
                    span,
                );
                self.emit(VMInstruction::AccStore { dst }, span);
                dst
            }
            LirNodeType::Range {
                from,
                to,
                inclusive,
            } => {
                let from = self.lower_node(*from, span);
                let to = self.lower_node(*to, span);
                let dst = self.alloc_reg();
                self.emit(
                    VMInstruction::Range {
                        dst,
                        from,
                        to,
                        inclusive,
                    },
                    span,
                );
                dst
            }
            LirNodeType::Deref(x) => {
                let value = self.lower_node(*x, span);
                let dst = self.alloc_reg();
                self.emit(VMInstruction::Deref { dst, value }, span);
                dst
            }
            LirNodeType::Ref(x) => match *x {
                LirNodeType::Load(name) => {
                    if !self.captures.contains(name.as_ref())
                        && (self.resolve_local_key(name.as_ref()).is_some()
                            || self.resolve_mapped_reg(name.as_ref()).is_some())
                    {
                        let src = self
                            .resolve_mapped_reg(name.as_ref())
                            .unwrap_or(self.null_reg);
                        let dst = self.alloc_reg();
                        self.emit(VMInstruction::LoadRegRef { dst, src }, span);
                        dst
                    } else {
                        let idx = self.add_string(name.to_string());
                        let dst = self.alloc_reg();
                        self.emit(VMInstruction::LoadGlobalRef { dst, name: idx }, span);
                        dst
                    }
                }
                other => {
                    let value = self.lower_node(other, span);
                    let dst = self.alloc_reg();
                    self.emit(VMInstruction::Ref { dst, value }, span);
                    dst
                }
            },
            LirNodeType::As(value, data_type, failure_mode) => {
                let src = self.lower_node(*value, span);
                let dst = self.alloc_reg();
                self.emit(
                    VMInstruction::As {
                        dst,
                        src,
                        data_type,
                        failure_mode,
                    },
                    span,
                );
                dst
            }
            LirNodeType::Is(value, data_type) => {
                let src = self.lower_node(*value, span);
                let dst = self.alloc_reg();
                self.emit(
                    VMInstruction::Is {
                        dst,
                        src,
                        data_type,
                    },
                    span,
                );
                dst
            }
            LirNodeType::Enum {
                name,
                variant,
                payload,
            } => {
                let payload = payload.map(|v| self.lower_node(*v, span));
                let name = self.add_string(name.to_string());
                let dst = self.alloc_reg();
                self.emit(
                    VMInstruction::Enum {
                        dst,
                        name,
                        variant: variant as u16,
                        payload,
                    },
                    span,
                );
                dst
            }
            LirNodeType::Index(value, index) => {
                let index = self.lower_node(*index, span);
                let value = self.lower_node(*value, span);
                let dst = self.alloc_reg();
                self.emit(VMInstruction::Index { dst, value, index }, span);
                dst
            }
            LirNodeType::Member(value, member) => {
                let value = self.lower_node(*value, span);
                let member = self.add_string(member.to_string());
                let dst = self.alloc_reg();
                self.emit(VMInstruction::LoadMember { dst, value, member }, span);
                dst
            }
            LirNodeType::Closure { label, captures } => {
                self.block.local_literals.push(VMLiteral::Closure {
                    label: label.to_string(),
                    captures: captures.into_iter().map(|x| x.to_string()).collect(),
                });
                let lit = (self.block.local_literals.len() - 1) as u16;
                let dst = self.alloc_reg();
                self.emit(VMInstruction::LoadLiteral { dst, literal: lit }, span);
                dst
            }
            LirNodeType::ExternFunction {
                abi,
                library,
                symbol,
                parameters,
                return_type,
            } => {
                self.block.local_literals.push(VMLiteral::ExternFunction {
                    abi: abi.to_string(),
                    library: library.to_string(),
                    symbol: symbol.to_string(),
                    parameters,
                    return_type,
                });
                let lit = (self.block.local_literals.len() - 1) as u16;
                let dst = self.alloc_reg();
                self.emit(VMInstruction::LoadLiteral { dst, literal: lit }, span);
                dst
            }
            LirNodeType::Assign { .. } | LirNodeType::Declare { .. } => self.null_reg,
        }
    }

    pub(super) fn lower_terminator(&mut self, node: LirTerminator) {
        match node {
            LirTerminator::Jump { span, target } => {
                self.emit(VMInstruction::Jump(target), span);
            }
            LirTerminator::Branch {
                span,
                condition,
                then_block,
                else_block,
            } => {
                let cond = self.lower_node(condition, span);
                let cond_reg = if cond == self.ret_reg {
                    let tmp = self.alloc_reg();
                    self.emit(
                        VMInstruction::Copy {
                            dst: tmp,
                            src: cond,
                        },
                        span,
                    );
                    tmp
                } else {
                    cond
                };
                self.emit(
                    VMInstruction::Branch {
                        cond: cond_reg,
                        then_block,
                        else_block,
                    },
                    span,
                );
            }
            LirTerminator::Return { span, value } => {
                let value = match value {
                    Some(LirNodeType::Drop(name)) => {
                        let _ = self.lower_node(LirNodeType::Drop(name), span);
                        Some(self.ret_reg)
                    }
                    Some(v) => Some(self.lower_node(v, span)),
                    None => Some(self.ret_reg),
                };
                self.emit(VMInstruction::Return { value }, span);
            }
        }
    }
}
