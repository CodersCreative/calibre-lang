use super::*;
use tracing::{instrument, trace};

pub mod access;
pub mod expressions;
pub mod flow;
pub mod literals;
pub mod memory;
pub mod statements;

#[allow(unused)]
pub trait VMLowering {
    fn lower<'a>(self, env: &mut BlockLoweringCtx<'a>, span: Span) -> Reg;

    #[inline(always)]
    fn lower_to<'a>(self, env: &mut BlockLoweringCtx<'a>, target: Reg, span: Span)
    where
        Self: Sized,
    {
        let reg = self.lower(env, span);
        if reg != target {
            env.emit(
                VMInstruction::Copy {
                    dst: target,
                    src: reg,
                },
                span,
            );
        }
    }

    #[inline(always)]
    fn lower_instr<'a>(
        self,
        env: &mut BlockLoweringCtx<'a>,
        _assigned: Option<Reg>,
        set_ret: bool,
        span: Span,
    ) where
        Self: Sized,
    {
        let reg = self.lower(env, span);
        if set_ret && reg != env.ret_reg {
            env.emit(
                VMInstruction::Copy {
                    dst: env.ret_reg,
                    src: reg,
                },
                span,
            );
        }
    }
}

impl<'a> BlockLoweringCtx<'a> {
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

    #[instrument(skip_all)]
    pub(super) fn lower_instr(&mut self, node: LirNode, assigned: Option<Reg>, set_ret: bool) {
        trace!("lowering LIR node to VM instruction");
        let span = node.span;
        match node.node_type {
            LirNodeType::Noop => {}
            LirNodeType::Declare(x) => x.lower_instr(self, assigned, set_ret, span),
            LirNodeType::Assign(x) => x.lower_instr(self, assigned, set_ret, span),
            other => {
                let reg = self.lower_node(other, node.span);
                if set_ret && reg != self.ret_reg {
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

    #[instrument(skip_all)]
    pub(super) fn lower_node_to(&mut self, node: LirNodeType, target: Reg, span: Span) {
        trace!("lowering LIR node to target register");
        match node {
            LirNodeType::Literal(x) => x.lower_to(self, target, span),
            LirNodeType::List(x) => x.lower_to(self, target, span),
            LirNodeType::Aggregate(x) => x.lower_to(self, target, span),
            LirNodeType::Enum(x) => x.lower_to(self, target, span),
            other => {
                let reg = self.lower_node(other, span);
                if reg != target {
                    self.emit(
                        VMInstruction::Copy {
                            dst: target,
                            src: reg,
                        },
                        span,
                    );
                }
            }
        }
    }

    #[instrument(skip_all)]
    pub(super) fn lower_node(&mut self, node: LirNodeType, span: Span) -> Reg {
        trace!("lowering LIR node to VM instruction");
        match node {
            LirNodeType::Noop => self.null_reg,

            LirNodeType::Spawn(x) => x.lower(self, span),
            LirNodeType::Move(x) => x.lower(self, span),
            LirNodeType::Drop(x) => x.lower(self, span),
            LirNodeType::Load(x) => x.lower(self, span),

            LirNodeType::Literal(x) => x.lower(self, span),
            LirNodeType::List(x) => x.lower(self, span),
            LirNodeType::Aggregate(x) => x.lower(self, span),
            LirNodeType::Enum(x) => x.lower(self, span),

            LirNodeType::Range(x) => x.lower(self, span),
            LirNodeType::Closure(x) => x.lower(self, span),

            LirNodeType::Boolean(x) => x.lower(self, span),
            LirNodeType::Comparison(x) => x.lower(self, span),
            LirNodeType::Binary(x) => x.lower(self, span),
            LirNodeType::As(x) => x.lower(self, span),
            LirNodeType::Is(x) => x.lower(self, span),

            LirNodeType::Call(x) => x.lower(self, span),
            LirNodeType::Deref(x) => x.lower(self, span),
            LirNodeType::Ref(x) => x.lower(self, span),
            LirNodeType::RefLoad(x) => x.lower(self, span),
            LirNodeType::Index(x) => x.lower(self, span),
            LirNodeType::Member(x) => x.lower(self, span),

            LirNodeType::ExternFunction(x) => x.lower(self, span),
            LirNodeType::Assign(x) => x.lower(self, span),
            LirNodeType::Declare(x) => x.lower(self, span),
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
