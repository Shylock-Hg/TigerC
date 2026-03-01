use std::vec::Vec;

use crate::amd64::FrameAmd64;
use crate::asm;
use crate::asm::Instruction;
use crate::canon;
use crate::frame::Frame;
use crate::ir;
use crate::temp::{Label, Temp};

pub struct Gen<F: Frame> {
    insts: Vec<asm::Instruction>,
    trace: asm::Trace,
    _frame: std::marker::PhantomData<F>,
}

// TODO: write a optimization rule to remove unused move/temp
impl<F: Frame> Gen<F> {
    pub fn new(done_label: Label) -> Self {
        Gen {
            insts: Vec::new(),
            trace: asm::Trace::new(done_label),
            _frame: std::marker::PhantomData,
        }
    }

    pub fn result(self) -> asm::Trace {
        self.trace
    }

    pub fn raw_result(self) -> Vec<Instruction> {
        self.insts
    }

    pub fn get_raw_result(&self) -> &Vec<Instruction> {
        &self.insts
    }

    pub fn munch_trace(&mut self, trace: canon::Trace) {
        for block in trace.blocks {
            self.munch_block(block);
            self.trace.add_block(asm::Block {
                instructions: self.insts.drain(..).collect(),
            });
        }
    }

    pub fn munch_block(&mut self, block: canon::Block) {
        for stmt in block.result() {
            self.munch_statement(stmt);
        }
    }

    pub fn munch_statement(&mut self, s: ir::Statement) {
        match s {
            ir::Statement::Move { dst, val } => {
                let inst = match (dst, val) {
                    (ir::Exp::Mem(dst), val) => {
                        let val_temp = self.munch_expression(val);
                        let dst_temp = self.munch_expression(*dst);
                        asm::Instruction::Move {
                            assembly: "mov [`d0], `s0".to_string(),
                            destination: vec![dst_temp],
                            source: vec![val_temp],
                        }
                    }
                    (dst, ir::Exp::Mem(val)) => {
                        let val_temp = self.munch_expression(*val);
                        let dst_temp = self.munch_expression(dst);
                        asm::Instruction::Move {
                            assembly: "mov `d0, [`s0]".to_string(),
                            destination: vec![dst_temp],
                            source: vec![val_temp],
                        }
                    }
                    (dst, val) => {
                        let val_temp = self.munch_expression(val);
                        let dst_temp = self.munch_expression(dst);
                        asm::Instruction::Move {
                            assembly: "mov `d0, `s0".to_string(),
                            destination: vec![dst_temp],
                            source: vec![val_temp],
                        }
                    }
                };
                self.emit(inst);
            }
            ir::Statement::Exp(e) => {
                self.munch_expression(e);
            }
            ir::Statement::Jump { exp, .. } => {
                let l = match exp {
                    ir::Exp::Name(l) => l,
                    _ => unreachable!(),
                };
                let inst = asm::Instruction::Operation {
                    assembly: format!("jmp {}", l),
                    destination: vec![],
                    source: vec![],
                    jump: Some(vec![l]),
                };
                self.emit(inst);
            }
            ir::Statement::CJump {
                op,
                left,
                right,
                then,
                else_,
            } => {
                let inst = asm::Instruction::Operation {
                    assembly: "cmp `s0, `s1".to_string(),
                    destination: vec![],
                    source: vec![self.munch_expression(left), self.munch_expression(right)],
                    jump: None,
                };
                self.emit(inst);

                let opcode = match op {
                    ir::CompareOp::Eq => "je",
                    ir::CompareOp::Ne => "jne",
                    ir::CompareOp::SignedGt => "jl",
                    ir::CompareOp::SignedGe => "jle",
                    ir::CompareOp::SignedLt => "jg",
                    ir::CompareOp::SignedLe => "jge",
                    ir::CompareOp::UnsignedGt => "jb",
                    ir::CompareOp::UnsignedGe => "jbe",
                    ir::CompareOp::UnsignedLt => "ja",
                    ir::CompareOp::UnsignedLe => "jae",
                };
                let inst = asm::Instruction::Operation {
                    assembly: std::format!("{} {}", opcode, then),
                    destination: vec![],
                    source: vec![],
                    jump: Some(vec![then, else_]),
                };
                self.emit(inst);
            }
            ir::Statement::Seq { .. } => unreachable!(),
            ir::Statement::Label(l) => {
                let inst = asm::Instruction::Label {
                    assembly: std::format!("{}:", l),
                    label: l,
                };
                self.emit(inst);
            }
        }
    }

    pub fn munch_expression(&mut self, e: ir::Exp) -> Temp {
        let temp = Temp::new();
        match e {
            ir::Exp::Const(i) => {
                // mov temp, i
                let inst = asm::Instruction::Move {
                    assembly: std::format!("mov `d0, {}", i),
                    destination: vec![temp],
                    source: vec![],
                };
                self.emit(inst);
            }
            ir::Exp::Temp(t) => {
                return t;
            }
            ir::Exp::Name(l) => {
                // mov temp, l
                let inst = asm::Instruction::Move {
                    assembly: std::format!("mov `d0, {}", l),
                    destination: vec![temp],
                    source: vec![],
                };
                self.emit(inst);
            }
            ir::Exp::Mem(addr) => {
                let addr_temp = self.munch_expression(*addr);
                // mov temp, addr_temp
                let inst = asm::Instruction::Move {
                    assembly: "mov `d0, `s0".to_string(),
                    destination: vec![temp],
                    source: vec![addr_temp],
                };
                self.emit(inst);
            }
            ir::Exp::ExpSeq { .. } => {
                unreachable!();
            }
            ir::Exp::Call { func, args } => {
                let args_len = args.len();
                let func_label = match *func {
                    ir::Exp::Name(l) => l,
                    _ => todo!(), // TODO need this?
                };
                let argument_temps = self.munch_args(args);
                let argument_temps_len = argument_temps.len();
                // call function_temp
                let inst = asm::Instruction::Operation {
                    assembly: format!("call {}", func_label),
                    // need return/arguments for live analysis
                    destination: vec![F::return_value()],
                    source: argument_temps,
                    jump: None,
                };
                self.emit(inst);

                // relax return value register
                let inst = asm::Instruction::Move {
                    assembly: "mov `d0, `s0".to_string(),
                    destination: vec![temp],
                    source: vec![F::return_value()],
                };
                self.emit(inst);

                // recall frame for spilled arguments
                let spilled = args_len - argument_temps_len;
                if spilled > 0 {
                    self.emit(Instruction::Operation {
                        assembly: format!("add `d0, {}", spilled * F::word_size() as usize),
                        destination: vec![F::sp()],
                        source: vec![F::sp()],
                        jump: None,
                    });
                }
            }
            ir::Exp::BinOp {
                op: ir::BinOp::Divide,
                left,
                right,
            } => {
                let left_temp = self.munch_expression(*left);
                let right_temp = self.munch_expression(*right);

                // mov rdx 0
                let inst = asm::Instruction::Move {
                    assembly: "mov `d0, 0".to_string(),
                    destination: vec![FrameAmd64::rdx()],
                    source: vec![],
                };
                self.emit(inst);

                // mov rax left_temp
                let inst = asm::Instruction::Move {
                    assembly: "mov `d0, `s0".to_string(),
                    destination: vec![FrameAmd64::rax()],
                    source: vec![left_temp],
                };
                self.emit(inst);

                // idiv right_temp
                let inst = asm::Instruction::Operation {
                    assembly: "idiv `s0".to_string(),
                    destination: vec![FrameAmd64::rax(), FrameAmd64::rdx()],
                    source: vec![right_temp, FrameAmd64::rax(), FrameAmd64::rdx()],
                    jump: None,
                };
                self.emit(inst);

                // mov temp rax
                let inst = asm::Instruction::Move {
                    assembly: "mov `d0, `s0".to_string(),
                    destination: vec![temp],
                    source: vec![FrameAmd64::rax()],
                };
                self.emit(inst);
            }
            ir::Exp::BinOp { op, left, right } => {
                let left_temp = self.munch_expression(*left);
                let right_temp = self.munch_expression(*right);

                // mov temp left_temp
                let inst = asm::Instruction::Move {
                    assembly: "mov `d0, `s0".to_string(),
                    destination: vec![temp],
                    source: vec![left_temp],
                };
                self.emit(inst);

                let assembly = match op {
                    ir::BinOp::Plus => "add `d0, `s0",
                    ir::BinOp::Minus => "sub `d0, `s0",
                    ir::BinOp::Multiply => "imul `d0, `s0",
                    ir::BinOp::Divide => unreachable!(),
                    ir::BinOp::And => "and `d0, `s0",
                    ir::BinOp::Or => "or `d0, `s0",
                    ir::BinOp::Xor => "xor `d0, `s0",
                    ir::BinOp::LShift => "shl `d0, `s0",
                    ir::BinOp::RShift => "shr `d0, `s0",
                    ir::BinOp::ARShift => "sar `d0, `s0",
                };
                let inst = asm::Instruction::Operation {
                    assembly: assembly.to_string(),
                    destination: vec![temp],
                    source: vec![right_temp, temp],
                    jump: None,
                };
                self.emit(inst);
            }
        }
        temp
    }

    fn munch_args(&mut self, args: Vec<ir::Exp>) -> Vec<Temp> {
        let mut temps = vec![];

        let arg_registers = F::arg_registers().into_iter();
        let mut args = args.into_iter();
        for arg_reg in arg_registers {
            if let Some(arg) = args.next() {
                let arg_temp = self.munch_expression(arg);
                self.emit(Instruction::Move {
                    assembly: "mov `d0, `s0".to_string(),
                    destination: vec![arg_reg],
                    source: vec![arg_temp],
                });
                temps.push(arg_reg);
            }
        }
        // spilled in reverse to be compatible with C-ABI
        let insts = args
            .map(|arg| Instruction::Operation {
                assembly: "push `s0".to_string(),
                destination: vec![F::sp()],
                source: vec![self.munch_expression(arg), F::sp()],
                jump: None,
            })
            .rev()
            .collect::<Vec<_>>();

        for inst in insts {
            self.emit(inst);
        }

        temps
    }

    pub fn emit(&mut self, i: asm::Instruction) -> usize {
        self.insts.push(i);
        self.insts.len() - 1
    }

    pub fn get_mut(&mut self, i: usize) -> &mut asm::Instruction {
        self.insts.get_mut(i).unwrap()
    }
}
