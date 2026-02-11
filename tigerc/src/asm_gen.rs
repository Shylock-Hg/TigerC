use std::vec::Vec;

use crate::asm;
use crate::canon;
use crate::frame::Frame;
use crate::ir;
use crate::temp::Temp;

pub struct Gen<F: Frame> {
    insts: Vec<asm::Instruction>,
    _frame: std::marker::PhantomData<F>,
}

impl<F: Frame> Gen<F> {
    pub fn new() -> Self {
        Gen {
            insts: Vec::new(),
            _frame: std::marker::PhantomData,
        }
    }

    pub fn result(self) -> Vec<asm::Instruction> {
        self.insts
    }

    pub fn munch_trace(&mut self, trace: canon::Trace) {
        for block in trace.blocks {
            self.munch_block(block);
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
                let dst = self.munch_expression(dst);
                let val = self.munch_expression(val);
                let inst = asm::Instruction::Move {
                    assembly: "mov `d0, `s0".to_string(),
                    destination: vec![dst],
                    source: vec![val],
                };
                self.emit(inst);
            }
            ir::Statement::Exp(e) => {
                self.munch_expression(e);
            }
            ir::Statement::Jump { exp, labels } => {
                assert!(matches!(exp, ir::Exp::Name(_)));
                let exp = self.munch_expression(exp);
                let inst = asm::Instruction::Operation {
                    assembly: "jmp `s0".to_string(),
                    destination: vec![],
                    source: vec![exp],
                    jump: Some(labels),
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

    fn munch_expression(&mut self, e: ir::Exp) -> Temp {
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
                let function_temp = self.munch_expression(*func);
                let argument_temps = args
                    .into_iter()
                    .map(|arg| self.munch_expression(arg))
                    .collect::<Vec<_>>();
                // call function_temp
                let inst = asm::Instruction::Operation {
                    assembly: "call `s0".to_string(),
                    // need return/arguments for live analysis
                    destination: vec![F::return_value()],
                    source: {
                        let mut v = vec![function_temp];
                        v.extend(argument_temps);
                        v
                    },
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
                    ir::BinOp::Divide => "idiv `d0, `s0",
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
                    source: vec![right_temp],
                    jump: None,
                };
                self.emit(inst);
            }
        }
        temp
    }

    fn emit(&mut self, i: asm::Instruction) {
        self.insts.push(i);
    }
}
