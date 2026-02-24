use std::sync::Once;

use indexmap::IndexMap;

use crate::asm::Trace;
use crate::frame::{Access, Frame, Variable};
use crate::temp::{Label, Temp};
use crate::{asm, ir};
use crate::{ident_pool, ir_gen};

// registers
static mut RBP: Option<Temp> = None;
static mut RSP: Option<Temp> = None;
static mut RAX: Option<Temp> = None;
static mut RBX: Option<Temp> = None;
static mut RCX: Option<Temp> = None;
static mut RDX: Option<Temp> = None;
static mut RSI: Option<Temp> = None;
static mut RDI: Option<Temp> = None;
static mut R8: Option<Temp> = None;
static mut R9: Option<Temp> = None;
static mut R10: Option<Temp> = None;
static mut R11: Option<Temp> = None;
static mut R12: Option<Temp> = None;
static mut R13: Option<Temp> = None;
static mut R14: Option<Temp> = None;
static mut R15: Option<Temp> = None;
static ONCE: Once = Once::new();

fn initialize() {
    unsafe {
        RBP = Some(Temp::new_named(ident_pool::symbol("rbp")));
        RSP = Some(Temp::new_named(ident_pool::symbol("rsp")));
        RAX = Some(Temp::new_named(ident_pool::symbol("rax")));
        RBX = Some(Temp::new_named(ident_pool::symbol("rbx")));
        RCX = Some(Temp::new_named(ident_pool::symbol("rcx")));
        RDX = Some(Temp::new_named(ident_pool::symbol("rdx")));
        RDI = Some(Temp::new_named(ident_pool::symbol("rdi")));
        RSI = Some(Temp::new_named(ident_pool::symbol("rsi")));
        R8 = Some(Temp::new_named(ident_pool::symbol("r8")));
        R9 = Some(Temp::new_named(ident_pool::symbol("r9")));
        R10 = Some(Temp::new_named(ident_pool::symbol("r10")));
        R11 = Some(Temp::new_named(ident_pool::symbol("r11")));
        R12 = Some(Temp::new_named(ident_pool::symbol("r12")));
        R13 = Some(Temp::new_named(ident_pool::symbol("r13")));
        R14 = Some(Temp::new_named(ident_pool::symbol("r14")));
        R15 = Some(Temp::new_named(ident_pool::symbol("r15")));
    }
}

// 8 bytes in amd64
const WORD_SIZE: i64 = 8;

// frame of a function
#[derive(Debug)]
pub struct FrameAmd64 {
    // function name
    name: Label,
    // parameters
    parameters: Vec<Variable>,
    // current offset of locals to frame pointer
    offset: i64,
}

impl PartialEq for FrameAmd64 {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

impl Eq for FrameAmd64 {}

impl FrameAmd64 {
    pub fn arg_registers() -> Vec<Temp> {
        vec![
            Self::rdi(),
            Self::rsi(),
            Self::rdx(),
            Self::rcx(),
            Self::r8(),
            Self::r9(),
        ]
    }

    fn callee_saved_registers() -> Vec<Temp> {
        vec![
            Self::rbx(),
            Self::r12(),
            Self::r13(),
            Self::r14(),
            Self::r15(),
        ]
    }

    fn special_registers() -> Vec<Temp> {
        vec![Self::rax(), Self::rbp(), Self::rsp()]
    }

    fn caller_saved_registers() -> Vec<Temp> {
        vec![Self::r10(), Self::r11()]
    }

    // pub fn calldefs() -> Vec<Temp> {
    // let mut registers = Self::caller_saved_registers();
    // registers.extend(Self::arg_registers());
    // registers.push(Self::return_value());
    // registers
    // }

    pub fn rsp() -> Temp {
        ONCE.call_once(initialize);
        unsafe { RSP.expect("temp") }
    }

    fn rbp() -> Temp {
        ONCE.call_once(initialize);
        unsafe { RBP.expect("temp") }
    }

    fn rdi() -> Temp {
        ONCE.call_once(initialize);
        unsafe { RDI.expect("temp") }
    }

    fn rsi() -> Temp {
        ONCE.call_once(initialize);
        unsafe { RSI.expect("temp") }
    }

    pub fn rax() -> Temp {
        ONCE.call_once(initialize);
        unsafe { RAX.expect("temp") }
    }

    fn rbx() -> Temp {
        ONCE.call_once(initialize);
        unsafe { RBX.expect("temp") }
    }

    fn rcx() -> Temp {
        ONCE.call_once(initialize);
        unsafe { RCX.expect("temp") }
    }

    pub fn rdx() -> Temp {
        ONCE.call_once(initialize);
        unsafe { RDX.expect("temp") }
    }

    fn r8() -> Temp {
        ONCE.call_once(initialize);
        unsafe { R8.expect("temp") }
    }

    fn r9() -> Temp {
        ONCE.call_once(initialize);
        unsafe { R9.expect("temp") }
    }

    fn r10() -> Temp {
        ONCE.call_once(initialize);
        unsafe { R10.expect("temp") }
    }

    fn r11() -> Temp {
        ONCE.call_once(initialize);
        unsafe { R11.expect("temp") }
    }

    fn r12() -> Temp {
        ONCE.call_once(initialize);
        unsafe { R12.expect("temp") }
    }

    fn r13() -> Temp {
        ONCE.call_once(initialize);
        unsafe { R13.expect("temp") }
    }

    fn r14() -> Temp {
        ONCE.call_once(initialize);
        unsafe { R14.expect("temp") }
    }

    fn r15() -> Temp {
        ONCE.call_once(initialize);
        unsafe { R15.expect("temp") }
    }
}

impl FrameAmd64 {
    #[must_use]
    fn allocate_variable(&mut self, var: ir::Variable) -> Variable {
        if var.0 {
            self.offset -= WORD_SIZE;
            Variable {
                access: Access::Frame(self.offset),
            }
        } else {
            Variable {
                access: Access::Register(Temp::new()),
            }
        }
    }
}

impl Frame for FrameAmd64 {
    fn new(name: Label, parameters: IndexMap<ir::LowerIdent, ir::Variable>) -> Self {
        // for the static link.
        let mut parameters = parameters;
        parameters.insert(
            ir::LowerIdent::new_named(ident_pool::symbol("__static_link")),
            ir::Variable(true),
        );

        let mut f = FrameAmd64 {
            name,
            parameters: Default::default(),
            offset: 0,
        };
        let parameters = parameters
            .into_iter()
            .map(|(_name, v)| f.allocate_variable(v))
            .collect::<Vec<_>>();
        f.parameters = parameters;
        f
    }

    fn name(&self) -> &Label {
        &self.name
    }

    fn fp() -> Temp {
        Self::rbp()
    }

    fn sp() -> Temp {
        Self::rsp()
    }

    fn return_value() -> Temp {
        Self::rax()
    }

    fn colors() -> Vec<Temp> {
        vec![
            Self::rbp(),
            Self::rsp(),
            Self::rax(),
            Self::rbx(),
            Self::rcx(),
            Self::rdx(),
            Self::rdi(),
            Self::rsi(),
            Self::r8(),
            Self::r9(),
            Self::r10(),
            Self::r11(),
            Self::r12(),
            Self::r13(),
            Self::r14(),
            Self::r15(),
        ]
    }

    fn word_size() -> i64 {
        WORD_SIZE
    }

    fn parameters(&self) -> &[Variable] {
        &self.parameters
    }

    fn allocate_local(&mut self, var: ir::Variable) -> Variable {
        self.allocate_variable(var)
    }

    fn proc_entry_exit1(&mut self, statement: ir::Statement) -> ir::Statement {
        let mut start_statements = vec![];
        let mut end_statements = vec![];

        let mut saved_register_locations = vec![];
        for register in Self::callee_saved_registers().into_iter() {
            let local = Temp::new();
            let memory = ir::Exp::Temp(local);
            saved_register_locations.push(memory.clone());
            start_statements.push(ir::Statement::Move {
                dst: memory,
                val: ir::Exp::Temp(register),
            });
        }

        let arg_registers = Self::arg_registers();
        let arg_registers_len = arg_registers.len();
        // save arguments from register
        for (formal, arg_register) in self.parameters.iter().zip(arg_registers) {
            let destination = ir_gen::access_var(formal, ir::Exp::Temp(Self::fp()));
            start_statements.push(ir::Statement::Move {
                dst: destination,
                val: ir::Exp::Temp(arg_register),
            });
        }
        // save arguments from frame
        for (index, formal) in self.parameters.iter().skip(arg_registers_len).enumerate() {
            let destination = ir_gen::access_var(formal, ir::Exp::Temp(Self::fp()));
            start_statements.push(ir::Statement::Move {
                dst: destination,
                val: ir::Exp::Mem(Box::new(ir::Exp::BinOp {
                    left: Box::new(ir::Exp::Temp(Self::fp())),
                    op: ir::BinOp::Plus,
                    right: Box::new(ir::Exp::Const(Self::word_size() * (index + 1) as i64)), // +1 to skip static_link
                })),
            });
        }

        for (register, location) in Self::callee_saved_registers()
            .into_iter()
            .zip(saved_register_locations)
        {
            end_statements.push(ir::Statement::Move {
                dst: ir::Exp::Temp(register),
                val: location,
            });
        }

        let start_statement = ir_gen::combine_statements(start_statements);
        let end_statement = ir_gen::combine_statements(end_statements);

        let statement = ir::Statement::Seq {
            s1: Box::new(start_statement),
            s2: Box::new(statement),
        };

        ir::Statement::Seq {
            s1: Box::new(statement),
            s2: Box::new(end_statement),
        }
    }

    fn proc_entry_exit2(&self, mut insts: Vec<asm::Instruction>) -> Vec<asm::Instruction> {
        // keep these registers until here
        let mut save = vec![Self::sp(), Self::return_value()];
        save.extend(Self::callee_saved_registers());
        let inst = asm::Instruction::Operation {
            assembly: "".to_string(),
            destination: vec![],
            source: save,
            jump: None,
        };

        insts.push(inst);
        insts
    }

    fn proc_entry_exit3(&self, body: asm::Trace) -> asm::Trace {
        let stack_size = {
            let size = -self.offset;
            if size % 16 != 0 {
                // Align the stack of 16 bytes according to x86_64 standard
                (size & !0xF) + 0x10
            } else {
                size
            }
        };

        // prologue
        let mut insts = Vec::new();
        insts.push(asm::Instruction::Label {
            assembly: format!("{}:", self.name),
            label: self.name,
        });
        insts.push(asm::Instruction::Operation {
            assembly: "push `s0".to_string(),
            destination: vec![Self::rsp()],
            source: vec![Self::rbp()],
            jump: None,
        });
        insts.push(asm::Instruction::Move {
            assembly: "mov `d0, `s0".to_string(),
            destination: vec![Self::rbp()],
            source: vec![Self::rsp()],
        });
        insts.push(asm::Instruction::Operation {
            assembly: format!("sub `d0, {}", stack_size),
            destination: vec![Self::rsp()],
            source: vec![],
            jump: None,
        });
        let mut new_trace = Trace::new(body.done_label);
        new_trace.add_block(asm::Block::new(insts));
        new_trace.extend(body);

        // epilogue
        let end_label = Label::new();
        let insts = vec![
            asm::Instruction::Label {
                assembly: format!("{}:", end_label),
                label: end_label,
            },
            asm::Instruction::Operation {
                assembly: "leave".to_string(),
                destination: vec![],
                source: vec![],
                jump: None,
            },
            asm::Instruction::Operation {
                assembly: "ret".to_string(),
                destination: vec![],
                source: vec![],
                jump: None,
            },
        ];
        new_trace.add_block(asm::Block::new(insts));

        new_trace
    }
}
