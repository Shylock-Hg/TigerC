use indexmap::IndexMap;

use crate::asm::{self, Instruction};
use crate::ident_pool::Symbol;
use crate::ir;
use crate::temp::{Label, Temp};

#[derive(Clone)]
pub enum Access {
    // offset to frame pointer
    Frame(i64),
    // register notion
    // But maybe spill to memory in later stage
    Register(Temp),
}

#[derive(Clone)]
pub struct Register(pub Symbol);

#[derive(Clone)]
pub struct Variable {
    pub access: Access,
}

impl Variable {
    pub fn mem(&self) -> i64 {
        match &self.access {
            Access::Frame(i) => *i,
            _ => unreachable!(),
        }
    }
}

pub trait Frame {
    fn new(name: Label, parameters: IndexMap<ir::LowerIdent, ir::Variable>) -> Self;

    fn fp() -> Temp;
    fn sp() -> Temp;
    fn return_value() -> Temp;
    fn word_size() -> i64;
    fn precoloered() -> Vec<Temp>;
    fn colors() -> Vec<Temp>;

    fn name(&self) -> &Label;
    fn parameters(&self) -> &[Variable];
    fn allocate_local(&mut self, var: ir::Variable) -> Variable;

    fn proc_entry_exit1(&mut self, statement: ir::Statement) -> ir::Statement;
    fn proc_entry_exit2(&self, insts: Vec<asm::Instruction>) -> Vec<asm::Instruction>;
}
