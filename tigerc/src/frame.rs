use indexmap::IndexMap;

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

pub trait Frame {
    fn new(name: Label, parameters: IndexMap<ir::LowerIdent, ir::Variable>) -> Self;

    fn fp() -> Temp;
    fn return_value() -> Temp;
    fn word_size() -> i64;

    fn name(&self) -> &Label;
    fn parameters(&self) -> &[Variable];
    fn allocate_local(&mut self, var: ir::Variable) -> Variable;
}
