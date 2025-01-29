use indexmap::IndexMap;

use crate::ident_pool::Symbol;
use crate::ir;

pub enum Access {
    // offset to frame pointer
    Frame(usize),
    // register notion
    Register(Register),
}

pub struct Register(pub Symbol);

pub struct Variable {
    // true for variable that must be addressed
    pub escape: bool,
    pub access: Access,
}

pub trait Frame {
    fn new(name: Symbol, parameters: IndexMap<ir::LowerIdent, Variable>) -> Self;
    fn name(&self) -> &ir::LowerIdent;
    fn allocate_local(&mut self, symbol: ir::LowerIdent, var: Variable);
}

// frame of a function
struct FrameAmd64 {
    // function name
    name: ir::LowerIdent,
    // parameters
    parameters: IndexMap<ir::LowerIdent, Variable>,
    // local variables
    locals: IndexMap<ir::LowerIdent, Variable>,
}

impl Frame for FrameAmd64 {
    fn new(name: Symbol, parameters: IndexMap<ir::LowerIdent, Variable>) -> Self {
        FrameAmd64 {
            name: ir::LowerIdent::new(name),
            parameters,
            locals: IndexMap::new(),
        }
    }

    fn name(&self) -> &ir::LowerIdent {
        &self.name
    }

    fn allocate_local(&mut self, symbol: ir::LowerIdent, var: Variable) {
        if self.locals.contains_key(&symbol) {
            self.locals.insert(symbol, var);
        } else {
            self.locals.insert(symbol, var);
        }
    }
}
