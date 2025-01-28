use indexmap::IndexMap;

use crate::ident_pool::{self, Symbol};

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
    fn new(name: Symbol, parameters: IndexMap<Symbol, Variable>) -> Self;
    fn name(&self) -> Symbol;
    fn allocate_local(&mut self, symbol: Symbol, var: Variable);
}

// frame of a function
struct FrameAmd64 {
    // function name
    name: Symbol,
    // parameters
    parameters: IndexMap<Symbol, Variable>,
    // local variables
    locals: IndexMap<Symbol, Variable>,
    // counting same name variable
    id: usize,
}

impl Frame for FrameAmd64 {
    fn new(name: Symbol, parameters: IndexMap<Symbol, Variable>) -> Self {
        FrameAmd64 {
            name,
            parameters,
            locals: IndexMap::new(),
            id: 0,
        }
    }

    fn name(&self) -> Symbol {
        self.name
    }

    fn allocate_local(&mut self, symbol: Symbol, var: Variable) {
        if self.locals.contains_key(&symbol) {
            self.locals.insert(
                ident_pool::create_symbol(&format!("{}:{}", symbol, self.id)),
                var,
            );
        } else {
            self.locals.insert(symbol, var);
        }
    }
}
