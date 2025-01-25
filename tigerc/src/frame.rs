use std::ops::Index;

use indexmap::IndexMap;

use crate::ident_pool::{self, Symbol};

enum Access {
    // offset to frame pointer
    Frame(usize),
    // register notion
    Register(Register),
}

struct Register(Symbol);

struct Variable {
    // true for variable that must be addressed
    escape: bool,
    access: Access,
}

// frame of a function
struct Frame {
    // function name
    name: Symbol,
    // parameters
    parameters: IndexMap<Symbol, Variable>,
    // local variables
    locals: IndexMap<Symbol, Variable>,
    // counting same name variable
    id: usize,
}

impl Frame {
    pub fn new(name: Symbol, parameters: IndexMap<Symbol, Variable>) -> Self {
        Frame {
            name,
            parameters,
            locals: IndexMap::new(),
            id: 0,
        }
    }

    pub fn allocate_local(&mut self, symbol: Symbol, var: Variable) {
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
