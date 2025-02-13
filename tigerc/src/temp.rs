use crate::ident_pool::Symbol;
use crate::ir::LowerIdent;

// for variable
#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct Temp(pub LowerIdent);

impl Temp {
    pub fn new() -> Self {
        Temp(LowerIdent::new_anonymous())
    }

    pub fn new_named(name: Symbol) -> Self {
        Temp(LowerIdent::new_named(name))
    }
}

// for function or code block location
#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct Label(pub LowerIdent);

impl Label {
    pub fn new() -> Self {
        Label(LowerIdent::new_anonymous())
    }

    pub fn new_named(name: Symbol) -> Self {
        Label(LowerIdent::new_named(name))
    }
}
