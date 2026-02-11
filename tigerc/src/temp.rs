use std::fmt::Display;

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

impl ToString for Temp {
    fn to_string(&self) -> String {
        self.0.to_string()
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

impl Display for Label {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}
