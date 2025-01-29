use std::hash::Hash;

use crate::ident_pool::Symbol;

// Identify different symbol with same name
// We will flatten symbol scope when translate it to IR which close to machine code
#[derive(Eq, Debug)]
pub struct LowerIdent {
    // this is used just for debug
    pub symbol: Symbol,
    pub number: usize,
}

impl LowerIdent {
    pub fn new(symbol: Symbol) -> LowerIdent {
        // It's safe in single thread
        static mut COUNTER: usize = 0;
        LowerIdent {
            symbol,
            number: unsafe {
                COUNTER += 1;
                COUNTER
            },
        }
    }
}

impl PartialEq for LowerIdent {
    fn eq(&self, other: &Self) -> bool {
        self.number == other.number
    }
}

impl Hash for LowerIdent {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.number.hash(state);
    }
}
