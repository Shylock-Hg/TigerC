use std::hash::Hash;

use crate::ident_pool::Symbol;
use crate::temp::{Label, Temp};

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

// IR
#[derive(Debug, PartialEq, Eq)]
pub enum Exp {
    // constant integer
    Const(i64),
    // code location
    Name(Label),
    // temporary variable
    Temp(Temp),
    BinOp {
        op: BinOp,
        left: Box<Exp>,
        right: Box<Exp>,
    },
    // exp evaluate the address number
    // get a word-size value from address number
    Mem(Box<Exp>),
    Call {
        func: Box<Exp>,
        args: Vec<Exp>,
    },
    ExpSeq {
        stmt: Box<Statement>,
        exp: Box<Exp>,
    },
}

#[derive(Debug, PartialEq, Eq)]
pub enum BinOp {
    Plus,
    Minus,
    Mul,
    Div,
    And,
    Or,
    ShiftLeft,
    ShiftRight,
    ArithmeticShiftRight,
    Xor,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Statement {
    // evaluate exp and move to dst
    MoveTemp {
        dst: Temp,
        val: Exp,
    },
    // evaluate exp and move to dst address
    // store a word-size value to dst
    MoveMem {
        dst: Exp,
        val: Exp,
    },
    // evaluate and discard result
    Exp(Exp),
    // jump to label which is evaluated from exp
    Jump {
        exp: Exp,
        labels: Vec<Label>,
    },
    // Jump according to condition
    CJump {
        op: CompareOp,
        left: Exp,
        right: Exp,
        then: Label,
        else_: Label,
    },
    // statements in sequence
    Seq {
        s1: Box<Statement>,
        s2: Box<Statement>,
    },
    Label(Label),
}

#[derive(Debug, PartialEq, Eq)]
pub enum CompareOp {
    Eq,
    Ne,
    SignedGt,
    SignedGe,
    SignedLt,
    SignedLe,
    UnsignedGt,
    UnsignedGe,
    UnsignedLt,
    UnsignedLe,
}
