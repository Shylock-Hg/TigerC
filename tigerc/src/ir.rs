use std::fmt::Display;
use std::hash::Hash;

use crate::ident_pool::{self, kw, Symbol};
use crate::temp::{Label, Temp};

// escape?
pub struct Variable(pub bool);

// Identify different symbol with same name
// We will flatten symbol scope when translate it to IR which close to machine code
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum LowerIdent {
    Number(usize),
    Name(Symbol),
}

impl LowerIdent {
    pub fn new_anonymous() -> LowerIdent {
        // It's safe in single thread
        static mut COUNTER: usize = 0;
        LowerIdent::Number(unsafe {
            COUNTER += 1;
            COUNTER
        })
    }

    pub fn new_named(name: Symbol) -> LowerIdent {
        LowerIdent::Name(name)
    }
}

impl Display for LowerIdent {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LowerIdent::Number(number) => write!(f, "{}_{}", "__anon", number),
            LowerIdent::Name(symbol) => write!(f, "{}", symbol),
        }
    }
}

// IR
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Exp {
    // constant integer
    Const(i64),
    // code location (e.g. string literal)
    Name(Label),
    // temporary variable value
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

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum BinOp {
    Plus,
    Minus,
    Multiply,
    Divide,
    And,
    Or,
    Xor,
    LShift,
    RShift,
    ARShift,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Statement {
    // evaluate val and move to dst
    // dst could be `Temp` for `Mem`
    Move {
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

#[derive(Debug, PartialEq, Eq, Clone)]
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
