use std::fmt::Display;
use std::hash::Hash;
use std::sync::atomic::AtomicUsize;

use crate::ident_pool::Symbol;
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
        // cargo test will run test concurrency so we use atomic here
        // TODO: Put this counter to Context of once compilation to keep stability of inner id number?
        // and about id number of symbol?
        static COUNTER: AtomicUsize = AtomicUsize::new(0);
        LowerIdent::Number(COUNTER.fetch_add(1, std::sync::atomic::Ordering::Relaxed))
    }

    pub fn new_named(name: Symbol) -> LowerIdent {
        LowerIdent::Name(name)
    }
}

impl Display for LowerIdent {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LowerIdent::Number(number) => write!(f, "__anon_{}", number),
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

pub fn concat(s1: Statement, s2: Statement) -> Statement {
    Statement::Seq {
        s1: Box::new(s1),
        s2: Box::new(s2),
    }
}

pub fn concat_stmt_exp(stmt: Statement, exp: Exp) -> Exp {
    Exp::ExpSeq {
        stmt: Box::new(stmt),
        exp: Box::new(exp),
    }
}
