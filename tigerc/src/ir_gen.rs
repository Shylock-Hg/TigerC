// Some utilities to generate ir

use crate::ir;

pub fn no_op() -> ir::Statement {
    ir::Statement::Exp(ir::Exp::Const(0))
}

pub fn combine_statements(stmts: Vec<ir::Statement>) -> ir::Statement {
    let mut s = no_op();
    for e in stmts {
        s = ir::Statement::Seq {
            s1: Box::new(s),
            s2: Box::new(e.clone()),
        };
    }
    s
}
