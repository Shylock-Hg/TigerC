// Some utilities to generate ir

use crate::frame::Access;
use crate::frame::Variable;
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

pub fn access_var(var: &Variable, fp: ir::Exp) -> ir::Exp {
    match var.access {
        Access::Register(tp) => ir::Exp::Temp(tp),
        Access::Frame(offset) => ir::Exp::Mem(Box::new(ir::Exp::BinOp {
            left: Box::new(fp),
            op: ir::BinOp::Plus,
            right: Box::new(ir::Exp::Const(offset)),
        })),
    }
}
