use std::vec;

use crate::{ir, ir_gen, temp::Temp};

pub fn canonicalize(stmt: ir::Statement) -> Vec<ir::Statement> {
    linearize(stmt)
}

// The result will fit two properties:
// 1. No Statement::Seq/Exp::ExpSeq
// 2. The parent of each Exp::Call is Statement::Exp or Statement::Move(temp, ...)
fn linearize(stmt: ir::Statement) -> Vec<ir::Statement> {
    let stmt = lift_exp_seq(stmt);

    fn collect(stmt: ir::Statement, acc: &mut Vec<ir::Statement>) {
        match stmt {
            ir::Statement::Seq { s1, s2 } => {
                collect(*s1, acc);
                collect(*s2, acc);
            }
            _ => acc.push(stmt),
        }
    }
    let mut acc = vec![];
    collect(stmt, &mut acc);
    acc
}

fn lift_exp_seq(stmt: ir::Statement) -> ir::Statement {
    match stmt {
        ir::Statement::Move { dst, val } => {
            let (stmt, val) = lift_exq_seq_exp(val);
            let (stmt2, dst) = lift_exq_seq_exp(dst);
            let stmt = concat(stmt, stmt2);
            let mv = ir::Statement::Move { dst, val };
            concat(stmt, mv)
        }
        ir::Statement::Exp(e) => {
            let (stmt, exp) = lift_exq_seq_exp(e);
            concat(stmt, ir::Statement::Exp(exp))
        }
        ir::Statement::Jump { exp, labels } => {
            let (stmt, exp) = lift_exq_seq_exp(exp);
            let j = ir::Statement::Jump { exp, labels };
            concat(stmt, j)
        }
        ir::Statement::CJump {
            op,
            left,
            right,
            then,
            else_,
        } => {
            let exps = vec![left, right];
            let (stmt, exps) = reorder2(&exps);
            let cj = ir::Statement::CJump {
                op,
                left: exps[0].clone(),
                right: exps[1].clone(),
                then,
                else_,
            };
            concat(stmt, cj)
        }
        ir::Statement::Seq { s1, s2 } => {
            let s1 = lift_exp_seq(*s1);
            let s2 = lift_exp_seq(*s2);
            concat(s1, s2)
        }
        ir::Statement::Label(l) => ir::Statement::Label(l),
    }
}

fn lift_exq_seq_exp(exp: ir::Exp) -> (ir::Statement, ir::Exp) {
    match exp {
        ir::Exp::Const(i) => (ir_gen::no_op(), ir::Exp::Const(i)),
        ir::Exp::Name(n) => (ir_gen::no_op(), ir::Exp::Name(n)),
        ir::Exp::Temp(t) => (ir_gen::no_op(), ir::Exp::Temp(t)),
        ir::Exp::BinOp { op, left, right } => {
            let exps = vec![*left, *right];
            let (stmt, exps) = reorder2(&exps);
            let binop = ir::Exp::BinOp {
                op,
                left: Box::new(exps[0].clone()),
                right: Box::new(exps[1].clone()),
            };
            (stmt, binop)
        }
        ir::Exp::Mem(m) => {
            let (stmt, m) = lift_exq_seq_exp(*m);
            let tp = ir::Exp::Temp(Temp::new());
            let mv = ir::Statement::Move {
                dst: tp.clone(),
                val: m,
            };
            (concat(stmt, mv), tp.clone())
        }
        ir::Exp::Call { func, args } => {
            let (stmt, args) = reorder2(&args);
            let call = ir::Exp::Call { func, args };
            (stmt, call)
        }
        ir::Exp::ExpSeq { stmt, exp } => {
            let stmt = lift_exp_seq(*stmt);
            let (stmt2, exp) = lift_exq_seq_exp(*exp);
            (concat(stmt, stmt2), exp)
        }
    }
}

fn concat(s1: ir::Statement, s2: ir::Statement) -> ir::Statement {
    ir::Statement::Seq {
        s1: Box::new(s1),
        s2: Box::new(s2),
    }
}

fn reorder2(exps: &[ir::Exp]) -> (ir::Statement, Vec<ir::Exp>) {
    if exps.is_empty() {
        // empty parameter list
        return (ir_gen::no_op(), vec![]);
    }

    if exps.len() == 1 {
        let (stmt, exp) = lift_exq_seq_exp(exps[0].clone());
        return (stmt, vec![exp]);
    }

    let (stmt1, exp1) = lift_exq_seq_exp(exps[0].clone());
    let (stmt2, exp2) = reorder2(&exps[1..]);
    if commute(&stmt2, &exp1) {
        let tp = ir::Exp::Temp(Temp::new());
        let mv = ir::Statement::Move {
            dst: tp.clone(),
            val: exp1,
        };
        let stmt = concat(mv, stmt2);
        let mut exps = vec![tp];
        exps.extend(exp2);
        debug_assert!(exps.len() == exps.len());
        (stmt, exps)
    } else {
        let stmt = concat(stmt1, stmt2);
        let mut exps = vec![exp1];
        exps.extend(exp2);
        debug_assert!(exps.len() == exps.len());
        (stmt, exps)
    }
}

// a naive implementation
fn commute(stmt: &ir::Statement, exp: &ir::Exp) -> bool {
    match (&stmt, &exp) {
        (ir::Statement::Exp(ir::Exp::Const(_)), _)
        | (_, ir::Exp::Const(_))
        | (_, ir::Exp::Name(_)) => true,
        _ => false,
    }
}
