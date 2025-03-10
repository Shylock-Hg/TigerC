use std::collections::HashMap;
use std::vec;

use crate::{
    ir, ir_gen,
    temp::{Label, Temp},
};

pub fn canonicalize(stmt: ir::Statement) -> Vec<Block> {
    basic_block(linearize(stmt)).0
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

// The first statement is a LABEL.
// The last statement is a JUMP or CJUMP.
// There are no other LABELs, JUMPs, or CJUMPs.
#[derive(Debug)]
pub struct Block(Vec<ir::Statement>);

impl Block {
    pub fn new(label: Label) -> Self {
        Self(vec![ir::Statement::Label(label)])
    }

    pub fn push(&mut self, stmt: ir::Statement) {
        self.0.push(stmt);
    }

    pub fn finish(mut self, stmt: ir::Statement) -> Self {
        debug_assert!(matches!(
            stmt,
            ir::Statement::Jump { .. } | ir::Statement::CJump { .. }
        ));
        self.0.push(stmt);
        self
    }

    // filter unused statements
    // a naive implementation, in fact, we can remove all pure expression which result is unused
    pub fn filter_unused(&mut self) {
        self.0.retain(|s| match s {
            ir::Statement::Exp(ir::Exp::Const(_)) => false,
            _ => true,
        });
    }
}

fn basic_block(stmts: Vec<ir::Statement>) -> (Vec<Block>, Label) {
    let done_label = Label::new();
    let mut blocks = vec![];
    let mut block: Option<Block> = None;
    for stmt in stmts {
        match stmt {
            ir::Statement::Label(l) => {
                if let Some(ref mut b) = block {
                    let jump = ir::Statement::Jump {
                        exp: ir::Exp::Name(l),
                        labels: vec![l],
                    };
                    // TODO call finish here
                    b.push(jump);
                    blocks.push(block.take().unwrap());
                } else {
                    block = Some(Block::new(l));
                }
            }
            ir::Statement::Seq { .. } => unreachable!(),
            ir::Statement::CJump { .. } | ir::Statement::Jump { .. } => {
                if let Some(ref mut block) = block {
                    // TODO call finish here
                    block.push(stmt);
                } else {
                    unreachable!();
                }
                blocks.push(block.take().unwrap());
            }
            ir::Statement::Exp(..) | ir::Statement::Move { .. } => {
                if let Some(ref mut block) = block {
                    block.push(stmt);
                } else {
                    block = Some(Block::new(Label::new()));
                }
            }
        }
    }
    if block.is_some() {
        blocks.push(block.take().unwrap());
    }
    // jump to end of function, avoid later reorder violate this
    blocks.last_mut().unwrap().push(ir::Statement::Jump {
        exp: ir::Exp::Name(done_label),
        labels: vec![done_label],
    });
    (clean(blocks), done_label)
}

// clean unused statements
fn clean(blocks: Vec<Block>) -> Vec<Block> {
    blocks
        .into_iter()
        .map(|mut block| {
            block.filter_unused();
            block
        })
        .collect::<Vec<_>>()
}

struct Trace {
    pub blocks: Vec<Block>,
    // index of block
    pub traces: Vec<Vec<usize>>,
}

fn trace_schedule(blocks: Vec<Block>) -> Trace {
    let label_index = blocks
        .iter()
        .enumerate()
        .map(|(i, b)| match b.0.first().unwrap() {
            ir::Statement::Label(l) => (l.clone(), i),
            _ => unreachable!(),
        })
        .collect::<HashMap<_, _>>();

    let mut marks = vec![false; blocks.len()];
    let mut traces = vec![];
    for (mut i, block) in blocks.iter().enumerate() {
        let mut current_trace = vec![];
        while !marks[i] {
            current_trace.push(i);
            marks[i] = true;

            let next = match block.0.last().unwrap() {
                ir::Statement::Jump { exp, .. } => {
                    let label = match exp {
                        ir::Exp::Name(l) => l,
                        _ => unreachable!(),
                    };
                    let next = label_index.get(label).unwrap();
                    *next
                }
                ir::Statement::CJump { then, else_, .. } => {
                    let next = label_index.get(else_).unwrap();
                    if !marks[*next] {
                        *next
                    } else {
                        let next = label_index.get(then).unwrap();
                        *next
                    }
                }
                _ => unreachable!(),
            };
            i = next;
        }
        traces.push(current_trace);
    }
    Trace { blocks, traces }
}
