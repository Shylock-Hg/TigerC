use std::cell::RefCell;
use std::rc::Rc;
use std::vec;
use std::{collections::HashMap, mem::swap};

use crate::{
    frame::Frame,
    ir, ir_gen,
    temp::{Label, Temp},
};

pub enum Fragment<F: Frame> {
    Function {
        label: Label,
        frame: Rc<RefCell<F>>,
        body: (Trace, Label),
    },
    StringLiteral(Label, String),
}

pub fn canonicalize(stmt: ir::Statement) -> (Trace, Label) {
    let (blocks, done_label) = basic_block(linearize(stmt));
    let trace = trace_schedule(blocks, done_label);
    (trace, done_label)
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
            (concat(stmt, mv), ir::Exp::Mem(Box::new(tp.clone())))
        }
        ir::Exp::Call { func, args } => {
            let (stmt, args) = reorder2(&args);
            let call = ir::Exp::Call { func, args }; // FIXME: lift func exp?
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
    let len = exps.len();
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
    if !commute(&stmt2, &exp1) {
        let tp = ir::Exp::Temp(Temp::new());
        let mv = ir::Statement::Move {
            dst: tp.clone(),
            val: exp1,
        };
        let stmt = concat(stmt1, concat(mv, stmt2));
        let mut exps = vec![tp];
        exps.extend(exp2);
        debug_assert!(len == exps.len());
        (stmt, exps)
    } else {
        let stmt = concat(stmt1, stmt2);
        let mut exps = vec![exp1];
        exps.extend(exp2);
        debug_assert!(len == exps.len());
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
// The last statement is a JUMP or CJUMP.? the done_label block is not ended with jump/cjump
// There are no other LABELs, JUMPs, or CJUMPs.
#[derive(Clone, Debug)]
pub struct Block(Vec<ir::Statement>);

impl Block {
    pub fn new(label: Label) -> Self {
        Self(vec![ir::Statement::Label(label)])
    }

    pub fn push(&mut self, stmt: ir::Statement) {
        self.0.push(stmt);
    }

    //pub fn finish(mut self, stmt: ir::Statement) -> Self {
    //debug_assert!(matches!(
    //stmt,
    //ir::Statement::Jump { .. } | ir::Statement::CJump { .. }
    //));
    //self.0.push(stmt);
    //self
    //}

    // filter unused statements
    // a naive implementation, in fact, we can remove all pure expression which result is unused
    pub fn filter_unused(&mut self) {
        self.0.retain(|s| match s {
            ir::Statement::Exp(ir::Exp::Const(_)) => false,
            _ => true,
        });
    }

    pub fn result(self) -> Vec<ir::Statement> {
        self.0
    }

    pub fn start_label(&self) -> Label {
        match self.0.first().unwrap() {
            ir::Statement::Label(l) => *l,
            _ => unreachable!(),
        }
    }

    pub fn last(&self) -> &ir::Statement {
        self.0.last().unwrap()
    }

    pub fn size(&self) -> usize {
        self.0.len()
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
                }
                block = Some(Block::new(l));
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

#[derive(Debug)]
pub struct Trace {
    pub blocks: Vec<Block>,
}

fn trace_schedule(blocks: Vec<Block>, done_label: Label) -> Trace {
    let mut label_index = blocks
        .iter()
        .enumerate()
        .map(|(i, b)| match b.0.first().unwrap() {
            ir::Statement::Label(l) => (*l, i),
            _ => unreachable!(),
        })
        .collect::<HashMap<_, _>>();
    const END_INDEX: usize = usize::MAX;
    label_index.insert(done_label, END_INDEX);

    fn mark(marks: &Vec<bool>, index: usize) -> bool {
        if index == END_INDEX {
            true
        } else {
            marks[index]
        }
    }

    let mut marks = vec![false; blocks.len()];
    let mut traces = vec![];
    for (mut i, block) in blocks.iter().enumerate() {
        let mut current_trace = vec![];
        while !mark(&marks, i) {
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
                    if !mark(&marks, *next) {
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

    // Put else branch be successor of cjump
    let mut new_blocks = Vec::with_capacity(blocks.len());
    let traces = traces
        .into_iter()
        .flat_map(|t| t.into_iter())
        .collect::<Vec<_>>();
    for i in 0..traces.len() {
        let current = &traces[i];
        debug_assert!(mark(&marks, *current));
        let next = traces.get(i + 1);
        // TODO avoid clone
        let mut current_block = blocks[*current].clone();
        match current_block.0.last_mut().unwrap() {
            ir::Statement::Jump { .. } => {
                // do nothing
                // TODO maybe we can merge block if next block be only jumped from this
            }
            ir::Statement::CJump {
                left,
                right,
                then,
                else_,
                ..
            } => {
                if let Some(next) = next {
                    let next_block = &blocks[*next];
                    if let ir::Statement::Label(label) = next_block.0.first().unwrap() {
                        if label == then {
                            // reverse condition, keep else branch be successor
                            swap(left, right);
                            swap(then, else_);
                        } else if label == else_ {
                            // do nothing
                        } else {
                            let new_else = Label::new();
                            *else_ = new_else;
                            let mut new_block = Block::new(new_else);
                            new_block.push(ir::Statement::Jump {
                                exp: ir::Exp::Name(*label),
                                labels: vec![*label],
                            });
                            new_blocks.push(new_block);
                        }
                    } else {
                        unreachable!();
                    }
                }
            }
            _ => unreachable!(),
        }
        new_blocks.push(current_block);
    }
    new_blocks.push(Block::new(done_label));
    Trace { blocks: new_blocks }
}

#[allow(unused)]
fn debug_trace(blocks: &Vec<Block>, traces: &Vec<usize>) {
    println!("trace:");
    for i in traces {
        println!("{:#?}", blocks[*i].0);
    }
}
