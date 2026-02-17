use std::collections::{HashMap, HashSet};

use crate::{
    asm,
    flow::{self, FlowGraph},
    frame::Frame,
    liveness::{self, InterferenceGraph, ProgramPoint},
    stack::Stack,
    temp::Temp,
};

struct Alloc<'a> {
    initial: Vec<Temp>,
    simplify_work_list: Vec<Temp>,
    freeze_work_list: Vec<Temp>,
    spill_work_list: Vec<Temp>,
    spilled_nodes: HashSet<Temp>,
    colored_nodes: HashSet<Temp>,
    select_stack: Stack<Temp>,
    coalesced_moves: Vec<ProgramPoint<'a>>, // moves that have been coalesced.
    constrained_moves: Vec<ProgramPoint<'a>>, // moves whose source and target interfere.
    frozen_moves: Vec<ProgramPoint<'a>>, // moves that will no longer be considered for coalescing.
    worklist_moves: Vec<ProgramPoint<'a>>, // moves enabled for possible coalescing.
    active_moves: Vec<ProgramPoint<'a>>, // moves not yet ready for coalescing.
    move_list: HashMap<Temp, Vec<ProgramPoint<'a>>>,

    flow: FlowGraph<'a>,
    inter_g: InterferenceGraph,
    k: usize,
}

impl<'a> Alloc<'a> {
    pub fn new<F: Frame>(
        flow: FlowGraph<'a>,
        inter_g: InterferenceGraph,
        work_move_list: Vec<ProgramPoint<'a>>,
        move_list: HashMap<Temp, Vec<ProgramPoint<'a>>>,
    ) -> Self {
        let k = F::colors().len();
        let initial = inter_g
            .g
            .nodes()
            .iter()
            .map(|v| *v.value())
            .collect::<Vec<_>>();
        let mut spill_work_list = vec![];
        let mut simplify_work_list = vec![];
        let mut freeze_work_list = vec![];
        for n in initial {
            if inter_g.degree(&n) >= k {
                spill_work_list.push(n);
            } else if move_list.contains_key(&n)
            // TODO speed up this liner find?
            {
                freeze_work_list.push(n);
            } else {
                simplify_work_list.push(n);
            }
        }

        Alloc {
            initial: vec![],
            simplify_work_list,
            freeze_work_list,
            spill_work_list,
            spilled_nodes: HashSet::new(),
            colored_nodes: HashSet::new(),
            select_stack: Stack::new(),
            coalesced_moves: Vec::<ProgramPoint<'a>>::new(),
            constrained_moves: Vec::<ProgramPoint<'a>>::new(),
            frozen_moves: Vec::<ProgramPoint<'a>>::new(),
            worklist_moves: work_move_list,
            active_moves: Vec::<ProgramPoint<'a>>::new(),
            move_list,
            flow,
            inter_g,
            k,
        }
    }
}

impl<'a> Alloc<'a> {
    pub fn simplify(&mut self) {
        let n = self.simplify_work_list.pop();
        if let Some(n) = n {
            self.select_stack.push(n);
            let node = self.inter_g.pop_node(&n);
            for outcome in node.outcome() {
                if self.inter_g.degree_entry(outcome) == self.k - 1 {}
            }
        }
    }

    fn node_moves(&self, t: &Temp) -> Vec<ProgramPoint<'_>> {
        let mut res = self.active_moves.clone();
        res.extend(self.worklist_moves.clone());
        // TODO intersect with work_list[n]
        let wn = self.move_list.get(t).cloned().unwrap_or(vec![]);
        res.into_iter()
            .filter(|v| wn.contains(v))
            .collect::<Vec<_>>()
    }
}

pub fn alloc<F: Frame>(trace: asm::Trace) -> asm::Trace {
    let flow = flow::flow_analyze(&trace);
    let (inter_g, work_move_list, work_list) = liveness::liveness_analyze(&flow, trace.done_label);
    let alloc = Alloc::new::<F>(flow, inter_g, work_move_list, work_list);
    // TODO return the rewritten trace
    trace
}
