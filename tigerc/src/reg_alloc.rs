use std::collections::HashSet;

use crate::{
    asm,
    flow::{self, FlowGraph},
    frame::Frame,
    liveness::{self, InterferenceGraph, Move, ProgramPoint},
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
}

impl<'a> Alloc<'a> {
    pub fn new<F: Frame>(_flow: &FlowGraph<'a>, inter_g: &InterferenceGraph) -> Self {
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
            } else if inter_g
                .m_list
                .iter()
                .find(|Move { dst, src }| *dst == n || *src == n)
                .is_some()
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
            worklist_moves: Vec::<ProgramPoint<'a>>::new(),
            active_moves: Vec::<ProgramPoint<'a>>::new(),
        }
    }
}

pub fn alloc<F: Frame>(trace: asm::Trace) -> asm::Trace {
    let flow = flow::flow_analyze(&trace);
    let inter_g = liveness::liveness_analyze(&flow, trace.done_label);
    let alloc = Alloc::new::<F>(&flow, &inter_g);
    trace
}
