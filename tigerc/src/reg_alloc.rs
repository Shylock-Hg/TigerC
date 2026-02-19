use std::collections::{HashMap, HashSet};

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
    coalesced_nodes: HashSet<Temp>,
    colored_nodes: HashSet<Temp>,
    select_stack: Stack<Temp>,
    coalesced_moves: Vec<Move>,   // moves that have been coalesced.
    constrained_moves: Vec<Move>, // moves whose source and target interfere.
    frozen_moves: Vec<Move>,      // moves that will no longer be considered for coalescing.
    worklist_moves: Vec<Move>,    // moves enabled for possible coalescing.
    active_moves: Vec<Move>,      // moves not yet ready for coalescing.
    move_list: HashMap<Temp, Vec<Move>>,

    alias: HashMap<Temp, Temp>,

    flow: FlowGraph<'a>,
    inter_g: InterferenceGraph,
    k: usize,
    precolored: Vec<Temp>,
}

impl<'a> Alloc<'a> {
    pub fn new<F: Frame>(
        flow: FlowGraph<'a>,
        inter_g: InterferenceGraph,
        work_list_moves: Vec<Move>,
        move_list: HashMap<Temp, Vec<Move>>,
    ) -> Self {
        let k = F::colors().len();
        let initial = inter_g.iter().map(|(k, _)| k).collect::<Vec<_>>();
        let mut spill_work_list = vec![];
        let mut simplify_work_list = vec![];
        let mut freeze_work_list = vec![];
        for n in initial {
            if inter_g.degree(&n) >= k {
                spill_work_list.push(*n);
            } else if move_list.contains_key(&n) {
                freeze_work_list.push(*n);
            } else {
                simplify_work_list.push(*n);
            }
        }

        Alloc {
            initial: vec![],
            simplify_work_list,
            freeze_work_list,
            spill_work_list,
            spilled_nodes: HashSet::new(),
            coalesced_nodes: HashSet::new(),
            colored_nodes: HashSet::new(),
            select_stack: Stack::new(),
            coalesced_moves: Vec::<Move>::new(),
            constrained_moves: Vec::<Move>::new(),
            frozen_moves: Vec::<Move>::new(),
            worklist_moves: work_list_moves,
            active_moves: Vec::<Move>::new(),
            alias: HashMap::new(),
            move_list,
            flow,
            inter_g,
            k,
            precolored: F::precoloered(),
        }
    }
}

impl<'a> Alloc<'a> {
    pub fn simplify(&mut self) {
        let n = self.simplify_work_list.pop();
        if let Some(n) = n {
            self.select_stack.push(n);
            let adjs = self.adjacent(&n);
            for adj in adjs {
                if self.inter_g.degree(&adj) == self.k - 1 {
                    let mut nodes = self.adjacent(&adj);
                    nodes.push(adj);
                    self.enable_moves(nodes);
                    self.spill_work_list.retain(|v| *v != adj);
                    if self.move_related(&adj) {
                        self.freeze_work_list.push(adj);
                    } else {
                        self.simplify_work_list.push(adj);
                    }
                }
            }
        }
    }

    pub fn coalesce(&mut self) {
        let m = self.worklist_moves.pop();
        if let Some(m) = m {
            let Move { dst, src } = m;
            let dst = self.get_alias(&dst);
            let src = self.get_alias(&src);
            let (u, v) = if self.precolored.contains(&src) {
                (src, dst)
            } else {
                (dst, src)
            };
            if u == v {
                self.coalesced_moves.push(m);
                self.add_work_list(u);
            } else if self.precolored.contains(&v) && self.inter_g.is_interfered(&u, &v) {
                self.constrained_moves.push(m);
                self.add_work_list(u);
                self.add_work_list(v);
            } else if (self.precolored.contains(&u) && self.george(&u, &v))
                || (!self.precolored.contains(&u) && self.briggs(&u, &v))
            {
                self.coalesced_moves.push(m);
                self.add_work_list(u);
                self.combine(&u, &v);
                self.add_work_list(u);
            } else {
                self.active_moves.push(m);
            }
        }
    }

    fn freeze(&mut self) {
        let u = self.freeze_work_list.pop();
        if let Some(u) = u {
            self.simplify_work_list.push(u.clone());
            self.freeze_moves(&u);
        }
    }

    fn freeze_moves(&mut self, u: &Temp) {
        let moves = self.node_moves(u);
        for m in moves {
            let Move { dst, src } = m;
            let v = if self.get_alias(&src) == self.get_alias(u) {
                self.get_alias(&dst)
            } else {
                self.get_alias(&src)
            };
            self.frozen_moves.push(
                self.active_moves
                    .remove(self.active_moves.iter().position(|val| val == &m).unwrap()),
            );
            if self.node_moves(&v).is_empty() && !self.is_significant(&v) {
                self.simplify_work_list.push(
                    self.freeze_work_list.remove(
                        self.freeze_work_list
                            .iter()
                            .position(|val| val == &v)
                            .unwrap(),
                    ),
                )
            }
        }
    }

    fn combine(&mut self, u: &Temp, v: &Temp) {
        let res = self.freeze_work_list.iter().position(|t| t == v);
        if let Some(i) = res {
            self.freeze_work_list.remove(i);
        } else {
            self.spill_work_list.retain(|t| t != v);
        }
        self.coalesced_nodes.insert(*v);
        self.alias.insert(*v, *u);
        let res = self.move_list.remove(v).unwrap();
        self.move_list.get_mut(u).unwrap().extend(res);
        for t in self.adjacent(v) {
            self.inter_g.add_interference(*u, t);
        }
        self.inter_g.pop_node(v);
        if self.is_significant(u) && self.freeze_work_list.contains(u) {
            let res = self
                .freeze_work_list
                .remove(self.freeze_work_list.iter().position(|v| v == u).unwrap());
            self.spill_work_list.push(res);
        }
    }

    fn briggs(&self, u: &Temp, v: &Temp) -> bool {
        let mut k = 0;
        for n in self.adjacent(u) {
            if self.is_significant(&n) {
                k += 1;
            }
        }
        for n in self.adjacent(v) {
            if self.is_significant(&n) {
                k += 1;
            }
        }
        k < self.k
    }

    fn george(&self, u: &Temp, v: &Temp) -> bool {
        self.adjacent(v).iter().all(|t| self.ok(t, u))
    }

    fn ok(&self, t: &Temp, r: &Temp) -> bool {
        !self.is_significant(t) || self.precolored.contains(t) || self.inter_g.is_interfered(t, r)
    }

    fn add_work_list(&mut self, t: Temp) {
        if !self.precolored.contains(&t) && !self.move_related(&t) && !self.is_significant(&t) {
            let res = self.freeze_work_list.iter().position(|v| *v == t).unwrap();
            self.simplify_work_list
                .push(self.freeze_work_list.remove(res));
        }
    }

    fn node_moves(&self, t: &Temp) -> Vec<Move> {
        let wn = self.move_list.get(t).cloned().unwrap_or(vec![]);
        wn.into_iter()
            .filter(|v| self.active_moves.contains(v) || self.worklist_moves.contains(&v))
            .collect::<Vec<_>>()
    }

    fn enable_moves(&mut self, nodes: Vec<Temp>) {
        for n in nodes {
            let nm = self.node_moves(&n.clone());
            for m in nm {
                let i = self.active_moves.iter().position(|v| *v == m);
                if let Some(i) = i {
                    let removed = self.active_moves.remove(i);
                    self.worklist_moves.push(removed);
                }
            }
        }
    }

    fn move_related(&self, t: &Temp) -> bool {
        !self.node_moves(t).is_empty()
    }

    fn is_significant(&self, t: &Temp) -> bool {
        self.inter_g.degree(t) >= self.k
    }

    fn get_alias(&self, t: &Temp) -> Temp {
        self.coalesced_nodes.get(t).cloned().unwrap_or(*t)
    }

    fn adjacent(&self, t: &Temp) -> Vec<Temp> {
        self.inter_g
            .node(t)
            .outcome
            .clone()
            .into_iter()
            .filter(|v| !self.select_stack.contains(v) && !self.coalesced_nodes.contains(v))
            .collect()
    }
}

pub fn alloc<F: Frame>(trace: asm::Trace) -> asm::Trace {
    let flow = flow::flow_analyze(&trace);
    let (inter_g, work_list_move, work_list) = liveness::liveness_analyze(&flow, trace.done_label);
    let alloc = Alloc::new::<F>(flow, inter_g, work_list_move, work_list);
    // TODO return the rewritten trace
    trace
}
