use std::{
    backtrace::Backtrace,
    cell::RefCell,
    collections::{HashMap, HashSet},
    rc::Rc,
};

use crate::{
    asm::{self, Block},
    asm_gen,
    flow::{self, FlowGraph},
    frame::{self, Frame, Variable},
    ir, ir_gen,
    liveness::{self, InterferenceGraph, Move, ProgramPoint},
    stack::Stack,
    temp::{Label, Temp},
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

    adj_set: HashSet<(Temp, Temp)>,
    adj_list: HashMap<Temp, Vec<Temp>>,
    degree: HashMap<Temp, usize>,
    move_list: HashMap<Temp, Vec<Move>>,
    alias: HashMap<Temp, Temp>,
    color: HashMap<Temp, Temp>, // The result of register assignment

    flow: FlowGraph<'a>,
    k: usize,
    precolored: Vec<Temp>,
    colors: Vec<Temp>,
}

fn adj_set_of_inter_graph(inter_g: &InterferenceGraph) -> HashSet<(Temp, Temp)> {
    let mut res = HashSet::new();
    inter_g.iter().for_each(|(t, n)| {
        n.outcome.iter().for_each(|v| {
            res.insert((*t, *v));
            res.insert((*v, *t));
        });
    });
    res
}

fn adj_list_of_inter_graph(inter_g: &InterferenceGraph) -> HashMap<Temp, Vec<Temp>> {
    inter_g
        .iter()
        .map(|(t, n)| (*t, n.outcome.clone()))
        .collect()
}

fn degree_of_inter_graph(inter_g: &InterferenceGraph) -> HashMap<Temp, usize> {
    inter_g.iter().map(|(t, n)| (*t, n.outcome.len())).collect()
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
            adj_set: adj_set_of_inter_graph(&inter_g),
            adj_list: adj_list_of_inter_graph(&inter_g),
            color: F::precolored().into_iter().map(|v| (v, v)).collect(),
            degree: degree_of_inter_graph(&inter_g),

            alias: HashMap::new(),
            move_list,
            flow,
            k,
            precolored: F::precolored(),
            colors: F::colors(),
        }
    }
}

impl<'a> Alloc<'a> {
    pub fn alloc<F: Frame>(
        &mut self,
        frame: Rc<RefCell<F>>,
        trace: asm::Trace,
    ) -> (asm::Trace, bool) {
        while !self.simplify_work_list.is_empty()
            || !self.freeze_work_list.is_empty()
            || !self.spill_work_list.is_empty()
            || !self.worklist_moves.is_empty()
        {
            if !self.simplify_work_list.is_empty() {
                self.simplify();
            } else if !self.worklist_moves.is_empty() {
                self.coalesce();
            } else if !self.freeze_work_list.is_empty() {
                self.freeze();
            } else if !self.spill_work_list.is_empty() {
                self.select_spill();
            }
        }
        self.assign_colors();
        if !self.spilled_nodes.is_empty() {
            let trace = self.rewrite_program(frame, trace);
            (trace, true)
        } else {
            (trace, false)
        }
    }

    fn rewrite_program<F: Frame>(
        &mut self,
        frame: Rc<RefCell<F>>,
        trace: asm::Trace,
    ) -> asm::Trace {
        let temp_loc = self
            .spilled_nodes
            .iter()
            .map(|v| {
                let var = frame.borrow_mut().allocate_local(ir::Variable(true));
                (*v, var)
            })
            .collect::<HashMap<_, _>>();

        asm::Trace {
            blocks: trace
                .blocks
                .into_iter()
                .map(|block| self.rewrite_block(frame.clone(), block, &temp_loc))
                .collect(),
            done_label: trace.done_label,
        }
    }

    fn rewrite_block<F: Frame>(
        &self,
        frame: Rc<RefCell<F>>,
        block: asm::Block,
        temp_loc: &HashMap<Temp, frame::Variable>,
    ) -> asm::Block {
        let mut gen = asm_gen::Gen::<F>::new(Label::new());
        for mut inst in block.instructions {
            let new_sources = inst
                .use_()
                .into_iter()
                .map(|s| {
                    if self.spilled_nodes.contains(&s) {
                        let new_source = gen.munch_expression(ir_gen::access_var(
                            &temp_loc[&s],
                            ir::Exp::Temp(F::fp()),
                        ));
                        new_source
                    } else {
                        s
                    }
                })
                .collect();
            inst.set_source(new_sources);
            let defs = inst.def();
            let inst_index = gen.emit(inst);
            let new_dests = defs
                .into_iter()
                .map(|d| {
                    if self.spilled_nodes.contains(&d) {
                        let new_dest = Temp::new();
                        gen.munch_statement(ir::Statement::Move {
                            dst: ir_gen::access_var(&temp_loc[&d], ir::Exp::Temp(F::fp())),
                            val: ir::Exp::Temp(new_dest),
                        });
                        new_dest
                    } else {
                        d
                    }
                })
                .collect();
            gen.get_mut(inst_index).set_dest(new_dests);
        }
        asm::Block::new(gen.raw_result())
    }

    fn assign_colors(&mut self) {
        while let Some(n) = self.select_stack.pop() {
            let mut colors = self.colors.clone();
            for adj in &self.adj_list[&n] {
                let adj = self.get_alias(adj);
                if self.colored_nodes.contains(&adj) || self.precolored.contains(&adj) {
                    colors.retain(|v| *v != self.color[&adj]);
                }
            }
            if colors.is_empty() {
                self.spilled_nodes.insert(n);
            } else {
                self.colored_nodes.insert(n);
                self.color.insert(n, colors.pop().unwrap());
            }
        }
        for n in &self.coalesced_nodes {
            self.color.insert(*n, self.color[&self.get_alias(&n)]);
        }
    }

    pub fn simplify(&mut self) {
        let n = self.simplify_work_list.pop();
        if let Some(n) = n {
            self.select_stack.push(n);
            let adjs = self.adjacent(&n);
            for adj in adjs {
                self.decrement_degree(&adj);
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
            } else if self.precolored.contains(&v) && self.adj_set.contains(&(u, v)) {
                self.constrained_moves.push(m);
                self.add_work_list(u);
                self.add_work_list(v);
            } else if (self.precolored.contains(&u) && self.george(&u, &v))
                || (!self.precolored.contains(&u) && self.briggs(&u, &v))
            {
                self.coalesced_moves.push(m);
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

    fn select_spill(&mut self) {
        let candidates = self
            .spill_work_list
            .iter()
            .filter(|v| !self.spilled_nodes.contains(v));
        let spill = candidates
            .max_by(|a, b| self.spill_cost(a).partial_cmp(&self.spill_cost(b)).unwrap())
            .cloned();
        if let Some(spill) = spill {
            self.simplify_work_list.push(
                self.spill_work_list.remove(
                    self.spill_work_list
                        .iter()
                        .position(|v| v == &spill)
                        .unwrap(),
                ),
            );
            self.freeze_moves(&spill);
        }
    }

    // spill temp with higher cost first
    fn spill_cost(&self, t: &Temp) -> f32 {
        self.degree[t] as f32
    }

    fn decrement_degree(&mut self, u: &Temp) {
        let dr = self.degree.get_mut(u).unwrap();
        let d = *dr;
        *dr -= 1;
        if d == self.k {
            let mut nodes = self.adjacent(u);
            nodes.push(*u);
            self.enable_moves(nodes);
            self.spill_work_list
                .remove(self.spill_work_list.iter().position(|v| v == u).unwrap());
            if self.move_related(&u) {
                self.freeze_work_list.push(*u);
            } else {
                self.simplify_work_list.push(*u);
            }
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
            self.spill_work_list.remove(
                self.spill_work_list
                    .iter()
                    .position(|val| val == v)
                    .unwrap(),
            );
        }
        self.coalesced_nodes.insert(*v);
        self.alias.insert(*v, *u);
        let res = self.move_list.remove(v).unwrap();
        self.move_list.get_mut(u).unwrap().extend(res);
        let adjs = self.adjacent(v);
        for t in adjs {
            self.add_edge(u, &t);
            self.decrement_degree(&t);
        }
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
        !self.is_significant(t) || self.precolored.contains(t) || self.adj_set.contains(&(*t, *r))
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
        self.degree[t] >= self.k
    }

    fn get_alias(&self, t: &Temp) -> Temp {
        if self.coalesced_nodes.contains(t) {
            self.get_alias(&self.alias[t])
        } else {
            *t
        }
    }

    fn adjacent(&self, t: &Temp) -> Vec<Temp> {
        self.adj_list[t]
            .clone()
            .into_iter()
            .filter(|v| !self.select_stack.contains(v) && !self.coalesced_nodes.contains(v))
            .collect()
    }

    fn add_edge(&mut self, a: &Temp, b: &Temp) {
        if !self.adj_set.contains(&(*a, *b)) && a != b {
            self.adj_set.insert((*a, *b));
            self.adj_set.insert((*b, *a));
            if !self.precolored.contains(a) {
                *self.degree.get_mut(a).unwrap() += 1;
                self.adj_list.get_mut(a).unwrap().push(*b);
            }
            if !self.precolored.contains(b) {
                *self.degree.get_mut(b).unwrap() += 1;
                self.adj_list.get_mut(b).unwrap().push(*a);
            }
        };
    }
}

pub fn alloc<F: Frame>(trace: asm::Trace, frame: Rc<RefCell<F>>) -> asm::Trace {
    let flow = flow::flow_analyze(&trace);
    let (inter_g, work_list_move, work_list) = liveness::liveness_analyze(&flow, trace.done_label);
    let mut allocator = Alloc::new::<F>(flow, inter_g, work_list_move, work_list);
    let (trace, continue_) = allocator.alloc(frame.clone(), trace.clone());
    if continue_ {
        alloc(trace, frame.clone())
    } else {
        trace
    }
}
