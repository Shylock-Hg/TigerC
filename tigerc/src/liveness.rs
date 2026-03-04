use std::{
    collections::{hash_map::Iter, HashMap, HashSet, VecDeque},
    hash::Hash,
    ptr,
};

use crate::{
    asm,
    flow::FlowGraph,
    frame::Frame,
    graph::{Entry, Node},
    ir::LowerIdent,
    symbol_table::SymbolTable,
    temp::{Label, Temp},
};

#[derive(Clone)]
pub struct ProgramPoint<'a> {
    pub block: &'a asm::Block,
    // offset in this Block
    pub offset: usize,
}

impl<'a> Hash for ProgramPoint<'a> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        ptr::hash(self.block, state);
        self.offset.hash(state);
    }
}

impl<'a> PartialEq for ProgramPoint<'a> {
    fn eq(&self, other: &Self) -> bool {
        if !ptr::eq(self.block, other.block) {
            return false;
        }
        self.offset.eq(&other.offset)
    }
}

impl<'a> Eq for ProgramPoint<'a> {}

pub struct LiveInterval<'a> {
    pub interval: Vec<ProgramPoint<'a>>,
}

#[derive(Clone)]
pub struct Neighbor {
    pub outcome: Vec<Temp>,
}

#[derive(Clone)]
pub struct InterferenceGraph {
    pub adj_set: HashSet<(Temp, Temp)>,
    pub adj_list: HashMap<Temp, Vec<Temp>>,
    pub degree: HashMap<Temp, usize>,
}

impl InterferenceGraph {
    pub fn new() -> Self {
        InterferenceGraph {
            adj_set: HashSet::new(),
            adj_list: HashMap::new(),
            degree: HashMap::new(),
        }
    }

    pub fn add_edge(&mut self, a: Temp, b: Temp, precolored: &Vec<Temp>) {
        if !self.adj_set.contains(&(a, b)) && a != b {
            self.adj_set.insert((a, b));
            self.adj_set.insert((b, a));
            if !precolored.contains(&a) {
                self.degree.entry(a).and_modify(|v| *v += 1).or_insert(1);
                self.adj_list
                    .entry(a)
                    .and_modify(|v| v.push(b))
                    .or_insert(vec![b]);
            }
            if !precolored.contains(&b) {
                self.degree.entry(b).and_modify(|v| *v += 1).or_insert(1);
                self.adj_list
                    .entry(b)
                    .and_modify(|v| v.push(a))
                    .or_insert(vec![a]);
            }
        };
    }

    //    fn add_move(&mut self, mv: Move) {
    //        self.m_list.push(mv)
    //    }

    //pub fn neighbor_mut(&mut self, t: &Temp) -> &mut Neighbor {
    //self.g.get_mut(t).unwrap()
    //}
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct Move {
    pub dst: Temp,
    pub src: Temp,
}

pub fn liveness_analyze<'a, F: Frame>(
    flow: &FlowGraph<'a>,
    done_label: Label,
) -> (InterferenceGraph, Vec<Move>, HashMap<Temp, Vec<Move>>) {
    let mut live_map = HashMap::<ProgramPoint, HashSet<Temp>>::new();
    let done_node = flow.done_node();
    assert!(done_node.value().start_label() == done_label);
    assert!(done_node.outcome().is_empty());
    build_live_map_loop(flow, &mut live_map);
    let live_map = live_map; // avoid modify
    let mut iter_g = InterferenceGraph::new();
    let mut work_list_moves = Vec::<Move>::new();
    let mut move_list = HashMap::<Temp, Vec<Move>>::new();
    let mut visited = HashSet::<Entry>::new();
    build_iterference_graph::<F>(
        flow,
        &live_map,
        done_node,
        &mut iter_g,
        &mut work_list_moves,
        &mut move_list,
        &mut visited,
    );
    (iter_g, work_list_moves, move_list)
}

fn build_iterference_graph<'a, F: Frame>(
    flow: &FlowGraph<'a>,
    live_map: &HashMap<ProgramPoint, HashSet<Temp>>,
    node: &Node<&'a asm::Block>,
    iter_g: &mut InterferenceGraph,
    work_list_moves: &mut Vec<Move>,
    move_list: &mut HashMap<Temp, Vec<Move>>,
    visited: &mut HashSet<Entry>,
) {
    let precolored = F::colors();
    let block = node.value();
    for (offset, inst) in block.instructions.iter().enumerate() {
        // If def is not used, it won't interference b at all
        // But this will be prevent in previous stages
        let use_ = inst.use_();
        for def in inst.def() {
            for b in live_map.get(&ProgramPoint { block, offset }).unwrap() {
                // an optimization for move
                // FIXME: this lead node outside of interference graph?
                /* !(inst.is_move() && use_.contains(b)) && */
                iter_g.add_edge(def, *b, &precolored);
            }
            for u in use_.iter() {
                if inst.is_move() {
                    work_list_moves.push(Move { dst: def, src: *u });
                    move_list
                        .entry(def)
                        .and_modify(|v| v.push(Move { dst: def, src: *u }))
                        .or_insert(vec![Move { dst: def, src: *u }]);
                    move_list
                        .entry(*u)
                        .and_modify(|v| v.push(Move { dst: def, src: *u }))
                        .or_insert(vec![Move { dst: def, src: *u }]);
                }
            }
        }
    }
    for income in node.income() {
        if visited.contains(income) {
            // the done block will never be re-visited, so we don't need to check it
            continue;
        }
        visited.insert(*income);
        let node = flow.get(income);
        build_iterference_graph::<F>(
            flow,
            live_map,
            node,
            iter_g,
            work_list_moves,
            move_list,
            visited,
        );
    }
}

fn build_live_map_loop<'a>(
    flow: &'a FlowGraph,
    live_map: &mut HashMap<ProgramPoint<'a>, HashSet<Temp>>,
) {
    // We will analyze live map from each block, this won't lose any block even with circle
    // TODO: But is it complete?
    for node in flow.nodes() {
        let mut visited = HashSet::new();
        let mut current = HashSet::new();
        build_live_map(flow, node, live_map, &mut visited, &mut current);
    }
}

fn build_live_map<'a>(
    flow: &'a FlowGraph,
    node: &Node<&'a asm::Block>,
    live_map: &mut HashMap<ProgramPoint<'a>, HashSet<Temp>>,
    visited: &mut HashSet<Entry>,
    current: &mut HashSet<Temp>,
) {
    // reverse iterate flow
    let block = node.value();
    for (i, inst) in block.instructions.iter().enumerate().rev() {
        let def = inst.def();
        current.extend(def.clone());
        live_map
            .entry(ProgramPoint { block, offset: i })
            .and_modify(|v| {
                v.extend(current.clone());
            })
            .or_insert(current.clone());
        current.retain(|v| !def.contains(v));
        current.extend(inst.use_());
    }
    for income in node.income() {
        if visited.contains(income) {
            continue;
        }
        visited.insert(*income);
        let node = flow.get(income);
        let mut derived = current.clone(); // TODO merge this into visited to avoid clone?
        build_live_map(flow, node, live_map, visited, &mut derived);
    }
}
