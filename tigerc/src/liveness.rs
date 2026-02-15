use std::{
    collections::{HashMap, HashSet},
    hash::Hash,
    ptr,
};

use crate::{
    asm,
    flow::FlowGraph,
    graph::{Entry, Graph, Node},
    temp::{Label, Temp},
};

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

pub struct InterferenceGraph {
    pub g: Graph<Temp>,
    pub m_list: Vec<Move>,
}

impl InterferenceGraph {
    pub fn add_interference(&mut self, a: Temp, b: Temp) {
        let b_entry = {
            let res = self.g.get_node(&b);
            match res {
                Some(v) => v,
                None => {
                    self.g.add_node(Node::<Temp>::new(a, vec![], vec![]));
                    self.g.last_entry()
                }
            }
        };

        let a_entry = {
            let res = self.g.get_node(&a);
            let (a_entry, a_node) = match res {
                Some(v) => (v.clone(), self.g.mut_node(&v)),
                None => {
                    self.g.add_node(Node::<Temp>::new(a, vec![], vec![]));
                    (self.g.last_entry(), self.g.last_node_mut())
                }
            };
            a_node.add_outcome(b_entry);
            a_entry
        };

        let b_node = self.g.mut_node(&b_entry);
        b_node.add_outcome(a_entry);
    }

    fn add_move(&mut self, mv: Move) {
        self.m_list.push(mv)
    }
}

pub struct Move {
    pub dst: Temp,
    pub src: Temp,
}

pub fn liveness_analyze(flow: &FlowGraph, done_label: Label) -> InterferenceGraph {
    let mut live_map = HashMap::<ProgramPoint, HashSet<Temp>>::new();
    let done_node = flow.done_node();
    assert!(done_node.value().start_label() == done_label);
    assert!(done_node.outcome().is_empty());
    let mut visited = HashSet::<Entry>::new();
    let mut current = HashSet::<Temp>::new();
    build_live_map(flow, done_node, &mut live_map, &mut visited, &mut current);
    let live_map = live_map; // avoid modify
    let mut iter_g = InterferenceGraph {
        g: Graph::<Temp>::new(),
        m_list: vec![],
    };
    let mut visited = HashSet::<Entry>::new();
    build_iterference_graph(flow, &live_map, done_node, &mut iter_g, &mut visited);
    iter_g
}

fn build_iterference_graph<'a>(
    flow: &'a FlowGraph,
    live_map: &HashMap<ProgramPoint, HashSet<Temp>>,
    node: &Node<&'a asm::Block>,
    iter_g: &mut InterferenceGraph,
    visited: &mut HashSet<Entry>,
) {
    let block = node.value();
    for (offset, inst) in block.instructions.iter().enumerate() {
        // If def is not used, it won't interference b at all
        // But this will be prevent in previous stages
        let use_ = inst.use_();
        for def in inst.def() {
            for b in live_map.get(&ProgramPoint { block, offset }).unwrap() {
                // an optimization for move
                if !(inst.is_move() && use_.contains(b)) {
                    iter_g.add_interference(def, *b);
                } else {
                    iter_g.add_move(Move { dst: def, src: *b });
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
        build_iterference_graph(flow, live_map, node, iter_g, visited);
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
        def.iter().for_each(|v| {
            current.remove(v);
        });
        current.extend(inst.use_());
        live_map
            .entry(ProgramPoint { block, offset: i })
            .and_modify(|v| {
                v.extend(current.clone());
            })
            .or_insert(current.clone());
    }
    for income in node.income() {
        if visited.contains(income) {
            // the done block will never be re-visited, so we don't need to check it
            continue;
        }
        visited.insert(*income);
        let node = flow.get(income);
        build_live_map(flow, node, live_map, visited, current);
    }
}
