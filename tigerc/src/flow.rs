use std::collections::HashMap;

use crate::{
    asm::{Block, Instruction, Trace},
    graph::{Entry, Graph, Node},
    temp::Temp,
};

#[derive(Debug)]
pub struct FlowGraph<'a> {
    graph: Graph<&'a Block>,
}

impl<'a> FlowGraph<'a> {
    pub fn new() -> Self {
        FlowGraph {
            graph: Graph::new(),
        }
    }

    pub fn add_block(&mut self, block: &'a Block, outcome: Vec<Entry>) {
        self.graph.add_node(Node::new(block, vec![], outcome));
    }

    pub fn add_income(&mut self, block: &Entry, income: Entry) {
        self.graph.mut_node(block).add_income(income);
    }

    pub fn done_node(&self) -> &Node<&'a Block> {
        self.graph.last_node()
    }

    pub fn def(&self, block: &Entry, offset: usize) -> Vec<Temp> {
        let block = self.graph.node(&block);
        let inst = block.value().instructions.get(offset).unwrap();
        inst.def()
    }

    pub fn use_(&self, block: &Entry, offset: usize) -> Vec<Temp> {
        let block = self.graph.node(&block);
        let inst = block.value().instructions.get(offset).unwrap();
        inst.use_()
    }

    pub fn is_move(&self, block: &Entry, offset: usize) -> bool {
        let block = self.graph.node(&block);
        let inst = block.value().instructions.get(offset).unwrap();
        inst.is_move()
    }

    pub fn get(&self, block: &Entry) -> &Node<&'a Block> {
        self.graph.node(&block)
    }
}

pub fn flow_analyze<'a>(trace: &'a Trace) -> FlowGraph<'a> {
    let mut flow_graph = FlowGraph::new();
    let index = trace
        .blocks
        .iter()
        .enumerate()
        .map(|(i, block)| (block.start_label(), i))
        .collect::<HashMap<_, _>>();

    for block in &trace.blocks {
        let mut outcome = Vec::new();
        let last = block.last();
        match last {
            &Instruction::Operation { ref jump, .. } => {
                if let Some(labels) = jump {
                    for target in labels {
                        outcome.push(Entry(*index.get(target).unwrap()));
                    }
                }
            }
            &Instruction::Label { label, .. } => assert!(label == trace.done_label),
            _ => unreachable!(),
        }

        flow_graph.add_block(block, outcome);
    }

    for block in &trace.blocks {
        let last = block.last();
        match last {
            &Instruction::Operation { ref jump, .. } => {
                if let Some(labels) = jump {
                    for target in labels {
                        flow_graph.add_income(
                            &Entry(*index.get(&target).unwrap()),
                            Entry(*index.get(&block.start_label()).unwrap()),
                        );
                    }
                }
            }
            &Instruction::Label { label, .. } => assert!(label == trace.done_label),
            _ => unreachable!(),
        }
    }

    flow_graph
}
