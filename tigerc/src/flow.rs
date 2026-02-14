use std::collections::HashMap;

use crate::{
    asm::{Block, Instruction, Trace},
    graph::{Entry, Graph, Node},
};

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
