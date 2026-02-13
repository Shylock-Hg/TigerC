use std::collections::HashMap;

use crate::{
    canon::{Block, Trace},
    graph::{Entry, Graph, Node},
    ir,
    temp::Label,
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

pub fn flow_analyze<'a>(trace: &'a Trace, done_label: &Label) -> FlowGraph<'a> {
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
            &ir::Statement::Jump { ref labels, .. } => {
                for target in labels {
                    outcome.push(Entry(*index.get(dbg!(target)).unwrap()));
                }
            }
            &ir::Statement::CJump {
                ref then,
                ref else_,
                ..
            } => {
                outcome.push(Entry(*index.get(then).unwrap()));
                outcome.push(Entry(*index.get(else_).unwrap()));
            }
            &ir::Statement::Label(l) => assert!(l == *done_label),
            _ => unreachable!(),
        }

        flow_graph.add_block(block, outcome);
    }

    for block in &trace.blocks {
        let last = block.last();
        match last {
            &ir::Statement::Jump { ref labels, .. } => {
                for target in labels {
                    flow_graph.add_income(
                        &Entry(*index.get(target).unwrap()),
                        Entry(*index.get(&block.start_label()).unwrap()),
                    );
                }
            }
            &ir::Statement::CJump {
                ref then,
                ref else_,
                ..
            } => {
                flow_graph.add_income(
                    &Entry(*index.get(then).unwrap()),
                    Entry(*index.get(&block.start_label()).unwrap()),
                );
                flow_graph.add_income(
                    &Entry(*index.get(else_).unwrap()),
                    Entry(*index.get(&block.start_label()).unwrap()),
                );
            }
            &ir::Statement::Label(l) => assert!(l == *done_label),
            _ => unreachable!(),
        }
    }

    flow_graph
}
