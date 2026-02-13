use crate::temp::{Label, Temp};

#[derive(Debug)]
pub enum Instruction {
    Operation {
        assembly: String,
        destination: Vec<Temp>,
        source: Vec<Temp>,
        jump: Option<Vec<Label>>,
    },
    Label {
        assembly: String,
        label: Label,
    },
    Move {
        assembly: String,
        destination: Vec<Temp>,
        source: Vec<Temp>,
    },
}

impl Instruction {
    pub fn instance(&self) -> String {
        match self {
            Instruction::Label { assembly, .. } => assembly.clone(),
            Instruction::Move {
                assembly,
                destination,
                source,
                ..
            }
            | Instruction::Operation {
                assembly,
                destination,
                source,
                ..
            } => {
                let mut assembly = assembly.clone();
                for (i, v) in destination.iter().enumerate() {
                    assembly = assembly.replace(&format!("`d{}", i), &v.to_string());
                }
                for (i, v) in source.iter().enumerate() {
                    assembly = assembly.replace(&format!("`s{}", i), &v.to_string());
                }
                assembly
            }
        }
    }
}

pub struct Block {
    pub instructions: Vec<Instruction>,
}

impl Block {
    pub fn new(instructions: Vec<Instruction>) -> Block {
        Block { instructions }
    }

    pub fn last(&self) -> &Instruction {
        self.instructions.last().unwrap()
    }

    pub fn start_label(&self) -> Label {
        match self.instructions.first().unwrap() {
            Instruction::Label { label, .. } => *label,
            _ => panic!("block should start with a label"),
        }
    }
}

pub struct Trace {
    pub blocks: Vec<Block>,
    pub done_label: Label,
}

impl Trace {
    pub fn new(done_label: Label) -> Trace {
        Trace {
            blocks: vec![],
            done_label,
        }
    }

    pub fn add_block(&mut self, block: Block) {
        self.blocks.push(block);
    }

    pub fn mut_last(&mut self) -> &mut Block {
        self.blocks.last_mut().unwrap()
    }

    pub fn mv_last(&mut self) -> Block {
        self.blocks.pop().unwrap()
    }
}

#[cfg(test)]
mod tests {
    use crate::{ident_pool, temp::Temp};

    use super::Instruction;

    #[test]
    fn test_basic() {
        let inst = Instruction::Move {
            assembly: "mov `d0 `s0".to_string(),
            destination: vec![Temp::new_named(ident_pool::symbol("rax"))],
            source: vec![Temp::new_named(ident_pool::symbol("rsp"))],
        };
        let result = inst.instance();
        assert_eq!(result, "mov rax rsp");
    }

    #[test]
    fn test_empty() {
        let inst = Instruction::Operation {
            assembly: "".to_string(),
            destination: vec![Temp::new_named(ident_pool::symbol("rax"))],
            source: vec![Temp::new_named(ident_pool::symbol("rsp"))],
            jump: None,
        };
        let result = inst.instance();
        assert_eq!(result, "");
    }
}
