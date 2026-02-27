use std::{fmt::Display, io::Write};

use crate::temp::{Label, Temp};

#[derive(Debug, Clone)]
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

    pub fn def(&self) -> Vec<Temp> {
        match &self {
            Instruction::Operation { destination, .. } => destination.clone(),
            Instruction::Label { .. } => vec![],
            Instruction::Move { destination, .. } => destination.clone(),
        }
    }

    pub fn def_mut(&mut self) -> Option<&mut Vec<Temp>> {
        match self {
            Instruction::Operation { destination, .. } => Some(destination),
            Instruction::Label { .. } => None,
            Instruction::Move { destination, .. } => Some(destination),
        }
    }

    pub fn set_dest(&mut self, dest: Vec<Temp>) {
        match self {
            Instruction::Operation { destination, .. } => *destination = dest,
            Instruction::Label { .. } => (),
            Instruction::Move { destination, .. } => *destination = dest,
        }
    }

    pub fn use_(&self) -> Vec<Temp> {
        match &self {
            Instruction::Operation { source, .. } => source.clone(),
            Instruction::Label { .. } => vec![],
            Instruction::Move { source, .. } => source.clone(),
        }
    }

    pub fn use_mut(&mut self) -> Option<&mut Vec<Temp>> {
        match self {
            Instruction::Operation { source, .. } => Some(source),
            Instruction::Label { .. } => None,
            Instruction::Move { source, .. } => Some(source),
        }
    }

    pub fn set_source(&mut self, s: Vec<Temp>) {
        match self {
            Instruction::Operation { source, .. } => *source = s,
            Instruction::Label { .. } => (),
            Instruction::Move { source, .. } => *source = s,
        }
    }

    pub fn is_move(&self) -> bool {
        matches!(&self, Instruction::Move { .. })
    }
}

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
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

    pub fn transform_last<F>(&mut self, f: F)
    where
        F: FnOnce(Block) -> Block,
    {
        let val = self.blocks.pop().unwrap();
        self.blocks.push(f(val))
    }

    pub fn extend(&mut self, app: Self) {
        self.blocks.extend(app.blocks);
        self.done_label = app.done_label;
    }

    pub fn write<W: Write>(&self, w: &mut W) {
        for b in &self.blocks {
            for inst in &b.instructions {
                writeln!(w, "{}", inst.instance()).unwrap();
            }
        }
    }
}

impl Display for Trace {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for b in &self.blocks {
            for i in &b.instructions {
                writeln!(f, "{}", i.instance())?;
            }
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use crate::{ident_pool, ir::LowerIdent, temp::Temp};

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
    fn test_address() {
        let inst = Instruction::Move {
            assembly: "mov `d0 [`s0]".to_string(),
            destination: vec![Temp::new_debug(LowerIdent::Number(234))],
            source: vec![Temp::new_debug(LowerIdent::Number(233))],
        };
        let result = inst.instance();
        assert_eq!(result, "mov __anon_234 [__anon_233]");
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
