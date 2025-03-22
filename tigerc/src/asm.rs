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
}
