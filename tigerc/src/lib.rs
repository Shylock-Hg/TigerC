use std::io::Write;

use translate::Fragment;

use crate::{amd64::FrameAmd64, frame::Frame, type_inference::TypeInference};

pub mod amd64;
pub mod asm;
pub mod asm_gen;
pub mod ast;
pub mod canon;
pub mod cursor;
pub mod data_layout;
pub mod escape;
pub mod flow;
pub mod frame;
pub mod graph;
pub mod ident_pool;
pub mod ir;
pub mod ir_gen;
pub mod liveness;
pub mod parser;
pub mod reg_alloc;
pub mod stack;
pub mod symbol_table;
pub mod temp;
pub mod tokenizer;
pub mod translate;
pub mod type_ast;
pub mod type_inference;

pub fn compile_file(f: &str, output_asm: &str) {
    let content = std::fs::read_to_string(f).unwrap();
    let it = tokenizer::tokenize(&content);
    let mut parser = parser::Parser::new(Box::new(it));
    let mut e = parser.parse();
    escape::Escape::new().escape(&mut e);
    let mut ti = type_inference::TypeInference::new();
    let te = ti.infer(&e).unwrap();
    let mut translator = translate::Translate::<amd64::FrameAmd64>::new();
    translator.translate(&te);
    let fragments = translator.fragments();
    let fragments = fragments
        .into_iter()
        .map(|f| match f {
            Fragment::Function { label, frame, body } => {
                // println!("origin statement: {:#?}", body);
                let regular_stmts = canon::canonicalize(body);
                // println!("{:?}: {:?}", label, regular_stmts.0.blocks);
                canon::Fragment::Function {
                    label,
                    frame,
                    body: regular_stmts,
                }
            }
            Fragment::StringLiteral(label, val) => {
                // println!("StringFragment");
                canon::Fragment::StringLiteral(label, val)
            }
        })
        .collect::<Vec<_>>();

    let mut output_asm_f = std::fs::File::create(output_asm).unwrap();
    writeln!(output_asm_f, "global main").unwrap();
    for (n, _) in TypeInference::external_function() {
        writeln!(output_asm_f, "extern {}", n).unwrap();
    }
    writeln!(output_asm_f, "section .rodata").unwrap();
    fragments.iter().for_each(|f| match f {
        canon::Fragment::Function { .. } => (),
        canon::Fragment::StringLiteral(label, val) => {
            // 0 for c-style compatible
            writeln!(output_asm_f, "{}:", label).unwrap();
            writeln!(output_asm_f, "dq {}", val.len()).unwrap();
            writeln!(output_asm_f, "db \"{}\", 0", val).unwrap();
        }
    });

    writeln!(output_asm_f, "section .text").unwrap();
    fragments.into_iter().for_each(|f| match f {
        canon::Fragment::Function { label, frame, body } => {
            let mut gen = asm_gen::Gen::<amd64::FrameAmd64>::new(body.1);
            gen.munch_trace(body.0);
            let mut trace = gen.result();
            trace.transform_last(|v| asm::Block {
                instructions: frame.borrow().proc_entry_exit2(v.instructions),
            });
            // the order of blocks in trace is invariant
            let trace = reg_alloc::alloc::<FrameAmd64>(trace, frame.clone());
            let trace = frame.borrow().proc_entry_exit3(trace);
            trace.write(&mut output_asm_f);
        }
        canon::Fragment::StringLiteral(..) => (),
    });

    output_asm_f.flush().unwrap();
    nasm(output_asm);
}

fn nasm(asm: &str) {
    let ret = std::process::Command::new("nasm")
        .arg("-f")
        .arg("elf64")
        .arg(asm)
        .status()
        .unwrap();
    if !ret.success() {
        panic!("{}", ret);
    }
}
