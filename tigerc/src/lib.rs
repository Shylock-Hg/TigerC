use std::io::Write;

use translate::Fragment;

use crate::frame::Frame;

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
pub mod parser;
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
                println!("StringFragment");
                canon::Fragment::StringLiteral(label, val)
            }
        })
        .collect::<Vec<_>>();

    let mut output_asm = std::fs::File::create(output_asm).unwrap();
    writeln!(output_asm, "global main").unwrap();
    writeln!(output_asm, "section .rodata").unwrap();
    fragments.iter().for_each(|f| match f {
        canon::Fragment::Function { .. } => (),
        canon::Fragment::StringLiteral(label, val) => {
            // 0 for c-style compatible
            writeln!(output_asm, "{}: db \"{}\", 0", label, val).unwrap();
        }
    });

    writeln!(output_asm, "section .text").unwrap();
    fragments.iter().for_each(|f| match f {
        canon::Fragment::Function { label, frame, body } => {
            let mut gen = asm_gen::Gen::<amd64::FrameAmd64>::new(&body.1);
            gen.munch_trace(&body.0);
            let mut trace = gen.result();
            let insts = frame
                .borrow()
                .proc_entry_exit2(trace.mv_last().instructions);
            trace.add_block(asm::Block {
                instructions: insts,
            });
            let flow = flow::flow_analyze(&(body.0), &(body.1));
        }
        canon::Fragment::StringLiteral(..) => (),
    });
}
