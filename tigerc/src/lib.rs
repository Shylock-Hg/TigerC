use translate::Fragment;

pub mod amd64;
pub mod ast;
pub mod canon;
pub mod cursor;
pub mod data_layout;
pub mod escape;
pub mod frame;
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

pub fn compile_file(f: &str) {
    let content = std::fs::read_to_string(f).unwrap();
    let it = tokenizer::tokenize(&content);
    let mut parser = parser::Parser::new(Box::new(it));
    let mut e = parser.parse();
    escape::Escape::new().escape(&mut e);
    let mut ti = type_inference::TypeInference::new();
    let te = ti.infer(&e).unwrap();
    let mut translator = translate::Translate::<amd64::FrameAmd64>::new();
    let ir = translator.translate(&te);
    let start = ir::Statement::Exp(ir);
    let regular_start = canon::canonicalize(start);
    let fragments = translator.fragments();
    let fragments = fragments.into_iter().map(|f| {
        if let Fragment::Function { label, frame, body } = f {
            let regular_stmts = canon::canonicalize(body.clone());
            println!("{:?}: {:?}", label, regular_stmts);
            Fragment::Function { label, frame, body }
        } else {
            f
        }
    });
}
