pub mod ast;
pub mod cursor;
pub mod escape;
pub mod frame;
pub mod ident_pool;
pub mod ir;
pub mod parser;
pub mod symbol_table;
pub mod tokenizer;
pub mod type_ast;
pub mod type_inference;

pub fn compile_file(f: &str) {
    let content = std::fs::read_to_string(f).unwrap();
    let it = tokenizer::tokenize(&content);
    let mut parser = parser::Parser::new(Box::new(it));
    let mut e = parser.parse();
    escape::Escape::new().escape(&mut e);
    let mut ti = type_inference::TypeInference::new();
    let _te = ti.infer(&e).unwrap();
}
