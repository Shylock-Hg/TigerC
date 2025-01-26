use clap::Parser as cParser;

use tiger_c;

#[derive(cParser, Debug)]
struct Args {
    input: String,
}

fn main() {
    let args = Args::parse();
    tiger_c::compile_file(&args.input);
}
