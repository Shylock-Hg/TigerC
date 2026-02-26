use clap::Parser as cParser;


#[derive(cParser, Debug)]
struct Args {
    input: String,
    output_asm: String,
}

fn main() {
    let args = Args::parse();
    tiger_c::compile_file(&args.input, &args.output_asm);
}
