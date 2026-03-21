# TigerC Compiler

TigerC is a complete compiler implementation for the Tiger programming language, written in Rust. Tiger is a Pascal-like educational language from Andrew Appel's "Modern Compiler Implementation" books.

## Features

- **Full Compiler Pipeline**: Lexical analysis → Parsing → Semantic analysis → IR generation → x86-64 assembly → Executable
- **Modern Implementation**: Written in Rust with clean, modular architecture
- **Tiger Language Support**: Complete implementation including nested functions, arrays, records, and type inference
- **x86-64 Code Generation**: Native assembly output with register allocation
- **Runtime Library**: Memory management and I/O support for compiled programs

## Prerequisites

- [Rust](https://rustup.rs/) (latest stable)
- [NASM](https://www.nasm.us/) - Netwide Assembler
- GCC - For linking

Install on Ubuntu/Debian:
```bash
sudo apt-get install nasm gcc
```

Install on macOS:
```bash
brew install nasm
# GCC is provided by Xcode Command Line Tools
```

## Building

```bash
# Build debug version
cargo build

# Build release version
cargo build --release
```

## Usage

Compile a Tiger source file:

```bash
cargo run -- input.tig output.t
```

Or use the built binary:

```bash
./target/debug/tigerc input.tig output.t
```

### Example

```bash
cargo run -- ./tigerc/tests/testcases/queens.tig ./tigerc/tests/testcases/queens.t
./tigerc/tests/testcases/queens.t
```

## Project Structure

```
/
├── tigerc/              # Main compiler crate
│   ├── src/             # Compiler source modules
│   └── tests/           # Test files and test cases
├── tigerc-macros/       # Procedural macros for symbol generation
└── runtime/             # Runtime static library for compiled programs
```

### Compiler Pipeline

1. **Tokenization** (`tokenizer.rs`) - Lexical analysis with Unicode support
2. **Parsing** (`parser.rs`) - Pratt (top-down operator precedence) parser
3. **Escape Analysis** (`escape.rs`) - Detects variables escaping their scope
4. **Type Checking** (`type_inference.rs`) - Semantic analysis and type inference
5. **Translation** (`translate.rs`, `ir_gen.rs`) - AST to Intermediate Representation
6. **Canonicalization** (`canon.rs`) - IR normalization and basic block formation
7. **Code Generation** (`asm_gen.rs`, `amd64.rs`) - x86-64 assembly generation
8. **Assembly & Linking** - NASM + GCC to produce executable

## Testing

```bash
# Run all tests
cargo test

# Run tests for specific crate
cargo test -p tigerc

# Run specific test
cargo test test_compile3

# Show print output during tests
cargo test -- --nocapture

# Update snapshots (when output intentionally changes)
cargo insta review
```

## Tiger Language

Tiger is a statically-typed language with:

- **Primitive types**: `int`, `string`, `nil`
- **Composite types**: Arrays, Records
- **Control flow**: `if-then-else`, `while`, `for`, `break`
- **Functions**: Nested functions with static links, recursion
- **Variables**: Mutable with `:=` assignment
- **Let blocks**: Declaration scopes with `let...in...end`

### Example Program

```tiger
let
    var N := 8
    type intArray = array of int
    var col := intArray[N] of 0
    
    function printBoard() = (
        for i := 0 to N - 1 do (
            for j := 0 to N - 1 do
                print(if col[i] = j then " O" else " .");
            print("\n")
        );
        print("\n")
    )
in
    printBoard()
end
```

## Development

```bash
# Check, format, lint
cargo check
cargo fmt
cargo clippy

# Run with debug logging
RUST_LOG=debug cargo run -- input.tig output.t
```

## License

This project is for educational purposes.
