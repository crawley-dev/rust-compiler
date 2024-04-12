mod generator;
mod lexer;
mod misc;
mod parser;
use crate::generator::Generator;
use crate::lexer::{Lexer, Token};
use crate::parser::Parser;

fn main() {
    // std::env::set_var("RUST_BACKTRACE", "1");

    let (file_path, file_name) = misc::get_file_name();
    let contents = misc::get_file_contents(file_path);
    println!("{:#?}", contents);
    let flattened = contents.into_iter().collect::<String>();

    let mut lexer = Lexer::new(flattened);
    let tokens: Vec<Token> = lexer.tokenize();
    println!("{:#?}", tokens);

    let mut parser = Parser::new(tokens);
    let nodes = parser.parse_prog().expect("Unable to parse program.");
    println!("{:#?}", nodes.stmts);

    let output_path = format!("./output/{}.asm", file_name);
    let mut generator = Generator::new(nodes, output_path.clone());
    generator.generate_prog().expect("Unable to generate code.");

    let nasm_output = std::process::Command::new("nasm")
        .args(&["-f", "win64", &output_path, "-o", "out.o"])
        .output()
        .expect("Failed to execute nasm");

    if !nasm_output.status.success() {
        eprintln!("nasm command failed: {:?}", nasm_output);
        return;
    }

    let ld_output = std::process::Command::new("ld")
        .args(&["out.o", "-o", "out.exe"])
        .output()
        .expect("Failed to execute ld");

    if !ld_output.status.success() {
        eprintln!("ld command failed: {:?}", ld_output);
        return;
    }
}
