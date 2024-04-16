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
    println!("\n{:#?}\n", nodes.stmts);

    let output_path = format!("./output/{}.asm", file_name);
    let mut generator = Generator::new(nodes, output_path.clone());
    generator.generate_prog().expect("Unable to generate code.");

    // UBUNTU bash script:
    // read file
    // sudo nasm -felf64 $file.asm -o $file.o
    // sudo ld $file.o -o $file
    // ./$file
    // echo $?

    // Usage:
    // bash SCRIPT_NAME.sh
    // FILE_NAME
}
