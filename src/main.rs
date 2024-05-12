mod misc;

mod lexer;
use crate::lexer::*;

mod parser;
use crate::parser::*;

// mod generator;
// use crate::generator::*;

fn main() {
    std::env::set_var("RUST_BACKTRACE", "1");

    let (file_path, file_name) = misc::get_file_name();
    let contents = misc::get_file_contents(file_path);
    println!("\n\n{:#?}\n\n", contents);
    let input = contents.into_iter().collect::<String>();

    let tokens = lex(input);
    let program = parse(tokens);
    // generate(program, file_name);
}

fn lex(input: String) -> Vec<lexer::Token> {
    let mut lexer = lexer::Lexer::new(input);
    let tokens = lexer.tokenize();
    print_aligned_tokens_v2(&tokens);
    return tokens;
}

fn parse(tokens: Vec<Token>) -> NodeProg {
    let mut parser = Parser::new(tokens);
    let program = match parser.parse_prog() {
        Ok(t) => t,
        Err(e) => panic!("\n{e}\n"),
    };
    println!("\n\n{:#?}\n\n", program.stmts);
    return program;
}

// fn generate(program: NodeProg, file_name: String) {
//     let output_path = format!("./output/{}.asm", file_name);
//     let mut generator = Generator::new(program, output_path.clone());
//     match generator.generate_prog() {
//         Ok(_) => println!("[COMPILER] output placed in '{output_path}'"),
//         Err(e) => panic!("\n{e}\n"),
//     };
// }

fn print_aligned_tokens_v2(tokens: &Vec<Token>) {
    // TODO: Improve.
    let max_len = tokens
        .iter()
        .map(|tok| format!("{}", format!("{tok:?}")).len())
        .max()
        .unwrap_or(0);
    for tok in tokens {
        let str = format!("{tok:?}");
        let whitespace = " ".repeat(max_len - str.len());
        println!("Token {{ {str}{whitespace} }}")
    }
}

// UBUNTU bash script:
// read file
// sudo nasm -felf64 $file.asm -o $file.o
// sudo ld $file.o -o $file
// ./$file
// echo $?

// Usage:
// bash gen.sh
// FILE_NAME
