mod misc;
mod new_lexer;
use crate::new_lexer::*;

// mod lexer;
// use crate::lexer::*;

// mod parser;
// use crate::parser::*;

// mod generator;
// use crate::generator::*;

fn main() {
    std::env::set_var("RUST_BACKTRACE", "1");

    let (file_path, file_name) = misc::get_file_name();
    let contents = misc::get_file_contents(file_path);
    println!("\n\n{:#?}\n\n", contents);
    let input = contents.into_iter().collect::<String>();

    let tokens = lex(input);
    // let program = parse(tokens);
    // generate(program, file_name);
}

fn lex(input: String) -> Vec<new_lexer::Token> {
    let mut lexer = new_lexer::Lexer::new(input);
    let tokens = lexer.tokenize();
    print_aligned_tokens_v2(&tokens);
    return tokens;
}

// fn parse(tokens: Vec<Token>) -> NodeProg {
//     let mut parser = Parser::new(tokens);
//     let program = match parser.parse_prog() {
//         Ok(t) => t,
//         Err(e) => panic!("\n{e}\n"),
//     };
//     println!("\n\n{:#?}\n\n", program.stmts);
//     return program;
// }

// fn generate(program: NodeProg, file_name: String) {
//     let output_path = format!("./output/{}.asm", file_name);
//     let mut generator = Generator::new(program, output_path.clone());
//     match generator.generate_prog() {
//         Ok(_) => println!("[COMPILER] output placed in '{output_path}'"),
//         Err(e) => panic!("\n{e}\n"),
//     };
// }

// fn print_aligned_tokens(tokens: &Vec<Token>) {
//     let longest_tok = tokens.iter().map(|t| t.kind.width()).max().unwrap_or(0);
//     let longest_ident = tokens
//         .iter()
//         .map(|t| format!("{:?}", t.value).len())
//         .max()
//         .unwrap_or(0);

//     for token in tokens {
//         token.debug_print(longest_tok, longest_ident);
//     }
// }

// fn lex_v1(input: String) -> Vec<Token> {
//     let mut lexer = lexer::Lexer::new(input);
//     let tokens: Vec<Token> = lexer.tokenize();
//     print_aligned_tokens(&tokens);
//     return tokens;
// }

fn print_aligned_tokens_v2(tokens: &Vec<Token>) {
    let max_len = tokens
        .iter()
        .map(|tok| format!("{}", tok.debug_print()).len())
        .max()
        .unwrap_or(0);
    for tok in tokens {
        let str = tok.debug_print();
        let whitespace = " ".repeat(max_len - str.len());
        println!("{} {whitespace}}}", str);
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
