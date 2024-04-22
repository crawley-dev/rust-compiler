mod misc;
// mod new_lexer;

mod lexer;
use crate::lexer::{Lexer, Token};

mod parser;
use crate::parser::Parser;

mod generator;
use crate::generator::Generator;

fn main() {
    std::env::set_var("RUST_BACKTRACE", "1");

    let (file_path, file_name) = misc::get_file_name();
    let contents = misc::get_file_contents(file_path);
    println!("\n\n{:#?}\n\n", contents);

    let flattened_contents = contents.into_iter().collect::<String>();
    let mut lexer = Lexer::new(flattened_contents);
    let tokens: Vec<Token> = lexer.tokenize();
    print_aligned_tokens(&tokens);

    let mut parser = Parser::new(tokens);
    let nodes = match parser.parse_prog() {
        Ok(t) => t,
        Err(e) => panic!("\n{e}\n"),
    };
    println!("\n\n{:#?}\n\n", nodes.stmts);

    let output_path = format!("./output/{}.asm", file_name);
    let mut generator = Generator::new(nodes, output_path.clone());
    match generator.generate_prog() {
        Ok(_) => println!("[COMPILER] output placed in '{output_path}'"),
        Err(e) => panic!("\n{e}\n"),
    };

    // UBUNTU bash script:
    // read file
    // sudo nasm -felf64 $file.asm -o $file.o
    // sudo ld $file.o -o $file
    // ./$file
    // echo $?

    // Usage:
    // bash gen.sh
    // FILE_NAME
}

fn print_aligned_tokens(tokens: &Vec<Token>) {
    let longest_tok = tokens.iter().map(|t| t.kind.width()).max().unwrap_or(0);
    let longest_ident = tokens
        .iter()
        .map(|t| format!("{:?}", t.value).len())
        .max()
        .unwrap_or(0);

    for token in tokens {
        token.debug_print(longest_tok, longest_ident);
    }
}
