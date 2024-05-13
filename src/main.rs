use std::io::{self, BufRead, Write};
mod code_gen;
mod lex;
mod parse;

fn main() {
    std::env::set_var("RUST_BACKTRACE", "1");

    let (file_path, file_name) = get_file_name();
    let contents = get_file_contents(file_path);
    println!("\n\n{:#?}\n\n", contents);
    let input = contents.into_iter().collect::<String>();

    let tokens = lex(input);
    let program = parse(tokens);
    generate(program, file_name);
}

/*----------------------------------------------------------------------------------------
---- Stuff -------------------------------------------------------------------------------
----------------------------------------------------------------------------------------*/

fn lex(input: String) -> Vec<lex::Token> {
    let mut lexer = lex::Lexer::new(input);
    let tokens = lexer.tokenize();
    print_aligned_tokens_v2(&tokens);
    tokens
}

fn parse(tokens: Vec<lex::Token>) -> parse::NodeProg {
    let mut parser = parse::Parser::new(tokens);
    let program = match parser.parse_prog() {
        Ok(t) => t,
        Err(e) => panic!("\n{e}\n"),
    };
    println!("\n\n{:#?}\n\n", program.stmts);
    program
}

fn generate(program: parse::NodeProg, file_name: String) {
    let file_path = format!("./output/{}.asm", file_name);
    let mut generator = code_gen::Generator::new(program);
    match generator.gen_asm() {
        Ok(string) => {
            println!("[COMPILER] output placed in '{file_path}'");
            let mut file = std::fs::File::create(file_path).expect("Invalid filepath given.");
            file.write_all(string.as_bytes()).unwrap();
        }
        Err(e) => panic!("\n{e}\n"),
    };
}

/*----------------------------------------------------------------------------------------
---- Misc --------------------------------------------------------------------------------
----------------------------------------------------------------------------------------*/

fn print_aligned_tokens_v2(tokens: &Vec<lex::Token>) {
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

pub fn get_file_name() -> (String, String) {
    let file_path = std::env::args().skip(1).take(1).collect::<String>();
    if file_path.is_empty() {
        panic!("[COMPILER] Invalid File Path!\n");
    }
    let file_name = file_path
        .split('/')
        .nth(2)
        .unwrap_or("default.txt")
        .split('.')
        .next()
        .unwrap()
        .to_string();
    (file_path, file_name)
}

pub fn get_file_contents(file_path: String) -> Vec<String> {
    let file = std::fs::File::open(file_path)
        .unwrap_or_else(|_| panic!("[COMPILER] Error opening file\n"));
    io::BufReader::new(file)
        .lines()
        .map(|line| line.unwrap() + "\n")
        .collect::<Vec<String>>()
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
