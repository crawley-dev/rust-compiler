#![allow(unused)]
use std::{
    env, fs,
    io::{BufRead, BufReader, Write},
};

mod lex;
use lex::*;

mod parse;
use parse::*;

mod semantic;

// mod code_gen;
// use code_gen::Generator;

fn main() {
    env::set_var("RUST_BACKTRACE", "1");

    let (file_path, file_name) = get_file_name();
    let contents = get_file_contents(file_path);
    // println!("\n\n{:#?}\n\n", contents);
    let input: String = contents.into_iter().collect();

    let tokens = Lexer::new(input).tokenize();
    // print_tokens(&tokens);
    let ast = parse(tokens);
    println!("\n\n{:#?}\n\n", ast);
    semantic_check(ast.clone()); // TODO(TOM): MAJOR SKILL ISSUE
                                 // code_gen(ast, file_name);
}

/*----------------------------------------------------------------------------------------
---- Stuff -------------------------------------------------------------------------------
----------------------------------------------------------------------------------------*/

fn parse(tokens: Vec<Token>) -> AST {
    let mut parser = Parser::new(tokens);
    match parser.parse_prog() {
        Ok(t) => t,
        Err(e) => panic!("\n{e}\n"),
    }
}

fn semantic_check(ast: AST) {
    match semantic::Checker::check_ast(ast) {
        Ok(t) => (),
        Err(e) => panic!("\n{e}\n"),
    }
}

/*
fn code_gen(ast: AST, file_name: String) {
    let file_path = format!("./output/{}.asm", file_name);
    let mut generator = Generator::new(ast);
    match generator.gen_asm() {
        Ok(string) => {
            println!("[COMPILER] output placed in '{file_path}'");
            let mut file = fs::File::create(file_path).expect("Invalid filepath given.");
            file.write_all(
                b"global _start\n\
                      _start:\n\
                     ; setup stack frame\n    \
                     push rbp\n    \
                     mov rbp, rsp\n    \
                     ; Program Start\n",
            )
            .unwrap();
            file.write_all(string.as_bytes()).unwrap();
        }
        Err(e) => panic!("\n{e}\n"),
    };
}
 */

/*----------------------------------------------------------------------------------------
---- Misc --------------------------------------------------------------------------------
----------------------------------------------------------------------------------------*/

fn print_tokens(tokens: &Vec<Token>) {
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

fn get_file_name() -> (String, String) {
    let file_path: String = env::args().skip(1).take(1).collect();
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

fn get_file_contents(file_path: String) -> Vec<String> {
    let file =
        fs::File::open(file_path).unwrap_or_else(|_| panic!("[COMPILER] Error opening file\n"));
    BufReader::new(file)
        .lines()
        .map(|line| line.unwrap() + "\n")
        .collect()
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
