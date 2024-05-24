use std::{
    env, fs,
    io::{BufRead, BufReader, Write},
};

// mod lex;
// use lex::{Lexer, Token};
// mod parse;
// use parse::{Parser, AST};
mod lex;
use lex::*;

mod parse;
use parse::*;

mod semantic;
use semantic::Checker;

// mod code_gen;
// use code_gen::Generator;

fn main() {
    env::set_var("RUST_BACKTRACE", "1");

    let (file_path, file_name) = get_file_name();
    let contents = get_file_contents(file_path);
    println!("\n\n{:#?}\n\n", contents);
    let input: String = contents.into_iter().collect();

    let tokens = lex(input);
    let ast = parse(tokens);
    semantic(&ast);
    // code_gen(ast, file_name);
}

/*----------------------------------------------------------------------------------------
---- Stuff -------------------------------------------------------------------------------
----------------------------------------------------------------------------------------*/

fn lex(input: String) -> Vec<Token> {
    let mut lexer = Lexer::new(input);
    let tokens = lexer.tokenize();

    let max_len = tokens
        .iter()
        .map(|tok| format!("{tok:?}"))
        .filter(|str| str.find("{").is_none())
        .map(|str| str.len() - str.find("(").unwrap())
        .max()
        .unwrap_or(0);
    for tok in &tokens {
        let str = format!("{tok:?}");
        match str.find("{") {
            Some(_) => {
                let (split1, split2) = str.split_once("{").unwrap();
                println!("  - {split1}{}{{{split2}", " ".repeat(max_len - 10))
            }
            None => {
                let paren_idx = str.find("(").unwrap();
                let (split1, split2) = str.split_once("(").unwrap();
                println!(
                    "  - {split1}{}({split2}",
                    " ".repeat(max_len - paren_idx - 7)
                )
            }
        };
    }

    tokens
}

fn parse(tokens: Vec<Token>) -> AST {
    let mut parser = Parser::new(tokens);
    let program = match parser.parse_prog() {
        Ok(t) => t,
        Err(e) => panic!("\n{e}\n"),
    };
    println!("\n\n{:#?}\n\n", program.stmts);
    program
}

fn semantic(ast: &AST) {
    Checker::check_ast(ast).unwrap();
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

/*
 let max_len = tokens
        .iter()
        .map(|tok| format!("{}", format!("{tok:?}")).len())
        .max()
        .unwrap_or(0);
    for tok in &tokens {
        let str = format!("{tok:?}");
        let whitespace = " ".repeat(max_len - str.len());
        println!("Token {{ {str}{whitespace} }}")
    }

let max_len = tokens
        .iter()
        .map(|tok| format!("{tok:?}"))
        .filter(|str| str.find("{").is_none())
        .map(|str| str.len() - str.find("(").unwrap())
        .max()
        .unwrap_or(0);
    for tok in &tokens {
        let str = format!("{tok:?}");
        match str.find("{") {
            Some(_) => {
                let (split1, split2) = str.split_once("{").unwrap();
                println!("  - {split1}{}{{{split2}", " ".repeat(max_len - 10))
            }
            None => {
                let paren_idx = str.find("(").unwrap();
                let (split1, split2) = str.split_once("(").unwrap();
                println!(
                    "  - {split1}{}({split2}",
                    " ".repeat(max_len - paren_idx - 7)
                )
            }
        };
    }
*/

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
