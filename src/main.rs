#![allow(unused)]
#![warn(clippy::all)]
#![warn(clippy::pedantic)]
#![warn(clippy::nursery)]
#![warn(clippy::cargo)]
#![warn(clippy::complexity)]
#![warn(clippy::perf)]
#![warn(clippy::style)]
use anyhow::Result;
use std::{
    cmp::max,
    collections::VecDeque,
    fs,
    io::{BufRead, BufReader, Write},
};

mod utils;
use utils::*;

mod lex;
use lex::*;

mod parse;
use parse::*;

mod semantic;
use semantic::*;

// mod code_gen;
// use code_gen::Generator;

fn main() {
    std::env::set_var("RUST_BACKTRACE", "1");
    std::env::set_var("RUST_LIB_BACKTRACE", "1");
    std::panic::set_hook(Box::new(|panic_info| {
        let panic_banner =
            match text_to_ascii_art::to_art(">Error!<".to_string(), "standard", 8, 0, 0) {
                Ok(art) => art,
                Err(e) => format!("[COMPILER] Error: {e}"),
            };
        let location = panic_info.location().unwrap();
        let message = if let Some(s) = panic_info.payload().downcast_ref::<&str>() {
            s
        } else if let Some(s) = panic_info.payload().downcast_ref::<String>() {
            s.as_str()
        } else {
            "Unknown error message type"
        };
        println!("\n{panic_banner}\n{message}");
    }));

    match text_to_ascii_art::to_art(">Toy Compiler<".to_string(), "standard", 8, 0, 0) {
        Ok(art) => println!("{}", art),
        Err(e) => println!("[COMPILER] Error: {e}"),
    }

    let file_name = get_file_name();
    let contents = get_file_contents(&file_name);
    let tokens = lex(contents);
    let ast = parse(tokens);
    let gen_data = semantic_check(ast);
    // code_gen(gen_data, file_name);
}

/*----------------------------------------------------------------------------------------
---- Stuff -------------------------------------------------------------------------------
----------------------------------------------------------------------------------------*/

fn lex(contents: Vec<String>) -> VecDeque<Token> {
    set_prefix(LogPrefix::Lexical);

    let tokens = Lexer::new(contents).tokenize();

    if do_log() {
        print_tokens(&tokens);
    }
    tokens
}

fn parse(tokens: VecDeque<Token>) -> Ast {
    set_prefix(LogPrefix::Parse);

    let ast = Parser::parse_ast(tokens);

    if do_log() {
        println!("\n{:#?}\n", ast);
    }

    ast
}

fn semantic_check(ast: Ast) -> Checker {
    set_prefix(LogPrefix::Semantic);

    let checked = Checker::check_ast(ast);
    println!("\n{:#?}\n", checked);
    checked
}

/*
fn code_gen(data: Checker, file_name: String) {
    let file_path = format!("./output/{}.asm", file_name);
    let mut generator = Generator::new(data);
    match generator.gen_asm() {
        Ok(asm) => {
            println!("[COMPILER] output placed in '{file_path}'");
            let mut file = fs::File::create(file_path).expect("Invalid filepath given.");
            // TODO(TOM): remove for functions impl
            // file.write_all(
            //     b"global _start\n\
            //           _start:\n\
            //          ; setup stack frame\n    \
            //          push rbp\n    \
            //          mov rbp, rsp\n    \
            //          ; Program Start\n",
            // )
            // .unwrap();
            file.write_all(asm.as_bytes()).unwrap();
        }
        Err(e) => panic!("\n{e}\n"),
    };
}
*/

/*----------------------------------------------------------------------------------------
---- Misc --------------------------------------------------------------------------------
----------------------------------------------------------------------------------------*/

fn print_tokens(tokens: &VecDeque<Token>) {
    fn fmt_123(tok: &Token) -> String {
        match &tok.value {
            Some(val) => match tok.as_str() {
                "" => format!("{:?}", tok.kind),
                val @ _ => match tok.kind {
                    TokenKind::Ident => format!("{:?}('{val}')", tok.kind),
                    _ => format!("{:?}({val})", tok.kind),
                },
            },
            None => format!("{:?}", tok.kind),
        }
    }

    let mut val_max_len = 0;
    let mut x_max_len = 0;
    let mut y_max_len = 0;
    for tok in tokens {
        let val_cur_len = fmt_123(tok).len();
        val_max_len = max(val_max_len, val_cur_len);

        let (x, y) = tok.pos;
        x_max_len = max(x_max_len, format!("{x}").len());
        y_max_len = max(y_max_len, format!("{y}").len());
    }

    for tok in tokens {
        let val_str = fmt_123(tok);
        let val_whitespace = " ".repeat(val_max_len - val_str.len());
        let x_str = format!("{x:?}", x = tok.pos.0);
        let x_whitespace = " ".repeat(x_max_len - x_str.len());
        let y_str = format!("{y:?}", y = tok.pos.1);
        let y_whitespace = " ".repeat(y_max_len - y_str.len());
        println!(
            "Token {{ {val_str}{val_whitespace} | (col: {y_whitespace}{y_str}, row: {x_whitespace}{x_str}) }}"
        )
    }
}

fn get_file_name() -> String {
    let args: String = std::env::args().skip(1).take(1).collect();
    assert!(!args.is_empty(), "[COMPILER] No file path given!\n");

    let file_name = args.split('.').take(1).collect::<String>();
    // TODO(TOM): re-enable after testing
    // let extension = args.split('.').skip(1).take(1).collect::<String>();
    // else if extension != "txt" {
    //     panic!("[COMPILER] Invalid file extension, '.txt' only\n")
    // }
    file_name
}

fn get_file_contents(file_name: &str) -> Vec<String> {
    let file = fs::File::open(format!("./examples/{file_name}.txt"))
        .unwrap_or_else(|_| panic!("[COMPILER] Error opening file '{file_name}'\n"));
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
