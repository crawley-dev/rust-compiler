#![allow(unused, static_mut_refs)]
#![feature(try_trait_v2)]
#![warn(
    clippy::all,
    clippy::pedantic,
    clippy::nursery,
    clippy::cargo,
    clippy::complexity,
    clippy::perf,
    clippy::style
)]
use anyhow::{Error, Result};
use core::error;
use std::{
    cmp::max,
    collections::VecDeque,
    fs,
    io::{BufRead, BufReader},
    panic::PanicHookInfo,
    process::exit,
};

mod utils;
use utils::{count_digits, pos, Contents, LogPrefix, Logger, Pos};

mod lex;
use lex::*;

mod parse;
use parse::*;

mod semantic;
use semantic::*;

// mod code_gen;
// use code_gen::Generator;

mod formatting;

fn main() {
    std::env::set_var("RUST_BACKTRACE", "1");
    std::env::set_var("RUST_LIB_BACKTRACE", "1");

    // Print Banner
    println!(
        "{}",
        text_to_ascii_art::to_art(">Toy Compiler<".to_string(), "standard", 8, 0, 0).unwrap()
    );

    let mut var = 5;
    let test = var = 10;

    // Get file contents, init to global buffer
    Contents::init();
    println!(
        "[COMPILER] File contents initialized:\n{:#?}",
        Contents::get_contents_ref()
    );

    let lexer = lex(Contents::get_contents_ref());
    let ast = parse(lexer.tokens);
    // let checker = semantic_check(ast);
    // code_gen(gen_data, file_name);
}

/*----------------------------------------------------------------------------------------
---- Stages of Compilation ----------------------------------------------------------------
----------------------------------------------------------------------------------------*/

fn lex(contents: &[&str]) -> Lexer {
    Logger::set_prefix(LogPrefix::Lex);

    if Logger::print_logs() {
        println!("\nContents:\n{contents:#?}\n");
    }

    let (lexer, error) = Lexer::new(contents).tokenize();

    if let Some(e) = error {
        let (start, tok_len) = match lexer.tokens.back() {
            Some(tok) => (tok.start, tok.len),
            None => (pos(0, 0), 0),
        };
        handle_error(lexer.tokens, e, start, pos(start.x + tok_len, start.y));
    }

    if Logger::print_output() {
        println!("{lexer}");
    }
    lexer
}

fn parse(tokens: VecDeque<Token>) -> Ast {
    Logger::set_prefix(LogPrefix::Parse);
    // Logger::set_short_fmt(true);

    let (ast, error) = Parser::new(tokens).parse_tokens();

    if let Some(e) = error {
        let end = Logger::get_pos();
        let start = Pos { x: 0, y: end.y };
        handle_error(ast, e, start, end);
    }

    // Logger::set_short_fmt(false);
    if Logger::print_output() {
        println!("\n{:#?}\n", ast);
    }

    ast
}

fn semantic_check(ast: Ast) -> Checker {
    Logger::set_prefix(LogPrefix::Semantic);
    Logger::set_short_fmt(true);

    let (checker, error) = Checker::new().check_ast(ast);

    if let Some(e) = error {
        let end = Logger::get_pos();
        let start = Pos { x: 0, y: end.y };
        handle_error(checker, e, start, end);
    }

    Logger::set_short_fmt(false);
    if Logger::print_output() {
        println!("\n{:#?}\n", checker);
    }

    checker
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

fn handle_error<T: std::fmt::Debug>(
    error_data: T,
    error: Error,
    error_start: Pos,
    error_end: Pos,
) -> ! {
    let panic_banner = match text_to_ascii_art::to_art(">Error<".to_string(), "standard", 8, 0, 0) {
        Ok(art) => art,
        Err(e) => format!("[COMPILER] Ascii Art Gen Error: {e}"),
    };

    println!("start: {error_start:#?}, end: {error_end:#?}");

    let src_content = Contents::get_src_lines(error_start.y, error_end.y);
    let erroring_code = src_content
        .iter()
        .flat_map(|x| x.chars())
        .collect::<String>();

    // TODO(TOM): this doesn't cover some edge cases.
    let (highlight_padding, error_highlight);
    let first_char = src_content
        .iter()
        .flat_map(|x| x.chars())
        .position(|x| x.is_alphanumeric())
        .unwrap_or(0);
    highlight_padding = " ".repeat(first_char);
    error_highlight = "^".repeat(error_end.x as usize - first_char);

    let len = error.chain().len();
    let mut error_chain = String::from("[\n");
    for (i, err) in error.chain().enumerate().rev() {
        let err_msg = err.to_string();
        for line in err_msg.lines() {
            error_chain.push_str(&"    ");
            error_chain.push_str(line);
            error_chain.push('\n');
        }
        if let Some('\n') = error_chain.chars().last() {
            error_chain.pop();
        }
        error_chain.push_str(",\n");
    }
    error_chain.pop();
    error_chain.push_str("\n]");

    println!(
        "\n{panic_banner}\n\
        \nBacktrace:\
        \n{backtrace}\n
        \nError Data:\
        \n{error_data:#?}\n\
        \nError Occurred near:\
        \n{err_line_num}: {erroring_code}\
        \n{line_digits}  {highlight_padding}{error_highlight}\n\
        \nError Chain:\
        \n{error_chain}\n",
        err_line_num = error_start.y + 1,
        line_digits = " ".repeat(count_digits(error_start.y) as usize),
        backtrace = error.backtrace(),
    );

    exit(0);
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
