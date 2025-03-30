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
use utils::{pos, Contents, LogPrefix, Logger, Pos};

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
    let checker = semantic_check(ast);
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
        let (start, end) = match ast.stmts.last() {
            Some(node) => match drill_down(node) {
                Some(stmt) => (stmt.start, stmt.end),
                None => (pos(0, 0), pos(0, 0)),
            },
            None => (pos(0, 0), pos(0, 0)),
        };
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
        let (start, end) = match checker.ast.stmts.last() {
            Some(node) => match drill_down(node) {
                Some(stmt) => (stmt.start, stmt.end),
                None => (pos(0, 0), pos(0, 0)),
            },
            None => (pos(0, 0), pos(0, 0)),
        };
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

    let src_content = Contents::get_src_lines(error_start.y, error_end.y);
    let erroring_code = src_content // why does Vec<&str> not impl Display???
        .iter()
        .flat_map(|x| x.chars())
        .collect::<String>();

    let (highlight_padding, error_highlight);
    if error_start.y == error_end.y {
        highlight_padding = " ".repeat(error_start.x as usize);
        error_highlight = "^".repeat(max(0, error_end.x as i32 - error_start.x as i32) as usize);
    } else {
        let idx = max(0, error_end.y - error_start.y) as usize;
        let mut first_char_pos = src_content[idx]
            .find(|x: char| x.is_alphanumeric())
            .unwrap_or(0);
        highlight_padding = " ".repeat(first_char_pos);
        error_highlight = "^".repeat(error_end.x as usize - first_char_pos);
    }

    let len = error.chain().len();
    let mut error_chain = String::from("[\n");
    for (i, err) in error.chain().enumerate() {
        error_chain.push_str("");
        error_chain.push_str(&format!("    {}", err.to_string().replace("\n", "\n    ")));
        if i != len - 1 {
            error_chain.push_str(",\n");
        }
    }
    error_chain.push_str("\n]");

    println!(
        "\n{panic_banner}\n\
        \nBacktrace:\
        \n{backtrace}\n
        \nError Data:\
        \n{error_data:#?}\n\
        \nError Occurred near:\
        \n'{erroring_code}'\
        \n{highlight_padding}{error_highlight}\n\
        \nError Chain:\
        \n{error_chain}\n",
        backtrace = error.backtrace(),
    );

    exit(0);
}

fn drill_down(stmt: &Node<Stmt>) -> Option<Node<Stmt>> {
    match &stmt.node {
        Stmt::FnDecl { scope, .. } | Stmt::NakedScope(scope) => {
            if scope.node.stmts.len() == 0 {
                println!("no stmts in scope");
                return None;
            }
            drill_down(&scope.node.stmts[scope.node.stmts.len() - 1])
        }
        _ => return Some(stmt.clone()),
    };
    None
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
