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
use std::{
    cmp::max,
    collections::VecDeque,
    fs,
    io::{BufRead, BufReader},
    panic::PanicHookInfo,
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

fn main() {
    std::env::set_var("RUST_BACKTRACE", "1");
    std::env::set_var("RUST_LIB_BACKTRACE", "1");

    // Print Banner
    match text_to_ascii_art::to_art(">Toy Compiler<".to_string(), "standard", 8, 0, 0) {
        Ok(art) => println!("{}", art),
        Err(e) => {
            println!("[COMPILER] Error: {e}");
            return;
        }
    }

    // Get file name
    Contents::init();

    let (tokens, error) = lex(Contents::get_contents_ref());
    if let Some(e) = error {
        let (start, tok_len) = match tokens.back() {
            Some(tok) => (tok.start, tok.len),
            None => {
                debug!("no tokens lex'd, error underline is incorrect");
                (pos(0, 0), 0)
            }
        };
        handle_error(e, start, pos(start.x + tok_len, start.y));
        return;
    }

    let (ast, error) = parse(tokens);
    if let Some(e) = error {
        let (start, end) = match ast.stmts.last() {
            Some(node) => match drill_down(node) {
                Some(stmt) => (stmt.start, stmt.end),
                None => (pos(0, 0), pos(0, 0)),
            },
            None => (pos(0, 0), pos(0, 0)),
        };
        handle_error(e, start, end);
        return;
    }

    let (checker, error) = semantic_check(ast);
    if let Some(e) = error {
        let (start, end) = match checker.ast.stmts.last() {
            Some(node) => match drill_down(node) {
                Some(stmt) => (stmt.start, stmt.end),
                None => (pos(123, 456), pos(456, 123)),
            },
            None => (pos(1, 2), pos(1, 2)),
        };
        handle_error(e, start, end);
        return;
    }

    // code_gen(gen_data, file_name);
}

/*----------------------------------------------------------------------------------------
---- Stags of Compilation ----------------------------------------------------------------
----------------------------------------------------------------------------------------*/

fn lex(contents: &[&str]) -> (VecDeque<Token>, Option<Error>) {
    Logger::set_prefix(LogPrefix::Lexical);

    let (lexer, error) = Lexer::new(contents).tokenize();

    if Logger::print_output() {
        println!("{lexer}");
    }
    (lexer.tokens, error)
}

fn parse(tokens: VecDeque<Token>) -> (Ast, Option<Error>) {
    Logger::set_prefix(LogPrefix::Parse);

    let (ast, error) = Parser::new(tokens).parse_tokens();

    if Logger::print_output() {
        println!("\n{:#?}\n", ast);
    }
    (ast, error)
}

fn semantic_check(ast: Ast) -> (Checker, Option<Error>) {
    Logger::set_prefix(LogPrefix::Semantic);

    let (checked, error) = Checker::new().check_ast(ast);

    if Logger::print_output() {
        println!("\n{:#?}\n", checked);
    }
    (checked, error)
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

// TODO(TOM): change tok_len to error_end_pos
fn handle_error(error: Error, error_start: Pos, error_end: Pos) {
    let panic_banner = match text_to_ascii_art::to_art(">Error<".to_string(), "standard", 8, 0, 0) {
        Ok(art) => art,
        Err(e) => format!("[COMPILER] Ascii Art Gen Error: {e}"),
    };

    println!("error_start: {error_start:?}, error_end: {error_end:?}");
    let erroring_code = Contents::get_src(error_start, error_end)
        .iter()
        .flat_map(|x| x.chars())
        .collect::<String>();

    let dots = ".".repeat(Logger::get_pos().x as usize);
    let error_highlight = "";

    let len = error.chain().len();
    let mut error_chain = String::from("[\n");
    for (i, err) in error.chain().enumerate() {
        error_chain.push_str("");
        error_chain.push_str(&format!(
            "    {}\n",
            err.to_string().replace("\n", "\n    ")
        ));
        if i != len - 1 {
            error_chain.push_str(",\n");
        }
    }
    error_chain.push_str("]");

    println!(
        "\n{panic_banner}\n\
         \nError Occurred near:\
         \n'{erroring_code}'\
         \n.{dots}{error_highlight}\n\
         \nError Chain:\
         \n{error_chain}
         \nBacktrace:\
         \n{backtrace}\n",
        backtrace = error.backtrace(),
    );
}

fn drill_down(stmt: &Node<Stmt>) -> Option<Node<Stmt>> {
    println!("drilling down: {stmt:#?}");
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
