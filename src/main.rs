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
        let (last_tok_pos, tok_len) = match tokens.back() {
            Some(tok) => (tok.start, tok.len),
            None => {
                debug!("no tokens lex'd, error underline is incorrect");
                (pos(0, 0), 0)
            }
        };
        handle_error(e, last_tok_pos, tok_len);
        return;
    }

    fn drill_down(stmt: &Node<Stmt>) -> Option<Node<Stmt>> {
        match &stmt.node {
            Stmt::FnDecl { scope, .. } | Stmt::NakedScope(scope) => {
                if scope.node.stmts.len() == 0 {
                    return None;
                }
                drill_down(&scope.node.stmts[scope.node.stmts.len() - 1])
            }
            _ => return Some(stmt.clone()),
        };
        None
    }

    let (ast, error) = parse(tokens);
    if let Some(e) = error {
        let error_start_pos = match ast.stmts.last() {
            Some(node) => match drill_down(node) {
                Some(stmt) => stmt.start,
                None => pos(0, 0),
            },
            None => {
                // TODO(TOM): slight issue, stmts are encapsulated into a fn decl stmt, no lower statements are pushed to vec.
                debug!("no stmts parsed, error underline is incorrect");
                pos(0, 0)
            }
        };
        handle_error(e, error_start_pos, 0);
        return;
    }

    let (checker, error) = semantic_check(ast);
    if let Some(e) = error {
        let error_start_pos = match checker.ast.stmts.last() {
            Some(node) => match drill_down(node) {
                Some(stmt) => stmt.start,
                None => pos(0, 0),
            },
            None => {
                // TODO(TOM): slight issue, stmts are encapsulated into a fn decl stmt, no lower statements are pushed to vec.
                debug!("no stmts parsed, error underline is incorrect");
                pos(0, 0)
            }
        };
        handle_error(e, error_start_pos, 0);
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
fn handle_error(error: Error, error_start_pos: Pos, tok_len: u32) {
    let panic_banner = match text_to_ascii_art::to_art(">Error<".to_string(), "standard", 8, 0, 0) {
        Ok(art) => art,
        Err(e) => format!("[COMPILER] Ascii Art Gen Error: {e}"),
    };
    let file_contents = Contents::get_contents_ref();

    let erroring_code = match file_contents.get(error_start_pos.y as usize) {
        Some(line) => {
            // remove newline char && whitespace before first char
            // line.trim_start().trim_end()
            line.trim_end()
        }
        None => "unknown location (´。＿。｀)",
    };

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
        dots = ".".repeat(Logger::get_pos().x as usize),
        error_highlight = "^".repeat(tok_len as usize),
        backtrace = error.backtrace(),
    );
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
