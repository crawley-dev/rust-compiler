#![allow(unused)]
#![warn(clippy::all)]
#![warn(clippy::pedantic)]
#![warn(clippy::nursery)]
#![warn(clippy::cargo)]
#![warn(clippy::complexity)]
#![warn(clippy::perf)]
#![warn(clippy::style)]
use anyhow::{Error, Result};
use std::{
    cmp::max,
    collections::VecDeque,
    fs,
    io::{BufRead, BufReader},
    panic::PanicHookInfo,
};

mod utils;
use utils::FILE_CONTENTS;

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
        Err(e) => println!("[COMPILER] Error: {e}"),
    }

    let file_name = utils::get_file_name();
    let contents_ref;
    unsafe {
        FILE_CONTENTS = utils::get_file_contents(&file_name);
        contents_ref = FILE_CONTENTS
            .iter()
            .map(|s| &**s as &'static str)
            .collect::<Vec<_>>();
    }

    let (tokens, error) = lex(contents_ref.as_slice());
    if let Some(e) = error {
        let (last_tok_pos, tok_len) = match tokens.back() {
            Some(tok) => (tok.pos, tok.len),
            None => ((0, 0), 0),
        };
        handle_error(e, contents_ref.as_slice(), last_tok_pos, tok_len);
        return;
    }

    // let (ast, error) = parse(tokens);
    // if let Some(e) = error {
    //     // let last_node_pos = match ast.last() {
    //     //     Some(node) =>
    //     //     None => 0,
    //     // };
    //     let last_node_pos = (0, 0);
    //     handle_error(e, contents_ref.as_slice(), last_node_pos, 0);
    //     return;
    // }

    // let gen_data = semantic_check(ast);
    // if let Some(e) = error {
    //     // let last_node_pos = match ast.last() {
    //     //     Some(node) =>
    //     //     None => 0,
    //     // };
    //     let last_node_pos = 0;
    //     handle_error(e, contents_ref.as_slice(), last_node_pos);
    //     return;
    // }

    // code_gen(gen_data, file_name);
}

/*----------------------------------------------------------------------------------------
---- Stags of Compilation ----------------------------------------------------------------
----------------------------------------------------------------------------------------*/

fn lex(contents: &[&str]) -> (VecDeque<Token>, Option<Error>) {
    utils::set_prefix(utils::LogPrefix::Lexical);

    let (lexer, error) = Lexer::new(contents).tokenize();

    if utils::do_log() {
        println!("{lexer}");
    }
    (lexer.tokens, error)
}

fn parse(tokens: VecDeque<Token>) -> (Ast, Option<Error>) {
    utils::set_prefix(utils::LogPrefix::Parse);

    let (ast, error) = Parser::new(tokens).parse_tokens();

    if utils::do_log() {
        println!("\n{:#?}\n", ast);
    }
    (ast, error)
}

fn semantic_check(ast: Ast) -> (Checker, Option<Error>) {
    utils::set_prefix(utils::LogPrefix::Semantic);

    let (checked, error) = Checker::new().check_ast(ast);

    if utils::do_log() {
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

fn handle_error(error: Error, file_contents: &[&str], error_end_pos: (u32, u32), tok_len: u32) {
    let panic_banner = match text_to_ascii_art::to_art(">Error!<".to_string(), "standard", 8, 0, 0)
    {
        Ok(art) => art,
        Err(e) => format!("[COMPILER] Error: {e}"),
    };

    let erroring_code = match file_contents.get(error_end_pos.1 as usize) {
        Some(line) => {
            // remove newline char && whitespace before first char
            // line.trim_start().trim_end()
            line.trim_end()
        }
        None => "unknown location (´。＿。｀)",
    };

    println!(
        "\n{panic_banner}\n\
         \nError Occurred at:\
         \n'{erroring_code}'\
         \n.{dots}{error_highlight}\n\
         \n{error}\
         \n{backtrace}\n",
        dots = ".".repeat(utils::get_pos().0 as usize),
        error_highlight = "^".repeat(tok_len as usize),
        backtrace = error.backtrace()
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
