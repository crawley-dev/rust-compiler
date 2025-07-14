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
use crate::{checker::*, compile_chain::*, formatting::*, lexer::*, parser::*, utils::*};
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

mod checker;
mod formatting;
mod lexer;
mod parser;
mod utils;

// mod code_gen;
mod compile_chain;

fn main() {
    std::env::set_var("RUST_BACKTRACE", "1");
    std::env::set_var("RUST_LIB_BACKTRACE", "1");

    // Print Banner
    println!(
        "{}",
        text_to_ascii_art::to_art(">Toy Compiler<".to_string(), "standard", 8, 0, 0).unwrap()
    );

    // Get file contents, put it into the global buffer
    Contents::init(utils::get_cmd_arg(1));
    println!(
        "[COMPILER] File contents initialized:\n{:#?}",
        Contents::get_contents()
    );

    Contents::get().lex().parse().check();
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
