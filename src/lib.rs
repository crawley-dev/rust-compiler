#![allow(static_mut_refs)]
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
pub mod checker;
pub mod formatting;
pub mod lexer;
pub mod parser;
pub mod utils;

// mod code_gen;
pub mod compile_chain;
