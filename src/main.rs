#![allow(unused)]
use std::{
    cmp::max,
    collections::VecDeque,
    env, fs,
    io::{BufRead, BufReader, Write},
};
mod macros;

mod lex;
use lex::*;

mod parse;
use parse::*;

mod semantic;
use semantic::*;

mod code_gen;
use code_gen::Generator;

fn main() {
    env::set_var("RUST_BACKTRACE", "1");

    let (file_path, file_name) = get_file_name();
    let contents = get_file_contents(file_path);
    // println!("\n\n{:#?}\n\n", contents);

    let tokens = Lexer::new(contents).tokenize();
    // print_tokens(&tokens);
    let ast = parse(tokens);
    // println!("\n\n{ast:#?}\n\n");
    let gen_data = semantic_check(ast);
    println!("\n\n{:#?}\n\n", gen_data.ast);
    code_gen(gen_data, file_name);
}

/*----------------------------------------------------------------------------------------
---- Stuff -------------------------------------------------------------------------------
----------------------------------------------------------------------------------------*/

fn parse(tokens: VecDeque<Token>) -> AST {
    // TODO(TOM): REMOVE CLONE AFTER DEBUG
    let mut parser = Parser::new(tokens.clone());
    match parser.parse_ast() {
        Ok(t) => t,
        Err(e) => panic!("{tokens:#?}\n\n{e}\n"),
    }
}

fn semantic_check(ast: AST) -> HandoffData {
    // TODO(TOM): REMOVE CLONE AFTER DEBUG
    match semantic::Checker::check_ast(ast.clone()) {
        Ok(info) => info,
        Err(e) => {
            panic!("{ast:#?}\n\n{e}\n")
        }
    }
}

fn code_gen(data: HandoffData, file_name: String) {
    let file_path = format!("./output/{}.asm", file_name);
    let mut generator = Generator::new(data);
    match generator.gen_asm() {
        Ok(string) => {
            println!("[COMPILER] output placed in '{file_path}'");
            let mut file = fs::File::create(file_path).expect("Invalid filepath given.");
            // TODO(TOM): remove for functions impl
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

/*----------------------------------------------------------------------------------------
---- Misc --------------------------------------------------------------------------------
----------------------------------------------------------------------------------------*/

fn print_tokens(tokens: &VecDeque<Token>) {
    fn fmt_123(tok: &Token) -> String {
        match &tok.value {
            Some(val) => match tok.kind {
                TokenKind::Ident => format!("{:?}('{val}')", tok.kind),
                _ => format!("{:?}({val})", tok.kind),
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
            "Token {{ {val_str}{val_whitespace} | {x_whitespace}{x_str}, {y_str}{y_whitespace} }}"
        )
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
