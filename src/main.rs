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
    let file_name = get_file_name();
    let contents = get_file_contents(&file_name);
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
        Ok(ast) => ast,
        Err(e) => panic!("\n{e}\n"),
        // Err(e) => panic!("{tokens:#?}\n\n{e}\n"),
    }
}

fn semantic_check(ast: AST) -> Checker {
    // TODO(TOM): REMOVE CLONE AFTER DEBUG
    match semantic::Checker::check_ast(ast.clone()) {
        Ok(data) => data,
        // Err(e) => panic!("\n{e}\n"),
        Err(e) => panic!("{ast:#?}\n\n{e}\n"),
    }
}

fn code_gen(data: Checker, file_name: String) {
    let file_path = format!("./output/{}.asm", file_name);
    let mut generator = Generator::new(data);
    match generator.gen_asm() {
        Ok(asm) => {
            /* formatting */
            let mut max_cmd_pos = 0; // longest cmd
                                     // let mut max_comma_pos = 0; // longest until comma
                                     // let mut max_comment_pos = 0; // longest until comment
            for line in asm.lines() {
                max_cmd_pos = max_cmd_pos.max(
                    line.chars()
                        .skip(4)
                        .position(char::is_whitespace)
                        .unwrap_or(0)
                        + 4,
                );
                // max_comma_pos = max_comma_pos.max(line.chars().position(|x| x == ',').unwrap_or(0));
                // max_comment_pos =
                //     max_comment_pos.max(line.chars().position(|x| x == ';').unwrap_or(0));
            }

            // applying formatting to each line.
            let mut fmt_asm = String::new();
            for line in asm.lines() {
                // Position is wrong by default! doesn't take into account iterations skipped!
                let cmd_pos = line
                    .chars()
                    .skip(4)
                    .position(|x| x.is_whitespace())
                    .unwrap_or(0)
                    + 4;
                // let comma_pos = line.chars().position(|x| x == ',').unwrap_or(0).
                // let comment_pos = line.chars().position(|x| x == ';').unwrap_or(0);

                if cmd_pos != 0 && max_cmd_pos - cmd_pos != 0 {
                    if line == "    cqo" {
                        fmt_asm += &line[..cmd_pos];
                        println!("pre:{:?}", &line[..cmd_pos]);
                        fmt_asm += " ".repeat(max_cmd_pos - cmd_pos).as_str(); // add extra whitespace
                        fmt_asm += &line[cmd_pos..];
                        fmt_asm += "\n";
                    } else {
                        fmt_asm += &line[..cmd_pos];
                        fmt_asm += " ".repeat(max_cmd_pos - cmd_pos).as_str(); // add extra whitespace
                        fmt_asm += &line[cmd_pos..];
                        fmt_asm += "\n";
                    }
                } else {
                    fmt_asm += line;
                }
            }
            /* formatting */

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

fn get_file_name() -> String {
    let args: String = env::args().skip(1).take(1).collect();
    let file_name = args.split('.').take(1).collect::<String>();
    let extension = args.split('.').skip(1).take(1).collect::<String>();
    if args.is_empty() {
        panic!("[COMPILER] No file path given!\n");
    } else if extension != "txt" {
        panic!("[COMPILER] Invalid file extension, '.txt' only\n")
    }
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
