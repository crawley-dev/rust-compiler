use std::collections::VecDeque;

use crate::{
    checker::Checker,
    lexer::{Lexer, Token},
    parser::{Ast, Parser},
    utils::{handle_compile_error, pos, CompilerResult, LogPrefix, Logger},
};

pub struct ParsableContents(VecDeque<Token>);

pub struct CheckableContents(Ast);

pub struct GeneratableContents(Checker);

pub trait Lexable {
    fn tokenise(self) -> ParsableContents;
}

impl Lexable for &str {
    fn tokenise(self) -> ParsableContents {
        Logger::set_prefix(LogPrefix::Lex);

        if Logger::print_logs() {
            println!("\nContents:\n{self:#?}\n");
        }

        let result = Lexer::new(&self).tokenise();

        match result {
            CompilerResult::Ok(tokens) => {
                if Logger::print_output() {
                    println!("{tokens:?}");
                }
                ParsableContents(tokens)
            }
            CompilerResult::Err { data, error } => {
                let tokens = data.unwrap_or(VecDeque::new());
                let (start, tok_len) = match tokens.back() {
                    Some(tok) => (tok.start, tok.len),
                    None => (pos(0, 0), 0),
                };
                handle_compile_error(tokens, error, start, pos(start.x + tok_len, start.y))
            }
        }
    }
}

impl ParsableContents {
    pub fn parse(self) -> CheckableContents {
        Logger::set_prefix(LogPrefix::Parse);

        let result = Parser::new(self.0).parse_tokens();

        match result {
            CompilerResult::Ok(tokens) => {
                if Logger::print_output() {
                    println!("{tokens:?}");
                }
                CheckableContents(tokens)
            }
            CompilerResult::Err { data, error } => {
                let ast = data.unwrap(); // TODO(TOM): this should never fail, but never know.
                let end = Logger::get_pos();
                let start = pos(0, end.y);
                handle_compile_error(ast, error, start, end)
            }
        }
    }
}

impl CheckableContents {
    pub fn check(self) -> GeneratableContents {
        Logger::set_prefix(LogPrefix::Semantic);
        Logger::set_short_fmt(true);

        let result = Checker::new().check_ast(self.0);

        match result {
            CompilerResult::Ok(checker) => {
                if Logger::print_output() {
                    println!("{checker:#?}");
                }
                GeneratableContents(checker)
            }
            CompilerResult::Err { data, error } => {
                let checker = data.unwrap(); // TODO(TOM): this should never fail, but never know.
                let end = Logger::get_pos();
                let start = pos(0, end.y);
                handle_compile_error(checker, error, start, end)
            }
        }
    }
}

// impl GeneratableContents {
//     pub fn generate(self, file_name: String) {
//         // Call the code generator here
//         // For example:
//         // code_gen(self.0, file_name);
//     }
// }

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
