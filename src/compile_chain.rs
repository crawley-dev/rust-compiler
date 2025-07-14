use std::collections::VecDeque;

use crate::{
    checker::Checker,
    lexer::{Lexer, Token},
    parser::{Ast, Parser},
    utils::{count_digits, pos, CompilerResult, Contents, LogPrefix, Logger, Pos},
};

#[derive(Debug, Clone)]
pub struct ParsableContents(pub VecDeque<Token>);

#[derive(Debug, Clone)]
pub struct CheckableContents(pub Ast);

#[derive(Debug, Clone)]
pub struct GeneratableContents(pub Checker);

pub trait Lexable {
    fn lex<'a>(&'a self) -> ParsableContents;
}

impl<'a> Lexable for &'a Contents {
    fn lex(&self) -> ParsableContents {
        Logger::set_prefix(LogPrefix::Lex);

        if Logger::print_logs() {
            println!("\nContents:\n{self:#?}\n");
        }

        let file_contents = self.contents.join("");
        let result = Lexer::new(&file_contents).tokenise();

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

pub fn handle_compile_error<T: std::fmt::Debug>(
    error_data: T,
    error: anyhow::Error,
    error_start: Pos,
    error_end: Pos,
) -> ! {
    let panic_banner = match text_to_ascii_art::to_art(">Error<".to_string(), "standard", 8, 0, 0) {
        Ok(art) => art,
        Err(e) => format!("[COMPILER] Ascii Art Gen Error: {e}"),
    };

    println!("start: {error_start:#?}, end: {error_end:#?}");

    let src_content = Contents::get_lines(error_start.y, error_end.y);
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

    std::process::exit(0)
}
