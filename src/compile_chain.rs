use crate::{
    checker::{CheckedData, Checker},
    lexer::{Lexer, Token},
    parser::{Ast, Parser},
    r#gen::CodeGen,
    utils::{self, pos, CompilerResult, Contents, Logger, Pos},
};
use std::collections::VecDeque;

pub struct Globals {
    pub registered_compile_chains: usize,
    pub compile_chains: Vec<CompileChain>,
}

static mut GLOBALS: Globals = Globals {
    registered_compile_chains: 0,
    compile_chains: Vec::new(),
};

impl Globals {
    pub fn get(id: usize) -> &'static CompileChain {
        unsafe {
            GLOBALS
                .compile_chains
                .get(id)
                .expect("[COMPILER] Invalid compile chain ID")
        }
    }

    pub fn register_new_compile_chain(
        name: Option<&str>,
        contents: Contents,
        logger: Logger,
    ) -> &'static mut CompileChain {
        unsafe {
            let name = match name {
                Some(n) => n.to_string(),
                None => format!("CompileChain_{}", GLOBALS.registered_compile_chains),
            };
            let id = GLOBALS.registered_compile_chains;
            GLOBALS.registered_compile_chains += 1;
            GLOBALS.compile_chains.push(CompileChain {
                compile_chain_id: id,
                name,
                logger,
                contents,
                cur_stage: None,
            });
            GLOBALS
                .compile_chains
                .last_mut()
                .expect("[COMPILER] Failed to register new compile chain")
        }
    }
}

#[derive(Debug)]
pub struct CompileChain {
    pub compile_chain_id: usize,
    pub name: String,
    pub logger: Logger,
    pub contents: Contents,
    pub cur_stage: Option<CompileStage>,
}

#[derive(Debug)]
pub enum CompileStage {
    Lex(VecDeque<Token>),
    Parse(Ast),
    Check(CheckedData),
    CodeGen(String),
}

impl CompileChain {
    pub fn lex(&mut self) -> &mut Self {
        self.logger.set_prefix("Lex");
        self.logger.print_output = false;
        self.logger.print_logs = false;

        match self.cur_stage {
            Some(CompileStage::Lex(_)) => {
                panic!(
                    "[COMPILER] Cannot lex at any compilation stage other than the initial stage."
                );
            }
            _ => {}
        }

        let lexer = Lexer::new(
            &self.contents,
            &mut self.logger,
            self.compile_chain_id as u8,
        );
        let result = lexer.tokenise();

        match result {
            CompilerResult::Ok(tokens) => {
                if self.logger.print_output {
                    println!("{tokens:?}");
                }
                self.cur_stage = Some(CompileStage::Lex(tokens));
                return self;
            }
            CompilerResult::Err { data, error } => {
                let tokens = data.unwrap_or(VecDeque::new());
                let (start, tok_len) = match tokens.back() {
                    Some(tok) => (tok.start, tok.len),
                    None => (pos(0, 0), 0),
                };
                return self.handle_compile_error(
                    tokens,
                    error,
                    start,
                    pos(start.x + tok_len, start.y),
                );
            }
        }
    }

    pub fn parse(&mut self) -> &mut Self {
        self.logger.set_prefix("Parse");
        self.logger.print_output = true;
        self.logger.print_logs = false;

        let tokens = match self.cur_stage.take() {
            Some(CompileStage::Lex(tokens)) => tokens,
            _ => {
                panic!(
                    "[COMPILER] Cannot parse at any compilation stage other than lexical analysis."
                );
            }
        };
        let result = Parser::new(tokens, &self.contents, &mut self.logger).parse_tokens();

        match result {
            CompilerResult::Ok(data) => {
                if self.logger.print_output {
                    println!("{data:?}");
                }
                self.cur_stage = Some(CompileStage::Parse(data));
                return self;
            }
            CompilerResult::Err { data, error } => {
                let ast = data.unwrap(); // TODO(TOM): this should never fail, but never know.
                let end = self.logger.get_pos();
                let start = pos(0, end.y);
                return self.handle_compile_error(ast, error, start, end);
            }
        }
    }

    pub fn check(&mut self) -> &mut Self {
        self.logger.set_prefix("Semantic");
        self.logger.print_output = true;
        self.logger.print_logs = false;

        let ast = match self.cur_stage.take() {
            Some(CompileStage::Parse(ast)) => ast,
            _ => {
                panic!("[COMPILER] Cannot check at any compilation stage other than parsing.");
            }
        };
        let result = Checker::new(&self.contents, &mut self.logger).check_ast(ast);

        match result {
            CompilerResult::Ok(checker) => {
                if self.logger.print_output {
                    println!("{checker:#?}");
                }
                self.cur_stage = Some(CompileStage::Check((checker)));
                return self;
            }
            CompilerResult::Err { data, error } => {
                let checker = data.unwrap(); // TODO(TOM): this should never fail, but never know.
                let end = self.logger.get_pos();
                let start = pos(0, end.y);
                return self.handle_compile_error(checker, error, start, end);
            }
        }
    }

    fn gen(&mut self) -> &mut Self {
        self.logger.set_prefix("CodeGen");
        self.logger.print_output = true;
        self.logger.print_logs = true;

        let checked_data = match self.cur_stage.take() {
            Some(CompileStage::Check(data)) => data,
            _ => {
                panic!(
                    "[COMPILER] Cannot generate code at any compilation stage other than semantic checking."
                );
            }
        };

        let result = CodeGen::new(&self.contents, &mut self.logger).generate_code(checked_data);

        match result {
            CompilerResult::Ok(generated_code) => {
                println!("[COMPILER] Code generation successful.");
                self.cur_stage = Some(CompileStage::CodeGen(generated_code));
                return self;
            }
            CompilerResult::Err { data: _, error } => {
                let end = self.logger.get_pos();
                let start = pos(0, end.y);
                return self.handle_compile_error((), error, start, end);
            }
        }
    }

    pub fn handle_compile_error<T: std::fmt::Debug>(
        &mut self,
        error_data: T,
        error: anyhow::Error,
        error_start: Pos,
        error_end: Pos,
    ) -> ! {
        let panic_banner =
            match text_to_ascii_art::to_art(">Error<".to_string(), "standard", 8, 0, 0) {
                Ok(art) => art,
                Err(e) => format!("[COMPILER] Ascii Art Gen Error: {e}"),
            };

        println!("start: {error_start:#?}, end: {error_end:#?}");

        let src_content = self.contents.get_lines(error_start.y, error_end.y);
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
            line_digits = " ".repeat(utils::count_digits(error_start.y) as usize),
            backtrace = error.backtrace(),
        );

        std::process::exit(0)
    }
}
