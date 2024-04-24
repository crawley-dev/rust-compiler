/******************************************
*** WIP ***********************************
******************************************/

use std::fmt;

#[derive(Debug, Clone, PartialEq)]
pub enum OperandKind {
    LogicalOr,
    LogicalNot,
    LogicalAnd,
    // Comparison
    Equal,
    NotEqual,
    LessThan,
    LessEqual,
    GreaterThan,
    GreaterEqual,
    // Binary
    Multiply,
    Divide,
    Add,
    Subtract,
}

#[derive(Debug, Clone, PartialEq)]
pub enum SymbolKind {
    LineComment,
    OpenComment,
    CloseComment,
    OpenSquirly,
    CloseSquirly,
    OpenParen,
    CloseParen,
    Separator,
    StmtEnd,
    Assign,
    Arrow,
}

#[derive(Debug, Clone, PartialEq)]
pub enum KeywordKind {
    Exit,
    Let,
    If,
    Else,
    Function,
}

#[derive(Clone, PartialEq)]
pub enum Token {
    Symbol(SymbolKind),
    Operand(OperandKind),
    Keyword(KeywordKind),
    Ident(String),
    IntLit(String),
}

#[derive(Debug, Clone, Copy, PartialEq)]
enum BufKind {
    Word,
    IntLit,
    Symbols,
    Illegal,
}

impl Token {
    pub fn dwa(&self) -> String {
        let contents = match self {
            Token::Symbol(kind) => format!("{:?}", kind),
            Token::Operand(kind) => format!("{:?}", kind),
            Token::Keyword(kind) => format!("{:?}", kind),
            Token::Ident(val) | Token::IntLit(val) => format!("Val({})", val),
        };
        return format!("Token {{ {}", contents);
    }
}

pub struct Lexer {
    pos: usize,
    input: Vec<u8>,
    buffer: Vec<u8>,
    symbol_reg: [(&'static str, Token); 24],
    keyword_reg: [(&'static str, Token); 5],
}

impl Lexer {
    pub fn new(input: String) -> Lexer {
        // TODO: dynamically create & populate HashMaps of same len keys.
        let mut symbol_reg = [
            ("{", Token::Symbol(SymbolKind::OpenSquirly)),
            ("}", Token::Symbol(SymbolKind::CloseSquirly)),
            ("(", Token::Symbol(SymbolKind::OpenParen)),
            (")", Token::Symbol(SymbolKind::CloseParen)),
            (";", Token::Symbol(SymbolKind::StmtEnd)),
            ("=", Token::Symbol(SymbolKind::Assign)),
            (",", Token::Symbol(SymbolKind::Separator)),
            ("/", Token::Operand(OperandKind::Divide)),
            ("*", Token::Operand(OperandKind::Multiply)),
            ("+", Token::Operand(OperandKind::Add)),
            ("-", Token::Operand(OperandKind::Subtract)),
            ("!", Token::Operand(OperandKind::LogicalNot)),
            ("==", Token::Operand(OperandKind::Equal)),
            ("!=", Token::Operand(OperandKind::NotEqual)),
            ("<", Token::Operand(OperandKind::LessThan)),
            ("<=", Token::Operand(OperandKind::LessEqual)),
            (">", Token::Operand(OperandKind::GreaterThan)),
            (">=", Token::Operand(OperandKind::GreaterEqual)),
            ("||", Token::Operand(OperandKind::LogicalOr)),
            ("&&", Token::Operand(OperandKind::LogicalAnd)),
            ("=>", Token::Symbol(SymbolKind::Arrow)),
            ("//", Token::Symbol(SymbolKind::LineComment)),
            ("/*", Token::Symbol(SymbolKind::OpenComment)),
            ("*/", Token::Symbol(SymbolKind::CloseComment)),
        ];
        let mut keyword_reg = [
            ("exit", Token::Keyword(KeywordKind::Exit)),
            ("let", Token::Keyword(KeywordKind::Let)),
            ("fn", Token::Keyword(KeywordKind::Function)),
            ("if", Token::Keyword(KeywordKind::If)),
            ("else", Token::Keyword(KeywordKind::Else)),
        ];
        symbol_reg.sort_unstable_by_key(|(str, _)| str.len());
        keyword_reg.sort_unstable_by_key(|(str, _)| str.len());

        return Lexer {
            pos: 0,
            input: input.into_bytes(),
            buffer: Vec::new(),
            symbol_reg,
            keyword_reg,
        };
    }

    pub fn tokenize(&mut self) -> Vec<Token> {
        let mut tokens = Vec::new();
        while self.pos < self.input.len() {
            match self.next_token() {
                Some(tok) => tokens.push(tok),
                None => continue,
            };
        }
        return tokens;
    }

    fn next_token(&mut self) -> Option<Token> {
        self.buffer = Vec::new();
        let mut buf_type: BufKind = BufKind::Illegal;

        loop {
            // print!("\n");
            let next_char = match self.peek(0) {
                Some(char) => *char,
                None => break,
            };
            // println!("next_char is some, '{}'", next_char as char);

            let char_type = match next_char {
                b'a'..=b'z' | b'A'..=b'Z' | b'_' => BufKind::Word,
                b'0'..=b'9' => BufKind::IntLit,
                33..=47 | 58..=64 | 91..=96 | 123..=126 => BufKind::Symbols,
                _ if next_char.is_ascii_whitespace() => {
                    self.consume();
                    break;
                }
                _ => break,
            };
            // println!(" .. char is: {:?}", char_type);
            if self.buffer.is_empty() {
                buf_type = char_type.clone();
                // println!(" .. set buf_type: {:?}", buf_type);
            }
            if char_type != buf_type {
                break;
            }

            let ch = self.consume();
            // println!(" .. adding char {}", ch as char);
            self.buffer.push(ch);
        }

        let imm_buf_str = self.buffer.iter().map(|x| *x as char).collect::<String>();
        let mut buf_str = imm_buf_str.clone();

        // println!("buf: {}", buf_str);

        let tok = match buf_type {
            BufKind::Word => {
                while !buf_str.is_empty() {
                    match self
                        .keyword_reg
                        .iter()
                        .rev()
                        .find(|(str, _)| *str == buf_str)
                    {
                        Some((_, tok)) => Some(tok.clone()),
                        None => None,
                    };
                    buf_str.pop();
                }
                Some(Token::Ident(imm_buf_str))
            }
            BufKind::Symbols => {
                while !buf_str.is_empty() {
                    match self
                        .symbol_reg
                        .iter()
                        .rev()
                        .find(|(str, _)| *str == buf_str)
                    {
                        Some((_, tok)) => Some(tok.clone()),
                        None => {
                            buf_str.pop();
                            self.pos -= 1;
                            continue;
                        }
                    };
                }
                None // illegal.
            }
            BufKind::IntLit => Some(Token::IntLit(imm_buf_str)),
            BufKind::Illegal => None,
        };
    }

    fn peek(&self, offset: usize) -> Option<&u8> {
        return self.input.get(self.pos + offset);
    }

    fn consume(&mut self) -> u8 {
        let i = self.pos;
        self.pos += 1;
        // println!("consuming {}", self.input.get(i).copied().unwrap() as char);
        return self.input.get(i).copied().unwrap();
    }
}
