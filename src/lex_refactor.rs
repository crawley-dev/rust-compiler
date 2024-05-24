use bitflags::bitflags;
use std::collections::HashMap;
const LOG_DEBUG_INFO: bool = false;

#[derive(Debug, Clone, Copy, PartialEq)]
enum BufKind {
    Word,
    IntLit,
    Symbol,
    Illegal,
}

// TODO(TOM): try create a derive macro for 'KindTrait'
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum OpKind {
    DirectAssign,
    Add,
    Divide,
    Subtract,
    Multiply,
    Remainder,
    Equal,
    NotEqual,
    LessThan,
    LessEqual,
    GreaterThan,
    GreaterEqual,
    LogicalOr,
    LogicalNot,
    LogicalAnd,
    BitwiseOr,
    BitwiseNot,
    BitwiseXor,
    BitwiseAnd,
    LeftShift,
    RightShift,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum SymbolKind {
    StmtEnd,
    Separator,
    TypeSeparator,
    OpenParen,
    CloseParen,
    LineComment,
    OpenSquirly,
    CloseSquirly,
    OpenMultiComment,
    CloseMultiComment,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum KeywordKind {
    Exit,
    Let,
    If,
    Else,
    While,
    Break,
    Function,
    Mutable,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TokenKind {
    Ident,
    IntLit,
    Symbol,
    Keyword,
    Op,
}

pub trait KindTrait {
    fn matches_token(&self, token: &Token) -> bool;
}
impl KindTrait for SymbolKind {
    fn matches_token(&self, token: &Token) -> bool {
        if let Token::Symbol(kind) = token {
            self == kind
        } else {
            false
        }
    }
}
impl KindTrait for KeywordKind {
    fn matches_token(&self, token: &Token) -> bool {
        if let Token::Keyword(kind) = token {
            self == kind
        } else {
            false
        }
    }
}
impl KindTrait for OpKind {
    fn matches_token(&self, token: &Token) -> bool {
        match token {
            Token::Op { kind, .. } => self == kind,
            _ => false,
        }
    }
}
impl KindTrait for TokenKind {
    fn matches_token(&self, token: &Token) -> bool {
        match token {
            Token::Ident(_) => self == &TokenKind::Ident,
            Token::IntLit(_) => self == &TokenKind::IntLit,
            Token::Symbol(_) => self == &TokenKind::Symbol,
            Token::Keyword(_) => self == &TokenKind::Keyword,
            Token::Op { .. } => self == &TokenKind::Op,
        }
    }
}

bitflags! {
    #[derive(Debug, Clone, Copy, PartialEq)]
    pub struct OpFlags: u8 {
        const ASSIGN = 1 << 0;
        const ARITH = 1 << 1;
        const CMP = 1 << 2;
        const BITWISE = 1 << 3;
        const LOGICAL = 1 << 4;
        const UNARY = 1 << 5; // TODO(TOM): for now, left associative
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Ident(String),
    IntLit(String),
    Symbol(SymbolKind),
    Keyword(KeywordKind),
    Op {
        kind: OpKind,
        flags: OpFlags,
        prec: u8,
    },
}

impl Token {
    fn new_op(kind: OpKind, flags: OpFlags, prec: u8) -> Token {
        Token::Op { kind, flags, prec }
    }
}

pub struct Lexer {
    pos: usize,
    input: Vec<u8>,
    buffer: Vec<u8>,
    is_linecomment: bool,
    is_multicomment: bool,
    reg: HashMap<&'static str, Token>,
}

impl Lexer {
    pub fn new(input: String) -> Lexer {
        let reg = HashMap::from([
            (";", Token::Symbol(SymbolKind::StmtEnd)),
            (":", Token::Symbol(SymbolKind::TypeSeparator)),
            (",", Token::Symbol(SymbolKind::Separator)),
            ("(", Token::Symbol(SymbolKind::OpenParen)),
            (")", Token::Symbol(SymbolKind::CloseParen)),
            ("{", Token::Symbol(SymbolKind::OpenSquirly)),
            ("}", Token::Symbol(SymbolKind::CloseSquirly)),
            ("//", Token::Symbol(SymbolKind::LineComment)),
            ("/*", Token::Symbol(SymbolKind::OpenMultiComment)),
            ("*/", Token::Symbol(SymbolKind::CloseMultiComment)),
            //
            ("=", Token::new_op(OpKind::DirectAssign, OpFlags::ASSIGN, 1)),
            (
                "+=",
                Token::new_op(OpKind::Add, OpFlags::ARITH | OpFlags::ASSIGN, 11),
            ),
            (
                "-=",
                Token::new_op(OpKind::Subtract, OpFlags::ARITH | OpFlags::ASSIGN, 11),
            ),
            (
                "/=",
                Token::new_op(OpKind::Divide, OpFlags::ARITH | OpFlags::ASSIGN, 12),
            ),
            (
                "*=",
                Token::new_op(OpKind::Multiply, OpFlags::ARITH | OpFlags::ASSIGN, 12),
            ),
            (
                "%=",
                Token::new_op(OpKind::Remainder, OpFlags::ARITH | OpFlags::ASSIGN, 12),
            ),
            (
                "|=",
                Token::new_op(OpKind::BitwiseOr, OpFlags::BITWISE | OpFlags::ASSIGN, 5),
            ),
            (
                "^=",
                Token::new_op(OpKind::BitwiseXor, OpFlags::BITWISE | OpFlags::ASSIGN, 6),
            ),
            (
                "&=",
                Token::new_op(OpKind::BitwiseAnd, OpFlags::BITWISE | OpFlags::ASSIGN, 4),
            ),
            (
                ">>=",
                Token::new_op(OpKind::LeftShift, OpFlags::BITWISE | OpFlags::ASSIGN, 10),
            ),
            (
                "<<=",
                Token::new_op(OpKind::RightShift, OpFlags::BITWISE | OpFlags::ASSIGN, 10),
            ),
            ("+", Token::new_op(OpKind::Add, OpFlags::ARITH, 11)),
            ("-", Token::new_op(OpKind::Subtract, OpFlags::ARITH, 11)),
            ("/", Token::new_op(OpKind::Divide, OpFlags::ARITH, 12)),
            ("*", Token::new_op(OpKind::Multiply, OpFlags::ARITH, 12)),
            ("%", Token::new_op(OpKind::Remainder, OpFlags::ARITH, 12)),
            ("|", Token::new_op(OpKind::BitwiseOr, OpFlags::BITWISE, 5)),
            ("^", Token::new_op(OpKind::BitwiseXor, OpFlags::BITWISE, 6)),
            ("&", Token::new_op(OpKind::BitwiseAnd, OpFlags::BITWISE, 4)),
            (">>", Token::new_op(OpKind::LeftShift, OpFlags::BITWISE, 10)),
            (
                "<<",
                Token::new_op(OpKind::RightShift, OpFlags::BITWISE, 10),
            ),
            (
                "~",
                Token::new_op(OpKind::BitwiseNot, OpFlags::BITWISE | OpFlags::UNARY, 13),
            ),
            ("==", Token::new_op(OpKind::Equal, OpFlags::CMP, 8)),
            ("!=", Token::new_op(OpKind::NotEqual, OpFlags::CMP, 8)),
            ("<", Token::new_op(OpKind::LessThan, OpFlags::CMP, 8)),
            ("<=", Token::new_op(OpKind::LessEqual, OpFlags::CMP, 8)),
            (">", Token::new_op(OpKind::GreaterThan, OpFlags::CMP, 8)),
            (">=", Token::new_op(OpKind::GreaterEqual, OpFlags::CMP, 8)),
            ("||", Token::new_op(OpKind::LogicalOr, OpFlags::CMP, 3)),
            ("&&", Token::new_op(OpKind::LogicalAnd, OpFlags::CMP, 4)),
            (
                "!",
                Token::new_op(OpKind::LogicalNot, OpFlags::CMP | OpFlags::UNARY, 13),
            ),
            //
            ("exit", Token::Keyword(KeywordKind::Exit)),
            ("let", Token::Keyword(KeywordKind::Let)),
            ("fn", Token::Keyword(KeywordKind::Function)),
            ("if", Token::Keyword(KeywordKind::If)),
            ("else", Token::Keyword(KeywordKind::Else)),
            ("mut", Token::Keyword(KeywordKind::Mutable)),
            ("while", Token::Keyword(KeywordKind::While)),
            ("break", Token::Keyword(KeywordKind::Break)),
        ]);
        Lexer {
            pos: 0,
            input: input.into_bytes(),
            buffer: Vec::new(),
            is_linecomment: false,
            is_multicomment: false,
            reg,
        }
    }

    pub fn tokenize(&mut self) -> Vec<Token> {
        let mut tokens = Vec::new();
        while self.pos < self.input.len() {
            let tok = match self.next_token() {
                Some(tok) => tok,
                None => continue,
            };
            match tok {
                Token::Symbol(SymbolKind::LineComment) => self.is_linecomment = true,
                Token::Symbol(SymbolKind::OpenMultiComment) => self.is_multicomment = true,
                Token::Symbol(SymbolKind::CloseMultiComment) => self.is_multicomment = false,
                _ if self.is_multicomment => (),
                _ => {
                    tokens.push(tok);
                    if LOG_DEBUG_INFO {
                        println!(
                            "[LEX_DEBUG] new tok: {:?} | pos {}\n",
                            tokens.last(),
                            self.pos
                        );
                    }
                }
            };
        }
        tokens
    }

    fn next_token(&mut self) -> Option<Token> {
        self.buffer = Vec::new();
        let mut buf_type = BufKind::Illegal;

        loop {
            let next_char = match self.peek(0) {
                Some(char) => *char,
                None => break,
            };

            if next_char == b'\n' {
                if self.buffer.is_empty() {
                    self.pos += 1;
                }
                self.is_linecomment = false;
                break;
            } else if self.is_linecomment || next_char.is_ascii_whitespace() {
                if self.buffer.is_empty() {
                    self.pos += 1;
                }
                break;
            }

            let char_type = match next_char {
                b'a'..=b'z' | b'A'..=b'Z' | b'_' => BufKind::Word,
                b'0'..=b'9' if buf_type == BufKind::Word => BufKind::Word,
                b'0'..=b'9' => BufKind::IntLit,
                33..=47 | 58..=64 | 91..=96 | 123..=126 => BufKind::Symbol,
                _ => break,
            };

            if self.buffer.is_empty() {
                buf_type = char_type.clone();
            }
            if char_type != buf_type {
                break;
            }

            let ch = self.consume();
            self.buffer.push(ch);
        }

        let buf_str: String = self.buffer.iter().map(|x| *x as char).collect();
        if LOG_DEBUG_INFO {
            println!("\n[LEX_DEBUG] buf: '{buf_str}' | pos: {}", self.pos);
        }

        match buf_type {
            BufKind::Illegal => None,
            BufKind::Word => self.match_word(buf_str),
            BufKind::Symbol => self.match_symbol(buf_str),
            BufKind::IntLit => Some(Token::IntLit(buf_str)),
        }
    }

    fn match_word(&self, buf_str: String) -> Option<Token> {
        match self.reg.get(buf_str.as_str()) {
            Some(tok) => Some(tok.clone()),
            None => Some(Token::Ident(buf_str)),
        }
    }

    fn match_symbol(&mut self, mut buf_str: String) -> Option<Token> {
        while !buf_str.is_empty() {
            match self.reg.get(buf_str.as_str()) {
                Some(tok) => {
                    return Some(tok.clone());
                }
                None => {
                    buf_str.pop();
                    self.pos -= 1;
                    if LOG_DEBUG_INFO {
                        println!("[LEX_DEBUG] reduce {} | new pos: {}", buf_str, self.pos);
                    }
                }
            }
        }
        self.pos += 1;
        None
    }

    fn peek(&self, offset: usize) -> Option<&u8> {
        self.input.get(self.pos + offset)
    }

    fn consume(&mut self) -> u8 {
        let i = self.pos;
        self.pos += 1;
        if LOG_DEBUG_INFO {
            println!(
                "[LEX_DEBUG] consuming '{}' | new pos {}",
                self.input.get(i).copied().unwrap() as char,
                self.pos
            );
        }
        self.input.get(i).copied().unwrap()
    }
}
