/******************************************
*** WIP ***********************************
******************************************/

use std::{collections::HashMap, fmt};

const LOG_DEBUG_INFO: bool = false;

#[derive(Debug, Clone, Copy, PartialEq)]
enum BufKind {
    Word,
    IntLit,
    Symbol,
    Illegal,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    // symbols
    StmtEnd,
    Separator,
    OpenParen,
    CloseParen,
    LineComment,
    OpenSquirly,
    CloseSquirly,
    OpenMultiComment,
    CloseMultiComment,

    // operators
    Assign,
    Add,
    Divide,
    Subtract,
    Multiply,
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

    // keywords
    Exit,
    Let,
    If,
    Else,
    Function,
    Mutable,

    Ident,
    IntLit,
}

#[derive(Clone, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub value: Option<String>,
}

impl fmt::Debug for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.value {
            Some(val) => match &self.kind {
                TokenKind::Ident => write!(f, "{:?}('{val}')", self.kind),
                _ => write!(f, "{:?}({val})", self.kind),
            },
            None => write!(f, "{:?}", self.kind),
        }
    }
}

impl TokenKind {
    // Precedence hierarchy: higher = done first
    // .. going based of c precedence hierarchy.. at: https://ee.hawaii.edu/~tep/EE160/Book/chap5/subsection2.1.4.1.html#:~:text=The%20precedence%20of%20binary%20logical,that%20of%20all%20binary%20operators.
    pub fn get_prec(&self) -> i32 {
        return match self {
            TokenKind::Assign => 1,
            TokenKind::LogicalOr => 3,
            TokenKind::BitwiseOr => 5,
            TokenKind::LogicalAnd => 4,
            TokenKind::BitwiseXor => 6,
            TokenKind::BitwiseAnd => 7,
            TokenKind::Equal
            | TokenKind::NotEqual
            | TokenKind::LessThan
            | TokenKind::LessEqual
            | TokenKind::GreaterThan
            | TokenKind::GreaterEqual => 8,
            TokenKind::LeftShift | TokenKind::RightShift => 10,
            TokenKind::Subtract | TokenKind::Add => 11,
            TokenKind::Divide | TokenKind::Multiply => 12,
            TokenKind::LogicalNot | TokenKind::BitwiseNot => 13,
            _ => -1000, // Option<i32> takes more space, also immediately 'break's when found
        };
    }

    pub fn is_cmp_op(&self) -> bool {
        return match self {
            TokenKind::Equal
            | TokenKind::NotEqual
            | TokenKind::GreaterThan
            | TokenKind::GreaterEqual
            | TokenKind::LessThan
            | TokenKind::LessEqual => true,
            _ => false,
        };
    }

    pub fn is_binary_op(&self) -> bool {
        return match self {
            TokenKind::Divide
            | TokenKind::Multiply
            | TokenKind::Add
            | TokenKind::Subtract
            | TokenKind::LogicalOr
            | TokenKind::LogicalAnd
            | TokenKind::BitwiseOr
            | TokenKind::BitwiseXor
            | TokenKind::BitwiseAnd
            | TokenKind::Equal
            | TokenKind::NotEqual
            | TokenKind::LessThan
            | TokenKind::LessEqual
            | TokenKind::GreaterThan
            | TokenKind::GreaterEqual
            | TokenKind::LeftShift
            | TokenKind::RightShift => true,
            _ => false,
        };
    }

    pub fn is_unary_op(&self) -> bool {
        return match self {
            TokenKind::BitwiseNot | TokenKind::LogicalNot => true,
            _ => false,
        };
    }
}

pub struct Lexer {
    pos: usize,
    input: Vec<u8>,
    buffer: Vec<u8>,
    reg: HashMap<&'static str, TokenKind>,
    is_linecomment: bool,
    is_multicomment: bool,
}

impl Lexer {
    pub fn new(input: String) -> Lexer {
        let reg: HashMap<&'static str, TokenKind> = HashMap::from([
            // symbols
            (";", TokenKind::StmtEnd),
            (",", TokenKind::Separator),
            ("(", TokenKind::OpenParen),
            (")", TokenKind::CloseParen),
            ("{", TokenKind::OpenSquirly),
            ("}", TokenKind::CloseSquirly),
            ("//", TokenKind::LineComment),
            ("/*", TokenKind::OpenMultiComment),
            ("*/", TokenKind::CloseMultiComment),
            // operators
            ("=", TokenKind::Assign),
            ("+", TokenKind::Add),
            ("/", TokenKind::Divide),
            ("-", TokenKind::Subtract),
            ("*", TokenKind::Multiply),
            ("==", TokenKind::Equal),
            ("!=", TokenKind::NotEqual),
            ("<", TokenKind::LessThan),
            ("<=", TokenKind::LessEqual),
            (">", TokenKind::GreaterThan),
            (">=", TokenKind::GreaterEqual),
            ("!", TokenKind::LogicalNot),
            ("||", TokenKind::LogicalOr),
            ("&&", TokenKind::LogicalAnd),
            ("|", TokenKind::BitwiseOr),
            ("~", TokenKind::BitwiseNot),
            ("^", TokenKind::BitwiseXor),
            ("&", TokenKind::BitwiseAnd),
            (">>", TokenKind::LeftShift),
            ("<<", TokenKind::RightShift),
            // keywords
            ("exit", TokenKind::Exit),
            ("let", TokenKind::Let),
            ("fn", TokenKind::Function),
            ("if", TokenKind::If),
            ("else", TokenKind::Else),
            ("mut", TokenKind::Mutable),
        ]);

        return Lexer {
            pos: 0,
            input: input.into_bytes(),
            buffer: Vec::new(),
            reg,
            is_linecomment: false,
            is_multicomment: false,
        };
    }

    pub fn tokenize(&mut self) -> Vec<Token> {
        let mut tokens = Vec::new();
        while self.pos < self.input.len() {
            match self.next_token() {
                Some(tok) => match tok.kind {
                    TokenKind::LineComment => self.is_linecomment = true,
                    TokenKind::OpenMultiComment => self.is_multicomment = true,
                    TokenKind::CloseMultiComment => self.is_multicomment = false,
                    _ if self.is_multicomment => (),
                    _ => {
                        tokens.push(tok);
                        if LOG_DEBUG_INFO {
                            println!("new tok: {:?} | pos {}\n", tokens.last(), self.pos);
                        }
                    }
                },
                None => continue,
            };
        }
        return tokens;
    }

    fn next_token(&mut self) -> Option<Token> {
        self.buffer = Vec::new();
        let mut buf_type: BufKind = BufKind::Illegal;

        loop {
            let next_char = match self.peek(0) {
                Some(char) => *char,
                None => break,
            };

            if next_char == b'\n' {
                self.is_linecomment = false;
                // println!("removing {:?}", self.input.get(self.pos));
                self.input.remove(self.pos);
                // self.pos -= 1;
                break;
            }
            if self.is_linecomment {
                // self.consume();
                // println!("removing {:?}", self.input.get(self.pos));
                self.input.remove(self.pos);
                // self.pos -= 1;
                break;
            } else if next_char.is_ascii_whitespace() {
                // self.consume();
                // println!("removing {:?}", self.input.get(self.pos));
                self.input.remove(self.pos);
                // self.pos -= 1;
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

        let buf_str = self.buffer.iter().map(|x| *x as char).collect::<String>();

        if LOG_DEBUG_INFO {
            println!("\nbuf: {buf_str} | pos: {}", self.pos);
        }

        // whitespace is consumed at the end of the buffer
        // .. consumed tokens in loop != buffer size
        // .. if .is_whitespace() { self.consume() }
        // .. .. ^^ increments self.pos && not buffer.
        return match buf_type {
            BufKind::Illegal => None,
            BufKind::Word => self.match_word(buf_str),
            BufKind::Symbol => self.match_symbol(buf_str),
            BufKind::IntLit => Some(Token {
                kind: TokenKind::IntLit,
                value: Some(buf_str),
            }),
        };
    }

    fn match_word(&self, mut buf_str: String) -> Option<Token> {
        let imm_buf = buf_str.clone();
        while !buf_str.is_empty() {
            match self.reg.get(buf_str.as_str()) {
                Some(kind) => {
                    return Some(Token {
                        kind: kind.clone(),
                        value: None,
                    })
                }
                None => buf_str.pop(),
            };
        }
        return Some(Token {
            kind: TokenKind::Ident,
            value: Some(imm_buf),
        });
    }

    fn match_symbol(&mut self, mut buf_str: String) -> Option<Token> {
        while !buf_str.is_empty() {
            match self.reg.get(buf_str.as_str()) {
                Some(kind) => {
                    return Some(Token {
                        kind: kind.clone(),
                        value: None,
                    });
                }
                None => {
                    buf_str.pop();
                    self.pos -= 1;
                    if LOG_DEBUG_INFO {
                        println!("reduce {} | new pos: {}", buf_str, self.pos);
                    }
                }
            }
        }
        return None;
    }

    fn peek(&self, offset: usize) -> Option<&u8> {
        return self.input.get(self.pos + offset);
    }

    fn consume(&mut self) -> u8 {
        let i = self.pos;
        self.pos += 1;
        if LOG_DEBUG_INFO {
            println!(
                "consuming '{}' | new pos {}",
                self.input.get(i).copied().unwrap() as char,
                // i
                self.pos
            );
        }
        return self.input.get(i).copied().unwrap();
    }
}
