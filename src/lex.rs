use bitflags::bitflags;
use std::{collections::HashMap, fmt};
const LOG_DEBUG_INFO: bool = false;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TokenKind {
    // Generic Symbols
    Comma,             // ","
    Colon,             // ":"
    SemiColon,         // ";"
    OpenParen,         // "("
    CloseParen,        // ")"
    LineComment,       // "//"
    OpenBrace,         // "{"
    CloseBrace,        // "}"
    OpenMultiComment,  // "/*"
    CloseMultiComment, // "*/"

    // Operators
    Ptr,    // "^"
    Eq,     // "="
    Add,    // "+"
    Sub,    // "-"
    Mul,    // "*"
    Quo,    // "/"
    Mod,    // "%"
    BitAnd, // "&"
    BitOr,  // "|"
    BitXor, // "~"
    AndNot, // "&~"
    Shl,    // "<<"
    Shr,    // ">>"
    // TODO(TOM): One's complement (bitwise not)

    // Combo Assign
    AddEq,    // "+="
    SubEq,    // "-="
    MulEq,    // "*="
    QuoEq,    // "/="
    ModEq,    // "%="
    AndEq,    // "&="
    OrEq,     // "|="
    XorEq,    // "~="
    AndNotEq, // "&~="
    ShlEq,    // "<<="
    ShrEq,    // ">>="

    // Comparison
    CmpAnd, // "&&"
    CmpOr,  // "||"
    CmpEq,  // "=="
    CmpNot, // "!"
    NotEq,  // "!="
    Lt,     // "<"
    Gt,     // ">"
    LtEq,   // "<="
    GtEq,   // ">="

    // Keywords
    Exit,
    Let,
    If,
    Else,
    While,
    Break,
    Fn,
    Mut,

    // Primitive Constructs
    Ident,
    IntLit,
}

#[derive(Debug)]
pub enum Associativity {
    Left,
    Right,
    None,
}

// TODO(TOM): Give TokenKind attributes, akin to impl, instead of some match statements
// .. declare all attributes in bitflag, in one place, i.e ATTR::ASSIGN | ATTR::ARITH
bitflags! {
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub struct TokenFlags: u8 {
        const ASSIGN = 1 << 0;
        const ARITH = 1 << 1;
        const CMP = 1 << 2;
        const LOG = 1 << 3;
        const BIT = 1 << 4;
        const UNARY = 1 << 5;
    }
}
impl TokenKind {
    pub fn get_flags(&self) -> TokenFlags {
        match self {
            // TokenKind::Ptr => TokenFlags::UNARY,  // "^"
            TokenKind::Eq => TokenFlags::ASSIGN, // "="
            TokenKind::Add => TokenFlags::ARITH, // "+"
            TokenKind::Sub => TokenFlags::ARITH | TokenFlags::UNARY, // "-"
            TokenKind::Mul => TokenFlags::ARITH, // "*"
            TokenKind::Quo => TokenFlags::ARITH, // "/"
            TokenKind::Mod => TokenFlags::ARITH, // "%"
            TokenKind::BitAnd => TokenFlags::BIT, // "&"
            TokenKind::BitOr => TokenFlags::BIT, // "|"
            TokenKind::BitXor => TokenFlags::BIT, // "~"
            TokenKind::AndNot => TokenFlags::BIT, // "&~"
            TokenKind::Shl => TokenFlags::BIT,   // "<<"
            TokenKind::Shr => TokenFlags::BIT,   // ">>"

            TokenKind::AddEq => TokenFlags::ASSIGN | TokenFlags::ARITH, // "+="
            TokenKind::SubEq => TokenFlags::ASSIGN | TokenFlags::ARITH, // "-="
            TokenKind::MulEq => TokenFlags::ASSIGN | TokenFlags::ARITH, // "*="
            TokenKind::QuoEq => TokenFlags::ASSIGN | TokenFlags::ARITH, // "/="
            TokenKind::ModEq => TokenFlags::ASSIGN | TokenFlags::ARITH, // "%="
            TokenKind::AndEq => TokenFlags::ASSIGN | TokenFlags::BIT,   // "&="
            TokenKind::OrEq => TokenFlags::ASSIGN | TokenFlags::BIT,    // "|="
            TokenKind::XorEq => TokenFlags::ASSIGN | TokenFlags::BIT,   // "~="
            TokenKind::AndNotEq => TokenFlags::ASSIGN | TokenFlags::BIT, // "&~="
            TokenKind::ShlEq => TokenFlags::ASSIGN | TokenFlags::BIT,   // "<<="
            TokenKind::ShrEq => TokenFlags::ASSIGN | TokenFlags::BIT,   // ">>="

            TokenKind::CmpNot => TokenFlags::LOG | TokenFlags::UNARY, // "!"
            TokenKind::CmpAnd => TokenFlags::CMP | TokenFlags::LOG,   // "&&"
            TokenKind::CmpOr => TokenFlags::CMP | TokenFlags::LOG,    // "||"
            TokenKind::CmpEq => TokenFlags::CMP,                      // "=="
            TokenKind::NotEq => TokenFlags::CMP,                      // "!="
            TokenKind::Lt => TokenFlags::CMP,                         // "<"
            TokenKind::Gt => TokenFlags::CMP,                         // ">"
            TokenKind::LtEq => TokenFlags::CMP,                       // "<="
            TokenKind::GtEq => TokenFlags::CMP,                       // ">="

            _ => TokenFlags::empty(),
        }
    }

    pub fn has_flags(&self, flags: TokenFlags) -> bool {
        self.get_flags().intersects(flags)
    }

    // Precedence hierarchy: higher = done first
    // .. going based of c precedence hierarchy.. at: https://ee.hawaii.edu/~tep/EE160/Book/chap5/subsection2.1.4.1.html#:~:text=The%20precedence%20of%20binary%20logical,that%20of%20all%20binary%20operators.
    // .. c++ associativity: https://en.wikipedia.org/wiki/Operators_in_C_and_C%2B%2B#Operator_precedence
    pub fn get_prec(&self) -> i32 {
        match self {
            TokenKind::Comma => 0,
            _ if self.has_flags(TokenFlags::ASSIGN) => 1,
            TokenKind::CmpOr => 2,
            TokenKind::CmpAnd => 3,
            TokenKind::BitOr => 5,
            TokenKind::BitXor => 6,
            TokenKind::BitAnd => 7,
            TokenKind::CmpEq | TokenKind::NotEq => 8,
            TokenKind::Lt | TokenKind::LtEq | TokenKind::Gt | TokenKind::GtEq => 9,
            TokenKind::Shl | TokenKind::Shr => 10,
            TokenKind::Sub | TokenKind::Add => 11,
            TokenKind::Mul | TokenKind::Quo | TokenKind::Mod => 12,
            TokenKind::CmpNot => 13,
            _ => -1,
        }
    }

    pub fn assign_to_arithmetic(&self) -> Result<TokenKind, String> {
        match self {
            TokenKind::AddEq => Ok(TokenKind::Add),
            TokenKind::SubEq => Ok(TokenKind::Sub),
            TokenKind::MulEq => Ok(TokenKind::Mul),
            TokenKind::QuoEq => Ok(TokenKind::Quo),
            TokenKind::ModEq => Ok(TokenKind::Mod),
            TokenKind::AndEq => Ok(TokenKind::BitAnd),
            TokenKind::OrEq => Ok(TokenKind::BitOr),
            TokenKind::XorEq => Ok(TokenKind::BitXor),
            TokenKind::AndNotEq => Ok(TokenKind::AndNot),
            TokenKind::ShlEq => Ok(TokenKind::Shl),
            TokenKind::ShrEq => Ok(TokenKind::Shr),
            _ => Err(format!("{self:?} cannot be converted to arithmetic")),
        }
    }

    pub fn get_associativity(&self) -> Associativity {
        match self {
            _ if self.has_flags(TokenFlags::ASSIGN) => Associativity::None,
            _ if self.has_flags(TokenFlags::UNARY) => Associativity::Left,
            _ => Associativity::Right,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
enum BufKind {
    Word,
    IntLit,
    Symbol,
    Illegal,
}

#[derive(Clone, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub value: Option<String>,
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
            // Generic Symbols
            (",", TokenKind::Comma),
            (":", TokenKind::Colon),
            (";", TokenKind::SemiColon),
            ("(", TokenKind::OpenParen),
            (")", TokenKind::CloseParen),
            ("{", TokenKind::OpenBrace),
            ("}", TokenKind::CloseBrace),
            ("//", TokenKind::LineComment),
            ("/*", TokenKind::OpenMultiComment),
            ("*/", TokenKind::CloseMultiComment),
            // Operators
            ("!", TokenKind::CmpNot),
            ("^", TokenKind::Ptr),
            ("=", TokenKind::Eq),
            ("+", TokenKind::Add),
            ("-", TokenKind::Sub),
            ("*", TokenKind::Mul),
            ("/", TokenKind::Quo),
            ("%", TokenKind::Mod),
            ("&", TokenKind::BitAnd),
            ("|", TokenKind::BitOr),
            ("~", TokenKind::BitXor),
            ("&~", TokenKind::AndNot),
            ("<<", TokenKind::Shl),
            (">>", TokenKind::Shr),
            // Combo Assign
            ("+=", TokenKind::AddEq),
            ("-=", TokenKind::SubEq),
            ("*=", TokenKind::MulEq),
            ("/=", TokenKind::QuoEq),
            ("%=", TokenKind::ModEq),
            ("&=", TokenKind::AndEq),
            ("|=", TokenKind::OrEq),
            ("~=", TokenKind::XorEq),
            ("&~=", TokenKind::AndNotEq),
            ("<<=", TokenKind::ShlEq),
            (">>=", TokenKind::ShrEq),
            // Comparison
            ("&&", TokenKind::CmpAnd),
            ("||", TokenKind::CmpOr),
            ("==", TokenKind::CmpEq),
            ("!=", TokenKind::NotEq),
            ("<", TokenKind::Lt),
            (">", TokenKind::Gt),
            ("<=", TokenKind::LtEq),
            (">=", TokenKind::GtEq),
            // Keywords
            ("exit", TokenKind::Exit),
            ("let", TokenKind::Let),
            ("fn", TokenKind::Fn),
            ("if", TokenKind::If),
            ("else", TokenKind::Else),
            ("mut", TokenKind::Mut),
            ("while", TokenKind::While),
            ("break", TokenKind::Break),
        ]);
        Lexer {
            pos: 0,
            input: input.into_bytes(),
            buffer: Vec::new(),
            reg,
            is_linecomment: false,
            is_multicomment: false,
        }
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
                            println!(
                                "[LEX_DEBUG] new tok: {:?} | pos {}\n",
                                tokens.last(),
                                self.pos
                            );
                        }
                    }
                },
                None => continue,
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

            // TODO: (done.. i think?)
            // requires (line + " \n"), e.g "//"
            // hits "//", consumes next.. " ", break
            // next iter: hits '\n', stop linecomment. CORRECT!
            // .. otherwise, it would consume "//" && \n, then: linecomment = true. BAD!
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
            BufKind::IntLit => Some(Token {
                kind: TokenKind::IntLit,
                value: Some(buf_str),
            }),
        }
    }

    fn match_word(&self, buf_str: String) -> Option<Token> {
        match self.reg.get(buf_str.as_str()) {
            Some(kind) => Some(Token {
                kind: kind.clone(),
                value: None,
            }),
            None => Some(Token {
                kind: TokenKind::Ident,
                value: Some(buf_str),
            }),
        }
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
