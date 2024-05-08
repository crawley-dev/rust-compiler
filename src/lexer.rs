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
    // ArithmeticLeftShift, // Arithmetic Shifting
    // ArithmeticRightShift,
    BitwiseOr,
    BitwiseNot,
    BitwiseXor,
    BitwiseAnd,
    LeftShift,
    RightShift,
    // BitwiseLeftShift, // Technically Logical shifting, but too confusing.
    // BitwiseRightShift,

    // keywords
    Exit,
    Let,
    If,
    Else,
    While,
    Break,
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
        match self {
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
            TokenKind::Divide | TokenKind::Multiply | TokenKind::Remainder => 12,
            TokenKind::LogicalNot | TokenKind::BitwiseNot => 13,
            _ => -1000, // Option<i32> takes more space, also immediately 'break's when found
        }
    }

    pub fn is_arithmetic(&self) -> bool {
        match self {
            TokenKind::Add
            | TokenKind::Subtract
            | TokenKind::Divide
            | TokenKind::Multiply
            | TokenKind::Remainder => true,
            _ => false,
        }
    }

    pub fn is_comparison(&self) -> bool {
        match self {
            TokenKind::Equal
            | TokenKind::NotEqual
            | TokenKind::GreaterThan
            | TokenKind::GreaterEqual
            | TokenKind::LessThan
            | TokenKind::LessEqual => true,
            _ => false,
        }
    }

    pub fn is_bitwise(&self) -> bool {
        match self {
            TokenKind::BitwiseOr
            | TokenKind::BitwiseXor
            | TokenKind::BitwiseAnd
            | TokenKind::BitwiseNot
            | TokenKind::LeftShift
            | TokenKind::RightShift => true,
            _ => false,
        }
    }

    pub fn is_logical(&self) -> bool {
        match self {
            TokenKind::LogicalOr | TokenKind::LogicalAnd | TokenKind::LogicalNot => true,
            _ => false,
        }
    }

    pub fn is_binary(&self) -> bool {
        match self {
            TokenKind::Divide
            | TokenKind::Multiply
            | TokenKind::Remainder
            | TokenKind::Add
            | TokenKind::Subtract
            | TokenKind::LogicalOr
            | TokenKind::LogicalAnd
            | TokenKind::BitwiseOr
            | TokenKind::BitwiseXor
            | TokenKind::BitwiseAnd
            | TokenKind::LeftShift
            | TokenKind::RightShift
            | TokenKind::Equal
            | TokenKind::NotEqual
            | TokenKind::LessThan
            | TokenKind::LessEqual
            | TokenKind::GreaterThan
            | TokenKind::GreaterEqual => true,
            _ => false,
        }
    }

    pub fn is_unary(&self) -> bool {
        match self {
            TokenKind::BitwiseNot | TokenKind::LogicalNot => true,
            _ => false,
        }
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
            ("%", TokenKind::Remainder),
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

    // TODO: parser bugs. i.e "*// /}", "/ /" line comment works here.
    pub fn tokenize(&mut self) -> Vec<Token> {
        let mut tokens = Vec::new();
        println!("pos 16: {}", self.input.get(16).unwrap());
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
        tokens
    }

    fn next_token(&mut self) -> Option<Token> {
        self.buffer = Vec::new();
        let mut buf_type: BufKind = BufKind::Illegal;

        loop {
            let next_char = match self.peek(0) {
                Some(char) => *char,
                None => break,
            };

            // TODO: requires (line + " \n"), e.g "//"
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

        let buf_str = self.buffer.iter().map(|x| *x as char).collect::<String>();
        if LOG_DEBUG_INFO {
            println!("\nbuf: '{buf_str}' | pos: {}", self.pos);
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
                        println!("reduce {} | new pos: {}", buf_str, self.pos);
                    }
                }
            }
        }
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
                "consuming '{}' | new pos {}",
                self.input.get(i).copied().unwrap() as char,
                self.pos
            );
        }
        self.input.get(i).copied().unwrap()
    }
}
