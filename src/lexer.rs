use std::{collections::HashMap, fmt};

#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    Illegal,
    OpenSquirly,
    CloseSquirly,
    OpenParen,
    CloseParen,
    SemiColon,
    Comma,
    Eof,    // End Of File
    Assign, // e.g =
    Ident,  // e.g: dwa  | a variable name
    IntLit, // e.g: 55123

    // Operators
    Multiply,
    Divide,
    Add,
    Subtract,

    Not,
    Equal,
    NotEqual,
    LessThan,
    LessEqual,
    GreaterThan,
    GreaterEqual,

    // keywords
    KeywordExit,
    KeywordLet,
    KeywordFunction,
    KeywordIf,
    // KeywordReturn,
    // KeywordBreak,
    // KeywordCase,
    // KeywordConst,
    // KeywordContinue,
    // KeywordDefault,
    // KeywordDo,
    // KeywordElse,
    // KeywordEnum,
    // KeywordFor,
    // KeywordSwitch,
    // KeywordVoid,
    // KeywordWhile,
    // KeywordInt,
}

#[derive(Clone, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub value: Option<String>,
}

impl fmt::Debug for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "Token {{ kind: {:?}, value: {:?} }}",
            self.kind, self.value
        )
    }
}

impl TokenKind {
    pub fn width(&self) -> usize {
        match self {
            TokenKind::Illegal => 7,
            TokenKind::OpenSquirly => 11,
            TokenKind::CloseSquirly => 12,
            TokenKind::OpenParen => 9,
            TokenKind::CloseParen => 10,
            TokenKind::SemiColon => 9,
            TokenKind::Comma => 5,
            TokenKind::Eof => 3,
            TokenKind::Assign => 6,
            TokenKind::Ident => 5,
            TokenKind::IntLit => 6,
            TokenKind::Multiply => 8,
            TokenKind::Divide => 6,
            TokenKind::Add => 3,
            TokenKind::Subtract => 8,
            TokenKind::Not => 3,
            TokenKind::Equal => 5,
            TokenKind::NotEqual => 8,
            TokenKind::LessThan => 8,
            TokenKind::LessEqual => 9,
            TokenKind::GreaterThan => 11,
            TokenKind::GreaterEqual => 12,
            TokenKind::KeywordExit => 11,
            TokenKind::KeywordLet => 10,
            TokenKind::KeywordFunction => 15,
            TokenKind::KeywordIf => 9,
        }
    }
}

impl Token {
    pub fn debug_print(&self, longest_tok: usize, longest_ident: usize) {
        let padding = longest_tok - self.kind.width();
        let binding = " ".repeat(longest_ident - format!("{:?}", self.value).len());

        println!(
            "Token {{ kind: {:?}{:width$}, value: {:?}{value_padding} }}",
            self.kind,
            "",
            self.value,
            width = padding,
            value_padding = binding,
        );
    }
}

pub struct Lexer {
    position: usize,
    read_position: usize,
    ch: u8,
    input: Vec<u8>,
    keywords_hash: HashMap<&'static str, TokenKind>,
    symbols_hash: HashMap<&'static str, TokenKind>,
}

impl Lexer {
    pub fn new(input: String) -> Lexer {
        let mut lex = Lexer {
            position: 0,
            read_position: 0,
            ch: 0,
            input: input.into_bytes(),
            keywords_hash: HashMap::from([
                ("exit", TokenKind::KeywordExit),
                ("let", TokenKind::KeywordLet),
                ("fn", TokenKind::KeywordFunction),
                ("if", TokenKind::KeywordIf),
            ]),
            symbols_hash: HashMap::from([
                ("==", TokenKind::Equal),
                ("!=", TokenKind::NotEqual),
                (">=", TokenKind::GreaterEqual),
                ("<=", TokenKind::LessEqual),
                ("!", TokenKind::Not),
                (">", TokenKind::GreaterThan),
                ("<", TokenKind::LessThan),
                ("/", TokenKind::Divide),
                ("*", TokenKind::Multiply),
                ("+", TokenKind::Add),
                ("-", TokenKind::Subtract),
                ("=", TokenKind::Assign),
                ("{", TokenKind::OpenSquirly),
                ("}", TokenKind::CloseSquirly),
                ("(", TokenKind::OpenParen),
                (")", TokenKind::CloseParen),
                (";", TokenKind::SemiColon),
                (",", TokenKind::Comma),
            ]),
        };
        lex.read_char();
        return lex;
    }

    pub fn tokenize(&mut self) -> Vec<Token> {
        let mut tokens: Vec<Token> = Vec::new();
        while tokens.is_empty() || tokens.last().unwrap().kind != TokenKind::Eof {
            tokens.push(self.next_token());
        }
        tokens.pop(); // Removes Eof token
        return tokens;
    }

    fn next_token(&mut self) -> Token {
        if self.ch.is_ascii_whitespace() {
            self.skip_whitespace();
        }

        return match self.ch {
            b'a'..=b'z' | b'A'..=b'Z' | b'_' => {
                let ident = self.read_identifier();
                if self.keywords_hash.contains_key(ident.as_str()) {
                    return Token {
                        kind: self.keywords_hash.get(ident.as_str()).unwrap().clone(),
                        value: None,
                    };
                }

                Token {
                    kind: TokenKind::Ident,
                    value: Some(ident),
                }
            }
            33..=47 | 58..=64 | 91..=96 | 123..=126 => {
                let symbols = self.read_symbols();
                if self.symbols_hash.contains_key(symbols.as_str()) {
                    return Token {
                        kind: self.symbols_hash.get(symbols.as_str()).unwrap().clone(),
                        value: None,
                    };
                }

                Token {
                    kind: TokenKind::Illegal,
                    value: None,
                }
            }
            b'0'..=b'9' => Token {
                kind: TokenKind::IntLit,
                value: Some(self.read_int_literal()),
            },
            0 => Token {
                kind: TokenKind::Eof,
                value: None,
            },
            _ => Token {
                kind: TokenKind::Illegal,
                value: None,
            },
        };
    }

    fn read_identifier(&mut self) -> String {
        let pos = self.position;
        while self.ch.is_ascii_alphanumeric() || self.ch == b'_' {
            self.read_char();
        }
        return unsafe { String::from_utf8_unchecked((&self.input[pos..self.position]).to_vec()) };
    }

    fn read_symbols(&mut self) -> String {
        let pos = self.position;
        loop {
            match self.ch {
                b'(' | b')' | b'{' | b'}' | b';' | b',' => {
                    // these can't be multi symbol
                    self.read_char();
                    break;
                }
                33..=47 | 58..=64 | 91..=96 | 123..=126 => self.read_char(),
                _ => break,
            }
        }
        return unsafe { String::from_utf8_unchecked((&self.input[pos..self.position]).to_vec()) };
    }

    fn read_int_literal(&mut self) -> String {
        let pos = self.position;
        while self.ch.is_ascii_digit() {
            self.read_char();
        }
        return unsafe { String::from_utf8_unchecked((&self.input[pos..self.position]).to_vec()) };
    }

    fn skip_whitespace(&mut self) {
        while self.ch.is_ascii_whitespace() {
            self.read_char();
        }
    }

    fn read_char(&mut self) {
        if self.read_position >= self.input.len() {
            self.ch = 0;
        } else {
            // println!("char {} | {}", self.ch as char, self.ch);
            self.ch = self.input[self.read_position];
        }
        self.position = self.read_position;
        self.read_position += 1;
    }
}

/*
// DONT REMOVE THE HASHMAP!!
    let keywords: HashMap<&str, TokenKind> = HashMap::from([
        ("if", TokenKind::KeywordIf),
        ("do", TokenKind::KeywordDo),
        ("for", TokenKind::KeywordFor),
        ("case", TokenKind::KeywordCase),
        ("else", TokenKind::KeywordElse),
        ("enum", TokenKind::KeywordEnum),
        ("void", TokenKind::KeywordVoid),
        ("const", TokenKind::KeywordConst),
        ("while", TokenKind::KeywordWhile),
        ("break", TokenKind::KeywordBreak),
        ("switch", TokenKind::KeywordSwitch),
        ("return", TokenKind::KeywordReturn),
        ("default", TokenKind::KeywordDefault),
        ("continue", TokenKind::KeywordContinue),
        // Integers
        ("i8", TokenKind::KeywordI8),
        ("i16", TokenKind::KeywordI16),
        ("i32", TokenKind::KeywordI32),
        ("i64", TokenKind::KeywordI64),
        ("u8", TokenKind::KeywordU8),
        ("u16", TokenKind::KeywordU16),
        ("u32", TokenKind::KeywordU32),
        ("u64", TokenKind::KeywordU64),
        // Floats
        ("f32", TokenKind::KeywordF32),
        ("f64", TokenKind::KeywordF64),
    ]);
*/
