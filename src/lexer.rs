use std::{
    collections::{HashMap, HashSet},
    fmt,
};

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

    LessThan,
    GreaterThan,
    Equal,
    NotEqual,
    Not,

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

pub struct Lexer {
    position: usize,
    read_position: usize,
    ch: u8,
    input: Vec<u8>,
    keywords_set: HashMap<&'static str, TokenKind>,
}

impl Lexer {
    pub fn new(input: String) -> Lexer {
        let mut lex = Lexer {
            position: 0,
            read_position: 0,
            ch: 0,
            input: input.into_bytes(),
            keywords_set: HashMap::from([
                ("exit", TokenKind::KeywordExit),
                ("let", TokenKind::KeywordLet),
                ("fn", TokenKind::KeywordFunction),
                ("if", TokenKind::KeywordIf),
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
        tokens.pop(); // TEMP: removes Eof token
        return tokens;
    }

    fn next_token(&mut self) -> Token {
        if self.ch.is_ascii_whitespace() {
            self.skip_whitespace();
        }
        let kind = match self.ch {
            b'a'..=b'z' | b'A'..=b'Z' | b'_' => {
                let ident = self.read_identifier();
                if self.keywords_set.contains_key(ident.as_str()) {
                    return Token {
                        kind: self.keywords_set.get(ident.as_str()).unwrap().clone(),
                        value: None,
                    };
                }
                return Token {
                    kind: TokenKind::Ident,
                    value: Some(ident),
                };
            }
            b'0'..=b'9' => {
                return Token {
                    kind: TokenKind::IntLit,
                    value: Some(self.read_int_literal()),
                };
            }
            b'{' => TokenKind::OpenSquirly,
            b'}' => TokenKind::CloseSquirly,
            b'(' => TokenKind::OpenParen,
            b')' => TokenKind::CloseParen,
            b';' => TokenKind::SemiColon,
            b',' => TokenKind::Comma,
            b'*' => TokenKind::Multiply,
            b'/' => TokenKind::Divide,
            b'+' => TokenKind::Add,
            b'-' => TokenKind::Subtract,
            b'=' => TokenKind::Assign,
            0 => TokenKind::Eof,
            _ => TokenKind::Illegal,
        };
        self.read_char();
        return Token { kind, value: None };
    }

    fn read_char(&mut self) {
        if self.read_position >= self.input.len() {
            self.ch = 0;
        } else {
            self.ch = self.input[self.read_position];
        }
        self.position = self.read_position;
        self.read_position += 1;
    }

    fn read_identifier(&mut self) -> String {
        let pos = self.position;
        while self.ch.is_ascii_alphanumeric() || self.ch == b'_' {
            self.read_char();
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
    let consume = |str: &Vec<char>, i: &mut usize| -> char {
        let j = i.clone();
        *i += 1;
        println!("consuming {}, iter: {}", *str.get(j).unwrap(), j);
        return *str.get(j).unwrap();
    };

    while str.get(i).is_some() {
        let mut c = str.get(i).unwrap();
        let mut buf: String = String::new();
        match c {
            '{' => {
                tokens.push(Token {
                    kind: TokenKind::OpenBrace,
                    value: None,
                });
                consume(&str, &mut i);
            }
            '}' => {
                tokens.push(Token {
                    kind: TokenKind::CloseBrace,
                    value: None,
                });
                consume(&str, &mut i);
            }
            '(' => {
                tokens.push(Token {
                    kind: TokenKind::OpenParen,
                    value: None,
                });
                consume(&str, &mut i);
            }
            ')' => {
                tokens.push(Token {
                    kind: TokenKind::CloseParen,
                    value: None,
                });
                consume(&str, &mut i);
            }
            ';' => {
                tokens.push(Token {
                    kind: TokenKind::SemiColon,
                    value: None,
                });
                consume(&str, &mut i);
            }
            _ if c.is_digit(10) => {
                buf.push(consume(&str, &mut i));
                while str.get(i).is_some() && str.get(i).unwrap().is_digit(10) {
                    buf.push(consume(&str, &mut i));
                }
                tokens.push(Token {
                    kind: TokenKind::IntLit,
                    value: Some(buf),
                });
            }
            _ if c.is_alphanumeric() => {
                buf.push(consume(&str, &mut i));
                while str.get(i).is_some() && str.get(i).unwrap().is_alphanumeric() {
                    buf.push(consume(&str, &mut i));
                }
                if keywords.contains_key(&buf[..]) {
                    tokens.push(Token {
                        kind: (*keywords.get(&buf[..]).unwrap()).clone(),
                        value: None,
                    })
                } else {
                    tokens.push(Token {
                        kind: TokenKind::Ident,
                        value: Some(buf),
                    })
                }
            }
            _ if c.is_whitespace() => {
                consume(&str, &mut i);
            }
            _ => {
                panic!("you messed up! {}", c);
            }
        }
    }
    return tokens;
}
 */
