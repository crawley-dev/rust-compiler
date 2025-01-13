use crate::{
    debug, err,
    utils::{self, get_pos, FILE_CONTENTS},
};
use anyhow::{Error, Result};
use bitflags::bitflags;
use core::fmt;
use std::{
    cmp::max,
    collections::{HashMap, VecDeque},
};

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
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
    Array,     // "[]"
    Ptr,       // "^"
    Eq,        // "="
    Add,       // "+"
    Sub,       // "-"
    Mul,       // "*"
    Quo,       // "/"
    Mod,       // "%"
    Ampersand, // "&" BitAnd, Address-of
    Bar,       // "|" BitOr
    Tilde,     // "~" BitXor
    AndNot,    // "&~"
    Shl,       // "<<"
    Shr,       // ">>"
    Arrow,     //  "->"

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
    Not,    // "!" BitNot and CmpNot
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
    Mut,
    Fn,
    Return,
    True,
    False,

    // Primitive Constructs
    Ident,
    IntLit,
}

#[derive(Debug)]
pub enum Associativity {
    Left,
    Right,
}

#[derive(Debug, Clone, Copy, PartialEq)]
enum BufKind {
    Word,
    IntLit,
    Symbol,
    Illegal,
    NewLine,
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Token {
    pub kind: TokenKind,
    // pub value: Option<String>,
    pub pos: (u32, u32),
    pub len: u32,
}

pub struct Lexer {
    idx: usize,
    input: Vec<u8>,
    reg: HashMap<&'static str, TokenKind>,
    is_linecomment: bool,
    is_multicomment: bool,

    pub tokens: VecDeque<Token>,
}

impl Lexer {
    pub fn new(input: &[&str]) -> Lexer {
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
            ("!", TokenKind::Not),
            ("^", TokenKind::Ptr),
            ("=", TokenKind::Eq),
            ("+", TokenKind::Add),
            ("-", TokenKind::Sub),
            ("*", TokenKind::Mul),
            ("/", TokenKind::Quo),
            ("%", TokenKind::Mod),
            ("&", TokenKind::Ampersand),
            ("|", TokenKind::Bar),
            ("~", TokenKind::Tilde),
            ("&~", TokenKind::AndNot),
            ("<<", TokenKind::Shl),
            (">>", TokenKind::Shr),
            ("->", TokenKind::Arrow),
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
            ("return", TokenKind::Return),
            ("if", TokenKind::If),
            ("else", TokenKind::Else),
            ("mut", TokenKind::Mut),
            ("while", TokenKind::While),
            ("break", TokenKind::Break),
            ("true", TokenKind::True),
            ("false", TokenKind::False),
        ]);
        Lexer {
            idx: 0,
            input: input
                .iter()
                .flat_map(|x| x.chars())
                .map(|x| x as u8)
                .collect(),
            reg,
            is_linecomment: false,
            is_multicomment: false,

            tokens: VecDeque::new(),
        }
    }

    pub fn tokenize(mut self) -> (Lexer, Option<Error>) {
        while self.idx < self.input.len() {
            match self.next_token() {
                Ok(Some(tok)) => match tok.kind {
                    TokenKind::LineComment => self.is_linecomment = true,
                    TokenKind::OpenMultiComment => self.is_multicomment = true,
                    TokenKind::CloseMultiComment => self.is_multicomment = false,
                    _ if self.is_multicomment => (),
                    _ => {
                        self.tokens.push_back(tok);
                        debug!("new tok: {:?}", self.tokens.back().as_ref().unwrap());
                    }
                },
                Ok(None) => continue,
                Err(e) => return (self, Some(e)),
            };
        }
        (self, None)
    }

    fn next_token(&mut self) -> Result<Option<Token>> {
        let mut buf = Vec::new();
        let mut buf_kind = BufKind::Illegal;

        while let Some(next_char) = self.peek(0) {
            // the order of these match statements matter!
            let char_type = match next_char {
                b'\n' => BufKind::NewLine,
                _ if self.is_linecomment || next_char.is_ascii_whitespace() => BufKind::Illegal, // collect together all the illegal stuff at once!
                b'0'..=b'9' | b'_' if buf_kind == BufKind::Word => BufKind::Word,
                // b'_' if buf_kind == BufKind::IntLit => {
                //     self.consume();
                //     continue;
                // } // skip number spacing, e.g 1_000_000 => 1000000
                b'0'..=b'9' => BufKind::IntLit,
                b'a'..=b'z' | b'A'..=b'Z' => BufKind::Word,
                b'!'..=b'/' | b':'..=b'@' | b'['..=b'`' | b'{'..=b'~' => BufKind::Symbol,
                _ => {
                    return err!("unknown char found {next_char}"); // result T can be anything.
                }
            };

            // buf_kind not set, set it.
            if buf.is_empty() {
                buf_kind = char_type;
            } else if char_type != buf_kind {
                break;
            }

            let ch = self.consume();
            buf.push(ch);
        }
        Ok(self.create_tok(buf_kind, &buf))
    }

    // TO FUTURE TOM: for future stuff, create a new bufkind and do stuff here.
    //  - trying to modify state in next_token causes bugs.
    //      .. because after creating a token, the next char may not be "next_char" due to a reduce
    //      .. !! watchout for repeats, e.g on newline buf: self.pos.1 += collected_newlines
    fn create_tok(&mut self, buf_kind: BufKind, buf: &[u8]) -> Option<Token> {
        if buf.is_empty() {
            self.idx += 1;
            utils::add_pos((1, 0));
            return None;
        }

        let buf_str = buf.iter().map(|x| *x as char).collect::<String>();
        let len = buf.len() as u32;
        debug!("buf: '{buf_str}', kind: {buf_kind:?} | pos: {}", self.idx); // TODO(TOM): formatting ruined on '\n' :/

        match buf_kind {
            BufKind::Illegal => None,
            BufKind::NewLine => {
                self.is_linecomment = false;
                utils::set_pos((0, utils::get_pos().1 + 1));
                None
            }
            BufKind::Word => self.match_word(&buf_str),
            BufKind::Symbol => self.match_symbol(&buf_str),
            BufKind::IntLit => Some(Token {
                kind: TokenKind::IntLit,
                pos: Self::get_pos_adjusted(len),
                len,
            }),
        }
    }

    fn match_word(&self, buf_str: &str) -> Option<Token> {
        let len = buf_str.len() as u32;
        match self.reg.get(buf_str) {
            Some(kind) => Some(Token {
                kind: *kind,
                pos: Self::get_pos_adjusted(len),
                len,
            }),
            None => Some(Token {
                kind: TokenKind::Ident,
                pos: Self::get_pos_adjusted(len),
                len,
            }),
        }
    }

    fn match_symbol(&mut self, buf_str: &str) -> Option<Token> {
        let mut buf_len = buf_str.len();
        let slice = &buf_str[..buf_len];
        while buf_len > 0 {
            match self.reg.get(slice) {
                Some(kind) => {
                    // early return if the symbol
                    return Some(Token {
                        kind: *kind,
                        pos: utils::get_pos(),
                        len: buf_len as u32,
                    });
                }
                None => {
                    buf_len -= 1;
                    self.idx -= 1;
                    utils::sub_pos((1, 0));
                    debug!("reduce {} | new pos: {}", buf_str, self.idx);
                }
            }
        }
        self.idx += 1;
        utils::add_pos((1, 0));
        None
    }

    fn peek(&self, offset: usize) -> Option<u8> {
        self.input.get(self.idx + offset).copied()
    }

    fn consume(&mut self) -> u8 {
        let i = self.idx;
        self.idx += 1;
        utils::add_pos((1, 0));

        let char = self.input.get(i).copied().unwrap();
        if char == b'\n' {
            debug!("consuming '{}'", r"\n");
        } else {
            debug!("consuming '{}'", char as char);
        }
        char
    }

    fn get_pos_adjusted(len: u32) -> (u32, u32) {
        let (x, y) = utils::get_pos();
        (x - len, y)
    }
}

impl Token {
    pub fn str(&self) -> &str {
        unsafe {
            match FILE_CONTENTS.get(self.pos.1 as usize) {
                Some(line) => {
                    utils::set_pos(self.pos);
                    &line[self.pos.0 as usize..(self.pos.0 + self.len) as usize]
                }
                None => panic!("Cannot get token str, invalid pos in: {self:#?}"),
            }
        }
    }
}
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
    pub fn get_flags(self) -> TokenFlags {
        match self {
            TokenKind::Ptr => TokenFlags::UNARY,                     // "^"
            TokenKind::Eq => TokenFlags::ASSIGN,                     // "="
            TokenKind::Add => TokenFlags::ARITH,                     // "+"
            TokenKind::Sub => TokenFlags::ARITH | TokenFlags::UNARY, // "-"
            TokenKind::Mul => TokenFlags::ARITH,                     // "*"
            TokenKind::Quo => TokenFlags::ARITH,                     // "/"
            TokenKind::Mod => TokenFlags::ARITH,                     // "%"
            TokenKind::Ampersand => TokenFlags::BIT | TokenFlags::UNARY, // "&"
            TokenKind::Bar => TokenFlags::BIT,                       // "|"
            TokenKind::Tilde => TokenFlags::BIT | TokenFlags::UNARY, // "~"
            TokenKind::AndNot => TokenFlags::BIT,                    // "&~"
            TokenKind::Shl => TokenFlags::BIT,                       // "<<"
            TokenKind::Shr => TokenFlags::BIT,                       // ">>"

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

            TokenKind::Not => TokenFlags::LOG | TokenFlags::BIT | TokenFlags::UNARY, // "!"
            TokenKind::CmpAnd => TokenFlags::CMP | TokenFlags::LOG,                  // "&&"
            TokenKind::CmpOr => TokenFlags::CMP | TokenFlags::LOG,                   // "||"
            TokenKind::CmpEq => TokenFlags::CMP,                                     // "=="
            TokenKind::NotEq => TokenFlags::CMP,                                     // "!="
            TokenKind::Lt => TokenFlags::CMP,                                        // "<"
            TokenKind::Gt => TokenFlags::CMP,                                        // ">"
            TokenKind::LtEq => TokenFlags::CMP,                                      // "<="
            TokenKind::GtEq => TokenFlags::CMP,                                      // ">="

            _ => TokenFlags::empty(),
        }
    }

    pub fn has_flags(&self, flags: TokenFlags) -> bool {
        self.get_flags().contains(flags)
    }

    pub fn has_some_flags(&self, flags: TokenFlags) -> bool {
        self.get_flags().intersects(flags)
    }

    // Precedence hierarchy: higher = done first
    // .. going based of c precedence hierarchy.. at: https://ee.hawaii.edu/~tep/EE160/Book/chap5/subsection2.1.4.1.html#:~:text=The%20precedence%20of%20binary%20logical,that%20of%20all%20binary%20operators.
    // .. c++ associativity: https://en.wikipedia.org/wiki/Operators_in_C_and_C%2B%2B#Operator_precedence
    pub fn get_prec_binary(&self) -> i32 {
        match self {
            TokenKind::Mul | TokenKind::Quo | TokenKind::Mod => 12,
            TokenKind::Sub | TokenKind::Add => 11,
            TokenKind::Shl | TokenKind::Shr => 10,
            TokenKind::Lt | TokenKind::LtEq | TokenKind::Gt | TokenKind::GtEq => 9,
            TokenKind::CmpEq | TokenKind::NotEq => 8,
            TokenKind::Ampersand => 7, // BitAnd
            TokenKind::Tilde => 6,     // BitXor
            TokenKind::Bar => 5,
            TokenKind::CmpAnd => 3,
            TokenKind::CmpOr => 2,
            _ if self.has_flags(TokenFlags::ASSIGN) => 1,
            TokenKind::Comma => 0,
            _ => -100,
        }
    }

    // Precedence hierarchy for unary operators, may be a variant of a multi-purpose operator
    // unary operators for now have a precedence of 13, may have some edge-cases.
    // .. e.g: "&":
    // .. .. Binary: BitAnd, prec: 7
    // .. .. Unary: Address-of, prec: 13
    pub fn get_prec_unary(&self) -> i32 {
        match self {
            _ if self.has_flags(TokenFlags::UNARY) => 13,
            _ => -100,
        }
    }

    pub fn assign_to_arithmetic(&self) -> Result<TokenKind> {
        match self {
            TokenKind::AddEq => Ok(TokenKind::Add),
            TokenKind::SubEq => Ok(TokenKind::Sub),
            TokenKind::MulEq => Ok(TokenKind::Mul),
            TokenKind::QuoEq => Ok(TokenKind::Quo),
            TokenKind::ModEq => Ok(TokenKind::Mod),
            TokenKind::AndEq => Ok(TokenKind::Ampersand),
            TokenKind::OrEq => Ok(TokenKind::Bar),
            TokenKind::XorEq => Ok(TokenKind::Tilde),
            TokenKind::AndNotEq => Ok(TokenKind::AndNot),
            TokenKind::ShlEq => Ok(TokenKind::Shl),
            TokenKind::ShrEq => Ok(TokenKind::Shr),
            _ => err!("{self:?} cannot be converted to arithmetic"),
        }
    }

    pub fn get_associativity(&self, is_unary: bool) -> Associativity {
        match self {
            _ if self.has_flags(TokenFlags::ASSIGN) => Associativity::Right,
            _ if is_unary => Associativity::Right,
            _ => Associativity::Left,
        }
    }
}

impl fmt::Debug for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if f.alternate() {
            writeln!(f, "Token {{")?;
            writeln!(f, "    kind: {:?}", self.kind)?;
            writeln!(f, "    pos: ({}, {})", self.pos.1 + 1, self.pos.0 + 1)?;
            writeln!(f, "    len: {}", self.len)?;
            write!(f, "}}")
        } else {
            f.debug_struct("Token")
                .field("kind", &self.kind)
                .field("pos", &self.pos)
                .field("len", &self.len)
                .finish()
        }
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.kind {
            TokenKind::Ident | TokenKind::IntLit => write!(f, "{:?}({})", self.kind, self.str()),
            _ => write!(f, "{:?}", self.kind),
        }
    }
}

impl fmt::Display for Lexer {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut val_max_len = 0;
        let mut x_max_len = 0;
        let mut y_max_len = 0;
        for tok in &self.tokens {
            let val_cur_len = format!("{tok}").len();
            val_max_len = max(val_max_len, val_cur_len);

            let (x, y) = tok.pos;
            x_max_len = max(x_max_len, format!("{x}").len());
            y_max_len = max(y_max_len, format!("{y}").len());
        }

        for tok in &self.tokens {
            let val_str = format!("{tok}");
            let val_whitespace = " ".repeat(val_max_len - val_str.len());
            let x_str = format!("{x:?}", x = tok.pos.0);
            let x_whitespace = " ".repeat(x_max_len - x_str.len());
            let y_str = format!("{y:?}", y = tok.pos.1);
            let y_whitespace = " ".repeat(y_max_len - y_str.len());
            write!(f,
                "Token {{ {val_str}{val_whitespace} | (col: {y_whitespace}{y_str}, row: {x_whitespace}{x_str}) }}\n"
            )?
        }
        Ok(())
    }
}
