use crate::{
    compile_chain::{CompileChain, Globals},
    debug, err,
    utils::{pos, CompilerResult, Contents, Logger, Pos},
};
use anyhow::{Error, Result};
use bitflags::bitflags;
use std::collections::{HashMap, VecDeque};

// region: Type Definitions

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
    OpenBracket,       // "["
    CloseBracket,      // "]"
    OpenMultiComment,  // "/*"
    CloseMultiComment, // "*/"

    // Operators
    Dot,       // "." Struct Member Access
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
    LogAnd, // "&&"
    LogOr,  // "||"
    CmpEq,  // "=="
    Not,    // "!" BitNot and CmpNot
    NotEq,  // "!="
    Lt,     // "<"
    Gt,     // ">"
    LtEq,   // "<="
    GtEq,   // ">="

    // Keywords
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
    Type,
    Struct,

    // Primitive Constructs
    Ident,  // e.g. "my_variable"
    IntLit, // e.g. "1234"
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

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Token {
    pub kind: TokenKind,
    pub start: Pos, // TODO(TOM): make this just an index - drop y val
    pub len: u32,
    pub compile_chain_id: u8,
}

pub struct Lexer<'a> {
    pub tokens: VecDeque<Token>,
    reg: HashMap<&'static str, TokenKind>,
    input: Vec<u8>,
    idx: usize,
    is_linecomment: bool,
    is_multicomment: bool,

    logger: &'a mut Logger,
    compile_chain_id: u8,
}

// endregion
impl<'a> Lexer<'a> {
    pub fn new(input: &Contents, logger: &'a mut Logger, compile_chain_id: u8) -> Lexer<'a> {
        let reg: HashMap<&'static str, TokenKind> = HashMap::from([
            // Generic Symbols
            (",", TokenKind::Comma),
            (":", TokenKind::Colon),
            (";", TokenKind::SemiColon),
            ("(", TokenKind::OpenParen),
            (")", TokenKind::CloseParen),
            ("{", TokenKind::OpenBrace),
            ("}", TokenKind::CloseBrace),
            ("[", TokenKind::OpenBracket),
            ("]", TokenKind::CloseBracket),
            ("//", TokenKind::LineComment),
            ("/*", TokenKind::OpenMultiComment),
            ("*/", TokenKind::CloseMultiComment),
            // Operators
            ("!", TokenKind::Not),
            ("^", TokenKind::Ptr),
            (".", TokenKind::Dot),
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
            ("&&", TokenKind::LogAnd),
            ("||", TokenKind::LogOr),
            ("==", TokenKind::CmpEq),
            ("!=", TokenKind::NotEq),
            ("<", TokenKind::Lt),
            (">", TokenKind::Gt),
            ("<=", TokenKind::LtEq),
            (">=", TokenKind::GtEq),
            // Keywords
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
            ("type", TokenKind::Type),
            ("struct", TokenKind::Struct),
        ]);
        Lexer {
            tokens: VecDeque::new(),
            reg,
            input: input.src.join("").as_bytes().to_vec(),
            idx: 0,
            is_linecomment: false,
            is_multicomment: false,
            logger,
            compile_chain_id,
        }
    }

    pub fn tokenise(mut self) -> CompilerResult<VecDeque<Token>, Error> {
        while self.idx < self.input.len() {
            match self.next_token() {
                Ok(Some(tok)) => match tok.kind {
                    TokenKind::LineComment => self.is_linecomment = true,
                    TokenKind::OpenMultiComment => self.is_multicomment = true,
                    TokenKind::CloseMultiComment => self.is_multicomment = false,
                    _ if self.is_multicomment => (),
                    _ => {
                        self.tokens.push_back(tok);
                        debug!(self, "new tok: {:?}", self.tokens.back().as_ref().unwrap());
                    }
                },
                Ok(None) => continue,
                Err(e) => {
                    return CompilerResult::Err {
                        data: Some(self.tokens),
                        error: e,
                    }
                }
            };
        }
        CompilerResult::Ok(self.tokens)
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
                    return err!(self, "unknown char found {next_char}"); // result T can be anything.
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
            self.logger.add_pos(pos(1, 0));
            return None;
        }

        let buf_str = buf.iter().map(|x| *x as char).collect::<String>();
        let len = buf.len() as u32;

        if self.logger.print_logs {
            let mut print_buf = buf_str.clone();
            let mut offset = 0;
            for (i, c) in buf_str.chars().enumerate() {
                if c == '\n' {
                    print_buf.remove(i + offset);
                    print_buf.insert_str(i + offset, r"\n");
                    offset += 1;
                }
            }
            debug!(
                self,
                "buf: '{print_buf}', kind: {buf_kind:?} | buf_len: {len}"
            );
        }

        match buf_kind {
            BufKind::Illegal => None,
            BufKind::NewLine => {
                self.is_linecomment = false;
                self.logger
                    .set_pos(pos(0, self.logger.get_pos().y + buf_str.len() as u32));
                None
            }
            BufKind::Word => self.match_word(&buf_str),
            BufKind::Symbol => self.match_symbol(&buf_str),
            BufKind::IntLit => Some(Token {
                kind: TokenKind::IntLit,
                start: self.get_start(len),
                len,
                compile_chain_id: self.compile_chain_id,
            }),
        }
    }

    fn match_word(&self, buf_str: &str) -> Option<Token> {
        let len = buf_str.len() as u32;
        debug!(self, "matching word: '{}' .. len: {len}", buf_str);
        match self.reg.get(buf_str) {
            Some(kind) => Some(Token {
                kind: *kind,
                start: self.get_start(len),
                len,
                compile_chain_id: self.compile_chain_id,
            }),
            None => Some(Token {
                kind: TokenKind::Ident,
                start: self.get_start(len),
                len,
                compile_chain_id: self.compile_chain_id,
            }),
        }
    }

    fn match_symbol(&mut self, buf_str: &str) -> Option<Token> {
        let mut buf_len = buf_str.len();
        while buf_len > 0 {
            let slice = &buf_str[..buf_len];
            match self.reg.get(slice) {
                Some(kind) => {
                    // early return if the symbol
                    return Some(Token {
                        kind: *kind,
                        start: self.get_start(buf_len as u32),
                        len: buf_len as u32,
                        compile_chain_id: self.compile_chain_id,
                    });
                }
                None => {
                    buf_len -= 1;
                    self.idx -= 1;
                    self.logger.sub_pos(pos(1, 0));
                    debug!(
                        self,
                        "reduce '{}' | new pos: {}",
                        &buf_str[..buf_len],
                        self.idx
                    );
                }
            }
        }
        self.idx += 1;
        self.logger.add_pos(pos(1, 0));
        debug!(self, "exiting symbol match, no match found");
        None
    }

    fn peek(&self, offset: usize) -> Option<u8> {
        self.input.get(self.idx + offset).copied()
    }

    fn consume(&mut self) -> u8 {
        let char = self.input.get(self.idx).copied().unwrap();
        if char == b'\n' {
            debug!(self, "consuming '{}'", r"\n");
        } else {
            debug!(self, "consuming '{}'", char as char);
        }

        self.idx += 1;
        self.logger.add_pos(pos(1, 0));

        char
    }

    fn get_start(&self, len: u32) -> Pos {
        let p = self.logger.get_pos();
        pos(p.x - len, p.y)
    }
}

impl Token {
    pub fn str<'a>(&self) -> &'a str {
        Globals::get(self.compile_chain_id as usize)
            .contents
            .get_src_oneline(self.start, self.end_pos())
    }

    pub fn end_pos(&self) -> Pos {
        pos(self.start.x + self.len, self.start.y)
    }
}

pub fn token(kind: TokenKind, start: Pos, len: u32, compile_chain_id: u8) -> Token {
    Token {
        kind,
        start,
        len,
        compile_chain_id,
    }
}

bitflags! {
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub struct TokenFlags: u8 {
        const ASSIGN = 1 << 0; // this flag indicates whether its an assignment operator
        const ARITH = 1 << 1; // this flag indicates whether its an arithmetic operator
        const CMP = 1 << 2; // this flag indicates whether its a comparison operator
        const LOG = 1 << 3; // this flag indicates whether its a logical operator
        const BIT = 1 << 4; // this flag indicates whether its a bitwise operator
        const MEM = 1 << 5; // this flag indicates whether its a memory operator (e.g. dereference)
        const LHS = 1 << 6; // this flag indicates whether the operator is on the left or right of an expression.
        const BASE = 1 << 7; // this flag indicates that it has flags, but doesn't actually mean anything on its own.s
    }
}

impl TokenKind {
    /// Returns the flags for this token when it is used in a unary context.
    /// - Tokens that are not valid in a unary context return empty.
    pub fn get_flags_unary(self) -> TokenFlags {
        match self {
            // Only valid in a unary position.
            TokenKind::Sub => TokenFlags::ARITH | TokenFlags::LHS,
            TokenKind::Not => TokenFlags::LOG | TokenFlags::LHS,
            TokenKind::Tilde => TokenFlags::BIT | TokenFlags::LHS,
            TokenKind::Ampersand => TokenFlags::MEM | TokenFlags::LHS,
            TokenKind::Ptr => TokenFlags::MEM, // needed to be able to give this a flag.
            _ => TokenFlags::empty(),
        }
    }

    /// Returns the flags for this token when it is used in a binary context.
    ///
    /// For binary operators we not only tag the operator’s intrinsic role but also
    /// include the `LHS` flag to indicate that it binds with a left-hand side operand.
    pub fn get_flags_binary(self) -> TokenFlags {
        use TokenFlags as Flag;
        use TokenKind as Kind;
        match self {
            // Arithmetic binary operators.
            Kind::Add | Kind::Sub | Kind::Mul | Kind::Quo | Kind::Mod => Flag::ARITH,
            // Comparison binary operators.
            Kind::CmpEq | Kind::NotEq | Kind::Lt | Kind::Gt | Kind::LtEq | Kind::GtEq => Flag::CMP,
            // Logical binary operators.
            Kind::LogAnd | Kind::LogOr => Flag::LOG,
            // Bitwise binary operators.
            Kind::Bar | Kind::Shl | Kind::Shr | Kind::AndNot | Kind::Ampersand => Flag::BIT,

            // Assignment operators.
            Kind::Eq => Flag::ASSIGN,
            // Arithmetic Compound assignment operators.
            Kind::AddEq | Kind::SubEq | Kind::MulEq | Kind::QuoEq | Kind::ModEq => {
                Flag::ASSIGN | Flag::ARITH
            }
            // Bitwise compound assignment operators.
            Kind::AndEq | Kind::OrEq | Kind::XorEq | Kind::ShlEq | Kind::ShrEq | Kind::AndNotEq => {
                Flag::ASSIGN | Flag::BIT
            }

            // Access
            Kind::Dot => Flag::MEM,

            // In this design, we treat commas (or similar separators) as nonoperators.
            Kind::Comma => Flag::BASE,
            _ => Flag::empty(),
        }
    }

    /// Returns the binary operator precedence.
    /// Higher precedence is evaluated first.
    pub fn get_prec_binary(&self) -> i32 {
        match self {
            _ if self.get_flags_binary().contains(TokenFlags::ASSIGN) => 1,
            TokenKind::Dot => 14,
            TokenKind::Mul | TokenKind::Quo | TokenKind::Mod => 12,
            TokenKind::Add | TokenKind::Sub => 11,
            TokenKind::Shl | TokenKind::Shr => 10,
            TokenKind::Lt | TokenKind::LtEq | TokenKind::Gt | TokenKind::GtEq => 9,
            TokenKind::CmpEq | TokenKind::NotEq => 8,
            TokenKind::Ampersand => 7,
            TokenKind::Bar => 5,
            TokenKind::LogAnd => 3,
            TokenKind::LogOr => 2,
            TokenKind::Comma => -100,
            _ => -100,
        }
    }

    /// Returns the unary operator precedence.
    ///
    /// In our language, valid unary operators have a fixed precedence of 13;
    /// tokens that aren’t valid in a unary context yield a low precedence.
    pub fn get_prec_unary(&self) -> i32 {
        match self {
            _ if !self.get_flags_unary().is_empty() => 13,
            _ => -100,
        }
    }

    /// For compound assignment operators (like `+=`), returns the underlying arithmetic operator.
    ///
    /// For example, `AddEq` converts to `Add`.
    pub fn assign_to_arithmetic(&self) -> Option<TokenKind> {
        match self {
            TokenKind::AddEq => Some(TokenKind::Add),
            TokenKind::SubEq => Some(TokenKind::Sub),
            TokenKind::MulEq => Some(TokenKind::Mul),
            TokenKind::QuoEq => Some(TokenKind::Quo),
            TokenKind::ModEq => Some(TokenKind::Mod),
            TokenKind::AndEq => Some(TokenKind::Ampersand),
            TokenKind::OrEq => Some(TokenKind::Bar),
            TokenKind::XorEq => Some(TokenKind::Tilde),
            TokenKind::AndNotEq => Some(TokenKind::AndNot),
            TokenKind::ShlEq => Some(TokenKind::Shl),
            TokenKind::ShrEq => Some(TokenKind::Shr),
            _ => None,
        }
    }

    /// Returns the associativity of an operator.
    ///
    /// In many languages assignment and unary operators are right associative,
    /// whereas most binary operators are left associative.
    ///
    /// The caller passes `true` for unary operators.
    pub fn get_associativity(&self, is_unary: bool) -> Associativity {
        if self.get_flags_binary().contains(TokenFlags::ASSIGN) || is_unary {
            Associativity::Right
        } else {
            Associativity::Left
        }
    }

    pub fn has_flags_binary(&self, flags: TokenFlags) -> bool {
        self.get_flags_binary().contains(flags)
    }

    pub fn has_flags_unary(&self, flags: TokenFlags) -> bool {
        self.get_flags_unary().contains(flags)
    }
}
