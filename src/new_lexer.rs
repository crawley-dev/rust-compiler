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
    OpenSquirly,
    CloseSquirly,
    OpenParen,
    CloseParen,
    StmtEnd,
    Comma,
    Assign,
}

#[derive(Debug, Clone, PartialEq)]
pub enum KeywordKind {
    KeywordExit,
    KeywordLet,
    KeywordIf,
    KeywordElse,
    KeywordFunction,
}

#[derive(Clone, PartialEq)]
pub enum Token {
    Symbol(SymbolKind),
    Operand(OperandKind),
    Keyword(KeywordKind),
    Ident(String),
    IntLit(String),
}

impl fmt::Debug for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let contents = match self {
            Token::Symbol(kind) => format!("kind: {:?}", kind),
            Token::Operand(kind) => format!("kind: {:?}", kind),
            Token::Keyword(kind) => format!("kind: {:?}", kind),
            Token::Ident(val) | Token::IntLit(val) => format!("value: {}", val),
        };
        write!(f, "Token {{ {} }}", contents)
    }
}

pub struct Lexer {
    pos: usize,
    input: Vec<u8>,
}

impl Lexer {
    fn new(input: String) -> Lexer {
        let mut lexer = Lexer {
            pos: 0,
            input: input.into_bytes(),
        };
        return lexer;
    }

    // can this func error ?
    pub fn tokenize(&mut self) -> Vec<Token> {
        let mut tokens = Vec::new();
        while self.pos < self.input.len() {
            // self.skip_comments();
            // self.skip_whitespace();

            match self.next_token() {
                Some(tok) => tokens.push(tok),
                None => continue,
            };
        }
        return tokens;
    }

    fn next_token(&mut self) -> Option<Token> {}

    fn read_char(&mut self) -> Option<&u8> {
        self.pos += 1;
        return self.input.get(self.pos);
    }
}
