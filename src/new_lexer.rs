/******************************************
*** WIP ***********************************
******************************************/

const LOG_DEBUG_INFO: bool = true;

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
    OpenMultiComment,
    CloseMultiComment,
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
    Symbol,
    Illegal,
}

impl Token {
    pub fn debug_print(&self) -> String {
        let contents = match self {
            Token::Symbol(kind) => format!("{:?}", kind),
            Token::Operand(kind) => format!("{:?}", kind),
            Token::Keyword(kind) => format!("{:?}", kind),
            Token::Ident(val) | Token::IntLit(val) => format!("Val({})", val),
        };
        return format!("Token {{ {}", contents);
    }
}

bitflags::bitflags! {
    pub struct Flags: u8 {
        const LINE_COMMENT = 1 << 0;
        const MULTI_COMMENT = 1 << 1;
    }
}

pub struct Lexer {
    pos: usize,
    input: Vec<u8>,
    buffer: Vec<u8>,
    symbol_reg: [(&'static str, Token); 24],
    keyword_reg: [(&'static str, Token); 5],
    is_linecomment: bool,
    is_multicomment: bool,
    // flags: u8,
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
            ("/*", Token::Symbol(SymbolKind::OpenMultiComment)),
            ("*/", Token::Symbol(SymbolKind::CloseMultiComment)),
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
            is_linecomment: false,
            is_multicomment: false,
            // flags: 0,
        };
    }

    pub fn tokenize(&mut self) -> Vec<Token> {
        let mut tokens = Vec::new();
        while self.pos < self.input.len() {
            match self.next_token() {
                Some(tok) => match tok {
                    Token::Symbol(SymbolKind::LineComment) => self.is_linecomment = true,
                    Token::Symbol(SymbolKind::OpenMultiComment) => self.is_multicomment = true,
                    Token::Symbol(SymbolKind::CloseMultiComment) => self.is_multicomment = false,
                    _ if self.is_multicomment => (),
                    _ => tokens.push(tok),
                },
                None => continue,
            };
            if LOG_DEBUG_INFO {
                println!("new tok: {}", tokens.last().unwrap().debug_print())
            }
        }
        return tokens;
    }

    fn next_token(&mut self) -> Option<Token> {
        self.buffer = Vec::new();
        let mut buf_type: BufKind = BufKind::Illegal;

        loop {
            if LOG_DEBUG_INFO {
                print!("\n");
            }

            let next_char = match self.peek(0) {
                Some(char) => *char,
                None => break,
            };

            if next_char == b'\n' {
                self.is_linecomment = false;
                self.consume();
                break;
            }
            if self.is_linecomment {
                self.consume();
                break;
            } else if next_char.is_ascii_whitespace() {
                self.consume();
                break;
            }

            if LOG_DEBUG_INFO {
                println!("next_char is some, '{}'", next_char as char);
            }

            let char_type = match next_char {
                // b'a'..=b'z' | b'A'..=b'Z' | b'_' | b'0'..=b'9' if buf_type == BufKind::Word => {
                //     BufKind::Word
                // }
                b'a'..=b'z' | b'A'..=b'Z' | b'_' => BufKind::Word,
                b'0'..=b'9' => BufKind::IntLit,
                33..=47 | 58..=64 | 91..=96 | 123..=126 => BufKind::Symbol,
                _ => break,
            };

            if LOG_DEBUG_INFO {
                println!(" .. char is: {:?}", char_type);
            }

            if self.buffer.is_empty() {
                buf_type = char_type.clone();
                if LOG_DEBUG_INFO {
                    println!(" .. set buf_type: {:?}", buf_type);
                }
            }
            if char_type != buf_type {
                break;
            }

            let ch = self.consume();

            if LOG_DEBUG_INFO {
                println!(" .. adding char {}", ch as char);
            }

            self.buffer.push(ch);
        }

        let buf_str = self.buffer.iter().map(|x| *x as char).collect::<String>();

        if LOG_DEBUG_INFO {
            println!("buf: {}", buf_str);
        }

        // let mut symbol: Option<Token> = None;
        return match buf_type {
            BufKind::Illegal => None,
            BufKind::Word => self.match_word(buf_str),
            BufKind::Symbol => self.match_symbol(buf_str),
            BufKind::IntLit => Some(Token::IntLit(buf_str)),
            // BufKind::Symbols => {
            //     while !buf_str.is_empty() {
            //         match self
            //             .symbol_reg
            //             .iter()
            //             .rev()
            //             .find(|(str, _)| *str == buf_str)
            //         {
            //             Some((_, tok)) => {
            //                 symbol = Some(tok.clone());
            //                 break;
            //             }
            //             None => {
            //                 buf_str.pop();
            //                 if LOG_DEBUG_INFO {
            //                     println!("decrementing pos");
            //                 }
            //                 self.pos -= 1;
            //             }
            //         };
            //     }
            // }
        };

        // match symbol {
        //     Some(Token::Symbol(SymbolKind::LineComment)) => {
        //         self.is_linecomment = true;
        //         None
        //     }
        //     Some(Token::Symbol(SymbolKind::OpenMultiComment)) => {
        //         self.is_multicomment = true;
        //         symbol
        //     }
        //     Some(Token::Symbol(SymbolKind::CloseMultiComment)) => {
        //         self.is_multicomment = false;
        //         symbol
        //     }
        //     _ => {
        //         if self.is_multicomment {
        //             return None;
        //         }
        //         return symbol;
        //     }
        // }
    }

    fn match_word(&self, mut buf_str: String) -> Option<Token> {
        let og_buf = buf_str.clone();
        while !buf_str.is_empty() {
            match self
                .keyword_reg
                .iter()
                .rev()
                .find(|(str, _)| *str == buf_str)
            {
                Some((_, tok)) => return Some(tok.clone()),
                None => buf_str.pop(),
            };
        }
        return Some(Token::Ident(og_buf));
    }

    fn match_symbol(&self, mut buf_str: String) -> Option<Token> {
        while !buf_str.is_empty() {
            match self
                .symbol_reg
                .iter()
                .rev()
                .find(|(str, _)| *str == buf_str)
            {
                Some((_, tok)) => return Some(tok.clone()),
                None => {
                    buf_str.pop();
                }
            }
        }
        return None;
    }

    fn is_unwanted(&mut self, next_char: u8) -> bool {
        if self.is_linecomment || next_char.is_ascii_whitespace() {
            if next_char == b'\n' {
                self.is_linecomment = false;
            }
            self.consume();
            return true;
        }
        return false;
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

// match buf_type {
//     BufKind::Word => {
//         while !buf_str.is_empty() {
//             match self
//                 .keyword_reg
//                 .iter()
//                 .rev()
//                 .find(|(str, _)| *str == buf_str)
//             {
//                 Some((_, tok)) => return Some(tok.clone()),
//                 None => buf_str.pop(),
//             };
//         }
//         return Some(Token::Ident(imm_buf_str));
//     }
//     BufKind::Symbols => {
//         while !buf_str.is_empty() {
//             match self
//                 .symbol_reg
//                 .iter()
//                 .rev()
//                 .find(|(str, _)| *str == buf_str)
//             {
//                 Some((_, tok)) => return Some(tok.clone()),
//                 None => {
//                     buf_str.pop();
//                     self.pos -= 1;
//                 }
//             };
//         }
//         return None; // illegal.
//     }
//     BufKind::IntLit => return Some(Token::IntLit(imm_buf_str)),
//     BufKind::Illegal => return None,
// };
