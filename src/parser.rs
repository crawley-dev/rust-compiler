use crate::lexer::*;

#[derive(Debug)]
pub enum StmtKind {
    Exit,
    Let,
}

#[derive(Debug)]
pub enum ExprKind {
    Term,
    BinExpr,
}

#[derive(Debug)]
pub enum TermKind {
    IntLit,
    Ident,
    Paren,
}

#[derive(Debug)]
pub enum BinExprKind {
    Divide,
    Multiply,
    Subtract,
    Add,
}

#[derive(Debug)]
pub struct NodeStmt {
    pub kind: StmtKind,
    pub ident: Option<Token>,
    pub expr: Option<NodeExpr>,
}

#[derive(Debug)]
pub struct NodeExpr {
    pub kind: ExprKind,
    pub term: Option<Box<NodeTerm>>,
    pub bin_expr: Option<Box<NodeBinExpr>>,
}

#[derive(Debug)]
pub struct NodeTerm {
    pub kind: TermKind,
    pub token: Option<Token>,
    pub expr: Option<NodeExpr>,
}

#[derive(Debug)]
pub struct NodeBinExpr {
    pub kind: BinExprKind,
    pub lhs: NodeExpr,
    pub rhs: NodeExpr,
}

#[derive(Debug)]
pub struct NodeProg {
    pub stmts: Vec<NodeStmt>,
}

pub struct Parser {
    tokens: Vec<Token>,
    position: usize,
}

impl Parser {
    pub fn new(input: Vec<Token>) -> Parser {
        let parser = Parser {
            tokens: input,
            position: 0,
        };
        return parser;
    }

    pub fn parse_prog(&mut self) -> Result<NodeProg, &'static str> {
        let mut prog = NodeProg { stmts: vec![] };
        while self.peek(0).is_some() {
            prog.stmts.push(self.parse_stmt()?);
        }

        return Ok(prog);
    }

    // TODO: don't propogate err up (?), use .is_ok()
    fn parse_stmt(&mut self) -> Result<NodeStmt, &'static str> {
        let cur_tok = &self.tokens[self.position];

        let stmt = match cur_tok.kind {
            TokenKind::KeywordExit => {
                self.try_consume(TokenKind::KeywordExit)?;
                self.try_consume(TokenKind::OpenParen)?;
                let stmt = NodeStmt {
                    kind: StmtKind::Exit,
                    ident: None,
                    expr: Some(self.parse_expr(0)?),
                };
                self.try_consume(TokenKind::CloseParen)?;
                stmt
            }
            TokenKind::KeywordLet => {
                self.try_consume(TokenKind::KeywordLet)?;
                let ident = self.try_consume(TokenKind::Ident)?;
                self.try_consume(TokenKind::Assign)?;
                NodeStmt {
                    kind: StmtKind::Let,
                    ident: Some(ident),
                    expr: Some(self.parse_expr(0)?),
                }
            }
            _ => return Err("Unable to parse expression"),
        };

        self.try_consume(TokenKind::SemiColon)?;
        return Ok(stmt);
    }

    fn parse_expr(&mut self, min_prec: i32) -> Result<NodeExpr, &'static str> {
        let term = self.parse_term()?;
        if self.peek(0).is_none() {
            return Err("No expression to parse");
        }

        let mut lhs = NodeExpr {
            kind: ExprKind::Term,
            term: Some(Box::new(term)),
            bin_expr: None,
        };

        loop {
            let (kind, prec): (BinExprKind, i32) = match self.peek(0).unwrap().kind {
                TokenKind::Divide => (BinExprKind::Divide, 1),
                TokenKind::Multiply => (BinExprKind::Multiply, 1),
                TokenKind::Subtract => (BinExprKind::Subtract, 0),
                TokenKind::Add => (BinExprKind::Add, 0),
                _ => break,
            };
            if prec < min_prec {
                break;
            }
            let next_prec = prec + 1;
            self.consume(); // consume operand, checked in match so don't check again
            let rhs = self.parse_expr(next_prec)?;

            lhs = NodeExpr {
                kind: ExprKind::BinExpr,
                term: None,
                bin_expr: Some(Box::new(NodeBinExpr { kind, lhs, rhs })),
            };
        }

        return Ok(lhs);
    }

    fn parse_term(&mut self) -> Result<NodeTerm, &'static str> {
        if self.peek(0).is_none() {
            return Err("No term to parse.");
        }
        // println!("Parsing term: {:?}", self.peek(0).unwrap());
        return match self.peek(0).unwrap().kind {
            TokenKind::IntLit => Ok(NodeTerm {
                kind: TermKind::IntLit,
                token: Some(self.consume()),
                expr: None,
            }),
            TokenKind::Ident => Ok(NodeTerm {
                kind: TermKind::Ident,
                token: Some(self.consume()),
                expr: None,
            }),
            TokenKind::OpenParen => {
                let term = NodeTerm {
                    kind: TermKind::Paren,
                    token: Some(self.consume()),
                    expr: Some(self.parse_expr(0)?),
                };
                self.try_consume(TokenKind::CloseParen)?;
                Ok(term)
            }
            _ => {
                println!("term: '{:?}'", self.peek(0).unwrap());
                Err("Unrecognized term, unable to parse.")
            }
        };
    }

    fn token_equals(&self, kind: TokenKind, offset: usize) -> Result<bool, &'static str> {
        if self.peek(offset).is_none() {
            return Err("no token to evaluate");
        }
        if self.peek(offset).unwrap().kind != kind {
            println!(
                "[COMPILER] Expected '{:?}', found '{:?}'",
                kind,
                self.peek(offset).unwrap()
            );
            return Err("token evaluation was false.");
        }
        return Ok(true);
    }

    fn peek(&self, offset: usize) -> Option<&Token> {
        return self.tokens.get(self.position + offset);
    }

    fn consume(&mut self) -> Token {
        // println!("consuming: {:?}", &self.tokens[i]);
        // return self.tokens.remove(0); // pop front value, bad big(o), always shifts every val.

        let i = self.position;
        self.position += 1;
        // println!("consuming: {:?}", &self.tokens[i]);
        return self.tokens[i].clone(); // this works aswell, but clones, ew.
    }

    // code for clarity.
    fn try_consume(&mut self, kind: TokenKind) -> Result<Token, &'static str> {
        self.token_equals(kind, 0)?;
        return Ok(self.consume());
    }
}
