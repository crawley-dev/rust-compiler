use crate::lexer::{KeywordKind, OperandKind, SymbolKind, Token};
const LOG_DEBUG_INFO: bool = true;

#[derive(Debug, PartialEq)]
pub struct NodeProg {
    pub stmts: Vec<NodeStmt>,
}

#[derive(Debug, PartialEq)]
pub struct NodeScope {
    pub stmts: Vec<NodeStmt>,
    pub inherits_stmts: bool,
}

#[derive(Debug, PartialEq)]
pub enum NodeStmt {
    Exit(NodeExpr),
    Let(Token, NodeExpr),
    Scope(NodeScope),
    If(NodeExpr, NodeScope, Vec<NodeStmt>),
    ElseIf(NodeExpr, NodeScope),
    Else(NodeScope),
}

#[derive(Debug, PartialEq)]
pub enum NodeExpr {
    Term(Box<NodeTerm>),
    BinExpr {
        op: OperandKind,
        lhs: Box<NodeExpr>,
        rhs: Box<NodeExpr>,
    },
    BoolExpr {
        op: OperandKind,
        lhs: Box<NodeExpr>,
        rhs: Box<NodeExpr>,
    },
}

#[derive(Debug, PartialEq)]
pub enum NodeTerm {
    Ident(Token),
    IntLit(Token),
    Paren(NodeExpr),
}

pub struct Parser {
    pub tokens: Vec<Token>,
    pub position: usize,
}

impl Parser {
    pub fn new(input: Vec<Token>) -> Parser {
        let parser = Parser {
            tokens: input,
            position: 0,
        };
        return parser;
    }

    pub fn parse_prog(&mut self) -> Result<NodeProg, String> {
        let mut prog = NodeProg { stmts: vec![] };
        while self.peek(0).is_some() {
            prog.stmts.push(self.parse_stmt()?);
        }
        return Ok(prog);
    }

    fn parse_stmt(&mut self) -> Result<NodeStmt, String> {
        let tok = match self.peek(0) {
            Some(tok) => tok,
            None => return Err(format!("[COMPILER] No statement to parse")),
        };
        if LOG_DEBUG_INFO {
            println!("\nparsing statement: {:?}", self.peek(0).unwrap());
        }

        let stmt = match tok.kind {
            TokenKind::KeywordExit => {
                self.try_consume(TokenKind::KeywordExit)?;
                self.try_consume(TokenKind::OpenParen)?;
                let stmt = NodeStmt::Exit(self.parse_expr(0)?);
                self.try_consume(TokenKind::CloseParen)?;

                stmt
            }
            TokenKind::KeywordLet => {
                self.try_consume(TokenKind::KeywordLet)?;
                let ident = self.try_consume(TokenKind::Ident)?;
                self.try_consume(TokenKind::Assign)?;

                NodeStmt::Let(ident, self.parse_expr(0)?)
            }
            TokenKind::KeywordIf => {
                self.try_consume(TokenKind::KeywordIf)?;
                let expr = self.parse_expr(0)?;
                let scope = self.parse_scope()?;

                let mut branches = Vec::new();
                loop {
                    // no more conditions
                    if self.try_consume(TokenKind::KeywordElse).is_err() {
                        break;
                    }
                    // "else if" condition
                    if self.try_consume(TokenKind::KeywordIf).is_ok() {
                        branches.push(NodeStmt::ElseIf(self.parse_expr(0)?, self.parse_scope()?));
                        continue;
                    }
                    // "else" condition
                    branches.push(NodeStmt::Else(self.parse_scope()?));
                    break;
                }

                NodeStmt::If(expr, scope, branches)
            }
            TokenKind::OpenSquirly => NodeStmt::Scope(self.parse_scope()?),
            _ => return Err(format!("[COMPILER] Unable to parse statement: {tok:?}",)),
        };

        // statments that do/don't require a ';' to end.
        return match stmt {
            NodeStmt::Let(_, _) | NodeStmt::Exit(_) => {
                if self.try_consume(TokenKind::Separator).is_err() {
                    println!("{:#?}", stmt);
                    return Err(format!("[COMPILER] Separator ';' not found"));
                }
                Ok(stmt)
            }
            _ => Ok(stmt),
        };
    }

    fn parse_scope(&mut self) -> Result<NodeScope, String> {
        self.try_consume(TokenKind::OpenSquirly)?;
        let mut stmts = Vec::new();
        // while not end of scope, will shit itself in parse_stmt if no CloseSquirly
        while self.token_equals(TokenKind::CloseSquirly, 0).is_err() {
            stmts.push(self.parse_stmt()?);
        }
        self.try_consume(TokenKind::CloseSquirly)?;

        return Ok(NodeScope {
            stmts,
            inherits_stmts: true,
        });
    }

    fn parse_expr(&mut self, min_prec: i32) -> Result<NodeExpr, String> {
        let term = self.parse_term()?;
        let mut expr = NodeExpr::Term(Box::new(term));

        loop {
            let prec = match self.peek(0) {
                Some(tok) => tok.kind.get_prec(),
                None => return Err(format!("[COMPILER] No token to parse")),
            };
            if prec < min_prec {
                if LOG_DEBUG_INFO {
                    println!(
                        "prec is lower {:?}:{prec} > {min_prec}",
                        self.peek(0).unwrap()
                    );
                }
                break;
            }

            let next_prec = prec + 1;
            let op = self.consume().kind;
            let rhs = self.parse_expr(next_prec)?;

            if op.is_bin_op() {
                expr = NodeExpr::BinExpr {
                    op,
                    lhs: Box::new(expr),
                    rhs: Box::new(rhs),
                };
            } else if op.is_logical_op() || op.is_comparison_op() {
                expr = NodeExpr::BoolExpr {
                    op,
                    lhs: Box::new(expr),
                    rhs: Box::new(rhs),
                };
            } else {
                return Err(format!("[COMPILER] Invalid operator, unable to parse"));
            }
        }
        return Ok(expr);
    }

    fn parse_term(&mut self) -> Result<NodeTerm, String> {
        let tok = match self.peek(0) {
            Some(tok) => tok,
            None => return Err(format!("[COMPILER] No term to parse")),
        };

        if LOG_DEBUG_INFO {
            println!("\nparsing term: {:?}", tok);
        }

        return match tok.kind {
            TokenKind::IntLit => Ok(NodeTerm::IntLit(self.consume())),
            TokenKind::Ident => Ok(NodeTerm::Ident(self.consume())),
            TokenKind::OpenParen => {
                self.try_consume(TokenKind::OpenParen)?;
                let term = NodeTerm::Paren(self.parse_expr(0)?);
                self.try_consume(TokenKind::CloseParen)?;
                Ok(term)
            }
            _ => Err(format!(
                "[COMPILER] Unable to parse term: {tok:?}:{}",
                self.position
            )),
        };
    }

    fn token_equals(&self, kind: TokenKind, offset: usize) -> Result<bool, String> {
        return match self.peek(offset) {
            Some(tok) if tok.kind == kind => Ok(true),
            None => Err(format!("[COMPILER] No token to evaluate")),
            tok @ _ => Err(format!(
                "[COMPILER] expected '{kind:?}', found {:?}",
                tok.unwrap().kind
            )),
        };
    }

    fn peek(&self, offset: usize) -> Option<&Token> {
        return self.tokens.get(self.position + offset);
    }

    // remove item from vec? << no clone, linear complexity though..
    fn consume(&mut self) -> Token {
        if LOG_DEBUG_INFO {
            println!("consuming: {:?}", self.peek(0).unwrap());
        }
        let i = self.position;
        self.position += 1;
        return self.tokens[i].clone(); // this works aswell, but clones, ew.
                                       // return self.tokens.get(i).unwrap()
    }

    fn try_consume(&mut self, kind: TokenKind) -> Result<Token, String> {
        self.token_equals(kind, 0)?;
        return Ok(self.consume());
    }
}
