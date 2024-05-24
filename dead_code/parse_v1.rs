use crate::lex::{Associativity, Token, TokenKind};
const LOG_DEBUG_INFO: bool = false;
const ERR_MSG: &'static str = "[ERROR_PARSE]";
const DBG_MSG: &'static str = "[DEBUG_PARSE]";

#[derive(Clone, Debug, PartialEq)]
pub struct AST {
    pub stmts: Vec<NodeStmt>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct NodeScope {
    pub stmts: Vec<NodeStmt>,
    pub inherits_stmts: bool,
}

#[derive(Clone, Debug, PartialEq)]
pub enum NodeStmt {
    Let(Box<NodeStmt>, bool), // Ident, Mutability.
    Assign(NodeExpr),
    // Expr(NodeExpr),
    Exit(NodeExpr),
    Scope(NodeScope),
    If(NodeExpr, NodeScope, Vec<NodeStmt>),
    ElseIf(NodeExpr, NodeScope),
    Else(NodeScope),
    While(NodeExpr, NodeScope),
    Break,
}

#[derive(Clone, Debug, PartialEq)]
pub enum NodeExpr {
    BinaryExpr {
        op: TokenKind,
        lhs: Box<NodeExpr>,
        rhs: Box<NodeExpr>,
    },
    UnaryExpr {
        op: TokenKind,
        operand: Box<NodeExpr>,
    },
    Term(NodeTerm),
}

#[derive(Clone, Debug, PartialEq)]
pub enum NodeTerm {
    Ident(Token),
    IntLit(Token),
}

pub struct Parser {
    pub tokens: Vec<Token>,
    pub position: usize,
}

impl Parser {
    pub fn new(input: Vec<Token>) -> Parser {
        Parser {
            tokens: input,
            position: 0,
        }
    }

    pub fn parse_prog(&mut self) -> Result<AST, String> {
        let mut ast = AST { stmts: vec![] };
        while self.peek(0).is_some() {
            ast.stmts.push(self.parse_stmt()?);
        }
        Ok(ast)
    }

    fn parse_stmt(&mut self) -> Result<NodeStmt, String> {
        let tok = match self.peek(0) {
            Some(tok) => tok,
            None => return Err(format!("{ERR_MSG} No statement to parse")),
        };
        if LOG_DEBUG_INFO {
            println!("\n{DBG_MSG} parsing statement: {:?}", self.peek(0).unwrap());
        }

        let stmt = match tok.kind {
            TokenKind::OpenSquirly => NodeStmt::Scope(self.parse_scope()?), // a naked scope.
            TokenKind::Break => {
                self.try_consume(TokenKind::Break)?;
                NodeStmt::Break
            }
            TokenKind::Exit => {
                self.try_consume(TokenKind::Exit)?;
                self.token_equals(TokenKind::OpenParen, 0)?;
                let expr = self.parse_expr(0)?;
                NodeStmt::Exit(expr)
            }
            TokenKind::Let => {
                self.try_consume(TokenKind::Let)?;
                let mutable = self.try_consume(TokenKind::Mutable).is_ok();
                self.token_equals(TokenKind::Ident, 0)?;
                NodeStmt::Let(Box::new(self.parse_stmt()?), mutable)
            }
            TokenKind::Ident => {
                if self.token_matches(|kind| kind.is_assignment(), 1)? == true {
                    NodeStmt::Assign(self.parse_expr(0)?)
                } else {
                    todo!("NodeStmt::Expr(), no semantic analysis support to make viable.")
                    // NodeStmt::Expr(self.parse_expr(0)?)
                }
            }
            TokenKind::If => {
                self.try_consume(TokenKind::If)?;
                let expr = self.parse_expr(0)?;
                let scope = self.parse_scope()?;

                let mut branches = Vec::new();
                loop {
                    if self.try_consume(TokenKind::Else).is_err() {
                        break;
                    } else if self.try_consume(TokenKind::If).is_ok() {
                        branches.push(NodeStmt::ElseIf(self.parse_expr(0)?, self.parse_scope()?));
                        continue;
                    }
                    branches.push(NodeStmt::Else(self.parse_scope()?));
                    break;
                }
                NodeStmt::If(expr, scope, branches)
            }
            TokenKind::While => {
                self.try_consume(TokenKind::While)?;
                let expr = self.parse_expr(0)?;
                let scope = self.parse_scope()?;
                NodeStmt::While(expr, scope)
            }
            _ => return Err(format!("{ERR_MSG} Invalid Statement: '{tok:?}'",)),
        };

        // statments that do/don't require a ';' to end.
        match stmt {
            NodeStmt::Exit(_) | NodeStmt::Assign(_) | NodeStmt::Break => {
                match self.try_consume(TokenKind::StmtEnd) {
                    Ok(_) => Ok(stmt),
                    Err(e) => Err(format!("{e}.\n {stmt:#?}")),
                }
            }
            _ => Ok(stmt),
        }
    }

    fn parse_scope(&mut self) -> Result<NodeScope, String> {
        self.try_consume(TokenKind::OpenSquirly)?;
        let mut stmts = Vec::new();
        // while not end of scope, will shit itself in parse_stmt if no CloseSquirly
        while self.token_equals(TokenKind::CloseSquirly, 0).is_err() {
            stmts.push(self.parse_stmt()?);
        }
        self.try_consume(TokenKind::CloseSquirly)?;

        Ok(NodeScope {
            stmts,
            inherits_stmts: true,
        })
    }

    fn parse_expr(&mut self, min_prec: i32) -> Result<NodeExpr, String> {
        let mut lhs = self.parse_term()?;

        loop {
            let op = match self.peek(0) {
                Some(tok) => &tok.kind,
                None => return Err(format!("{ERR_MSG} No operand to parse")),
            };

            let prec = op.get_prec();
            if prec < min_prec {
                if LOG_DEBUG_INFO {
                    println!("{DBG_MSG} climb ended: {op:?}, < {min_prec}");
                }
                break;
            }

            let next_prec = match op.get_associativity() {
                Associativity::Left => prec + 1,
                Associativity::Right => prec,
            };

            lhs = NodeExpr::BinaryExpr {
                op: self.consume().kind,
                lhs: Box::new(lhs),
                rhs: Box::new(self.parse_expr(next_prec)?),
            }
        }
        Ok(lhs)
    }

    fn parse_term(&mut self) -> Result<NodeExpr, String> {
        let tok = match self.peek(0) {
            Some(_) => self.consume(),
            None => return Err(format!("{ERR_MSG} No term to parse")),
        };

        match tok.kind {
            op @ _ if op.is_unary() => {
                let operand = self.parse_expr(op.get_prec() + 1)?;
                Ok(NodeExpr::UnaryExpr {
                    op,
                    operand: Box::new(operand),
                })
            }
            TokenKind::OpenParen => {
                let expr = self.parse_expr(0)?;
                if LOG_DEBUG_INFO {
                    println!("{DBG_MSG} parsed parens {expr:#?}");
                }
                self.try_consume(TokenKind::CloseParen)?;
                Ok(expr)
            }
            TokenKind::Ident => Ok(NodeExpr::Term(NodeTerm::Ident(tok))),
            TokenKind::IntLit => Ok(NodeExpr::Term(NodeTerm::IntLit(tok))),
            _ => Err(format!("{ERR_MSG} Invalid Term: '{tok:?}'")),
        }
    }

    fn token_equals(&self, kind: TokenKind, offset: usize) -> Result<(), String> {
        match self.peek(offset) {
            Some(tok) if tok.kind == kind => Ok(()),
            Some(tok) => Err(format!(
                "{ERR_MSG} expected '{kind:?}', found {:?}",
                tok.kind
            )),
            None => Err(format!("{ERR_MSG} No token to evaluate")),
        }
    }

    fn token_matches(
        &self,
        pattern: impl Fn(&TokenKind) -> bool,
        offset: usize,
    ) -> Result<bool, String> {
        match self.peek(offset) {
            Some(tok) => Ok(pattern(&tok.kind)),
            None => Err(format!("{ERR_MSG} No token to evalutate")),
        }
    }

    fn peek(&self, offset: usize) -> Option<&Token> {
        self.tokens.get(self.position + offset)
    }

    // remove item from vec? << no clone, linear complexity though..
    fn consume(&mut self) -> Token {
        if LOG_DEBUG_INFO {
            println!("consuming: {:?}", self.peek(0).unwrap());
        }
        let i = self.position;
        self.position += 1;
        self.tokens.get(i).unwrap().clone()
    }

    fn try_consume(&mut self, kind: TokenKind) -> Result<Token, String> {
        self.token_equals(kind, 0)?;
        Ok(self.consume())
    }
}
