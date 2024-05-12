use crate::lexer::{Associativity, Token, TokenKind};
const LOG_DEBUG_INFO: bool = true;

#[derive(Clone, Debug, PartialEq)]
pub struct NodeProg {
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
    Assign(Token, NodeExpr),  // Ident, Expr

    Exit(NodeExpr), // a template for functions (kinda)
    Scope(NodeScope),
    If(NodeExpr, NodeScope, Vec<NodeStmt>),
    ElseIf(NodeExpr, NodeScope),
    Else(NodeScope),
    While(NodeExpr, NodeScope),
    Break,
}

#[derive(Clone, Debug, PartialEq)]
pub enum NodeExpr {
    Term(NodeTerm),
    UnaryExpr {
        op: TokenKind,
        operand: Box<NodeExpr>,
    },
    BinaryExpr {
        op: TokenKind,
        lhs: Box<NodeExpr>,
        rhs: Box<NodeExpr>,
    },
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

    pub fn parse_prog(&mut self) -> Result<NodeProg, String> {
        let mut prog = NodeProg { stmts: vec![] };
        while self.peek(0).is_some() {
            prog.stmts.push(self.parse_stmt()?);
        }
        Ok(prog)
    }

    fn parse_stmt(&mut self) -> Result<NodeStmt, String> {
        let tok = match self.peek(0) {
            Some(tok) => tok,
            None => return Err(format!("[COMPILER_PARSE] No statement to parse")),
        };
        if LOG_DEBUG_INFO {
            println!("\nparsing statement: {:?}", self.peek(0).unwrap());
        }

        let stmt = match tok.kind {
            TokenKind::OpenSquirly => NodeStmt::Scope(self.parse_scope()?), // naked scope.
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
                let ident = self.try_consume(TokenKind::Ident)?;
                self.try_consume(TokenKind::Assign)?;
                NodeStmt::Assign(ident, self.parse_expr(0)?)
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
            _ => {
                return Err(format!(
                    "[COMPILER_PARSE] Unable to parse statement: {tok:?}",
                ))
            }
        };

        // statments that do/don't require a ';' to end.
        match stmt {
            NodeStmt::Exit(_) | NodeStmt::Assign(_, _) | NodeStmt::Break => {
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

    // E --> Exp(0)
    // Exp(p) --> P {B Exp(q)}
    // P --> U Exp(q) | "(" E ")" | v
    // B --> "+" | "-"  | "*" |"/" | "^" | "||" | "&&" | "="
    // U --> "-"

    fn parse_expr(&mut self, min_prec: i32) -> Result<NodeExpr, String> {
        let mut lhs = self.parse_term()?;

        loop {
            let op = match self.peek(0) {
                Some(tok) => &tok.kind,
                None => return Err(format!("[COMPILER_PARSE] No operand to parse")),
            };

            let prec = op.get_prec();
            if prec < min_prec {
                println!("climb ended: {op:?}, < {min_prec}");
                break;
            }

            let next_prec = match op.get_associativity() {
                Associativity::Left => prec + 1,
                Associativity::Right => prec,
                Associativity::None => {
                    return Err(format!(
                        "[COMPILER_PARSE] Illegal non-associative operator found"
                    ))
                }
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
            None => return Err(format!("[COMPILER_PARSE] No term to parse")),
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
                println!("parsed parens {expr:#?}");
                self.try_consume(TokenKind::CloseParen)?;
                Ok(expr)
            }
            TokenKind::Ident => Ok(NodeExpr::Term(NodeTerm::Ident(tok))),
            TokenKind::IntLit => Ok(NodeExpr::Term(NodeTerm::IntLit(tok))),
            _ => Err(format!("[COMPILER_PARSE] Invalid Term, unable to parse.")),
        }
    }

    fn token_equals(&self, kind: TokenKind, offset: usize) -> Result<bool, String> {
        match self.peek(offset) {
            Some(tok) if tok.kind == kind => Ok(true),
            Some(tok) => Err(format!(
                "[COMPILER_PARSE] expected '{kind:?}', found {:?}",
                tok.kind
            )),
            None => Err(format!("[COMPILER_PARSE] No token to evaluate")),
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

// Old Expr Parsing, fake news
/*
fn recurse_expr(&mut self, min_prec: i32) -> Result<NodeExpr, String> {
    if LOG_DEBUG_INFO {
        println!("Parsing expr: {:?}", self.peek(0));
    }

    // if self.peek(0).unwrap().kind.is_unary() {
    //     let op = self.consume().kind;
    //     let operand = self.parse_expr(op.get_prec() + 1)?;

    //     expr = NodeExpr::UnaryExpr {
    //         op,
    //         operand: Box::new(operand),
    //     };
    // } else {
    //     expr = NodeExprself.parse_term()?));
    // }
    // if found close parenthesis
    // decrement paren_count
    // continue to next operator. << should it not break?
    let mut expr = self.parse_term()?;
    loop {
        let mut flag = false;
        if self.try_consume(TokenKind::CloseParen).is_ok() {
            self.ctx.paren_count -= 1;
            if self.ctx.paren_count == 0 {
                break;
            }
            flag = true;
            // close paren consumed,
            // next token gets eaten..
            // set prec to close paren (1000)
        }
        let tok = match self.peek(0) {
            Some(tok) => tok,
            None => return Err(format!("[COMPILER_PARSE] No expr to parse")),
        };
        let prec = if flag == true {
            TokenKind::CloseParen.get_prec()
        } else {
            tok.kind.get_prec()
        };
        if prec < min_prec {
            if LOG_DEBUG_INFO {
                println!("'{tok:?}' prec is lower: {prec} < {min_prec}",);
            }
            break;
        }
        if tok.kind.is_unary() {
            expr = NodeExpr::UnaryExpr {
                op: self.consume().kind,
                operand: Box::new(self.recurse_expr(prec + 1)?),
            }
        } else if tok.kind.is_binary() {
            expr = NodeExpr::BinaryExpr {
                op: self.consume().kind,
                lhs: Box::new(expr),
                rhs: Box::new(self.recurse_expr(prec + 1)?),
            };
        } else {
            return Err(format!(
                "[COMPILER_PARSE] Invalid operator '{:?}', unable to parse",
                tok.kind
            ));
        }
    }
    Ok(expr)
}

fn parse_term(&mut self) -> Result<NodeExpr, String> {
    let tok = match self.peek(0) {
        Some(_) => self.consume(),
        None => return Err(format!("[COMPILER_PARSE] No term to parse")),
    };

    if LOG_DEBUG_INFO {
        println!("\nparsing term: {:?}", tok);
    }

    match tok.kind {
        TokenKind::IntLit => Ok(NodeExpr::IntLit(tok)),
        TokenKind::Ident => Ok(NodeExpr::Ident(tok)),
        TokenKind::OpenParen => {
            self.ctx.paren_count += 1;
            if LOG_DEBUG_INFO {
                println!("\nFound open paren, count: {}", self.ctx.paren_count);
            }
            let expr = self.recurse_expr(0)?;
            println!("[COMPILER_PARSE] Encountered ')', returning: {expr:#?}");
            Ok(NodeExpr::Paren(Box::new(expr))) // temp
        }
        _ if tok.kind.is_unary() => {
            let operand = self.recurse_expr(tok.kind.get_prec() + 1)?;
            Ok(NodeExpr::UnaryExpr {
                op: tok.kind,
                operand: Box::new(operand),
            })
        }
        _ => Err(format!(
            "[COMPILER_PARSE] Unable to parse term: {tok:?}:{}",
            self.position
        )),
    }

    // let mut expr = match self.peek(0) {
    //     // because unary ops are prefix, it comes before any terms.
    //     Some(tok) if tok.kind.is_unary() => {
    //         let op = self.consume().kind;
    //         let operand = self.parse_expr(op.get_prec() + 1)?;
    //         NodeExpr::UnaryExpr {
    //             op,
    //             operand: Box::new(operand),
    //         }
    //     }
    //     Some(_) if self.token_equals(TokenKind::OpenParen, 0).is_ok() => {
    //         self.ctx.paren_count += 1;
    //         if LOG_DEBUG_INFO {
    //             println!("\nFound open paren, count: {}", self.ctx.paren_count);
    //         }
    //         let expr = self.parse_expr(0)?;
    //         if self.ctx.paren_count != 0 {
    //             return Err(format!("[COMPILER_PARSE] No ')' found near {expr:#?}"));
    //         }
    //         expr
    //     }
    //     Some(_) => NodeExpr::Term(Box::new(self.parse_term()?)),
    //     None => return Err(format!("[COMPILER_PARSE] No token to parse")),
    // };
}
*/
