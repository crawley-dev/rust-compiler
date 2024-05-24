use crate::lex_refactor::*;
const LOG_DEBUG_INFO: bool = true;
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
    Let {
        expr: Box<NodeStmt>,
        mutable: bool,
        type_ident: NodeTerm,
    },
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
        op: OpKind,
        lhs: Box<NodeExpr>,
        rhs: Box<NodeExpr>,
    },
    UnaryExpr {
        op: OpKind,
        operand: Box<NodeExpr>,
    },
    Term(NodeTerm),
}

#[derive(Clone, Debug, PartialEq)]
pub enum NodeTerm {
    Type(String),
    Ident(String),
    IntLit(String),
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
            Some(tok) => tok.clone(), // TODO(TOM): fix
            None => return Err(format!("{ERR_MSG} No statement to parse")),
        };
        if LOG_DEBUG_INFO {
            println!("\n{DBG_MSG} parsing statement: {:?}", self.peek(0).unwrap());
        }

        let stmt = match tok {
            Token::Symbol(SymbolKind::OpenSquirly) => NodeStmt::Scope(self.parse_scope()?), // a naked scope.
            Token::Keyword(KeywordKind::Break) => {
                self.try_consume(KeywordKind::Break)?;
                NodeStmt::Break
            }
            Token::Keyword(KeywordKind::Exit) => {
                self.try_consume(KeywordKind::Exit)?;
                self.token_equals(SymbolKind::OpenParen, 0)?;
                let expr = self.parse_expr(0)?;
                NodeStmt::Exit(expr)
            }
            Token::Keyword(KeywordKind::Let) => {
                // TODO(TOM): need to consume type separator && type ident.. whilst preserving ident?
                self.try_consume(KeywordKind::Let)?;
                let mutable = self.try_consume(KeywordKind::Mutable).is_ok();
                let ident = self.try_consume(TokenKind::Ident)?;
                self.try_consume(SymbolKind::TypeSeparator)?;
                let type_ident = match self.try_consume(TokenKind::Ident) {
                    Ok(Token::Ident(name)) => name,
                    Err(e) => return Err(e),
                    _ => unreachable!(),
                };
                self.tokens.insert(self.position, ident);
                NodeStmt::Let {
                    expr: Box::new(self.parse_stmt()?),
                    mutable,
                    type_ident: NodeTerm::Type(type_ident),
                }
            }
            Token::Keyword(KeywordKind::If) => {
                self.try_consume(KeywordKind::If)?;
                let expr = self.parse_expr(0)?;
                let scope = self.parse_scope()?;

                let mut branches = Vec::new();
                loop {
                    if self.try_consume(KeywordKind::Else).is_err() {
                        break;
                    } else if self.try_consume(KeywordKind::If).is_ok() {
                        branches.push(NodeStmt::ElseIf(self.parse_expr(0)?, self.parse_scope()?));
                        continue;
                    }
                    branches.push(NodeStmt::Else(self.parse_scope()?));
                    break;
                }
                NodeStmt::If(expr, scope, branches)
            }
            Token::Keyword(KeywordKind::While) => {
                self.try_consume(KeywordKind::While)?;
                let expr = self.parse_expr(0)?;
                let scope = self.parse_scope()?;
                NodeStmt::While(expr, scope)
            }
            Token::Ident(_) => {
                match self.peek(1) {
                    // 0
                    // create expr, lhs,op, parse_expr(0)
                    Some(Token::Op { flags, .. }) if flags.contains(OpFlags::ASSIGN) => {
                        NodeStmt::Assign(self.parse_expr(0)?)
                    }
                    a @ _ => {
                        todo!("NodeExpr\n{tok:?} {a:?}")
                    }
                }
            }
            _ => return Err(format!("{ERR_MSG} Invalid Statement: '{tok:?}'",)),
        };

        // statments that do/don't require a ';' to end.
        match stmt {
            NodeStmt::Exit(_) | NodeStmt::Assign(_) | NodeStmt::Break => {
                match self.try_consume(SymbolKind::StmtEnd) {
                    Ok(_) => Ok(stmt),
                    Err(e) => Err(format!("{e}.\n {stmt:#?}")),
                }
            }
            _ => Ok(stmt),
        }
    }

    fn parse_scope(&mut self) -> Result<NodeScope, String> {
        self.try_consume(SymbolKind::OpenSquirly)?;
        let mut stmts = Vec::new();
        // while not end of scope, will shit itself in parse_stmt if no CloseSquirly
        while self.token_equals(SymbolKind::CloseSquirly, 0).is_err() {
            stmts.push(self.parse_stmt()?);
        }
        self.try_consume(SymbolKind::CloseSquirly)?;

        Ok(NodeScope {
            stmts,
            inherits_stmts: true,
        })
    }

    fn parse_expr(&mut self, min_prec: u8) -> Result<NodeExpr, String> {
        let mut lhs = self.parse_term()?;

        loop {
            let (op, flags, prec) = match self.peek(0) {
                Some(Token::Op { kind, flags, prec }) => (*kind, *flags, *prec),
                Some(_) => break,
                None => return Err(format!("{ERR_MSG} No operand to parse")),
            };
            if prec < min_prec {
                if LOG_DEBUG_INFO {
                    println!("{DBG_MSG} climb ended: {op:?}, < {min_prec}");
                }
                break;
            }
            let next_prec = prec + (flags.contains(OpFlags::UNARY)) as u8; // TODO(TOM): this will change/break
            self.consume();
            lhs = NodeExpr::BinaryExpr {
                op,
                lhs: Box::new(lhs),
                rhs: Box::new(self.parse_expr(next_prec)?),
            };
        }
        Ok(lhs)
    }

    fn parse_term(&mut self) -> Result<NodeExpr, String> {
        let tok = match self.peek(0) {
            Some(_) => self.consume(),
            None => return Err(format!("{ERR_MSG} No term to parse")),
        };

        match tok {
            Token::Op { kind, flags, prec } if flags.contains(OpFlags::UNARY) => {
                let operand = self.parse_expr(prec + 1)?;
                Ok(NodeExpr::UnaryExpr {
                    op: kind,
                    operand: Box::new(operand),
                })
            }
            Token::Symbol(SymbolKind::OpenParen) => {
                let expr = self.parse_expr(0)?;
                if LOG_DEBUG_INFO {
                    println!("{DBG_MSG} parsed parens {expr:#?}");
                }
                self.try_consume(SymbolKind::CloseParen)?;
                Ok(expr)
            }
            Token::Ident(name) => Ok(NodeExpr::Term(NodeTerm::Ident(name))),
            Token::IntLit(val) => Ok(NodeExpr::Term(NodeTerm::IntLit(val))),
            _ => Err(format!("{ERR_MSG} Invalid Term: '{tok:?}'")),
        }
    }

    fn peek(&self, offset: usize) -> Option<&Token> {
        self.tokens.get(self.position + offset)
    }

    fn consume(&mut self) -> Token {
        if LOG_DEBUG_INFO {
            println!("consuming: {:?}", self.peek(0).unwrap());
        }
        let i = self.position;
        self.position += 1;
        self.tokens.get(i).unwrap().clone()
    }

    fn token_equals<K>(&self, kind: K, offset: usize) -> Result<(), String>
    where
        K: KindTrait + std::fmt::Debug,
    {
        match self.peek(offset) {
            Some(tok) if kind.matches_token(tok) => Ok(()),
            Some(tok) => Err(format!("{ERR_MSG} expected '{kind:?}', found {tok:?}",)),
            _ => Err(format!("{ERR_MSG} No token to evaluate")),
        }
    }

    fn try_consume<K>(&mut self, kind: K) -> Result<Token, String>
    where
        K: KindTrait + std::fmt::Debug,
    {
        self.token_equals(kind, 0)?;
        Ok(self.consume())
    }
}
