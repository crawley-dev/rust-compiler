// >>PARSER<< Constructs statements out of tokens from the lexer.
use crate::{
    debug, err,
    lex::{Associativity, Token, TokenFlags, TokenKind},
    semantic::SemVariable,
};
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
    Decl {
        init_expr: Option<NodeExpr>,
        ident: String,
        type_ident: String,
        mutable: bool,
        ptr: bool, // TODO(TOM): turn this into enum for different forms, e.g array
    },
    If {
        condition: NodeExpr,
        scope: NodeScope,
        branches: Vec<NodeStmt>,
    },
    ElseIf {
        condition: NodeExpr,
        scope: NodeScope,
    },
    Else(NodeScope),
    While {
        condition: NodeExpr,
        scope: NodeScope,
    },
    Assign(NodeExpr),
    Exit(NodeExpr),
    NakedScope(NodeScope),
    Break,
    // SEMANTIC STMT "CONVERSIONS"
    SemDecl(SemVariable),
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
        let mut ast = AST { stmts: Vec::new() };
        while self.peek(0).is_some() {
            ast.stmts.push(self.parse_stmt()?);
        }
        Ok(ast)
    }

    fn parse_stmt(&mut self) -> Result<NodeStmt, String> {
        let tok = match self.peek(0) {
            Some(tok) => tok,
            None => return err!("No statement to parse"),
        };
        debug!("\nparsing statement: {:?}", self.peek(0).unwrap());

        let stmt = match tok.kind {
            TokenKind::Let => {
                self.try_consume(TokenKind::Let)?;
                let mutable = self.try_consume(TokenKind::Mut).is_ok();
                let ident = self.try_consume(TokenKind::Ident)?.value.unwrap();

                self.try_consume(TokenKind::Colon)?;
                let ptr = self.try_consume(TokenKind::Ptr).is_ok();
                let type_ident = self.try_consume(TokenKind::Ident)?.value.unwrap();

                let init_expr = match self.try_consume(TokenKind::Eq) {
                    Ok(_) => Some(self.parse_expr(0)?),
                    Err(_) => None,
                };

                NodeStmt::Decl {
                    init_expr,
                    ident,
                    type_ident,
                    mutable,
                    ptr,
                }
            }
            TokenKind::If => {
                self.try_consume(TokenKind::If)?;
                let condition = self.parse_expr(0)?;
                let scope = self.parse_scope()?;

                let mut branches = Vec::new();
                loop {
                    if self.try_consume(TokenKind::Else).is_err() {
                        break;
                    } else if self.try_consume(TokenKind::If).is_ok() {
                        branches.push(NodeStmt::ElseIf {
                            condition: self.parse_expr(0)?,
                            scope: self.parse_scope()?,
                        });
                        continue;
                    }
                    branches.push(NodeStmt::Else(self.parse_scope()?));
                    break;
                }
                NodeStmt::If {
                    condition,
                    scope,
                    branches,
                }
            }
            TokenKind::While => {
                self.try_consume(TokenKind::While)?;
                let condition = self.parse_expr(0)?;
                let scope = self.parse_scope()?;
                NodeStmt::While { condition, scope }
            }
            TokenKind::Ident => {
                if self.token_matches(
                    |kind| kind.has_flags(TokenFlags::ASSIGN) && kind != &TokenKind::Eq,
                    1,
                )? == true
                {
                    let comp_assign = self.tokens.get_mut(self.position + 1).unwrap();
                    comp_assign.kind = comp_assign.kind.assign_to_arithmetic()?;
                    NodeStmt::Assign(self.parse_expr(0)?)
                } else {
                    todo!("Naked Expression, Currently not valid.") // TODO(TOM): an expr is a scope's return statement, only valid if last stmt/expr.
                }
            }
            TokenKind::Exit => {
                self.try_consume(TokenKind::Exit)?;
                self.token_equals(TokenKind::OpenParen, 0)?;
                let expr = self.parse_expr(0)?;
                NodeStmt::Exit(expr)
            }
            TokenKind::Break => {
                self.try_consume(TokenKind::Break)?;
                NodeStmt::Break
            }
            TokenKind::OpenBrace => NodeStmt::NakedScope(self.parse_scope()?),
            _ => return err!("Invalid Statement: '{tok:?}'"),
        };

        // statments that do/don't require a ';' to end.
        match stmt {
            NodeStmt::Exit(_)
            | NodeStmt::Assign { .. }
            | NodeStmt::Decl { .. }
            | NodeStmt::Break => match self.try_consume(TokenKind::SemiColon) {
                Ok(_) => Ok(stmt),
                Err(e) => Err(format!("{e}.\n {stmt:#?}")),
            },
            _ => Ok(stmt),
        }
    }

    fn parse_scope(&mut self) -> Result<NodeScope, String> {
        // consumes statements until a matching closebrace is found.
        self.try_consume(TokenKind::OpenBrace)?;
        let mut stmts = Vec::new();
        while self.try_consume(TokenKind::CloseBrace).is_err() {
            stmts.push(self.parse_stmt()?);
        }

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
                None => return err!("No operand to parse near => \n{lhs:#?}"),
            };

            // NOTE: tokens with no precedence are valued at -1, therefore always exit loop.
            // .. parse_expr escapes when it hits a semicolon because its prec is -1 !! thats unclear
            let prec = op.get_prec();
            if prec < min_prec {
                debug!("climb ended: {op:?}({prec}) < {min_prec}");
                break;
            }

            let next_prec = match op.get_associativity() {
                Associativity::Left => prec + 1,
                Associativity::Right => prec,
                Associativity::None => {
                    unreachable!("{ERR_MSG} Usage of a non-associative operator: {op:?}")
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
            None => return err!("No term to parse"),
        };

        match tok.kind {
            op @ _ if op.has_flags(TokenFlags::UNARY) => {
                let operand = self.parse_expr(op.get_prec() + 1)?;
                Ok(NodeExpr::UnaryExpr {
                    op,
                    operand: Box::new(operand),
                })
            }
            TokenKind::OpenParen => {
                let expr = self.parse_expr(0)?;
                debug!("parsed parens {expr:#?}");
                self.try_consume(TokenKind::CloseParen)?;
                Ok(expr)
            }
            TokenKind::Ident => Ok(NodeExpr::Term(NodeTerm::Ident(tok))),
            TokenKind::IntLit => Ok(NodeExpr::Term(NodeTerm::IntLit(tok))),
            _ => err!("Invalid Term: '{tok:?}'"),
        }
    }

    fn token_equals(&self, kind: TokenKind, offset: usize) -> Result<(), String> {
        match self.peek(offset) {
            Some(tok) if tok.kind == kind => Ok(()),
            Some(tok) => err!("expected '{kind:?}', found {:?}", tok.kind,),
            None => err!("No token to evaluate"),
        }
    }

    fn token_matches(
        &self,
        pattern: impl Fn(&TokenKind) -> bool,
        offset: usize,
    ) -> Result<bool, String> {
        match self.peek(offset) {
            Some(tok) => Ok(pattern(&tok.kind)),
            None => err!("No token to evalutate"),
        }
    }

    fn peek(&self, offset: usize) -> Option<&Token> {
        self.tokens.get(self.position + offset)
    }

    // remove item from vec? << no clone, linear complexity though..
    fn consume(&mut self) -> Token {
        debug!("consuming: {:?}", self.peek(0).unwrap());
        let i = self.position;
        self.position += 1;
        self.tokens.get(i).unwrap().clone()
    }

    fn try_consume(&mut self, kind: TokenKind) -> Result<Token, String> {
        self.token_equals(kind, 0)?;
        Ok(self.consume())
    }
}
