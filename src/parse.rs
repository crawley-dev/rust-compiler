use crate::{
    debug, err,
    lex::{Associativity, Token, TokenFlags, TokenKind},
    semantic::{AddressingMode, Variable},
    utils,
};
use anyhow::{Error, Result};
use std::collections::VecDeque;

#[derive(Debug, Clone)]
pub struct Arg {
    pub ident: Token,
    pub mutable: bool,
    pub parse_type: ParseType,
}

#[derive(Debug, Clone)]
pub struct ParseType {
    pub type_tok: Token,
    pub addr_mode: AddressingMode,
}

#[derive(Debug, Clone)]
pub enum InitExpr {
    Some(NodeExpr),
    None,
    Deferred, // trust me bro, it exists.
}

#[derive(Debug, Clone)]
pub struct NodeScope {
    pub stmts: Vec<NodeStmt>,
    pub inherits_stmts: bool,
}

#[derive(Clone)]
pub struct Ast {
    pub stmts: Vec<NodeStmt>,
}

// pub struct Node<T> {
//     pub node: T,
//     pub pos: (u32, u32),
//     pub end: (u32, u32),
// }

// TODO(TOM): impl this
// pub enum NodeTopLevel {
//     FnDecl {
//         ident: Token,
//         args: Vec<Arg>,
//         scope: NodeScope,
//         return_type_tok: Option<Token>,
//         return_addr_mode: Option<AddressingMode>,
//     },
// }
// pub enum SemNodeTopLevel

#[derive(Debug, Clone)]
pub enum NodeStmt {
    FnDecl {
        ident: Token,
        args: Vec<Arg>,
        scope: NodeScope,
        return_type: Option<ParseType>,
        // return_type_tok: Option<Token>,
        // return_addr_mode: Option<AddressingMode>,
    },
    VarDecl {
        init_expr: InitExpr,
        arg: Arg,
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
    Assign {
        ident: Token,
        expr: NodeExpr,
    },
    Exit(NodeExpr),
    NakedScope(NodeScope),
    Break,
    Return(Option<NodeExpr>),
    // SEMANTIC STMT "CONVERSIONS"
    VarSemantics(Variable),
    FnSemantics {
        id: usize,
    },
    // ReturnSemantics {
    //     expr: Option<ExprData>,
    // },
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
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

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum NodeTerm {
    True,
    False,
    Ident(Token),
    IntLit(Token),
    FnCall { ident: Token, args: Vec<NodeExpr> },
}

pub struct Parser {
    pub tokens: VecDeque<Token>,
    pub idx: usize,
}

impl Parser {
    pub fn new(tokens: VecDeque<Token>) -> Self {
        Self { tokens, idx: 0 }
    }

    pub fn parse_tokens(mut self) -> (Ast, Option<Error>) {
        let mut ast: Ast = Ast { stmts: Vec::new() };

        while self.peek(0).is_some() {
            match self.parse_top_level() {
                Ok(stmt) => ast.stmts.push(stmt),
                Err(e) => return (ast, Some(e)),
            };
        }
        (ast, None)
    }

    fn parse_top_level(&mut self) -> Result<NodeStmt> {
        if !self.expect(TokenKind::Fn).is_ok() {
            return err!(
                "A Program only consists of functions, this is =>\n{:#?}",
                self.peek(0)
            );
        }

        let ident = self.expect(TokenKind::Ident)?;
        self.expect(TokenKind::OpenParen)?;

        // parsing function arguments
        let mut args = Vec::new();
        while self.token_equals(TokenKind::CloseParen, 0).is_err() {
            if !args.is_empty() {
                self.expect(TokenKind::Comma)?;
            }

            let mutable = self.expect(TokenKind::Mut).is_ok();
            let ident = self.expect(TokenKind::Ident)?;
            self.expect(TokenKind::Colon)?;
            let parse_type = self.parse_type()?;
            args.push(Arg {
                ident,
                mutable,
                parse_type,
            });
        }
        self.expect(TokenKind::CloseParen)?;

        // parse function return type
        let return_type = match self.expect(TokenKind::Arrow) {
            Ok(_) => Some(self.parse_type()?),
            Err(_) => None,
        };
        let scope = self.parse_scope(false)?;

        Ok(NodeStmt::FnDecl {
            ident,
            args,
            scope,
            return_type,
        })
    }

    fn parse_stmt(&mut self) -> Result<NodeStmt> {
        let tok = match self.peek(0) {
            Some(tok) => tok,
            None => return err!("No statement to parse"),
        };
        debug!("parsing statement: {tok:?}");

        let stmt = match tok.kind {
            TokenKind::Let => {
                self.expect(TokenKind::Let)?;
                let mutable = self.expect(TokenKind::Mut).is_ok();
                let ident = self.expect(TokenKind::Ident)?;

                self.expect(TokenKind::Colon)?;
                let parse_type = self.parse_type()?;

                let init_expr = match self.expect(TokenKind::Eq) {
                    Ok(_) => InitExpr::Some(self.parse_expr(0)?),
                    Err(_) => InitExpr::None,
                };

                NodeStmt::VarDecl {
                    init_expr,
                    arg: Arg {
                        ident,
                        mutable,
                        parse_type,
                    },
                }
            }
            TokenKind::If => {
                self.expect(TokenKind::If)?;
                let condition = self.parse_expr(0)?;
                let scope = self.parse_scope(true)?;

                let mut branches = Vec::new();
                loop {
                    if self.expect(TokenKind::Else).is_err() {
                        break;
                    } else if self.expect(TokenKind::If).is_ok() {
                        branches.push(NodeStmt::ElseIf {
                            condition: self.parse_expr(0)?,
                            scope: self.parse_scope(true)?,
                        });
                        continue;
                    }
                    branches.push(NodeStmt::Else(self.parse_scope(true)?));
                    break;
                }

                NodeStmt::If {
                    condition,
                    scope,
                    branches,
                }
            }
            TokenKind::Fn => {
                return err!("Functions cannot be nested, they're top level statements")
            }
            TokenKind::Return => {
                self.expect(TokenKind::Return)?;
                match self.peek(0) {
                    Some(tok) if tok.kind == TokenKind::SemiColon => NodeStmt::Return(None),
                    _ => NodeStmt::Return(Some(self.parse_expr(0)?)),
                }
            }
            TokenKind::While => {
                self.expect(TokenKind::While)?;
                let condition = self.parse_expr(0)?;
                let scope = self.parse_scope(true)?;
                NodeStmt::While { condition, scope }
            }
            TokenKind::Ident => {
                let ident = self.expect(TokenKind::Ident)?;
                match self.peek(0) {
                    // Assignment: consume ident & '='. parse expr.
                    Some(tok) if tok.kind == TokenKind::Eq => {
                        self.expect(TokenKind::Eq)?;
                        NodeStmt::Assign {
                            ident,
                            expr: self.parse_expr(0)?,
                        }
                    }
                    // Compound Assign: clone ident, swap assign to arith counterpart, parse expr
                    //      - 'ident += 5;' => 'ident + 5;'
                    Some(tok) if tok.kind.has_flags(TokenFlags::ASSIGN) => {
                        self.tokens.push_front(ident.clone()); // TODO(TOM): this may not work !
                        let comp_assign = self.peek_mut(1).unwrap();
                        comp_assign.kind = comp_assign.kind.assign_to_arithmetic()?;
                        NodeStmt::Assign {
                            ident,
                            expr: self.parse_expr(0)?,
                        }
                    }
                    _ => return err!("Naked Expression => '{:?}', Not Valid", self.peek(0)),
                }
            }
            TokenKind::Exit => {
                self.expect(TokenKind::Exit)?;
                self.token_equals(TokenKind::OpenParen, 0)?;
                let expr = self.parse_expr(0)?;
                NodeStmt::Exit(expr)
            }
            TokenKind::Break => {
                self.expect(TokenKind::Break)?;
                NodeStmt::Break
            }
            TokenKind::OpenBrace => NodeStmt::NakedScope(self.parse_scope(true)?),
            _ => return err!("Invalid Statement =>\n{tok:#?}"),
        };

        // statments that do/don't require a ';' to end.
        match stmt {
            NodeStmt::Exit(_)
            | NodeStmt::Assign { .. }
            | NodeStmt::VarDecl { .. }
            | NodeStmt::Break
            | NodeStmt::Return(_) => match self.expect(TokenKind::SemiColon) {
                Ok(_) => Ok(stmt),
                Err(e) => err!("{e}.\n{stmt:#?}"),
            },
            _ => Ok(stmt),
        }
    }

    fn parse_scope(&mut self, inherits_stmts: bool) -> Result<NodeScope> {
        // consumes statements until a closebrace is found.
        self.expect(TokenKind::OpenBrace)?;
        let mut stmts = Vec::new();
        while self.expect(TokenKind::CloseBrace).is_err() {
            stmts.push(self.parse_stmt()?);
        }

        Ok(NodeScope {
            stmts,
            inherits_stmts,
        })
    }

    fn parse_expr(&mut self, min_prec: i32) -> Result<NodeExpr> {
        let mut lhs = self.parse_term()?;

        loop {
            let op = match self.peek(0) {
                Some(tok) => &tok.kind,
                None => return err!("No token to parse near =>\n{lhs:#?}"),
            };
            // unary expressions don't recurse as no rhs, only iterate so
            let bin_prec = op.get_prec_binary();
            let un_prec = op.get_prec_unary();

            // NOTE: tokens with no precedence are valued at -1, therefore always exit loop.
            // .. parse_expr escapes when it hits a semicolon because its prec is -1 !! thats unclear
            if bin_prec < min_prec && un_prec < min_prec {
                debug!("precedence climb ended: {op:?}({bin_prec}) < {min_prec}");
                break;
            }

            let is_unary = un_prec >= 0;
            if is_unary {
                let tok = match self.peek(1) {
                    Some(tok) => tok,
                    None => return err!("No token to parse near =>\n{lhs:#?}"),
                };
                match tok.kind {
                    // tok is an expression, must be binary
                    TokenKind::IntLit | TokenKind::Ident | TokenKind::OpenParen => {
                        debug!("found rhs of an expression '{tok:?}', operator must not be unary!")
                    }
                    // not a 'NodeTerm', must be unary.
                    _ => {
                        lhs = NodeExpr::UnaryExpr {
                            op: self.consume().kind,
                            operand: Box::new(lhs),
                        };
                        continue;
                    }
                }
            }

            let next_prec = match op.get_associativity(is_unary) {
                Associativity::Right => bin_prec,
                Associativity::Left => bin_prec + 1,
                // Associativity::None => return err!(self, "non-associative operator => '{op:?}'"),
            };

            lhs = NodeExpr::BinaryExpr {
                op: self.consume().kind,
                lhs: Box::new(lhs),
                rhs: Box::new(self.parse_expr(next_prec)?),
            }
        }
        Ok(lhs)
    }

    // peeking next token might not work because it could be a close paren?
    fn parse_term(&mut self) -> Result<NodeExpr> {
        let tok = match self.peek(0) {
            Some(_) => self.consume(),
            None => return err!("Expected term, found nothing."),
        };

        match tok.kind {
            op @ _ if op.has_flags(TokenFlags::UNARY) => {
                debug!("found unary expression: '{op:?}'");
                let operand = self.parse_expr(op.get_prec_unary() + 1)?;
                Ok(NodeExpr::UnaryExpr {
                    op,
                    operand: Box::new(operand),
                })
            }
            TokenKind::OpenParen => {
                // greedily consume everything in parenthesis.
                let expr = self.parse_expr(0)?;
                debug!("parsed parens {expr:#?}");
                self.expect(TokenKind::CloseParen)?;
                Ok(expr)
            }
            TokenKind::Ident => {
                match self.peek(0) {
                    // Function Calls
                    Some(next) if next.kind == TokenKind::OpenParen => {
                        self.expect(TokenKind::OpenParen)?;
                        let mut args = Vec::new();
                        while self.expect(TokenKind::CloseParen).is_err() {
                            if args.len() > 1 {
                                self.expect(TokenKind::Comma)?;
                            }
                            args.push(self.parse_expr(0)?);
                        }
                        Ok(NodeExpr::Term(NodeTerm::FnCall { ident: tok, args }))
                    }
                    Some(_) => Ok(NodeExpr::Term(NodeTerm::Ident(tok))),
                    None => err!("Incomplete expression, nothing after =>\n{tok:#?}"),
                }
            }
            TokenKind::IntLit => Ok(NodeExpr::Term(NodeTerm::IntLit(tok))),
            TokenKind::True => Ok(NodeExpr::Term(NodeTerm::True)),
            TokenKind::False => Ok(NodeExpr::Term(NodeTerm::False)),
            _ => err!("Invalid Term =>\n{tok:#?}"),
        }
    }

    fn parse_type(&mut self) -> Result<ParseType> {
        // let mut addr_mode = AddressingMode::Primitive;
        let mut depth: u32 = 0;
        let addr_mode = match self.peek(0) {
            Some(tok) if tok.kind == TokenKind::Ptr => {
                depth += 1;
                while self.expect(TokenKind::Ptr).is_ok() {
                    depth += 1;
                }
                AddressingMode::Pointer(depth)
            }
            Some(tok) if tok.kind == TokenKind::Array => {
                depth += 1;
                while self.expect(TokenKind::Array).is_ok() {
                    depth += 1;
                }
                AddressingMode::Array(depth)
            }
            Some(_) => AddressingMode::Primitive,
            None => return err!("No token to parse"),
        };

        let type_tok = self.expect(TokenKind::Ident)?;
        Ok(ParseType {
            type_tok,
            addr_mode,
        })
    }

    fn token_equals(&self, kind: TokenKind, offset: usize) -> anyhow::Result<()> {
        match self.peek(offset) {
            Some(tok) if tok.kind == kind => Ok(()),
            Some(tok) => err!("expected '{kind:?}', found => '{:?}'", tok.kind),
            None => err!("No token to evaluate"),
        }
    }

    fn peek(&self, offset: usize) -> Option<&Token> {
        self.tokens.get(self.idx + offset)
    }

    fn peek_mut(&mut self, offset: usize) -> Option<&mut Token> {
        self.tokens.get_mut(self.idx + offset)
    }

    fn consume(&mut self) -> Token {
        debug!("consuming: {:?}", self.peek(0).unwrap());
        match self.tokens.pop_front() {
            Some(tok) => {
                match self.peek(0) {
                    // peek "next" tok (just consumed so idx == 0)
                    Some(next) => utils::set_pos(next.pos),
                    None => utils::set_pos(tok.pos),
                }
                tok
            }
            None => err!("expected token to consume, found nothing.").unwrap(),
        }
    }

    fn expect(&mut self, kind: TokenKind) -> anyhow::Result<Token> {
        self.token_equals(kind, 0)?;
        Ok(self.consume())
    }
}

impl std::fmt::Debug for Ast {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        for stmt in &self.stmts {
            writeln!(f, "{stmt:#?},")?;
        }
        Ok(())
    }
}
