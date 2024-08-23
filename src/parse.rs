// >>PARSER<< Constructs statements out of tokens from the lexer.
//  PARSE_TYPE:
//      - to handle generic types, e.g Vec<u16>
use crate::{
    debug, debugln, err,
    lex::{Associativity, Token, TokenFlags, TokenKind},
    semantic::{SemFn, SemVariable},
    AddressingMode, ExprData, ExprForm, Type, TypeMode,
};
use std::collections::VecDeque;

const LOG_DEBUG_INFO: bool = false;
const MSG: &'static str = "PARSE";

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct AST {
    pub stmts: Vec<NodeStmt>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct NodeScope {
    pub stmts: Vec<NodeStmt>,
    pub inherits_stmts: bool,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Arg {
    pub mutable: bool,
    pub ident: Token,
    pub type_tok: Token,
    pub addr_mode: AddressingMode,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum NodeStmt {
    FnDecl {
        ident: Token,
        args: Vec<Arg>,
        scope: NodeScope,
        return_type_tok: Option<Token>,
        return_addr_mode: Option<AddressingMode>,
    },
    VarDecl {
        init_expr: Option<NodeExpr>,
        ident: Token,
        type_tok: Token,
        type_addr_mode: AddressingMode,
        mutable: bool,
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
    VarSemantics(SemVariable),
    FnSemantics {
        ident: Token,
    },
    ReturnSemantics {
        expr: Option<ExprData>,
    },
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
    pub pos: (u32, u32),
}

impl Parser {
    pub fn new(input: VecDeque<Token>) -> Parser {
        Parser {
            tokens: input,
            idx: 0,
            pos: (0, 0),
        }
    }

    pub fn parse_ast(&mut self) -> Result<AST, String> {
        let mut ast: AST = AST { stmts: Vec::new() };
        while self.peek(0).is_some() {
            ast.stmts.push(self.parse_func()?);
        }
        Ok(ast)
    }

    fn parse_func(&mut self) -> Result<NodeStmt, String> {
        match self.peek(0) {
            Some(tok) if tok.kind == TokenKind::Fn => {
                self.expect(TokenKind::Fn)?;
                let ident = self.expect(TokenKind::Ident)?;
                self.expect(TokenKind::OpenParen)?;

                let mut args = Vec::new();
                while self.token_equals(TokenKind::CloseParen, 0).is_err() {
                    if args.len() > 0 {
                        self.expect(TokenKind::Comma)?;
                    }

                    let mutable = self.expect(TokenKind::Mut).is_ok();
                    let ident = self.expect(TokenKind::Ident)?;
                    self.expect(TokenKind::Colon)?;
                    let (type_tok, addr_mode) = self.parse_type()?;
                    args.push(Arg {
                        mutable,
                        ident,
                        type_tok,
                        addr_mode,
                    });
                }
                self.expect(TokenKind::CloseParen)?;

                let mut return_type_tok = None;
                let mut return_addr_mode = None;
                if self.expect(TokenKind::Arrow).is_ok() {
                    let (tok, addr_mode) = self.parse_type()?;
                    return_type_tok = Some(tok);
                    return_addr_mode = Some(addr_mode);
                }
                let scope = self.parse_scope()?;

                Ok(NodeStmt::FnDecl {
                    ident,
                    args,
                    scope,
                    return_type_tok,
                    return_addr_mode,
                })
            }
            Some(tok) => {
                err!(
                    self,
                    "A Program only consists of functions, this is a {tok:?}"
                )
            }
            None => err!(self, "No function to parse"),
        }
    }

    fn parse_stmt(&mut self) -> Result<NodeStmt, String> {
        let tok = match self.peek(0) {
            Some(tok) => tok,
            None => return err!(self, "No statement to parse"),
        };
        debugln!(self, "parsing statement: {tok:?}");

        let stmt = match tok.kind {
            TokenKind::Let => {
                self.expect(TokenKind::Let)?;
                let mutable = self.expect(TokenKind::Mut).is_ok();
                let ident = self.expect(TokenKind::Ident)?;

                self.expect(TokenKind::Colon)?;
                let (type_tok, type_addr_mode) = self.parse_type()?;

                let init_expr = match self.expect(TokenKind::Eq) {
                    Ok(_) => Some(self.parse_expr(0)?),
                    Err(_) => None,
                };

                NodeStmt::VarDecl {
                    init_expr,
                    ident,
                    type_tok,
                    type_addr_mode,
                    mutable,
                }
            }
            TokenKind::If => {
                self.expect(TokenKind::If)?;
                let condition = self.parse_expr(0)?;
                let scope = self.parse_scope()?;

                let mut branches = Vec::new();
                loop {
                    if self.expect(TokenKind::Else).is_err() {
                        break;
                    } else if self.expect(TokenKind::If).is_ok() {
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
            TokenKind::Fn => {
                return err!(
                    self,
                    "Functions cannot be nested, they're top level statements"
                )
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
                let scope = self.parse_scope()?;
                NodeStmt::While { condition, scope }
            }
            TokenKind::Ident => {
                let ident = self.expect(TokenKind::Ident)?;
                match self.peek(0) {
                    // Assignment: consume ident & '='. parse expr.
                    Some(tok) if tok.kind == TokenKind::Eq => NodeStmt::Assign {
                        ident,
                        expr: self.parse_expr(0)?,
                    },
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
                    _ => return err!(self, "Naked Expression => '{:?}', Not Valid", self.peek(0)),
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
            TokenKind::OpenBrace => NodeStmt::NakedScope(self.parse_scope()?),
            _ => return err!(self, "Invalid Statement =>\n{tok:#?}"),
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

    fn parse_scope(&mut self) -> Result<NodeScope, String> {
        // consumes statements until a matching closebrace is found.
        self.expect(TokenKind::OpenBrace)?;
        let mut stmts = Vec::new();
        while self.expect(TokenKind::CloseBrace).is_err() {
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
                None => return err!(self, "No token to parse near =>\n{lhs:#?}"),
            };
            // unary expressions don't recurse as no rhs, only iterate so
            let bin_prec = op.get_prec_binary();
            let un_prec = op.get_prec_unary();

            // NOTE: tokens with no precedence are valued at -1, therefore always exit loop.
            // .. parse_expr escapes when it hits a semicolon because its prec is -1 !! thats unclear
            if bin_prec < min_prec && un_prec < min_prec {
                debug!(
                    self,
                    "precedence climb ended: {op:?}({bin_prec}) < {min_prec}"
                );
                break;
            }

            let is_unary = un_prec >= 0;
            if is_unary {
                let tok = match self.peek(1) {
                    Some(tok) => tok,
                    None => return err!(self, "No token to parse near =>\n{lhs:#?}"),
                };
                match tok.kind {
                    // tok is an expression, must be binary
                    TokenKind::IntLit | TokenKind::Ident | TokenKind::OpenParen => {
                        debug!(
                            self,
                            "found rhs of an expression '{tok:?}', operator must not be unary!"
                        )
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
                Associativity::None => return err!(self, "non-associative operator => '{op:?}'"),
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
    fn parse_term(&mut self) -> Result<NodeExpr, String> {
        let tok = match self.peek(0) {
            Some(_) => self.consume(),
            None => return err!(self, "Expected term, found nothing."),
        };

        match tok.kind {
            op @ _ if op.has_flags(TokenFlags::UNARY) => {
                debug!(self, "found unary expression: '{op:?}'");
                let operand = self.parse_expr(op.get_prec_unary() + 1)?;
                Ok(NodeExpr::UnaryExpr {
                    op,
                    operand: Box::new(operand),
                })
            }
            TokenKind::OpenParen => {
                // greedily consume everything in parenthesis.
                let expr = self.parse_expr(0)?;
                debug!(self, "parsed parens {expr:#?}");
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
                    None => err!(self, "Incomplete expression, nothing after =>\n{tok:#?}"),
                }
            }
            TokenKind::IntLit => Ok(NodeExpr::Term(NodeTerm::IntLit(tok))),
            TokenKind::True => Ok(NodeExpr::Term(NodeTerm::True)),
            TokenKind::False => Ok(NodeExpr::Term(NodeTerm::False)),
            _ => err!(self, "Invalid Term =>\n{tok:#?}"),
        }
    }

    fn parse_type(&mut self) -> Result<(Token, AddressingMode), String> {
        let mut addr_mode = AddressingMode::Primitive;
        if self.expect(TokenKind::Ptr).is_ok() {
            addr_mode = AddressingMode::Pointer;
        } else if self.expect(TokenKind::Array).is_ok() {
            addr_mode = AddressingMode::Array;
        }
        let type_ident = self.expect(TokenKind::Ident)?;
        Ok((type_ident, addr_mode))
    }

    fn token_equals(&self, kind: TokenKind, offset: usize) -> Result<(), String> {
        match self.peek(offset) {
            Some(tok) if tok.kind == kind => Ok(()),
            Some(tok) => err!(self, "expected '{kind:?}', found => '{:?}'", tok.kind),
            None => err!(self, "No token to evaluate"),
        }
    }

    fn peek(&self, offset: usize) -> Option<&Token> {
        self.tokens.get(self.idx + offset)
    }

    fn peek_mut(&mut self, offset: usize) -> Option<&mut Token> {
        self.tokens.get_mut(self.idx + offset)
    }

    fn consume(&mut self) -> Token {
        debug!(self, "consuming: {:?}", self.peek(0).unwrap());
        match self.tokens.pop_front() {
            Some(tok) => {
                self.pos = tok.pos;
                tok
            }
            None => err!(self, "expected token to consume, found nothing.").unwrap(),
        }
    }

    fn expect(&mut self, kind: TokenKind) -> Result<Token, String> {
        self.token_equals(kind, 0)?;
        Ok(self.consume())
    }
}
