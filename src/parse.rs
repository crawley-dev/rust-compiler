use crate::{
    debug, err,
    lex::{Associativity, Token, TokenFlags, TokenKind},
    semantic::{AddressingMode, Variable},
    utils::{self, pos, CompilerResult, Contents, Logger, Pos},
};
use anyhow::{Context, Error, Result};
use core::fmt;
use std::{collections::VecDeque, convert::Infallible};

#[derive(Debug, Clone)]
pub struct Arg {
    pub ident: Token,
    pub mutable: bool,
    pub parse_type: ParseType,
}

#[derive(Debug, Clone, Copy)]
pub struct ParseType {
    pub type_tok: Token,
    pub addr_mode: AddressingMode,
}

#[derive(Debug, Clone)]
pub enum InitExpr {
    Some(Node<Expr>),
    None,
    Deferred, // trust me bro, it exists.
}

#[derive(Clone)]
pub struct Ast {
    pub stmts: Vec<Node<Stmt>>,
}

#[derive(Debug, Clone)]
pub struct Scope {
    pub stmts: Vec<Node<Stmt>>,
    pub inherits_stmts: bool,
}
// Generic node wrapper to add extra info
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Node<T: std::fmt::Debug> {
    pub start: Pos,
    pub end: Pos,
    pub node: T,
}

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

#[derive(Debug, Clone)]
pub enum Stmt {
    FnDecl {
        ident: Token,
        args: Vec<Arg>,
        scope: Node<Scope>,
        return_type: Option<ParseType>,
    },
    VarDecl {
        init_expr: InitExpr,
        arg: Arg,
    },
    If {
        condition: Node<Expr>,
        scope: Node<Scope>,
        branches: Vec<Node<Stmt>>,
    },
    ElseIf {
        condition: Node<Expr>,
        scope: Node<Scope>,
    },
    Else(Node<Scope>),
    While {
        condition: Node<Expr>,
        scope: Node<Scope>,
    },
    Assign {
        ident: Token,
        expr: Node<Expr>,
    },
    Exit(Node<Expr>),
    NakedScope(Node<Scope>),
    Break,
    Return(Option<Node<Expr>>),
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
pub enum Expr {
    BinaryExpr {
        op: TokenKind,
        lhs: Box<Node<Expr>>,
        rhs: Box<Node<Expr>>,
    },
    UnaryExpr {
        op: TokenKind,
        operand: Box<Node<Expr>>,
    },
    // It contains itself, so Node<Expr::Term> is ~ Node<Term>
    // But still have to pass an extra Node<> Wrapper w(ﾟДﾟ)w
    Term(Node<Term>),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Term {
    True,
    False,
    Ident,
    IntLit,
    FnCall { ident: Token, args: Vec<Node<Expr>> },
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
                CompilerResult::Ok(stmt) => ast.stmts.push(stmt),
                CompilerResult::Err {
                    data: Some(data),
                    error,
                } => {
                    ast.stmts.push(data);
                    return (ast, Some(error));
                }
                CompilerResult::Err { data: None, error } => return (ast, Some(error)),
            };
        }
        (ast, None)
    }

    fn parse_top_level(&mut self) -> CompilerResult<Node<Stmt>> {
        let fn_keyword = self.expect(TokenKind::Fn)?;

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
        let scope = self
            .parse_scope(false)
            .context("Failed to parse function body")?;
        // let scope = self.parse_scope(false).context("Cannot Parse Function scope")?;

        CompilerResult::Ok(Node {
            start: fn_keyword.start,
            end: scope.end,
            node: Stmt::FnDecl {
                ident,
                args,
                scope,
                return_type,
            },
        })
    }

    fn parse_stmt(&mut self) -> Result<Node<Stmt>> {
        let kind = match self.peek(0) {
            Some(tok) => {
                debug!("\n\nparsing statement: {tok:?}");
                tok.kind
            } // cannot consume here,
            None => return err!("No statement to parse"),
        };

        // let tok = self.consume();
        let stmt = match kind {
            TokenKind::Let => {
                let let_tok = self.expect(TokenKind::Let)?;
                let mutable = self.expect(TokenKind::Mut).is_ok();
                let ident = self.expect(TokenKind::Ident)?;

                self.expect(TokenKind::Colon)?;
                let parse_type = self.parse_type()?;

                let init_expr = match self.expect(TokenKind::Eq) {
                    Ok(_) => InitExpr::Some(self.parse_expr(0)?),
                    Err(_) => InitExpr::None,
                };

                let end = match init_expr {
                    InitExpr::Some(ref expr) => expr.end,
                    _ => parse_type.type_tok.end_pos(),
                };
                Node {
                    start: let_tok.start,
                    end,
                    node: Stmt::VarDecl {
                        init_expr,
                        arg: Arg {
                            ident,
                            mutable,
                            parse_type,
                        },
                    },
                }
            }
            TokenKind::If => {
                self.expect(TokenKind::If)?;
                let condition = self.parse_expr(0)?;
                let scope = self.parse_scope(true)?;

                let mut branches = Vec::new();
                loop {
                    // no branches left, exit loop
                    if self.expect(TokenKind::Else).is_err() {
                        break;
                        // Found an else if, parse condition & scope, push to branches
                    } else if self.expect(TokenKind::If).is_ok() {
                        let condition = self.parse_expr(0)?;
                        let scope = self.parse_scope(true)?;
                        branches.push(Node {
                            start: condition.start,
                            end: scope.end,
                            node: Stmt::ElseIf { condition, scope },
                        });
                        continue;
                    }

                    // Found an else, parse scope, push to branches
                    let condition = self.parse_expr(0)?;
                    let scope = self.parse_scope(true)?;

                    branches.push(Node {
                        start: condition.start,
                        end: scope.end,
                        node: Stmt::Else(self.parse_scope(true)?),
                    });
                    break;
                }

                let start = condition.start;
                let end = match branches.last() {
                    Some(branch) => branch.end,
                    None => scope.end,
                };
                Node {
                    start,
                    end,
                    node: Stmt::If {
                        condition,
                        scope,
                        branches,
                    },
                }
            }
            TokenKind::Fn => {
                return err!("Functions cannot be nested, they're top level statements")
            }
            TokenKind::Return => {
                let tok = self.expect(TokenKind::Return)?;
                match self.peek(0) {
                    Some(tok) if tok.kind == TokenKind::SemiColon => Node {
                        start: tok.start,
                        end: tok.end_pos(),
                        node: Stmt::Return(None),
                    },
                    _ => Node {
                        start: tok.start,
                        end: tok.end_pos(),
                        node: Stmt::Return(Some(self.parse_expr(0)?)),
                    },
                }
            }
            TokenKind::While => {
                let tok = self.expect(TokenKind::While)?;
                let condition = self.parse_expr(0)?;
                let scope = self.parse_scope(true)?;
                Node {
                    start: tok.start,
                    end: scope.end,
                    node: Stmt::While { condition, scope },
                }
            }
            TokenKind::Ident => {
                // let ident = self.expect(TokenKind::Ident)?;
                match self.peek(1) {
                    // Assignment: consume ident & '='. parse expr.
                    Some(tok) if tok.kind == TokenKind::Eq => {
                        let ident = self.expect(TokenKind::Ident)?;
                        self.expect(TokenKind::Eq)?;
                        let expr = self.parse_expr(0)?;
                        Node {
                            start: ident.start,
                            end: expr.end,
                            node: Stmt::Assign { ident, expr },
                        }
                    }
                    // Compound Assign: clone ident, swap assign to arith counterpart, parse expr
                    //      - 'ident += 5;' => 'ident = ident + 5;'
                    Some(tok) if tok.kind.has_flags(TokenFlags::ASSIGN) => {
                        let ident = self.peek(0).copied().unwrap();

                        let assign = self.peek_mut(1).unwrap();
                        assign.kind = assign.kind.assign_to_arithmetic()?;
                        assign.start = pos(assign.start.x - 1, assign.start.y);
                        assign.len = 1;

                        let expr = self.parse_expr(0)?;

                        Node {
                            start: ident.start,
                            end: expr.end,
                            node: Stmt::Assign { ident, expr },
                        }
                    }
                    _ => return err!("Naked Expression => '{:?}', Not Valid", self.peek(0)),
                }
            }
            TokenKind::Exit => {
                let tok = self.expect(TokenKind::Exit)?;
                // TODO(TOM): feels like this will break lol.
                todo!("panic! trying to parse exit");
                self.token_equals(TokenKind::OpenParen, 0)?;
                let expr = self.parse_expr(0)?;
                Node {
                    start: tok.start,
                    end: expr.end,
                    node: Stmt::Exit(expr),
                }
            }
            TokenKind::Break => {
                let tok = self.expect(TokenKind::Break)?;
                Node {
                    start: tok.start,
                    end: tok.end_pos(),
                    node: Stmt::Break,
                }
            }
            TokenKind::OpenBrace => {
                let tok = self.expect(TokenKind::OpenBrace)?;
                let scope = self.parse_scope(true)?;
                Node {
                    start: tok.start,
                    end: scope.end,
                    node: Stmt::NakedScope(scope),
                }
            }
            _ => return err!("Invalid Statement =>\n{:#?}", self.tokens.front()),
        };

        // statments that require a ';' to end.
        match stmt.node {
            Stmt::Exit(_)
            | Stmt::Assign { .. }
            | Stmt::VarDecl { .. }
            | Stmt::Break
            | Stmt::Return(_) => match self.expect(TokenKind::SemiColon) {
                Ok(_) => Ok(stmt),
                Err(e) => err!("{e}.\n{stmt:#?}"),
            },
            _ => Ok(stmt),
        }
    }

    fn parse_scope(&mut self, inherits_stmts: bool) -> Result<Node<Scope>> {
        // consumes statements until a closebrace is found.
        let mut error = None;
        let open_brace = self.expect(TokenKind::OpenBrace)?;

        let mut stmts = Vec::new();
        while self.expect(TokenKind::CloseBrace).is_err() {
            match self.parse_stmt() {
                Ok(stmt) => stmts.push(stmt),
                Err(e) => {
                    error = Some(e);
                    break;
                }
            }
        }

        let end = match stmts.last() {
            Some(stmt) => stmt.end,
            None => open_brace.end_pos(),
        };

        Ok(Node {
            start: open_brace.start,
            end,
            node: Scope {
                stmts,
                inherits_stmts,
            },
        })
    }

    fn parse_expr(&mut self, min_prec: i32) -> Result<Node<Expr>> {
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
                        // TODO(TOM): start,end dependent on whether its a lhs or rhs operator.
                        // e.g. "array[i]" or "&array"
                        lhs = Node {
                            start: lhs.start,
                            end: tok.end_pos(),
                            node: Expr::UnaryExpr {
                                op: self.consume().kind,
                                operand: Box::new(lhs),
                            },
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

            let op = self.consume().kind;
            let rhs = self.parse_expr(next_prec)?;
            lhs = Node {
                start: lhs.start,
                end: rhs.end,
                node: Expr::BinaryExpr {
                    op,
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                },
            };
        }
        Ok(lhs)
    }

    // peeking next token might not work because it could be a close paren?
    fn parse_term(&mut self) -> Result<Node<Expr>> {
        let tok = match self.peek(0) {
            Some(_) => self.consume(),
            None => return err!("Expected term, found nothing."),
        };

        match tok.kind {
            op @ _ if op.has_flags(TokenFlags::UNARY) => {
                debug!("found unary expression: '{op:?}'");
                let operand = self.parse_expr(op.get_prec_unary() + 1)?;
                Ok(Node {
                    start: tok.start,
                    end: operand.end,
                    node: Expr::UnaryExpr {
                        op,
                        operand: Box::new(operand),
                    },
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

                        Ok(Node {
                            start: tok.start,
                            end: args.last().unwrap().end,
                            node: Expr::Term(Node {
                                start: tok.start,
                                end: args.last().unwrap().end,
                                node: Term::FnCall { ident: tok, args },
                            }),
                        })
                    }
                    // Just an Ident
                    Some(_) => Ok(Node {
                        start: tok.start,
                        end: tok.end_pos(),
                        node: Expr::Term(Node {
                            start: tok.start,
                            end: tok.end_pos(),
                            node: Term::Ident,
                        }),
                    }),
                    None => err!("Incomplete expression, nothing after =>\n{tok:#?}"),
                }
            }
            TokenKind::IntLit => Ok(Node {
                start: tok.start,
                end: tok.end_pos(),
                node: Expr::Term(Node {
                    start: tok.start,
                    end: tok.end_pos(),
                    node: Term::IntLit,
                }),
            }),
            TokenKind::True => Ok(Node {
                start: tok.start,
                end: tok.end_pos(),
                node: Expr::Term(Node {
                    start: tok.start,
                    end: tok.end_pos(),
                    node: Term::True,
                }),
            }),
            TokenKind::False => Ok(Node {
                start: tok.start,
                end: tok.end_pos(),
                node: Expr::Term(Node {
                    start: tok.start,
                    end: tok.end_pos(),
                    node: Term::False,
                }),
            }),
            _ => err!("Invalid Term =>\n{tok:#?}"),
        }
    }

    fn parse_type(&mut self) -> Result<ParseType> {
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

    fn expect(&mut self, kind: TokenKind) -> Result<Token> {
        self.token_equals(kind, 0)?;
        Ok(self.consume())
    }

    fn token_equals(&self, kind: TokenKind, offset: usize) -> Result<()> {
        match self.peek(offset) {
            Some(tok) if tok.kind == kind => Ok(()),
            Some(tok) => err!("expected '{kind:?}', found => '{:?}'", tok.kind),
            None => err!("No token to evaluate"),
        }
    }

    fn consume(&mut self) -> Token {
        debug!("consuming: {:?}", self.peek(0).unwrap());
        match self.tokens.pop_front() {
            Some(tok) => {
                match self.peek(0) {
                    // peek "next" tok (just consumed so next has offset == 0)
                    Some(next) => Logger::set_pos(next.start),
                    None => Logger::set_pos(tok.start),
                }
                tok
            }
            None => {
                let err: Result<Infallible> = err!("expected token to consume, found nothing.");
                panic!("{err:?}")
            }
        }
    }

    fn peek(&self, offset: usize) -> Option<&Token> {
        self.tokens.get(self.idx + offset)
    }

    fn peek_mut(&mut self, offset: usize) -> Option<&mut Token> {
        self.tokens.get_mut(self.idx + offset)
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

impl<T: std::fmt::Debug> std::fmt::Debug for Node<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let node_name = std::any::type_name::<T>().split("::").last().unwrap_or("");
        f.debug_struct(&format!("Node<{}>", node_name))
            .field("start", &self.start)
            .field("end", &self.end)
            .field("node", &self.node)
            .finish()
    }
}
