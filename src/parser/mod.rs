pub mod parse_type;
use parse_type::*;

use crate::{
    checker::{AddressingMode, Variable},
    comp_err, debug, err,
    lexer::{Associativity, Token, TokenFlags, TokenKind},
    upgrade_err, upgrade_result,
    utils::{self, pos, CompilerResult, Contents, Logger, Pos},
};
use anyhow::{Context, Error, Result};
use core::fmt;
use educe::Educe;
use std::{collections::VecDeque, convert::Infallible, fmt::Formatter};

pub const DEFAULT_DEPTH: usize = 1;

// region: Type Definitions
#[derive(Debug, Clone)]
pub enum InitExpr {
    Some(Node<Expr>),
    None,
    Deferred, // trust me bro, it exists.
}

#[derive(Debug, Clone)]
pub struct Arg {
    pub ident: Token,
    pub mutable: bool,
    pub parse_type: ParseType,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct StructField {
    pub ident: Token,
    pub expr: Node<Expr>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Term {
    True,
    False,
    Ident,
    IntLit,
    StructLit {
        ident: Token,
        fields: Vec<StructField>,
    },
    ArrayLit {
        elements: Vec<Node<Expr>>,
    },
    FnCall {
        ident: Token,
        args: Vec<Node<Expr>>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Expr {
    Binary {
        op: TokenKind,
        lhs: Box<Node<Expr>>,
        rhs: Box<Node<Expr>>,
    },
    Unary {
        op: TokenKind,
        expr: Box<Node<Expr>>,
    },
    Term(Term),
}

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
    NakedScope(Node<Scope>),
    NakedExpr(Node<Expr>),
    Break,
    Return(Option<Node<Expr>>),
    TypeAlias {
        ident: Token,
        parse_type: ParseType,
    },
    StructDecl {
        ident: Token,
        fields: Vec<Arg>,
    },
    // SEMANTIC STMTs
    VarSemantics(Variable),
    FnSemantics {
        id: usize,
    },
}

// Generic node wrapper to add extra info
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Node<T: std::fmt::Debug> {
    pub start: Pos,
    pub end: Pos,
    pub node: T,
}

#[derive(Debug, Clone)]
pub struct Scope {
    pub stmts: Vec<Node<Stmt>>,
    pub inherits_stmts: bool,
}

#[derive(Clone)]
pub struct Ast {
    pub stmts: Vec<Node<Stmt>>,
}

pub struct Parser {
    pub tokens: VecDeque<Token>,
    pub idx: usize,
}

// endregion

impl Parser {
    pub fn new(tokens: VecDeque<Token>) -> Self {
        Self { tokens, idx: 0 }
    }

    pub fn parse_tokens(mut self) -> CompilerResult<Ast, Error> {
        let mut ast: Ast = Ast { stmts: Vec::new() };

        while self.peek(0).is_some() {
            match self.parse_top_level() {
                CompilerResult::Ok(stmt) => ast.stmts.push(stmt),
                CompilerResult::Err {
                    data: Some(data),
                    error,
                } => {
                    ast.stmts.push(data);
                    return CompilerResult::Err {
                        data: Some(ast),
                        error,
                    };
                }
                CompilerResult::Err { data: None, error } => {
                    return CompilerResult::Err {
                        data: Some(ast),
                        error,
                    }
                }
            };
        }
        CompilerResult::Ok(ast)
    }

    // region: Top Level
    fn parse_top_level(&mut self) -> CompilerResult<Node<Stmt>> {
        match self.peek(0) {
            Some(tok) if tok.kind == TokenKind::Fn => self.parse_fn_decl(),
            Some(tok) if tok.kind == TokenKind::Type => self.parse_type_alias(),
            Some(tok) if tok.kind == TokenKind::Struct => self.parse_struct_decl(),
            Some(tok) => comp_err!("Invalid Top Level Token => '{tok:?}'"),
            None => comp_err!("No token to parse"),
        }
    }

    fn parse_fn_decl(&mut self) -> CompilerResult<Node<Stmt>> {
        let fn_keyword = self.expect(TokenKind::Fn)?;

        let ident = self.expect(TokenKind::Ident)?;
        self.expect(TokenKind::OpenParen)?;

        // parsing function arguments
        let args = self
            .parse_fn_args()
            .with_context(|| "failed to parse function arguments")?;

        // parse function return type
        let return_type = match self.expect(TokenKind::Arrow) {
            Ok(_) => Some(
                self.parse_type()
                    .with_context(|| "faield to parse function return type")?,
            ),
            Err(_) => None,
        };

        // Parse the scope, if it fails, return the partially parsed function
        let scope = upgrade_err!(self.parse_scope(false), |scope| Node {
            start: fn_keyword.start,
            end: scope.end,
            node: Stmt::FnDecl {
                ident,
                args,
                scope,
                return_type,
            },
        });

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

    fn parse_fn_args(&mut self) -> Result<Vec<Arg>> {
        let mut args = Vec::new();
        while self.token_equals(TokenKind::CloseParen, 0).is_err() {
            if !args.is_empty() {
                self.expect(TokenKind::Comma)?;
            }

            let mutable = self.expect(TokenKind::Mut).is_ok();
            let ident = self.expect(TokenKind::Ident)?;
            self.expect(TokenKind::Colon)?;
            let parse_type = self
                .parse_type()
                .with_context(|| "failed to parse type for function argument")?;

            args.push(Arg {
                ident,
                mutable,
                parse_type,
            });
        }
        self.expect(TokenKind::CloseParen)?;

        return Ok(args);
    }

    fn parse_type_alias(&mut self) -> CompilerResult<Node<Stmt>> {
        let type_keyword = self.expect(TokenKind::Type)?;
        let new_type = self.expect(TokenKind::Ident)?;

        self.expect(TokenKind::Eq)?;

        let parse_type = self
            .parse_type()
            .with_context(|| "failed to parse type for type alias")?;

        // expect a semicolon to end the type alias statement.
        self.expect(TokenKind::SemiColon)?;

        CompilerResult::Ok(Node {
            start: type_keyword.start,
            end: parse_type.get_ident().end_pos(),
            node: Stmt::TypeAlias {
                ident: new_type,
                parse_type,
            },
        })
    }

    fn parse_struct_decl(&mut self) -> CompilerResult<Node<Stmt>> {
        let struct_keyword = self.expect(TokenKind::Struct)?;
        let ident = self.expect(TokenKind::Ident)?;

        self.expect(TokenKind::OpenBrace)?;
        let mut fields = Vec::new();
        while self.token_equals(TokenKind::CloseBrace, 0).is_err() {
            // do this first in next iter as it may be the last field.
            if !fields.is_empty() {
                self.expect(TokenKind::Comma)?;
            }

            let ident = self.expect(TokenKind::Ident)?;
            self.expect(TokenKind::Colon)?;
            let parse_type = self
                .parse_type()
                .with_context(|| "failed to parse type for struct field")?;

            fields.push(Arg {
                ident,
                mutable: false,
                parse_type,
            });

            // TODO(TOM): nested type declarations
            // let next_token = match self.peek(0) {
            //     Some(tok) => tok,
            //     None => return comp_err!("No token to parse for struct field"),
            // };
            // match next_token.kind {
            //     TokenKind::Struct => {}
            //     TokenKind::Ident => {}
            //     _ => return comp_err!("Invalid token found for struct field => '{next_token:?}'"),
            // }
        }
        let close_brace = self.expect(TokenKind::CloseBrace)?;

        CompilerResult::Ok(Node {
            start: struct_keyword.start,
            end: close_brace.end_pos(),
            node: Stmt::StructDecl { ident, fields },
        })
    }
    // endregion

    fn parse_scope(&mut self, inherits_stmts: bool) -> CompilerResult<Node<Scope>> {
        let open_brace = self.expect(TokenKind::OpenBrace)?;

        // go through each statement, if it fails. return the partially complete scope.
        let mut stmts = Vec::new();
        while self.token_equals(TokenKind::CloseBrace, 0).is_err() {
            match self.parse_stmt() {
                CompilerResult::Ok(stmt) => stmts.push(stmt),
                CompilerResult::Err { data, error } => {
                    if let Some(data) = data {
                        stmts.push(data);
                    }
                    let end = match stmts.last() {
                        Some(stmt) => stmt.end,
                        None => open_brace.end_pos(),
                    };
                    return CompilerResult::Err {
                        data: Some(Node {
                            start: open_brace.start,
                            end,
                            node: Scope {
                                stmts,
                                inherits_stmts,
                            },
                        }),
                        error,
                    }
                    .with_context(|| "failed to parse scope");
                }
            }
        }
        let close_brace = self.expect(TokenKind::CloseBrace)?;

        CompilerResult::Ok(Node {
            start: open_brace.start,
            end: close_brace.end_pos(),
            node: Scope {
                stmts,
                inherits_stmts,
            },
        })
    }

    fn parse_stmt(&mut self) -> CompilerResult<Node<Stmt>> {
        let kind = match self.peek(0) {
            Some(tok) => {
                debug!("parsing statement: {tok:?}");
                tok.kind
            } // cannot consume here,
            None => return comp_err!("No statement to parse"),
        };

        let stmt = match kind {
            TokenKind::Let => self
                .parse_var_decl()
                .with_context(|| "failed to parse variable declaration")?,
            TokenKind::If => self
                .parse_if()
                .with_context(|| "failed to parse if statement")?,
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
                        node: Stmt::Return(Some(
                            self.parse_expr(0)
                                .with_context(|| "failed to parse return expression")?,
                        )),
                    },
                }
            }
            TokenKind::While => {
                let tok = self.expect(TokenKind::While)?;
                let condition = self
                    .parse_expr(0)
                    .with_context(|| "failed to parse while condition")?;

                upgrade_result!(self.parse_scope(true), |scope| Node {
                    start: tok.start,
                    end: scope.end,
                    node: Stmt::While { condition, scope },
                })?
            }
            TokenKind::Ident => {
                match self.peek(1) {
                    // Assignment: consume ident & '='. parse expr.
                    Some(tok) if tok.kind == TokenKind::Eq => {
                        let ident = self.expect(TokenKind::Ident)?;
                        self.expect(TokenKind::Eq)?;
                        let expr = self
                            .parse_expr(0)
                            .with_context(|| "failed to parse assignment expression")?;
                        Node {
                            start: ident.start,
                            end: expr.end,
                            node: Stmt::Assign { ident, expr },
                        }
                    }
                    // Compound Assign: clone ident, swap assign to arith counterpart, parse expr
                    //      - 'ident += 5;' => 'ident = ident + 5;'
                    Some(tok) if tok.kind.has_flags_binary(TokenFlags::ASSIGN) => {
                        let ident = self.peek(0).copied().unwrap();

                        let assign = self.peek_mut(1).unwrap();
                        assign.kind = assign.kind.assign_to_arithmetic()?;
                        assign.start = pos(assign.start.x - 1, assign.start.y);
                        assign.len = 1;

                        let expr = self
                            .parse_expr(0)
                            .with_context(|| "failed to parse compound assign expression")?;

                        Node {
                            start: ident.start,
                            end: expr.end,
                            node: Stmt::Assign { ident, expr },
                        }
                    }
                    _ => {
                        let expr = self
                            .parse_expr(0)
                            .with_context(|| "failed to parse naked expression")?;
                        Node {
                            start: expr.start,
                            end: expr.end,
                            node: Stmt::NakedExpr(expr),
                        }
                    }
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
                upgrade_result!(self.parse_scope(true), |scope| Node {
                    start: tok.start,
                    end: scope.end,
                    node: Stmt::NakedScope(scope),
                })?
            }
            TokenKind::Fn => {
                return comp_err!("Functions cannot be nested, they're top level statements")
            }
            _ => return comp_err!("Invalid Statement =>\n{:#?}", self.tokens.front()),
        };

        // statments that require a ';' to end.
        match stmt.node {
            Stmt::Assign { .. }
            | Stmt::VarDecl { .. }
            | Stmt::Break
            | Stmt::Return(_)
            | Stmt::NakedExpr(_) => match self.expect(TokenKind::SemiColon) {
                Ok(_) => CompilerResult::Ok(stmt),
                Err(e) => comp_err!((stmt), "Expected ';' to end statement\n{e}"),
            },
            _ => CompilerResult::Ok(stmt),
        }
    }

    fn parse_var_decl(&mut self) -> Result<Node<Stmt>> {
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
            _ => parse_type.get_ident().end_pos(),
        };

        Ok(Node {
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
        })
    }

    fn parse_if(&mut self) -> CompilerResult<Node<Stmt>> {
        let if_tok = self.expect(TokenKind::If)?;
        let condition = self
            .parse_expr(0)
            .with_context(|| "failed to parse if statement's condition")?;
        let scope = upgrade_err!(self.parse_scope(true), |scope| Node {
            start: if_tok.start,
            end: scope.end,
            node: Stmt::If {
                condition,
                scope,
                branches: Vec::new(),
            },
        });

        let mut branches = Vec::new();
        loop {
            // no branches left, exit loop
            if self.expect(TokenKind::Else).is_err() {
                break;
            }
            // Found an else if, parse condition & scope, push to branches
            if self.expect(TokenKind::If).is_ok() {
                let condition = self
                    .parse_expr(0)
                    .with_context(|| "failed to parse else if statement's condition")?;

                let upgraded_stmt = upgrade_result!(self.parse_scope(true), |scope| Node {
                    start: if_tok.start,
                    end: scope.end,
                    node: Stmt::ElseIf { condition, scope },
                })?;

                branches.push(upgraded_stmt);
                continue;
            }

            // Found an else, parse scope, push to branches
            let upgraded_stmt = upgrade_result!(self.parse_scope(true), |scope| Node {
                start: if_tok.start,
                end: scope.end,
                node: Stmt::Else(scope),
            })?;

            branches.push(upgraded_stmt);
            break; // found an else, this is the last branch
        }

        let end = match branches.last() {
            Some(branch) => branch.end,
            None => scope.end,
        };

        CompilerResult::Ok(Node {
            start: condition.start,
            end,
            node: Stmt::If {
                condition,
                scope,
                branches,
            },
        })
    }

    fn parse_expr(&mut self, min_prec: i32) -> Result<Node<Expr>> {
        debug!("parsing expression with min_prec: {min_prec}");
        let mut lhs = self
            .parse_term()
            .with_context(|| "failed to parse lhs of the expression")?;

        loop {
            let op = match self.peek(0) {
                Some(tok) => &tok.kind,
                None => return err!("No token to parse for the expression rhs =>\n{lhs:#?}"),
            };

            debug!("peeked at op: {op:?}");

            let un_prec = op.get_prec_unary();
            let bin_prec = op.get_prec_binary();

            debug!(
                "checking if {:?} is a valid unary op: {bin_prec:?}, {un_prec:?}, {}",
                self.peek(0).unwrap(),
                op.has_flags_unary(TokenFlags::LHS),
            );

            // if unary is the only valid operator, and its lhs. its not valid here, so lets return a nice error msg.
            if bin_prec < 0 && op.has_flags_unary(TokenFlags::LHS) {
                return err!("{op:?} is a unary operator for the left, it cannot be used to the right of an expression.\n{lhs:#?}");
            }

            // Found a RHS unary operator, e.g. deref pointer.
            if un_prec >= 0 && !op.has_flags_unary(TokenFlags::LHS) {
                let op_token = self.consume();
                lhs = Node {
                    start: lhs.start,
                    end: op_token.end_pos(),
                    node: Expr::Unary {
                        op: op_token.kind,
                        expr: Box::new(lhs),
                    },
                };
                continue; // there might be more unary operators, e.g. "i^^ + 5"
            }

            // NOTE: tokens with no precedence are valued as negative, therefore they always exit the loop.
            // .. parse_expr escapes when it hits a semicolon because its prec is -1 !! thats unclear
            if bin_prec < min_prec {
                debug!("precedence climb ended: {op:?}({bin_prec}) < {min_prec}");
                break;
            }

            let next_prec = match op.get_associativity(false) {
                // the op to the right is the first item, so will have the lowest precedence, so lower the prec.
                Associativity::Right => bin_prec,
                Associativity::Left => bin_prec + 1, // make this bigger, not right smaller. so its a left leaning tree.
            };

            let op = self.consume().kind;

            let rhs = self
                .parse_expr(next_prec)
                .with_context(|| "failed to parse rhs of the expression")?;

            lhs = Node {
                start: lhs.start,
                end: rhs.end,
                node: Expr::Binary {
                    op,
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                },
            };
        }
        Ok(lhs)
    }

    fn parse_term(&mut self) -> Result<Node<Expr>> {
        let tok = match self.peek(0) {
            Some(_) => self.consume(),
            None => return err!("Expected term, found nothing."),
        };

        match tok.kind {
            // Unary Expressions
            op @ _ if op.has_flags_unary(TokenFlags::LHS) => {
                debug!("found unary expression: '{op:?}'");
                let expr = self
                    .parse_expr(op.get_prec_unary() + 1)
                    .with_context(|| "failed to parse unary term")?;
                Ok(Node {
                    start: tok.start,
                    end: expr.end,
                    node: Expr::Unary {
                        op,
                        expr: Box::new(expr),
                    },
                })
            }
            // Parenthesized Expressions
            TokenKind::OpenParen => {
                // greedily consume everything in parenthesis.
                let expr = self
                    .parse_expr(0)
                    .with_context(|| "failed to parse parentheses term")?;
                debug!("parsed parens {expr:#?}");
                self.expect(TokenKind::CloseParen)?;
                Ok(expr)
            }
            // Array Literals
            TokenKind::OpenBracket => {
                let mut elements = Vec::new();
                while self.token_equals(TokenKind::CloseBracket, 0).is_err() {
                    if !elements.is_empty() {
                        self.expect(TokenKind::Comma)?;
                    }
                    elements.push(
                        self.parse_expr(0)
                            .with_context(|| "failed to parse array literal element")?,
                    );
                }

                Ok(Node {
                    start: tok.start,
                    end: self.expect(TokenKind::CloseBracket)?.end_pos(),
                    node: Expr::Term(Term::ArrayLit { elements }),
                })
            }
            TokenKind::Ident => {
                match self.peek(0) {
                    // Function Calls
                    Some(Token {
                        kind: TokenKind::OpenParen,
                        ..
                    }) => {
                        self.expect(TokenKind::OpenParen)?;

                        let mut args = Vec::new();
                        while self.token_equals(TokenKind::CloseParen, 0).is_err() {
                            if !args.is_empty() {
                                self.expect(TokenKind::Comma)?;
                            }
                            args.push(
                                self.parse_expr(0)
                                    .with_context(|| "failed to parse function call argument")?,
                            );
                        }
                        let close_paren = self.expect(TokenKind::CloseParen)?;

                        Ok(Node {
                            start: tok.start,
                            end: close_paren.end_pos(),
                            node: Expr::Term(Term::FnCall { ident: tok, args }),
                        })
                    }
                    // Struct Literals
                    Some(Token {
                        kind: TokenKind::OpenBrace,
                        ..
                    }) => {
                        self.expect(TokenKind::OpenBrace)?;

                        let mut fields = Vec::new();
                        while self.token_equals(TokenKind::CloseBrace, 0).is_err() {
                            if !fields.is_empty() {
                                self.expect(TokenKind::Comma)?;
                            }

                            let ident = self.expect(TokenKind::Ident)?;
                            self.expect(TokenKind::Colon)?;

                            fields.push(StructField {
                                ident,
                                expr: self
                                    .parse_expr(0)
                                    .with_context(|| "failed to parse struct literal field")?,
                            })
                        }
                        let close_brace = self.expect(TokenKind::CloseBrace)?;

                        Ok(Node {
                            start: tok.start,
                            end: close_brace.end_pos(),
                            node: Expr::Term(Term::StructLit { ident: tok, fields }),
                        })
                    }
                    Some(_) => Ok(Node {
                        start: tok.start,
                        end: tok.end_pos(),
                        node: Expr::Term(Term::Ident),
                    }),
                    None => err!("Incomplete expression, nothing after =>\n{tok:#?}"),
                }
            }
            TokenKind::IntLit => Ok(Node {
                start: tok.start,
                end: tok.end_pos(),
                node: Expr::Term(Term::IntLit),
            }),
            TokenKind::True => Ok(Node {
                start: tok.start,
                end: tok.end_pos(),
                node: Expr::Term(Term::True),
            }),
            TokenKind::False => Ok(Node {
                start: tok.start,
                end: tok.end_pos(),
                node: Expr::Term(Term::False),
            }),
            _ => err!("Invalid Term =>\n{tok:#?}"),
        }
    }

    fn parse_type(&mut self) -> Result<ParseType> {
        self.internal_parse_type(DEFAULT_DEPTH)
            .with_context(|| "failed to parse type parameter")
    }

    // this function will attempt to parse a type parameter.
    // - this can be for a function argument, return type or a variable declaration.
    // - the type will be one of: primitive, pointer, array.
    // - the type can be nested, e.g. `ptr ptr array[5] ident`
    fn internal_parse_type(&mut self, depth: usize) -> Result<ParseType> {
        let mut inner_type = None;
        let addr_mode = match self.peek(0) {
            Some(tok) if tok.kind == TokenKind::Ptr => {
                self.consume(); // consume the ptr token
                inner_type = Some(InnerType::Nested {
                    inner: Box::new(self.internal_parse_type(depth + 1)?),
                });

                AddressingMode::Pointer { depth }
            }
            Some(tok) if tok.kind == TokenKind::OpenBracket => {
                self.consume(); // consume the array open token
                inner_type = Some(InnerType::Nested {
                    inner: Box::new(self.internal_parse_type(depth + 1)?),
                });

                self.expect(TokenKind::SemiColon)
                    .with_context(|| "Expected ';' after array type")?;

                let len = match self.peek(0) {
                    Some(tok) if tok.kind == TokenKind::IntLit => {
                        let len_tok = self.consume(); // consume the int literal token
                        len_tok.str().parse::<usize>().with_context(|| {
                            "Invalid array length, expected a valid integer literal"
                        })?
                    }
                    _ => {
                        return err!(
                            "Expected an integer literal for array length, found {:?}",
                            self.peek(0)
                        )
                    }
                };

                self.expect(TokenKind::CloseBracket)
                    .with_context(|| "Expected ']' to close array type")?;

                AddressingMode::Array { depth, len }
            }
            Some(tok) if tok.kind == TokenKind::Ident => {
                inner_type = Some(InnerType::Primitive {
                    ident: self.consume(),
                });

                AddressingMode::Primitive
            }

            None => return err!("No token to parse"),
            _ => return err!("Invalid type parameter, expected 'ptr', 'array', or 'ident'"),
        };

        Ok(ParseType {
            inner_type: inner_type
                .with_context(|| "failed to parse inner type, its none for some reason??")?,
            addr_mode,
        })
    }

    // region: little ones
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
    // endregion
}
