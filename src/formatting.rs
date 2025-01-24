use crate::{
    utils::{pos, Contents, Pos},
    Ast, Expr, Lexer, Node, Scope, Stmt, Term, Token, TokenKind,
};
use std::{
    cmp::max,
    fmt::{self, Debug, Display, Formatter},
};

const PRINT_TERM_POS: bool = false;

// region: lex.rs
impl Debug for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if f.alternate() {
            writeln!(f, "Token {{")?;
            writeln!(f, "    str: {:?}", self.str())?;
            writeln!(
                f,
                "    pos: {}",
                self.start
                    .fmt_range(pos(self.start.x + self.len, self.start.y))
            )?;
            write!(f, "}}")
        } else {
            f.debug_struct("Token")
                .field("kind", &self.kind)
                .field("pos", &self.start)
                .field("str", &self.str())
                .finish()
        }
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self.kind {
            TokenKind::Ident | TokenKind::IntLit => write!(f, "{:?}({})", self.kind, self.str()),
            _ => write!(f, "{:?}", self.kind),
        }
    }
}

impl Display for Lexer {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut val_max_len = 0;
        let mut x_max_len = 0;
        let mut y_max_len = 0;
        for tok in &self.tokens {
            let val_cur_len = format!("{tok}").len();
            val_max_len = max(val_max_len, val_cur_len);

            let x = tok.start.x;
            let y = tok.start.y;
            x_max_len = max(x_max_len, format!("{x}").len());
            y_max_len = max(y_max_len, format!("{y}").len());
        }

        for tok in &self.tokens {
            let val_str = format!("{tok}");
            let val_whitespace = " ".repeat(val_max_len - val_str.len());
            let x_str = format!("{x:?}", x = tok.start.x);
            let x_whitespace = " ".repeat(x_max_len - x_str.len());
            let y_str = format!("{y:?}", y = tok.start.y);
            let y_whitespace = " ".repeat(y_max_len - y_str.len());
            write!(f,
                "Token {{ {val_str}{val_whitespace} | (col: {y_whitespace}{y_str}, row: {x_whitespace}{x_str}) }}\n"
            )?
        }
        Ok(())
    }
}
// endregion

// region: parse.rs

// Not directly done on Node<Stmt> because ...(*￣０￣)ノ . (～￣▽￣)～ ... I don't remember
pub trait PosAwareDebug {
    fn fmt_with_pos(&self, f: &mut Formatter<'_>, start: &Pos, end: &Pos) -> fmt::Result;
}

impl<T: PosAwareDebug + Debug> Debug for Node<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        self.node.fmt_with_pos(f, &self.start, &self.end)
    }
}

impl PosAwareDebug for Stmt {
    fn fmt_with_pos(&self, f: &mut Formatter<'_>, start: &Pos, end: &Pos) -> fmt::Result {
        // The galaxy brain move, derive(Debug) for stmt, then use that to get variant's name.
        // could I just do this in the match below? yes. did I? no.
        let stmt_owned = format!("{:#?}", self);
        let variant_name = stmt_owned
            .lines()
            .next()
            .unwrap()
            .trim_end_matches(|c| c == '(' || c == '{' || c == ' ');

        let mut dbg = f.debug_struct(variant_name);
        dbg.field("pos", &format_args!("{}", start.fmt_range(*end)));

        match self {
            Stmt::FnDecl {
                ident,
                args,
                scope,
                return_type,
            } => {
                dbg.field("ident", ident)
                    .field("args", args)
                    .field("scope", scope)
                    .field("return_type", return_type);
            }
            Stmt::VarDecl { init_expr, arg } => {
                dbg.field("init_expr", init_expr).field("arg", arg);
            }
            Stmt::If {
                condition,
                scope,
                branches,
            } => {
                dbg.field("condition", condition)
                    .field("scope", scope)
                    .field("branches", branches);
            }
            Stmt::ElseIf { condition, scope } => {
                dbg.field("condition", condition).field("scope", scope);
            }
            Stmt::Else(node) => {
                dbg.field("node", node);
            }
            Stmt::While { condition, scope } => {
                dbg.field("condition", condition).field("scope", scope);
            }
            Stmt::Assign { ident, expr } => {
                dbg.field("ident", ident).field("expr", expr);
            }
            Stmt::Exit(node) => {
                dbg.field("node", node);
            }
            Stmt::NakedScope(node) => {
                dbg.field("node", node);
            }
            Stmt::Break => (),
            Stmt::Return(node) => {
                dbg.field("node", node);
            }
            Stmt::VarSemantics(variable) => {
                dbg.field("variable", variable);
            }
            Stmt::FnSemantics { id } => {
                dbg.field("id", id);
            }
        }

        dbg.finish()
    }
}

// == derive(Debug)
impl PosAwareDebug for Scope {
    fn fmt_with_pos(&self, f: &mut Formatter<'_>, start: &Pos, end: &Pos) -> fmt::Result {
        f.debug_struct("Scope")
            .field("pos", &format_args!("{}", start.fmt_range(*end)))
            .field("stmts", &self.stmts)
            .finish()
    }
}

impl PosAwareDebug for Expr {
    fn fmt_with_pos(&self, f: &mut Formatter<'_>, start: &Pos, end: &Pos) -> fmt::Result {
        // The galaxy brain move, derive(Debug) for stmt, then use that to get variant's name.
        // could I just do this in the match below? yes. did I? no.
        let expr_owned = format!("{:#?}", self);
        let variant_name = expr_owned
            .lines()
            .next()
            .unwrap()
            .trim_end_matches(|c| c == '(' || c == '{' || c == ' ');

        match self {
            Expr::Term(term) => {
                write!(f, "{term:?}")
            }
            Expr::Unary { op, expr } => f
                .debug_struct(variant_name)
                .field("pos", &format_args!("{}", start.fmt_range(*end)))
                .field("op", op)
                .field("expr", expr)
                .finish(),
            Expr::Binary { lhs, op, rhs } => f
                .debug_struct(variant_name)
                .field("pos", &format_args!("{}", start.fmt_range(*end)))
                .field("lhs", lhs)
                .field("op", op)
                .field("rhs", rhs)
                .finish(),
        }
    }
}

impl PosAwareDebug for Term {
    fn fmt_with_pos(&self, f: &mut Formatter<'_>, start: &Pos, end: &Pos) -> fmt::Result {
        let term_owned = format!("{:#?}", self);
        let variant_name = term_owned
            .lines()
            .next()
            .unwrap()
            .trim_end_matches(|c| c == '(' || c == '{' || c == ' ');
        let pos = start.fmt_range(*end);

        match self {
            Term::Ident | Term::IntLit | Term::False | Term::True => {
                let src = Contents::get_src_oneline(*start, *end);
                if PRINT_TERM_POS {
                    write!(f, "{variant_name}({src}) | {pos}",)
                } else {
                    write!(f, "{variant_name}({src})")
                }
            }
            Term::FnCall { ident, args } => f
                .debug_struct(variant_name)
                .field("pos", &format_args!("{pos}"))
                .field("ident", ident)
                .field("args", args)
                .finish(),
        }
    }
}

impl Debug for Ast {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{:#?}", self.stmts)
    }
}
// endregion
