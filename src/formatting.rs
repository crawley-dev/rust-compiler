use crate::{
    utils::{pos, Contents, Pos},
    Ast, Expr, Lexer, Node, Scope, Stmt, Term, Token, TokenKind,
};
use std::{
    cmp::max,
    fmt::{self, Debug, Display, Formatter},
};

const PRINT_TERM_POS: bool = false;
pub static mut SHORT_NODE_PRINT: bool = false;

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
                .field("str", &self.str())
                .field("kind", &self.kind)
                .field("pos", &self.start)
                .finish()
        }
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let str = self.str().replace("\n", "\\n");
        match self.kind {
            // TokenKind::Ident | TokenKind::IntLit => write!(f, "{:?}({})", self.kind, self.str()),
            // _ => write!(f, "{:?}", self.kind),
            _ => write!(f, "{:?}({str})", self.kind),
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
            let x_str = format!("{:?}", tok.start.x);
            let x_whitespace = " ".repeat(x_max_len - x_str.len());
            let y_str = format!("{:?}", tok.start.y);
            let y_whitespace = " ".repeat(y_max_len - y_str.len());
            write!(f,
                "Token {{ {val_str}{val_whitespace} | (col: {y_whitespace}{y_str}, row: {x_whitespace}{x_str}) | len({:2}) }}\n", tok.len
            )?
        }
        Ok(())
    }
}
// endregion

// region: parse.rs, semantic.rs

// Not directly done on Node<Stmt> because ...(*￣０￣)ノ . (～￣▽￣)～ ... I don't remember
pub trait PosAwareDebug {
    fn get_variant_name(&self) -> String;
    fn fmt_with_pos(&self, f: &mut Formatter<'_>, start: Pos, end: Pos) -> fmt::Result;
}

// For Any type of Node<T>, it will use "PosAwareDebug" to print the contents with position as an additional field.
impl<T: PosAwareDebug + Debug> Debug for Node<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        unsafe {
            if SHORT_NODE_PRINT {
                self.fmt_short(f)
            } else {
                self.node.fmt_with_pos(f, self.start, self.end)
            }
        }
    }
}

impl<T: PosAwareDebug + Debug> Node<T> {
    pub fn fmt_short(&self, f: &mut Formatter) -> fmt::Result {
        write!(
            f,
            "{} {}",
            self.node.get_variant_name(),
            self.start.fmt_range(self.end)
        )
    }
}

impl PosAwareDebug for Stmt {
    fn get_variant_name(&self) -> String {
        let stmt_owned: String = format!("{:#?}", self);
        stmt_owned
            .lines()
            .next()
            .unwrap()
            .trim_end_matches(|c| c == '(' || c == '{' || c == ' ')
            .to_string()
    }

    fn fmt_with_pos(&self, f: &mut Formatter<'_>, start: Pos, end: Pos) -> fmt::Result {
        // The galaxy brain move, derive(Debug) for stmt, then use that to get variant's name.
        // could I just do this in the match below? yes. did I? no.
        let mut dbg = f.debug_struct(&self.get_variant_name());
        dbg.field("pos", &format_args!("{}", start.fmt_range(end)));

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
            Stmt::NakedScope(node) => {
                dbg.field("node", node);
            }
            Stmt::NakedExpr(expr) => {
                dbg.field("expr", expr);
            }
            Stmt::Break => (),
            Stmt::Return(node) => {
                dbg.field("node", node);
            }
            Stmt::TypeAlias { ident, parse_type } => {
                dbg.field("ident", ident).field("parse_type", parse_type);
            }
            Stmt::StructDecl { ident, fields } => {
                dbg.field("ident", ident).field("fields", fields);
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

impl PosAwareDebug for Scope {
    fn get_variant_name(&self) -> String {
        let expr_owned = format!("{:#?}", self);
        expr_owned
            .lines()
            .next()
            .unwrap()
            .trim_end_matches(|c| c == '(' || c == '{' || c == ' ')
            .to_owned()
    }

    fn fmt_with_pos(&self, f: &mut Formatter<'_>, start: Pos, end: Pos) -> fmt::Result {
        f.debug_struct("Scope")
            .field("pos", &format_args!("{}", start.fmt_range(end)))
            .field("stmts", &self.stmts)
            .finish()
    }
}

impl PosAwareDebug for Expr {
    fn get_variant_name(&self) -> String {
        let expr_owned = format!("{:#?}", self);
        expr_owned
            .lines()
            .next()
            .unwrap()
            .trim_end_matches(|c| c == '(' || c == '{' || c == ' ')
            .to_owned()
    }

    fn fmt_with_pos(&self, f: &mut Formatter<'_>, start: Pos, end: Pos) -> fmt::Result {
        // The galaxy brain move, derive(Debug) for stmt, then use that to get variant's name.
        // could I just do this in the match below? yes. did I? no.

        let variant_name = self.get_variant_name();
        match self {
            Expr::Term(term) => match term {
                Term::Ident | Term::IntLit | Term::False | Term::True => {
                    let src = Contents::get_src_oneline(start, end);
                    if PRINT_TERM_POS {
                        write!(f, "{variant_name}({src})")
                    } else {
                        write!(f, "{variant_name}({src})")
                    }
                }
                Term::FnCall { .. } | Term::StructLit { .. } => write!(f, "{term:#?}"),
            },
            Expr::Unary { op, expr } => f
                .debug_struct(&variant_name)
                .field("pos", &format_args!("{}", start.fmt_range(end)))
                .field("op", op)
                .field("expr", expr)
                .finish(),
            Expr::Binary { lhs, op, rhs } => f
                .debug_struct(&variant_name)
                .field("pos", &format_args!("{}", start.fmt_range(end)))
                .field("lhs", lhs)
                .field("op", op)
                .field("rhs", rhs)
                .finish(),
        }
    }
}

// Practically useles, I don't wrap Term as Node<Term> because its always Node<Expr::Term> anyways
impl PosAwareDebug for Term {
    fn get_variant_name(&self) -> String {
        let term_owned = format!("{:#?}", self);
        term_owned
            .lines()
            .next()
            .unwrap()
            .trim_end_matches(|c| c == '(' || c == '{' || c == ' ')
            .to_owned()
    }
    fn fmt_with_pos(&self, f: &mut Formatter<'_>, start: Pos, end: Pos) -> fmt::Result {
        let variant_name = self.get_variant_name();
        let pos = start.fmt_range(end);

        match self {
            Term::Ident | Term::IntLit | Term::False | Term::True => {
                let src = Contents::get_src_oneline(start, end);
                if PRINT_TERM_POS {
                    write!(f, "{variant_name}({src}) | {pos}")
                } else {
                    write!(f, "{variant_name}({src})")
                }
            }
            Term::StructLit { ident, fields } => f
                .debug_struct(&variant_name)
                .field("pos", &format_args!("{pos}"))
                .field("ident", ident)
                .field("fields", fields)
                .finish(),
            Term::FnCall { ident, args } => f
                .debug_struct(&variant_name)
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
pub fn format_optional<T: fmt::Debug>(
    field: &Option<T>,
    f: &mut fmt::Formatter<'_>,
) -> fmt::Result {
    if let Some(value) = field {
        if f.alternate() {
            write!(f, "{:#?}", value)
        } else {
            write!(f, "{:?}", value)
        }
    } else {
        write!(f, "None")
    }
}
