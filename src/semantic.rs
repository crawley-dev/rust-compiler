// >>SEMANTIC<<
//  Assignment:
//      - arith-assign on new var or literal
//      - re-assign on immutable vars
//  Types:
//      - expression (lhs & rhs) MUST have SAME type
//      - expression operators only work on specified types, e.g:
//          - "bool PLUS u8" doesn't compile
//          - LogicalAnd: bool, UnaryMinus: signed int
//      - statements expect specific type, e.g expr must eval to bool for 'if' statement.
//      - no implict type conversions, all explicit e.g: (type_1 as type_2)
//      - intlit is not a concrete type, can be coerced into any integer, after bounds checked.

#![allow(unused)]

use crate::{
    lex::TokenKind,
    parse::{NodeExpr, NodeScope, NodeStmt, NodeTerm, AST},
};

struct Variable {
    ident: Option<String>,
    mutable: bool,
}

pub struct Checker<'a> {
    // context stuff
    ast: &'a AST,
}

impl Checker<'_> {
    pub fn check_ast(ast: &AST) -> Result<(), String> {
        let mut checker = Checker { ast };
        for stmt in &checker.ast.stmts {
            checker.check_stmt(stmt)?;
        }
        Ok(())
    }

    fn check_stmt(&mut self, stmt: &NodeStmt) -> Result<(), String> {
        Ok(())
    }

    fn check_scope(&mut self, scope: &NodeScope) -> Result<(), String> {
        todo!("")
    }

    fn check_expr(&mut self, expr: &NodeExpr) -> Result<(), String> {
        // recurse check expr's
        // binary:
        //  - check op is valid for operand types
        // unary: check op is valid for operand's type
        // term: get type

        match expr {
            NodeExpr::BinaryExpr { op, lhs, rhs } => todo!(),
            NodeExpr::UnaryExpr { op, operand } => todo!(),
            NodeExpr::Term(_) => Ok(()),
        }
    }
}
