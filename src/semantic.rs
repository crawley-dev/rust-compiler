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

use std::collections::{HashMap, HashSet};

use crate::{
    lex::TokenKind,
    parse::{NodeExpr, NodeScope, NodeStmt, NodeTerm, AST},
};

struct Type {
    name: String,
    pointer: bool,
}

struct Variable {
    ident: Option<String>,
    mutable: bool,
    type_ident: Type,
}

pub struct Checker<'a> {
    // context stuff
    ast: &'a AST,
    stack: Vec<Variable>,
    var_map: HashMap<String, Variable>,
    type_map: HashSet<&'a str>,
}

impl Checker<'_> {
    pub fn check_ast(ast: &AST) -> Result<(), String> {
        let mut checker = Checker {
            ast,
            stack: Vec::new(),
            var_map: HashMap::new(),
            type_map: HashSet::from([
                "u8", "u16", "u32", "u64", "usize", "i8", "i16", "i32", "i64", "isize", "f32",
                "f64", "bool",
            ]),
        };
        for stmt in &checker.ast.stmts {
            checker.check_stmt(stmt)?;
        }
        Ok(())
    }

    fn check_stmt(&mut self, stmt: &NodeStmt) -> Result<(), String> {
        match stmt {
            NodeStmt::Let {
                expr,
                mutable,
                type_ident,
            } => {
                // create new variable, add to vars
                // - get name, check for collisons
                //
            }
            NodeStmt::Assign(_) => todo!(),
            NodeStmt::Exit(_) => todo!(),
            NodeStmt::Scope(_) => todo!(),
            NodeStmt::If(_, _, _) => todo!(),
            NodeStmt::ElseIf(_, _) => todo!(),
            NodeStmt::Else(_) => todo!(),
            NodeStmt::While(_, _) => todo!(),
            NodeStmt::Break => todo!(),
        }
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
