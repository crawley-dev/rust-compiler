// >>SEMANTIC<< The rules of the language, not grammar or syntax!
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

use std::{
    collections::{HashMap, HashSet},
    fmt::Binary,
};

use crate::{
    lex::TokenKind,
    parse::{NodeExpr, NodeScope, NodeStmt, NodeTerm, ParseType, AST},
    TokenFlags,
};
const LOG_DEBUG_INFO: bool = true;
const ERR_MSG: &'static str = "[ERROR_SEMANTIC]";
const DBG_MSG: &'static str = "[DEBUG_SEMANTIC]";

// TODO(TOM): addressing modes
bitflags::bitflags! {
    #[derive(Debug, Clone, Copy, PartialEq)]
    pub struct TypeFlags: u8 {
        const POINTER = 1 << 0;
        const BOOLEAN = 1 << 1; // or arith
        const SIGNED = 1 << 2; // or unsigned
        const FLOAT = 1 << 3;
    }
}

#[derive(Debug, Clone, PartialEq)]
struct Variable {
    ident: Option<String>,
    mutable: bool,
    type_ident: String,
}

struct SemanticContext {
    loop_count: i32,
}

pub struct Checker<'a> {
    ctx: SemanticContext,
    stack: Vec<Variable>,
    var_map: HashMap<String, Variable>,
    types: HashMap<&'a str, TypeFlags>,
}

impl Checker<'_> {
    pub fn check_ast(mut ast: AST) -> Result<AST, String> {
        let mut checker = Checker {
            ctx: SemanticContext { loop_count: 0 },
            stack: Vec::new(),
            var_map: HashMap::new(),
            types: HashMap::from([
                ("u8", TypeFlags::empty()),
                ("u16", TypeFlags::empty()),
                ("u32", TypeFlags::empty()),
                ("u64", TypeFlags::empty()),
                ("usize", TypeFlags::empty()),
                ("i8", TypeFlags::SIGNED),
                ("i16", TypeFlags::SIGNED),
                ("i32", TypeFlags::SIGNED),
                ("i64", TypeFlags::SIGNED),
                ("isize", TypeFlags::SIGNED),
                ("f32", TypeFlags::FLOAT),
                ("f64", TypeFlags::FLOAT),
                ("bool", TypeFlags::BOOLEAN),
            ]),
        };

        for stmt in &mut ast.stmts {
            checker.check_stmt(stmt)?;
        }
        Ok(ast)
    }

    fn check_stmt(&mut self, stmt: &mut NodeStmt) -> Result<TypeFlags, String> {
        match stmt {
            NodeStmt::Let {
                expr,
                ident,
                mutable,
                var_type,
            } => {
                if let Some(var) = self.var_map.get(ident.as_str()) {
                    return Err(format!(
                        "{ERR_MSG} Re-Initialisation of a Variable:\n{var:?}"
                    ));
                }
                self.is_valid_type(var_type.type_ident.value.as_ref().unwrap().as_str())?;
                let var = Variable {
                    ident: Some(ident.clone()),
                    mutable: *mutable,
                    type_ident: var_type.type_ident.value.as_ref().unwrap().clone(),
                };
                self.stack.push(var.clone());
                self.var_map.insert(ident.clone(), var);
                self.check_expr(expr)
            }
            NodeStmt::If {
                condition,
                scope,
                branches,
            } => {
                assert_eq!(
                    self.check_expr(&condition)?,
                    TypeFlags::BOOLEAN,
                    "{ERR_MSG} 'If' statement condition not of type bool\n{condition:#?}"
                );
                for branch in branches {
                    self.check_stmt(branch)?;
                }
                self.check_scope(scope)
            }
            NodeStmt::ElseIf { condition, scope } => {
                assert_eq!(
                    self.check_expr(&condition)?,
                    TypeFlags::BOOLEAN,
                    "{ERR_MSG} 'Else If' statement condition not of type bool\n{condition:#?}"
                );
                self.check_scope(scope)
            }
            NodeStmt::Else(scope) => self.check_scope(scope),
            NodeStmt::While { condition, scope } => {
                self.ctx.loop_count += 1;
                self.check_expr(condition)?;
                let scope = self.check_scope(scope);
                self.ctx.loop_count -= 1;
                scope
            }
            NodeStmt::Assign(expr) => {
                let ident = match &expr {
                    NodeExpr::BinaryExpr { lhs, .. } => match **lhs {
                        NodeExpr::Term(NodeTerm::Ident(ref tok)) => {
                            tok.value.as_ref().unwrap().as_str()
                        }
                        _ => unreachable!("{ERR_MSG} Invalid Assignment: '{expr:#?}'"),
                    },
                    _ => unreachable!("{ERR_MSG} Invalid Assignment: '{expr:#?}'"),
                };
                match self.var_map.get(ident) {
                    Some(var) if !var.mutable => {
                        return Err(format!("{ERR_MSG} Re-Assignment of a Constant:\n{var:?}"))
                    }
                    None => return Err(format!("{ERR_MSG} Variable '{ident}' does not exist")),
                    _ => (),
                }
                self.check_expr(expr)
                /*
                let ident = match &expr {
                    NodeExpr::BinaryExpr { lhs, .. } => match **lhs {
                        NodeExpr::Term(NodeTerm::Ident(ref tok)) => {
                            tok.value.as_ref().unwrap().clone()
                        }
                        _ => return Err(format!("{ERR_MSG} Invalid Assignment: '{expr:#?}'")),
                    },
                    _ => return Err(format!("{ERR_MSG} Invalid Assignment: '{expr:#?}'")),
                };

                // if 'eq', remove op & lhs, gen_expr(rhs)
                // else check if var exists, remove Assign from OpAssign, e.g '+=' --> '+'
                let mut arith_assign = false;
                *expr = match expr {
                    NodeExpr::BinaryExpr { op, lhs, rhs } => {
                        if op == &TokenKind::Eq {
                            *dir_assign_ident = Some(ident);
                            *rhs.clone()
                        } else {
                            arith_assign = true;
                            NodeExpr::BinaryExpr {
                                op: op.assign_to_arithmetic()?,
                                lhs: Box::new(*lhs.clone()),
                                rhs: Box::new(*rhs.clone()),
                            }
                        }
                    }
                    _ => unreachable!(),
                };

                // check a variable is in the map,
                // .. in: if trying to bind new_var, NO! if not mutable, NO! else, all good.
                // .. not: being assigned in a 'let binding' or invalid (unreachable!)
                if let Some(var) = self.var_map.get(ident.as_str()) {
                    if self.ctx.binding_var.is_some() {
                        return Err(format!(
                            "{ERR_MSG} Attempted Re-initialisation of variable '{var:#?}'"
                        ));
                    } else if !var.mutable {
                        return Err(format!(
                            "{ERR_MSG} Attempted Re-assignment of constant: '{var:#?}'"
                        ));
                    }
                    self.check_expr(&expr)
                } else {
                    if arith_assign {
                        return Err(format!(
                            "{ERR_MSG} Attempted Compound Assignment on initialisation:\n'{ident:?}'"
                        ));
                    }
                    match self.ctx.binding_var.take() {
                        Some(mut var) => {
                            var.ident = Some(ident.clone());
                            self.stack.push(var.clone());
                            self.var_map.insert(ident, var);
                            self.check_expr(&expr)
                        }
                        None => unreachable!("{ERR_MSG} No variable to bind to '{ident:?}' to"),
                    }
                }
                 */
            }
            // doesn't care about type, only that is valid.
            NodeStmt::Exit(expr) => self.check_expr(expr),
            NodeStmt::NakedScope(scope) => self.check_scope(scope),
            NodeStmt::Break => {
                if self.ctx.loop_count > 0 {
                    Ok(TypeFlags::empty())
                } else {
                    Err(format!("{ERR_MSG} Not inside a loop! cannot break"))
                }
            }
        }
    }

    fn check_scope(&mut self, scope: &mut NodeScope) -> Result<TypeFlags, String> {
        let var_count = self.stack.len();
        for stmt in &mut scope.stmts {
            self.check_stmt(stmt)?;
        }

        let pop_amt = self.stack.len() - var_count;
        if LOG_DEBUG_INFO {
            println!("{DBG_MSG} Ending scope, pop({pop_amt})");
        }
        for _ in 0..pop_amt {
            let popped_var = match self.stack.pop() {
                Some(var) => self.var_map.remove(var.ident.as_ref().unwrap()),
                None => return Err(format!("{ERR_MSG} uhh.. scope messed up")),
            };
            if LOG_DEBUG_INFO {
                println!("{DBG_MSG} Scope ended, removing {popped_var:#?}");
            }
        }
        Ok(TypeFlags::empty())
    }

    fn check_expr(&self, expr: &NodeExpr) -> Result<TypeFlags, String> {
        // recurse check expr's
        // binary:
        //  - check op is valid for operand types
        // unary: check op is valid for operand's type
        // term: get type
        match expr {
            NodeExpr::BinaryExpr { op, lhs, rhs } => {
                let lhs_type = self.check_expr(&*lhs)?;
                let rhs_type = self.check_expr(&*rhs)?;
                let op_dbg = format!(
                    "\n{lhs:?} .. {op:?} .. {rhs:?}\n{lhs_type:?} .. {op:?} .. {rhs_type:?}"
                );
                if LOG_DEBUG_INFO {
                    println!("{DBG_MSG} {lhs_type:?} .. {op:?}.. {rhs_type:?}")
                }

                // if lhs,rhs types don't match
                // arith op doesn't work on cmp
                // logical doesn't work on arith
                // cmp works on arith & cmp
                if lhs_type != rhs_type {
                    return Err(format!(
                        "{ERR_MSG} Mismatched binary expression types:{op_dbg}"
                    ));
                }
                match op.get_flags() {
                    TokenFlags::ARITH if lhs_type.contains(TypeFlags::BOOLEAN) => Err(format!(
                        "{ERR_MSG} Mismatched Operand and Op Types:{op_dbg}"
                    )),
                    TokenFlags::LOG if !lhs_type.contains(TypeFlags::BOOLEAN) => Err(format!(
                        "{ERR_MSG} Mismatched Operand and Op Types:{op_dbg}"
                    )),
                    TokenFlags::ARITH => Ok(TypeFlags::SIGNED), // TODO(TOM): unsigned
                    TokenFlags::LOG => Ok(TypeFlags::BOOLEAN),
                    TokenFlags::CMP => Ok(TypeFlags::BOOLEAN),
                    _ => Err(format!("{ERR_MSG} Unsupported Binary Expression:{op_dbg}")),
                }
            }
            NodeExpr::UnaryExpr { op, operand } => {
                let operand_type = self.check_expr(&*operand)?;
                if LOG_DEBUG_INFO {
                    println!("{DBG_MSG} {op:?} .. {operand_type:?}")
                }
                Ok(TypeFlags::empty())
            }
            NodeExpr::Term(term) => self.check_term(term),
        }
    }

    fn check_term(&self, term: &NodeTerm) -> Result<TypeFlags, String> {
        match term {
            NodeTerm::Ident(name) => match self.var_map.get(name.value.as_ref().unwrap()) {
                Some(var) => self.is_valid_type(var.type_ident.as_str()),
                None => Err(format!("{ERR_MSG} Variable not found '{name:?}'")),
            },
            NodeTerm::IntLit(_) => Ok(TypeFlags::SIGNED), // TODO(TOM): non-concrete types
        }
    }

    fn is_valid_type(&self, ident: &str) -> Result<TypeFlags, String> {
        match self.types.get(ident) {
            Some(flags) => Ok(*flags),
            None => Err(format!("{ERR_MSG} Type not found")),
        }
    }
}
