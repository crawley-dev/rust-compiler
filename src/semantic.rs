// >>SEMANTIC<< The rules of the language, not grammar or syntax!
//  Assignment:
//      - ✅ arith-assign on new var or literal
//      - ✅ re-assign on immutable vars
//  type_map:
//      - ✅ expression (lhs & rhs) MUST have SAME type
//      - ✅ expression operators only work on specified type_map, e.g:
//          - "bool PLUS u8" doesn't compile
//          - LogicalAnd: bool, UnaryMinus: signed int
//      - ✅ statements expect specific type, e.g expr must eval to bool for 'if' statement.
//      - ✅ intlit is not a concrete type, can be coerced into any integer, after bounds checked.
//      - no implict type conversions, all explicit e.g: (type_1 as type_2)
//      - integer bounds checks
//          - requires me to interpret every arith expression?
//      - ✅ pointers, always have usize, not a defined type but an attribute, that modifies byte_size?
//          - kindof its own type (set size), but loose (inherits type's attr)
//          - a ptr is the original type with modified byte_width (4) & ptr flag set.

use crate::{
    lex::{TokenFlags, TokenKind},
    parse::{NodeExpr, NodeScope, NodeStmt, NodeTerm, AST},
};
use std::collections::HashMap;

const LOG_DEBUG_INFO: bool = true;
const ERR_MSG: &'static str = "[ERROR_SEMANTIC]";
const DBG_MSG: &'static str = "[DEBUG_SEMANTIC]";

// TODO(TOM): addressing modes
bitflags::bitflags! {
    #[derive(Debug, Clone, Copy, PartialEq)]
    pub struct TypeFlags: u16 {
        const POINTER = 1 << 0;
        const BOOLEAN = 1 << 1;
        const ARITH = 1 << 2;
        const SIGNED = 1 << 3; // or unsigned
        const FLOAT = 1 << 4;
        const ARRAY = 1 << 5;
        const NONCONCRETE = 1 << 6;
        const NONPRIMITIVE = 1 << 7;
    }
}

// TODO(TOM): an enum instead of flags, atleast for ptr?
#[derive(Clone, Debug, PartialEq)]
pub struct Type {
    pub byte_width: u8,
    pub ident: String,
    pub flags: TypeFlags,
}

impl Type {
    fn new(ident: &str, byte_width: u8, flags: TypeFlags) -> Type {
        Type {
            ident: ident.to_string(),
            byte_width,
            flags,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
struct Variable {
    ident: Option<String>,
    mutable: bool,
    var_type: Type,
}

struct SemanticContext {
    loop_count: i32,
}

pub struct Checker<'a> {
    ctx: SemanticContext,
    stack: Vec<Variable>,
    var_map: HashMap<String, Variable>,
    type_map: HashMap<&'a str, Type>,
}

impl Checker<'_> {
    pub fn check_ast(mut ast: AST) -> Result<AST, String> {
        let mut checker = Checker {
            ctx: SemanticContext { loop_count: 0 },
            stack: Vec::new(),
            var_map: HashMap::new(),
            type_map: HashMap::from([
                ("nil", Type::new("nil", 0, TypeFlags::empty())),
                (
                    "intlit",
                    Type::new("intlit", 0, TypeFlags::NONCONCRETE | TypeFlags::ARITH),
                ),
                ("bool", Type::new("bool", 1, TypeFlags::BOOLEAN)),
                ("u8", Type::new("u8", 1, TypeFlags::empty())),
                ("u16", Type::new("u16", 2, TypeFlags::empty())),
                ("u32", Type::new("u32", 3, TypeFlags::empty())),
                ("u64", Type::new("u64", 4, TypeFlags::empty())),
                ("usize", Type::new("usize", 4, TypeFlags::empty())),
                ("i8", Type::new("i8", 1, TypeFlags::SIGNED)),
                ("i16", Type::new("i16", 2, TypeFlags::SIGNED)),
                ("i32", Type::new("i32", 3, TypeFlags::SIGNED)),
                ("i64", Type::new("i64", 4, TypeFlags::SIGNED)),
                ("isize", Type::new("isize", 4, TypeFlags::SIGNED)),
                ("f32", Type::new("f32", 2, TypeFlags::FLOAT)),
                ("f64", Type::new("f64", 4, TypeFlags::FLOAT)),
            ]),
        };

        for stmt in &mut ast.stmts {
            checker.check_stmt(stmt)?;
        }
        Ok(ast)
    }

    fn check_stmt(&mut self, stmt: &mut NodeStmt) -> Result<&Type, String> {
        match stmt {
            NodeStmt::Let {
                ident,
                mutable,
                var_type,
                init_expr,
            } => {
                // need to add type information based on "var_type"
                if let Some(var) = self.var_map.get(ident.as_str()) {
                    return Err(format!(
                        "{ERR_MSG} Re-Initialisation of a Variable:\n{var:?}"
                    ));
                }
                // if ptr flag set, set byte_width: 4?
                let sem_type = self.get_type(var_type.ident.as_str())?;
                var_type.byte_width = sem_type.byte_width;
                var_type.flags = sem_type.flags.union(var_type.flags); // adds pointer flag if found.
                let var = Variable {
                    ident: Some(ident.clone()),
                    mutable: *mutable,
                    var_type: var_type.clone(),
                };
                self.stack.push(var.clone());
                self.var_map.insert(ident.clone(), var);
                if let Some(expr) = init_expr {
                    return self.check_expr(expr);
                }
                Ok(self.get_type("nil")?)
            }
            NodeStmt::If {
                condition,
                scope,
                branches,
            } => {
                assert_eq!(
                    self.check_expr(&condition)?.flags,
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
                    self.check_expr(&condition)?.flags,
                    TypeFlags::BOOLEAN,
                    "{ERR_MSG} 'Else If' statement condition not of type bool\n{condition:#?}"
                );
                self.check_scope(scope)
            }
            NodeStmt::Else(scope) => self.check_scope(scope),
            NodeStmt::While { condition, scope } => {
                self.ctx.loop_count += 1;
                self.check_expr(condition)?;
                self.check_scope(scope)?; // scope returns "nil", so do that and avoid a borrow checker err
                self.ctx.loop_count -= 1;
                Ok(self.get_type("nil")?)
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
            }
            // doesn't care about type, only that is valid.
            NodeStmt::Exit(expr) => self.check_expr(expr),
            NodeStmt::NakedScope(scope) => self.check_scope(scope),
            NodeStmt::Break => {
                if self.ctx.loop_count > 0 {
                    return Ok(self.get_type("nil")?);
                }
                Err(format!("{ERR_MSG} Not inside a loop! cannot break"))
            }
        }
    }

    fn check_scope(&mut self, scope: &mut NodeScope) -> Result<&Type, String> {
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
        Ok(self.get_type("nil")?)
    }

    fn check_expr(&self, expr: &NodeExpr) -> Result<&Type, String> {
        // recurse check expr's
        // binary:
        //  - check type_map match
        //  - check op is valid for operand type_map
        // unary: check op is valid for operand's type
        // term: get type
        match expr {
            NodeExpr::BinaryExpr { op, lhs, rhs } => {
                let lhs_type = self.check_expr(&*lhs)?;
                let rhs_type = self.check_expr(&*rhs)?;
                let op_dbg = format!(
                    "\n{lhs:#?}\n.. {op:?} ..\n{rhs:#?}\n{}\n{lhs_type:#?}\n.. {op:?} ..\n{rhs_type:#?}",
                    "-".repeat(20)
                );
                if LOG_DEBUG_INFO {
                    println!("{DBG_MSG} {lhs_type:?} .. {op:?}.. {rhs_type:?}")
                }

                // check if lhs,rhs are of same family of type_map
                // .. if both are arithmetic,
                // .. .. check sign is the same.(either both signed) or none are signed
                // .. .. check if either side is a non concrete (literal),
                if lhs_type.flags != rhs_type.flags
                    && !self.check_flags_equal(&lhs_type, &rhs_type, TypeFlags::ARITH)
                    && !(self.check_flags_equal(&lhs_type, &rhs_type, TypeFlags::SIGNED)
                        || self.check_flags_either(&lhs_type, &rhs_type, TypeFlags::SIGNED))
                    && !self.check_flags_either(&lhs_type, &rhs_type, TypeFlags::NONCONCRETE)
                {
                    return Err(format!(
                        "{ERR_MSG} Mismatched binary expression type_map:{op_dbg}"
                    ));
                }

                // arith op doesn't work on cmp
                // logical doesn't work on arith
                // cmp doesn't work on cmp
                match op.get_flags() {
                    // TODO(TOM): potential bug, matches if ONLY intersects that flag.
                    TokenFlags::ARITH if lhs_type.flags.intersects(TypeFlags::BOOLEAN) => Err(
                        format!("{ERR_MSG} Mismatched Operand and Op type_map:{op_dbg}"),
                    ),
                    TokenFlags::LOG if lhs_type.flags.intersects(TypeFlags::ARITH) => Err(format!(
                        "{ERR_MSG} Mismatched Operand and Op type_map:{op_dbg}"
                    )),
                    TokenFlags::CMP if lhs_type.flags.intersects(TypeFlags::BOOLEAN) => Err(
                        format!("{ERR_MSG} Mismatched Operand and Op type_map:{op_dbg}"),
                    ),
                    TokenFlags::ARITH => Ok(lhs_type), // resolve to arithmetic (lhs has to be arith alrady)
                    TokenFlags::LOG => Ok(lhs_type), // resolve to bool (lhs has to be bool already)
                    TokenFlags::CMP => Ok(self.get_type("bool")?), // resolve to bool (lhs isn't bool!)
                    _ => Err(format!("{ERR_MSG} Unsupported Binary Expression:{op_dbg}")),
                }
            }
            NodeExpr::UnaryExpr { op, operand } => {
                let operand_type = self.check_expr(&*operand)?;
                let op_dbg = format!("{op:?} .. {operand:?}({operand_type:?})");
                if LOG_DEBUG_INFO {
                    println!("{DBG_MSG} {op:?} .. {operand_type:?}")
                }
                // unary sub fails on: non-arith, unsigned
                // CmpNot fails on: non-boolean
                match op {
                    TokenKind::Sub => todo!("unary sub semantics"),
                    TokenKind::CmpNot => {
                        if !operand_type.flags.intersects(TypeFlags::BOOLEAN) {
                            return Err(format!("{ERR_MSG} Mismatched Op, Operand:{op_dbg}"));
                        }
                        Ok(operand_type)
                    }
                    _ => Err(format!("{ERR_MSG} Unsupported Unary Expression:{op_dbg}")),
                }
            }
            NodeExpr::Term(term) => self.check_term(term),
        }
    }

    fn check_term(&self, term: &NodeTerm) -> Result<&Type, String> {
        match term {
            NodeTerm::Ident(name) => match self.var_map.get(name.value.as_ref().unwrap()) {
                Some(var) => self.get_type(&var.var_type.ident.as_str()),
                None => Err(format!("{ERR_MSG} Variable not found '{name:?}'")),
            },
            NodeTerm::IntLit(_) => Ok(self.get_type("intlit")?), // TODO(TOM): non-concrete type_map
        }
    }

    fn get_type(&self, ident: &str) -> Result<&Type, String> {
        match self.type_map.get(ident) {
            Some(var_type) => Ok(var_type),
            None => Err(format!("{ERR_MSG} Type not found")),
        }
    }

    fn check_flags_equal(&self, t1: &Type, t2: &Type, flags: TypeFlags) -> bool {
        t1.flags.intersects(flags) && t2.flags.intersects(flags)
    }

    fn check_flags_either(&self, t1: &Type, t2: &Type, flags: TypeFlags) -> bool {
        t1.flags.intersects(flags) || t2.flags.intersects(flags)
    }
}
