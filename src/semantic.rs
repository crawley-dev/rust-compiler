// >>SEMANTIC<< The rules of the language, not grammar or syntax!
//  Assignment:
//      - ✅ arith-assign on new var or literal
//      - ✅ re-assign on immutable vars
//  Types:
//      - ✅ expression (lhs & rhs) MUST have SAME type
//      - ✅ expression operators only work on specified type_map, e.g:
//          - "bool PLUS u8" doesn't compile
//          - LogicalAnd: bool, UnaryMinus: signed int
//      - ✅ statements expect specific type, e.g expr must eval to bool for 'if' statement.
//      - ✅ intlit is not a concrete type, can be coerced into any integer, after bounds checked.
//      - no implict type conversions, all explicit e.g: (type_1 as type_2)
//      - integer bounds checks
//          - requires me to interpret every arith expression? let it be ub for now :)
//      - ✅ pointers, always have usize, not a defined type but an attribute, that modifies byte_size?
//          - kindof its own type (set size), but loose (inherits type's attr)
//          - a ptr is the original type with modified byte_width (4) & ptr flag set.
//      ✅ FORM:
//          - Types have a form, which is the group they fall under, e.g struct or array.
//          - each form has unique behaviour, such as a literal being non-concrete or an array being index-able
//      ADDRESSING MODES:
//          - todo with how the computer reads memory (idk if it matters tbh)
//      CLONING!:
//          - AST isn't a tree, contiguous "NodeStmt" Unions, some contain boxed data but not much
//          - if everything is a ptr "box", manipulating data MUCH easier, borrow checker not angry at me!

use crate::{
    debug, err,
    lex::{TokenFlags, TokenKind},
    parse::{NodeExpr, NodeScope, NodeStmt, NodeTerm, AST},
};
use std::{collections::HashMap, ptr::NonNull};

const LOG_DEBUG_INFO: bool = true;
const ERR_MSG: &'static str = "[ERROR_SEMANTIC]";
const DBG_MSG: &'static str = "[DEBUG_SEMANTIC]";

// TODO(TOM): addressing modes
bitflags::bitflags! {
    #[derive(Debug, Clone, Copy, PartialEq)]
    pub struct PrimFlags: u16 {
        const NONE = 1 << 0;
        const UNSIGNED = 1 << 1;
        const SIGNED = 1 << 2;
        const BOOLEAN = 1 << 3;
        const FLOAT = 1 << 4;
        const INTEGRAL = 1 << 5;
        const LITERAL = 1 << 6;
        // const VARIABLE = 1 << 7;
    }
}

// >>Form: A set of unique behaviour for a type<<
// Primitive: Represented by a number in asm
// Literal: can be coerced into any type with equivalent flags.
// Pointer: fixed byte_width, flags for underlying type
// Array: contains a length, flags for underlying type
// Struct: contains a vector of member types.
#[derive(Clone, Debug, PartialEq)]
pub enum TypeForm {
    Primitive {
        flags: PrimFlags,
    }, // e.g: "i16" => Type { ident: "i16", form: Primitive { flags: INTEGER} }
    Pointer {
        underlying: Box<TypeForm>,
    }, // e.g: "^i16" => Type { ident: "i16", form: Pointer { underlying: Primitive } }
    Struct {
        members: Vec<Type>,
    }, // e.g: "Struct123" => Type { ident: "Struct123", form: Struct { members: ... } }
    Array {
        length: usize,
        underlying: Box<TypeForm>,
    }, // e.g: "[5]i16" => Type { ident: i16, form: Primitive { length: 5, flags: INTEGER } }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Type {
    pub width: usize,
    pub ident: String, // TODO(TOM): drop the string, store id into Vec
    pub form: TypeForm,
}

#[derive(Debug, Clone, PartialEq)]
pub struct SemVariable {
    pub ident: String,
    pub mutable: bool,
    pub var_type: Type,
    pub init_expr: Option<NodeExpr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct CodeGenData {
    pub ast: AST,
}

// a weak context..
struct CheckerContext {
    loop_count: isize, // so the program doesn't immediately crash..
    base_intlit: TypeForm,
    base_bool: TypeForm,
}

pub struct Checker<'a> {
    ctx: CheckerContext,
    stack: Vec<String>, // TODO(TOM): remove stack, store &'a str.
    var_map: HashMap<String, SemVariable>, // TODO(TOM): this can be a <&'str, &SemVariable>
    prim_map: HashMap<&'a str, (usize, PrimFlags)>,
}

impl Checker<'_> {
    pub fn check_ast(mut ast: AST) -> Result<CodeGenData, String> {
        let mut checker = Checker {
            ctx: CheckerContext {
                loop_count: 0,
                base_intlit: TypeForm::Primitive {
                    flags: PrimFlags::INTEGRAL | PrimFlags::LITERAL,
                },
                base_bool: TypeForm::Primitive {
                    flags: PrimFlags::BOOLEAN,
                },
            },
            stack: Vec::new(),
            var_map: HashMap::new(),
            prim_map: HashMap::from([
                // TODO(TOM): generic solution for flags for any type form
                ("nil", (0, PrimFlags::NONE)),
                ("bool", (1, PrimFlags::BOOLEAN)),
                ("u8", (1, PrimFlags::UNSIGNED | PrimFlags::INTEGRAL)),
                ("u16", (2, PrimFlags::UNSIGNED | PrimFlags::INTEGRAL)),
                ("u32", (4, PrimFlags::UNSIGNED | PrimFlags::INTEGRAL)),
                ("u64", (8, PrimFlags::UNSIGNED | PrimFlags::INTEGRAL)),
                ("usize", (8, PrimFlags::UNSIGNED | PrimFlags::INTEGRAL)),
                ("i8", (1, PrimFlags::SIGNED | PrimFlags::INTEGRAL)),
                ("i16", (2, PrimFlags::SIGNED | PrimFlags::INTEGRAL)),
                ("i32", (4, PrimFlags::SIGNED | PrimFlags::INTEGRAL)),
                ("i64", (8, PrimFlags::SIGNED | PrimFlags::INTEGRAL)),
                ("isize", (8, PrimFlags::SIGNED | PrimFlags::INTEGRAL)),
                ("f32", (4, PrimFlags::SIGNED | PrimFlags::FLOAT)),
                ("f64", (8, PrimFlags::SIGNED | PrimFlags::FLOAT)),
            ]),
        };

        let mut new_ast = AST { stmts: Vec::new() };
        for stmt in ast.stmts {
            new_ast.stmts.push(checker.check_stmt(stmt)?);
        }

        Ok(CodeGenData { ast: new_ast })
    }

    fn check_stmt(&mut self, stmt: NodeStmt) -> Result<NodeStmt, String> {
        match stmt {
            NodeStmt::Decl {
                init_expr,
                ident,
                type_ident,
                mutable,
                ptr,
            } => {
                if self.var_map.contains_key(ident.as_str()) {
                    return err!("Attempted re-initialisation of a Variable: '{ident}'",);
                } else if self.prim_map.contains_key(ident.as_str()) {
                    return err!("Illegal Variable name, Types are keywords: '{ident}'",);
                }

                let (mut width, mut flags) = *self.prim_map.get(type_ident.as_str()).unwrap();
                // flags |= PrimFlags::VARIABLE;
                let form = match ptr {
                    true => {
                        width = 8;
                        TypeForm::Pointer {
                            underlying: Box::new(TypeForm::Primitive { flags }),
                        }
                    } // TODO(TOM): not generic to any type form.
                    false => TypeForm::Primitive { flags },
                };
                let var = SemVariable {
                    ident: ident,
                    mutable: mutable,
                    var_type: Type::new(type_ident, width, form),
                    init_expr,
                };
                self.stack.push(var.ident.clone());
                self.var_map.insert(var.ident.clone(), var.clone());
                if let Some(ref expr) = var.init_expr {
                    self.check_expr(expr)?;
                }

                return Ok(NodeStmt::SemDecl(var));
            }
            NodeStmt::If {
                condition,
                scope,
                branches,
            } => {
                if self.form_intersects(self.check_expr(&condition)?, PrimFlags::BOOLEAN) {
                    return err!("'If' statement condition not 'boolean'\n{condition:#?}",);
                }

                let mut new_branches = Vec::new();
                for branch in branches {
                    new_branches.push(self.check_stmt(branch)?);
                }
                return Ok(NodeStmt::If {
                    condition,
                    scope: self.check_scope(scope)?,
                    branches: new_branches,
                });
            }
            NodeStmt::ElseIf { condition, scope } => {
                if self.form_intersects(self.check_expr(&condition)?, PrimFlags::BOOLEAN) {
                    return err!("'ElseIf' statement condition not 'boolean'\n{condition:#?}",);
                };
                return Ok(NodeStmt::ElseIf {
                    condition,
                    scope: self.check_scope(scope)?,
                });
            }
            NodeStmt::Else(scope) => return Ok(NodeStmt::Else(self.check_scope(scope)?)),
            NodeStmt::While { condition, scope } => {
                self.ctx.loop_count += 1;
                self.check_expr(&condition)?;
                let new_scope = self.check_scope(scope)?;
                self.ctx.loop_count -= 1;
                return Ok(NodeStmt::While {
                    condition,
                    scope: new_scope,
                });
            }
            NodeStmt::Assign(ref expr) => {
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
                        return err!("Re-Assignment of a Constant:\n{var:?}",)
                    }
                    None => return err!("SemVariable '{ident}' does not exist",),
                    _ => (),
                }
                self.check_expr(expr)?;
            }
            NodeStmt::Exit(ref expr) => {
                self.check_expr(expr)?;
            }
            NodeStmt::NakedScope(scope) => {
                return Ok(NodeStmt::NakedScope(self.check_scope(scope)?));
            }
            NodeStmt::Break => {
                if self.ctx.loop_count <= 0 {
                    return err!("Not inside a loop! cannot break",);
                }
            }
            NodeStmt::SemDecl { .. } => unreachable!("{ERR_MSG} Found {stmt:#?}.. shouldn't have."),
        };
        Ok(stmt)
    }

    fn check_scope(&mut self, scope: NodeScope) -> Result<NodeScope, String> {
        let var_count = self.stack.len();

        let mut stmts = Vec::new();
        for stmt in scope.stmts {
            stmts.push(self.check_stmt(stmt)?);
        }

        let pop_amt = self.stack.len() - var_count;
        debug!("Ending scope, pop({pop_amt})",);

        for _ in 0..pop_amt {
            let popped_var = match self.stack.pop() {
                Some(var) => self.var_map.remove(var.as_str()),
                None => return err!("uhh.. scope messed up",),
            };
            debug!("Scope ended, removing {popped_var:#?}",);
        }

        Ok(NodeScope {
            stmts,
            inherits_stmts: true,
        })
    }

    fn check_expr(&self, expr: &NodeExpr) -> Result<&TypeForm, String> {
        match expr {
            NodeExpr::BinaryExpr { op, lhs, rhs } => {
                let mut lform = self.check_expr(&*lhs)?;
                let rform = self.check_expr(&*rhs)?;
                let op_dbg = format!(
                    "\n{sep}{sep}BinExpr{sep}{sep}\n\
                        {lhs:#?}\n.. {op:?} ..\n\
                        {rhs:#?}\n\
                    {sep}TYPE INFO{sep}\n\
                        {lform:#?}\n..\
                        {op:?} ..\n\
                        {rform:#?}",
                    sep = "-".repeat(5),
                );
                debug!("{op_dbg}",);

                if !self.check_type_equality(lform, rform) {
                    return err!("Mismatched Op:Operand =>{op_dbg}",);
                } // recursive so has to be seperate function

                // arithmetic operator doesn't work on boolean        operand
                // logical    operator doesn't work on integral/float operand
                // cmp        operator works on everything!! given they're of the same type!
                let op_flags = op.get_flags();
                match op_flags {
                    _ if op_flags.intersects(TokenFlags::CMP) => Ok(&self.ctx.base_bool),
                    _ if op_flags.intersects(TokenFlags::ARITH) => {
                        if self.form_intersects(lform, PrimFlags::BOOLEAN) {
                            return err!("Mismatched Op:Operand =>{op_dbg}",);
                        }
                        debug!("Bin Expr all good: {lform:#?}",);
                        Ok(lform) // TODO(TOM): need flags.intersection()
                    }
                    _ if op_flags.intersects(TokenFlags::LOG) => {
                        if self.form_intersects(lform, PrimFlags::INTEGRAL) {
                            return err!("Mismatched Op:Operand =>{op_dbg}",);
                        }
                        debug!("Bin Expr all good: {lform:#?}",);
                        Ok(lform)
                    } // TODO(TOM): floats not supported so don't match against.
                    _ => return err!("Unsupported binary expression:{op_dbg}",),
                }
            }
            NodeExpr::UnaryExpr { op, operand } => {
                let operand_form = self.check_expr(&*operand)?;
                let op_dbg = format!("{op:?} .. {operand:#?}\n{operand_form:#?}");
                debug!("{op_dbg}",);

                // unary sub fails on: non-arith, unsigned
                // CmpNot fails on: non-boolean
                // Addr of fails on: non-allocated
                match op {
                    TokenKind::Sub => todo!("unary sub semantics"),
                    TokenKind::CmpNot => {
                        match self.form_intersects(operand_form, PrimFlags::BOOLEAN) {
                            true => err!("Mismatched Op:Operand => {op_dbg}",),
                            false => Ok(operand_form),
                        }
                    }
                    TokenKind::BitAnd => {
                        debug!("mem addr of: {operand_form:#?}",);
                        match &**operand { // jesus double de-ref + ref??
                            NodeExpr::Term(NodeTerm::Ident(_)) => Ok(operand_form), // this works on types ?/
                            _ => err!(
                                "Cannot de-reference an expression / literal, no associated memory address\n{op_dbg}",
                            ),
                        }
                    }
                    _ => err!("Unsupported Unary Expression:{op_dbg}",),
                }
            }
            NodeExpr::Term(term) => self.check_term(term),
        }
    }

    fn check_term(&self, term: &NodeTerm) -> Result<&TypeForm, String> {
        match term {
            NodeTerm::Ident(name) => {
                match self.var_map.get(name.value.as_ref().unwrap().as_str()) {
                    Some(var) => Ok(&var.var_type.form),
                    None => err!("Variable Not Found '{}'", name.value.as_ref().unwrap()),
                }
            }
            NodeTerm::IntLit(val) => Ok(&self.ctx.base_intlit),
        }
    }

    fn check_type_equality<'a>(
        &'a self,
        lform: &TypeForm, // modify lform, as its returned out of check_expr()
        rform: &TypeForm,
    ) -> bool {
        // check if lhs,rhs are of same form
        // .. if both are arithmetic,
        // .. .. check if either side is a literal,
        // .. .. check sign is the same.
        if lform == rform {
            return true;
        }
        match lform {
            TypeForm::Primitive { flags: lf } => match rform {
                TypeForm::Primitive { flags: rf } => {
                    if !(self.flags_either_equal(*lf, *rf, PrimFlags::LITERAL)
                        || !(self.flags_equal(*lf, *rf, PrimFlags::UNSIGNED)
                            || self.flags_equal(*lf, *rf, PrimFlags::SIGNED)))
                    {
                        return false;
                    }
                    // *lf = lf.intersection(*rf); // settings bits of lform to be common bits between the 2.
                    true
                }
                _ => false,
            },
            _ => false,
        }
    }

    fn form_intersects<'a>(&'a self, form: &TypeForm, desired_flags: PrimFlags) -> bool {
        match form {
            TypeForm::Primitive { flags } if flags.intersects(desired_flags) => true,
            _ => false,
        }
    }

    fn get_primflags(&self, ident: &str) -> Result<&(usize, PrimFlags), String> {
        match self.prim_map.get(ident) {
            Some(data) => Ok(data),
            None => err!("Primitive Type '{ident}' not found",),
        }
    }

    fn flags_equal(&self, t1: PrimFlags, t2: PrimFlags, flags: PrimFlags) -> bool {
        t1.intersects(flags) && t2.intersects(flags)
    }

    fn flags_either_equal(&self, t1: PrimFlags, t2: PrimFlags, flags: PrimFlags) -> bool {
        t1.intersects(flags) || t2.intersects(flags)
    }
}

impl Type {
    fn new(ident: String, width: usize, form: TypeForm) -> Type {
        Type { width, ident, form }
    }
}
