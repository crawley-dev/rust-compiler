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
//          - each form has unique behaviour, such as a literal being non-concrete or an array being index-able]
//      ADDRESSING MODES:
//          - todo with how the computer reads memory (idk if it matters tbh)
//      CLONING:
//          - AST isn't a tree, contiguous "NodeStmt" Unions, some contain boxed data but not much
//          - if everything is a ptr "box", manipulating data MUCH easier, borrow checker not angry at me!
//          - Can't manipulate current AST freely, because it has to be rigid in size, its on the STACK!
//      Var impl:
//          - contains: name, optional initialisation expression, flags (mutability etc), Type
//          - Wrong! store a "base type" + modifications this variable has made to it.
//          - e.g its i16, but a pointer! or.. an array!

use crate::{
    debug, err,
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
        const NONE = 1 << 0;
        const SIGNED = 1 << 2;
        const BOOLEAN = 1 << 3;
        const FLOAT = 1 << 4;
        const INTEGRAL = 1 << 5;
        const LITERAL = 1 << 6;
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
        flags: TypeFlags,
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
    pub ident: String,
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

struct CheckerContext {
    loop_count: isize, // isize to get useful error messages in debug mode, not "usize >= 0"
    base_intlit: TypeForm,
    base_bool: TypeForm,
}

pub struct Checker<'a> {
    ctx: CheckerContext,
    stack: Vec<SemVariable>, // TODO(TOM): remove stack, store &'a str.
    var_map: HashMap<String, usize>, // TODO(TOM): this can be a <&'str, &SemVariable>
    prim_map: HashMap<&'a str, (usize, TypeFlags)>,
}

impl Checker<'_> {
    pub fn check_ast(ast: AST) -> Result<CodeGenData, String> {
        let mut checker = Checker {
            ctx: CheckerContext {
                loop_count: 0,
                base_intlit: TypeForm::Primitive {
                    flags: TypeFlags::INTEGRAL | TypeFlags::LITERAL,
                },
                base_bool: TypeForm::Primitive {
                    flags: TypeFlags::BOOLEAN,
                },
            },
            stack: Vec::new(),
            var_map: HashMap::new(),
            prim_map: HashMap::from([
                // TODO(TOM): generic solution for flags for any type form
                ("nil", (0, TypeFlags::NONE)),
                ("bool", (1, TypeFlags::BOOLEAN)),
                ("u8", (1, TypeFlags::INTEGRAL)),
                ("u16", (2, TypeFlags::INTEGRAL)),
                ("u32", (4, TypeFlags::INTEGRAL)),
                ("u64", (8, TypeFlags::INTEGRAL)),
                ("usize", (8, TypeFlags::INTEGRAL)),
                ("i8", (1, TypeFlags::SIGNED | TypeFlags::INTEGRAL)),
                ("i16", (2, TypeFlags::SIGNED | TypeFlags::INTEGRAL)),
                ("i32", (4, TypeFlags::SIGNED | TypeFlags::INTEGRAL)),
                ("i64", (8, TypeFlags::SIGNED | TypeFlags::INTEGRAL)),
                ("isize", (8, TypeFlags::SIGNED | TypeFlags::INTEGRAL)),
                ("f32", (4, TypeFlags::SIGNED | TypeFlags::FLOAT)),
                ("f64", (8, TypeFlags::SIGNED | TypeFlags::FLOAT)),
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
            NodeStmt::VarDecl {
                init_expr,
                ident,
                type_ident,
                mutable,
                ptr,
            } => {
                self.check_var_ident(ident.as_str())?;
                let (mut width, flags) = self.get_typeflags(type_ident.as_str())?;
                let form = match ptr {
                    true => {
                        width = 8;
                        TypeForm::Pointer {
                            underlying: Box::new(TypeForm::Primitive { flags: *flags }),
                        }
                    } // TODO(TOM): not generic to any type form.
                    false => TypeForm::Primitive { flags: *flags },
                };
                let var = SemVariable {
                    ident,
                    mutable,
                    init_expr,
                    var_type: Type::new(type_ident, width, form),
                };
                self.var_map.insert(var.ident.clone(), self.stack.len());
                self.stack.push(var.clone()); // have to clone as am creating a new NodeStmt.
                if let Some(ref expr) = var.init_expr {
                    self.check_expr(expr)?;
                }
                return Ok(NodeStmt::SemVarDecl(var));
            }
            NodeStmt::If {
                condition,
                scope,
                branches,
            } => {
                if !self.form_has_flags(self.check_expr(&condition)?, TypeFlags::BOOLEAN) {
                    return err!("'If' statement condition not 'boolean'\n{condition:#?}");
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
                if !self.form_has_flags(self.check_expr(&condition)?, TypeFlags::BOOLEAN) {
                    return err!("'ElseIf' statement condition not 'boolean'\n{condition:#?}");
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
            NodeStmt::Assign {
                ref ident,
                ref expr,
            } => {
                let var = self.get_var(ident.as_str())?;
                if !var.mutable {
                    return err!("Re-Assignment of a Constant:\n{var:?}");
                }
                self.check_expr(expr)?;
            }
            NodeStmt::Exit(ref expr) => {
                self.check_expr(&expr)?;
            }
            NodeStmt::NakedScope(scope) => {
                return Ok(NodeStmt::NakedScope(self.check_scope(scope)?));
            }
            NodeStmt::Break => {
                if self.ctx.loop_count <= 0 {
                    return err!("Not inside a loop! cannot break");
                }
            }
            NodeStmt::SemVarDecl { .. } => return err!("Found {stmt:#?}.. shouldn't have."),
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
        debug!("Ending scope, pop({pop_amt})");

        for _ in 0..pop_amt {
            let popped_var = match self.stack.pop() {
                Some(var) => self.var_map.remove(var.ident.as_str()),
                None => return err!("uhh.. scope messed up"),
            };
            debug!("Scope ended, removing {popped_var:#?}");
        }

        Ok(NodeScope {
            stmts,
            inherits_stmts: true,
        })
    }

    fn check_expr(&self, expr: &NodeExpr) -> Result<&TypeForm, String> {
        match expr {
            NodeExpr::BinaryExpr { op, lhs, rhs } => {
                let lform = self.check_expr(&*lhs)?;
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
                debug!("{op_dbg}");

                if !self.check_type_equality(lform, rform) {
                    return err!("Mismatched Op:Operand =>{op_dbg}");
                }

                // arithmetic operator doesn't work on boolean        operand
                // logical    operator doesn't work on integral/float operand
                // cmp        operator works on everything!! given they're of the same type!
                let op_flags = op.get_flags();
                match op_flags {
                    _ if op_flags.contains(TokenFlags::CMP) => Ok(&self.ctx.base_bool),
                    _ if op_flags.contains(TokenFlags::ARITH) => {
                        if self.form_has_flags(lform, TypeFlags::BOOLEAN) {
                            return err!("Mismatched Op:Operand =>{op_dbg}");
                        }
                        debug!("Bin Expr all good: {lform:#?}");
                        Ok(lform) // TODO(TOM): need flags.intersection() on both forms in future.
                    }
                    _ if op_flags.contains(TokenFlags::LOG) => {
                        if self.form_has_flags(lform, TypeFlags::INTEGRAL) {
                            return err!("Mismatched Op:Operand =>{op_dbg}");
                        }
                        debug!("Bin Expr all good: {lform:#?}");
                        Ok(lform)
                    } // TODO(TOM): floats not supported so don't match against.
                    _ => return err!("Unsupported binary expression:{op_dbg}"),
                }
            }
            NodeExpr::UnaryExpr { op, operand } => {
                let operand_form = self.check_expr(&*operand)?;
                let op_dbg = format!("{op:?} .. {operand:#?}\n{operand_form:#?}");
                debug!("{op_dbg}");

                // 'Unary sub' works on: integral, thats signed or a literal
                // 'Cmp Not'   works on: boolean
                // 'Addr of'   works on: allocated memory
                // 'Ptr Deref' works on: addresses to allocated memory
                match op {
                    TokenKind::Sub => {
                        match self.form_has_some_flags(
                            operand_form,
                            TypeFlags::INTEGRAL | TypeFlags::SIGNED,
                        ) || self.form_has_flags(operand_form, TypeFlags::LITERAL)
                        {
                            true => Ok(operand_form),
                            false => err!("Mismatched Op:Operand => {op_dbg}"),
                        }
                    }
                    TokenKind::CmpNot => {
                        match self.form_has_flags(operand_form, TypeFlags::BOOLEAN) {
                            true => Ok(operand_form),
                            false => err!("Mismatched Op:Operand => {op_dbg}"),
                        }
                    }
                    TokenKind::Ampersand => {
                        // TODO(TOM): need to change var form, primitive => pointer. can't because borrowing rules
                        match &**operand {
                            NodeExpr::Term(NodeTerm::Ident(ident)) => {
                                // let var =
                                //     self.get_var_mut(ident.value.as_ref().unwrap().as_str())?;
                                // var.var_type.form = TypeForm::Pointer {
                                //     underlying: Box::new(var.var_type.form),
                                // };
                                // Ok(&var.var_type.form)
                                let var = self.get_var(ident.value.as_ref().unwrap().as_str())?;

                                // idk what todo tbh... ;-; can't add a new entry into var_map or anything..
                                // unsafe rust time! not needed but
                                Ok(operand_form)
                            }
                            _ => err!("Operand has no address, cannot get address of\n{op_dbg}"),
                        }
                    }
                    // if operand is a variable identifier,
                    // which is of type form pointer, all good!
                    // return the underlying type.
                    TokenKind::Ptr => match &**operand {
                        NodeExpr::Term(NodeTerm::Ident(_)) => match operand_form {
                            TypeForm::Pointer { underlying } => Ok(&**underlying),
                            _ => err!("Operand has no address, cannot dereference\n{op_dbg}"),
                        },
                        _ => err!("Operand has no address, cannot dereference\n{op_dbg}"),
                    },
                    _ => err!("Unsupported Unary Expression:{op_dbg}"),
                }
            }
            NodeExpr::Term(term) => self.check_term(term),
        }
    }

    // can't give a mutable reference to typeform or variable, e.g "ident = 5; dwa = ident + ident".
    // "check_expr()" is called twice for ident, each requesting a mutable reference to "ident", Impossible!
    fn check_term(&self, term: &NodeTerm) -> Result<&TypeForm, String> {
        match term {
            NodeTerm::Ident(tok) => {
                let var = self.get_var(tok.value.as_ref().unwrap().as_str())?;
                Ok(&var.var_type.form)
            }
            NodeTerm::IntLit(_) => Ok(&self.ctx.base_intlit),
        }
    }

    fn check_type_equality(&self, lform: &TypeForm, rform: &TypeForm) -> bool {
        // check if lhs,rhs are of same form
        // .. if both are arithmetic,
        // .. .. check if either side is a literal,
        // .. .. sign is the same.
        if lform == rform {
            return true;
        }
        match lform {
            TypeForm::Primitive { flags: lf } => match rform {
                TypeForm::Primitive { flags: rf } => {
                    if !(self.flags_either_equal(*lf, *rf, TypeFlags::LITERAL)
                        || (self.flags_equal(*lf, *rf, TypeFlags::SIGNED)))
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

    fn check_var_ident(&self, ident: &str) -> Result<(), String> {
        if self.var_map.contains_key(ident) {
            return err!("Attempted re-initialisation of a Variable: '{ident}'");
        } else if self.prim_map.contains_key(ident) {
            return err!("Illegal Variable name, Types are keywords: '{ident}'");
        }
        Ok(())
    }

    fn get_var(&self, ident: &str) -> Result<&SemVariable, String> {
        match self.var_map.get(ident) {
            Some(idx) => Ok(self.stack.get(*idx).unwrap()),
            None => err!("Variable: {ident:?} doesn't exist."),
        }
    }

    fn get_var_mut(&mut self, ident: &str) -> Result<&mut SemVariable, String> {
        match self.var_map.get(ident) {
            Some(idx) => Ok(self.stack.get_mut(*idx).unwrap()),
            None => err!("Variable: {ident:?} doesn't exist."),
        }
    }

    fn get_typeflags(&self, ident: &str) -> Result<&(usize, TypeFlags), String> {
        match self.prim_map.get(ident) {
            Some(data) => Ok(data),
            None => err!("Primitive Type '{ident}' not found"),
        }
    }

    fn form_has_flags<'a>(&'a self, form: &TypeForm, desired_flags: TypeFlags) -> bool {
        match form {
            TypeForm::Primitive { flags } if flags.contains(desired_flags) => true,
            _ => false,
        }
    }

    fn form_has_some_flags<'a>(&'a self, form: &TypeForm, desired_flags: TypeFlags) -> bool {
        match form {
            TypeForm::Primitive { flags } if flags.intersects(desired_flags) => true,
            _ => false,
        }
    }

    fn flags_equal(&self, t1: TypeFlags, t2: TypeFlags, flags: TypeFlags) -> bool {
        t1.contains(flags) && t2.contains(flags)
    }

    fn flags_either_equal(&self, t1: TypeFlags, t2: TypeFlags, flags: TypeFlags) -> bool {
        t1.contains(flags) || t2.contains(flags)
    }
}

impl Type {
    fn new(ident: String, width: usize, form: TypeForm) -> Type {
        Type { width, ident, form }
    }
}
