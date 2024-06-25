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
//      ✅ Type impl:
//          - either a primitive or >>FUTURE:<< struct or union
//      ✅ Var impl:
//          - store a "type" + modifications, "form".
//          - e.g its i16, but a pointer! or.. an array!
//      Type Width:
//          - don't currently check if an assignment is of valid size, can't downcast type size, only up.

use crate::{
    debug, err,
    lex::{TokenFlags, TokenKind},
    parse::{NodeExpr, NodeScope, NodeStmt, NodeTerm, AST},
};
use std::collections::HashMap;

const LOG_DEBUG_INFO: bool = false;
const ERR_MSG: &'static str = "[ERROR_SEMANTIC]";
const DBG_MSG: &'static str = "[DEBUG_SEMANTIC]";

// Bool    | Int | Float
// Signed  | Unsigned
// Literal | Variable
bitflags::bitflags! {
    #[derive(Debug, Clone, Copy, PartialEq)]
    pub struct TypeFlags: u8 {
        const NONE = 1 << 0;
        const INT = 1 << 1;
        const BOOL = 1 << 2;
        const FLOAT = 1 << 3;
        const SIGNED = 1 << 4;
        const LITERAL = 1 << 5; // can be coerced into any type with equivalent flags.
    }
    #[derive(Debug, Clone, Copy, PartialEq)]
    pub struct FormFlags: u8 { // bit weird using bitflags as each (except mutable) is unique
        const PRIMITIVE = 1 << 0;
        const PTR = 1 << 1;
        const ARRAY = 1 << 2;
        const MUTABLE = 1 << 3;
    }
}

// >>Form: Different forms a type can be>>
// Base: just a type, has some flags, chill.
// Struct: a group of types, stores type id, not type.
// Union: a group of types that share the same storage.
#[derive(Debug, Clone, PartialEq)]
pub enum TypeForm {
    Base {
        flags: TypeFlags,
    },
    Struct {
        member_ids: Vec<usize>,
    },
    Union {
        // TODO(TOM): define later
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct Type {
    pub ident: String,
    pub width: usize,
    pub form: TypeForm,
}

#[derive(Debug, Clone, PartialEq)]
pub struct SemVariable {
    pub ident: String,
    pub width: usize, // depending in whether its form, width != base_type.width
    pub type_id: usize,
    pub flags: FormFlags,
    pub init_expr: Option<NodeExpr>,
}

struct CheckerContext {
    loop_count: isize, // not usize to get useful error messages in debug mode, not "usize >= 0"
}

pub struct HandoffData {
    pub ast: AST,
    pub types: Vec<Type>,
    pub type_map: HashMap<String, usize>,
}

pub struct Checker {
    ctx: CheckerContext,
    stack: Vec<SemVariable>,
    var_map: HashMap<String, usize>,
    types: Vec<Type>,
    type_map: HashMap<String, usize>,
}

fn new_base(ident: &str, width: usize, flags: TypeFlags) -> Type {
    Type {
        ident: ident.to_string(),
        width,
        form: TypeForm::Base { flags },
    }
}

fn new_struct(ident: &str, width: usize, member_ids: Vec<usize>) -> Type {
    Type {
        ident: ident.to_string(),
        width,
        form: TypeForm::Struct { member_ids },
    }
}

impl Checker {
    pub fn check_ast(mut ast: AST) -> Result<HandoffData, String> {
        let types = Vec::from([
            new_base("nil", 0, TypeFlags::NONE),
            new_base("bool", 1, TypeFlags::BOOL),
            new_base("u8", 1, TypeFlags::INT),
            new_base("u16", 2, TypeFlags::INT),
            new_base("u32", 4, TypeFlags::INT),
            new_base("u64", 8, TypeFlags::INT),
            new_base("usize", 8, TypeFlags::INT),
            new_base("i8", 1, TypeFlags::SIGNED | TypeFlags::INT),
            new_base("i16", 2, TypeFlags::SIGNED | TypeFlags::INT),
            new_base("i32", 4, TypeFlags::SIGNED | TypeFlags::INT),
            new_base("i64", 8, TypeFlags::SIGNED | TypeFlags::INT),
            new_base("isize", 8, TypeFlags::SIGNED | TypeFlags::INT),
            new_base("f32", 4, TypeFlags::SIGNED | TypeFlags::FLOAT),
            new_base("f64", 8, TypeFlags::SIGNED | TypeFlags::FLOAT),
        ]);
        let mut checker = Checker {
            ctx: CheckerContext { loop_count: 0 },
            stack: Vec::new(),
            var_map: HashMap::new(),
            types,
            type_map: HashMap::new(),
        };

        for (n, base) in checker.types.iter().enumerate() {
            checker.type_map.insert(base.ident.clone(), n);
        }

        let mut sem_ast = AST { stmts: Vec::new() };
        for stmt in ast.stmts {
            sem_ast.stmts.push(checker.check_stmt(stmt)?);
        }

        Ok(HandoffData {
            ast: sem_ast,
            types: checker.types,
            type_map: checker.type_map,
        })
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
                let type_id = self.get_type_id(type_ident.as_str())?;
                let _type = self.types.get(type_id).unwrap();

                let mut width = _type.width;
                let mut flags = FormFlags::PRIMITIVE;
                if ptr {
                    width = 8; // TODO(TOM): not gonna do 32 bit compatability, 64 bits only.
                    flags = FormFlags::PTR;
                }
                if mutable {
                    flags |= FormFlags::MUTABLE;
                }

                let mut var = SemVariable {
                    ident,
                    width,
                    type_id,
                    flags,
                    init_expr,
                };

                self.var_map.insert(var.ident.clone(), self.stack.len());
                self.stack.push(var.clone()); // have to clone as am creating a new NodeStmt.

                if let Some(ref expr) = var.init_expr {
                    let (typeflags, formflags) = self.check_expr(expr)?;
                    if !var.flags.contains(formflags) {
                        debug!("Form Inequality! {:?} vs {formflags:?}", var.flags);
                        return err!("Incorrect rhs type form => {var:#?}\n.. {expr:#?}");
                    }

                    if let TypeForm::Base { flags: varflags } =
                        self.types.get(var.type_id).unwrap().form
                    {
                        if !(varflags.contains(typeflags)
                            || self.check_type_equality(typeflags, varflags))
                        {
                            debug!("Type inequality! {varflags:?} vs {typeflags:?}");
                            return err!("Incorrect rhs type => {var:#?}\n.. {expr:#?}");
                        }
                    }
                }

                return Ok(NodeStmt::SemVarDecl(var));
            }
            NodeStmt::If {
                condition,
                scope,
                branches,
            } => {
                let (typeflags, formflags) = self.check_expr(&condition)?;
                if !typeflags.contains(TypeFlags::BOOL) {
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
                let (typeflags, formflags) = self.check_expr(&condition)?;
                if !typeflags.contains(TypeFlags::BOOL) {
                    return err!("'ElseIf' statement condition not 'boolean'\n{condition:#?}");
                }

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
                if !var.flags.contains(FormFlags::MUTABLE) {
                    return err!("Re-Assignment of a Constant:\n{var:#?}");
                }

                let (typeflags, formflags) = self.check_expr(expr)?;
                if !var.flags.contains(formflags) {
                    // TODO(TOM): error on FormFlags::Mutable, we don't care about that!
                    debug!("Form Inequality! {:?} vs {formflags:?}", var.flags);
                    return err!("Invalid Assign: rhs type => {var:#?}\n.. {expr:#?}");
                }
                if let TypeForm::Base { flags } = self.types.get(var.type_id).unwrap().form {
                    if !(flags.contains(typeflags) || self.check_type_equality(typeflags, flags)) {
                        debug!("Type inequality! {flags:?} vs {typeflags:?}");
                        return err!("Invalid Assign: rhs type form => {var:#?}\n.. {expr:#?}");
                    }
                }
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

    fn check_expr(&self, expr: &NodeExpr) -> Result<(TypeFlags, FormFlags), String> {
        match expr {
            NodeExpr::BinaryExpr { op, lhs, rhs } => {
                let (ltype, lform) = self.check_expr(&*lhs)?;
                let (rtype, rform) = self.check_expr(&*rhs)?;
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

                if !self.check_type_equality(ltype, rtype) {
                    return err!("Mismatched operand types =>\n{op_dbg}");
                }

                // cmp        type, type => bool
                // logical    bool, bool => bool
                // arithmetic int,  int  => int
                let op_flags = op.get_flags();
                match op_flags {
                    _ if op_flags.contains(TokenFlags::CMP) => {
                        Ok((TypeFlags::BOOL, FormFlags::PRIMITIVE))
                    }
                    _ if op_flags.contains(TokenFlags::LOG) => {
                        if ltype.contains(TypeFlags::INT) {
                            return err!("Mismatched binary operation =>\n{op_dbg}");
                        }
                        debug!("BinExpr all good: {op_dbg}");
                        Ok((TypeFlags::BOOL, FormFlags::PRIMITIVE))
                    }
                    _ if op_flags.contains(TokenFlags::ARITH) => {
                        if ltype.contains(TypeFlags::BOOL) {
                            return err!("Mismatched binary operation =>\n{op_dbg}");
                        }
                        debug!("BinExpr all good: {op_dbg}");
                        Ok((ltype, lform))
                    }
                    _ => err!("Unsupported binary expression =>\n{op_dbg}"),
                }
            }
            NodeExpr::UnaryExpr { op, operand } => {
                let (typeflags, formflags) = self.check_expr(&*operand)?;
                let op_dbg = format!("{op:?} .. {operand:#?}\n{typeflags:#?}\n{formflags:#?}");
                debug!("{op_dbg}");

                // 'Unary sub' int(lit) => int | signed
                // 'Cmp Not'   bool => bool
                // 'Addr of'   var => ptr
                // 'Ptr Deref' ptr => var
                match op {
                    TokenKind::Sub => {
                        if !typeflags.contains(TypeFlags::INT) {
                            return err!("'-' unary operator only works on integers =>\n{op_dbg}");
                        }
                        Ok((TypeFlags::INT | TypeFlags::SIGNED, formflags))
                    }
                    TokenKind::CmpNot => {
                        if !typeflags.contains(TypeFlags::BOOL) {
                            return err!("'!' operator only works on booleans =>\n{op_dbg}");
                        }
                        Ok((TypeFlags::BOOL, formflags))
                    }
                    TokenKind::Ampersand => {
                        match &**operand {
                            NodeExpr::Term(NodeTerm::Ident(ident))
                                if !formflags.contains(FormFlags::PTR) =>
                            {
                                let var = self.get_var(ident.value.as_ref().unwrap().as_str())?;
                                // TODO(TOM): i am extreme dumb, changing the var to a ptr if it EVER gets '&' ????
                                // unsafe {
                                //     let var_ptr = var as *const SemVariable;
                                //     let var_mut_ptr = var_ptr as *mut SemVariable;
                                //     if (*var_mut_ptr).flags.contains(FormFlags::MUTABLE) {
                                //         (*var_mut_ptr).flags = FormFlags::PTR | FormFlags::MUTABLE;
                                //     } else {
                                //         (*var_mut_ptr).flags = FormFlags::PTR;
                                //     }
                                // }
                                // debug!("should've added ptr form flag to: {var:#?}");
                                Ok((typeflags, FormFlags::PTR))
                            }
                            _ => err!(
                                "'&' operator requires expr to have a memory address =>\n{op_dbg}"
                            ),
                        }
                    }
                    TokenKind::Ptr => {
                        if !formflags.contains(FormFlags::PTR) {
                            return err!(
                            "{operand:#?}\n'^' operator requires expr to be a pointer\n{op_dbg}"
                        );
                        }
                        // TODO(TOM): currently blind yolo deref the mem address, could be nullptr!
                        // .. could dis-allow ptr arith, only "&VAR_NAME" to get ptr
                        let without_ptr = formflags & !FormFlags::PTR;
                        Ok((typeflags, without_ptr))
                        // match &**operand {
                        //     NodeExpr::UnaryExpr { ref operand, .. } => {
                        //         if let NodeExpr::Term(NodeTerm::Ident(ident)) = **operand {}
                        //     }
                        //     NodeExpr::Term(NodeTerm::Ident(ident)) => {
                        //         let var = self.get_var(ident.value.as_ref().unwrap().as_str())?;
                        //         let without_ptr = formflags & !FormFlags::PTR;
                        //         Ok((typeflags, without_ptr))
                        //     }
                        //     _ => err!(
                        //         "{operand:#?}\n'^' operator requires expr to be a pointer\n{op_dbg}"
                        //     ),
                        // }
                    }
                    _ => err!("Unsupported Unary Expression:{op_dbg}"),
                }
            }
            NodeExpr::Term(term) => self.check_term(term),
        }
    }

    fn check_term(&self, term: &NodeTerm) -> Result<(TypeFlags, FormFlags), String> {
        match term {
            NodeTerm::Ident(tok) => {
                let var = self.get_var(tok.value.as_ref().unwrap().as_str())?;
                let baseflags = match self.types.get(var.type_id).unwrap().form {
                    TypeForm::Base { flags } => flags,
                    _ => TypeFlags::NONE,
                };
                Ok((baseflags, var.flags))
            }
            NodeTerm::IntLit(_) => Ok((TypeFlags::INT | TypeFlags::LITERAL, FormFlags::PRIMITIVE)),
        }
    }

    fn check_type_equality(&self, lf: TypeFlags, rf: TypeFlags) -> bool {
        if lf == rf {
            return true;
        }
        // checks if they're the same sign, and one is literal.
        self.flags_either_equal(lf, rf, TypeFlags::LITERAL)
            || (self.flags_equal(lf, rf, TypeFlags::SIGNED))
    }

    fn check_var_ident(&self, ident: &str) -> Result<(), String> {
        if self.var_map.contains_key(ident) {
            return err!("Attempted re-initialisation of a Variable: '{ident}'");
        } else if self.type_map.contains_key(ident) {
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

    fn get_type_id(&self, ident: &str) -> Result<usize, String> {
        match self.type_map.get(ident) {
            Some(id) => Ok(*id),
            None => err!("Type '{ident}' not found"),
        }
    }

    fn flags_equal(&self, t1: TypeFlags, t2: TypeFlags, flags: TypeFlags) -> bool {
        t1.contains(flags) && t2.contains(flags)
    }

    fn flags_either_equal(&self, t1: TypeFlags, t2: TypeFlags, flags: TypeFlags) -> bool {
        t1.contains(flags) || t2.contains(flags)
    }

    fn add_type(&mut self, new_type: Type) {
        self.type_map
            .insert(new_type.ident.clone(), self.types.len());
        self.types.push(new_type);
    }
}
