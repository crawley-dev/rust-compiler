// >>SEMANTIC<< The rules of the language, not grammar or syntax!
//  CLONING:
//      - AST isn't a tree, contiguous "NodeStmt" Unions, some contain boxed data but not much
//      - if everything is a ptr "box", manipulating data MUCH easier, borrow checker not angry at me!
//      - Can't manipulate current AST freely, because it has to be rigid in size, its on the STACK!
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
//      ✅ Type impl:
//          - either a primitive or >>FUTURE:<< struct or union
//      ✅ Var impl:
//          - store a "type" + modifications, "form".
//          - e.g its i16, but a pointer! or.. an array!
//      ✅ Structure Revision:
//          Either passing Variable or Literal
//          - Both: TypeMode, AddressingMode, e.g Boolean Array
//          - Var: ptr to the var
//          - Literal: Inherited Width
//          - TypeMode:
//              - What operations can be performed
//              - (OPTIONAL): Sign, if numerical
//          - AddressingMode:
//              - how is it represented in memory, if at all
//              - (OPTIONAL): mutability, if represented in memory
//      Type Narrowing:
//          - check if the assigned expr is wider than the assignee variable

use crate::{
    debug, err,
    lex::{TokenFlags, TokenKind},
    parse::{NodeExpr, NodeScope, NodeStmt, NodeTerm, AST},
};
use std::{collections::HashMap, fmt::format, ops::Add, ptr::NonNull};

const LOG_DEBUG_INFO: bool = true;
const ERR_MSG: &'static str = "[ERROR_SEMANTIC]";
const DBG_MSG: &'static str = "[DEBUG_SEMANTIC]";

// Bool    | Int | Float
// Signed  | Unsigned
// Literal | Variable
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TypeMode {
    Bool,
    Int { signed: bool },
    Float { signed: bool },
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum AddressingMode {
    Primitive,
    Pointer, // only literal == nullptr
    Array,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExprForm {
    Variable { ptr: NonNull<SemVariable> },
    Literal { inherited_width: usize },
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExprData {
    type_mode: TypeMode,
    addr_mode: AddressingMode,
    form: ExprForm,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypeForm {
    Base {
        type_mode: TypeMode,
    }, // Base: just a type, has some flags, chill.
    Struct {
        member_ids: Vec<usize>,
    }, // Struct: a group of types, stores type id, not type.
    Union {
        // TODO(TOM): define later
    }, // Union: a group of types that share the same storage.
}

#[derive(Debug, Clone, PartialEq)]
pub struct Type {
    pub width: usize,
    pub ident: String,
    pub form: TypeForm,
}

#[derive(Debug, Clone, PartialEq)]
pub struct SemVariable {
    pub width: usize, // depending in whether its form, width != base_type.width
    pub ident: String,
    pub type_id: usize,
    pub mutable: bool,
    pub addr_mode: AddressingMode,
    pub init_expr: Option<NodeExpr>,
}

//////////////////////////////////////////////////////////////////////////////////////////

pub struct HandoffData {
    pub ast: AST,
    pub types: Vec<Type>,
    pub type_map: HashMap<String, usize>,
}

struct CheckerContext {
    loop_count: isize, // not usize to get useful error messages in debug mode, not "usize >= 0"
}

pub struct Checker {
    ctx: CheckerContext,
    stack: Vec<SemVariable>,
    var_map: HashMap<String, usize>,
    types: Vec<Type>,
    type_map: HashMap<String, usize>,
}

fn new_base(ident: &str, width: usize, type_mode: TypeMode) -> Type {
    Type {
        ident: ident.to_string(),
        width,
        form: TypeForm::Base { type_mode },
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
            new_base("bool", 1, TypeMode::Bool),
            new_base("u8", 1, TypeMode::Int { signed: false }),
            new_base("u16", 2, TypeMode::Int { signed: false }),
            new_base("u32", 4, TypeMode::Int { signed: false }),
            new_base("u64", 8, TypeMode::Int { signed: false }),
            new_base("usize", 8, TypeMode::Int { signed: false }),
            new_base("i8", 1, TypeMode::Int { signed: true }),
            new_base("i16", 2, TypeMode::Int { signed: true }),
            new_base("i32", 4, TypeMode::Int { signed: true }),
            new_base("i64", 8, TypeMode::Int { signed: true }),
            new_base("isize", 8, TypeMode::Int { signed: true }),
            new_base("f32", 4, TypeMode::Int { signed: true }),
            new_base("f64", 8, TypeMode::Int { signed: true }),
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
                let var_type = self.types.get(type_id).unwrap();

                let mut width = var_type.width;
                let addr_mode = if ptr {
                    width = 8;
                    AddressingMode::Pointer
                } else {
                    AddressingMode::Primitive
                };

                let mut var = SemVariable {
                    width,
                    ident,
                    type_id,
                    mutable,
                    addr_mode,
                    init_expr,
                };

                self.var_map.insert(var.ident.clone(), self.stack.len());
                self.stack.push(var.clone()); // have to clone as am creating a new NodeStmt.

                if let Some(ref expr) = var.init_expr {
                    let checked = self.check_expr(expr)?;

                    if var.addr_mode != checked.addr_mode {
                        debug!(
                            "Init Expr of different AddrMode! {:?} vs {:?}",
                            var.addr_mode, checked.addr_mode
                        );
                        return err!("Init Expr of different AddrMode! => {var:#?}\n.. {expr:#?}");
                    }

                    match &self.types.get(var.type_id).unwrap().form {
                        TypeForm::Base {
                            type_mode: var_type_mode,
                        } => {
                            if var_type_mode != &checked.type_mode {
                                debug!(
                                    "Init Expr of different Type! {:?} vs {:?}",
                                    var_type_mode, checked.type_mode
                                );
                                return err!(
                                    "Init Expr of different Type! => {var:#?}\n.. {expr:#?}"
                                );
                            }
                        }
                        TypeForm::Struct { member_ids } => {
                            todo!("Struct semantics un-implemented")
                        }
                        TypeForm::Union {} => todo!("Union semantics un-implemented"),
                    }
                }

                return Ok(NodeStmt::SemVarDecl(var));
            }
            NodeStmt::If {
                condition,
                scope,
                branches,
            } => {
                let checked = self.check_expr(&condition)?;
                match checked.type_mode {
                    TypeMode::Bool => (),
                    _ => return err!("'If' statement condition not 'boolean'\n{condition:#?}"),
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
                let checked = self.check_expr(&condition)?;
                match checked.type_mode {
                    TypeMode::Bool => (),
                    _ => return err!("'ElseIf' statement condition not 'boolean'\n{condition:#?}"),
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

                // Mutability Check
                if !var.mutable {
                    return err!("Re-assignment of a Constant:\n{var:#?}");
                }

                // Addressing Mode Check
                let checked = self.check_expr(expr)?;
                if var.addr_mode != checked.addr_mode {
                    debug!(
                        "Init Expr of different AddrMode! {:?} vs {:?}",
                        var.addr_mode, checked.addr_mode
                    );
                    return err!("Init Expr of different AddrMode! => {var:#?}\n.. {expr:#?}");
                }

                // Type Mode Check
                match &self.types.get(var.type_id).unwrap().form {
                    TypeForm::Base {
                        type_mode: var_type_mode,
                    } => {
                        if var_type_mode != &checked.type_mode {
                            debug!(
                                "Init Expr of different Type! {:?} vs {:?}",
                                var_type_mode, checked.type_mode
                            );
                            return err!("Init Expr of different Type! => {var:#?}\n.. {expr:#?}");
                        }
                    }
                    TypeForm::Struct { member_ids } => {
                        todo!("Struct semantics un-implemented")
                    }
                    TypeForm::Union {} => todo!("Union semantics un-implemented"),
                }

                // Type Narrowing Check
                // Literals have no width, conform to when expr is wider than variable
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

    // fn check_expr(&self, expr: &NodeExpr) -> Result<(TypeFlags, FormFlags), String> {
    //     match expr {
    //         NodeExpr::BinaryExpr { op, lhs, rhs } => {
    //             let (ltype, lform) = self.check_expr(&*lhs)?;
    //             let (rtype, rform) = self.check_expr(&*rhs)?;
    //             let op_dbg = format!(
    //                 "\n\n{sep}BinExpr{sep}\n\
    //                  {op:?} =>\
    //                 \n LHS({lname}): {ltype:?} .. {lform:?}\
    //                 \n RHS({rname}): {ltype:?} .. {lform:?}",
    //                 sep = "-".repeat(5),
    //                 lname = self.get_expr_ident(&*lhs, false),
    //                 rname = self.get_expr_ident(&*rhs, true)
    //             );
    //             if LOG_DEBUG_INFO {
    //                 println!("{op_dbg}")
    //             }
    //             if !self.check_type_equality(ltype, rtype) {
    //                 return err!("Mismatched operand types =>\n{op_dbg}");
    //             }
    //             // cmp        type, type => bool
    //             // logical    bool, bool => bool
    //             // arithmetic int,  int  => int
    //             let op_flags = op.get_flags();
    //             match op_flags {
    //                 _ if op_flags.contains(TokenFlags::CMP) => {
    //                     Ok((TypeFlags::BOOL, FormFlags::PRIMITIVE))
    //                 }
    //                 _ if op_flags.contains(TokenFlags::LOG) => {
    //                     if ltype.contains(TypeFlags::INT) {
    //                         return err!("Mismatched binary operation =>\n{op_dbg}");
    //                     }
    //                     Ok((TypeFlags::BOOL, FormFlags::PRIMITIVE))
    //                 }
    //                 _ if op_flags.contains(TokenFlags::ARITH) => {
    //                     if ltype.contains(TypeFlags::BOOL) {
    //                         return err!("Mismatched binary operation =>\n{op_dbg}");
    //                     }
    //                     Ok((ltype, lform))
    //                 }
    //                 _ => err!("Unsupported binary expression =>\n{op_dbg}"),
    //             }
    //         }
    //         NodeExpr::UnaryExpr { op, operand } => {
    //             let (typeflags, formflags) = self.check_expr(&*operand)?;
    //             let op_dbg = format!(
    //                 "\n\n{sep}UnaryExpr{sep}\n\
    //                  {op:?} =>\
    //                  \n OPERAND({name}): {typeflags:?} .. {formflags:?}",
    //                 sep = "-".repeat(5),
    //                 name = self.get_expr_ident(&*operand, false)
    //             );
    //             if LOG_DEBUG_INFO {
    //                 println!("{op_dbg}")
    //             }
    //             // 'Unary sub' int(lit) => int | signed
    //             // 'Cmp Not'   bool => bool
    //             // 'Addr of'   var => ptr
    //             // 'Ptr Deref' ptr => var
    //             match op {
    //                 TokenKind::Sub => {
    //                     if !typeflags.contains(TypeFlags::INT) {
    //                         return err!("'-' unary operator only works on integers =>\n{op_dbg}");
    //                     }
    //                     Ok((TypeFlags::INT | TypeFlags::SIGNED, formflags))
    //                 }
    //                 TokenKind::CmpNot => {
    //                     if !typeflags.contains(TypeFlags::BOOL) {
    //                         return err!("'!' operator only works on booleans =>\n{op_dbg}");
    //                     }
    //                     Ok((TypeFlags::BOOL, formflags))
    //                 }
    //                 TokenKind::Ampersand => match &**operand {
    //                     NodeExpr::Term(NodeTerm::Ident(ident))
    //                         if !formflags.contains(FormFlags::PTR) =>
    //                     {
    //                         let var = self.get_var(ident.value.as_ref().unwrap().as_str())?;
    //                         Ok((typeflags, FormFlags::PTR))
    //                     }
    //                     _ => {
    //                         err!("'&' operator requires expr to have a memory address =>\n{op_dbg}")
    //                     }
    //                 },
    //                 TokenKind::Ptr => {
    //                     if !formflags.contains(FormFlags::PTR) {
    //                         return err!(
    //                             "{operand:#?}\n'^' operator requires expr to be a pointer\n{op_dbg}"
    //                         );
    //                     }
    //                     // TODO(TOM): currently blind yolo deref the mem address, could be nullptr!
    //                     // .. could dis-allow ptr arith, only "&VAR_NAME" to get ptr
    //                     let without_ptr = formflags & !FormFlags::PTR;
    //                     Ok((typeflags, without_ptr))
    //                 }
    //                 _ => err!("Unsupported Unary Expression:{op_dbg}"),
    //             }
    //         }
    //         NodeExpr::Term(term) => self.check_term(term),
    //     }
    // }

    fn check_expr(&self, expr: &NodeExpr) -> Result<ExprData, String> {
        match expr {
            NodeExpr::BinaryExpr { op, lhs, rhs } => {
                let ldata = self.check_expr(lhs)?;
                let rdata = self.check_expr(rhs)?;

                // if TypeMode is equal
                // if either is literal
                // if both have same sign
                if ldata.type_mode != rdata.type_mode {
                    let mut literal_found = false;
                    if let ExprForm::Literal { inherited_width } = ldata.form {
                        literal_found = true;
                    } else if let ExprForm::Literal { inherited_width } = rdata.form {
                        literal_found = true;
                    }
                    if literal_found {
                        let sign_match = match ldata.type_mode {
                            TypeMode::Int { signed: lsign } | TypeMode::Float { signed: lsign } => {
                                let rsign = match rdata.type_mode {
                                    TypeMode::Int { signed } | TypeMode::Float { signed } => signed,
                                    TypeMode::Bool => false,
                                };
                                lsign == rsign
                            }
                            TypeMode::Bool => false,
                        };
                        if !sign_match {
                            return err!("Mismatched operand types =>\n");
                        }
                    }
                }

                // cmp        type, type => bool
                // logical    bool, bool => bool
                // arithmetic int,  int  => int
                let op_flags = op.get_flags();
                match op_flags {
                    _ if op_flags.contains(TokenFlags::CMP) => Ok(ExprData {
                        type_mode: TypeMode::Bool,
                        addr_mode: AddressingMode::Primitive,
                        form: ExprForm::Literal { inherited_width: 8 },
                    }),
                    _ if op_flags.contains(TokenFlags::LOG) => match ldata.type_mode {
                        TypeMode::Bool => Ok(ExprData {
                            type_mode: TypeMode::Bool,
                            addr_mode: AddressingMode::Primitive,
                            form: ExprForm::Literal { inherited_width: 8 },
                        }),
                        TypeMode::Int { signed } | TypeMode::Float { signed } => {
                            err!("Cannot perform logical operation on numerical types")
                        }
                    },
                    _ if op_flags.contains(TokenFlags::ARITH) => match ldata.type_mode {
                        TypeMode::Bool => {
                            err!("Cannot perform arithmetic operation on boolean types.")
                        }
                        TypeMode::Int { signed } | TypeMode::Float { signed } => Ok(ExprData {
                            type_mode: ldata.type_mode,
                            addr_mode: ldata.addr_mode,
                            form: ExprForm::Literal { inherited_width: 8 },
                        }),
                    },
                    _ => err!("Unsupported binary expression =>\n{lhs:#?}\n..\n{rhs:#?}"),
                }
            }
            NodeExpr::UnaryExpr { op, operand } => todo!("Unary expr semantics un-implemented"),
            NodeExpr::Term(term) => self.check_term(term),
        }
    }

    fn check_term(&self, term: &NodeTerm) -> Result<ExprData, String> {
        match term {
            NodeTerm::Ident(tok) => {
                let var = self.get_var(tok.value.as_ref().unwrap().as_str())?;
                match self.types.get(var.type_id).unwrap().form {
                    TypeForm::Base { type_mode } => Ok(ExprData {
                        type_mode,
                        addr_mode: var.addr_mode,
                        form: ExprForm::Variable {
                            ptr: NonNull::new(unsafe {
                                var as *const SemVariable as *mut SemVariable // Really dumb..
                            })
                            .unwrap_or(
                                return err!("Found nullptr when creating 'ExprData'\n{var:#?}"),
                            ),
                        },
                    }),
                    TypeForm::Struct { ref member_ids } => err!("Struct Semantics un-implemented"),
                    TypeForm::Union {} => err!("Union semantics un-implemented"),
                }
            }
            NodeTerm::IntLit(_) => Ok(ExprData {
                type_mode: TypeMode::Int { signed: false },
                addr_mode: AddressingMode::Primitive,
                form: ExprForm::Literal { inherited_width: 8 }, // TODO(TOM): hardcoded val
            }),
        }
    }

    //////////////////////////////////////////////////////////////////////////////////////////

    fn get_expr_ident(&self, expr: &NodeExpr, right_side: bool) -> String {
        match expr {
            NodeExpr::BinaryExpr { op, lhs, rhs } => {
                if right_side {
                    self.get_expr_ident(&*rhs, false)
                } else {
                    self.get_expr_ident(&*lhs, false)
                }
            }
            NodeExpr::UnaryExpr { op, operand } => self.get_expr_ident(&*operand, false),
            NodeExpr::Term(term) => match term {
                NodeTerm::IntLit(tok) | NodeTerm::Ident(tok) => tok.value.as_ref().unwrap().clone(),
            },
        }
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

    fn add_type(&mut self, new_type: Type) {
        self.type_map
            .insert(new_type.ident.clone(), self.types.len());
        self.types.push(new_type);
    }
}
