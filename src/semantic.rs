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
//      Type Coersion:
//          - currently any literal can be coerced!
//          - new ExprForm, 'expression' a combination of literal's and variables.
//      CRAZY IDEA!!: (bad idea)
//          - every combination of typemode, addressing mode, exprform shoved in a table for direct comparison and lookup!

use crate::{
    debug, err,
    lex::{TokenFlags, TokenKind},
    parse::{NodeExpr, NodeScope, NodeStmt, NodeTerm, AST},
};
use std::{
    cell::UnsafeCell,
    collections::HashMap,
    ptr::{self, NonNull},
};

const PTR_WIDTH: usize = 8;
const LOG_DEBUG_INFO: bool = false;
const MSG: &'static str = "SEMANTIC";

// Bool    | Int | Float
// Signed  | Unsigned
// Literal | Variable
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TypeMode {
    Bool,
    Int { signed: bool },
    Float { signed: bool },
    IntLit,
}

// TODO(TOM): this isn't correct im sure.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum AddressingMode {
    Primitive,
    Pointer, // not need an underlying addressing mode?
    Array,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExprForm {
    Variable { ptr: NonNull<SemVariable> },
    Expr { inherited_width: usize },
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
    pub mutable: bool,
    pub ident: String,
    pub type_id: usize,
    pub addr_mode: AddressingMode,
    pub init_expr: Option<NodeExpr>,
}

//////////////////////////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone, PartialEq)]
pub struct HandoffData {
    pub ast: AST,
    pub types: Vec<Type>,
    pub type_map: HashMap<String, usize>,
}

pub struct Checker {
    loop_count: isize, // not usize to get useful error messages in debug mode, not "usize >= 0"
    pos: (usize, usize),
    stack: Vec<SemVariable>,
    var_map: HashMap<String, usize>,
    types: Vec<Type>,
    type_map: HashMap<String, usize>,
}

impl Checker {
    pub fn check_ast(mut ast: AST) -> Result<HandoffData, String> {
        let types = Vec::from([
            new_base("bool", 1, TypeMode::Bool),
            new_base("u8", 1, TypeMode::Int { signed: false }),
            new_base("u16", 2, TypeMode::Int { signed: false }),
            new_base("u32", 4, TypeMode::Int { signed: false }),
            new_base("u64", PTR_WIDTH, TypeMode::Int { signed: false }),
            new_base("usize", PTR_WIDTH, TypeMode::Int { signed: false }),
            new_base("i8", 1, TypeMode::Int { signed: true }),
            new_base("i16", 2, TypeMode::Int { signed: true }),
            new_base("i32", 4, TypeMode::Int { signed: true }),
            new_base("i64", PTR_WIDTH, TypeMode::Int { signed: true }),
            new_base("isize", PTR_WIDTH, TypeMode::Int { signed: true }),
            new_base("f32", 4, TypeMode::Int { signed: true }),
            new_base("f64", PTR_WIDTH, TypeMode::Int { signed: true }),
        ]);
        let mut checker = Checker {
            loop_count: 0,
            pos: (0, 0),
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
                    width = PTR_WIDTH;
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
                    match self.check_assign(&var, &checked) {
                        Ok(_) => (),
                        Err(e) => return err!(self, "INIT EXPR:\n{e}"),
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
                    _ => {
                        return err!(
                            self,
                            "'If' statement condition not 'boolean'\n{condition:#?}"
                        )
                    }
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
                    _ => {
                        return err!(
                            self,
                            "'ElseIf' statement condition not 'boolean'\n{condition:#?}"
                        )
                    }
                }

                return Ok(NodeStmt::ElseIf {
                    condition,
                    scope: self.check_scope(scope)?,
                });
            }
            NodeStmt::Else(scope) => return Ok(NodeStmt::Else(self.check_scope(scope)?)),
            NodeStmt::While { condition, scope } => {
                self.loop_count += 1;
                self.check_expr(&condition)?;
                let new_scope = self.check_scope(scope)?;
                self.loop_count -= 1;
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
                    return err!(self, " Re-assignment of a Constant:\n{var:#?}");
                }
                let checked = self.check_expr(expr)?;
                self.check_assign(var, &checked)?;
            }
            NodeStmt::Exit(ref expr) => {
                self.check_expr(&expr)?;
            }
            NodeStmt::NakedScope(scope) => {
                return Ok(NodeStmt::NakedScope(self.check_scope(scope)?));
            }
            NodeStmt::Break => {
                if self.loop_count <= 0 {
                    return err!(self, " Not inside a loop! cannot break");
                }
            }
            NodeStmt::SemVarDecl { .. } => return err!(self, " Found {stmt:#?}.. shouldn't have."),
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
        debug!(self, " Ending scope, pop({pop_amt})");
        for _ in 0..pop_amt {
            let popped_var = match self.stack.pop() {
                Some(var) => self.var_map.remove(var.ident.as_str()),
                None => return err!(self, " uhh.. scope messed up"),
            };
            debug!(self, " Scope ended, removing {popped_var:#?}");
        }
        Ok(NodeScope {
            stmts,
            inherits_stmts: true,
        })
    }

    fn check_expr(&self, expr: &NodeExpr) -> Result<ExprData, String> {
        match expr {
            NodeExpr::BinaryExpr { op, lhs, rhs } => {
                let ldata = self.check_expr(lhs)?;
                let rdata = self.check_expr(rhs)?;
                debug!(self, " lhs: {ldata:#?}\nrhs: {rdata:#?}");

                // Binary expressions disallowed for arrays!
                match ldata.addr_mode {
                    AddressingMode::Primitive | AddressingMode::Pointer => match rdata.addr_mode {
                        AddressingMode::Primitive | AddressingMode::Pointer => (),
                        AddressingMode::Array => {
                            return err!(self, " Binary Expressions invalid for Arrays")
                        }
                    },
                    AddressingMode::Array => {
                        return err!(self, " Binary Expressions invalid for Arrays")
                    }
                }

                let err_msg = format!("Expr of different Type! => {ldata:#?}\n.. {rdata:#?}");
                self.check_type_mode(&ldata, &rdata, &err_msg)?;

                // cmp        type, type => bool
                // logical    bool, bool => bool
                // arithmetic int,  int  => int
                let op_flags = op.get_flags();
                match op_flags {
                    _ if op_flags.contains(TokenFlags::CMP) => Ok(ExprData {
                        type_mode: TypeMode::Bool,
                        addr_mode: AddressingMode::Primitive,
                        form: ExprForm::Expr {
                            inherited_width: PTR_WIDTH,
                        },
                    }),
                    _ if op_flags.contains(TokenFlags::LOG) => match ldata.type_mode {
                        TypeMode::Int { .. } | TypeMode::Float { .. } | TypeMode::IntLit => {
                            err!(self, " '{op:?}' requires expr to be a boolean")
                        }
                        TypeMode::Bool => Ok(ExprData {
                            type_mode: TypeMode::Bool,
                            addr_mode: AddressingMode::Primitive,
                            form: ExprForm::Expr {
                                inherited_width: PTR_WIDTH,
                            },
                        }),
                    },
                    _ if op_flags.contains(TokenFlags::ARITH) => match ldata.type_mode {
                        TypeMode::Bool => {
                            err!(self, "'{op:?}' requires expr to be an integer or float")
                        }
                        TypeMode::Int { .. } | TypeMode::Float { .. } | TypeMode::IntLit => {
                            Ok(ExprData {
                                type_mode: ldata.type_mode,
                                addr_mode: ldata.addr_mode,
                                form: ExprForm::Expr {
                                    inherited_width: PTR_WIDTH,
                                },
                            })
                        }
                    },
                    _ => err!(
                        self,
                        "Unsupported binary expression =>\n{lhs:#?}\n..\n{rhs:#?}"
                    ),
                }
            }
            NodeExpr::UnaryExpr { op, operand } => {
                let checked = self.check_expr(&*operand)?;
                debug!(self, " {checked:#?}");

                // 'Unary sub' signed int or lit => int | signed
                // 'Cmp Not'   bool => bool
                // 'Addr of'   var => ptr
                // 'Ptr Deref' ptr => var
                let inherited_width = match checked.form {
                    ExprForm::Variable { ptr } => unsafe { (*ptr.as_ptr()).width },
                    ExprForm::Expr { inherited_width } => inherited_width,
                };
                match op {
                    TokenKind::Sub => match checked.type_mode {
                        TypeMode::Int { signed } | TypeMode::Float { signed } if signed => {
                            Ok(ExprData {
                                type_mode: TypeMode::Int { signed },
                                addr_mode: AddressingMode::Primitive,
                                form: ExprForm::Expr { inherited_width },
                            })
                        }
                        TypeMode::IntLit => Ok(ExprData {
                            type_mode: TypeMode::Int { signed: true },
                            addr_mode: AddressingMode::Primitive,
                            form: ExprForm::Expr { inherited_width },
                        }),
                        _ => {
                            err!(self, " '-' unary operator requires expr to be a signed integers =>\n{checked:#?}")
                        }
                    },
                    TokenKind::CmpNot => match checked.type_mode {
                        TypeMode::Bool => Ok(ExprData {
                            type_mode: TypeMode::Bool,
                            addr_mode: AddressingMode::Primitive,
                            form: ExprForm::Expr { inherited_width },
                        }),
                        _ => err!(self, " '!' unary operator requires expr to be a boolean =>\n{checked:#?}"),
                    },
                    TokenKind::Ampersand => match checked.addr_mode {
                        AddressingMode::Pointer => err!(
                            self, "'&' unary operator requires expr to have a memory address =>\n{checked:#?}"
                        ),
                        AddressingMode::Primitive => match checked.form {
                            ExprForm::Variable { ptr } => Ok(ExprData {
                                type_mode: checked.type_mode,
                                addr_mode: AddressingMode::Pointer,
                                form: ExprForm::Expr {
                                    inherited_width: PTR_WIDTH,
                                },
                            }),
                            _ => err!(self, " '&' unary operator requires expr to be a memory address."),
                        },
                        _ => err!(self, " '&' unary operator not supported for Arrays"),
                    },
                    TokenKind::Ptr => match checked.addr_mode {
                        AddressingMode::Pointer => Ok(ExprData {
                            type_mode: checked.type_mode,
                            addr_mode: AddressingMode::Primitive,
                            form: ExprForm::Expr { inherited_width }, // TODO(TOM): not sure about this?
                        }),
                        _ => err!(self, " '^' unary operator requires expr to be a pointer\n{checked:#?}"),
                    },
                    _ => err!(self, " Unsupported Unary Expression:{checked:#?}"),
                }
            }
            NodeExpr::Term(term) => self.check_term(term),
        }
    }

    fn check_term(&self, term: &NodeTerm) -> Result<ExprData, String> {
        match term {
            NodeTerm::Ident(tok) => {
                unsafe {
                    let mut_self = self as *const Checker as *mut Checker;
                    (*mut_self).pos = tok.pos;
                }
                let var = self.get_var(tok.value.as_ref().unwrap().as_str())?;
                match self.types.get(var.type_id).unwrap().form {
                    TypeForm::Base { type_mode } => Ok(ExprData {
                        type_mode,
                        addr_mode: var.addr_mode,
                        form: ExprForm::Variable {
                            ptr: self.new_nonnull(var)?,
                        },
                    }),
                    TypeForm::Struct { ref member_ids } => {
                        err!(self, " Struct Semantics un-implemented")
                    }
                    TypeForm::Union {} => err!(self, " Union semantics un-implemented"),
                }
            }
            NodeTerm::IntLit(_) => Ok(ExprData {
                type_mode: TypeMode::IntLit,
                addr_mode: AddressingMode::Primitive,
                form: ExprForm::Expr {
                    inherited_width: PTR_WIDTH,
                },
            }),
        }
    }

    fn check_var_ident(&self, ident: &str) -> Result<(), String> {
        if self.var_map.contains_key(ident) {
            return err!(self, "Attempted re-initialisation of a Variable: '{ident}'");
        } else if self.type_map.contains_key(ident) {
            return err!(self, "Illegal Variable name, Types are keywords: '{ident}'");
        }
        Ok(())
    }

    fn check_type_mode(&self, data1: &ExprData, data2: &ExprData, msg: &str) -> Result<(), String> {
        if data1 == data2 {
            return Ok(());
        }

        // Check sign equality
        let sign_match = match data1.type_mode {
            TypeMode::IntLit => return Ok(()),
            TypeMode::Int { signed: sign1 } | TypeMode::Float { signed: sign1 } => {
                match data2.type_mode {
                    TypeMode::IntLit => return Ok(()),
                    TypeMode::Int { signed: sign2 } | TypeMode::Float { signed: sign2 } => {
                        sign1 == sign2
                    }
                    TypeMode::Bool => false,
                }
            }
            TypeMode::Bool => false,
        };
        match sign_match {
            true => Ok(()),
            false => {
                debug!(
                    self,
                    "Expr sign mismatch! {:?} vs {:?}", data1.type_mode, data2.type_mode
                );
                err!(self, " {msg}")
            }
        }
    }

    fn check_assign(&self, var: &SemVariable, checked: &ExprData) -> Result<(), String> {
        // Check Addressing Mode
        if var.addr_mode != checked.addr_mode {
            debug!(
                self,
                "Expr of different AddrMode! {:?} vs {:?}", var.addr_mode, checked.addr_mode
            );
            return err!(
                self,
                "Expr of different AddrMode! => {var:#?}\n.. {checked:#?}"
            );
        }

        // Check Type Mode
        match &self.types.get(var.type_id).unwrap().form {
            TypeForm::Base {
                type_mode: var_type_mode,
            } => {
                let msg = format!("Expr of different Type! => {var:#?}\n.. {checked:#?}");
                let temp = ExprData {
                    type_mode: *var_type_mode,
                    addr_mode: var.addr_mode,
                    form: ExprForm::Variable {
                        ptr: self.new_nonnull(var)?,
                    },
                };
                self.check_type_mode(&temp, &checked, msg.as_str())
            }
            TypeForm::Struct { member_ids } => {
                todo!("Struct semantics un-implemented")
            }
            TypeForm::Union {} => todo!("Union semantics un-implemented"),
        }

        // TODO(TOM): Check Type Narrowing
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

    fn get_var(&self, ident: &str) -> Result<&SemVariable, String> {
        match self.var_map.get(ident) {
            Some(idx) => Ok(self.stack.get(*idx).unwrap()),
            None => err!(self, " Variable: {ident:?} doesn't exist."),
        }
    }

    fn get_type_id(&self, ident: &str) -> Result<usize, String> {
        match self.type_map.get(ident) {
            Some(id) => Ok(*id),
            None => err!(self, " Type '{ident}' not found"),
        }
    }

    fn add_type(&mut self, new_type: Type) {
        self.type_map
            .insert(new_type.ident.clone(), self.types.len());
        self.types.push(new_type);
    }

    fn new_nonnull(&self, reference: &SemVariable) -> Result<NonNull<SemVariable>, String> {
        match NonNull::new(reference as *const SemVariable as *mut SemVariable) {
            Some(ptr) => Ok(ptr),
            None => err!(
                self,
                "Found nullptr when creating 'ExprData'\n{reference:#?}"
            ),
        }
    }
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
