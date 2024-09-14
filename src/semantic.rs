// >>SEMANTIC<< The rules of the language, not grammar or syntax!
//  ✅ CLONING:
//      - AST isn't a tree, contiguous "NodeStmt" Unions, some contain boxed data but not much
//      - if everything is a ptr "box", manipulating data MUCH easier, borrow checker not angry at me!
//      - Can't manipulate current AST freely, because it has to be rigid in size, its on the vars!
//  ✅ Assignment:
//      - ✅ arith-assign on new var or literal
//      - ✅ re-assign on immutable vars
//  ✅ Types:
//      ✅ Operators expect specific types.
//          - "bool PLUS u8" doesn't compile
//          - LogicalAnd: bool, UnaryMinus: signed int
//      ✅ Type Conversions
//          - Implicit: integers being converted to a larger integer, e.g u16 = u8
//          - Explicit: Everything else, using syntax: type_x as type_y
//      ❌ Integer Bounds Checks
//          - requires me to interpret every arith expression? let it be ub for now :)
//      ✅ IntegerLitereal Coercion
//          - its not a concrete type and can be coerced into any integer, after bounds checked.
//      ✅ Pointers
//          - always have usize, not a defined type but an attribute, that modifies byte_size?
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
//      ✅ Structure Revision: Either passing Variable or Literal
//          - Both: TypeMode, AddressingMode, e.g Boolean Array
//          - Var: ptr to the var
//          - Literal: Inherited Width
//          - TypeMode:
//              - What operations can be performed
//              - (OPTIONAL): Sign, if numerical
//          - AddressingMode:
//              - how is it represented in memory, if at all
//              - (OPTIONAL): mutability, if represented in memory
//      ✅ Type Narrowing:
//          - check if the assigned expr is wider than the assignee variable
//      ✅ Type Coersion: (check_assign() IS type coersion, if the 2 types don't  deviate too far, e.g narrowing, addr mode its coerced. TYPES DON'T EXIST!)
//          - Literals can be coerced into a type of same mode and addressing mode
//          - Expressions and Variables are unable to be coerced whatsoever, an explicit cast must take place.

//  ❌ ExprData Rethink (removal):
//      - Consolidate TypeMode & AddresingMode to ExprForm::Expr
//          - because ExprForm::Var holds a Variable,
//          - Variable has a type (which has a typemode) & addressingmode
//      - Con: lots of indirection faff
//      - TypeForm not accounted for properly!! ExprData needs TypeForm, not type mode !!

//  ✅ Cpp Style Function overriding:
//      - match function uniqueness based on its "signature" (name + argument types).
//      - e.g func123(int,bool) != func123(int). UNIQUE!

use crate::{
    debug, err,
    lex::{Token, TokenFlags, TokenKind},
    parse::{NodeExpr, NodeScope, NodeStmt, NodeTerm, AST},
};
use std::{
    collections::{HashMap, HashSet},
    ptr::NonNull,
};

pub type Byte = usize;
const PTR_WIDTH: Byte = 8;
const LOG_DEBUG_INFO: bool = true;
const MSG: &'static str = "SEMANTIC";

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum AddressingMode {
    Primitive,
    Pointer,
    Array,
}
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]

pub enum TypeMode {
    Void,
    Bool,
    IntLit,
    Int { signed: bool },
    Float { signed: bool },
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum TypeForm {
    Base {
        type_mode: TypeMode,
    }, // Base: just a type, has some flags, chill.
    Struct {
        member_ids: Vec<ExprData>,
    }, // Struct: a group of types, stores type id, not type.
    Union {
        // TODO(TOM): define later
    }, // Union: a group of types that share the same storage.
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum ExprForm {
    Variable { ptr: NonNull<SemVariable> },
    Expr { inherited_width: Byte },
    // Expr {
    //     inherited_width: Byte,
    //     type_mode: TypeMode,
    //     addr_mode: AddressingMode,
    // },
}

// TODO(TOM): re-work to use TypeForm..
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct ExprData {
    // pub type_form: TypeForm,
    pub type_mode: TypeMode,
    pub addr_mode: AddressingMode,
    pub form: ExprForm,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Type {
    pub width: Byte,
    pub ident: String,
    pub form: TypeForm,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum InitExpr {
    Some(NodeExpr),
    None,
    Deferred,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct SemVariable {
    pub ident: Token,
    pub mutable: bool,
    pub width: Byte,
    pub scope_id: usize,
    pub type_id: usize, // Can get a TypeMode from this
    pub addr_mode: AddressingMode,
    pub init_expr: InitExpr,
}

// need name, return semantics, arg semantics
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct SemFn {
    pub signature: String,
    pub scope: NodeScope,
    pub arg_semantics: Vec<SemVariable>, // treat like semantic variables ??
    pub return_type_id: Option<usize>,
    pub return_type_data: Option<ExprData>,
}

struct SemContext {
    loop_count: isize, // not usize to get useful error messages in debug build, instead of oob error
    cur_scope_id: usize,
    scope_inherit_bounds_id: Option<usize>,

    valid_return: bool,
    function_decl_name: Option<String>,
    return_type_tok: Option<Token>,
    return_type_id: Option<usize>,
    return_type_data: Option<ExprData>,
}

pub struct Checker {
    pub ast: AST,
    ctx: SemContext,
    pos: (u32, u32),
    pub types: Vec<Type>,
    pub fn_map: HashMap<String, SemFn>,
    vars: Vec<SemVariable>,
    var_map: HashMap<String, usize>,
    pub type_map: HashMap<String, usize>,
}

impl Checker {
    pub fn check_ast(ast: AST) -> Result<Checker, String> {
        let types = Vec::from([
            new_base("void", 0, TypeMode::Void),
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
            ast: AST { stmts: Vec::new() },
            ctx: SemContext {
                loop_count: 0,
                cur_scope_id: 0,
                scope_inherit_bounds_id: None,
                valid_return: false,
                function_decl_name: None,
                return_type_tok: None,
                return_type_id: None,
                return_type_data: None,
            },
            pos: (0, 0),
            vars: Vec::new(),
            fn_map: HashMap::new(),
            var_map: HashMap::new(),
            types,
            type_map: HashMap::new(),
        };

        for (n, base) in checker.types.iter().enumerate() {
            checker.type_map.insert(base.ident.clone(), n);
        }

        let mut sem_ast = AST { stmts: Vec::new() };
        for stmt in ast.stmts {
            sem_ast.stmts.push(checker.check_top_level(stmt)?);
        }
        checker.ast = sem_ast;

        // TODO(TOM): for ref, cpp "main" function either:
        //      - takes no arguments, main().
        //      - takes 2 arguments, main(int argc, char* argv[]).
        //          - argc: amount of arguments given when the program is run (cmd line!)
        //          - argv: an array of length argc+1, each pointer points to a null terminated char[]
        // Instead: use one array with a length
        // Checking for the Entry Point
        match checker.fn_map.get("main") {
            Some(func) if !func.arg_semantics.is_empty() => {
                err!(
                    &checker,
                    "The 'main' function takes no arguments =>\nremove {:#?}",
                    func.arg_semantics
                )
            }
            None => {
                err!(
                    &checker,
                    "No entry point for the program found. Add a 'main' function."
                )
            }
            _ => Ok(checker),
        }
    }

    fn check_top_level(&mut self, stmt: NodeStmt) -> Result<NodeStmt, String> {
        match stmt {
            NodeStmt::FnDecl {
                ident,
                args,
                scope,
                return_type_tok,
                return_addr_mode,
            } => {
                // check for name collisions
                let fn_ident = ident.as_str();
                if self.type_map.contains_key(fn_ident) {
                    return err!(
                        self,
                        "Illegal Function name, Types are reserved: '{fn_ident}'"
                    );
                }

                // Validate arguments' semantics.
                let mut arg_idents: Vec<String> = Vec::new();
                let mut arg_semantics = Vec::new();
                for arg in args {
                    let arg_ident = arg.ident.as_str();

                    // O(n^2) complexity.. funcs normally < ~6 params, so alright! use set otherwise
                    // need this to create signature, so can't give most accurate err msgs..
                    if arg_idents.contains(&arg.ident.value.as_ref().unwrap()) {
                        return err!(
                            "Duplicate argument name: '{arg_ident}' in function {fn_ident}"
                        );
                    } else if self.var_map.contains_key(arg_ident) {
                        return err!(
                            self,
                            "Argument name in use: {arg_ident} in function: {fn_ident}"
                        );
                    } else if self.type_map.contains_key(arg_ident) {
                        return err!(
                            self,
                            "Illegal argument name: {arg_ident} in function: {fn_ident}, Types are reserve keywords"
                        );
                    }

                    let type_id = self.get_type_id(&arg.type_tok.value.unwrap())?;
                    let type_ref = self.types.get(type_id).unwrap();

                    arg_semantics.push(SemVariable {
                        ident: arg.ident,
                        mutable: arg.mutable,
                        width: type_ref.width,
                        scope_id: self.ctx.cur_scope_id + 1, // haven't incremented yet, in check_scope()
                        type_id,
                        addr_mode: arg.addr_mode,
                        init_expr: InitExpr::None,
                    });
                    arg_idents.push(arg_semantics.last().unwrap().ident.as_str().to_string());
                }

                // Creates a function signature, to allow for overloading
                // e.g plus5(i32,i32)
                let signature = match ident.as_str() {
                    "main" => "main".to_owned(),
                    name @ _ => {
                        let mut str = String::new();
                        str += name;
                        str += "(";
                        for (i, arg) in arg_semantics.iter().enumerate() {
                            str += self.types.get(arg.type_id).unwrap().ident.as_str();
                            str += ",";
                        }
                        str.pop(); // removes extra ','
                        str + ")"
                    }
                };

                // check for name collisions with signature.
                if self.fn_map.contains_key(signature.as_str()) {
                    return err!(self, "Duplicate definition of a Function: '{signature}'");
                }

                // Set shared data, for self.check_stmt()'s
                self.ctx.function_decl_name = Some(signature.clone());
                self.ctx.return_type_tok = return_type_tok;
                match self.ctx.return_type_tok {
                    Some(ref ident) => {
                        let return_ident = ident.as_str();
                        let return_type_id = self.get_type_id(return_ident)?;
                        let return_type = self.types.get(return_type_id).unwrap();
                        let return_type_mode = match &return_type.form {
                            TypeForm::Base { type_mode } => *type_mode,
                            TypeForm::Struct { .. } => todo!("fn return struct"),
                            TypeForm::Union {} => todo!("fn return union"),
                        };

                        self.ctx.return_type_id = Some(return_type_id);
                        self.ctx.return_type_data = Some(ExprData {
                            type_mode: return_type_mode,
                            addr_mode: return_addr_mode.unwrap(),
                            form: ExprForm::Expr {
                                inherited_width: return_type.width,
                            },
                        });
                    }
                    None => {
                        self.ctx.return_type_id = None;
                        self.ctx.return_type_data = None;
                    }
                }

                // Create lambda for custom scope check
                let checked_scope;
                let mut_self = self as *const Checker as *mut Checker;
                let lambda = |stmts: Vec<NodeStmt>| -> Result<Vec<NodeStmt>, String> {
                    debug!(self, "checking {signature}'s statements!");
                    self.ctx.scope_inherit_bounds_id = Some(self.ctx.cur_scope_id);

                    for arg in &arg_semantics {
                        // var_map insertion first as vars.len() is 1 larger, but negated by 0-indexing!
                        self.var_map
                            .insert(arg.ident.value.as_ref().unwrap().clone(), self.vars.len());
                        self.vars.push(arg.clone());
                    }

                    let mut checked_stmts = Vec::new();
                    for stmt in stmts {
                        checked_stmts.push(self.check_stmt(stmt)?);
                        debug!(self, "added {:#?}", checked_stmts.last())
                    }

                    if !self.ctx.valid_return {
                        return err!(self, "Not all code paths return in '{signature}'");
                    }
                    checked_stmts.reverse();

                    // removes args for me! (check_scope() that is)
                    Ok(checked_stmts)
                };
                unsafe {
                    checked_scope = (*mut_self).check_scope(scope, Some(lambda))?;
                }

                // un-set shared data.
                self.ctx.function_decl_name = None;
                self.ctx.scope_inherit_bounds_id = None;
                self.fn_map.insert(
                    signature.clone(),
                    SemFn {
                        signature: signature.clone(),
                        scope: checked_scope,
                        arg_semantics,
                        return_type_id: self.ctx.return_type_id,
                        return_type_data: self.ctx.return_type_data,
                    },
                );

                Ok(NodeStmt::FnSemantics { signature })
            }
            _ => {
                self.check_stmt(stmt)
                // err!(
                //     self,
                //     "A Program only consists of functions, this is a {stmt:?}"
                // )
            }
        }
    }

    fn check_stmt(&mut self, stmt: NodeStmt) -> Result<NodeStmt, String> {
        match stmt {
            NodeStmt::VarDecl {
                init_expr,
                ident,
                type_tok,
                type_addr_mode,
                mutable,
            } => {
                // check for name collisions
                let str = ident.as_str();
                if self.var_map.contains_key(str) {
                    return err!(self, "Duplicate definition of a Variable: '{str}'");
                } else if self.type_map.contains_key(str) {
                    return err!(self, "Illegal Variable name, Types are reserved: '{str}'");
                }

                let type_id = self.get_type_id(type_tok.as_str())?;
                let var_type = self.types.get(type_id).unwrap();

                // change byte width if its a pointer
                let mut width = var_type.width;
                match type_addr_mode {
                    AddressingMode::Primitive => (),
                    AddressingMode::Pointer => width = PTR_WIDTH,
                    AddressingMode::Array => todo!("array byte width modifications"),
                }

                let var = SemVariable {
                    ident,
                    mutable,
                    width,
                    scope_id: self.ctx.cur_scope_id,
                    type_id,
                    addr_mode: type_addr_mode,
                    init_expr,
                };
                // insert into registry
                self.var_map
                    .insert(var.ident.value.as_ref().unwrap().clone(), self.vars.len());
                self.vars.push(var.clone());

                // check intial expression
                if let InitExpr::Some(ref expr) = var.init_expr {
                    let checked = self.check_expr(expr)?;
                    let var_data = ExprData {
                        type_mode: {
                            match &self.types.get(var.type_id).unwrap().form {
                                TypeForm::Base { type_mode } => *type_mode,
                                TypeForm::Struct { .. } => todo!(),
                                TypeForm::Union {} => todo!(),
                            }
                        },
                        addr_mode: var.addr_mode,
                        form: ExprForm::Variable {
                            ptr: self.new_nonnull(&var)?,
                        },
                    };
                    self.check_type_equivalence(&var_data, &checked)?;
                }

                Ok(NodeStmt::VarSemantics(var))
            }
            NodeStmt::FnDecl { .. } => {
                return err!(
                    self,
                    "Functions cannot be nested, they're top level statements"
                )
            }
            NodeStmt::Return(_) if self.ctx.function_decl_name.is_none() => {
                err!(self, "return not expected outside a function declaration.")
            }
            NodeStmt::Return(expr) if expr.is_some() => {
                let expr = expr.unwrap();
                let expr_type_data = self.check_expr(&expr)?;

                // check for return mismatch with void.
                let return_type = match self.ctx.return_type_tok {
                    Some(ref ident) => self.types.get(self.get_type_id(ident.as_str())?).unwrap(),
                    None => {
                        return err!(
                            self,
                            "Mismatched '{signature}' return, expected 'void', found =>\n'{expr_type_data:#?}'",
                            signature = self.ctx.function_decl_name.as_ref().unwrap(),
                        );
                    }
                };

                // checked prior to "check_type_equivalence" for better err message
                if expr_type_data.addr_mode != self.ctx.return_type_data.as_ref().unwrap().addr_mode
                {
                    return err!(self,"Mismatched function and return type, '{return_type:#?}'\n .. \n'{expr_type_data:#?}'");
                }
                self.check_type_equivalence(&self.ctx.return_type_data.unwrap(), &expr_type_data)?;
                self.ctx.valid_return = true;

                Ok(NodeStmt::ReturnSemantics {
                    expr: Some(expr_type_data),
                })
            }
            NodeStmt::Return(expr) => match &self.ctx.return_type_tok {
                Some(tok) => {
                    err!(
                        self,
                        "Mismatched function and return type, 'void'\n .. \n'{tok:#?}'"
                    )
                }
                _ => {
                    self.ctx.valid_return = true;
                    Ok(NodeStmt::ReturnSemantics { expr: None })
                }
            },
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
                        );
                    }
                }

                let checked_scope = self.check_scope_default(scope)?;
                let mut new_branches = Vec::new();
                for branch in branches {
                    new_branches.push(self.check_stmt(branch)?);
                }

                // Avoid function return semantics
                if self.ctx.function_decl_name.is_none() {
                    return Ok(NodeStmt::If {
                        condition,
                        scope: checked_scope,
                        branches: new_branches,
                    });
                }

                // if a return statement is present within the 'if' scope:
                // - check for an 'else'.
                //  - if present, a 'return' MUST be present.
                let found_return = checked_scope.stmts.iter().rev().find(|stmt| match stmt {
                    NodeStmt::ReturnSemantics { .. } => true,
                    _ => false,
                });

                if let Some(NodeStmt::Else(scope)) = new_branches.last() {
                    let found_return_else = scope.stmts.iter().rev().find(|stmt| match stmt {
                        NodeStmt::ReturnSemantics { .. } => true,
                        _ => false,
                    });
                    if found_return.is_some() != found_return_else.is_some() {
                        return err!(self, "An unconditional 'if' .. 'else if' statement must both return or neither:\nif: {found_return:#?}\nelse if: {found_return_else:#?}");
                    }
                }

                Ok(NodeStmt::If {
                    condition,
                    scope: checked_scope,
                    branches: new_branches,
                })
            }
            NodeStmt::ElseIf { condition, scope } => {
                let checked = self.check_expr(&condition)?;
                match checked.type_mode {
                    TypeMode::Bool => Ok(NodeStmt::ElseIf {
                        condition,
                        scope: self.check_scope_default(scope)?,
                    }),
                    _ => {
                        err!(
                            self,
                            "'ElseIf' statement condition not 'boolean'\n{condition:#?}"
                        )
                    }
                }
            }
            NodeStmt::Else(scope) => return Ok(NodeStmt::Else(self.check_scope_default(scope)?)),
            NodeStmt::While { condition, scope } => {
                self.ctx.loop_count += 1;
                self.check_expr(&condition)?;
                let new_scope = self.check_scope_default(scope)?;
                self.ctx.loop_count -= 1;

                Ok(NodeStmt::While {
                    condition,
                    scope: new_scope,
                })
            }
            NodeStmt::Assign {
                ref ident,
                ref expr,
            } => {
                let var = self.get_var(ident.as_str())?;
                let var_data = self.get_exprdata(var)?;
                if !var.mutable {
                    // if the variable is not initialised, this is the initialisation!
                    match var.init_expr {
                        InitExpr::None => {
                            let var_mut = self.get_var_mut(ident.as_str())?;
                            var_mut.init_expr = InitExpr::Deferred
                        }
                        _ => return err!(self, "Re-assignment of a Constant:\n{var:#?}"),
                    }
                }
                let checked = self.check_expr(expr)?;
                self.check_type_equivalence(&var_data, &checked)?;
                Ok(stmt)
            }
            NodeStmt::Exit(ref expr) => {
                self.check_expr(&expr)?;
                Ok(stmt)
            }
            NodeStmt::NakedScope(scope) => {
                Ok(NodeStmt::NakedScope(self.check_scope_default(scope)?))
            }
            NodeStmt::Break => {
                if self.ctx.loop_count <= 0 {
                    return err!(self, "Not inside a loop! cannot break");
                }
                Ok(stmt)
            }
            NodeStmt::VarSemantics { .. }
            | NodeStmt::FnSemantics { .. }
            | NodeStmt::ReturnSemantics { .. } => {
                err!(self, "Found {stmt:#?}.. shouldn't have.")
            }
        }
    }

    // 1. checks all stmts in scope
    // 2. once scope has ended, removes all variables confined to that scopes
    fn check_scope<F>(&mut self, scope: NodeScope, func: Option<F>) -> Result<NodeScope, String>
    where
        F: FnMut(Vec<NodeStmt>) -> Result<Vec<NodeStmt>, String>,
    {
        self.ctx.cur_scope_id += 1;
        let does_inherit = scope.inherits_stmts;
        if !does_inherit {
            self.ctx.scope_inherit_bounds_id = Some(self.ctx.cur_scope_id);
        }

        let stmts = match func {
            Some(mut lambda) => lambda(scope.stmts)?,
            None => {
                let mut stmts = Vec::new();
                for stmt in scope.stmts {
                    stmts.push(self.check_stmt(stmt)?);
                }
                stmts
            }
        };

        self.ctx.cur_scope_id -= 1;
        loop {
            match self.vars.last() {
                Some(var) if var.scope_id <= self.ctx.cur_scope_id => break,
                Some(var) => {
                    // debug!(self, "Scope ended, removing '{}'", var.ident.as_str());
                    let var = self.vars.pop().unwrap(); // assign for borrow checkers sake!
                    self.var_map.remove(var.ident.value.unwrap().as_str());
                }
                None => break,
            }
        }

        Ok(NodeScope {
            stmts,
            inherits_stmts: does_inherit,
        })
    }

    // Compiler doesn't understand type of 'None', so must hide away type annotations in this function.
    fn check_scope_default(&mut self, scope: NodeScope) -> Result<NodeScope, String> {
        self.check_scope(
            scope,
            None::<fn(Vec<NodeStmt>) -> Result<Vec<NodeStmt>, String>>,
        )
    }

    fn check_expr(&self, expr: &NodeExpr) -> Result<ExprData, String> {
        match expr {
            NodeExpr::BinaryExpr { op, lhs, rhs } => {
                let ldata = self.check_expr(lhs)?;
                let rdata = self.check_expr(rhs)?;
                // debug!(self, "lhs: {ldata:#?}\nrhs: {rdata:#?}");

                // Binary ops allowed for primitives && pointers.
                match ldata.addr_mode {
                    AddressingMode::Primitive | AddressingMode::Pointer => match rdata.addr_mode {
                        AddressingMode::Primitive | AddressingMode::Pointer => (),
                        _ => {
                            return err!(
                                self,
                                "Binary Expressions invalid for {:?}",
                                ldata.addr_mode
                            )
                        }
                    },
                    _ => return err!(self, "Binary Expressions invalid for {:?}", ldata.addr_mode),
                }

                let err_msg = format!("Expr of different Type! => {ldata:#?}\n.. {rdata:#?}");
                self.check_type_mode(ldata.type_mode, rdata.type_mode, &err_msg)?;

                // cmp        type, type => bool
                // logical    bool, bool => bool
                // arithmetic int,  int  => int
                let op_flags = op.get_flags();
                let width = self.get_width(&ldata.form);
                match op_flags {
                    _ if op_flags.contains(TokenFlags::CMP) => Ok(ExprData {
                        type_mode: TypeMode::Bool,
                        addr_mode: AddressingMode::Primitive,
                        form: ExprForm::Expr {
                            inherited_width: width,
                        },
                    }),
                    _ if op_flags.contains(TokenFlags::LOG) => match ldata.type_mode {
                        TypeMode::Bool => Ok(ExprData {
                            type_mode: TypeMode::Bool,
                            addr_mode: AddressingMode::Primitive,
                            form: ExprForm::Expr {
                                inherited_width: width,
                            },
                        }),
                        _ => {
                            err!(
                                self,
                                "'{op:?}' requires expr to be a boolean =>\n{ldata:#?}"
                            )
                        }
                    },
                    _ if op_flags.intersects(TokenFlags::ARITH | TokenFlags::BIT) => {
                        match ldata.type_mode {
                            TypeMode::Int { .. } | TypeMode::Float { .. } | TypeMode::IntLit => {
                                Ok(ExprData {
                                    type_mode: ldata.type_mode,
                                    addr_mode: ldata.addr_mode,
                                    form: ExprForm::Expr {
                                        inherited_width: width,
                                    },
                                })
                            }
                            _ => {
                                err!(self, "'{op:?}' requires expr to be an integer or float =>\n{ldata:#?}")
                            }
                        }
                    }
                    _ => err!(
                        self,
                        "Illegal binary expression =>\n{lhs:#?}\n.. '{op:?}' ..\n{rhs:#?}"
                    ),
                }
            }
            NodeExpr::UnaryExpr { op, operand } => {
                let checked = self.check_expr(&*operand)?;
                // debug!(self, "{checked:#?}");

                // 'Unary sub' signed int or lit => int | signed
                // 'Cmp Not'   bool => bool
                // 'Bit Not'   primitive => primitive
                // 'Addr of'   var => ptr
                // 'Ptr Deref' ptr => var
                let inherited_width = match checked.form {
                    ExprForm::Variable { ptr } => unsafe { (*ptr.as_ptr()).width },
                    ExprForm::Expr { inherited_width } => inherited_width,
                };
                match op {
                    TokenKind::Tilde => match checked.addr_mode  {
                        AddressingMode::Primitive => Ok(checked),
                        _ => err!(self, "'~' unary operator requires 'primitive' addressing =>\n{checked:#?}")
                    }
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
                        _ => err!(self, "'-' unary operator requires expr to be a signed integers =>\n{checked:#?}"),
                    },
                    TokenKind::CmpNot => match checked.type_mode {
                        TypeMode::Bool => Ok(ExprData {
                            type_mode: TypeMode::Bool,
                            addr_mode: AddressingMode::Primitive,
                            form: ExprForm::Expr { inherited_width },
                        }),
                        _ => err!(self, "'!' unary operator requires expr to be a boolean =>\n{checked:#?}"),
                    },
                    TokenKind::Ampersand => match checked.addr_mode {
                        AddressingMode::Primitive => match checked.form {
                            ExprForm::Variable { .. } => Ok(ExprData { // TODO(TOM): use variable's ptr?
                                type_mode: checked.type_mode,
                                addr_mode: AddressingMode::Pointer,
                                form: ExprForm::Expr {
                                    inherited_width: PTR_WIDTH,
                                },
                            }),
                            _ => err!(self, "'&' unary operator requires expr to be a memory address."),
                        },
                        _ => err!(self, "'&' unary operator requires expr to have a memory address =>\n{checked:#?}"),
                    },
                    TokenKind::Ptr => match checked.addr_mode {
                        AddressingMode::Pointer => Ok(ExprData {
                            type_mode: checked.type_mode,
                            addr_mode: AddressingMode::Primitive,
                            form: ExprForm::Expr { inherited_width }, // TODO(TOM): not sure about this?
                        }),
                        _ => err!(self, "'^' unary operator requires expr to be a pointer =>\n{checked:#?}"),
                    },
                    _ => err!(self, "Illegal unary Expression '{op:?}' =>\n{checked:#?}"),
                }
            }
            NodeExpr::Term(term) => self.check_term(term),
        }
    }

    fn check_term(&self, term: &NodeTerm) -> Result<ExprData, String> {
        match term {
            NodeTerm::IntLit(tok) => {
                self.update_pos(tok.pos);

                Ok(ExprData {
                    type_mode: TypeMode::IntLit,
                    addr_mode: AddressingMode::Primitive,
                    form: ExprForm::Expr { inherited_width: 0 },
                })
            }
            NodeTerm::Ident(tok) => {
                self.update_pos(tok.pos);

                let var = self.get_var(tok.as_str())?;
                match &self.types.get(var.type_id).unwrap().form {
                    TypeForm::Base { type_mode } => Ok(ExprData {
                        type_mode: *type_mode,
                        addr_mode: var.addr_mode,
                        form: ExprForm::Variable {
                            ptr: self.new_nonnull(var)?,
                        },
                    }),
                    TypeForm::Struct { member_ids } => {
                        todo!("check_term Ident Struct")
                    }
                    TypeForm::Union {} => todo!("check_term Ident Union"),
                }
            }

            NodeTerm::True | NodeTerm::False => {
                let type_ref = self.types.get(*self.type_map.get("bool").unwrap()).unwrap();
                match &type_ref.form {
                    TypeForm::Base { type_mode } => Ok(ExprData {
                        type_mode: *type_mode,
                        addr_mode: AddressingMode::Primitive,
                        form: ExprForm::Expr {
                            inherited_width: type_ref.width,
                        },
                    }),
                    TypeForm::Struct { member_ids } => todo!("check_term boolean struct"),
                    TypeForm::Union {} => todo!("check_term boolean union"),
                }
            }
            NodeTerm::FnCall { ident, args } => {
                self.update_pos(ident.pos);

                // check fn of that name exists
                // iterating over hash map aswell! bad!!!

                // iterate over fn_map
                // delimit by '(', take first to get fn_str
                // compare to attempted fncall
                // match to see if associated function is found for call.
                let fn_str = ident.as_str();
                let is_fn_name_valid = self.fn_map.iter().find(|(sig, fn_ref)| {
                    sig.as_str()
                        .split('(')
                        .collect::<Vec<&str>>()
                        .get(1)
                        .unwrap()
                        == &fn_str
                });
                let (signature, fn_ref) = match is_fn_name_valid {
                    Some((sig, fn_ref)) => (sig.as_str(), fn_ref),
                    None => {
                        return err!(
                            self,
                            "No associated function with attempted call. '{fn_str}'"
                        )
                    }
                };

                // check correct amount of arguments
                if args.len() != fn_ref.arg_semantics.len() {
                    return err!(
                        self,
                        "Incorrect amount of arguments for function '{signature}'. {} missing",
                        fn_ref.arg_semantics.len() - args.len()
                    );
                }

                // check args are of valid type
                for (i, arg) in args.into_iter().enumerate() {
                    let arg_expr = self.check_expr(&arg)?;
                    let fn_arg =
                        self.get_exprdata(fn_ref.arg_semantics.get(i).as_ref().unwrap())?;
                    self.check_type_equivalence(&fn_arg, &arg_expr)?;
                }

                Ok(fn_ref.return_type_data.unwrap())
            }
        }
    }

    // AddrMode, TypeMode, Width
    fn check_type_equivalence(
        &self,
        assigner: &ExprData,
        assignee: &ExprData,
    ) -> Result<(), String> {
        // Check Addressing Mode
        if assigner.addr_mode != assignee.addr_mode {
            return err!(
                self,
                "Expr of different AddrMode! {:?} vs {:?} =>\n{assigner:#?}\n.. {assignee:#?}",
                assigner.addr_mode,
                assignee.addr_mode
            );
        }

        // Check Type Mode
        let msg = format!("Expr of different Type! =>\n{assigner:#?}\n.. {assignee:#?}");
        self.check_type_mode(assigner.type_mode, assignee.type_mode, &msg)?;

        // Check for Type Narrowing
        let assigner_width = self.get_width(&assigner.form);
        let assignee_width = self.get_width(&assignee.form);
        if assigner_width < assignee_width {
            return err!(
                self,
                "Illegal Type Narrowing, Assignee({assignee_width}) < Assigner({assigner_width}) =>\n{assigner:#?}\n.. {assignee:#?}"
            );
        }
        Ok(())
    }

    fn get_exprdata(&self, var: &SemVariable) -> Result<ExprData, String> {
        match &self.types.get(var.type_id).unwrap().form {
            TypeForm::Base { type_mode } => Ok(ExprData {
                type_mode: *type_mode,
                addr_mode: var.addr_mode,
                form: ExprForm::Variable {
                    ptr: self.new_nonnull(var)?,
                },
            }),
            TypeForm::Struct { .. } => {
                todo!("Struct type mode")
            }
            TypeForm::Union {} => todo!("Union type mode"),
        }
    }

    fn check_type_mode(
        &self,
        assigner: TypeMode,
        assignee: TypeMode,
        msg: &str,
    ) -> Result<(), String> {
        if assigner == assignee {
            return Ok(());
        }

        // Check integer sign equality
        let sign_match = match assigner {
            TypeMode::IntLit => return Ok(()),
            TypeMode::Int { signed: sign1 } | TypeMode::Float { signed: sign1 } => match assignee {
                TypeMode::IntLit => return Ok(()),
                TypeMode::Int { signed: sign2 } | TypeMode::Float { signed: sign2 } => {
                    sign1 == sign2
                }
                TypeMode::Bool | TypeMode::Void => false,
            },
            TypeMode::Bool | TypeMode::Void => false,
        };

        if !sign_match {
            return err!(
                self,
                "Expr sign mismatch! {assigner:?} vs {assignee:?} => {msg}"
            );
        }
        Ok(())
    }

    ////////////////////////////////////////////////////////////////////////////////////////////////

    fn get_expr_ident(&self, expr: &NodeExpr, right_side: bool) -> String {
        match expr {
            NodeExpr::BinaryExpr { lhs, rhs, .. } => {
                if right_side {
                    self.get_expr_ident(&*rhs, false)
                } else {
                    self.get_expr_ident(&*lhs, false)
                }
            }
            NodeExpr::UnaryExpr { operand, .. } => self.get_expr_ident(&*operand, false),
            NodeExpr::Term(term) => match term {
                NodeTerm::True => "true".to_string(),
                NodeTerm::False => "false".to_string(),
                NodeTerm::IntLit(tok)
                | NodeTerm::Ident(tok)
                | NodeTerm::FnCall { ident: tok, .. } => tok.value.as_ref().unwrap().clone(),
            },
        }
    }

    fn get_var(&self, ident: &str) -> Result<&SemVariable, String> {
        match self.var_map.get(ident) {
            Some(idx) if self.ctx.scope_inherit_bounds_id.is_none() => {
                Ok(self.vars.get(*idx).unwrap())
            }
            Some(idx) => {
                let var = self.vars.get(*idx).unwrap();
                if var.scope_id < self.ctx.scope_inherit_bounds_id.unwrap() {
                    return err!(
                        self,
                        "Variable '{ident}' outside scope inheritance bounds, {} < {}",
                        var.scope_id,
                        self.ctx.scope_inherit_bounds_id.unwrap()
                    );
                }
                Ok(var)
            }
            None => err!(self, "Variable '{ident}' not found"),
        }
    }

    fn get_var_mut(&mut self, ident: &str) -> Result<&mut SemVariable, String> {
        match self.var_map.get(ident) {
            Some(idx) if self.ctx.scope_inherit_bounds_id.is_none() => {
                Ok(self.vars.get_mut(*idx).unwrap())
            }
            Some(idx) => {
                let var = self.vars.get_mut(*idx).unwrap();
                if var.scope_id < self.ctx.scope_inherit_bounds_id.unwrap() {
                    return err!(
                        self,
                        "Variable '{ident}' outside scope inheritance bounds, {} < {}",
                        var.scope_id,
                        self.ctx.scope_inherit_bounds_id.unwrap()
                    );
                }
                Ok(var)
            }
            None => err!(self, "Variable '{ident}' not found"),
        }
    }

    fn get_type_id(&self, ident: &str) -> Result<usize, String> {
        match self.type_map.get(ident) {
            Some(id) => Ok(*id),
            None => err!(self, "Type '{ident}' not found"),
        }
    }

    fn get_width(&self, form: &ExprForm) -> usize {
        match form {
            ExprForm::Variable { ptr } => unsafe { (*ptr.as_ptr()).width },
            ExprForm::Expr { inherited_width } => *inherited_width,
        }
    }

    fn add_type(&mut self, new_type: Type) {
        self.type_map
            .insert(new_type.ident.clone(), self.types.len());
        self.types.push(new_type);
    }

    fn update_pos(&self, pos: (u32, u32)) {
        unsafe {
            let mut_self = self as *const Checker as *mut Checker;
            (*mut_self).pos = pos;
        }
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
