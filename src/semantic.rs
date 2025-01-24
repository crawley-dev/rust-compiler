/* >>SEMANTIC<< The rules of the language, checking the meaning of the program.
 ✅ CLONING:
     - AST isn't a tree, contiguous "NodeStmt" Unions, some contain boxed data but not much
     - if everything is a ptr "box", manipulating data MUCH easier, borrow checker not angry at me!
     - Can't manipulate current AST freely, because it has to be rigid in size, its on the vars!
 ✅ Assignment:
     - ✅ arith-assign on new var or literal
     - ✅ re-assign on immutable vars
 ✅ Types:
     ✅ Operators expect specific types.
         - "bool PLUS u8" doesn't compile
         - LogicalAnd: bool, UnaryMinus: signed int
     ✅ Type Conversions
         - Implicit: integers being converted to a larger integer, e.g u16 = u8
         - Explicit: Everything else, using syntax: type_x as type_y
     ❌ Integer Bounds Checks
         - requires me to interpret every arith expression? let it be ub for now :)
     ✅ IntegerLitereal Coercion
         - its not a concrete type and can be coerced into any integer, after bounds checked.
     ✅ Pointers
         - always have usize, not a defined type but an attribute, that modifies byte_size?
         - kindof its own type (set size), but loose (inherits type's attr)
         - a ptr is the original type with modified byte_width (4) & ptr flag set.
     ✅ FORM:
         - Types have a form, which is the group they fall under, e.g struct or array.
         - each form has unique behaviour, such as a literal being non-concrete or an array being index-able]
     ✅ Type impl:
         - either a primitive or >>FUTURE:<< struct or union
     ✅ Var impl:
         - store a "type" + modifications, "form".
         - e.g its i16, but a pointer! or.. an array!
     ✅ Structure Revision: Either passing Variable or Literal
         - Both: TypeMode, AddressingMode, e.g Boolean Array
         - Var: ptr to the var
         - Literal: Inherited Width
         - TypeMode:
             - What operations can be performed
             - (OPTIONAL): Sign, if numerical
         - AddressingMode:
             - how is it represented in memory, if at all
             - (OPTIONAL): mutability, if represented in memory
     ✅ Type Narrowing:
         - check if the assigned expr is wider than the assignee variable
     ✅ Type Coersion: (check_assign() IS type coersion, if the 2 types don't  deviate too far, e.g narrowing, addr mode its coerced. TYPES DON'T EXIST!)
         - Literals can be coerced into a type of same mode and addressing mode
         - Expressions and Variables are unable to be coerced whatsoever, an explicit cast must take place.

 ❌ ExprData Rethink (removal):
     - Consolidate TypeMode & AddresingMode to ExprForm::Expr
         - because ExprForm::Var holds a Variable,
         - Variable has a type (which has a typemode) & addressingmode
     - Con: lots of indirection faff
     - TypeForm not accounted for properly!! ExprData needs TypeForm, not type mode !!

 ✅ Cpp Style Function overriding:
     - match function uniqueness based on its "signature" (name + argument types).
     - e.g func123(int,bool) != func123(int). UNIQUE!
*/

use crate::{
    debug, err, comp_err, lex::{Token, TokenFlags, TokenKind}, parse::{Arg, Ast, Expr, InitExpr, Node, Scope, Stmt, Term}, utils::{self, CompilerResult, Contents, Logger}
};
use anyhow::{Error, Result};
use educe::Educe;
use std::{
    collections::{HashMap},
    ptr::NonNull,
};

// region: Type Definitions

pub type Byte = usize;
const PTR: Byte = 8;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AddressingMode {
    Primitive,
    Pointer(u32), // stores "depth"
    Array(u32),   // stores "depth"
                  // None, // For zero width "marker types", e.g. void
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum TypeMode {
    Boolean,
    Int(bool), // sign
    Void,
    Struct,
    Union,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ExprForm {
    Variable, // a variable, allows for addr of etc
    Compound, // a compound expression, e.g. a + b, has less coercion than a literal
    Literal, // has some freedoms as its a literal!
}

// A base type does not have addresssing mode, e.g. '[]'. Mode is INTRINSIC to a BASE, inherited upwards
#[derive(Debug, Clone)]
struct BaseType {
    ident: String,
    mode: TypeMode,
    width: Byte,
}

#[derive(Debug, Clone)]
struct FullType {
    width: Byte, // width accounting for the addressing mode
    type_id: usize,
    addr_mode: AddressingMode,
}

// TODO(TOM): make this generic over the type, e.g. a partial base, or a partial struct?
#[derive(Debug, Clone)]
enum Type<T> {
    Primitive(T),
    // TODO(TOM): does this need a typemode, its mode is kinda itself.. a struct??
    Struct {
        ident: Token,
        members: Vec<T>,
        width: Byte,
    },
    Union {
        ident: Token,
        members: Vec<T>,
        width: Byte,
    },
}

// An expression is evaluated based on:
// TypeMode: what operations can be performed
// AddressingMode: how is it represented in memory, if at all
// Width: to know how big the result should be: MAX(op1.width, op2.width)

// how am I going to get types from these?
// I need to understand and inherit the types of concrete values
#[derive(Debug, Clone)]
struct ExprSem {
    form: ExprForm,
    type_mode: TypeMode,
    addr_mode: AddressingMode,
    width: Byte,
}

#[derive(Debug, Clone)]
pub struct Variable {
    ident: Token,
    mutable: bool,
    var_type: Type<FullType>,
    init_expr: InitExpr,
    scope_id: usize,
}

#[derive(Debug, Clone)]
pub struct Function {
    ident: Token,
    signature: String,
    args: Vec<Type<FullType>>,
    scope: Node<Scope>,
    return_type_id: Option<usize>, // none == void
}

#[derive(Debug)]
struct FuncContext {
    valid_return: bool,
}

#[derive(Debug)]
struct SemContext {
    loop_count: isize, // usize means I can get useful error messages in debug build, instead of oob error
    scope_depth: usize, // how many scopes we are deep! 0 --> infinity
    inherit_bounds: Vec<usize>, // a stack for storing function call scopes, don't inherit values past these!
    func: Option<FuncContext>, // fn_decl_id: Option<usize>,  // for when checking a function declaration
}

#[derive(Educe)]
#[educe(Debug)]
pub struct Checker {
    pub ast: Ast,
    ctx: SemContext,

    #[educe(Debug(ignore))]
    type_vec: Vec<Type<BaseType>>,
    #[educe(Debug(ignore))]
    pub type_map: HashMap<String, usize>,
    
    #[educe(Debug(ignore))]
    stack_var_vec: Vec<Variable>,
    #[educe(Debug(ignore))]
    pub stack_var_map: HashMap<String, usize>,
    
    pub fn_map: HashMap<String, usize>,
    pub fn_vec: Vec<Function>,
    
}
// endregion

impl Checker {
    pub fn new() -> Checker {
        let type_vec = Vec::from([
            Self::new_prim("bool", 1, TypeMode::Boolean),
            Self::new_prim("u8", 1, TypeMode::Int(false)),
            Self::new_prim("u16", 2, TypeMode::Int(false)),
            Self::new_prim("u32", 4, TypeMode::Int(false)),
            Self::new_prim("u64", PTR, TypeMode::Int(false)),
            Self::new_prim("usize", PTR, TypeMode::Int(false)),
            Self::new_prim("i8", 1, TypeMode::Int(true)),
            Self::new_prim("i16", 2, TypeMode::Int(true)),
            Self::new_prim("i32", 4, TypeMode::Int(true)),
            Self::new_prim("i64", PTR, TypeMode::Int(true)),
            Self::new_prim("isize", PTR, TypeMode::Int(true)),
            Self::new_prim("f32", 4, TypeMode::Int(true)),
            Self::new_prim("f64", PTR, TypeMode::Int(true)),
        ]);
        let mut type_map = HashMap::with_capacity(type_vec.len());
        for (idx, base) in type_vec.iter().enumerate() {
            match base {
                Type::Primitive(base) => type_map.insert(base.ident.clone(), idx),
                _ => todo!("type_map for non-primitive types"),
            };
        }

         Self {
            ast: Ast { stmts: Vec::new() },
            ctx: SemContext {
                loop_count: 0,
                scope_depth: 0,
                inherit_bounds: Vec::new(),
                func: None,
            },

            type_vec,
            fn_vec: Vec::new(),
            stack_var_vec: Vec::new(),
            type_map,
            fn_map: HashMap::new(),
            stack_var_map: HashMap::new(),
        }
    }

    pub fn check_ast(mut self, ast: Ast) -> (Checker, Option<Error>) {
        let mut sem_ast = Ast {
            stmts: Vec::with_capacity(ast.stmts.len()),
        };

        for stmt in ast.stmts {
             match self.check_top_level(stmt) {
                CompilerResult::Ok(data) => sem_ast.stmts.push(data),
                CompilerResult::Err { data, error } => {
                    if let Some(data) = data {
                        sem_ast.stmts.push(data);
                    }

                    self.ast = sem_ast;
                    return (self, Some(error))
                },
            };
        }
        self.ast = sem_ast;

        // TODO(TOM): for ref, cpp "main" function either:
        //      - takes no arguments, main().
        //      - takes 2 arguments, main(int argc, char* argv[]).
        //          - argc: amount of arguments given when the program is run (cmd line!)
        //          - argv: an array of length argc+1, each pointer points to a null terminated char[]
        // Instead: use one array with a length

        // Checking for the Entry Point
        // let fn_id = match checker.fn_map.get("main") {
        //     Some(id) => id,
        //     None => {
        //         return err!(
        //             &checker,
        //             "No entry point for the program found. Add a 'main' function."
        //         )
        //     }
        // };
        // match checker.fn_vec.get(*fn_id) {
        //     Some(func) if !func.args.is_empty() => {
        //         err!(
        //             &checker,
        //             "The 'main' function takes no arguments =>\nremove {:#?}",
        //             func.args
        //         )
        //     }
        //     None => {
        //         err!(
        //             &checker,
        //             "No entry point for the program found. Add a 'main' function."
        //         )
        //     }
        //     _ => Ok(checker),
        // }

        (self, None)
    }

    fn check_top_level(&mut self, stmt: Node<Stmt>) -> CompilerResult<Node<Stmt>> {
        match stmt.node {
            Stmt::FnDecl {
                ident,
                args,
                scope,
                return_type,
            } => {
                // check for name collisions
                let fn_ident = ident.str();

                // Create arg semantics
                // - check for duplicates
                // - check for used names (keywords & other variables)
                let mut args_semantics: Vec<Type<FullType>> = Vec::new();
                for arg in &args {
                    let arg_ident = arg.ident.str();

                    if args_semantics
                        .iter()
                        .map(|x| self.get_full_ident(x))
                        .find(|x| *x == arg_ident)
                        .is_some()
                    {
                        return comp_err!(
                            "Duplicate argument name: '{arg_ident}' in function {fn_ident}"
                        );
                    } else if self.stack_var_map.contains_key(arg_ident) {
                        return comp_err!(
                            "Argument name in use: {arg_ident} in function: {fn_ident}"
                        );
                    } else if self.type_map.contains_key(arg_ident) {
                        return comp_err!(
                            "Illegal argument name: {arg_ident} in function: {fn_ident}, Types are reserve keywords"
                        );
                    }
                    let base_id = *self.type_map.get(arg.parse_type.type_tok.str()).unwrap();
                    args_semantics.push(self.new_full(base_id, arg.parse_type.addr_mode));
                }

                // Creates a function signature, to allow for overloading, e.g plus5(i32,i32)

                let signature = match ident.str() {
                    "main" => "main".to_owned(), // NOTE(TOM): main is a special case, no overloading
                    name @ _ => {
                        let mut str = String::new();
                        str += name;
                        str += "(";
                        for (i, arg) in args_semantics.iter().enumerate() {
                            str += self.get_full_ident(arg);
                            str += ",";
                        }
                        if !args_semantics.is_empty() {
                            str.pop(); // removes extra ','
                        }
                        str + ")"
                    }
                };

                // check for name collisions with signature.
                if self.fn_map.contains_key(signature.as_str()) {
                    return comp_err!(
                        "Duplicate definition of a Function: '{signature}'"
                    );
                } else if self.type_map.contains_key(fn_ident) {
                    return comp_err!( 
                        "Illegal Function name, Types are reserved: '{fn_ident}'"
                    );
                }

                let return_type_id = match return_type {
                    Some(parse_type) => {
                        Some(*self.type_map.get(parse_type.type_tok.str()).unwrap())
                    }
                    None => None,
                };

                // Create lambda for custom scope check
                let mut scope_check_result;
                unsafe {
                    let mut_self = self as *mut Self;
                    scope_check_result = (*mut_self).check_scope(
                        scope,
                        Some(|stmts: Vec<Node<Stmt>>| -> CompilerResult<Scope> {
                            debug!("checking {signature}'s statements!");

                            let mut checked_stmts = Vec::with_capacity(stmts.len());

                            // add each arg as a variable for use in the function
                            for (arg_type, parse) in args_semantics.iter().zip(args.iter()) {
                                let arg_stmt = Node {
                                    start: parse.ident.start,
                                    end: parse.parse_type.type_tok.end_pos(),
                                    node: Stmt::VarDecl {
                                        init_expr: InitExpr::None,
                                        arg: Arg {
                                            ident: parse.ident.clone(),
                                            mutable: parse.mutable,
                                            parse_type: parse.parse_type.clone(),
                                        },
                                    }
                                };
                                match self.check_stmt(arg_stmt) {
                                    CompilerResult::Ok(data) => checked_stmts.push(data),
                                    CompilerResult::Err { data, error } => {
                                        if let Some(data) = data {
                                            checked_stmts.push(data)
                                        }
                                        return CompilerResult::Err {
                                            data: Some(Scope {
                                                stmts: checked_stmts,
                                                inherits_stmts: false,
                                            }),
                                            error,
                                        }
                                    }
                                }
                                debug!("added\n{:#?}", checked_stmts.last());
                            }

                            for stmt in stmts {
                                match self.check_stmt(stmt) {
                                    CompilerResult::Ok(data) => checked_stmts.push(data),
                                    CompilerResult::Err { data, error } => {
                                        if let Some(data) = data {
                                            checked_stmts.push(data)
                                        }
                                        return CompilerResult::Err {
                                            data: Some(Scope {
                                                stmts: checked_stmts,
                                                inherits_stmts: false,
                                            }),
                                            error,
                                        }
                                    }
                                }
                                debug!("added\n{:#?}", checked_stmts.last())
                            }

                            let scope = Scope {
                                stmts: checked_stmts,
                                inherits_stmts: false,
                            };

                            match self.ctx.func { // can't do if let with other conditionals (21.1.25)
                                Some(ref func) if !func.valid_return => {
                                    return comp_err!((scope),"Not all code paths return in '{signature}'")
                                }
                                _ => CompilerResult::Ok(scope),
                            }

                            // cleans up  args for me! (check_scope() that is)
                        }),
                    );
                }

                let (checked_scope, error) = match scope_check_result {
                    CompilerResult::Ok(node) => (node, None),
                    CompilerResult::Err { data, error } => {
                        (data.unwrap_or(Node {
                            start: stmt.start,
                            end: stmt.end,
                            node: Scope {
                                stmts: Vec::new(),
                                inherits_stmts: false,
                            },
                        }), Some(error))
                    }
                };

                self.fn_map.insert(signature.clone(), self.fn_vec.len());
                self.fn_vec.push(Function {
                    ident,
                    signature,
                    scope: checked_scope,
                    args: args_semantics,
                    return_type_id,
                });

                let data = Node { start: stmt.start, end: stmt.end, node: Stmt::FnSemantics {
                    id: self.fn_vec.len() - 1,
                }};
                match error {
                    Some(error) => CompilerResult::Err {
                        data: Some(data),
                        error,
                    },
                    None => CompilerResult::Ok(data),
                }
            }
            _ => comp_err!(
                "A Program only consists of functions, this is a {stmt:?}"
            ),
        }
    }

    // region: Scope
    fn check_scope<F>(
        &mut self,
        scope: Node<Scope>,
        special_checks: Option<F>,
    ) -> CompilerResult<Node<Scope>>
    where
        F: FnMut(Vec<Node<Stmt>>) -> CompilerResult<Scope>,
    {
        self.ctx.scope_depth += 1;
        let does_inherit = scope.node.inherits_stmts;
        if !does_inherit {
            self.ctx.inherit_bounds.push(self.ctx.scope_depth)
        }

        let node = match special_checks {
            Some(mut lambda) => match lambda(scope.node.stmts) {
                CompilerResult::Ok(node) => node,
                CompilerResult::Err { data, error} => return CompilerResult::Err {
                    data: Some(Node {
                        start: scope.start,
                        end: scope.end,
                        node: data.unwrap_or(Scope {
                            stmts: Vec::new(),
                            inherits_stmts: does_inherit,
                        }),
                    }),
                    error 
                },
            },
            None => {
                let mut stmts = Vec::new();
                for stmt in scope.node.stmts {
                    match self.check_stmt(stmt) {
                        CompilerResult::Ok(data) => stmts.push(data),
                        CompilerResult::Err { data, error } => {
                            if let Some(data) = data {
                                stmts.push(data)
                            }
                            return CompilerResult::Err {
                                data: Some(Node {
                                    start: scope.start,
                                    end: scope.end,
                                    node: Scope {
                                        stmts,
                                        inherits_stmts: does_inherit,
                                    },
                                }),
                                error,
                            }
                        }
                    }
                }

                Scope {
                    stmts, inherits_stmts: does_inherit,
                }
            }
        };

        self.ctx.scope_depth -= 1;
        loop {
            match self.stack_var_vec.last() {
                Some(var) if var.scope_id <= self.ctx.scope_depth => break,
                Some(var) => {
                    // debug!(self, "Scope ended, removing '{}'", var.ident.as_str());
                    let var = self.stack_var_vec.pop().unwrap();
                    self.stack_var_map.remove(var.ident.str());
                }
                None => break,
            }
        }

        CompilerResult::Ok(Node { start: scope.start, end: scope.end, node })
    }

    // Compiler doesn't understand type of 'None', so must hide away type annotations in this function.
    fn check_scope_default(&mut self, scope: Node<Scope>) -> CompilerResult<Node<Scope>> {
        self.check_scope(
            scope,
            None::<fn(Vec<Node<Stmt>>) -> CompilerResult<Scope>>,
        )
    }
    // endregion

    fn check_stmt(&mut self, stmt: Node<Stmt>) -> CompilerResult<Node<Stmt>> {
        match stmt.node {
            Stmt::VarDecl { init_expr, arg } => {
                // check for name collisions
                let str = arg.ident.str();
                if self.stack_var_map.contains_key(str) {
                    return comp_err!("Duplicate definition of a Variable: '{str}'");
                } else if self.type_map.contains_key(str) {
                    return comp_err!("Illegal Variable name, Types are reserved: '{str}'");
                }

                let base_id = *self.type_map.get(arg.parse_type.type_tok.str()).unwrap();
                let var_type = self.new_full(base_id, arg.parse_type.addr_mode);

                let var = Variable {
                    ident: arg.ident,
                    mutable: arg.mutable,
                    var_type,
                    init_expr,
                    scope_id: self.ctx.scope_depth,
                };

                // insert variable into registry
                self.stack_var_map
                    .insert(var.ident.str().to_string(), self.stack_var_vec.len());
                self.stack_var_vec.push(var.clone());

                // check intial expression
                if let InitExpr::Some(ref expr) = var.init_expr {
                    let expected = self.get_type_sem(&var.var_type);
                    let init_expr = self.check_expr(expr)?;


                    if let Err(error) = self.check_type_equivalence(&expected, &init_expr) {
                        return comp_err!((Node { start: stmt.start, end: stmt.end, node: Stmt::VarSemantics(var.clone())}), "Invalid init expr for variable {}\n{error}", var.ident);
                    }
                }

                CompilerResult::Ok(Node { start: stmt.start, end: stmt.end, node: Stmt::VarSemantics(var)})
            }
            Stmt::Assign {
                ref ident,
                ref expr,
            } => {
                let var = self.get_var(ident.str())?;

                let expected = self.get_type_sem(&var.var_type);
                let assign_rhs = self.check_expr(expr)?;
                self.check_type_equivalence(&expected, &assign_rhs)?;

                if !var.mutable {
                    // if the variable is not initialised, this is the initialisation!
                    match var.init_expr {
                        InitExpr::None => {
                            let var_mut = self.get_var_mut(ident.str())?;
                            var_mut.init_expr = InitExpr::Deferred
                        }
                        _ => return comp_err!((stmt), "Re-assignment of a Constant:\n{var:#?}"),
                    }
                }

                CompilerResult::Ok(stmt)
            }
            /*
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
                if expr_type_data.addr_mode != self.ctx.return_type_data.unwrap().addr_mode {
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
            NodeStmt::Exit(ref expr) => {
                self.check_expr(&expr)?;
                Ok(stmt)
            }
            NodeStmt::Break => {
                if self.ctx.loop_count <= 0 {
                    return err!(self, "Not inside a loop! cannot break");
                    }
                    Ok(stmt)
                    } */
           Stmt::NakedScope(scope) => {
                match self.check_scope_default(scope) {
                    CompilerResult::Ok(data) => CompilerResult::Ok(
                        Node { 
                            start: data.start, 
                            end: data.end, 
                            node: Stmt::NakedScope(data) 
                        }),
                    CompilerResult::Err {data, error} => {
                        let data = match data {
                            Some(node) => node,
                            None => return CompilerResult::Err { data: None, error},
                        };
                        // TODO(TOM): is this not a bit ridiculous? 
                        // node containg a singular item. so much wasted data
                        CompilerResult::Err { 
                            data: Some(Node {
                                start: stmt.start,
                                end: stmt.end,
                                node: Stmt::NakedScope(data),
                            }),
                            error
                        }
                    }
                }
           }
            Stmt::FnDecl { ident, .. } => {
                comp_err!((stmt),"Functions cannot be nested, they're top level statements, {ident:#?}")
            }
            _ => comp_err!((stmt.clone()), "Unexpected statement {stmt:#?}"),
        }
    }

    fn check_expr(&self, expr: &Node<Expr>) -> Result<ExprSem> {
        match &expr.node {
            Expr::Term(term) => self.check_term(term),
            Expr::Binary { op, lhs, rhs } => {
                let lhs_checked = self.check_expr(lhs)?;
                let rhs_checked = self.check_expr(rhs)?;
                self.check_type_equivalence(&lhs_checked, &rhs_checked)?;

                match lhs_checked.addr_mode {
                    // can check lhs or rhs, doesn't matter, they are equal
                    AddressingMode::Array(_) => {
                        err!(
                            "[ARR] Invalid binary(two) expression: {op:?}..\n{lhs:#?}..\n{rhs:#?}"
                        )
                    }
                    AddressingMode::Pointer(_) => {
                        err!(
                            "[PTR] Invalid binary(two) expression: {op:?}..\n{lhs:#?}..\n{rhs:#?}"
                        )
                    }
                    AddressingMode::Primitive => {
                        // 'CMP'   => T, T       => bool
                        // 'LOG'   => bool, bool => bool
                        // 'Arith' => int, int   => int
                        match op {
                            _ if op.has_flags(TokenFlags::CMP) => {
                                Ok(ExprSem {
                                    form: ExprForm::Literal,
                                    type_mode: TypeMode::Boolean,
                                    addr_mode: AddressingMode::Primitive,
                                    width: 1,
                                })
                            }

                            _ if op.has_flags(TokenFlags::LOG) => {
                                match lhs_checked.type_mode { 
                                    TypeMode::Boolean => { 
                                        Ok(ExprSem {
                                            form: ExprForm::Literal,
                                            type_mode: TypeMode::Boolean,
                                            addr_mode: AddressingMode::Primitive,
                                            width: 1,
                                        })
                                    }
                                    _ => err!("logical operations require: {op:?}..\n{lhs:#?}..\n{rhs:#?}"),
                                }
                            }

                            _ if op.has_flags(TokenFlags::ARITH) => {
                                match lhs_checked.type_mode {
                                    TypeMode::Int(_) => Ok(ExprSem {
                                        form: ExprForm::Literal,
                                        type_mode: lhs_checked.type_mode,
                                        addr_mode: AddressingMode::Primitive,
                                        width: lhs_checked.width,
                                    }),
                                    _ => err!("Arithmetic require integers: {op:?}..\n{lhs:#?}..\n{rhs:#?}"),
                                }
                            }
                            
                            _ if op.has_flags(TokenFlags::BIT) => {
                                match lhs_checked.type_mode {
                                    TypeMode::Int(_) => Ok(ExprSem {
                                        form: ExprForm::Literal,
                                        type_mode: lhs_checked.type_mode,
                                        addr_mode: AddressingMode::Primitive,
                                        width: lhs_checked.width,
                                    }),
                                    _ => err!("Bitwise require integers: {op:?}..\n{lhs:#?}..\n{rhs:#?}"),
                                }
                            }

                            _ => err!("[PRM] Invalid binary(two) expression: {op:?}..\n{lhs:#?}..\n{rhs:#?}")
                        }
                    }
                }
            }
            // unary operators tend to be very unqiue, so they are individually matched.
            Expr::Unary { op, expr } => {
                let checked = self.check_expr(&*expr)?;
                
                // 'Unary sub' signed int or lit => signed int literal
                // 'Cmp Not'   bool              => bool
                // 'Bit Not'   primitive         => primitive
                // 'Addr of'   var               => ptr
                // 'Ptr Deref' ptr               => var
                match checked.addr_mode {
                    AddressingMode::Array(_) => {
                        err!("[ARR] Invalid Unary Expression: {op:?}\n{checked:#?}")
                    }
                    AddressingMode::Pointer(depth) => {
                        match op {
                            TokenKind::Ptr if depth == 1 => {
                                Ok(ExprSem {
                                    form: ExprForm::Literal,
                                    type_mode: checked.type_mode,
                                    addr_mode: AddressingMode::Primitive,
                                    // width: checked.width,
                                    // calculate width of type it was pointing to, e.g. bool == 1.
                                    // because currently checked.width == 8 (ptr)
                                    // not ideal, should have this information saved?
                                    width: match checked.type_mode {
                                        TypeMode::Boolean => 1,
                                        TypeMode::Int(_) => 8, // will be shrunk to match caller.
                                        _ => todo!("width calculation for type"),
                                    },
                                })
                            }
                            TokenKind::Ptr => {
                                Ok(ExprSem {
                                    form: ExprForm::Literal,
                                    type_mode: checked.type_mode,
                                    addr_mode: AddressingMode::Pointer(depth - 1),
                                    width: checked.width,
                                })
                            }
                            _ => err!(
                                "[PTR] Invalid Unary Expression: {op:?}..\n{checked:#?}"
                            ),
                        }
                    }
                    AddressingMode::Primitive => {
                        match op {
                            TokenKind::Sub => match checked.type_mode {
                                TypeMode::Int(true) => Ok(ExprSem {
                                    form: ExprForm::Compound,
                                    ..checked
                                }),
                                TypeMode::Int(false) if checked.form == ExprForm::Literal => Ok(ExprSem {
                                    form: ExprForm::Compound,
                                    type_mode: TypeMode::Int(true),
                                    addr_mode: AddressingMode::Primitive,
                                    width: checked.width,
                                }),
                                _ => err!("UnarySub expects a signed integer, found {checked:#?}")
                            }
                           
                            // TODO(TOM): bitwise not on signed integers?
                            TokenKind::Not if checked.type_mode != TypeMode::Boolean && checked.type_mode != TypeMode::Int(false) => err!("Not expects a boolean or unsigned integer, found {checked:#?}"), 
                            TokenKind::Not => Ok(ExprSem {
                                form: ExprForm::Compound,
                                ..checked
                            }),

                            TokenKind::Ampersand if checked.form != ExprForm::Variable => err!("AddressOf expects a variable, found {checked:#?}"),
                            TokenKind::Ampersand => Ok(ExprSem {
                                form: ExprForm::Compound,
                                type_mode: checked.type_mode,
                                addr_mode: AddressingMode::Pointer(1),
                                width: PTR,
                            }),

                            _ => err!("Invalid Unary Expression on Primitive: {op:?}..\n{checked:#?}"),
                        }
                    }
                }
            }
        }
    }

    fn check_term(&self, term: &Node<Term>) -> Result<ExprSem> {
        // TODO(TOM): NodeTerm really should unconditionally contain a position,
        //  >> detach pos from token and give it to the node itself
        Logger::set_pos(term.start);

        match &term.node {
            Term::True | Term::False => Ok(ExprSem {
                form: ExprForm::Literal,
                type_mode: TypeMode::Boolean,
                addr_mode: AddressingMode::Primitive,
                width: 1,
            }),
            Term::Ident => {
                let var = self.get_var(&Contents::get_src_oneline(term.start, term.end))?;
                let addr_mode = match &var.var_type {
                    Type::Primitive(full_type) => full_type.addr_mode,
                    Type::Struct {
                        ident,
                        members,
                        width,
                    } => todo!("struct semantics"),
                    Type::Union {
                        ident,
                        members,
                        width,
                    } => todo!("union semantics"),
                };

                Ok(ExprSem {
                    form: ExprForm::Variable,
                    addr_mode,
                    type_mode: self.get_full_mode(&var.var_type),
                    width: self.get_full_width(&var.var_type),
                })
            }
            Term::IntLit => {
                Ok(ExprSem {
                    form: ExprForm::Literal,
                    type_mode: TypeMode::Int(false),
                    addr_mode: AddressingMode::Primitive,
                    width: 0,
                })
            }
            Term::FnCall { ident, args } => {
                todo!("check_term fncall")
            }
        }
    }

    fn check_type_equivalence(&self, a: &ExprSem, b: &ExprSem) -> Result<()> {
        debug!("checking type equivalence {a:#?}\n{b:#?}");

        if a.addr_mode != b.addr_mode {
            return err!(
                "Expr of different AddrMode! {a:?} vs {b:?}, {a:#?}\n.. {b:#?}",
                a = a.addr_mode,
                b = b.addr_mode
            );
        }

        let literal_expr = a.form == ExprForm::Literal || b.form == ExprForm::Literal;

        // cannot assign something bigger than the 'container'
        if !literal_expr && a.width < b.width {
            return err!(
                "Illegal Type Narrowing, Assignee({}) < Assigner({}), {a:#?}\n.. {b:#?}",
                a.width,
                b.width
            );
        }

        match (a.type_mode, b.type_mode) {
            (TypeMode::Boolean, TypeMode::Boolean) => Ok(()),
            (TypeMode::Int(_), TypeMode::Int(_)) if literal_expr => Ok(()),
            (TypeMode::Int(a_sign), TypeMode::Int(b_sign)) if a_sign == b_sign => Ok(()),
            _ => {
                err!(
                    "TypeMode mismatch: {:?} != {:?} ..\n{a:#?}\n.. {b:#?}",
                    a.type_mode,
                    b.type_mode
                )
            }
        }
    }

    // region: Small_Components

    fn add_type(&mut self, new_base: BaseType) {
        self.type_map
            .insert(new_base.ident.clone(), self.type_vec.len());
        self.type_vec.push(Type::Primitive(new_base));
    }

    fn new_nonnull(&self, reference: &Variable) -> Result<NonNull<Variable>> {
        match NonNull::new(reference as *const Variable as *mut Variable) {
            Some(ptr) => Ok(ptr),
            None => err!(
                "Found nullptr when creating 'ExprData'\n{reference:#?}"
            ),
        }
    }

    fn new_prim(ident: &str, width: usize, mode: TypeMode) -> Type<BaseType> {
        Type::Primitive(BaseType {
            ident: ident.to_string(),
            mode,
            width,
        })
    }
    
    fn new_full(&self, base_id: usize, addr_mode: AddressingMode) -> Type<FullType> {
        let base = self.type_vec.get(base_id).unwrap();
        match base {
            Type::Primitive(base) => Type::Primitive(FullType {
                width: base.width,
                type_id: base_id,
                addr_mode,
            }),
            Type::Struct { .. } => todo!("struct full type"),
            Type::Union { .. } => todo!("union full type"),
        }
    }

    fn get_var(&self, str: &str) -> Result<&Variable> {
        match self.stack_var_map.get(str) {
            Some(id) => Ok(self.stack_var_vec.get(*id).unwrap()),
            None => err!("Variable not found: '{str}'"),
        }
    }

    fn get_var_mut(&mut self, str: &str) -> Result<&mut Variable> {
        match self.stack_var_map.get(str) {
            Some(id) => Ok(self.stack_var_vec.get_mut(*id).unwrap()),
            None => err!("Variable not found: '{str}'"),
        }
    }

    fn get_type_sem(&self, var_type: &Type<FullType>) -> ExprSem {
        ExprSem {
            form: ExprForm::Compound,
            type_mode: self.get_full_mode(var_type),
            addr_mode: self.get_full_addrmode(var_type),
            width: self.get_full_width(var_type),
        }
    }

    fn get_full_ident(&self, inp_type: &Type<FullType>) -> &str {
        match inp_type {
            Type::Primitive(full) => {
                let base = self.type_vec.get(full.type_id).unwrap();
                Self::get_base_ident(base)
            }
            Type::Struct { .. } => todo!("struct ident calculation"),
            Type::Union { .. } => todo!("union ident calculation"),
        }
    }

    fn get_full_mode(&self, inp_type: &Type<FullType>) -> TypeMode {
        match inp_type {
            Type::Primitive(full) => {
                let base = self.type_vec.get(full.type_id).unwrap();
                Self::get_base_mode(base)
            }
            Type::Struct { .. } => todo!("struct mode calculation"),
            Type::Union { .. } => todo!("union mode calculation"),
        }
    }

    fn get_full_width(&self, inp_type: &Type<FullType>) -> usize {
        match inp_type {
            Type::Primitive(full) => {
                match full.addr_mode {
                    AddressingMode::Primitive => full.width,
                    AddressingMode::Pointer(_) => PTR,
                    AddressingMode::Array(_) => todo!("array width calculation"),
                }
            }
            Type::Struct { .. } => todo!("struct width calculation"),
            Type::Union { .. } => todo!("union width calculation"),
        }
    }

    fn get_full_addrmode(&self, inp_type: &Type<FullType>) -> AddressingMode {
        match inp_type {
            Type::Primitive(full) => full.addr_mode,
            Type::Struct { .. } => todo!("struct addr_mode calculation"),
            Type::Union { .. } => todo!("union addr_mode calculation"),
        }
    }

     // base type width depends solely on form
     fn get_base_width(inp_type: &Type<BaseType>) -> usize {
        match inp_type {
            Type::Primitive(base) => base.width,
            Type::Struct { .. } => todo!("struct width calculation"),
            Type::Union { .. } => todo!("union width calculation"),
        }
    }

    fn get_base_mode(inp_type: &Type<BaseType>) -> TypeMode {
        match inp_type {
            Type::Primitive(base) => base.mode,
            Type::Struct { .. } => todo!("struct mode calculation"),
            Type::Union { .. } => todo!("union mode calculation"),
        }
    }

    fn get_base_ident(inp_type: &Type<BaseType>) -> &str {
        match inp_type {
            Type::Primitive(base) => base.ident.as_str(),
            Type::Struct { .. } => todo!("struct ident calculation"),
            Type::Union { .. } => todo!("union ident calculation"),
        }
    }

    // endregion
}

/*
fn check_expr(&self, expr: &NodeExpr) -> Result<ExprData, String> {
    match expr {
        NodeExpr::Binary { op, lhs, rhs } => {
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

            match op_flags {
                _ if op_flags.contains(TokenFlags::CMP) => Ok(ExprData {
                    ptr: None,
                    width: ldata.width,
                    type_mode: TypeMode::Bool,
                    addr_mode: AddressingMode::Primitive,
                }),
                _ if op_flags.contains(TokenFlags::LOG) => match ldata.type_mode {
                    TypeMode::Bool => Ok(ExprData {
                        ptr: None,
                        type_mode: TypeMode::Bool,
                        addr_mode: AddressingMode::Primitive,
                        width: ldata.width,
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
                                ptr: None,
                                width: ldata.width,
                                type_mode: ldata.type_mode,
                                addr_mode: ldata.addr_mode,
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
        NodeExpr::Unary { op, expr } => {
            let checked = self.check_expr(&*expr)?;
            // debug!(self, "{checked:#?}");

            // 'Unary sub' signed int or lit => int | signed
            // 'Cmp Not'   bool => bool
            // 'Bit Not'   primitive => primitive
            // 'Addr of'   var => ptr
            // 'Ptr Deref' ptr => var

            // let inherited_width = match checked.form {
            //     ExprForm::Variable { ptr } => unsafe { (*ptr.as_ptr()).width },
            //     ExprForm::Expr { inherited_width } => inherited_width,
            // };
            match op {
                TokenKind::Tilde => match checked.addr_mode  {
                    AddressingMode::Primitive => Ok(checked),
                    _ => err!(self, "'~' unary operator requires 'primitive' addressing =>\n{checked:#?}")
                }
                TokenKind::Sub => match checked.type_mode {
                    TypeMode::Int { signed } | TypeMode::Float { signed } if signed => {
                        Ok(ExprData {
                            ptr: None,
                            width: checked.width,
                            type_mode: TypeMode::Int { signed },
                            addr_mode: AddressingMode::Primitive,
                        })
                    }
                    TypeMode::IntLit => Ok(ExprData {
                        ptr: None,
                        width: checked.width,
                        type_mode: TypeMode::Int { signed: true },
                        addr_mode: AddressingMode::Primitive,
                    }),
                    _ => err!(self, "'-' unary operator requires expr to be a signed integers =>\n{checked:#?}"),
                },
                TokenKind::CmpNot => match checked.type_mode {
                    TypeMode::Bool => Ok(ExprData {
                        ptr: None,
                        width: checked.width,
                        type_mode: TypeMode::Bool,
                        addr_mode: AddressingMode::Primitive,
                    }),
                    _ => err!(self, "'!' unary operator requires expr to be a boolean =>\n{checked:#?}"),
                },
                TokenKind::Ampersand => match checked.addr_mode {
                    AddressingMode::Primitive if checked.ptr.is_some() =>
                        Ok(ExprData {
                                    ptr: None, // TODO(TOM): use variable's ptr?
                                    width: PTR,
                                    type_mode: checked.type_mode,
                                    addr_mode: AddressingMode::Pointer,
                                }),
                    _ => err!(self, "'&' unary operator requires expr to have a memory address =>\n{checked:#?}"),
                },
                TokenKind::Ptr => match checked.addr_mode {
                    AddressingMode::Pointer => Ok(ExprData {
                        ptr: None,
                        width: checked.width, // TODO(TOM): not sure about this?
                        type_mode: checked.type_mode,
                        addr_mode: AddressingMode::Primitive,
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
            self.set_pos(tok.pos);

            Ok(ExprData {
                ptr: None,
                width: 0,
                type_mode: TypeMode::IntLit,
                addr_mode: AddressingMode::Primitive,
            })
        }
        NodeTerm::Ident(tok) => {
            self.set_pos(tok.pos);

            let var = self.get_var(tok.as_str())?;
            match &self.types.get(var.type_id).unwrap().form {
                TypeForm::Base { type_mode } => Ok(ExprData {
                    ptr: Some(self.new_nonnull(var)?),
                    width: var.width,
                    type_mode: *type_mode,
                    addr_mode: var.addr_mode,
                }),
                TypeForm::Struct {} => {
                    todo!("check_term Ident Struct")
                }
                TypeForm::Union {} => todo!("check_term Ident Union"),
            }
        }

        NodeTerm::True | NodeTerm::False => {
            let type_ref = self.types.get(*self.type_map.get("bool").unwrap()).unwrap();
            match &type_ref.form {
                TypeForm::Base { type_mode } => Ok(ExprData {
                    ptr: None,
                    width: type_ref.width,
                    type_mode: *type_mode,
                    addr_mode: AddressingMode::Primitive,
                }),
                TypeForm::Struct {} => todo!("check_term boolean struct"),
                TypeForm::Union {} => todo!("check_term boolean union"),
            }
        }
        NodeTerm::FnCall { ident, args } => {
            self.set_pos(ident.pos);

            // check fn of that name exists
            // iterating over hash map aswell! bad!!!

            // check args are of valid type
            // for (i, arg) in args.into_iter().enumerate() {
            //     let arg_expr = self.check_expr(&arg)?;
            //     let fn_arg =
            //         self.get_exprdata(fn_ref.arg_semantics.get(i).as_ref().unwrap())?;
            //     self.check_type_equivalence(&fn_arg, &arg_expr)?;
            // }

            let fn_str = ident.as_str();
            let mut args_data = Vec::with_capacity(args.len());
            for arg in args.into_iter() {
                args_data.push(self.check_expr(arg)?);
            }

            // https://en.wikipedia.org/wiki/Type_inference
            // https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system

            // need to perform type inference on "args_data"
            // to get the names of the types
            // then to construct a function signature
            // then to check if that exists.

            let signature = match ident.as_str() {
                "main" => "main".to_owned(),
                name @ _ => {
                    let mut str = String::new();
                    str += name;
                    str += "(";
                    for (i, arg) in args_data.iter().enumerate() {
                        // ExprData => Type
                        // str += self.types.get(arg.type_id).unwrap().ident.as_str();
                        str += ",";
                    }
                    str.pop(); // removes extra ','
                    str + ")"
                }
            };

            // iterate over fn_map
            // compare to attempted fncall
            //      - amount of args first
            //      - compare each arg id.
            //      - then by name (delimit by '(')
            // match to see if associated function is found for call.
            // for (sig, fn_ref) in &self.fn_map {
            //     if fn_ref.arg_semantics.len() != args.len() {
            //         continue;
            //     }
            // }

            // let is_fn_name_valid = self.fn_map.iter().find(|(sig, fn_ref)| {
            //     sig.as_str()
            //         .split('(')
            //         .collect::<Vec<&str>>()
            //         .get(0)
            //         .unwrap()
            //         == &fn_str
            // });
            // let (signature, fn_ref) = match is_fn_name_valid {
            //     Some((sig, fn_ref)) => (sig.as_str(), fn_ref),
            //     None => {
            //         return err!(
            //             self,
            //             "No associated function with attempted call. '{fn_str}'"
            //         )
            //     }
            // };

            // check correct amount of arguments
            // if args.len() != fn_ref.arg_semantics.len() {
            //     return err!(
            //         self,
            //         "Incorrect amount of arguments for function '{signature}'. {} missing",
            //         fn_ref.arg_semantics.len() - args.len()
            //     );
            // }

            // Ok(fn_ref.return_type_data.unwrap())
            todo!("")
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
    if assigner.width < assignee.width {
        return err!(
            self,
            "Illegal Type Narrowing, Assignee({}) < Assigner({}) =>\n{assigner:#?}\n.. {assignee:#?}",
            assignee.width, assigner.width
        );
    }
    Ok(())
}

fn get_exprdata(&self, var: &SemVariable) -> Result<ExprData, String> {
    match &self.types.get(var.type_id).unwrap().form {
        TypeForm::Base { type_mode } => Ok(ExprData {
            ptr: Some(self.new_nonnull(var)?),
            width: var.width,
            type_mode: *type_mode,
            addr_mode: var.addr_mode,
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
        NodeExpr::Binary { lhs, rhs, .. } => {
            if right_side {
                self.get_expr_ident(&*rhs, false)
            } else {
                self.get_expr_ident(&*lhs, false)
            }
        }
        NodeExpr::Unary { expr, .. } => self.get_expr_ident(&*expr, false),
        NodeExpr::Term(term) => match term {
            NodeTerm::True => "true".to_string(),
            NodeTerm::False => "false".to_string(),
            NodeTerm::IntLit(tok)
            | NodeTerm::Ident(tok)
            | NodeTerm::FnCall { ident: tok, .. } => tok.as_str().to_string(),
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

*/
