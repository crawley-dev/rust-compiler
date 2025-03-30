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
    comp_err, debug, err, formatting::{self, PosAwareDebug}, lex::{Token, TokenFlags, TokenKind}, parse::{Arg, Ast, Expr, InitExpr, Node, ParseType, Scope, Stmt, Term}, upgrade_err, upgrade_result, utils::{self, CompilerResult, Contents, Logger, Pos}
};
use anyhow::{Error, Result};
use educe::Educe;
use std::{
    cmp::max, collections::HashMap, convert::Infallible, ptr::NonNull
};

// region: Type Definitions

pub type Byte = usize;
const PTR: Byte = 8;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum AddressingMode {
    Primitive,
    Pointer(u32), // stores "depth"
    Array(u32),   // stores "depth"
                  // None, // For zero width "marker types", e.g. void
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum TypeMode {
    Boolean,
    Int(bool), // sign
    Struct,
    Union,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum ExprForm {
    Variable, // a variable, allows for addr of etc
    Compound, // a compound expression, e.g. a + b, doesn't have coercion semantics like a literal
    Literal, // has some freedoms as its a literal!
}

// A base type does not have addresssing mode, e.g. '[]'. Mode is INTRINSIC to a BASE, inherited upwards
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
struct BaseType {
    ident: String,
    mode: TypeMode,
    width: Byte,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
struct FullType {
    width: Byte, // width accounting for the addressing mode
    type_id: usize,
    addr_mode: AddressingMode,
}

// TODO(TOM): make this generic over the type, e.g. a partial base, or a partial struct?
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
enum Type<T> {
    Void, // Don't really know how Void ptrs are gonna work.. another variant? do I even want them?
    Primitive(T),
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

// this contains the signature and a possible function,
// this is so we can index all functions so other funcs can reference them.

#[derive(Educe, Clone)]
#[educe(Debug)]
pub struct Function {
    #[educe(Debug(ignore))]
    ident: Token,
    signature: String,
    args: Vec<Type<FullType>>,
    return_type: Type<FullType>,
    #[educe(Debug(method = "crate::formatting::format_optional"))]
    scope: Option<Node<Scope>>,
}

#[derive(Debug)]
struct FuncContext {
    return_type: Type<FullType>, // optional as it may be void, which isn't a type!
    signature: String, // should be a str really.
}

#[derive(Debug)]
struct SemContext {
    loop_count: isize, // picked isize so I get useful error messages in debug build, instead of an oob error
    scope_depth: usize, // how many scopes we are deep! 0 --> 2^64 (a few)
    inherit_bounds: Vec<usize>, // a stack for storing function call scopes, don't inherit values past these!
    func: FuncContext,
}

#[derive(Educe)]
#[educe(Debug)]
pub struct Checker {
    pub ast: Ast,
    ctx: SemContext,

    // the type vec stores the "true" types
    #[educe(Debug(ignore))]
    type_vec: Vec<Type<BaseType>>,
    // whilst the map also contains aliases that map to the original type.
    #[educe(Debug(ignore))]
    pub type_map: HashMap<String, usize>,
    
    #[educe(Debug(ignore))]
    stack_var_vec: Vec<Variable>,
    #[educe(Debug(ignore))]
    pub stack_var_map: HashMap<String, usize>,
    
    // this stores the names of all functions names,
    // this then gives us a list of all overloads for this function.
    pub fn_map: HashMap<String, Vec<usize>>,
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
            Self::new_prim("u64", 8, TypeMode::Int(false)),
            Self::new_prim("usize", PTR, TypeMode::Int(false)),
            Self::new_prim("i8", 1, TypeMode::Int(true)),
            Self::new_prim("i16", 2, TypeMode::Int(true)),
            Self::new_prim("i32", 4, TypeMode::Int(true)),
            Self::new_prim("i64", 8, TypeMode::Int(true)),
            Self::new_prim("isize", PTR, TypeMode::Int(true)),
            // Self::new_prim("f32", 4, TypeMode::Int(true)),
            // Self::new_prim("f64", 8, TypeMode::Int(true)),
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
                func: FuncContext {
                    return_type: Type::Void,
                    signature: String::new(),
                },
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


        // Index all top level data. function decl ordering doesn't matter!
        for stmt in &ast.stmts {
             if let Result::Err(error)  = self.index_top_level(stmt) {
                return (self, Some(error))
            };
        }

        // Now check all the functions.
        for stmt in ast.stmts {
            match self.check_top_level(stmt) {
                CompilerResult::Ok(Some(data)) => sem_ast.stmts.push(data),
                CompilerResult::Ok(None) => (),
                CompilerResult::Err { data, error } => {
                    if let Some(Some(data)) = data {
                        sem_ast.stmts.push(data);
                    }

                    self.ast = sem_ast;
                    return (self, Some(error))
                },
            }
        }

        self.ast = sem_ast;

        /*
        // TODO(TOM): for ref, cpp "main" function either:
        //  - Main must return int - exit code.
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
        */
        (self, None)
    }

    // region: Top Level
    fn index_top_level(&mut self, stmt: &Node<Stmt>) -> Result<()> {
        // index all the functions and type aliases, so we can call a func later in the file & recurse.
        debug!("Indexing top level stmt: {stmt:?}");

        match &stmt.node {
            Stmt::FnDecl { ident, args, scope, return_type } => {
                let fn_ident = ident.str();
                let semantics = self.create_arg_semantics(&args, fn_ident)?;
                let signature = self.create_func_signature(ident.str(), &semantics);
                
                self.check_fn_overloads(&semantics, fn_ident)?;

                let return_type = match return_type {
                    Some(parse_type) => {
                        let base_id = *self.type_map.get(parse_type.type_tok.str()).unwrap();
                        self.new_full(base_id, parse_type.addr_mode)
                    }
                    None => Type::Void,
                };

                self.fn_vec.push(Function {
                    ident: ident.clone(),
                    signature,
                    args: semantics,
                    return_type,
                    scope: None,
                });

                if self.fn_map.contains_key(fn_ident) {
                    self.fn_map.get_mut(fn_ident).unwrap().push(self.fn_vec.len() - 1);
                } else {
                    self.fn_map.insert(fn_ident.to_string(), vec![self.fn_vec.len() - 1]);
                }

                Ok(())
            }  
            Stmt::TypeAlias { ident, parse_type } => {
                self.check_type_alias(ident.str(), parse_type.type_tok.str())
            }, 
            _ => err!(
                "A Program only consists of Top-Level Statements, this is a {stmt:?}"
            ),
        }
    }

    fn check_top_level(&mut self, stmt: Node<Stmt>) -> CompilerResult<Option<Node<Stmt>>> {
        match stmt.node {
            Stmt::FnDecl {
                ident,
                args,
                scope,
                return_type,
            } => {
                match self.check_fn_decl(stmt.start, stmt.end, ident, args, scope, return_type) { 
                    CompilerResult::Ok(data) => CompilerResult::Ok(Some(data)),
                    CompilerResult::Err { data, error } => CompilerResult::Err { data: Some(data), error }}
            },
            Stmt::TypeAlias { .. } => CompilerResult::Ok(None),
            _ => comp_err!(
                "A Program only consists of Top-Level Statements, this is a {stmt:?}"
            ),
        }
    }

    // region: check_function
    fn check_fn_decl(&mut self, start: Pos, end: Pos, ident: Token, args: Vec<Arg>, scope: Node<Scope>, return_type: Option<ParseType>) -> CompilerResult<Node<Stmt>> {
        // check for name collisions
        let fn_ident = ident.str();

        // checks overloads, first non declared function is this one. (indexed in order)
        let mut function = None;
        let mut function_index = None;
        let overloads = self.fn_map.get(fn_ident).unwrap();

        debug!("Checking function '{}'", fn_ident);

        for overload_idx in overloads {
            let overload = self.fn_vec.get(*overload_idx).unwrap();

            debug!("Checking overload: '{}'", overload.signature);

            if overload.scope.is_none() {
                function = Some(overload);
                function_index = Some(*overload_idx);
                break;
            }
        }

        // these should always be the same, as we are checking an indexed func, but compiler cannot be sure.
        let (function, function_index) = match (function, function_index) {
            (Some(function), Some(function_index)) => (function, function_index),
            _ => return comp_err!((Node { start, end, node: Stmt::FnSemantics {id: self.fn_vec.len()} }), "Function '{}' does not exist with the same signature", fn_ident),
        };

        self.ctx.func.signature = function.signature.clone();
        self.ctx.func.return_type = function.return_type.clone();
        
        // Create lambda for custom scope check
        println!("{}", text_to_ascii_art::to_art(function.signature.clone(), "small", 2, 0, 0).unwrap());
        
        // Does not error here, so I can construct 'fn_sem', to then error with that information.
        let (checked_scope, func_body_error) = match self.check_fn_scope(scope, &function.args, &args) {
            CompilerResult::Ok(node) => (node, None),
            CompilerResult::Err { data, error } => {
            let data = match data {
                    Some(node) => node,
                    None => Node {
                        start,
                        end,
                        node: Scope {
                            stmts: Vec::new(),
                            inherits_stmts: false,
                        },
                    }
                };
                
                (data, Some(error))
            }
        };

        let fn_sem = Node {
            start: checked_scope.start,
            end: checked_scope.end,
            node: Stmt::FnSemantics {id: self.fn_vec.len()} // haven't pushed to vec yet
        };
        
        if let Some(error) = func_body_error {
            return CompilerResult::Err {data: Some(fn_sem), error}
        }

        if self.ctx.func.return_type != Type::Void {
            let scope_returns = match checked_scope.node.stmts.last() {
                Some(stmt) => Self::check_node_returns(&stmt.node),
                None => return comp_err!((fn_sem), "Not all code paths return in '{}'", function.signature),
            };
        }

        // add the checked function scope 
        let mut_function = self.fn_vec.get_mut(function_index).unwrap();
        mut_function.scope = Some(checked_scope);

        println!();
        debug!("Function '{}' checked successfully", mut_function.signature);
        CompilerResult::Ok(fn_sem)
    }
    
    // Create arg semantics
    // - check for duplicates
    // - check for used names (keywords & other variables)
    fn create_arg_semantics(&self, args: &[Arg], fn_ident: &str) -> Result<Vec<Type<FullType>>> {
        let mut semantics = Vec::new();
        for arg in args {
            let arg_ident = arg.ident.str();

            if semantics
                .iter()
                .map(|x| self.get_full_ident(x))
                .find(|x| *x == arg_ident)
                .is_some()
            {
                return err!(
                    "Duplicate argument name: '{arg_ident}' in function {fn_ident}"
                );
            } else if self.stack_var_map.contains_key(arg_ident) {
                return err!(
                    "Argument name in use: {arg_ident} in function: {fn_ident}"
                );
            } else if self.type_map.contains_key(arg_ident) {
                return err!(
                    "Illegal argument name: {arg_ident} in function: {fn_ident}, Types are reserve keywords"
                );
            }
            let base_id = *self.type_map.get(arg.parse_type.type_tok.str()).unwrap();
            semantics.push(self.new_full(base_id, arg.parse_type.addr_mode));
        }

        Ok(semantics)
    }

    // iters over all overloads of a function, 
    // checks if the function already exists with the same signature 
    // if it finds a single match it returns its idx, else err.
    fn check_fn_overloads(&self, semantics: &[Type<FullType>], fn_ident: &str) -> Result<usize> {
        let overloads = match self.fn_map.get(fn_ident) {
            Some(overloads) => overloads,
            None => return Ok(0), // no existing overloads, this is the first index.
        };

        let mut matching = None;

        for overload in overloads {
            let overload = self.fn_vec.get(*overload).unwrap();
            if overload.args.len() != semantics.len() {
                continue;
            }

            for (idx, (overload, new)) in overload.args.iter().zip(semantics.iter()).enumerate() {
                if overload == new {
                    match matching {
                        Some(overload) => return err!(
                            "Function '{fn_ident}' already exists with the same signature"
                        ),
                        None => matching = Some(idx),                        
                    }
                }
            }
        }

        match matching {
            Some(overload) => Ok(overload),
            None => Ok(0) // no existing overloads, this is the first appearance of this function
        }
    }

    // check if its a stmt, or has a scope, which you should check.
    fn check_scope_returns(node: &Scope) -> bool {
        match node.stmts.last() {
            Some(stmt) => Self::check_node_returns(&stmt.node),
            None => false,
        }
    }
    fn check_node_returns(stmt: &Stmt) -> bool { 
        match &stmt {
            Stmt::Return(_) => return true, // already checked to be of valid return type.
            //Stmt::While { scope, ..} => Self::check_scope_returns(&scope.node), 
            Stmt::NakedScope(node) => Self::check_scope_returns(&node.node),
            Stmt::If { condition, scope, branches } => {
                if !Self::check_scope_returns(&scope.node) {
                    return false;
                }

                let mut branches_return = true; 
                for branch in branches {
                    if !Self::check_node_returns(&branch.node) {
                        return false;
                    }
                }

                true
            }
            Stmt::ElseIf { scope, .. } => Self::check_scope_returns(&scope.node),
            Stmt::Else(scope) => Self::check_scope_returns(&scope.node),
            _ => false,
        }   
    }

    fn check_fn_scope(&self, scope: Node<Scope>, semantics: &[Type<FullType>], args: &[Arg]) -> CompilerResult<Node<Scope>>{
        let mut scope_check;
        unsafe {
            let mut_self = self as *const Self as *mut Self;
            scope_check = (*mut_self).check_scope(
                scope,
                Some(|stmts: Vec<Node<Stmt>>| -> CompilerResult<Scope> {

                    let mut checked_stmts = Vec::with_capacity(stmts.len());

                    // add each arg as a variable for use in the function
                    for (arg_type, parse) in semantics.iter().zip(args.iter()) {
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

                        match (*mut_self).check_stmt(arg_stmt) {
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
                        match (*mut_self).check_stmt(stmt) {
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

                    CompilerResult::Ok(Scope {
                        stmts: checked_stmts,
                        inherits_stmts: false,
                    })
                }),
            );
        }

        scope_check
    }
    // endregion
    
    fn check_type_alias(&mut self, ident_str: &str, parse_type_str: &str) -> Result<()> {
        debug!("checking type alias: {ident_str}");

        // check if its already a type
        if self.type_map.get(ident_str).is_some() {
            return err!("Duplicate definition of a Type: '{}'", ident_str);
        } else if self.stack_var_map.contains_key(ident_str) {
            return err!("Illegal Type name, Variables are reserved: '{}'", ident_str);
        }

        let original_id = match self.type_map.get(parse_type_str) {
            Some(id) => *id,
            None => return err!("Type not found: '{}'", parse_type_str),
        };
        let original_type = self.type_vec.get(original_id).unwrap();

        self.type_vec.push(original_type.clone());
        self.type_map.insert(ident_str.to_string(), original_id); // points to original.
        
        Ok(())
    }
    // endregion
    
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
                CompilerResult::Err { data, error} => {
                    let node = match data {
                        Some(node) => node,
                        None => Scope {
                            stmts: Vec::new(),
                            inherits_stmts: does_inherit,
                        },
                    };
                    let end = match node.stmts.last() {
                        Some(stmt) => stmt.end,
                        None => scope.end,
                    };
                    return CompilerResult::Err {
                        data: Some(Node {
                            start: scope.start,
                            end,
                            node,
                        }),
                        error 
                    }
                },
            },
            None => {
                let mut stmts = Vec::new();
                for stmt in scope.node.stmts {
                    match self.check_stmt(stmt) {
                        CompilerResult::Ok(data) => stmts.push(data),
                        CompilerResult::Err { data, error } => {
                            let mut end = scope.end;
                            if let Some(data) = data {
                                end = data.end;
                                stmts.push(data)
                            }
                            return CompilerResult::Err {
                                data: Some(Node {
                                    start: scope.start,
                                    end,
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
        Logger::set_pos(stmt.start);
        print!("\n");
        debug!("checking {stmt:#?}");
        
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
                if let Type::Void = var_type {
                    return comp_err!("Cannot declare a variable of type 'void'");
                }

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

                // check assignment expression
                if let InitExpr::Some(ref expr) = var.init_expr {
                    let expected = self.get_type_sem(&var.var_type);
                    let init_expr = self.check_expr(expr)?;

                    debug!("init expr for '{}'\n{init_expr:#?}", var.ident.str());


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
            Stmt::If {
                condition,
                scope,
                branches,
            } => {
                let checked = self.check_expr(&condition)?;
                match checked.type_mode {
                    TypeMode::Boolean => (),
                    _ => {
                        return comp_err!(
                            "'If' statement condition not 'boolean'\n{condition:#?}"
                        );
                    }
                }

                let checked_scope = upgrade_err!(self.check_scope_default(scope), |scope| Node {
                    start: stmt.start,
                    end: scope.end,
                    node: Stmt::If {
                        condition,
                        scope,
                        branches,
                    },
                });

                let mut new_branches = Vec::new();
                for branch in branches {
                    new_branches.push(self.check_stmt(branch)?);
                }

                CompilerResult::Ok(Node {
                    start: stmt.start,
                    end: stmt.end,
                    node: Stmt::If {
                        condition,
                        scope: checked_scope,
                        branches: new_branches,
                    }
                })
            }
            Stmt::ElseIf { condition, scope } => {
                let checked = self.check_expr(&condition)?;
                if checked.type_mode != TypeMode::Boolean {
                    return comp_err!(
                        "'ElseIf' statement condition not 'boolean'\n{condition:#?}"
                    );
                }

                upgrade_result!(self.check_scope_default(scope), |scope| Node {
                    start: stmt.start,
                    end: scope.end,
                    node: Stmt::ElseIf {
                        condition,
                        scope,
                    },
                })
            }
            Stmt::Else(scope) => {
                upgrade_result!(self.check_scope_default(scope), |scope| Node {
                    start: stmt.start,
                    end: scope.end,
                    node: Stmt::Else(scope),
                })
            }
            Stmt::While { condition, scope } => {
                self.ctx.loop_count += 1;
                self.check_expr(&condition)?;
                let new_scope = upgrade_err!(self.check_scope_default(scope), |scope| Node {
                    start: stmt.start,
                    end: scope.end,
                    node: Stmt::While {
                        condition,
                        scope,
                    },
                });
                self.ctx.loop_count -= 1;

                CompilerResult::Ok(Node {
                    start: stmt.start,
                    end: stmt.end,
                    node: Stmt::While {
                        condition,
                        scope: new_scope,
                    }
                })
            }
            Stmt::Break => {
                if self.ctx.loop_count <= 0 {
                    return comp_err!((stmt), "Not inside a loop! cannot break");
                }
                CompilerResult::Ok(stmt)
            }
            Stmt::Return(ref expr) => {
                // Void return check
                let expr = match expr {
                    Some(expr) => expr,
                    None if self.ctx.func.return_type == Type::Void => return CompilerResult::Ok(stmt),
                    None => {
                        return comp_err!((stmt), "Mismatched '{}' return, expected '{:#?}', found =>\n'void'", self.ctx.func.signature, self.ctx.func.return_type);
                    }
                };

                let proposed_return_sem = self.check_expr(expr)?;
                let expected_return_sem = self.get_type_sem(&self.ctx.func.return_type);

                self.check_type_equivalence(&expected_return_sem, &proposed_return_sem)?;
                
                CompilerResult::Ok(stmt)
            }
            Stmt::NakedScope(scope) => {
                upgrade_result!(self.check_scope_default(scope), |scope| Node {
                    start: stmt.start,
                    end: scope.end,
                    node: Stmt::NakedScope(scope),
                })
            }
            Stmt::NakedExpr(ref expr) => {
                self.check_expr(expr)?;
                CompilerResult::Ok(stmt)
            }
            Stmt::FnDecl { ident, .. } => {
                comp_err!((stmt),"Functions cannot be nested, they're top level statements, {ident:#?}")
            }
            _ => comp_err!((stmt.clone()), "Unexpected statement {stmt:#?}"),
        }
    }

    fn check_expr(&self, expr: &Node<Expr>) -> Result<ExprSem> {
        match &expr.node {
            Expr::Term(term) => self.check_term(term, expr.start, expr.end),
            Expr::Binary { op, lhs, rhs } => {
                let lhs_checked = self.check_expr(lhs)?;
                let rhs_checked = self.check_expr(rhs)?;
                
                let resultant_form = self.check_type_equivalence(&lhs_checked, &rhs_checked)?;

                match resultant_form.addr_mode {
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
                    // TODO:(TOM) this need to be changed, 
                    AddressingMode::Primitive => {
                        // 'CMP'   => T, T       => bool
                        // 'LOG'   => bool, bool => bool
                        // 'Arith' => int, int   => int
                        match op {
                            _ if op.has_flags(TokenFlags::CMP) => {
                                Ok(ExprSem {
                                    form: resultant_form.form,
                                    type_mode: TypeMode::Boolean,
                                    addr_mode: AddressingMode::Primitive,
                                    width: 1,
                                })
                            }

                            _ if op.has_flags(TokenFlags::LOG) => {
                                match resultant_form.type_mode { 
                                    TypeMode::Boolean => { 
                                        Ok(ExprSem {
                                            form: resultant_form.form,
                                            type_mode: TypeMode::Boolean,
                                            addr_mode: AddressingMode::Primitive,
                                            width: 1,
                                        })
                                    }
                                    _ => err!("logical operations require: {op:?}..\n{lhs:#?}..\n{rhs:#?}"),
                                }
                            }

                            _ if op.has_flags(TokenFlags::ARITH) => {
                                match resultant_form.type_mode {
                                    TypeMode::Int(_) => Ok(ExprSem {
                                        form: resultant_form.form,
                                        type_mode: resultant_form.type_mode,
                                        addr_mode: AddressingMode::Primitive,
                                        width: resultant_form.width,
                                    }),
                                    _ => err!("Arithmetic require integers: {op:?}..\n{lhs:#?}..\n{rhs:#?}"),
                                }
                            }
                            
                            _ if op.has_flags(TokenFlags::BIT) => {
                                match resultant_form.type_mode {
                                    TypeMode::Int(_) => Ok(ExprSem {
                                        form: resultant_form.form,
                                        type_mode: resultant_form.type_mode,
                                        addr_mode: AddressingMode::Primitive,
                                        width: resultant_form.width,
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
                        err!("[ARRAY] Invalid Unary Expression: {op:?}\n{checked:#?}")
                    }
                    AddressingMode::Pointer(depth) => {
                        match op {
                            // deref address into a variable.
                            TokenKind::Ptr if depth == 1 => {
                                Ok(ExprSem {
                                    form: ExprForm::Variable, // this is going to point to a mem loc, a var!
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
                                    form: ExprForm::Variable, // dereferencing into a memory location, must be a var.
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
                                TypeMode::Int(true) => Ok(checked),
                                TypeMode::Int(false) if checked.form == ExprForm::Literal => Ok(ExprSem {
                                    form: checked.form,
                                    type_mode: TypeMode::Int(true),
                                    addr_mode: AddressingMode::Primitive,
                                    width: checked.width,
                                }),
                                _ => err!("'UnarySub' expects a signed integer, found {checked:#?}")
                            }
                           
                            TokenKind::Not => match checked.type_mode {
                                TypeMode::Boolean | TypeMode::Int(_) => Ok(checked),
                                _ => err!("'Not' expects a boolean or integer, found {checked:#?}")
                            }

                            TokenKind::Ampersand if checked.form != ExprForm::Variable => err!("'AddressOf' expects a variable, found {checked:#?}"),
                            TokenKind::Ampersand => Ok(ExprSem {
                                form: ExprForm::Compound,
                                type_mode: checked.type_mode,
                                addr_mode: AddressingMode::Pointer(1),
                                width: PTR,
                            }),

                            _ => err!("[PRIMITIVE] Invalid Unary Expression: {op:?}..\n{checked:#?}"),
                        }
                    }
                }
            }
        }
    }

    fn check_term(&self, term: &Term, start: Pos, end: Pos) -> Result<ExprSem> {
        Logger::set_pos(start);

        match &term {
            Term::True | Term::False => Ok(ExprSem {
                form: ExprForm::Literal,
                type_mode: TypeMode::Boolean,
                addr_mode: AddressingMode::Primitive,
                width: 1,
            }),
            Term::Ident => {
                let var = self.get_var(&Contents::get_src_oneline(start, end))?;
                let addr_mode = match &var.var_type {
                    Type::Void => return err!("Cannot use the void type in expression"),
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
                // get function from map
                let func_ids = match self.fn_map.get(ident.str()) {
                    Some(ids) => ids.as_slice(),
                    None => return err!("Function not found: '{ident:#?}'"),
                };

                let sem_args = args.iter().map(|arg| self.check_expr(arg)).collect::<Result<Vec<_>>>()?;

                let mut matched_overload = -1;
                for overload in func_ids {
                    if matched_overload != -1 {
                        break;
                    }

                    let func = self.fn_vec.get(*overload).unwrap();
                    if sem_args.len() != func.args.len() {
                        continue;
                    }

                    let mut matches_overload = true;
                    for (arg, arg_sem) in func.args.iter().zip(sem_args.iter()) {
                        let expected = self.get_type_sem(arg);
                        if self.check_type_equivalence(&expected, arg_sem).is_err() {
                            matches_overload = false;
                            break;
                        }
                    }
                    if matches_overload {
                        matched_overload = (*overload) as isize;
                        break;
                    }
                }

                if matched_overload == -1 {
                    return err!("No matching function overload for '{ident:#?}'");
                }

                let func_semantics = self.fn_vec.get(matched_overload as usize).unwrap();

                Ok(ExprSem {
                    form: ExprForm::Compound,
                    type_mode: self.get_full_mode(&func_semantics.return_type),
                    addr_mode: self.get_full_addrmode(&func_semantics.return_type),
                    width: self.get_full_width(&func_semantics.return_type),
                })
            }
        }
    }

    fn check_type_equivalence(&self, a: &ExprSem, b: &ExprSem) -> Result<ExprSem> {
        if a.addr_mode != b.addr_mode {
            return err!(
                "Expr of different AddrMode! {a:?} vs {b:?}, {a:#?}\n.. {b:#?}",
                a = a.addr_mode,
                b = b.addr_mode
            );
        }

        // the expression involves a literal e.g. 5 == var_name_here;
        // literals have no defined width yet, so we assume the width of the other thing.
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
            (TypeMode::Boolean, TypeMode::Boolean) => (),
            (TypeMode::Int(_), TypeMode::Int(_)) if literal_expr => (),
            (TypeMode::Int(a_sign), TypeMode::Int(b_sign)) if a_sign == b_sign => (),
            _ => {
                return err!(
                    "TypeMode mismatch: {:?} != {:?} ..\n{a:#?}\n.. {b:#?}",
                    a.type_mode,
                    b.type_mode
                )
            }
        }

        // prevents later coercion as it won't be a pure literal e.g. 5 + 5 != 5 + var_name_here
        let form= if a.form == ExprForm::Literal && b.form == ExprForm::Literal {
            ExprForm::Literal
        } else {
            ExprForm::Compound
        };

        let expr_sem = ExprSem {
            form,
            addr_mode: a.addr_mode,
            type_mode: a.type_mode,
            width: max(a.width, b.width),
        };
        debug!("New: {expr_sem:#?}");
        Ok(expr_sem)
    }

    // region: Small_Components

    fn create_func_signature(&self, ident: &str, args_semantics: &[Type<FullType>]) -> String {
        match ident {
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
        }
    }

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
            Type::Void => Type::Void,
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

    fn get_full_ident(&self, inp_type: &Type<FullType>) -> &str {
        match inp_type {
            Type::Void => todo!("void semantics"),
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
            Type::Void => todo!("void semantics"),
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
            Type::Void => todo!("void semantics"),
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
            Type::Void => todo!("void semantics"),
            Type::Primitive(full) => full.addr_mode,
            Type::Struct { .. } => todo!("struct addr_mode calculation"),
            Type::Union { .. } => todo!("union addr_mode calculation"),
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

     // base type width depends solely on form
     fn get_base_width(inp_type: &Type<BaseType>) -> usize {
        match inp_type {
            Type::Void => todo!("void semantics"),
            Type::Primitive(base) => base.width,
            Type::Struct { .. } => todo!("struct width calculation"),
            Type::Union { .. } => todo!("union width calculation"),
        }
    }

    fn get_base_mode(inp_type: &Type<BaseType>) -> TypeMode {
        match inp_type {
            Type::Void => todo!("void semantics"),
            Type::Primitive(base) => base.mode,
            Type::Struct { .. } => todo!("struct mode calculation"),
            Type::Union { .. } => todo!("union mode calculation"),
        }
    }

    fn get_base_ident(inp_type: &Type<BaseType>) -> &str {
        match inp_type {
            Type::Void => todo!("void semantics"),
            Type::Primitive(base) => base.ident.as_str(),
            Type::Struct { .. } => todo!("struct ident calculation"),
            Type::Union { .. } => todo!("union ident calculation"),
        }
    }

    // endregion
}