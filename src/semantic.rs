/* >>SEMANTIC<< The rules of the language, checking the meaning of the program.
(1) ❌  - Allow for function overloading on return types?
            - Do I want this, function overloads prob should just have different args but same purpose.
            - requries a more robust type checker, must know the desired type ahead, so it can choose the correct overload.
                , This is because currently it only checks args.
(2) ❌  - Expand the alias system, to allow for addressed types to be aliased
            - currently only primitive types, e.g. "int" can be aliased, not "int*" 
            - required 2 types of alias, 
                - Primitive Alias: "int" -> "my_int"
                - Addressed Alias: "int*" -> "my_int_ptr"
*/

use crate::{
    comp_err, debug, err, formatting::{self, PosAwareDebug}, lex::{Token, TokenFlags, TokenKind}, parse::{Arg, Ast, Expr, InitExpr, Node, ParseType, Scope, Stmt, StructField, Term}, upgrade_err, upgrade_result, utils::{self, CompilerResult, Contents, Logger, Pos}
};
use anyhow::{Context, Error, Result};
use educe::Educe;
use std::{
    cmp::max, collections::HashMap, convert::Infallible, ops::Add, ptr::NonNull
};

// region: Type Definitions
pub type Bytes = usize;
const PTR: Bytes = 8;
const VOID_ID: usize = 0; // void is a special case, its not properly incorporated.

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum AddressingMode {
    Primitive,
    Pointer { depth: u32 },
    Array { depth: u32, len: usize },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum TypeMode {
    Boolean,
    Int { signed: bool },
    Struct,
    Union,
    Void, // represents the special case of 'void'.
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum ExprForm {
    Sized, // a variable, allows for addr of etc
    Compound, // a compound expression, e.g. a + b, doesn't have coercion semantics like a literal
    Literal, // has some sizing freedoms as its a literal (0 size rn)!
}

// This defines a language primitive type, such as 'i32'
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
struct PrimitiveType {
    ident: String,
    mode: TypeMode,
    width: Bytes,
}

// it is a type that has an addressing mode, e.g. an 'i32' that is a 'pointer'
// This is used by statements and expressions,
#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct AddressedType {
    type_id: usize,
    addr_mode: AddressingMode,
}

#[derive(Debug, Clone)]
pub struct StructMember {
    ident: String,
    addr_type: AddressedType, // the type of the member, with addressing mode
}

// These are what is stored globally, in the 'type registry'
// these are then referenced via id by addressed types.
#[derive(Debug, Clone)]
pub enum StoredType {
    Primitive {
        ident: String,
        mode: TypeMode,
        width: Bytes,
    },
    // TODO(2): currently this is restricted to stored types,
    // it cannot represent an addressed type, e.g. a pointer to a struct.
    // this would require a distinction between an "alias" and "addressed alias".
    Alias {
        ident: String,
        aliased_id: usize,
    },
    Struct {
        ident: String,
        width: Bytes,
        members: Vec<StructMember>,
    },
}


#[derive(Debug, Clone, Copy)]
pub enum ExprData<'a> {
    Primitive {
        width: Bytes,
    },
    Pointer {
        root_type: AddressedType,
    },
    Struct {
        width: Bytes,
        members: &'a[StructMember], 
    },
}

// An expression is evaluated based on:
// TypeMode: what operations can be performed
// AddressingMode: how is it represented in memory, if at all
// Width: to know how big the result should be: MAX(op1.width, op2.width)
#[derive(Debug, Clone, Copy)]
struct ExprSem<'a> {
    form: ExprForm,
    type_mode: TypeMode,
    addr_mode: AddressingMode,
    expr_data: ExprData<'a>,
}
 

#[derive(Debug, Clone)]
pub struct Variable {
    ident: Token,
    mutable: bool,
    var_type: AddressedType,
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
    args: Vec<AddressedType>,
    return_type: AddressedType,
    #[educe(Debug(method = "crate::formatting::format_optional"))]
    scope: Option<Node<Scope>>,
}

#[derive(Debug)]
struct FuncContext {
    return_type: AddressedType, // optional as it may be void, which isn't a type!
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

    #[educe(Debug(ignore))]
    pub type_map: HashMap<String, usize>,
    pub type_vec: Vec<StoredType>,

    #[educe(Debug(ignore))]
    stack_var_vec: Vec<Variable>,
    #[educe(Debug(ignore))]
    pub stack_var_map: HashMap<String, usize>,
    
    // this stores the names of all functions names,
    // this then gives us a list of all overloads for this function.
    pub fn_vec: Vec<Function>,
    pub fn_map: HashMap<String, Vec<usize>>,
}

// endregion

impl Checker {
    pub fn new() -> Checker {
        let type_vec = Vec::from([
            new_primitive("void", 0, TypeMode::Void), // void is a special case, no size
            new_primitive("bool", 1, TypeMode::Boolean),
            new_primitive("u8", 1, TypeMode::Int { signed: false }),
            new_primitive("u16", 2, TypeMode::Int { signed: false }),
            new_primitive("u32", 4, TypeMode::Int { signed: false }),
            new_primitive("u64", 8, TypeMode::Int { signed: false }),
            new_primitive("usize", PTR, TypeMode::Int { signed: false }),
            new_primitive("i8", 1, TypeMode::Int { signed: true }),
            new_primitive("i16", 2, TypeMode::Int { signed: true }),
            new_primitive("i32", 4, TypeMode::Int { signed: true }),
            new_primitive("i64", 8, TypeMode::Int { signed: true }),
            new_primitive("isize", PTR, TypeMode::Int { signed: true }),

            // Self::new_prim("f32", 4, TypeMode::Int(true)),
            // Self::new_prim("f64", 8, TypeMode::Int(true)),
        ]);
        let mut type_map = HashMap::with_capacity(type_vec.len());
        for (idx, base) in type_vec.iter().enumerate() {
            let ident = match base {
                StoredType::Primitive { ident, .. } => ident.clone(),
                _ => unreachable!()
            };  
            type_map.insert(ident, idx);
        }

         Self {
            ast: Ast { stmts: Vec::new() },
            ctx: SemContext {
                loop_count: 0,
                scope_depth: 0,
                inherit_bounds: Vec::new(),
                func: FuncContext {
                    return_type: AddressedType { type_id: VOID_ID, addr_mode: AddressingMode::Primitive },
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
        Logger::set_pos(stmt.start);

        // index all the functions and type aliases, so we can call a func later in the file & recurse.
        debug!("Indexing top level stmt: {stmt:#?}");

        match &stmt.node {
            Stmt::FnDecl { ident, args, scope, return_type } => {
                let fn_ident = ident.str();
                let semantics = self.create_arg_semantics(&args, fn_ident).with_context(|| "failed to index function arguments")?;
                let signature = self.create_func_signature(ident.str(), &semantics);
                
                let return_type = match return_type {
                    Some(parse_type) => {
                        AddressedType {
                            type_id: *self.type_map.get(parse_type.ident.str()).unwrap(),
                            addr_mode: parse_type.addr_mode,
                        }
                    }
                    None => AddressedType { type_id: VOID_ID, addr_mode: AddressingMode::Primitive },
                };

                self.check_overload_collisions(fn_ident, &semantics, return_type)
                    .with_context(|| format!("The function overload '{signature}' is not valid"))?;

                self.fn_vec.push(Function {
                    ident: ident.clone(),
                    signature,
                    args: semantics,
                    return_type,
                    scope: None,
                });

                let is_overload = self.fn_map.contains_key(fn_ident);
                if is_overload {
                    self.fn_map.get_mut(fn_ident).unwrap().push(self.fn_vec.len() - 1);
                } else {
                    self.fn_map.insert(fn_ident.to_string(), vec![self.fn_vec.len() - 1]);
                }

                Ok(())
            }  
            Stmt::TypeAlias { ident, parse_type } => {
                self.check_type_alias(ident.str(), parse_type.ident.str())
            }, 
            Stmt::StructDecl { ident, fields } => {
                self.check_struct_decl(*ident, fields)
            }
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
            Stmt::TypeAlias { .. } | Stmt::StructDecl { .. } => CompilerResult::Ok(None),
            _ => comp_err!(
                "A Program only consists of Top-Level Statements, this is a {stmt:?}"
            ),
        }
    }

    // region: check_function
    fn check_fn_decl(&mut self, start: Pos, end: Pos, ident: Token, args: Vec<Arg>, scope: Node<Scope>, return_type: Option<ParseType>) -> CompilerResult<Node<Stmt>> {
        // check for name collisions
        let fn_ident = ident.str();

        // Functions are indexed in order, so the first non initialised overload is the current one.
        let mut function = None;
        let mut function_index = None;
        let overloads = self.fn_map.get(fn_ident).unwrap();

        debug!("Checking function '{}'", fn_ident);

        for overload_idx in overloads {
            let overload = self.fn_vec.get(*overload_idx).unwrap();

            debug!("Checking overload: '{}'", overload.signature);

            // this overload isn't initialised, so its the current overload.
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
        let func_name = function.signature.split('(').next().unwrap().to_string();
        let func_args = function.signature.split(')').skip(1).next().unwrap_or("");;
        println!("{}\n{func_args}\n", text_to_ascii_art::to_art(func_name, "small", 2, 0, 0).unwrap());
        
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

        if self.ctx.func.return_type.type_id != VOID_ID {
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
    fn create_arg_semantics(&self, args: &[Arg], fn_ident: &str) -> Result<Vec<AddressedType>> {
        let mut semantics = Vec::new();
        let mut semantic_map = HashMap::new();
        for arg in args {
            let arg_ident = arg.ident.str();

            if !self.is_ident_unique(arg_ident) || semantic_map.contains_key(arg_ident) {
                return err!("Argument's name is already in use, '{arg_ident}' in function {fn_ident}");
            }

            let addr_type = AddressedType {
                type_id: *self.type_map.get(arg.parse_type.ident.str()).unwrap(),
                addr_mode: arg.parse_type.addr_mode,
            };
            semantic_map.insert(arg_ident.to_string(), addr_type.clone());
            semantics.push(addr_type);
        }

        Ok(semantics)
    }

    // iters over all overloads of a function, 
    // checks if the function already exists with the same signature 
    // if it finds a single match it returns its idx, else err.
    fn check_overload_collisions(&self, fn_ident: &str, semantics: &[AddressedType], return_type: AddressedType) -> Result<usize> {
        let overloads = match self.fn_map.get(fn_ident) {
            Some(overloads) => overloads,
            None => {
                debug!("no instances of this function found, returning 0");
                return Ok(0)}, // this is a new function, its all good!
        };
        
        let original_decl = self.fn_vec.get(overloads[0]).unwrap();

        let mut matching = None;
        for overload in overloads {
            let overload = self.fn_vec.get(*overload).unwrap();

            if (return_type != original_decl.return_type) {
                return err!("All overloads of Function '{fn_ident}' must have the same return type");
            }

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
            None => Ok(0) // no matching overloads, this is unique!
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

    fn check_fn_scope(&self, scope: Node<Scope>, semantics: &[AddressedType], args: &[Arg]) -> CompilerResult<Node<Scope>>{
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
                            end: parse.parse_type.ident.end_pos(),
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
    
    fn check_type_alias(&mut self, alias_str: &str, parse_type_str: &str) -> Result<()> {
        debug!("checking type alias: {alias_str}");

        // check if its already a type
        if self.type_map.get(alias_str).is_some() {
            return err!("Duplicate definition of a Type: '{}'", alias_str);
        } else if self.stack_var_map.contains_key(alias_str) {
            return err!("Illegal Type alias, a variable is assigned this name: '{}'", alias_str);
        }

        let original_id = match self.type_map.get(parse_type_str) {
            Some(id) => *id,
            None => return err!("Type not found: '{}'", parse_type_str),
        };
        let aliased_type = StoredType::Alias { ident: alias_str.to_string(), aliased_id: original_id };

        self.type_map.insert(alias_str.to_string(), self.type_vec.len()); 
        self.type_vec.push(aliased_type);
        
        Ok(())
    }

    fn check_struct_decl(&mut self, ident: Token, fields: &[Arg]) -> Result<()> {

        debug!("checking struct decl: {ident:#?}");

        // check if its already a type
        if self.type_map.get(ident.str()).is_some() {
            return err!("Duplicate definition of a Type: '{}'", ident.str());
        } 

        let mut width = 0;
        let mut members = Vec::with_capacity(fields.len());
        for (i, field) in fields.iter().enumerate() {
            if fields.iter().skip(i+1).position(|x| x.ident.str() == field.ident.str()).is_some() {
                return err!("Duplicate definition of a Struct field: '{}'", field.ident.str());
            }
            else if field.ident.str() == ident.str() {
                match field.parse_type.addr_mode {
                    AddressingMode::Pointer { .. } => (), // not recursive.
                    _ => return err!("Struct field cannot be the same name as the struct: '{}'", field.ident.str()),
                }
            }
            else if self.type_map.contains_key(field.ident.str()) {
                return err!("Illegal struct field, a type is assigned this name: '{}'", field.ident.str());
            } 

            let field_type_ident = field.parse_type.ident.str();
            debug!("trying to find field type: {field_type_ident}");
            let field_id = match self.type_map.get(field_type_ident) {
                Some(id) => *id,
                None => return err!("Struct field's type not found: '{}'", field_type_ident),
            };
            let field_type = self.type_vec.get(field_id).unwrap();

            width += self.get_width(field_type);
            members.push(StructMember {
                ident: field.ident.str().to_string(),
                addr_type: AddressedType {
                    type_id: field_id,
                    addr_mode: field.parse_type.addr_mode,
                }
            });
        }

        
        let struct_type = StoredType::Struct {
            ident: ident.str().to_string(),
            width,
            members,
        };
        
        debug!("struct decl looks ok, adding type.. {:#?}", struct_type);
        
        self.type_map.insert(ident.str().to_string(), self.type_vec.len());
        self.type_vec.push(struct_type);
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
        print!("\n\n");
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

                let base_id = *self.type_map.get(arg.parse_type.ident.str()).unwrap();
                if base_id == VOID_ID {
                    return comp_err!("Cannot declare a variable of type 'void'");
                };

                let var_type = AddressedType {
                    type_id: base_id,
                    addr_mode: arg.parse_type.addr_mode,
                };
                
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
                    let expected = self.create_expr_semantics(var.var_type)?;
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

                let expected = self.create_expr_semantics(var.var_type)?;
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
                    None if self.ctx.func.return_type.type_id == VOID_ID => return CompilerResult::Ok(stmt),
                    None => {
                        return comp_err!((stmt), "Mismatched '{}' return, expected '{:#?}', found =>\n'void'", self.ctx.func.signature, self.ctx.func.return_type);
                    }
                };

                let proposed_return_sem = self.check_expr(expr)?;
                let expected_return_sem = self.create_expr_semantics(self.ctx.func.return_type)?;

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

    // region: check expr
    fn check_expr(&self, expr: &Node<Expr>) -> Result<ExprSem> {
        match &expr.node {
            Expr::Term(term) => self.check_term(term, expr.start, expr.end),
            Expr::Binary { op, lhs, rhs } => {
                match op {
                    TokenKind::Dot => self.check_expr_dot(*op, lhs, rhs),
                    _ => self.check_expr_unified_binary(*op, lhs, rhs)
                }
            }
            // unary operators tend to be very unique, so they are individually matched.
            Expr::Unary { op, expr } => self.check_expr_unary(*op, expr)
        }
    }

    fn check_expr_dot(&self, op: TokenKind, lhs: &Box<Node<Expr>>, rhs: &Box<Node<Expr>>) -> Result<ExprSem> {
        let lhs_ident = Contents::get_src_oneline(lhs.start, lhs.end);
        let lhs_checked = self.check_expr(lhs)?;

        if lhs_checked.type_mode != TypeMode::Struct {
            return err!(
                "[STRUCT] '{lhs_ident}' is not a struct, it does not have member fields to access.\n{lhs_checked:#?}..\n{rhs:#?}\n"
            );
        }

        // Either another expr, or an ident, that MUST be a member of the structure.
        let accessed_member_sem = match &rhs.node {
            Expr::Term(Term::Ident) => {
                let struct_members = match lhs_checked.expr_data {
                    ExprData::Struct { members, .. } => members,
                    _ => return err!("Expected struct data in ExprSem for dot access, found\n{lhs_checked:#?}"),
                };

                let rhs_ident = Contents::get_src_oneline(rhs.start, rhs.end);
                match struct_members.iter().find(|m| m.ident == rhs_ident) {
                    Some(member) =>  {
                        self.create_expr_semantics(member.addr_type)?
                    },
                    _ => return err!("'{lhs_ident}' does not have a member named '{rhs_ident}'"),
                }
            }
            _ => self.check_expr(rhs)?,
        };

        Ok(accessed_member_sem)
    }

    fn check_expr_unified_binary(&self, op: TokenKind, lhs: &Box<Node<Expr>>, rhs: &Box<Node<Expr>>) -> Result<ExprSem> {
        let lhs = self.check_expr(lhs)?;
        let rhs = self.check_expr(rhs)?;
        let agreed_type = self.check_type_equivalence(&lhs, &rhs)?;
        match agreed_type.addr_mode {
            AddressingMode::Array { .. } => {
                err!(
                    "[ARRAY] Invalid binary expression: {op:?}..\n{lhs:#?}..\n{rhs:#?}"
                )
            }
            AddressingMode::Pointer{ .. } => {
                err!(
                    "[POINTER] Invalid binary expression: {op:?}..\n{lhs:#?}..\n{rhs:#?}"
                )
            }
            AddressingMode::Primitive => {
                // 'CMP'   => T, T       => bool
                // 'LOG'   => bool, bool => bool
                // 'Arith' => int, int   => int
                match op {
                    _ if op.has_flags_binary(TokenFlags::CMP) => {
                        Ok(ExprSem {
                            form: ExprForm::Sized,
                            type_mode: TypeMode::Boolean,
                            addr_mode: AddressingMode::Primitive,
                            expr_data: ExprData::Primitive { width: 1 }
                        })
                    }

                    _ if op.has_flags_binary(TokenFlags::LOG) => {
                        match agreed_type.type_mode { 
                            TypeMode::Boolean => { 
                                Ok(ExprSem {
                                    form: agreed_type.form,
                                    type_mode: TypeMode::Boolean,
                                    addr_mode: AddressingMode::Primitive,
                                    expr_data: ExprData::Primitive { width: 1 },
                                })
                            }
                            _ => err!("[PRIMITIVE] logical operations require: {op:?}..\n{lhs:#?}..\n{rhs:#?}"),
                        }
                    }

                    _ if op.has_flags_binary(TokenFlags::ARITH) => {
                        match agreed_type.type_mode {
                            TypeMode::Int { .. } => Ok(ExprSem {
                                form: ExprForm::Sized,
                                type_mode: agreed_type.type_mode,
                                addr_mode: AddressingMode::Primitive,
                                expr_data: ExprData::Primitive { width: self.get_expr_width(&agreed_type) },
                            }),
                            _ => err!("[PRIMITIVE] Arithmetic require integers: {op:?}..\n{lhs:#?}..\n{rhs:#?}"),
                        }
                    }
                    
                    _ if op.has_flags_binary(TokenFlags::BIT) => {
                        match agreed_type.type_mode {
                            TypeMode::Int { .. } => Ok(ExprSem {
                                addr_mode: AddressingMode::Primitive,
                                form: ExprForm::Sized,
                                type_mode: agreed_type.type_mode,
                                expr_data: ExprData::Primitive { width: self.get_expr_width(&agreed_type) },
                            }),
                            _ => err!("[PRIMITIVE] Bitwise require integers: {op:?}..\n{lhs:#?}..\n{rhs:#?}"),
                        }
                    }

                    _ => err!("[PRIMITIVE] Invalid binary expression: {op:?}..\n{lhs:#?}..\n{rhs:#?}")
                }
            }
        }
    }

    fn check_expr_unary(&self, op: TokenKind, expr: &Node<Expr>) -> Result<ExprSem> {
        let checked = self.check_expr(&*expr)?;
        // 'Unary sub' signed int or lit => signed int literal
        // 'Cmp Not'   bool              => bool
        // 'Bit Not'   primitive         => primitive
        // 'Addr of'   var               => ptr
        // 'Ptr Deref' ptr               => var
        match checked.addr_mode {
            AddressingMode::Array{ .. } => {
                err!("[ARRAY] Invalid Unary Expression: {op:?}\n{checked:#?}")
            }
            AddressingMode::Pointer{ depth } => {
                match op {
                    // deref address into a variable.
                    TokenKind::Ptr if depth == 1 => {
                        match checked.expr_data {
                            ExprData::Pointer { root_type } => {
                                let stored_type = self.type_vec.get(root_type.type_id).unwrap();
                                let mut expr_semantics = self.create_expr_semantics(root_type)?;

                                expr_semantics.form = ExprForm::Sized;
                                Ok(expr_semantics)
                            }
                            _ => err!("[POINTER] 'Ptr Deref' expects a pointer, found {checked:#?}"),
                        }
                    }
                    TokenKind::Ptr => {
                        Ok(ExprSem{
                            form: ExprForm::Sized,
                            addr_mode: AddressingMode::Pointer { depth: depth - 1 },
                            .. checked
                        })
                    }
                    _ => err!(
                        "[POINTER] Invalid Unary Expression: {op:?}..\n{checked:#?}"
                    ),
                }
            }
            AddressingMode::Primitive => {
                match op {
                    TokenKind::Sub => match checked.type_mode {
                        TypeMode::Int{signed: true} => Ok(checked),
                        TypeMode::Int{signed:false} if checked.form == ExprForm::Literal => {
                            Ok(ExprSem {
                                type_mode: TypeMode::Int{ signed: true },
                                addr_mode: AddressingMode::Primitive,
                                .. checked
                            })
                        }
                        _ => err!("[PRIMITIVE] 'UnarySub' expects a signed integer, found {checked:#?}")
                    }
                    
                    TokenKind::Not => match checked.type_mode {
                        TypeMode::Boolean | TypeMode::Int{ .. } => Ok(checked),
                        _ => err!("[PRIMITIVE] 'Not' expects a boolean or integer, found {checked:#?}")
                    }

                    TokenKind::Ampersand if checked.form != ExprForm::Sized => err!("[PRIMITIVE] 'AddressOf' expects a variable, found {checked:#?}"),
                    TokenKind::Ampersand => Ok(ExprSem {
                            form: ExprForm::Compound,
                            type_mode: checked.type_mode,
                            addr_mode: AddressingMode::Pointer { depth: 1 },
                            expr_data: ExprData::Pointer {
                                // THIS NEEDS TO BE THE TYPE AFTER DEREF
                                root_type: AddressedType {
                                    type_id: 1,
                                    addr_mode: todo!()
                                },
                            },
                        }),
                    _ => err!("[PRIMITIVE] Invalid Unary Expression: {op:?}..\n{checked:#?}"),
                }
            }
        }
    }
    // endregion: check expr

    fn check_term(&self, term: &Term, start: Pos, end: Pos) -> Result<ExprSem> {
        Logger::set_pos(start);

        match &term {
            Term::True | Term::False => Ok(ExprSem {
                form: ExprForm::Literal,
                type_mode: TypeMode::Boolean,
                addr_mode: AddressingMode::Primitive,
                expr_data: ExprData::Primitive { width: 1 },
            }),
            Term::Ident => {
                let var = self.get_var(&Contents::get_src_oneline(start, end))?;
                self.create_expr_semantics(var.var_type)
            }
            Term::IntLit => {
                Ok(ExprSem {
                    form: ExprForm::Literal,
                    type_mode: TypeMode::Int{ signed: false },
                    addr_mode: AddressingMode::Primitive,
                    expr_data: ExprData::Primitive { width: 0 },
                })
            }
            Term::StructLit { ident, fields } => {
                let expr_data = self.check_struct_literal(ident, fields)?;
                Ok(ExprSem {
                    form: ExprForm::Literal,
                    addr_mode: AddressingMode::Primitive,
                    type_mode: TypeMode::Struct,
                    expr_data,
                })
            },
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
                        let expected = self.create_expr_semantics(*arg)?;
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
             
                self.create_expr_semantics(func_semantics.return_type)
            }
        }
    }

    fn check_type_equivalence<'a>(&'a self, a: &'a ExprSem, b: &'a ExprSem) -> Result<ExprSem<'a>> {
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
        match (&a.expr_data, &b.expr_data) {
            _ if literal_expr => (),
            // cannot assign something bigger than the 'container'
            (ExprData::Primitive { width: a_width }, ExprData::Primitive { width: b_width }) if a_width < b_width => {
                return err!(
                    "Illegal Type Narrowing, Assignee({}) < Assigner({}), {a:#?}\n.. {b:#?}",
                    a_width,
                    b_width
                );
            },
            _ => (),
        }

        match (a.type_mode, b.type_mode) {
            (TypeMode::Int{ .. }, TypeMode::Int{..}) if literal_expr => (),
            (TypeMode::Int{ signed: a_sign}, TypeMode::Int{ signed: b_sign }) if a_sign == b_sign => (),
            (TypeMode::Struct, TypeMode::Struct) => {
                // check that members are equal. 
                debug!("Checking Struct equivalence: {a:#?} vs {b:#?}");
                match (&a.expr_data, &b.expr_data) {
                    (ExprData::Struct { members: a_members, width: a_width }, ExprData::Struct { members: b_members, width: b_width }) if a_members.len() == b_members.len() && a_width == b_width => {
                        for (a_member, b_member) in a_members.iter().zip(b_members.iter()) {
                            let a_member_sem = self.create_expr_semantics(a_member.addr_type)?;
                            let b_member_sem = self.create_expr_semantics(b_member.addr_type)?;

                            if self.check_type_equivalence(&a_member_sem, &b_member_sem).is_err() {
                                return err!(
                                    "Struct members mismatch: {a_member:#?} != {b_member:#?}\n{a:#?}\n.. {b:#?}",
                                    a = a,
                                    b = b
                                );
                            }
                        }
                    },
                    _ => return err!("Struct data mismatch: {a:#?} != {b:#?}"),
                }
            },
            _ if a.type_mode == b.type_mode => (),
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
            .. *a
        };
        debug!("New: {expr_sem:#?}");
        Ok(expr_sem)
    }

    // region: Small_Components
    fn check_struct_literal(
        &self,
        ident: &Token,
        fields: &[StructField],
    ) -> Result<ExprData> {
        let struct_id = match self.type_map.get(ident.str()) {
            Some(id) => *id,
            None => return err!("Struct type not found: '{}'", ident.str()),
        };
        let stored_type = self.type_vec.get(struct_id).unwrap();
        match stored_type {
            StoredType::Struct { members, width, ident } => {
                if members.len() != fields.len() {
                    return err!(
                        "Struct literal field count mismatch, expected {} fields, found {}",
                        members.len(),
                        fields.len()
                    );
                }

                // Check that all the field names are correct.
                for (i, (addr_type,struct_field)) in members.iter().zip(fields.iter()).enumerate() {
                    if struct_field.ident.str() != addr_type.ident {
                        return err!(
                            "Struct literal field name mismatch at index {i}, expected '{}', found '{}'",
                            struct_field.ident.str(),
                            ident,
                        );
                    }
                    
                    // check that the expression given to each field is of the correct type.
                    let field_type = self.type_vec.get(addr_type.addr_type.type_id).unwrap();
                    let field_sem = self.create_expr_semantics(addr_type.addr_type)?;
                    let field_expr = self.check_expr(&struct_field.expr)?;
                    self.check_type_equivalence(&field_sem, &field_expr)?;
                }
               
                Ok(ExprData::Struct { members: members.as_slice(), width: *width })
            }
            _ => err!("Expected a struct type, found '{stored_type:?}'"),
        }
    }

    fn create_func_signature(&self, ident: &str, args_semantics: &[AddressedType]) -> String {
        match ident {
            "main" => "main".to_owned(), // NOTE(TOM): main is a special case, no overloading
            name @ _ => {
                let mut str = String::new();
                str += name;
                str += "(";
                for (i, arg) in args_semantics.iter().enumerate() {
                    let stored_type = self.type_vec.get(arg.type_id).unwrap();
                    let var_ident = match stored_type {
                        StoredType::Primitive { ident, .. } => ident.as_str(),
                        StoredType::Struct { ident, .. } => ident.as_str(),
                        StoredType::Alias { ident, .. } => ident.as_str(),
                    };
                    match arg.addr_mode {
                        AddressingMode::Primitive => {
                        }
                        AddressingMode::Pointer { depth } => {
                            for _ in 0..depth {
                                str += "^";
                            }
                        }
                        AddressingMode::Array { depth, len } => {
                            for _ in 0..depth {
                                str += "[]";
                            }
                        }
                    }
                    str += var_ident;
                    str += ",";
                }
                if !args_semantics.is_empty() {
                    str.pop(); // removes extra ','
                }
                str + ")"
            }
        }
    }

    fn create_expr_semantics(&self, var_type: AddressedType) -> Result<ExprSem> {
        match self.type_vec.get(var_type.type_id) {
            Some(StoredType::Primitive { ident, mode, width: stored_width }) => {
                let width = match var_type.addr_mode {
                    AddressingMode::Pointer { .. } => PTR,
                    _ => *stored_width,
                };
                Ok(ExprSem {
                    form: ExprForm::Compound,
                    type_mode: *mode,
                    addr_mode: var_type.addr_mode,
                    expr_data: ExprData::Primitive { width },
                })
            },
            Some(StoredType::Struct { ident, width: stored_width, members }) => {
                let width = match var_type.addr_mode {
                    AddressingMode::Pointer { .. } => PTR,
                    _ => *stored_width,
                };
                Ok(ExprSem {
                    form: ExprForm::Compound,
                    type_mode: TypeMode::Struct,
                    addr_mode: var_type.addr_mode,
                    expr_data: ExprData::Struct {
                        members: members.as_slice(),
                        width,
                    },
                })
            }
            Some(StoredType::Alias { aliased_id, .. }) => {
                let base = self.type_vec.get(*aliased_id).unwrap();
                self.create_expr_semantics(AddressedType {
                    type_id: *aliased_id,
                    addr_mode: var_type.addr_mode,
                })
            }
            None => {
                err!("Type not found for AddressedType: {var_type:?}")
            }
        }
    }

    fn is_ident_unique(&self, ident: &str) -> bool {
        !self.stack_var_map.contains_key(ident) && !self.type_map.contains_key(ident) && !self.fn_map.contains_key(ident)
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

    fn get_type_mode(&self, var_type: &StoredType) -> TypeMode {
        match var_type {
            StoredType::Primitive { mode, .. } => *mode,
            StoredType::Struct { .. } => TypeMode::Struct,
            StoredType::Alias { aliased_id, .. } => {
                let base = self.type_vec.get(*aliased_id).unwrap();
                self.get_type_mode(&base)
            }
        }
    }

    fn get_width(&self, var_type: &StoredType) -> usize {
        match var_type {
            StoredType::Primitive{width, ..} => *width,
            StoredType::Struct { width, .. } => *width,
            StoredType::Alias { ident, aliased_id } => {
                let base = self.type_vec.get(*aliased_id).unwrap();
                self.get_width(base)
            }
        }
    }

    fn get_expr_width(&self, expr_sem: &ExprSem) -> usize {
        match expr_sem.expr_data {
            ExprData::Primitive { width } => width,
            ExprData::Struct { width, .. } => width,
            ExprData::Pointer { root_type } => PTR,
        }
    }
    // endregion
}

fn new_primitive(ident: &str, width: usize, mode: TypeMode) -> StoredType {
    StoredType::Primitive {
        ident: ident.to_string(),
        mode,
        width,
    }
}
