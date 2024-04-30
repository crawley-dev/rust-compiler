use crate::{lexer::TokenKind, parser::*};
use std::{collections::HashMap, io::Write};

const WORD_SIZE: usize = 8;

#[derive(Debug, Clone, PartialEq)]
struct Variable {
    stk_pos: usize,
    ident: Option<String>,
    mutable: bool,
}

pub struct Generator {
    prog: NodeProg,
    file_path: String,
    stk_ptr: usize,
    label_count: usize,
    scopes: Vec<usize>,
    vars_map: HashMap<String, Variable>,
    vars_vec: Vec<Variable>,
}

impl Generator {
    pub fn new(prog: NodeProg, file_path: String) -> Generator {
        let generator = Generator {
            prog,
            file_path,
            stk_ptr: 0,
            label_count: 0,
            scopes: vec![],
            vars_map: HashMap::new(),
            vars_vec: Vec::new(),
        };
        return generator;
    }

    pub fn generate_prog(&mut self) -> Result<(), String> {
        let mut file = std::fs::File::create(&self.file_path).expect("Invalid filepath given.");

        file.write_all(
            b"global _start\n\
                  _start:\n",
        )
        .expect("unable to do init write to file.");

        while !self.prog.stmts.is_empty() {
            let stmt = self.prog.stmts.remove(0);
            let stmt_asm = self.gen_stmt(stmt)?;

            file.write_all(stmt_asm.as_bytes())
                .expect("unable to write to file.");
        }

        return Ok(());
    }

    // TODO: BYTE ARRAYS!
    fn gen_stmt(&mut self, stmt: NodeStmt) -> Result<String, String> {
        return match stmt {
            NodeStmt::Exit(expr) => {
                if self.stk_ptr > 0 {
                    self.stk_ptr -= 1; // because indexing starts at 1, not 0
                }
                let expr_asm = self.gen_expr(expr)?;
                let pop = self.pop("rdi");

                Ok(format!(
                    "; Exit Program\n\
                     {expr_asm}    \
                     mov rax, 60\n\
                     {pop}\n    \
                     syscall\n",
                ))
            }
            NodeStmt::Let(assignment, mutable) => {
                self.vars_vec.push(Variable {
                    stk_pos: self.stk_ptr,
                    ident: None,
                    mutable,
                });

                self.gen_stmt(*assignment)
            }
            NodeStmt::Assign(ident, expr) => {
                if let Some(var) = self.vars_map.get(ident.value.as_ref().unwrap()) {
                    if !var.mutable {
                        return Err(format!(
                            "[COMPILER_GEN] Attempted re-assignment of constant {:?}",
                            ident.value.unwrap()
                        ));
                    }
                    return self.gen_expr(expr);
                }

                // TODO: remove this mess..
                if let Some(var) = self.vars_vec.last_mut() {
                    if var.ident == None {
                        self.vars_map
                            .insert(ident.value.clone().unwrap(), var.clone()); // TODO: fix skill issue
                        var.ident = Some(ident.value.unwrap());

                        return self.gen_expr(expr);
                    }
                };

                return Err(format!(
                    "[COMPILER_GEN] Variable '{}' doesn't exist, cannot assign",
                    ident.value.unwrap()
                ));
            }
            NodeStmt::Scope(scope) => self.gen_scope(scope),
            NodeStmt::If(expr, scope, branches) => {
                // Asm Breakdown:
                // generate expr for boolean comp lhs & rhs
                // pop into rax, rdx
                // cmp rax, rdx
                // (jump instruction) (label)
                // (label):
                // generate scope asm

                // To note:
                // .. Format: if (expr) scope
                // .. operand changes jump instruction, e.g je (jump if equal)
                // .. .. do the inverse of the condition:
                // .. .. .. if expr is false (0): jump to else[if] // end of if statement scope.

                let expr_asm = self.gen_expr(expr)?;
                let pop_expr = self.pop("rax");
                let cmp_asm = "    cmp rax, 0\n";
                let jmp_asm = "    je"; // eval true if non-zero
                let label = self.create_label("if_false");

                let scope_asm = self.gen_scope(scope)?;
                let mut branches_asm = String::new();
                for branch in branches {
                    branches_asm += &self.gen_stmt(branch)?;
                }

                Ok(format!(
                    "{expr_asm}\
                     {pop_expr}\n\
                     {cmp_asm}\
                     {jmp_asm} {label}\n\
                     {scope_asm}\
                     {label}:\n\
                     {branches_asm}"
                ))
            }
            NodeStmt::ElseIf(expr, scope) => {
                let expr_asm = self.gen_expr(expr)?;
                let pop_expr = self.pop("rax");

                let cmp_asm = "    cmp rax, 0\n";
                let jmp_asm = "    je"; // TODO: unsigned jump ??

                let label = self.create_label("elif");
                let scope_asm = self.gen_scope(scope)?;

                Ok(format!(
                    "{expr_asm}\
                     {pop_expr}\n\
                     {cmp_asm}\
                     {jmp_asm} {label}\n\
                     {scope_asm}\
                     {label}:\n"
                ))
            }
            NodeStmt::Else(scope) => {
                let label = self.create_label("else");
                let scope_asm = self.gen_scope(scope)?;
                Ok(format!(
                    "    jmp {label}\n\
                     {label}:\n\
                     {scope_asm}"
                ))
            }
        };
    }

    fn gen_scope(&mut self, scope: NodeScope) -> Result<String, String> {
        self.scopes.push(scope.stmts.len());

        let mut asm = String::new();
        for stmt in scope.stmts {
            asm += &self.gen_stmt(stmt)?;
        }

        if self.vars_map.len() > 0 && !scope.inherits_stmts {
            let pop_amt = self.vars_map.len() - self.scopes.last().unwrap();
            asm += &format!("    add rsp, {}\n", pop_amt * WORD_SIZE);
            self.stk_ptr -= pop_amt;

            for _ in 0..pop_amt {
                if let Some(var) = self.vars_vec.pop() {
                    self.vars_map.remove(&var.ident.unwrap());
                } else {
                    break;
                }
            }
        }

        self.scopes.pop(); // can remove 'scopes', push & pop all in here.
        Ok(asm)
    }

    fn gen_expr(&mut self, expr: NodeExpr) -> Result<String, String> {
        return match expr {
            NodeExpr::Term(term) => return self.gen_term(*term),
            NodeExpr::BinaryExpr { op, lhs, rhs } => {
                let comment = op.clone();

                // // TODO: remove hardcoded "rax,rdx"
                let operation_asm = match op {
                    _ if op.is_logical_op() => return self.gen_logical(op, *lhs, *rhs),
                    _ if op.is_bitwise_op() => self.gen_bitwise(op)?,
                    _ if op.is_cmp_op() => self.gen_cmp(op)?,
                    TokenKind::Divide => "    div rdx\n".to_owned(),
                    TokenKind::Multiply => "    mul rdx\n".to_owned(),
                    TokenKind::Subtract => "    sub rax, rdx\n".to_owned(),
                    TokenKind::Add => "    add rax, rdx\n".to_owned(),
                    _ => {
                        return Err(format!(
                            "[COMPILER_GEN] Unable to generate Binary expression: '{op:?}'"
                        ))
                    }
                };

                let lhs_asm = self.gen_expr(*rhs)?; // flipped because its a stack.
                let rhs_asm = self.gen_expr(*lhs)?;

                let get_lhs = self.pop("rax");
                let get_rhs = self.pop("rdx");
                let push_ans = self.push("rax");

                // TODO: try flipping lhs,rhs expr & pop
                Ok(format!(
                    "; Binary Expr: {comment:?}\n\
                        {lhs_asm}\
                        {rhs_asm}\
                        {get_lhs}\n\
                        {get_rhs}\n\
                        {operation_asm}\
                        {push_ans}\n",
                ))
            }
            NodeExpr::UnaryExpr { op, operand } => {
                let op_asm = match op {
                    TokenKind::BitwiseNot => self.gen_bitwise(op)?,
                    _ => todo!("logical not."), // need to invert cmp.. to later.
                };

                let comment = *operand.clone();

                let expr_asm = self.gen_expr(*operand)?;
                let pop_expr = self.pop("rax");
                let push_ans = self.push("rax");

                Ok(format!(
                    "; Unary Expr: {comment:?}\
                     {expr_asm}\
                     {pop_expr}\n\
                     {op_asm}\
                     {push_ans}\n",
                ))
            }
        };
    }

    fn gen_term(&mut self, term: NodeTerm) -> Result<String, String> {
        return match term {
            NodeTerm::IntLit(token) => {
                let int_lit = token.value.clone().unwrap();
                let push_int = self.push("rax");

                Ok(format!(
                    "    mov rax, {int_lit}\n\
                     {push_int} ; {token:?}\n"
                ))
            }
            NodeTerm::Ident(token) => {
                let ident = &token.value.clone().unwrap();
                if !self.vars_map.contains_key(ident) {
                    return Err(format!("[COMPILER_GEN] Variable: {ident:?} doesn't exist."));
                }

                let stk_index = &self.vars_map.get(ident).unwrap().stk_pos;
                let stk_offset = (self.stk_ptr - stk_index) * WORD_SIZE;
                let push_copy = self.push(format!("QWORD [rsp + {stk_offset}]").as_str());

                Ok(format!("{push_copy} ; {token:?}\n"))
            }
            NodeTerm::Paren(expr) => self.gen_expr(expr),
        };
    }

    fn gen_logical(
        &mut self,
        op: TokenKind,
        lhs: NodeExpr,
        rhs: NodeExpr,
    ) -> Result<String, String> {
        return match op {
            TokenKind::LogicalAnd => {
                // cmp lhs
                // jmp on false to __LABEL1
                // cmp rhs
                // jmp on false to __LABEL2
                // mov rax, 1
                // jmp __LABEL2

                // __LABEL1:
                // mov rax, 0

                // __LABLEL2:
                // mov rax, al ; when was 'al' set?? magic.
                // push rax; put var onto stack

                let lhs_asm = self.gen_expr(lhs)?;
                let rhs_asm = self.gen_expr(rhs)?;
                println!("{lhs_asm}\n{rhs_asm}");

                let get_lhs = self.pop("rax");
                let lhs_cmp = "    cmp rax, 0\n";
                let lhs_jmp = "    je";
                let false_label = self.create_label("AND1");

                let get_rhs = self.pop("rax");
                let rhs_cmp = "    cmp rax, 0\n";
                let rhs_jmp = "    je";
                let true_label = self.create_label("AND2");

                let push_ans = self.push("rax");

                Ok(format!(
                    "{lhs_asm}\
                     {get_lhs}\n\
                     {lhs_cmp}\
                     {lhs_jmp} {false_label}\n\
                     {rhs_asm}\
                     {get_rhs}\n\
                     {rhs_cmp}\
                     {rhs_jmp} {false_label}\n    \
                     mov rax, 1\n    \
                     jmp {true_label}\n\
                     {false_label}:\n    \
                     mov rax, 0\n\
                     {true_label}:\n\
                     {push_ans}\n"
                ))
            }
            TokenKind::LogicalOr => {
                // cmp lhs
                // jmp on true to __LABEL1
                // cmp rhs
                // jmp on false to __LABEL2

                // __LABEL1:
                // mov rax, 1
                // jmp __LABEL3

                // __LABEL2:
                // mov rax, 0

                // __LABEL3:
                // mov rax, al ; wizardry is afoot.
                // push rax; put var onto stack
                Ok(format!("; 'LogicalOr' comparison HERE <--\n"))
            }
            _ => Err(format!(
                "[COMPILER_GEN] Unable to generate logical operation"
            )),
        };
    }

    fn gen_cmp(&mut self, op: TokenKind) -> Result<String, String> {
        let set_asm = format!(
            "    {} al\n",
            match op {
                TokenKind::Equal => "sete",
                TokenKind::NotEqual => "setne",
                TokenKind::GreaterThan => "setg",
                TokenKind::GreaterEqual => "setge",
                TokenKind::LessThan => "setl",
                TokenKind::LessEqual => "setle",
                _ => return Err(format!("[COMPILER_GEN] Unable to generate comparison")),
            }
        );

        let cmp_asm = "    cmp rax, rdx\n";
        let mov_asm = "    movzx rax, al\n";

        Ok(format!(
            "{cmp_asm}\
             {set_asm}\
             {mov_asm}"
        ))
    }

    fn gen_jmp(&mut self, op: TokenKind) -> Result<&str, String> {
        return match op {
            TokenKind::Equal => Ok("je"),
            TokenKind::NotEqual => Ok("jne"),
            TokenKind::GreaterThan => Ok("jg"),
            TokenKind::GreaterEqual => Ok("jge"),
            TokenKind::LessThan => Ok("jl"),
            TokenKind::LessEqual => Ok("jle"),
            _ => return Err(format!("[COMPILER_GEN] Unable to generate comparison")),
        };
    }

    fn gen_bitwise(&self, op: TokenKind) -> Result<String, String> {
        let asm = match op {
            TokenKind::LeftShift => "sal",
            TokenKind::RightShift => "sar",
            TokenKind::BitwiseOr => "or",
            TokenKind::BitwiseXor => "xor",
            TokenKind::BitwiseAnd => "and",
            TokenKind::BitwiseNot => "not",
            _ => {
                return Err(format!(
                    "[COMPILER_GEN] Unable to generate bitwise operation"
                ));
            }
        };

        Ok(format!("    {asm} rax, rdx\n"))
    }

    fn push(&mut self, reg: &str) -> String {
        self.stk_ptr += 1;
        // println!("+1, stk: {}", self.stk_ptr);
        return format!("    push {}", reg);
    }

    fn pop(&mut self, reg: &str) -> String {
        self.stk_ptr -= 1;
        // println!("-1, stk: {}", self.stk_ptr);
        return format!("    pop {}", reg);
    }

    fn create_label(&mut self, name: &'static str) -> String {
        self.label_count += 1;
        return format!("label{}_{name}", self.label_count);
    }
}
