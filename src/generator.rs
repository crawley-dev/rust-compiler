use crate::{lexer::TokenKind, parser::*};
use std::{collections::HashMap, io::Write};

const WORD_SIZE: usize = 8;
const SPACE: &'static str = "    ";

#[derive(Debug, Clone, PartialEq)]
struct Variable {
    stk_index: usize,
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
    stack: Vec<Variable>,
}

impl Generator {
    pub fn new(prog: NodeProg, file_path: String) -> Generator {
        Generator {
            prog,
            file_path,
            stk_ptr: 0,
            label_count: 0,
            scopes: vec![],
            vars_map: HashMap::new(),
            stack: Vec::new(),
        }
    }

    pub fn generate_prog(&mut self) -> Result<(), String> {
        let mut file = std::fs::File::create(&self.file_path).expect("Invalid filepath given.");
        let expect_msg = "unable to write to file.";

        file.write_all(
            b"global _start\n\
                  _start:\n\
                 ; setup stack frame\n    \
                 push rbp\n    \
                 mov rbp, rsp\n",
        )
        .expect(expect_msg);
        file.write_all(b"; Program Start\n").expect(expect_msg);

        while !self.prog.stmts.is_empty() {
            let stmt = self.prog.stmts.remove(0);
            let stmt_asm = self.gen_stmt(stmt)?;

            file.write_all(stmt_asm.as_bytes()).expect(expect_msg);
        }

        return Ok(());
    }

    // TODO: BYTE ARRAYS!
    // TODO: subtract
    fn gen_stmt(&mut self, stmt: NodeStmt) -> Result<String, String> {
        match stmt {
            NodeStmt::Scope(scope) => self.gen_scope(scope),
            NodeStmt::Exit(expr) => {
                let expr_asm = self.gen_expr(expr, "rdi")?;
                Ok(format!(
                    "; Exit Program\n\
                     {expr_asm}\
                     {SPACE}mov rax, 60\n\
                     {SPACE}syscall\n"
                ))
            }

            NodeStmt::Let(assignment, mutable) => {
                if self.stack.len() != 0 {
                    self.stk_ptr += 1;
                }
                self.stack.push(Variable {
                    stk_index: self.stk_ptr,
                    ident: None,
                    mutable,
                });

                self.gen_stmt(*assignment)
            }
            NodeStmt::Assign(ident, expr) => {
                // TODO: what types of expr can a let statement handle ??
                match self.vars_map.get(ident.value.as_ref().unwrap()) {
                    Some(var) if var.mutable => {
                        let reg = self.gen_stk_pos(var.stk_index);
                        self.gen_expr(expr, reg.as_str())
                    }
                    Some(_) => Err(format!(
                        "[COMPILER_GEN] Re-assignment of constant '{:?}'",
                        ident.value.unwrap()
                    )),
                    None => match self.stack.last_mut() {
                        Some(new_var) if new_var.ident.is_none() => {
                            new_var.ident = ident.value.clone();
                            self.vars_map.insert(ident.value.unwrap(), new_var.clone());

                            // let reg = self.gen_stk_pos(new_var.stk_index);
                            // TODO: refactor.
                            let reg = format!("QWORD [rsp+{}]", new_var.stk_index * WORD_SIZE);
                            self.gen_expr(expr, reg.as_str())
                        }
                        _ => Err(format!(
                            "[COMPILER_GEN] Variable '{}' doesn't exit, cannot assign",
                            ident.value.unwrap()
                        )),
                    },
                }
            }
            NodeStmt::If(expr, scope, branches) => {
                // To note:
                // .. Format: if (expr) scope
                // .. operand changes jump instruction, e.g je (jump if equal)
                // .. .. do the inverse of the condition:
                // .. .. .. if expr is false (0): jump to else[if] // end of if statement scope.

                let expr_asm = self.gen_expr(expr, "rax")?;
                let skip_label = self.gen_label("IF_FALSE");
                let scope_asm = self.gen_scope(scope)?;
                let mut branches_asm = String::new();
                for branch in branches {
                    branches_asm += &self.gen_stmt(branch)?;
                }

                Ok(format!(
                    "; If\n\
                     {expr_asm}\
                     {SPACE}cmp rax, 0 \n\
                     {SPACE}je {skip_label}\
                     {scope_asm}\
                     {skip_label}:\n\
                     {branches_asm}"
                ))
            }
            NodeStmt::ElseIf(expr, scope) => {
                let skip_label = self.gen_label("ELIF_FALSE");
                let scope_asm = self.gen_scope(scope)?;
                let expr_asm = self.gen_expr(expr, "rax")?;

                Ok(format!(
                    "{expr_asm}\n\
                     {SPACE}cmp rax, 0\n\
                     {SPACE}je {skip_label}\
                     {scope_asm}\
                     {skip_label}:\n"
                ))
            }
            NodeStmt::Else(scope) => {
                let scope_asm = self.gen_scope(scope)?;
                Ok(format!(
                    "; Else\n\
                     {scope_asm}"
                ))
            }
        }
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
                if let Some(var) = self.stack.pop() {
                    self.vars_map.remove(&var.ident.unwrap());
                } else {
                    break;
                }
            }
        }

        self.scopes.pop(); // can remove 'scopes', push & pop all in here.
        Ok(asm)
    }

    // TODO: logical or/and bug, causes duplicate asm.
    fn gen_expr(&mut self, expr: NodeExpr, reg: &str) -> Result<String, String> {
        // TODO: remove if using int_lit, put directly in operation.
        let mov_ans = if reg != "rax" {
            format!("{SPACE}mov {reg}, rax; \n")
        } else {
            format!("")
        };
        match expr {
            NodeExpr::Term(term) => self.gen_term(*term, reg),
            NodeExpr::BinaryExpr { op, lhs, rhs } => {
                let comment = op.clone();
                let reg1 = "rax";
                let reg2 = "rcx";

                let lhs_asm = self.gen_expr(*lhs, reg1)?;
                let rhs_asm = self.gen_expr(*rhs, reg2)?;
                let op_asm = match op {
                    _ if op.is_bitwise() => self.gen_bitwise(op, reg1, reg2)?,
                    _ if op.is_comparison() => self.gen_cmp(op, reg1, reg2)?,
                    _ if op.is_arithmetic() => self.gen_arithmetic(op, reg1, reg2)?,
                    _ if op.is_logical() => {
                        return self.gen_logical(op, mov_ans, reg1, reg2, &lhs_asm, &rhs_asm)
                    }
                    _ => {
                        return Err(format!(
                            "[COMPILER_GEN] Unable to generate binary expression"
                        ))
                    }
                };

                Ok(format!(
                    "; {comment:?}\n\
                     {lhs_asm}\
                     {rhs_asm}\
                     {op_asm}\
                     {mov_ans}"
                ))
            }
            NodeExpr::UnaryExpr { op, operand } => {
                let comment = op.clone();
                let op_asm = match op {
                    TokenKind::BitwiseNot => format!("{SPACE}not rax\n"),
                    _ => todo!("logical not."), // need to invert cmp.. do later.
                };

                let expr_asm = self.gen_expr(*operand, "rax")?;

                Ok(format!(
                    "; {comment:?}\n\
                     {expr_asm}\
                     {op_asm}\
                     {mov_ans}"
                ))
            }
        }
    }

    fn gen_term(&mut self, term: NodeTerm, reg: &str) -> Result<String, String> {
        match term {
            NodeTerm::IntLit(token) => {
                // reg might be e.g QWORD[rsp+____], but can't know variable ident in context
                let int_lit = token.value.unwrap();
                Ok(format!("{SPACE}mov {reg}, {int_lit}\n"))
            }
            NodeTerm::Ident(token) => {
                let ident = token.value.clone().unwrap();
                match self.vars_map.get(ident.as_str()) {
                    Some(var) => {
                        let stk_pos = self.gen_stk_pos(var.stk_index);
                        Ok(format!("{SPACE}mov {reg}, {stk_pos}; {token:?}\n"))
                    }
                    None => Err(format!("[COMPILER_GEN] Variable: {ident:?} doesn't exist.")),
                }
            }
            NodeTerm::Paren(expr) => self.gen_expr(expr, reg),
        }
    }

    // TODO: branchless comparisons (MENTAL IDEA !!), Remove excess 'cmp'
    // "movzx {reg1},al" << zero inits reg && validates val is 1 or 0.
    fn gen_logical(
        &mut self,
        op: TokenKind,
        mov_ans: String,
        reg1: &str,
        reg2: &str,
        lhs_asm: &str,
        rhs_asm: &str,
    ) -> Result<String, String> {
        match op {
            TokenKind::LogicalAnd => {
                let false_label = self.gen_label("AND_FALSE");
                let true_label = self.gen_label("AND_TRUE");

                Ok(format!(
                    "; LogicalAnd\n\
                     {lhs_asm}\
                     {SPACE}cmp {reg1}, 0\n\
                     {SPACE}je {false_label}\n\
                     {rhs_asm}\
                     {SPACE}cmp {reg2}, 0\n\
                     {SPACE}je {true_label}\n\
                     {SPACE}mov {reg1}, 1\n\
                     {SPACE}jmp {true_label}\n\
                     {false_label}:\n\
                     {SPACE}mov {reg1}, 0\n\
                     {true_label}:\n\
                     {SPACE}movzx {reg1}, al\n\
                     {mov_ans}"
                ))
            }
            TokenKind::LogicalOr => {
                let false_label = self.gen_label("OR_FALSE");
                let true_label = self.gen_label("OR_TRUE");
                let final_label = self.gen_label("OR_FINAL");

                Ok(format!(
                    "; LogicalOr\n\
                     {lhs_asm}\
                     {SPACE}cmp {reg1}, 0\n\
                     {SPACE}jne {true_label}\n\
                     {rhs_asm}\
                     {SPACE}cmp {reg2}, 0\n\
                     {SPACE}je {false_label}\n\
                     {true_label}:\n\
                     {SPACE}mov {reg1}, 1\n\
                     {SPACE}jmp {final_label}\n\
                     {false_label}:\n\
                     {SPACE}mov {reg1}, 0\n\
                     {final_label}:\n\
                     {SPACE}movzx {reg1}, al\n\
                     {mov_ans}"
                ))
            }
            _ => Err(format!(
                "[COMPILER_GEN] Unable to generate Logical comparison"
            )),
        }
    }

    fn gen_arithmetic(&mut self, op: TokenKind, reg1: &str, reg2: &str) -> Result<String, String> {
        let operation_asm = match op {
            TokenKind::Divide => format!("xor rdx,rdx\n{SPACE}idiv {reg2}"), // TODO: this qword business..
            TokenKind::Multiply => format!("imul {reg1}, {reg2}"),
            TokenKind::Subtract => format!("sub {reg1}, {reg2}"),
            TokenKind::Add => format!("add {reg1}, {reg2}"),
            TokenKind::Remainder => todo!("modulus operator.."), // div + mov rax, rdx
            _ => {
                return Err(format!(
                    "[COMPILER_GEN] Unable to generate Arithmetic operation: '{op:?}'"
                ))
            }
        };

        Ok(format!("{SPACE}{operation_asm}\n"))
    }

    fn gen_bitwise(&self, op: TokenKind, reg1: &str, reg2: &str) -> Result<String, String> {
        let asm = match op {
            TokenKind::BitwiseOr => "or",
            TokenKind::BitwiseXor => "xor",
            TokenKind::BitwiseAnd => "and",
            TokenKind::LeftShift => "sal",
            TokenKind::RightShift => "sar",
            // TODO: (Types) Unsigned shift: shl, shr
            _ => {
                return Err(format!(
                    "[COMPILER_GEN] Unable to generate Bitwise operation"
                ))
            }
        };

        Ok(format!("{SPACE}{asm} {reg1}, {reg2}\n"))
    }

    fn gen_cmp(&mut self, op: TokenKind, reg1: &str, reg2: &str) -> Result<String, String> {
        let set_asm = format!("set{}", self.gen_cmp_modifier(op)?);

        Ok(format!(
            "{SPACE}cmp {reg1}, {reg2}\n\
             {SPACE}{set_asm} al\n\
             {SPACE}movzx {reg1}, al\n"
        ))
    }

    fn gen_cmp_modifier(&self, op: TokenKind) -> Result<&str, String> {
        match op {
            TokenKind::Equal => Ok("e"),
            TokenKind::NotEqual => Ok("ne"),
            TokenKind::GreaterThan => Ok("g"),
            TokenKind::GreaterEqual => Ok("ge"),
            TokenKind::LessThan => Ok("l"),
            TokenKind::LessEqual => Ok("le"),
            _ => Err(format!(
                "[COMPILER_GEN] Unable to generate comparison modifier"
            )),
        }
    }

    fn gen_label(&mut self, name: &'static str) -> String {
        self.label_count += 1;
        format!("label{}_{name}", self.label_count)
    }

    // TODO: this will change depending on the size of var, e.g u8 or u32
    fn gen_stk_pos(&self, stk_index: usize) -> String {
        format!("QWORD [rsp+{}]", stk_index * WORD_SIZE)
    }
}

// fn push(&mut self, reg: &str) -> String {
//     self.stk_ptr += 1;
//     return format!("{SPACE}push {}", reg);
// }

// fn pop(&mut self, reg: &str) -> String {
//     self.stk_ptr -= 1;
//     return format!("{SPACE}pop {}", reg);
// }
