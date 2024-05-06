use crate::{lexer::TokenKind, parser::*};
use std::{collections::HashMap, io::Write};

const WORD_SIZE: usize = 8;
const SPACE: &'static str = "    ";

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
    stack: Vec<Variable>,
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
            stack: Vec::new(),
        };
        return generator;
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
        return match stmt {
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
                    stk_pos: self.stk_ptr,
                    ident: None,
                    mutable,
                });

                self.gen_stmt(*assignment)
            }
            NodeStmt::Assign(ident, expr) => {
                return match self.vars_map.get(ident.value.as_ref().unwrap()) {
                    Some(var) if var.mutable => {
                        let reg = format!("QWORD [rsp+{}]", var.stk_pos * WORD_SIZE,);
                        self.gen_expr(expr, reg.as_str())
                    }
                    None => match self.stack.last_mut() {
                        Some(new_var) if new_var.ident.is_none() => {
                            new_var.ident = ident.value.clone();
                            self.vars_map.insert(ident.value.unwrap(), new_var.clone());

                            let reg = format!("QWORD [rsp+{}]", new_var.stk_pos * WORD_SIZE,);
                            self.gen_expr(expr, reg.as_str())
                        }
                        _ => Err(format!(
                            "[COMPILER_GEN] Variable '{}' doesn't exit, cannot assign",
                            ident.value.unwrap()
                        )),
                    },
                    _ => Err(format!(
                        "[COMPILER_GEN] Re-assignment of constant '{:?}'",
                        ident.value.unwrap()
                    )),
                };
            }
            _ => todo!("if"), /*
                                            NodeStmt::If(expr, scope, branches) => {
                                                // Asm Breakdown:
                                                // generate expr for boolean comp lhs & rhs
                                                // pop into rax, rcx
                                                // cmp rax, rcx
                                                // (jump instruction) (label)
                                                // (label):
                                                // generate scope asm

                                                // To note:
                                                // .. Format: if (expr) scope
                                                // .. operand changes jump instruction, e.g je (jump if equal)
                                                // .. .. do the inverse of the condition:
                                                // .. .. .. if expr is false (0): jump to else[if] // end of if statement scope.
                                                todo!("change if");

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
                              */
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

    fn gen_expr(&mut self, expr: NodeExpr, reg: &str) -> Result<String, String> {
        return match expr {
            NodeExpr::Term(term) => return self.gen_term(*term, reg),
            NodeExpr::BinaryExpr { op, lhs, rhs } => {
                let comment = op.clone();
                let reg1 = "rax";
                let reg2 = "rcx";

                let lhs_asm = self.gen_expr(*lhs, reg1)?;
                let rhs_asm = self.gen_expr(*rhs, reg2)?;
                let op_asm = match op {
                    _ if op.is_comparison() => self.gen_cmp(op, reg1, reg2)?,
                    _ if op.is_bitwise() => self.gen_bitwise(op, reg1, reg2)?,
                    _ if op.is_logical() => self.gen_logical(op, reg1, reg2, &lhs_asm, &rhs_asm)?,
                    _ if op.is_arithmetic() => self.gen_arithmetic(op, reg1, reg2)?,
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
                     {SPACE}mov {reg}, rax\n"
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
                     {SPACE}mov {reg}, rax\n"
                ))
            }
        };
    }

    fn gen_term(&mut self, term: NodeTerm, reg: &str) -> Result<String, String> {
        return match term {
            NodeTerm::IntLit(token) => {
                let int_lit = token.value.clone().unwrap();
                // reg might be e.g QWORD[rsp+____], but can't know variable ident in context
                Ok(format!("{SPACE}mov {reg}, {int_lit}\n"))
            }
            NodeTerm::Ident(token) => {
                let ident = &token.value.clone().unwrap();
                if !self.vars_map.contains_key(ident) {
                    return Err(format!("[COMPILER_GEN] Variable: {ident:?} doesn't exist."));
                }

                let stk_index = &self.vars_map.get(ident).unwrap().stk_pos * WORD_SIZE;
                Ok(format!(
                    "{SPACE}mov {reg}, QWORD [rsp+{stk_index}] ; {token:?}\n"
                ))
            }
            NodeTerm::Paren(expr) => self.gen_expr(expr, reg),
        };
    }

    fn gen_logical(
        &mut self,
        op: TokenKind,
        reg1: &str,
        reg2: &str,
        lhs_asm: &str,
        rhs_asm: &str,
    ) -> Result<String, String> {
        return match op {
            TokenKind::LogicalAnd => {
                let false_label = self.create_label("AND_FALSE");
                let true_label = self.create_label("AND_TRUE");

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
                     {true_label}:\n"
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

                let false_label = self.create_label("OR_FALSE");
                let true_label = self.create_label("OR_TRUE");
                let final_label = self.create_label("OR_FINAL");

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
                     {SPACE}movzx {reg1}, al\n"
                ))
            }
            _ => Err(format!(
                "[COMPILER_GEN] Unable to generate Logical comparison"
            )),
        };
    }

    fn gen_cmp(&mut self, op: TokenKind, reg1: &str, reg2: &str) -> Result<String, String> {
        let set_asm = format!("set{}", self.gen_cmp_asm(op)?);

        Ok(format!(
            "{SPACE}cmp {reg1}, {reg2}\n\
             {SPACE}{set_asm} al\n\
             {SPACE}movzx {reg1}, al\n"
        ))
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
                ));
            }
        };

        Ok(format!("{SPACE}{asm} {reg1}, {reg2}\n"))
    }

    fn gen_cmp_asm(&self, op: TokenKind) -> Result<&str, String> {
        return match op {
            TokenKind::Equal => Ok("e"),
            TokenKind::NotEqual => Ok("ne"),
            TokenKind::GreaterThan => Ok("g"),
            TokenKind::GreaterEqual => Ok("ge"),
            TokenKind::LessThan => Ok("l"),
            TokenKind::LessEqual => Ok("le"),
            _ => {
                return Err(format!(
                    "[COMPILER_GEN] Unable to generate comparison modifier"
                ))
            }
        };
    }

    fn create_label(&mut self, name: &'static str) -> String {
        self.label_count += 1;
        return format!("label{}_{name}", self.label_count);
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
