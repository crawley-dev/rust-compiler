#![allow(dead_code, unused_mut, unused_assignments)]
use crate::parser::*;
use std::{collections::HashMap, io::Write};

pub struct Variable {
    stk_pos: usize,
}

pub struct Generator {
    prog: NodeProg,
    file_path: String,
    stk_ptr: usize,
    vars: HashMap<String, Variable>,
}

impl Generator {
    pub fn new(prog: NodeProg, file_path: String) -> Generator {
        let mut generator = Generator {
            prog: prog,
            file_path: file_path,
            stk_ptr: 0,
            vars: HashMap::new(),
        };
        return generator;
    }

    pub fn generate_prog(&mut self) -> Result<(), &'static str> {
        let mut file = std::fs::File::create(&self.file_path).expect("Invalid filepath given.");

        file.write_all(
            b"BITS 64\n\
                        global _start\n\
                        _start:\n",
        )
        .expect("unable to write to file.");

        while !self.prog.stmts.is_empty() {
            let stmt = self.prog.stmts.remove(0);
            let dwa = self.gen_stmt(stmt)?;

            file.write_all(dwa.as_bytes())
                .expect("unable to write to file.");
        }

        return Ok(());
    }

    // TODO: BYTE ARRAYS!
    fn gen_stmt(&mut self, stmt: NodeStmt) -> Result<String, &'static str> {
        match stmt.kind {
            StmtKind::Exit => {
                return Ok(format!(
                    "{}    mov rax, 60\n\
                     {}    syscall\n",
                    self.gen_expr(stmt.expr.unwrap()).unwrap(),
                    self.pop("rdi")
                ));
            }
            StmtKind::Let => {
                // unoptimal, clones identifier string, could use an id?.. it just works
                let ident = &stmt.ident.as_ref().unwrap().value.clone().unwrap();
                if self.vars.contains_key(ident) {
                    return Err("identifier already used.");
                }

                self.vars.insert(
                    stmt.ident.unwrap().value.unwrap(), // consumes identifier
                    Variable {
                        stk_pos: self.stk_ptr + 1,
                    },
                );

                return Ok(self.gen_expr(stmt.expr.unwrap()).unwrap());
            }
            _ => return Err("Invalid statement."),
        };
    }

    fn gen_expr(&mut self, expr: NodeExpr) -> Result<String, &'static str> {
        match expr.kind {
            ExprKind::Term => return self.gen_term(expr.term.unwrap()),
            ExprKind::BinExpr => {
                let bin_expr = expr.bin_expr.unwrap();
                let lhs = self.gen_expr(bin_expr.lhs)?;
                let rhs = self.gen_expr(bin_expr.rhs)?;
                return match bin_expr.kind {
                    BinExprKind::Add => Ok(format!(
                        "{}\
                     {}\
                     {}\
                     {}    \
                     add rax, rbx\n\
                     {}",
                        lhs,
                        rhs,
                        self.pop("rax"),
                        self.pop("rbx"),
                        self.push("rax"),
                    )),
                    BinExprKind::Multiply => Ok(format!(
                        "{}\
                             {}\
                             {}\
                             {}    \
                             mul rbx\n\
                             {}",
                        lhs,
                        rhs,
                        self.pop("rax"),
                        self.pop("rbx"),
                        self.push("rax"),
                    )),
                    BinExprKind::Divide => todo!(),
                    BinExprKind::Subtract => todo!(),
                };
            }
            _ => return Err("Invalid Expression."),
        }
    }

    fn gen_term(&mut self, term: NodeTerm) -> Result<String, &'static str> {
        match term.kind {
            TermKind::IntLit => {
                return Ok(format!(
                    "    mov rax, {}\n\
                     {}",
                    term.token.value.as_ref().unwrap(),
                    self.push("rax")
                ))
            }
            TermKind::Ident => {
                let ident = &term.token.value.as_ref().unwrap().clone();
                if !self.vars.contains_key(ident) {
                    return Err("Identifier doesn't exist.");
                }

                let word_size = 8;
                let stk_index = &self
                    .vars
                    .get(&term.token.value.clone().unwrap())
                    .unwrap()
                    .stk_pos;
                let stk_offset = (self.stk_ptr - stk_index) * word_size;

                return Ok(self.push(format!("QWORD [rsp + {}]\n", stk_offset).as_str()));
            }
            _ => return Err("Invalid term"),
        };
    }

    fn push(&mut self, reg: &str) -> String {
        self.stk_ptr += 1;
        return format!("    push {}\n", reg);
    }

    fn pop(&mut self, reg: &str) -> String {
        self.stk_ptr -= 1;
        return format!("    pop {}\n", reg);
    }
}
