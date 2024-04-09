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

        file.write_all(b"global _start\n_start:\n")
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
                    "{}    mov rax, 60\n{}    syscall\n",
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
                        stk_pos: self.stk_ptr + 1, // call gen_expr after so stk_ptr not up to date
                    },
                );

                return Ok(self.gen_expr(stmt.expr.unwrap()).unwrap());
            }
            _ => return Err("Invalid statement."),
        };
    }

    fn gen_expr(&mut self, expr: NodeExpr) -> Result<String, &'static str> {
        match expr.kind {
            ExprKind::IntLit => {
                return Ok(format!(
                    "    mov rax, {}\n{}",
                    expr.token.value.as_ref().unwrap(),
                    self.push("rax")
                ))
            }
            ExprKind::Ident => {
                let ident = &expr.token.value.as_ref().unwrap().clone();
                if !self.vars.contains_key(ident) {
                    return Err("Identifier doesn't exist.");
                }

                let word_size = 8;
                let stk_index = &self
                    .vars
                    .get(&expr.token.value.clone().unwrap())
                    .unwrap()
                    .stk_pos;
                let stk_offset = (self.stk_ptr - stk_index) * word_size;

                return Ok(self.push(format!("QWORD [rsp + {}]\n", stk_offset).as_str()));
            }
            _ => return Err("Invalid expression."),
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
