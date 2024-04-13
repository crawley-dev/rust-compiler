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
        let generator = Generator {
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
        };
    }

    fn gen_expr(&mut self, expr: NodeExpr) -> Result<String, &'static str> {
        match expr.kind {
            ExprKind::Term => return self.gen_term(*expr.term.unwrap()),
            ExprKind::BinExpr => {
                let bin_expr = expr.bin_expr.unwrap();
                let lhs = self.gen_expr(bin_expr.rhs)?; // these are flipped, for asm reasons
                let rhs = self.gen_expr(bin_expr.lhs)?;
                let operation_asm = match bin_expr.kind {
                    BinExprKind::Divide => "    div rbx\n",
                    BinExprKind::Multiply => "    mul rbx\n",
                    BinExprKind::Subtract => "    sub rax, rbx\n",
                    BinExprKind::Add => "    add rax, rbx\n",
                };
                return Ok(format!(
                    "{lhs}\
                     {rhs}\
                     {}\
                     {}\
                     {operation_asm}\
                     {}",
                    self.pop("rax"),
                    self.pop("rbx"),
                    self.push("rax"),
                ));
            }
        }
    }

    fn gen_term(&mut self, term: NodeTerm) -> Result<String, &'static str> {
        match term.kind {
            TermKind::IntLit => {
                return Ok(format!(
                    "    mov rax, {}\n\
                     {}",
                    term.token.unwrap().value.as_ref().unwrap(),
                    self.push("rax")
                ))
            }
            TermKind::Ident => {
                let ident = &term.token.as_ref().unwrap().value.as_ref().unwrap().clone();
                if !self.vars.contains_key(ident) {
                    return Err("Identifier doesn't exist.");
                }

                let word_size = 8;
                let stk_index = &self
                    .vars
                    .get(&term.token.unwrap().value.clone().unwrap())
                    .unwrap()
                    .stk_pos;
                let stk_offset = (self.stk_ptr - stk_index) * word_size;

                return Ok(self.push(format!("QWORD [rsp + {}]\n", stk_offset).as_str()));
            }
            TermKind::Paren => return self.gen_expr(term.expr.unwrap()),
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
