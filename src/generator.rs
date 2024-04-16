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
            prog,
            file_path,
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
        match stmt {
            NodeStmt::Exit(expr) => Ok(format!(
                "{}    mov rax, 60\n\
                 {}    syscall\n",
                self.gen_expr(expr)?,
                self.pop("rdi")
            )),
            NodeStmt::Let(ident, expr) => {
                if self.vars.contains_key(ident.value.as_ref().unwrap()) {
                    return Err("Identifier already used.");
                }

                self.vars.insert(
                    ident.value.unwrap(),
                    Variable {
                        stk_pos: self.stk_ptr + 1,
                    },
                );

                return Ok(self.gen_expr(expr)?);
            }
        }
    }

    fn gen_expr(&mut self, expr: NodeExpr) -> Result<String, &'static str> {
        match expr {
            NodeExpr::Term(term) => return self.gen_term(*term),
            NodeExpr::BinExpr(bin_expr) => {
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
        match term {
            NodeTerm::IntLit(token) => {
                return Ok(format!(
                    "    mov rax, {}\n\
                     {}",
                    token.value.unwrap(),
                    self.push("rax")
                ));
            }
            NodeTerm::Ident(token) => {
                let ident = &token.value.unwrap();
                if !self.vars.contains_key(ident) {
                    return Err("Identifier doesn't exist.");
                }

                let word_size = 8;
                let stk_index = &self.vars.get(ident).unwrap().stk_pos;
                let stk_offset = (self.stk_ptr - stk_index) * word_size;

                return Ok(self.push(format!("QWORD [rsp + {}]\n", stk_offset).as_str()));
            }
            NodeTerm::Paren(expr) => return self.gen_expr(expr),
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
