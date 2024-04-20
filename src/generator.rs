use crate::{lexer::TokenKind, parser::*};
use std::{collections::HashMap, io::Write};

const WORD_SIZE: usize = 8;

#[derive(Debug, Clone, PartialEq)]
pub struct Variable {
    stk_pos: usize,
    ident: String,
}

pub struct Generator {
    prog: NodeProg,
    file_path: String,
    stk_ptr: usize,
    label_count: usize,
    scopes: Vec<usize>, // could try Vec<Vec<Variable>>
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

    pub fn generate_prog(&mut self) -> Result<(), &'static str> {
        let mut file = std::fs::File::create(&self.file_path).expect("Invalid filepath given.");

        file.write_all(
            b"global _start\n\
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
            NodeStmt::Exit(expr) => {
                return Ok(format!(
                    "{expr_asm}    \
                     mov rax, 60\n\
                     {pop}    \
                     syscall\n",
                    expr_asm = self.gen_expr(expr)?,
                    pop = self.pop("rdi")
                ))
            }
            NodeStmt::Let(ident, expr) => {
                if self.vars_map.contains_key(ident.value.as_ref().unwrap()) {
                    return Err("[COMPILER] Identifier already used.");
                }

                let var = Variable {
                    stk_pos: self.stk_ptr + 1,
                    ident: ident.value.unwrap(),
                };

                self.vars_map.insert(var.ident.clone(), var.clone()); // yummy clones
                self.vars_vec.push(var);

                return Ok(self.gen_expr(expr)?);
            }
            NodeStmt::Scope(scope) => self.gen_scope(scope),
            NodeStmt::If(expr, scope, branches) => {
                return Ok(format!(
                    "{expr_asm}\
                     {pop_rax}    \
                     test rax, rax\n    \
                     jz {jmp_label}\n\
                     {scope_asm}\
                     {jmp_label}:\n",
                    expr_asm = self.gen_expr(expr)?,
                    pop_rax = self.pop("rax"),
                    jmp_label = self.create_label(),
                    scope_asm = self.gen_scope(scope)?
                ));
                // todo!("implement else (if) branch gen")
            }
            NodeStmt::ElseIf(_, _) => todo!("elif"),
            NodeStmt::Else(_) => todo!("else"),
        }
    }

    fn gen_scope(&mut self, scope: NodeScope) -> Result<String, &'static str> {
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
                    self.vars_map.remove(&var.ident);
                } else {
                    break;
                }
            }
        }

        self.scopes.pop(); // can remove 'scopes', push & pop all in here.
        Ok(asm)
    }

    fn gen_expr(&mut self, expr: NodeExpr) -> Result<String, &'static str> {
        match expr {
            NodeExpr::Term(term) => return self.gen_term(*term),
            NodeExpr::BinExpr { op, lhs, rhs } => {
                let (lhs_inp, rhs_inp, operation_asm) = match op {
                    TokenKind::Divide => (*lhs, *rhs, "    div rbx\n"),
                    TokenKind::Multiply => (*lhs, *rhs, "    mul rbx\n"),
                    TokenKind::Subtract => (*lhs, *rhs, "    sub rax, rbx\n"),
                    TokenKind::Add => (*lhs, *rhs, "    add rax, rbx\n"),
                    _ => return Err("[COMPILER] Unable to generate binary expression"),
                };
                let lhs = self.gen_expr(rhs_inp)?; // these are flipped, for asm reasons
                let rhs = self.gen_expr(lhs_inp)?;

                return Ok(format!(
                    "{lhs}\
                     {rhs}\
                     {pop_rax}\
                     {pop_rbx}\
                     {operation_asm}\
                     {push_rax}",
                    pop_rax = self.pop("rax"),
                    pop_rbx = self.pop("rbx"),
                    push_rax = self.push("rax"),
                ));
            }
            NodeExpr::BoolExpr { op, lhs, rhs } => {
                todo!("bool comp");
                // Err("[COMPILER] ooops")
            }
        }
    }

    fn gen_term(&mut self, term: NodeTerm) -> Result<String, &'static str> {
        match term {
            NodeTerm::IntLit(token) => {
                return Ok(format!(
                    "    mov rax, {int_lit}\n\
                     {push_rax}",
                    int_lit = token.value.unwrap(),
                    push_rax = self.push("rax")
                ))
            }
            NodeTerm::Ident(token) => {
                let ident = &token.value.unwrap();
                if !self.vars_map.contains_key(ident) {
                    return Err("[COMPILER] Identifier doesn't exist.");
                }

                let stk_index = &self.vars_map.get(ident).unwrap().stk_pos;
                let stk_offset = (self.stk_ptr - stk_index) * WORD_SIZE;

                return Ok(self.push(format!("QWORD [rsp + {}]", stk_offset).as_str()));
            }
            NodeTerm::Paren(expr) => self.gen_expr(expr),
        }
    }

    fn push(&mut self, reg: &str) -> String {
        self.stk_ptr += 1;
        return format!("    push {}\n", reg);
    }

    fn pop(&mut self, reg: &str) -> String {
        self.stk_ptr -= 1;
        return format!("    pop {}\n", reg);
    }

    fn create_label(&self) -> String {
        return format!("label{}", self.label_count);
    }
}
