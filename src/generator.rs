use crate::{lexer::TokenKind, parser::*};
use std::{collections::HashMap, io::Write};

const WORD_SIZE: usize = 8;

#[derive(Debug, Clone, PartialEq)]
struct Variable {
    stk_pos: usize,
    ident: String,
    mutable: bool,
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
                let expr_asm = self.gen_expr(expr)?;
                let pop = self.pop("rdi");

                Ok(format!(
                    "{expr_asm}    \
                     mov rax, 60\n\
                     {pop}    \
                     syscall\n",
                ))
            }
            NodeStmt::Let(ident, assignment, mutable) => {
                if self.vars_map.contains_key(ident.value.as_ref().unwrap()) {
                    return Err(format!("[COMPILER_GEN] Variable already exists."));
                }

                let assign_asm = self.gen_stmt(*assignment)?;

                let var = Variable {
                    stk_pos: self.stk_ptr + 1,
                    ident: ident.value.unwrap(), // << needs ident.
                    mutable,
                };

                self.vars_map.insert(var.ident.clone(), var.clone()); // TODO: fix skill issue.
                self.vars_vec.push(var);

                Ok(assign_asm)
            }
            NodeStmt::Assign(ident, expr) => {
                return match self.vars_map.get(ident.value.as_ref().unwrap()) {
                    Some(var) if !var.mutable => Err(format!(
                        "[COMPILER_GEN] Attempted re-assignment of constant '{}'",
                        ident.value.unwrap()
                    )),
                    _ => Ok(self.gen_expr(expr)?),
                }
            }
            NodeStmt::Scope(scope) => self.gen_scope(scope),
            NodeStmt::If(expr, scope, branches) => {
                // Asm Breakdown:
                // generate expr for boolean comp lhs & rhs
                // pop into rax, rbx
                // cmp rax, rbx
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
                let jmp_asm = "    jle"; // jump if false | TODO: unsigned jump ??
                let label = self.create_label("if_false");

                let scope_asm = self.gen_scope(scope)?;
                let mut branches_asm = String::new();
                for branch in branches {
                    branches_asm += &self.gen_stmt(branch)?;
                }

                Ok(format!(
                    "{expr_asm}\
                     {pop_expr}\
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
                let jmp_asm = "    jle"; // jump if false | TODO: unsigned jump ??

                let label = self.create_label("elif");
                let scope_asm = self.gen_scope(scope)?;

                Ok(format!(
                    "{expr_asm}\
                     {pop_expr}\
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
                    self.vars_map.remove(&var.ident);
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
            NodeExpr::BinExpr { op, lhs, rhs } => {
                let lhs_asm = self.gen_expr(*rhs)?; // these are flipped, for asm reasons
                let rhs_asm = self.gen_expr(*lhs)?;

                let pop_lhs = self.pop("rax");
                let pop_rhs = self.pop("rbx");

                let operation_asm = match op {
                    TokenKind::Divide => "    div rbx\n",
                    TokenKind::Multiply => "    mul rbx\n",
                    TokenKind::Subtract => "    sub rax, rbx\n",
                    TokenKind::Add => "    add rax, rbx\n",
                    _ => {
                        return Err(format!(
                            "[COMPILER_GEN] Unable to generate binary expression"
                        ))
                    }
                };

                let push_ans = self.push("rax");

                Ok(format!(
                    "{lhs_asm}\
                     {rhs_asm}\
                     {pop_lhs}\
                     {pop_rhs}\
                     {operation_asm}\
                     {push_ans}",
                ))
            }
            // boolean expression generates cmp && jump instruction?
            // yeah I guess..
            NodeExpr::BoolExpr { op, lhs, rhs } => {
                let lhs_asm = self.gen_expr(*rhs)?;
                let rhs_asm = self.gen_expr(*lhs)?;

                let pop_lhs = self.pop("rax");
                let pop_rhs = self.pop("rbx");

                let cmp_asm = "    cmp rax, rbx\n";

                let set_asm = match op {
                    TokenKind::Equal => "    sete al\n",
                    TokenKind::GreaterThan => "    setg al\n",
                    TokenKind::GreaterEqual => "    setge al\n",
                    TokenKind::LessThan => "    setl al\n",
                    TokenKind::LessEqual => "    setle al\n",
                    _ => return Err(format!("[COMPILER_GEN] Unable to generate bool comparison")),
                };

                let mov_asm = "    movzx rax, al\n";
                let push_ans = self.push("rax");

                Ok(format!(
                    "{lhs_asm}\
                     {rhs_asm}\
                     {pop_lhs}\
                     {pop_rhs}\
                     {cmp_asm}\
                     {set_asm}\
                     {mov_asm}\
                     {push_ans}"
                ))
            }
        };
    }

    fn gen_term(&mut self, term: NodeTerm) -> Result<String, String> {
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
                    return Err(format!("[COMPILER_GEN] Variable: {ident:?} doesn't exist."));
                }

                let stk_index = &self.vars_map.get(ident).unwrap().stk_pos;
                let stk_offset = (self.stk_ptr - stk_index) * WORD_SIZE;

                return Ok(self.push(format!("QWORD [rsp + {}]", stk_offset).as_str()));
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

    fn create_label(&mut self, name: &'static str) -> String {
        self.label_count += 1;
        return format!("label{}_{name}", self.label_count);
    }
}
