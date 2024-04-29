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
            NodeStmt::Let(assignment, mutable) => {
                self.vars_vec.push(Variable {
                    stk_pos: self.stk_ptr,
                    ident: None,
                    mutable,
                });

                Ok(self.gen_stmt(*assignment)?)
            }
            NodeStmt::Assign(ident, expr) => {
                if let Some(var) = self.vars_map.get(ident.value.as_ref().unwrap()) {
                    if !var.mutable {
                        return Err(format!(
                            "[COMPILER_GEN] Attempted re-assignment of constant {:?}",
                            ident.value.unwrap()
                        ));
                    }
                    return Ok(self.gen_expr(expr)?);
                }

                if let Some(var) = self.vars_vec.last_mut() {
                    // TODO: ident: Option<String> is not nice.
                    if var.ident == None {
                        // println!(
                        //     "init var: '{:?}' => '{}'",
                        //     var.ident,
                        //     ident.value.as_ref().unwrap()
                        // );
                        self.vars_map
                            .insert(ident.value.clone().unwrap(), var.clone()); // TODO: fix skill issue
                        var.ident = Some(ident.value.unwrap());

                        return Ok(self.gen_expr(expr)?);
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
                let jmp_asm = "    je"; // eval true if non-zero
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
                let jmp_asm = "    je"; // TODO: unsigned jump ??

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
                let lhs_asm = self.gen_expr(*rhs)?; // these are flipped, for asm reasons
                let rhs_asm = self.gen_expr(*lhs)?;

                let pop_lhs = self.pop("rax");
                let pop_rhs = self.pop("rbx");
                let push_ans = self.push("rax");

                let comment = op.clone();

                // TODO: remove hardcoded "rax,rbx"
                let operation_asm = match op {
                    TokenKind::Divide => "    div rbx\n".to_owned(),
                    TokenKind::Multiply => "    mul rbx\n".to_owned(),
                    TokenKind::Subtract => "    sub rax, rbx\n".to_owned(),
                    TokenKind::Add => "    add rax, rbx\n".to_owned(),
                    _ if op.is_logical_op() => self.gen_logical(op)?,
                    _ if op.is_bitwise_op() => self.gen_bitwise(op)?,
                    _ if op.is_cmp_op() => self.gen_cmp(op)?,
                    _ => {
                        return Err(format!(
                            "[COMPILER_GEN] Unable to generate Binary expression: '{op:?}'"
                        ))
                    }
                };

                // logical needs lhs_asm -> lhs_cmp

                Ok(format!(
                    "{lhs_asm}\
                     {rhs_asm}\
                     {pop_lhs}\
                     {pop_rhs}\
                     ; Binary Expr: {comment:?}\n\
                     {operation_asm}\
                     {push_ans}",
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
                     {pop_expr}\
                     {op_asm}\
                     {push_ans}",
                ))
            }
        };
    }

    fn gen_term(&mut self, term: NodeTerm) -> Result<String, String> {
        return match term {
            NodeTerm::IntLit(token) => {
                let int_lit = token.value.clone().unwrap();
                let push_rax = self.push("rax");

                Ok(format!(
                    ";     {:?}\n    \
                    mov rax, {int_lit}\n\
                    {push_rax}",
                    token
                ))
            }
            NodeTerm::Ident(token) => {
                let ident = &token.value.clone().unwrap();
                if !self.vars_map.contains_key(ident) {
                    return Err(format!("[COMPILER_GEN] Variable: {ident:?} doesn't exist."));
                }

                let stk_index = &self.vars_map.get(ident).unwrap().stk_pos;
                // println!("stk_ptr: {} - stk_index: {stk_index}", self.stk_ptr);
                let stk_offset = (self.stk_ptr - stk_index) * WORD_SIZE;
                let push_copy = self.push(format!("QWORD [rsp + {}]", stk_offset).as_str());

                Ok(format!(
                    ";     {token:?}\n\
                     {push_copy}"
                ))
            }
            NodeTerm::Paren(expr) => self.gen_expr(expr),
        };
    }

    fn gen_logical(&mut self, op: TokenKind) -> Result<String, String> {
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
                Ok(format!("; 'LogicalAnd' comparison HERE <--"))
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
                Ok(format!("; 'LogicalOr' comparison HERE <--"))
            }
            _ => Err(format!(
                "[COMPILER_GEN] Unable to generate logical operation"
            )),
        };
    }

    fn gen_cmp(&mut self, op: TokenKind) -> Result<String, String> {
        let set_asm = match op {
            TokenKind::Equal => "    sete al\n",
            TokenKind::NotEqual => "    setne al\n",
            TokenKind::GreaterThan => "    setg al\n",
            TokenKind::GreaterEqual => "    setge al\n",
            TokenKind::LessThan => "    setl al\n",
            TokenKind::LessEqual => "    setle al\n",
            _ => return Err(format!("[COMPILER_GEN] Unable to generate comparison")),
        };

        let cmp_asm = "    cmp rax, rbx\n";
        let mov_asm = "    movzx rax, al\n";
        let push_ans = self.push("rax");

        Ok(format!(
            "{cmp_asm}\
             {set_asm}\
             {mov_asm}\
             {push_ans}"
        ))
    }

    fn gen_bitwise(&self, op: TokenKind) -> Result<String, String> {
        let asm = match op {
            TokenKind::LeftShift => "    sal rax, rbx\n",
            TokenKind::RightShift => "    sar rax, rbx\n",
            TokenKind::BitwiseOr => "    or rax, rbx\n",
            TokenKind::BitwiseXor => "    xor rax, rbx\n",
            TokenKind::BitwiseAnd => "    and rax, rbx\n",
            TokenKind::BitwiseNot => "    not rax, rbx\n",
            _ => {
                return Err(format!(
                    "[COMPILER_GEN] Unable to generate bitwise operation"
                ));
            }
        };

        Ok(format!("{asm}"))
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
