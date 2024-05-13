use crate::{
    lex::TokenKind,
    parse::{NodeExpr, NodeProg, NodeScope, NodeStmt, NodeTerm},
};

use std::collections::HashMap;

const LOG_DEBUG_INFO: bool = false;
const WORD_SIZE: usize = 8;
const SPACE: &'static str = "    ";

#[derive(Debug, Clone, PartialEq)]
struct Variable {
    stk_index: usize,
    ident: Option<String>,
    mutable: bool,
}

struct Context {
    stk_ptr: usize,
    reg_count: usize,
    endif_label: String,
    loop_end_label: String,
}

pub struct Generator {
    prog: NodeProg,
    label_count: usize,
    stack: Vec<Variable>,
    var_map: HashMap<String, Variable>,
    ctx: Context,
}

impl Generator {
    pub fn new(prog: NodeProg) -> Generator {
        Generator {
            prog,
            label_count: 0,
            stack: Vec::new(),
            var_map: HashMap::new(),
            ctx: Context {
                stk_ptr: 0,
                reg_count: 0,
                endif_label: String::new(),
                loop_end_label: String::new(),
            },
        }
    }

    pub fn gen_asm(&mut self) -> Result<String, String> {
        let mut asm = String::new();
        while !self.prog.stmts.is_empty() {
            let stmt = self.prog.stmts.remove(0);
            asm += self.gen_stmt(stmt)?.as_str();
        }
        Ok(asm)
    }

    // TODO: BYTE ARRAYS!
    fn gen_stmt(&mut self, stmt: NodeStmt) -> Result<String, String> {
        match stmt {
            NodeStmt::Scope(scope) => self.gen_scope(scope),
            NodeStmt::Exit(expr) => {
                let expr_asm = self.gen_expr(expr, Some("rdi"))?;
                Ok(format!(
                    "; Exit Program\n\
                     {expr_asm}\
                     {SPACE}mov rax, 60\n\
                     {SPACE}syscall\n"
                ))
            }
            NodeStmt::Let(assignment, mutable) => {
                if self.stack.len() != 0 {
                    self.ctx.stk_ptr += 1;
                }
                self.stack.push(Variable {
                    stk_index: self.ctx.stk_ptr,
                    ident: None,
                    mutable,
                });
                self.gen_stmt(*assignment)
            }
            NodeStmt::Assign(ident, expr) => {
                // TODO: what types of expr can a let statement handle ??
                match self.var_map.get(ident.value.as_ref().unwrap()) {
                    Some(var) if var.mutable => {
                        let reg = self.gen_stk_pos(var.stk_index);
                        self.gen_expr(expr, Some(reg.as_str()))
                    }
                    Some(_) => Err(format!(
                        "[COMPILER_GEN] Re-assignment of constant {:?}",
                        ident.value.unwrap()
                    )),
                    None => match self.stack.last_mut() {
                        Some(new_var) if new_var.ident.is_none() => {
                            new_var.ident = ident.value.clone();
                            self.var_map.insert(ident.value.unwrap(), new_var.clone());

                            // TODO: refactor.
                            // let reg = self.gen_stk_pos(new_var.stk_index);
                            // self.gen_expr(expr, reg.as_str())
                            let reg = format!("QWORD [rsp+{}]", new_var.stk_index * WORD_SIZE);
                            self.gen_expr(expr, Some(reg.as_str()))
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

                let mut endif_jmp = String::new();
                let mut endif_goto = String::new();
                if !branches.is_empty() {
                    self.ctx.endif_label = self.gen_label("END_IF");
                    endif_goto = format!("{}:\n", self.ctx.endif_label.as_str());
                    endif_jmp = format!("{SPACE}jmp {}\n", self.ctx.endif_label.as_str());
                }
                let false_label = self.gen_label("IF_FALSE");

                let expr_asm = self.gen_expr(expr, None)?;
                let scope_asm = self.gen_scope(scope)?;

                let mut branches_asm = String::new();
                for branch in branches {
                    branches_asm += &self.gen_stmt(branch)?;
                }

                Ok(format!(
                    "; If\n\
                     {expr_asm}\
                     {SPACE}cmp rax, 0 \n\
                     {SPACE}je {false_label}\n\
                     {scope_asm}\
                     {endif_jmp}\
                     {false_label}:\n\
                     {branches_asm}\
                     {endif_goto}"
                ))
            }
            NodeStmt::ElseIf(expr, scope) => {
                let false_label = self.gen_label("ELIF_FALSE");
                let scope_asm = self.gen_scope(scope)?;
                let expr_asm = self.gen_expr(expr, None)?;
                let endif_label = self.ctx.endif_label.as_str();

                Ok(format!(
                    "{expr_asm}\n\
                     {SPACE}cmp rax, 0\n\
                     {SPACE}je {false_label}\n\
                     {scope_asm}\
                     {SPACE}jmp {endif_label}\n\
                     {false_label}:\n"
                ))
            }
            NodeStmt::Else(scope) => {
                let scope_asm = self.gen_scope(scope)?;
                Ok(format!(
                    "; Else\n\
                     {scope_asm}"
                ))
            }
            NodeStmt::While(expr, scope) => {
                let cmp_label = self.gen_label("WHILE_CMP");
                let scope_label = self.gen_label("WHILE_SCOPE");
                let loop_end_label = self.gen_label("WHILE_END");
                self.ctx.loop_end_label = loop_end_label.clone();

                let scope_asm = self.gen_scope(scope)?;
                let cmp_asm = self.gen_expr(expr, None)?;

                Ok(format!(
                    "; While\n\
                     {SPACE}jmp {cmp_label}\n\
                     {scope_label}:\n\
                     {scope_asm}\
                     {cmp_label}:\n\
                     {cmp_asm}\
                     {SPACE}cmp rax, 0\n\
                     {SPACE}jne {scope_label}\n\
                     {loop_end_label}:\n"
                ))
            }
            NodeStmt::Break => Ok(format!(
                "{SPACE}jmp {label} ; break\n",
                label = self.ctx.loop_end_label.as_str()
            )),
        }
    }

    // TODO: scope.inherits_stmts does nothing currently.
    fn gen_scope(&mut self, scope: NodeScope) -> Result<String, String> {
        let var_count = self.stack.len();
        let mut asm = String::new();
        for stmt in scope.stmts {
            asm += self.gen_stmt(stmt)?.as_str();
        }

        let pop_amt = self.stack.len() - var_count;
        println!("{} - {pop_amt}", self.ctx.stk_ptr);
        if self.ctx.stk_ptr != 0 {
            self.ctx.stk_ptr -= pop_amt - 1;
        }
        for _ in 0..pop_amt {
            let popped_var = match self.stack.pop() {
                Some(var) => self.var_map.remove(&var.ident.unwrap()),
                None => return Err(format!("[COMPILER_GEN] uhh.. scope messed up")),
            };
            if LOG_DEBUG_INFO {
                println!("[GEN_DEBUG] Scope ended, removing {popped_var:#?}");
            }
        }
        Ok(asm)
    }

    fn gen_expr(&mut self, expr: NodeExpr, ans_reg: Option<&str>) -> Result<String, String> {
        if LOG_DEBUG_INFO {
            println!("\n[GEN_DEBUG] gen expr, reg: {ans_reg:?} = {expr:#?}\n");
        }
        match expr {
            NodeExpr::Term(term) => self.gen_term(term, ans_reg),
            NodeExpr::BinaryExpr { op, lhs, rhs } => {
                let mut asm = String::new();
                let lhs_asm = self.gen_expr(*lhs, None)?;
                let rhs_asm = self.gen_expr(*rhs, None)?;

                let op_asm = match op {
                    _ if op.is_bitwise() => self.gen_bitwise(op)?,
                    _ if op.is_arithmetic() => self.gen_arithmetic(op)?,
                    _ if op.is_comparison() => self.gen_comparison(op)?,
                    _ if op.is_logical() => return self.gen_logical(op, ans_reg, lhs_asm, rhs_asm),
                    _ => {
                        return Err(format!(
                            "[COMPILER_GEN] Unable to generate binary expression"
                        ))
                    }
                };
                self.release_reg();
                // first reg stores arithmetic answer, don't release it.

                asm += lhs_asm.as_str();
                asm += rhs_asm.as_str();
                asm += op_asm.as_str();
                if let Some(reg) = ans_reg {
                    asm += format!("{SPACE}mov {reg}, {}\n", self.get_reg(self.ctx.reg_count))
                        .as_str();
                    self.release_reg();
                }

                Ok(asm)
            }
            NodeExpr::UnaryExpr { op, operand } => {
                let mut asm = String::new();
                asm += self.gen_expr(*operand, None)?.as_str();

                let reg = self.get_reg(self.ctx.reg_count);
                let op_asm = match op {
                    TokenKind::BitwiseNot => format!("{SPACE}not {reg}\n",),
                    TokenKind::LogicalNot => format!(
                        "{SPACE}test {reg}, {reg}\n\
                         {SPACE}sete al\n\
                         {SPACE}movzx {reg}, al\n"
                    ),
                    _ => {
                        return Err(format!(
                            "[COMPILER_GEN] Unable to generate unary expression"
                        ))
                    }
                };
                asm += op_asm.as_str();
                // don't need to release reg if its just operation, just doing stuff on data.
                // only release if moving data onto stack.
                if let Some(reg) = ans_reg {
                    asm += format!("{SPACE}mov {reg}, {}\n", self.get_reg(self.ctx.reg_count))
                        .as_str();
                    self.release_reg();
                }
                Ok(asm)
            }
        }
    }

    fn gen_term(&mut self, term: NodeTerm, ans_reg: Option<&str>) -> Result<String, String> {
        match term {
            NodeTerm::IntLit(tok) => {
                let reg = match ans_reg {
                    Some(reg) => reg.to_string(),
                    None => self.next_reg(),
                };
                Ok(format!("{SPACE}mov {reg}, {}\n", tok.value.unwrap()))
            }
            NodeTerm::Ident(tok) => {
                let ident = tok.value.clone().unwrap();
                match self.var_map.get(ident.as_str()) {
                    Some(var) => {
                        let stk_pos = self.gen_stk_pos(var.stk_index);
                        let reg = match ans_reg {
                            Some(reg) => reg.to_string(),
                            None => self.next_reg(),
                        };
                        Ok(format!("{SPACE}mov {reg}, {stk_pos} ; {tok:?}\n"))
                    }
                    None => Err(format!("[COMPILER_GEN] Variable: {ident:?} doesn't exist.")),
                }
            }
        }
    }

    // TODO: Remove excess 'cmp', 'Constant Folding'
    // "movzx {reg1},al" << zero inits reg && validates val is 1 or 0.
    fn gen_logical(
        &mut self,
        op: TokenKind,
        ans_reg: Option<&str>,
        lhs_asm: String,
        rhs_asm: String,
    ) -> Result<String, String> {
        let reg1 = self.get_reg(self.ctx.reg_count - 1);
        let reg2 = self.get_reg(self.ctx.reg_count);
        let mut mov_ans = String::new();
        if let Some(reg) = ans_reg {
            mov_ans = format!("{SPACE}mov {reg}, {}\n", self.get_reg(self.ctx.reg_count));
            self.release_reg();
        }
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

    fn gen_arithmetic(&mut self, op: TokenKind) -> Result<String, String> {
        let reg1 = self.get_reg(self.ctx.reg_count - 1); // because its a stack
        let reg2 = self.get_reg(self.ctx.reg_count);
        let operation_asm = match op {
            TokenKind::Divide => format!("cqo\n{SPACE}idiv {reg2}"),
            TokenKind::Multiply => format!("imul {reg1}, {reg2}"),
            TokenKind::Subtract => format!("sub {reg1}, {reg2}"),
            TokenKind::Add => format!("add {reg1}, {reg2}"),
            TokenKind::Remainder => format!("cqo\n{SPACE}idiv {reg2}\n{SPACE}mov {reg1}, rdx"), // TODO: 'cqo' changes with reg size
            _ => {
                return Err(format!(
                    "[COMPILER_GEN] Unable to generate Arithmetic operation: '{op:?}'"
                ))
            }
        };
        Ok(format!("{SPACE}{operation_asm}\n"))
    }

    fn gen_bitwise(&mut self, op: TokenKind) -> Result<String, String> {
        let reg1 = self.get_reg(self.ctx.reg_count - 1);
        let reg2 = self.get_reg(self.ctx.reg_count);
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

    fn gen_comparison(&mut self, op: TokenKind) -> Result<String, String> {
        let reg1 = self.get_reg(self.ctx.reg_count - 1);
        let reg2 = self.get_reg(self.ctx.reg_count);

        let cmp_mod = self.gen_cmp_modifier(op)?;
        let set_asm = format!("set{}", cmp_mod);

        Ok(format!(
            "{SPACE}cmp {reg1}, {reg2}\n\
             {SPACE}{set_asm} al\n\
             {SPACE}movzx {reg1}, al\n"
        ))
    }

    fn gen_cmp_modifier(&mut self, op: TokenKind) -> Result<&str, String> {
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
        format!(".{:X}_{name}", self.label_count) // '.' denotes a local scoped label
    }

    // TODO: in future, this will change depending on the size of var, e.g u8 or u32
    fn gen_stk_pos(&self, stk_index: usize) -> String {
        format!("QWORD [rsp+{}]", stk_index * WORD_SIZE)
    }

    fn next_reg(&mut self) -> String {
        // preserved_registers = ["rdx", ...],
        let scratch_registers = ["rax", "rcx", "rsi", "rdi", "r8", "r9", "r10", "r11"];
        match scratch_registers.get(self.ctx.reg_count) {
            Some(reg) => {
                self.ctx.reg_count += 1;
                if LOG_DEBUG_INFO {
                    println!("[GEN_DEBUG] next reg: {} | {}", reg, self.ctx.reg_count);
                }
                reg.to_string()
            }
            None => todo!("no available reg!"),
        }
    }

    fn get_reg(&mut self, index: usize) -> String {
        // preserved_registers = ["rdx", ...],
        let scratch_registers = ["rax", "rcx", "rsi", "rdi", "r8", "r9", "r10", "r11"];
        if LOG_DEBUG_INFO {
            println!("[GEN_DEBUG] getting reg[{} - 1]", index);
        }
        match scratch_registers.get(index - 1) {
            Some(reg) => reg.to_string(), // TODO: need lifetimes, change to &'str
            None => todo!("oob reg check"),
        }
    }

    fn release_reg(&mut self) {
        self.ctx.reg_count -= 1;
    }

    #[allow(dead_code)]
    fn push(&mut self, reg: &str) -> String {
        self.ctx.stk_ptr += 1;
        if LOG_DEBUG_INFO {
            println!("[GEN_DEBUG] pushing {reg} | {}", self.ctx.stk_ptr);
        }
        return format!("{SPACE}push {reg}");
    }

    #[allow(dead_code)]
    fn pop(&mut self, reg: &str) -> String {
        self.ctx.stk_ptr -= 1;
        if LOG_DEBUG_INFO {
            println!("[GEN_DEBUG] popping into {reg} | {}", self.ctx.stk_ptr);
        }
        return format!("{SPACE}pop {reg}");
    }
}

/*
// TODO: logical or/and bug, causes duplicate asm.
fn gen_expr(&mut self, expr: NodeExpr, reg: &str) -> Result<String, String> {
    // TODO: remove if using int_lit, put directly in operation.
    // let push_ans = if reg != "rax" {
    //     format!("{SPACE}mov {reg}, rax\n")
    // } else {
    //     format!("{}\n", self.push("rax"))
    // };
    match expr {
        NodeExpr::Term(term) => self.gen_term(*term, reg),
        NodeExpr::BinaryExpr { op, lhs, rhs } => {
            let comment = op.clone();
            let reg1 = "rax";
            let reg2 = "rcx";
            // let ans_reg = self.next_reg();

            let lhs_asm = self.gen_expr(*lhs, reg1.as_str())?;
            let rhs_asm = self.gen_expr(*rhs, reg2.as_str())?;
            let op_asm = match op {
                _ if op.is_bitwise() => self.gen_bitwise(op, reg1.as_str(), reg2.as_str())?,
                _ if op.is_comparison() => self.gen_cmp(op, reg1.as_str(), reg2.as_str())?,
                _ if op.is_arithmetic() => {
                    self.gen_arithmetic(op, reg1.as_str(), reg2.as_str())?
                }
                _ if op.is_logical() => {
                    return self.gen_logical(
                        op,
                        "AAA".to_string(),
                        reg1.as_str(),
                        reg2.as_str(),
                        &lhs_asm,
                        &rhs_asm,
                    )
                }
                _ => {
                    return Err(format!(
                        "[COMPILER_GEN] Unable to generate binary expression"
                    ))
                }
            };

            // self.ctx.reg_count -= 2;
            // {push_ans}
            Ok(format!(
                "; {comment:?}\n\
                 {lhs_asm}\
                 {rhs_asm}\
                 {op_asm}"
            ))
        }
        NodeExpr::UnaryExpr { op, operand } => {
            let comment = op.clone();
            let op_asm = match op {
                TokenKind::BitwiseNot => format!("{SPACE}not rax\n"),
                _ => todo!("logical not."), // need to invert cmp.. do later.
            };

            let expr_asm = self.gen_expr(*operand, "rax")?;

            // {push_ans}
            Ok(format!(
                "; {comment:?}\n\
                 {expr_asm}\
                 {op_asm}"
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
            // Ok(self.push("reg"))
        }
        NodeTerm::Ident(token) => {
            let ident = token.value.clone().unwrap();
            match self.var_map.get(ident.as_str()) {
                Some(var) => {
                    let stk_pos = self.gen_stk_pos(var.stk_index);
                    // Ok(format!("{}; {token:?}\n", self.push(stk_pos.as_str())))
                    Ok(format!("{SPACE}mov {reg}, {stk_pos} ; {token:?}\n"))
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
        TokenKind::Divide => format!("cqo\n{SPACE}idiv {reg2}"),
        TokenKind::Multiply => format!("imul {reg1}, {reg2}"),
        TokenKind::Subtract => format!("sub {reg1}, {reg2}"),
        TokenKind::Add => format!("add {reg1}, {reg2}"),
        TokenKind::Remainder => format!("cqo\n{SPACE}idiv {reg2}\n{SPACE}mov rax, rdx"), // TODO: 'cqo' changes with reg size
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
                // parse & generate rhs first until its a term.
                // .. then generate lhs.
                // .. then asm order: lhs --> rhs.

                // match *rhs {
                //     NodeExpr::UnaryExpr { .. } => todo!(),
                //     NodeExpr::BinaryExpr { .. } => asm += self.gen_expr(*rhs, None)?.as_str(),
                //     NodeExpr::Term(term) => {
                //         asm += self.gen_expr(*lhs, None)?.as_str();
                //         asm += self.gen_term(*term, None)?.as_str();
                //         // if let NodeTerm::Paren(paren_expr) = *term {
                //         //     asm += self.gen_expr(paren_expr, None)?.as_str();
                //         // } else {
                //         //     asm += self.gen_term(*term, None)?.as_str();
                //         // }
                //     }
                // }
*/
