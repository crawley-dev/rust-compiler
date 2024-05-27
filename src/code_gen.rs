use crate::{
    lex::TokenKind,
    parse::{NodeExpr, NodeScope, NodeStmt, NodeTerm, AST},
};
use std::collections::HashMap;
const LOG_DEBUG_INFO: bool = false;
const WORD_SIZE: usize = 8;
const SPACE: &'static str = "    ";
const ERR_MSG: &'static str = "[ERROR_CODEGEN]";
const DBG_MSG: &'static str = "[DEBUG_CODEGEN]";

#[derive(Debug, Clone, PartialEq)]
struct Variable {
    stk_index: usize,
    ident: Option<String>,
}

struct Context {
    stk_ptr: usize,
    reg_count: usize,
    endif_label: String,
    loop_end_label: String,
}

pub struct Generator {
    ast: AST,
    label_count: usize,
    stack: Vec<Variable>,
    var_map: HashMap<String, Variable>,
    ctx: Context,
}

impl Generator {
    pub fn new(ast: AST) -> Generator {
        Generator {
            ast,
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
        while !self.ast.stmts.is_empty() {
            let stmt = self.ast.stmts.remove(0);
            asm += self.gen_stmt(stmt)?.as_str();
        }
        Ok(asm)
    }

    // TODO: BYTE ARRAYS!
    fn gen_stmt(&mut self, stmt: NodeStmt) -> Result<String, String> {
        match stmt {
            NodeStmt::NakedScope(scope) => self.gen_scope(scope),
            NodeStmt::Exit(expr) => {
                let expr_asm = self.gen_expr(expr, Some("rdi"))?;
                Ok(format!(
                    "; Exit Program\n\
                     {expr_asm}\
                     {SPACE}mov rax, 60\n\
                     {SPACE}syscall\n"
                ))
            }
            NodeStmt::Let { expr, ident, .. } => {
                if let Some(var) = self.var_map.get(ident.as_str()) {
                    return Err(format!(
                        "{ERR_MSG} Re-Initialisation of a Variable:\n{var:?}"
                    ));
                }
                if self.stack.len() != 0 {
                    self.ctx.stk_ptr += 1;
                }
                let var = Variable {
                    ident: Some(ident.clone()),
                    stk_index: self.ctx.stk_ptr,
                };
                let reg = self.gen_stk_pos(var.stk_index);
                self.stack.push(var.clone());
                self.var_map.insert(ident.clone(), var);
                self.gen_expr(expr, Some(reg.as_str()))
                //
                //     if self.ctx.binding_var.is_some() {
                //         return Err(format!("{ERR_MSG} Uhhh, trying to bind a binding?"));
                //     }
                //     if self.stack.len() != 0 {
                //         self.ctx.stk_ptr += 1;
                //     }
                //     let var = Variable {
                //         stk_index: self.ctx.stk_ptr,
                //         ident: None,
                //         mutable,
                //     };
                //     self.ctx.binding_var = Some(var);
                //     self.gen_stmt(*assign)
                // }
            }
            NodeStmt::Assign(expr) => {
                let ident = match &expr {
                    NodeExpr::BinaryExpr { lhs, .. } => match **lhs {
                        NodeExpr::Term(NodeTerm::Ident(ref tok)) => {
                            tok.value.as_ref().unwrap().as_str()
                        }
                        _ => unreachable!("{ERR_MSG} Invalid Assignment: '{expr:#?}'"),
                    },
                    _ => unreachable!("{ERR_MSG} Invalid Assignment: '{expr:#?}'"),
                };
                match self.var_map.get(ident) {
                    Some(var) => {
                        let reg = self.gen_stk_pos(var.stk_index);
                        self.gen_expr(expr, Some(reg.as_str()))
                    }
                    None => unreachable!(""),
                }
            }
            // NodeStmt::Assign(mut expr) => {
            //     if LOG_DEBUG_INFO {
            //         println!("{DBG_MSG} assign: {expr:#?}");
            //     }
            //     // check a variable is in the map,
            //     // .. in: if mutable, all good, else ERR!
            //     // .. not: is it an invalid variable, or being assigned in a 'let binding'?
            //     let ident = match &expr {
            //         NodeExpr::BinaryExpr { lhs, .. } => match **lhs {
            //             NodeExpr::Term(NodeTerm::Ident(ref tok)) => {
            //                 tok.value.as_ref().unwrap().clone()
            //             }
            //             _ => return Err(format!("{ERR_MSG} Invalid Assignment: '{expr:#?}'")),
            //         },
            //         _ => return Err(format!("{ERR_MSG} Invalid Assignment: '{expr:#?}'")),
            //     };

            //     // if '=', remove op & lhs, gen_expr(rhs)
            //     // elif '', check if var exists, replace Op= with Op, e.g '+=' --> '='
            //     let mut arith_assign = false;
            //     if let NodeExpr::BinaryExpr { op, lhs, rhs } = expr {
            //         if op == TokenKind::Assign {
            //             expr = *rhs;
            //         } else {
            //             arith_assign = true;
            //             expr = NodeExpr::BinaryExpr {
            //                 op: op.assign_to_arithmetic()?,
            //                 lhs,
            //                 rhs,
            //             };
            //         }
            //     }

            //     // Re-assigning
            //     if let Some(var) = self.var_map.get(ident.as_str()) {
            //         if !var.mutable {
            //             return Err(format!("{ERR_MSG} Re-assignment of constant: '{var:#?}'"));
            //         }
            //         let reg = self.gen_stk_pos(var.stk_index);
            //         return self.gen_expr(expr, Some(reg.as_str()));
            //     }

            //     // Binding new var, cannot arith-assign (e.g +=) to new var!
            //     match self.ctx.binding_var.take() {
            //         Some(_) if arith_assign => Err(format!(
            //             "{ERR_MSG} Cannot arith-assign an unitialised variable:\n'{ident:?}'"
            //         )),
            //         Some(mut var) => {
            //             let reg = self.gen_stk_pos(var.stk_index);
            //             var.ident = Some(ident.clone());
            //             self.stack.push(var.clone());
            //             self.var_map.insert(ident, var);
            //             self.gen_expr(expr, Some(reg.as_str()))
            //         }
            //         None => Err(format!("{ERR_MSG} No var to bind '{ident:?}' to")),
            //     }
            // }
            NodeStmt::If {
                condition,
                scope,
                branches,
            } => {
                // TODO(TOM): operand changes jump instruction, e.g je (jump if equal)
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

                let condition_asm = self.gen_expr(condition, None)?;
                let scope_asm = self.gen_scope(scope)?;

                let mut branches_asm = String::new();
                for branch in branches {
                    branches_asm += &self.gen_stmt(branch)?;
                }

                Ok(format!(
                    "; If\n\
                     {condition_asm}\
                     {SPACE}cmp rax, 0 \n\
                     {SPACE}je {false_label}\n\
                     {scope_asm}\
                     {endif_jmp}\
                     {false_label}:\n\
                     {branches_asm}\
                     {endif_goto}"
                ))
            }
            NodeStmt::ElseIf { condition, scope } => {
                let false_label = self.gen_label("ELIF_FALSE");
                let scope_asm = self.gen_scope(scope)?;
                let condition_asm = self.gen_expr(condition, None)?;
                let endif_label = self.ctx.endif_label.as_str();

                Ok(format!(
                    "{condition_asm}\n\
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
            NodeStmt::While { condition, scope } => {
                let cmp_label = self.gen_label("WHILE_CMP");
                let scope_label = self.gen_label("WHILE_SCOPE");
                let loop_end_label = self.gen_label("WHILE_END");
                self.ctx.loop_end_label = loop_end_label.clone();

                let scope_asm = self.gen_scope(scope)?;
                let condition_asm = self.gen_expr(condition, None)?;

                Ok(format!(
                    "; While\n\
                     {SPACE}jmp {cmp_label}\n\
                     {scope_label}:\n\
                     {scope_asm}\
                     {cmp_label}:\n\
                     {condition_asm}\
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
        if LOG_DEBUG_INFO {
            println!("{DBG_MSG} Beginning scope");
        }
        let var_count = self.stack.len();
        let mut asm = String::new();
        for stmt in scope.stmts {
            asm += self.gen_stmt(stmt)?.as_str();
        }

        let pop_amt = self.stack.len() - var_count;
        if LOG_DEBUG_INFO {
            println!("{DBG_MSG} Ending scope, pop({pop_amt})");
        }
        for _ in 0..pop_amt {
            self.ctx.stk_ptr -= 1;
            let popped_var = match self.stack.pop() {
                Some(var) => self.var_map.remove(var.ident.as_ref().unwrap()),
                None => return Err(format!("{ERR_MSG} uhh.. scope messed up")),
            };
            if LOG_DEBUG_INFO {
                println!("{DBG_MSG} Scope ended, removing {popped_var:#?}");
            }
        }
        Ok(asm)
    }

    fn gen_expr(&mut self, expr: NodeExpr, ans_reg: Option<&str>) -> Result<String, String> {
        if LOG_DEBUG_INFO {
            println!(
                "{0}\n{DBG_MSG} gen expr, reg: {ans_reg:?} \n{expr:#?}\n",
                "-".repeat(20)
            );
        }
        match expr {
            NodeExpr::BinaryExpr { op, lhs, rhs } => {
                let mut asm = String::new();
                let lhs_asm = self.gen_expr(*lhs, None)?;
                let rhs_asm = self.gen_expr(*rhs, None)?;

                let op_asm = match op {
                    _ if op.is_bitwise() => self.gen_bitwise(op)?,
                    _ if op.is_arithmetic() => self.gen_arithmetic(op)?,
                    _ if op.is_logical() => return self.gen_logical(op, ans_reg, lhs_asm, rhs_asm),
                    _ if op.is_comparison() => self.gen_comparison(op)?,
                    _ if op.is_assignment() => {
                        return Err(format!(
                            "{ERR_MSG} Unable to assign to a constant:\n{lhs_asm}{op:?}\n{rhs_asm}"
                        ))
                    }
                    _ => {
                        return Err(format!(
                            "{ERR_MSG} Unable to generate binary expression:\n{lhs_asm}..{op:?}..\n{rhs_asm}"
                        ))
                    }
                };
                self.release_reg(); // first reg stores arithmetic answer, don't release it.

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
                    TokenKind::Sub => todo!("unary sub. do '0-EXPR' ??"),
                    // TokenKind:: => format!("{SPACE}not {reg}\n",),
                    TokenKind::CmpNot => format!(
                        "{SPACE}test {reg}, {reg}\n\
                         {SPACE}sete al\n\
                         {SPACE}movzx {reg}, al\n"
                    ),
                    _ => {
                        return Err(format!(
                            "{ERR_MSG} Unable to generate unary expression: '{op:?}'"
                        ))
                    }
                };
                asm += op_asm.as_str();
                // don't need to release reg if its just operation, just doing stuff on data.
                // only release if changing stack data.
                if let Some(reg) = ans_reg {
                    asm += format!("{SPACE}mov {reg}, {}\n", self.get_reg(self.ctx.reg_count))
                        .as_str();
                    self.release_reg();
                }
                Ok(asm)
            }
            NodeExpr::Term(term) => self.gen_term(term, ans_reg),
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
                    None => Err(format!("{ERR_MSG} Variable: {ident:?} doesn't exist.")),
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
            TokenKind::CmpAnd => {
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
            TokenKind::CmpOr => {
                let false_label = self.gen_label("OR_FALSE");
                let true_label = self.gen_label("OR_TRUE");
                let final_label = self.gen_label("OR_FINAL");

                Ok(format!(
                    "; CmpOr\n\
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
            _ => Err(format!("{ERR_MSG} Unable to generate Logical comparison")),
        }
    }

    fn gen_arithmetic(&mut self, op: TokenKind) -> Result<String, String> {
        let reg1 = self.get_reg(self.ctx.reg_count - 1); // because its a stack
        let reg2 = self.get_reg(self.ctx.reg_count);
        let operation_asm = match op {
            TokenKind::Add => format!("add {reg1}, {reg2}"),
            TokenKind::Sub => format!("sub {reg1}, {reg2}"),
            TokenKind::Mul => format!("imul {reg1}, {reg2}"),
            TokenKind::Quo => format!("cqo\n{SPACE}idiv {reg2}"),
            TokenKind::Mod => format!("cqo\n{SPACE}idiv {reg2}\n{SPACE}mov {reg1}, rdx"), // TODO: 'cqo' changes with reg size
            _ => {
                return Err(format!(
                    "{ERR_MSG} Unable to generate Arithmetic operation: '{op:?}'"
                ))
            }
        };
        Ok(format!("{SPACE}{operation_asm}\n"))
    }

    fn gen_bitwise(&mut self, op: TokenKind) -> Result<String, String> {
        let reg1 = self.get_reg(self.ctx.reg_count - 1);
        let reg2 = self.get_reg(self.ctx.reg_count);
        let asm = match op {
            TokenKind::BitOr => "or",
            TokenKind::BitXor => "xor",
            TokenKind::BitAnd => "and",
            TokenKind::Shl => "sal",
            TokenKind::Shr => "sar",
            // TODO: (Types) Unsigned shift: shl, shr
            _ => return Err(format!("{ERR_MSG} Unable to generate Bitwise operation")),
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
            TokenKind::CmpEq => Ok("e"),
            TokenKind::NotEq => Ok("ne"),
            TokenKind::Gt => Ok("g"),
            TokenKind::GtEq => Ok("ge"),
            TokenKind::Lt => Ok("l"),
            TokenKind::LtEq => Ok("le"),
            _ => Err(format!("{ERR_MSG} Unable to generate comparison modifier")),
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
                // if LOG_DEBUG_INFO {
                //     println!("{DBG_MSG} next reg: {} | {}", reg, self.ctx.reg_count);
                // }
                reg.to_string()
            }
            None => todo!("no available reg!"),
        }
    }

    fn get_reg(&mut self, index: usize) -> String {
        // preserved_registers = ["rdx", ...],
        let scratch_registers = ["rax", "rcx", "rsi", "rdi", "r8", "r9", "r10", "r11"];
        // if LOG_DEBUG_INFO {
        //     println!("{DBG_MSG} getting reg[{} - 1]", index);
        // }
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
            println!("{DBG_MSG} pushing {reg} | {}", self.ctx.stk_ptr);
        }
        return format!("{SPACE}push {reg}");
    }

    #[allow(dead_code)]
    fn pop(&mut self, reg: &str) -> String {
        self.ctx.stk_ptr -= 1;
        if LOG_DEBUG_INFO {
            println!("{DBG_MSG} popping into {reg} | {}", self.ctx.stk_ptr);
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
                        "{ERR_MSG} Unable to generate binary expression"
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
                None => Err(format!("{ERR_MSG} Variable: {ident:?} doesn't exist.")),
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
            "{ERR_MSG} Unable to generate Logical comparison"
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
                "{ERR_MSG} Unable to generate Arithmetic operation: '{op:?}'"
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
                "{ERR_MSG} Unable to generate Bitwise operation"
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
