// >>CODE GEN<< Taking AST from parse && info from semantic and generating (hopefully optimising) code!
// Useful Semantic Info:
//      - types && type_map, provides info on byte size, etc (is array...)
//      - Let stmt --> Semantic Variable created, use that! don't need to consume

use crate::{
    lex::{TokenFlags, TokenKind},
    parse::{NodeExpr, NodeScope, NodeStmt, NodeTerm, AST},
    semantic::{CodeGenData, PrimFlags, Type},
};
use std::collections::HashMap;
const LOG_DEBUG_INFO: bool = false;
const WORD_SIZE: usize = 8;
const SPACE: &'static str = "    ";
const ERR_MSG: &'static str = "[ERROR_CODEGEN]";
const DBG_MSG: &'static str = "[DEBUG_CODEGEN]";

#[derive(Debug, Clone, PartialEq)]
struct GenVariable {
    ident: String,
    var_type: Type,
    stk_index: usize,
}

struct CodeGenContext {
    reg_count: usize,
    label_count: usize,
    endif_label: String,
    loop_end_label: String, //Vec<String>,
}

pub struct Generator {
    ast: AST,
    ctx: CodeGenContext,
    stk_pos: usize,
    stack: Vec<GenVariable>,
    var_map: HashMap<String, GenVariable>,
}

impl Generator {
    pub fn new(gen_data: CodeGenData) -> Generator {
        Generator {
            ast: gen_data.ast,
            stk_pos: 0,
            stack: Vec::new(),
            var_map: HashMap::new(),
            ctx: CodeGenContext {
                reg_count: 0,
                label_count: 0,
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
            NodeStmt::SemDecl(sem_var) => {
                if let Some(var) = self.var_map.get(sem_var.ident.as_str()) {
                    return Err(format!(
                        "{ERR_MSG} Re-Initialisation of a GenVariable:\n{sem_var:#?}"
                    ));
                }
                let width = sem_var.var_type.width;
                let var = GenVariable {
                    ident: sem_var.ident.clone(),
                    stk_index: self.stk_pos + width,
                    var_type: sem_var.var_type,
                };
                self.stk_pos += width;
                self.stack.push(var.clone());
                self.var_map.insert(var.ident.clone(), var);
                if let Some(expr) = sem_var.init_expr {
                    return self.gen_expr(expr, Some(&self.gen_stk_access(self.stk_pos, width)));
                }
                Ok("\n".to_string()) // just variable decl, no assignment.
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
                    Some(var) => self.gen_expr(
                        expr,
                        Some(&self.gen_stk_access(var.stk_index, var.var_type.width)),
                    ),
                    None => unreachable!("{ERR_MSG} GenVariable not found for ident: '{ident}'"),
                }
            }
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
            NodeStmt::Decl { .. } => unreachable!("{ERR_MSG} Found {stmt:#?}.. shouldn't have."),
        }
    }

    // TODO: scope.inherits_stmts does nothing currently.
    fn gen_scope(&mut self, scope: NodeScope) -> Result<String, String> {
        debug_print(format!("{DBG_MSG} Beginning scope").as_str());
        let var_count = self.stack.len();
        let mut asm = String::new();
        for stmt in scope.stmts {
            asm += self.gen_stmt(stmt)?.as_str();
        }

        let pop_amt = self.stack.len() - var_count;
        debug_print(format!("{DBG_MSG} Ending scope, pop({pop_amt})").as_str());
        for _ in 0..pop_amt {
            let popped_var = match self.stack.pop() {
                Some(var) => var,
                None => return Err(format!("{ERR_MSG} uhh.. scope messed up")),
            };
            self.stk_pos -= popped_var.var_type.width;
            self.var_map.remove(popped_var.ident.as_str()).unwrap();
            debug_print(format!("{DBG_MSG} Scope ended, removing {popped_var:#?}").as_str());
        }
        Ok(asm)
    }

    fn gen_expr(&mut self, expr: NodeExpr, ans_reg: Option<&str>) -> Result<String, String> {
        debug_print(
            format!(
                "{}\n{DBG_MSG} gen expr, reg: {ans_reg:?} \n{expr:#?}\n",
                "-".repeat(20)
            )
            .as_str(),
        );
        let mut asm = String::new();
        match expr {
            NodeExpr::Term(term) => return self.gen_term(term, ans_reg),
            NodeExpr::BinaryExpr { op, lhs, rhs } => {
                let lhs_asm = self.gen_expr(*lhs, None)?;
                let rhs_asm = self.gen_expr(*rhs, None)?;

                let flags = op.get_flags();
                let op_asm = match flags {
                    _ if flags.intersects(TokenFlags::BIT) => self.gen_bitwise(op)?,
                    _ if flags.intersects(TokenFlags::ARITH) => self.gen_arithmetic(op)?,
                    _ if flags.intersects(TokenFlags::CMP) => self.gen_comparison(op)?,
                    _ if flags.intersects(TokenFlags::LOG) => return self.gen_logical(op, ans_reg, lhs_asm, rhs_asm),
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
            }
            NodeExpr::UnaryExpr { op, operand } => {
                asm += self.gen_expr(*operand, None)?.as_str();

                let reg = self.get_reg(self.ctx.reg_count);
                let op_asm = match op {
                    // TokenKind::Ones_Complement_Goes_HERE! => format!("{SPACE}not {reg}\n",),
                    TokenKind::Sub => todo!("unary sub. do '0-EXPR' ??"),
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
            }
        }
        // don't need to release reg if its just operation, just doing stuff on data.
        // only release if changing stack data.
        if let Some(reg) = ans_reg {
            // if reg == "push" {
            //     asm += format!("{SPACE}push {}\n", self.get_reg(self.ctx.reg_count)).as_str();
            //     self.release_reg();
            // }
            // else {
            asm += format!("{SPACE}mov {reg}, {}\n", self.get_reg(self.ctx.reg_count)).as_str();
            self.release_reg();
            // }
        }
        Ok(asm)
    }

    fn gen_term(&mut self, term: NodeTerm, ans_reg: Option<&str>) -> Result<String, String> {
        match term {
            NodeTerm::IntLit(tok) => {
                let reg = match ans_reg {
                    // Some(reg) if reg == "push" => {
                    //     return Ok(format!("{SPACE}{reg} {}\n", tok.value.unwrap()));
                    //     // confusing, reg == "push "
                    // }
                    Some(reg) => reg,
                    None => self.next_reg(),
                };
                Ok(format!("{SPACE}mov {reg}, {}\n", tok.value.unwrap()))
            }
            NodeTerm::Ident(tok) => {
                let ident = tok.value.clone().unwrap();
                match self.var_map.get(ident.as_str()) {
                    Some(var) => {
                        let stk_pos = self.gen_var_access(var.stk_index, var.var_type.width);
                        let reg = match ans_reg {
                            Some(reg) => reg,
                            None => self.next_reg(),
                        };
                        Ok(format!("{SPACE}mov {reg}, {stk_pos} ; {tok:?}\n"))
                    }
                    None => Err(format!("{ERR_MSG} GenVariable: {ident:?} doesn't exist.")),
                }
            }
        }
    }

    // TODO: Remove excess 'cmp', do 'Constant Folding'
    // "movzx {reg1},al" << zeros reg && moves in al (0,1).
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
        let reg1 = self.get_reg(self.ctx.reg_count - 1); // first value is further down because its a stack
        let reg2 = self.get_reg(self.ctx.reg_count);
        let operation_asm = match op {
            TokenKind::Add => format!("add {reg1}, {reg2}"),
            TokenKind::Sub => format!("sub {reg1}, {reg2}"),
            TokenKind::Mul => format!("imul {reg1}, {reg2}"),
            TokenKind::Quo => format!("cqo\n{SPACE}idiv {reg2}"),
            TokenKind::Mod => format!("cqo\n{SPACE}idiv {reg2}\n{SPACE}mov {reg1}, rdx"), // TODO: 'cqo', instruction changes with reg size
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
        self.ctx.label_count += 1;
        format!(".{:X}_{name}", self.ctx.label_count) // '.' denotes a local scoped label in asm
    }

    fn gen_var_access(&self, stk_index: usize, word_size: usize) -> String {
        format!("[rbp-{stk_index}]")
    }

    fn gen_stk_access(&self, stk_index: usize, word_size: usize) -> String {
        format!("{} [rbp-{stk_index}]", self.gen_access_size(word_size))
    }

    fn gen_access_size(&self, word_size: usize) -> &str {
        match word_size {
            1 => "byte",
            2 => "word",
            4 => "dword",
            8 => "qword",
            _ => unreachable!("{ERR_MSG} Invalid word_size found: '{word_size}'"),
        }
    }

    fn next_reg(&mut self) -> &'static str {
        // preserved_registers = ["rdx", ...],
        let scratch_registers = ["rax", "rcx", "rsi", "rdi", "r8", "r9", "r10", "r11"];
        match scratch_registers.get(self.ctx.reg_count) {
            Some(reg) => {
                self.ctx.reg_count += 1;
                reg
            }
            None => todo!("no available reg!"),
        }
    }

    fn get_reg(&mut self, index: usize) -> &'static str {
        // preserved_registers = ["rdx", ...],
        let scratch_registers = ["rax", "rcx", "rsi", "rdi", "r8", "r9", "r10", "r11"];
        match scratch_registers.get(index - 1) {
            Some(reg) => reg,
            None => todo!("oob reg check"),
        }
    }

    fn release_reg(&mut self) {
        self.ctx.reg_count -= 1;
    }

    // #[allow(dead_code)]
    // fn gen_push(&mut self, reg: &str) -> String {
    //     if LOG_DEBUG_INFO {
    //         println!("{DBG_MSG} pushing {reg} | {}", self.ctx.stk_ptr);
    //     }
    //     format!("{SPACE}push {reg}\n")
    // }

    // #[allow(dead_code)]
    // fn gen_pop(&mut self, reg: &str) -> String {
    //     if LOG_DEBUG_INFO {
    //         println!("{DBG_MSG} popping into {reg} | {}", self.ctx.stk_ptr);
    //     }
    //     format!("{SPACE}pop {reg}\n")
    // }
}

fn debug_print(msg: &str) {
    if LOG_DEBUG_INFO {
        println!("{msg}")
    }
}
