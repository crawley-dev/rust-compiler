// >>CODE GEN<< Taking AST from parse && info from semantic and generating (hopefully optimising) code!
//  Useful Semantic Info:
//      - ✅ replace stmt NodeStmt with semantic equivalent (holds different info, types etc.)
//      - Let stmt --> Semantic Variable created, use that! don't need to consume
//  Pointers:
//      - address of: get var's stk_pos and use "lea" to get the memory address
//      - deref: currently blind trust towards the memory address that is being de-referenced, may seg faults to come!

use crate::{
    debug, err,
    lex::{Token, TokenFlags, TokenKind},
    parse::{NodeExpr, NodeScope, NodeStmt, NodeTerm, AST},
    semantic::{Checker, SemFn, SemVariable, Type},
};
use std::collections::{HashMap, HashSet};

const LOG_DEBUG_INFO: bool = false;
const SPACE: &'static str = "    ";
const MSG: &'static str = "CODEGEN";

#[derive(Debug, Clone, PartialEq)]
struct GenVariable {
    ident: Token,
    width: usize,
    type_id: usize,
    stk_index: usize,
}

struct CodeGenContext {
    reg_count: usize,
    label_count: usize,
    endif_label: String,
    loop_end_label: String,
}

pub struct Generator {
    ast: AST,
    stk_pos: usize,
    pos: (u32, u32),
    types: Vec<Type>,
    ctx: CodeGenContext,
    fn_map: HashMap<String, SemFn>,
    type_map: HashMap<String, usize>,
    stack: Vec<GenVariable>,         // stack contains variables,
    var_map: HashMap<String, usize>, // var_map contains index to variable
}

impl Generator {
    pub fn new(data: Checker) -> Generator {
        Generator {
            pos: (0, 0),
            stk_pos: 0,
            ast: data.ast,
            types: data.types,
            type_map: data.type_map,
            fn_map: data.fn_map,
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
            NodeStmt::VarSemantics(sem_var) => {
                if self
                    .get_var(sem_var.ident.value.as_ref().unwrap().as_str())
                    .is_ok()
                {
                    return err!("Re-Initialisation of a Variable:\n{sem_var:#?}");
                }
                let name = sem_var.ident.clone();
                let var = GenVariable {
                    ident: sem_var.ident,
                    stk_index: self.stk_pos + sem_var.width,
                    type_id: sem_var.type_id,
                    width: sem_var.width,
                };

                self.stk_pos += sem_var.width;
                self.var_map
                    .insert(var.ident.value.as_ref().unwrap().clone(), self.stack.len());
                self.stack.push(var);

                let mut str = String::new();
                if let Some(expr) = sem_var.init_expr {
                    let stk_pos = self.gen_stk_access(self.stk_pos, sem_var.width);
                    str += self.gen_expr(expr, Some(stk_pos.as_str()))?.as_str();
                }
                str.pop(); // remove '\n'
                str += format!(" ; Ident('{}')\n", name.value.as_ref().unwrap()).as_str();
                Ok(str)
            }
            NodeStmt::Assign { ident, expr } => {
                let var = self.get_var(ident.value.as_ref().unwrap().as_str())?;
                let ans_reg = self.gen_stk_access(var.stk_index, var.width);
                self.gen_expr(expr, Some(ans_reg.as_str()))
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
            NodeStmt::FnSemantics { .. } => {
                todo!("fn codegen")
            }
            NodeStmt::ReturnSemantics { expr } => {
                todo!("return codegen")
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
            NodeStmt::VarDecl { .. } | NodeStmt::FnDecl { .. } | NodeStmt::Return { .. } => {
                err!("Found {stmt:#?}.. shouldn't have.")
            }
            NodeStmt::FnCall { ident, args } => todo!(),
            NodeStmt::FnCallSemantics(_) => todo!(),
        }
    }

    // TODO: scope.inherits_stmts does nothing currently.
    fn gen_scope(&mut self, scope: NodeScope) -> Result<String, String> {
        debug!("Beginning scope");
        let var_count = self.stack.len();
        let mut asm = String::new();
        for stmt in scope.stmts {
            asm += self.gen_stmt(stmt)?.as_str();
        }

        let pop_amt = self.stack.len() - var_count;
        debug!("Ending scope, pop({pop_amt})");
        for _ in 0..pop_amt {
            let popped_var = match self.stack.pop() {
                Some(var) => var,
                None => return err!("uhh.. scope messed up"),
            };
            self.stk_pos -= popped_var.width;
            self.var_map
                .remove(popped_var.ident.value.as_ref().unwrap().as_str())
                .unwrap();
            debug!("Scope ended, removing {popped_var:#?}");
        }
        Ok(asm)
    }

    fn gen_expr(&mut self, expr: NodeExpr, ans_reg: Option<&str>) -> Result<String, String> {
        debug!(
            self,
            "{}\ngen expr, reg: {ans_reg:?} \n{expr:#?}\n",
            "-".repeat(20)
        );
        let mut asm = String::new();
        match expr {
            NodeExpr::Term(term) => return self.gen_term(term, ans_reg),
            NodeExpr::BinaryExpr { op, lhs, rhs } => {
                let lhs_asm = self.gen_expr(*lhs, None)?;
                let rhs_asm = self.gen_expr(*rhs, None)?;

                let flags = op.get_flags();
                let op_asm = match flags {
                    _ if flags.contains(TokenFlags::BIT) => self.gen_bitwise(op)?,
                    _ if flags.contains(TokenFlags::ARITH) => self.gen_arithmetic(op)?,
                    _ if flags.contains(TokenFlags::CMP) => self.gen_comparison(op)?,
                    _ if flags.contains(TokenFlags::LOG) => {
                        return self.gen_logical(op, ans_reg, lhs_asm, rhs_asm)
                    }
                    _ => {
                        return err!(
                            "Unable to generate binary expression:\n{lhs_asm}..{op:?}..\n{rhs_asm}"
                        )
                    }
                };
                self.release_reg(); // first reg stores arithmetic answer, don't release it.

                asm += lhs_asm.as_str();
                asm += rhs_asm.as_str();
                asm += op_asm.as_str();
            }
            NodeExpr::UnaryExpr { op, operand } => {
                let operand_clone = *operand.clone();
                asm += self.gen_expr(*operand, None)?.as_str();

                let reg = self.get_reg(self.ctx.reg_count);
                let op_asm = match op {
                    TokenKind::Tilde => format!("{SPACE}not {reg}\n"),
                    TokenKind::Sub => format!("{SPACE}neg {reg}\n"),
                    TokenKind::CmpNot => format!(
                        "{SPACE}test {reg}, {reg}\n\
                         {SPACE}sete al\n\
                         {SPACE}movzx {reg}, al\n"
                    ),
                    TokenKind::Ampersand => match operand_clone {
                        NodeExpr::Term(NodeTerm::Ident(name)) => {
                            let stk_pos = self
                                .get_var(name.value.as_ref().unwrap().as_str())?
                                .stk_index;
                            format!("{SPACE}lea {reg}, [rbp+{stk_pos}]\n")
                        }
                        _ => return err!("Attempted 'addr_of' operation, found right hand value"),
                    },
                    TokenKind::Ptr => {
                        // TODO(TOM): hardcoded 8 byte ptr size. doesn't work proper..
                        // format!("{SPACE}mov {reg}, {} [{reg}]\n", self.gen_access_size(8))
                        format!("{SPACE}mov {reg}, [{reg}]\n")
                    }
                    _ => return err!("Unable to generate unary expression: '{op:?}'"),
                };
                asm += op_asm.as_str();
            }
        }
        // don't need to release reg if its just operation, just doing stuff on data.
        // only release if changing stack data.
        if let Some(reg) = ans_reg {
            asm += format!("{SPACE}mov {reg}, {}\n", self.get_reg(self.ctx.reg_count)).as_str();
            self.release_reg();
        }
        Ok(asm)
    }

    fn gen_term(&mut self, term: NodeTerm, ans_reg: Option<&str>) -> Result<String, String> {
        match term {
            NodeTerm::False | NodeTerm::True => todo!("NodeTerm False/True"),
            NodeTerm::IntLit(tok) => {
                self.pos = tok.pos;
                let reg = match ans_reg {
                    Some(reg) => reg,
                    None => self.next_reg(),
                };
                Ok(format!(
                    "{SPACE}mov {reg}, {}\n",
                    tok.value.as_ref().unwrap()
                ))
            }
            NodeTerm::Ident(tok) => {
                self.pos = tok.pos;
                let ident = tok.value.clone().unwrap();
                let var = self.get_var(ident.as_str())?;
                let stk_pos = self.gen_var_access(var.stk_index, var.width);
                let reg = match ans_reg {
                    Some(reg) => reg,
                    None => self.next_reg(),
                };
                Ok(format!("{SPACE}mov {reg}, {stk_pos} ; {tok:?}\n"))
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
            _ => err!("Unable to generate Logical comparison"),
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
            _ => return err!("Unable to generate Arithmetic operation: '{op:?}'"),
        };
        Ok(format!("{SPACE}{operation_asm}\n"))
    }

    fn gen_bitwise(&mut self, op: TokenKind) -> Result<String, String> {
        let reg1 = self.get_reg(self.ctx.reg_count - 1);
        let reg2 = self.get_reg(self.ctx.reg_count);
        let asm = match op {
            TokenKind::Bar => "or",
            TokenKind::Tilde => "xor",
            TokenKind::Ampersand => "and",
            TokenKind::Shl => "sal",
            TokenKind::Shr => "sar",
            // TODO: (Types) Unsigned shift: shl, shr
            _ => return err!("Unable to generate Bitwise operation"),
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
            _ => err!("Unable to generate comparison modifier"),
        }
    }

    fn gen_label(&mut self, name: &'static str) -> String {
        self.ctx.label_count += 1;
        format!(".{:X}_{name}", self.ctx.label_count) // '.' denotes a local scoped label in asm
    }

    // TODO(TOM): need to use word_size??
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
            _ => err!("Invalid word_size found: '{word_size}' .. should be unreachable?").unwrap(),
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
            None => err!("out of registers! uhh probably should fix this").unwrap(),
        }
    }

    fn get_reg(&mut self, index: usize) -> &'static str {
        // preserved_registers = ["rdx", ...],
        let scratch_registers = ["rax", "rcx", "rsi", "rdi", "r8", "r9", "r10", "r11"];
        match scratch_registers.get(index - 1) {
            Some(reg) => reg,
            None => err!("out of registers! uhh probably should fix this").unwrap(),
            // TODO(TOM): either figure out when to use reserved registers, split expressions that are too long to let registers reset, or use stack!
        }
    }

    fn release_reg(&mut self) {
        self.ctx.reg_count -= 1;
    }

    fn get_var(&self, ident: &str) -> Result<&GenVariable, String> {
        match self.var_map.get(ident) {
            Some(idx) => Ok(self.stack.get(*idx).unwrap()),
            None => err!("Variable: {ident:?} doesn't exist."),
        }
    }
}
