global _start
_start:
; Var: IntLit(5)
    mov rax, 5
    push rax
; Var: IntLit(10)
    mov rax, 10
    push rax
; Var: Ident('a')
    push QWORD [rsp + 16]
; Var: Ident('b')
    push QWORD [rsp + 16]
; Var: Ident('a')
    push QWORD [rsp + 32]
    pop rax
    pop rbx
; Binary Expr: BitwiseOr
    or rax, rbx
    push rax
; Var: IntLit(2)
    mov rax, 2
    push rax
; Var: Ident('b')
    push QWORD [rsp + 32]
    pop rax
    pop rbx
; Binary Expr: LeftShift
    sal rax, rbx    
    push rax
; Var: IntLit(3)
    mov rax, 3
    push rax
; Var: Ident('c')
    push QWORD [rsp + 32]
    pop rax
    pop rbx
; Binary Expr: RightShift
    sar rax, rbx
    push rax
; Var: Ident('b')
    push QWORD [rsp + 40]
    mov rax, 60
    pop rdi
    syscall
