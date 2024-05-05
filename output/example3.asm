global _start
_start:
; setup stack frame
    push rbp
    mov rbp, rsp
; Program Start
    mov QWORD [rsp+0], 20
; Add
    mov rax, QWORD [rsp+0] ; Ident('x')
    mov rcx, 30
    add rax, rcx
    mov QWORD [rsp+8], rax
; Subtract
    mov rax, QWORD [rsp+0] ; Ident('x')
    mov rcx, 10
    sub rax, rcx
    mov QWORD [rsp+16], rax
; Multiply
    mov rax, QWORD [rsp+0] ; Ident('x')
    mov rcx, 2
    imul rax, rcx
    mov QWORD [rsp+24], rax
; Divide
    mov rax, QWORD [rsp+0] ; Ident('x')
    mov rcx, 2
    xor rdx,rdx
    idiv rcx
    mov QWORD [rsp+32], rax
; BitwiseNot
    mov rax, QWORD [rsp+0] ; Ident('x')
    not rax
    mov QWORD [rsp+40], rax
; BitwiseOr
    mov rax, QWORD [rsp+0] ; Ident('x')
    mov rcx, QWORD [rsp+8] ; Ident('a')
    or rax, rcx
    mov QWORD [rsp+48], rax
; BitwiseAnd
    mov rax, QWORD [rsp+0] ; Ident('x')
    mov rcx, QWORD [rsp+8] ; Ident('a')
    and rax, rcx
    mov QWORD [rsp+56], rax
; BitwiseXor
    mov rax, QWORD [rsp+0] ; Ident('x')
    mov rcx, 2
    xor rax, rcx
    mov QWORD [rsp+64], rax
; RightShift
    mov rax, QWORD [rsp+0] ; Ident('x')
    mov rcx, 2
    sar rax, rcx
    mov QWORD [rsp+72], rax
; LeftShift
    mov rax, QWORD [rsp+0] ; Ident('x')
    mov rcx, 3
    sal rax, rcx
    mov QWORD [rsp+80], rax
; LogicalAnd
; GreaterThan
    mov rax, 4
    mov rcx, 2
    cmp rax, rcx
    setg al
    mov rax, al
    mov rax, rax
; Equal
    mov rax, 2
    mov rcx, 2
    cmp rax, rcx
    sete al
    mov rax, al
    mov rcx, rax
__LHS-ASM__
    cmp rax, 0
    je label1_AND1
__RHS-ASM__
    cmp rcx, 0
    je label2_AND2
    mov rax, 1
    jmp label2_AND2
label1_AND1:
    mov rax, 0
label2_AND2:
    mov QWORD [rsp+88], rax
; Exit Program
    mov rdi, QWORD [rsp+88] ; Ident('n')
    mov rax, 60
    syscall
