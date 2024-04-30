global _start
_start:
    mov rax, 10
    push rax ; IntLit(10)
    mov rax, 20
    push rax ; IntLit(20)
    mov rax, 30
    push rax ; IntLit(30)
    mov rax, 40
    push rax ; IntLit(40)
    mov rax, 50
    push rax ; IntLit(50)
; Binary Expr: GreaterThan
    mov rax, 2
    push rax ; IntLit(2)
    mov rax, 1
    push rax ; IntLit(1)
    pop rax
    pop rdx
    cmp rax, rdx
    setg al
    movzx rax, al
    push rax
    pop rax
    cmp rax, 0
    je label1_AND1
; Binary Expr: LessThan
    mov rax, 3
    push rax ; IntLit(3)
    mov rax, 2
    push rax ; IntLit(2)
    pop rax
    pop rdx
    cmp rax, rdx
    setl al
    movzx rax, al
    push rax
    pop rax
    cmp rax, 0
    je label1_AND1
    mov rax, 1
    jmp label2_AND2
label1_AND1:
    mov rax, 0
label2_AND2:
    push rax
; Exit Program
    push QWORD [rsp + 0] ; Ident('x')
    mov rax, 60
    pop rdi
    syscall
