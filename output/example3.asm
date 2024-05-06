global _start
_start:
; setup stack frame
    push rbp
    mov rbp, rsp
; Program Start
    mov QWORD [rsp+0], 20
; If
; LogicalOr
    mov rax, 0
    cmp rax, 0
    jne label2_OR_TRUE
; GreaterEqual
; Add
    mov rax, 5
    mov rcx, 1
    add rax, rcx
    mov rcx, 6
    cmp rax, rcx
    setge al
    movzx rax, al
    mov rcx, rax; 
    cmp rcx, 0
    je label1_OR_FALSE
label2_OR_TRUE:
    mov rax, 1
    jmp label3_OR_FINAL
label1_OR_FALSE:
    mov rax, 0
label3_OR_FINAL:
    movzx rax, al
    cmp rax, 0 
    je label4_IF_FALSE; Exit Program
    mov rdi, 20
    mov rax, 60
    syscall
label4_IF_FALSE:
; Equal
    mov rax, QWORD [rsp+0]; Ident('x')
    mov rcx, 20
    cmp rax, rcx
    sete al
    movzx rax, al

    cmp rax, 0
    je label5_ELIF_FALSE; Exit Program
    mov rdi, 19
    mov rax, 60
    syscall
label5_ELIF_FALSE:
; Else
; Exit Program
    mov rdi, 10
    mov rax, 60
    syscall
; Exit Program
    mov rdi, 100
    mov rax, 60
    syscall
