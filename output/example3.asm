global _start
_start:
; setup stack frame
    push rbp
    mov rbp, rsp
; Program Start
; LogicalOr
; NotEqual
    mov rax, 10
    mov rcx, 10
    cmp rax, rcx
    setne al
    movzx rax, al
    mov rax, rax
; Equal
    mov rax, 5
    mov rcx, 4
    cmp rax, rcx
    sete al
    movzx rax, al
    mov rcx, rax
; LogicalOr
; NotEqual
    mov rax, 10
    mov rcx, 10
    cmp rax, rcx
    setne al
    movzx rax, al
    mov rax, rax
    cmp rax, 0
    jne label2_OR_TRUE
; Equal
    mov rax, 5
    mov rcx, 4
    cmp rax, rcx
    sete al
    movzx rax, al
    mov rcx, rax
    cmp rcx, 0
    je label1_OR_FALSE
label2_OR_TRUE:
    mov rax, 1
    jmp label3_OR_FINAL
label1_OR_FALSE:
    mov rax, 0
label3_OR_FINAL:
    movzx rax, al
    mov QWORD [rsp+0], rax
; Exit Program
    mov rdi, QWORD [rsp+0] ; Ident('o')
    mov rax, 60
    syscall
