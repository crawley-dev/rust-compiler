global _start
_start:
; setup stack frame
    push rbp
    mov rbp, rsp
; Program Start
; If
; LogicalOr
    mov rax, 0
    cmp rax, 0
    jne .4_OR_TRUE
    mov rcx, 5
    mov rsi, 1
    add rcx, rsi
    mov rsi, 6
    cmp rcx, rsi
    setg al
    movzx rcx, al
    cmp rcx, 0
    je .3_OR_FALSE
.4_OR_TRUE:
    mov rax, 1
    jmp .5_OR_FINAL
.3_OR_FALSE:
    mov rax, 0
.5_OR_FINAL:
    movzx rax, al
    cmp rax, 0 
    je .2_IF_FALSE
; Exit Program
    mov rdi, 69
    mov rax, 60
    syscall
    jmp .1_END_IF
.2_IF_FALSE:
    mov rsi, 0
    mov rdi, 0
    cmp rsi, rdi
    sete al
    movzx rsi, al
    test rsi, rsi
    sete al
    movzx rsi, al

    cmp rax, 0
    je .6_ELIF_FALSE
; Exit Program
    mov rdi, 25
    mov rax, 60
    syscall
    jmp .1_END_IF
.6_ELIF_FALSE:
.1_END_IF:
; Exit Program
    mov rdi, 5
    mov rax, 60
    syscall
