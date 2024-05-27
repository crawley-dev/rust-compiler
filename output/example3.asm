global _start
_start:
; setup stack frame
    push rbp
    mov rbp, rsp
    ; Program Start
    push 0
    push 2
; While
    jmp .1_WHILE_CMP
.2_WHILE_SCOPE:
    mov rax, QWORD [rbp-16] ; Ident('ans')
    mov rcx, 2
    imul rax, rcx
    mov QWORD [rbp-16], rax
    mov rax, QWORD [rbp-8] ; Ident('i')
    mov rcx, 1
    add rax, rcx
    mov QWORD [rbp-8], rax
; If
    mov rax, QWORD [rbp-8] ; Ident('i')
    mov rcx, 5
    cmp rax, rcx
    setg al
    movzx rax, al
    cmp rax, 0 
    je .4_IF_FALSE
    jmp .3_WHILE_END ; break
; While
    jmp .5_WHILE_CMP
.6_WHILE_SCOPE:
; If
    mov rcx, 5
    mov rsi, 5
    add rcx, rsi
    mov rsi, 10
    cmp rcx, rsi
    sete al
    movzx rcx, al
    cmp rax, 0 
    je .8_IF_FALSE
    jmp .7_WHILE_END ; break
.8_IF_FALSE:
.5_WHILE_CMP:
    mov rsi, 2
    mov rdi, 2
    cmp rsi, rdi
    sete al
    movzx rsi, al
    cmp rax, 0
    jne .6_WHILE_SCOPE
.7_WHILE_END:
.4_IF_FALSE:
.1_WHILE_CMP:
    mov rdi, 1
    mov r8, 1
    cmp rdi, r8
    sete al
    movzx rdi, al
    cmp rax, 0
    jne .2_WHILE_SCOPE
.3_WHILE_END:
; Exit Program
    mov rdi, QWORD [rbp-16] ; Ident('ans')
    mov rax, 60
    syscall
