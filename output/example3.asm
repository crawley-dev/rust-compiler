global _start
_start:
; setup stack frame
    push rbp
    mov rbp, rsp
    ; Program Start
    mov qword [rbp-8], 0
    mov byte [rbp-9], 2
; While
    jmp .1_WHILE_CMP
.2_WHILE_SCOPE:
    mov rax, [rbp-9] ; Ident('ans')
    mov rcx, 2
    imul rax, rcx
    mov byte [rbp-9], rax
    mov rax, [rbp-8] ; Ident('i')
    mov rcx, 1
    add rax, rcx
    mov qword [rbp-8], rax
; If
    mov rax, [rbp-8] ; Ident('i')
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
    mov rdi, [rbp-8] ; Ident('i')
    mov r8, 2
    add rdi, r8
    mov qword [rbp-8], rdi
.1_WHILE_CMP:
    mov rdi, 1
    mov r8, 1
    cmp rdi, r8
    sete al
    movzx rdi, al
    cmp rax, 0
    jne .2_WHILE_SCOPE
.3_WHILE_END:
