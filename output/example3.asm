global _start
_start:
; setup stack frame
    push rbp
    mov rbp, rsp
    ; Program Start
    mov dword [rbp-4], 5
    mov dword [rbp-8], 0
    mov byte [rbp-9], 2
    mov byte [rbp-10], 5
    mov rax, 5
    neg rax
    mov dword [rbp-8], rax
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
    mov dword [rbp-8], rax
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
.1_WHILE_CMP:
    mov rdi, 1
    mov r8, 1
    cmp rdi, r8
    sete al
    movzx rdi, al
    cmp rax, 0
    jne .2_WHILE_SCOPE
.3_WHILE_END:
; If
    mov r8, 5
    mov r9, 5
    cmp r8, r9
    sete al
    movzx r8, al
    cmp rax, 0 
    je .9_IF_FALSE
    mov r9, [rbp-9] ; Ident('ans')
    mov r10, 2
    imul r9, r10
    mov byte [rbp-9], r9
; If
    mov r9, 10
    mov r10, 10
    cmp r9, r10
    sete al
    movzx r9, al
    cmp rax, 0 
    je .A_IF_FALSE
    mov r10, [rbp-9] ; Ident('ans')
    mov r11, 5
    add r10, r11
    mov byte [rbp-9], r10
.A_IF_FALSE:
.9_IF_FALSE:
