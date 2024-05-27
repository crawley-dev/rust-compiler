global _start
_start:
; setup stack frame
    push rbp
    mov rbp, rsp
    ; Program Start
    mov QWORD [rsp+0], 2
    mov QWORD [rsp+8], 0
; While
    jmp .1_WHILE_CMP
.2_WHILE_SCOPE:
    mov rax, QWORD [rsp+0] ; Ident('ans')
    mov rcx, 2
    imul rax, rcx
    mov QWORD [rsp+0], rax
    mov rax, QWORD [rsp+8] ; Ident('i')
    mov rcx, 1
    add rax, rcx
    mov QWORD [rsp+8], rax
; If
    mov rax, QWORD [rsp+8] ; Ident('i')
    mov rcx, 5
    cmp rax, rcx
    setg al
    movzx rax, al
    cmp rax, 0 
    je .4_IF_FALSE
    jmp .3_WHILE_END ; break
.4_IF_FALSE:
.1_WHILE_CMP:
    mov rcx, 1
    mov rsi, 1
    cmp rcx, rsi
    sete al
    movzx rcx, al
    cmp rax, 0
    jne .2_WHILE_SCOPE
.3_WHILE_END:
; Exit Program
    mov rdi, QWORD [rsp+0] ; Ident('ans')
    mov rax, 60
    syscall
