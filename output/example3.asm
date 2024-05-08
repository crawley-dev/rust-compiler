global _start
_start:
; setup stack frame
    push rbp
    mov rbp, rsp
; Program Start
    mov QWORD [rsp+0], 20
    mov QWORD [rsp+8], 0
    mov QWORD [rsp+16], 0
    mov QWORD [rsp+24], 100
; While
    jmp .1_WHILE_CMP
.2_WHILE_SCOPE:
; If
; Equal
    mov rax, QWORD [rsp+16] ; Ident('i')
    mov rcx, 10
    cmp rax, rcx
    sete al
    movzx rax, al
    cmp rax, 0 
    je .4_IF_FALSE
    jmp .3_WHILE_END ; break
.4_IF_FALSE:
    mov QWORD [rsp+32], 0
; While
    jmp .5_WHILE_CMP
.6_WHILE_SCOPE:
; If
; Equal
    mov rax, QWORD [rsp+32] ; Ident('j')
    mov rcx, 10
    cmp rax, rcx
    sete al
    movzx rax, al
    cmp rax, 0 
    je .8_IF_FALSE
    jmp .7_WHILE_END ; break
.8_IF_FALSE:
; Add
    mov rax, QWORD [rsp+8] ; Ident('ans')
    mov rcx, 1
    add rax, rcx
    mov QWORD [rsp+8], rax
; Add
    mov rax, QWORD [rsp+32] ; Ident('j')
    mov rcx, 1
    add rax, rcx
    mov QWORD [rsp+32], rax
.5_WHILE_CMP:
; Equal
    mov rax, 1
    mov rcx, 1
    cmp rax, rcx
    sete al
    movzx rax, al
    cmp rax, 0
    jne .6_WHILE_SCOPE
.7_WHILE_END:
; Add
    mov rax, QWORD [rsp+16] ; Ident('i')
    mov rcx, 1
    add rax, rcx
    mov QWORD [rsp+16], rax
.1_WHILE_CMP:
; Equal
    mov rax, 1
    mov rcx, 1
    cmp rax, rcx
    sete al
    movzx rax, al
    cmp rax, 0
    jne .2_WHILE_SCOPE
.3_WHILE_END:
; Remainder
    mov rax, QWORD [rsp+8] ; Ident('ans')
    mov rcx, 3
    cqo
    idiv rcx
    mov rax, rdx
    mov QWORD [rsp+40], rax
; Exit Program
    mov rdi, QWORD [rsp+40] ; Ident('ans123')
    mov rax, 60
    syscall
