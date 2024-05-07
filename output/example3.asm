global _start
_start:
; setup stack frame
    push rbp
    mov rbp, rsp
; Program Start
    mov QWORD [rsp+0], 20
    mov QWORD [rsp+8], 0
; While
    jmp .1_WHILE_CMP
.2_WHILE_SCOPE:
; Add
    mov rax, QWORD [rsp+8]; Ident('i')
    mov rcx, 1
    add rax, rcx
    mov QWORD [rsp+8], rax; 
.1_WHILE_CMP:
; LessThan
    mov rax, QWORD [rsp+8]; Ident('i')
    mov rcx, 10
    cmp rax, rcx
    setl al
    movzx rax, al
    cmp rax, 0
    jne .2_WHILE_SCOPE
; Exit Program
    mov rdi, QWORD [rsp+8]; Ident('i')
    mov rax, 60
    syscall
