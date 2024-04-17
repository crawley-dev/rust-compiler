global _start
_start:
    mov rax, 5
    push rax
    mov rax, 6
    push rax
    mov rax, 0
    push rax
    pop rax
    test rax, rax
    jz label0
    push QWORD [rsp + 8]
    mov rax, 60
    pop rdi
    syscall
label0:
    push QWORD [rsp + 0]
    mov rax, 60
    pop rdi
    syscall
