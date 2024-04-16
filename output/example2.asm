BITS 64
global _start
_start:
    mov rax, 5
    push rax
    mov rax, 20
    push rax
    add rsp, 8
    push QWORD [rsp + 0]
    mov rax, 60
    pop rdi
    syscall
