BITS 64
global _start
_start:
    mov rax, 69
    push rax
    mov rax, 60
    pop rdi
    syscall
