BITS 64
global _start
_start:
    mov rax, 5
    push rax
    mov rax, 20
    push rax
    mov rax, 5
    push rax
    pop rax
    pop rbx
    add rax, rbx
    push rax
    pop rax
    pop rbx
    sub rax, rbx
    push rax
    mov rax, 5
    push rax
    add rsp, 8
    push QWORD [rsp + 0]
    mov rax, 60
    pop rdi
    syscall
