global _start
_start:
    mov rax, 20
    push rax
    mov rax, 10
    push rax
    pop rax
    pop rbx
    cmp rax, rbx
    setg al
    movzx rax, al
    push rax
    push rax
    mov rax, 20
    push rax
    push QWORD [rsp + 24]
    mov rax, 60
    pop rdi
    syscall
