global _start
_start:
    mov rax, 5
    push rax
    mov rax, 20
    push rax
    mov rax, 40
    push rax
    mov rax, 1
    push rax
    push QWORD [rsp + 8]
    pop rax
    pop rbx
    add rax, rbx
    push rax
